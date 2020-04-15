package xyz.bitml.api

import java.io.File

import akka.actor.{Actor, ActorRef, ActorSystem, CoordinatedShutdown, Props}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import fr.acinq.bitcoin.Crypto.PrivateKey
import xyz.bitml.api.ChunkPrivacy.ChunkPrivacy
import xyz.bitml.api.messaging.{AskForSigs, AssembledTx, Authorize, CurrentState, DumpState, Init, Internal, Listen, Node, PreInit, Query, StopListening, TryAssemble}
import xyz.bitml.api.persistence.State
import xyz.bitml.api.serialization.Serializer

case class Client() extends Actor with LazyLogging{

  private var state : State = State()
  private var identity : PrivateKey = _

  private var msgNode : Option[ActorRef] = Option.empty[ActorRef]
  private var system : Option[ActorSystem] = Option.empty[ActorSystem]

  val ser = new Serializer()
  val conv = new SegwitConverter()
  val sig = new Signer()

  override def receive : Receive = {
    case Init(pk, x) => identity = pk; initState(x)
    case Listen(c,s) => listenMsg(c,s)
    case StopListening() => shutdownMsg()
    case PreInit() => preInit()
    case TryAssemble(t) => sender() ! assembleTx(t)
    case AskForSigs(t) => retrieveSigs(t)
    case Authorize(t) => authorize(t)
    case DumpState() => sender() ! CurrentState(ser.prettyPrintState(state))
    case _ => logger.error("Unexpected event type")
  }

  // Load a state from an external json string, convert to witness and fill with signatures from our identity.
  def initState(jsonState : String) : Unit = {

    // Load an input state and convert into witness-based equivalent if not already converted
    val tmpState = ser.loadState(jsonState)
    conv.convertTree(tmpState.metadb, tmpState.txdb)

    // Scroll through the meta info and fill in the signatures dependent on our own identity.
    for (t <- tmpState.metadb.dump()) sig.fillEntry(tmpState.txdb.fetch(t._1).get, t._2, identity)

    // Save processed state as this client's new state.
    state = tmpState

    // Verify whether our identity matches one of the participants
    state.partdb.fetch(identity.publicKey.toString()).getOrElse(logger.error("Identity doesn't match any of the contract participants!"))
  }

  // Start an ActorSystem with a predetermined config and the current state objects.
  def listenMsg(configPath : String, systemName : String): Unit ={
    if (system.nonEmpty) {
      logger.error("System already active!")
      return
    }
    // Setup network
    val configFile = getClass.getClassLoader.
      getResource(configPath).getFile
    val config = ConfigFactory.parseFile(new File(configFile))
    system = Option(ActorSystem(systemName , config))
    msgNode = Option(system.get.actorOf(Props(classOf[Node], state.metadb, state.txdb), name = "ContractNode"))

    // Check if the config data matches our participant's declared info.
    // there are a few network configurations where they reasonably won't match, so this is not a critical error.
    val declaredAddr = state.partdb.fetch(identity.publicKey.toString()).get.endpoint
    if ((config.getInt("akka.remote.artery.canonical.port") != declaredAddr.port.get ) || (systemName != declaredAddr.system)) {
      logger.warn("Contract-declared endpoint doesn't match node configuration for this participant!")
    }
  }

  // Stop the current ActorSystem
  def shutdownMsg(): Unit = {
    if (system.isEmpty) {
      logger.warn("System already empty!")
      return
    }
    CoordinatedShutdown(system.get).run(CoordinatedShutdown.clusterLeavingReason)
    system = Option.empty
    msgNode = Option.empty
  }

  // Ask any participant involved in the tx to share their signatures.
  def retrieveSigs(txName : String): Unit = {

    val m = msgNode.getOrElse({
      logger.error("Messaging node not initialized!")
      return
    })

    val partList = txPendingList(txName)
    for (p <- partList) {
       m ! Query(p.endpoint.toString, txName)
    }
  }

  // Retrieve a list of participants whose data is missing to complete a certain tx.
  def txPendingList(txName : String) : Seq[Participant] = {
    val meta = state.metadb.fetch(txName).get
    val pubs = meta.indexData.flatMap(_._2.chunkData.filter(_.data.isEmpty).map(_.owner.get))
    pubs.map(x => state.partdb.fetch(x.toString()).get).toSeq
  }

  // Retrieve a list of participants with missing data at the expected privacy level.
  def txPendingPriv(txName: String, privacyLevel: ChunkPrivacy) = {
    val meta = state.metadb.fetch(txName).get
    val pubs = meta.indexData.flatMap(_._2.chunkData.filter(x => x.data.isEmpty && x.chunkPrivacy == privacyLevel).map(_.owner.get))
    pubs.map(x => state.partdb.fetch(x.toString()).get).toSeq
  }

  // If completable, assemble given tx and return raw serialized form. TODO: proper try/success/failure?
  def assembleTx(txName : String) : AssembledTx = {
    val canComplete = txPendingList(txName)
    if (canComplete.nonEmpty) {
      // IF we're the only participant, we will need to add the missing non-signature data.
      if (!canComplete.exists(p => p != state.partdb.fetch(identity.publicKey.toString()).get)) {
        //TODO: Insert element as hex string -> ByteVector
      }else{ // If other participants' info is missing (either by lack of authorization or no response), query them again.
        logger.error("Cannot assemble tx "+txName+": Missing datapoints from "+canComplete.map(_.name))
        // Automatically start a retrieveSigs on failure.
        retrieveSigs(txName)
        logger.debug("Waiting for responses...")
        Thread.sleep(1000) // TODO: class-defined default timeout variable
        val newMissing = txPendingList(txName)
        if (newMissing.nonEmpty) {
          logger.error("Cannot assemble tx "+txName+": Missing datapoints from "+newMissing.map(_.name))
          return null
        }
      }
    }
    val assembled = sig.assembleTx(state.txdb.fetch(txName).get, state.metadb.fetch(txName).get)
    // log txid change upon completion of non-segwit transaction.
    // This would introduce a separate malleability problem.
    // The best solution would be to only accept segwit contract inputs
    if (assembled.txid != state.txdb.fetch(txName).get.txid){
      logger.warn("Transaction "+ txName +" changed txid to "+ assembled.txid.toHex +" upon assembly.")
      // Store assembled tx
      state.txdb.save(txName, assembled)
    }

    AssembledTx(txName, assembled.toString())
  }

  // We won't release our TInit signature until all chunks marked with PUBLIC have been shared with us.
  def preInit(): Unit ={
    val m = msgNode.getOrElse({
      logger.error("Messaging node not initialized!")
      return
    })

    var missing = Set[Participant]()

    val metaSet = state.metadb.dump().keySet
    for (t <- metaSet) {
      val tMiss = txPendingPriv(t, ChunkPrivacy.PUBLIC)
      missing = missing ++ tMiss.toSet
      for (p <- tMiss) m ! Query(p.endpoint.toString, t)
    }
    if (missing.isEmpty) {
      // TODO: Mark TInit as public now that we have verified ownership of every public chunk
    } else {
      logger.info("Preinit: Missing chunk info from " + missing)
    }
  }

  // Verify if we need one or more participants' authorization data to complete a transaction and return their set.
  def checkAuth(txName : String): Seq[Participant] ={
    txPendingPriv(txName, ChunkPrivacy.AUTH).filter(p=> p != state.partdb.fetch(identity.publicKey.toString()).get)
  }

  // Switch any AUTH chunk we own in a transaction into PUBLIC. This will allow us to share their content if asked.
  def authorize(txName : String): Unit = {
    val toAuth = state.metadb.fetch(txName).get
    val authorized = toAuth.indexData.map(f => ({
      // Any chunk that we own inside the transaction is switched from AUTH to PUBLIC
      val newChunks = f._2.chunkData.map(c =>
        if (state.partdb.fetch(c.owner.get.toString()) == state.partdb.fetch(identity.publicKey.toString()))
          c.copy(
            chunkPrivacy = c.chunkPrivacy match {
              case ChunkPrivacy.AUTH => ChunkPrivacy.PUBLIC
            })
        else c)
      f.copy(_2 = f._2.copy(chunkData = newChunks))
    }))
    state.metadb.save(toAuth.copy(indexData = authorized))
  }
}
