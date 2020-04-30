package xyz.bitml.api

import java.io.File

import akka.actor.{Actor, ActorRef, ActorSystem, CoordinatedShutdown, Props}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import fr.acinq.bitcoin.Crypto.PrivateKey
import scodec.bits.ByteVector
import wf.bitcoin.javabitcoindrpcclient.BitcoinJSONRPCClient
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
  val rpc = new BitcoinJSONRPCClient()

  override def receive : Receive = {
    case Init(pk, x) => identity = pk; initState(x)
    case Listen(c,s) => listenMsg(c,s)
    case StopListening() => shutdownMsg()
    case PreInit() => preInit()
    case TryAssemble(t, ap) => sender() ! assembleTx(t, ap)
    case AskForSigs(t) => retrieveSigs(t)
    case Authorize(t) => authorize(t)
    case DumpState() => sender() ! CurrentState(new Serializer(ChunkPrivacy.PRIVATE).prettyPrintState(state))
    case _ => logger.error("Unexpected event type")
  }

  // Load a state from an external json string, convert to witness and fill with signatures from our identity.
  def initState(jsonState : String) : Unit = {

    // Load an input state and convert into witness-based equivalent if not already converted
    val tmpState = ser.loadState(jsonState)
    conv.convertTree(tmpState.metadb, tmpState.txdb)

    // Save processed state as this client's new state.
    state = tmpState

    fillSigs()

    // Verify whether our identity matches one of the participants
    state.partdb.fetch(identity.publicKey.toString()).getOrElse(
      logger.error("Identity %s doesn't match any of the contract participants!" format (identity.publicKey))
    )
  }

  def fillSigs(): Unit ={
    // Scroll through the meta info and fill in the signatures dependent on our own identity.
    for (t <- state.metadb.dump()) sig.fillEntry(state.txdb.fetch(t._1).get, t._2, identity)
    state.metadb.validateAll(state.txdb)
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
  def txPendingList(txName : String) : Set[Participant] = {
    val meta = state.metadb.fetch(txName).get
    val pubs = meta.indexData.flatMap(_._2.chunkData.filter(_.data.isEmpty).map(_.owner.get))
    pubs.map(x => state.partdb.fetch(x.toString()).get).toSet
  }

  // Retrieve a list of participants with missing data at the expected privacy level.
  def txPendingPriv(txName: String, privacyLevel: ChunkPrivacy): Set[Participant] = {
    val meta = state.metadb.fetch(txName).get
    val pubs = meta.indexData.flatMap(_._2.chunkData.filter(x => x.data.isEmpty && x.chunkPrivacy == privacyLevel).map(_.owner.get))
    pubs.map(x => state.partdb.fetch(x.toString()).get).toSet
  }

  // If completable, assemble given tx and return raw serialized form. TODO: proper try/success/failure?
  def assembleTx(txName : String, autoPublish : Boolean) : AssembledTx = {
    val canComplete = txPendingList(txName)
    if (canComplete.nonEmpty) {
      // IF we're the only participant, we will need to add the missing non-signature data manually at runtime.
      if (!canComplete.exists(p => p != state.partdb.fetch(identity.publicKey.toString()).get)) {
        fillIn(txName)
        return null
      }else{ // If other participants' info is missing (either by lack of authorization or no response), query them again.
        logger.error("Cannot assemble tx "+txName+": Missing datapoints from "+canComplete.map(_.name))
        // Automatically start a retrieveSigs on failure.
        retrieveSigs(txName)
        logger.debug("Waiting for responses...")
        Thread.sleep(500) // TODO: class-defined default timeout variable
        val newMissing = txPendingList(txName)
        if (newMissing.nonEmpty) {
          // If at least some of the missing data is a participant's authorization, reflect more specific info in the error.
          val missingAuths = checkAuth(txName)
          if (missingAuths.nonEmpty){
            logger.error("Cannot assemble tx " + txName + ": Missing authorization from " + missingAuths.map(_.name))
          }else {
            logger.error("Cannot assemble tx " + txName + ": Missing datapoints from " + newMissing.map(_.name))
          }
          return AssembledTx(txName, "")
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
      // Validate every signature that may have been broken by this hash change and repopulate our own.
      state.metadb.validateAll(state.txdb)
      fillSigs()
    }

    if (autoPublish){
      // Send raw tx
      val addedTx = rpc.sendRawTransaction(assembled.toString())
      logger.info("Sent raw transaction, txid %s" format (addedTx))
    }
    // Return serialized tx
    logger.debug("Assembled transaction %s into %s" format (txName, assembled.toString()))

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
      logger.info("Preinit: All public chunks retrieved. Authorizing Tinit...")
      authorize("Tinit") // TODO: init not hardcoded?
    } else {
      logger.info("Preinit: Missing chunk info from " + missing.map(_.name))
    }
  }


  // Verify if we need one or more participants' authorization data to complete a transaction and return their set.
  def checkAuth(txName : String): Set[Participant] ={
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

  // Interactively insert non-signature data that we own into the transaction.
  // This is very finicky and should only be used if adding the data into the bitml or into the json state is impossible.
  def fillIn(txName : String): Unit ={
    val identityParticipant = state.partdb.fetch(identity.publicKey.toString()).get
    val baseTx = state.metadb.fetch(txName).get
    val updatedIndex = baseTx.indexData.map(f => {
      val newChunks = f._2.chunkData.map(c => {
        if ((state.partdb.fetch(c.owner.get.toString()).get == identityParticipant) &&
          (c.data.isEmpty))
          c.copy(data = ByteVector.fromValidHex(scala.io.StdIn.readLine("Insert hex for value at tx %s index %d chunk %d: " format (txName, f._1, c.chunkIndex))))
        else c.copy()
      })
      f.copy(_2 = f._2.copy(chunkData = newChunks))
    })
    val updatedTx = baseTx.copy(indexData = updatedIndex)
    state.metadb.save(updatedTx)
  }
}
