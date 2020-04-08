package xyz.bitml.api

import java.io.File

import akka.actor.{Actor, ActorRef, ActorSystem, CoordinatedShutdown, Props}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import fr.acinq.bitcoin.Crypto.PrivateKey
import xyz.bitml.api.messaging.{AskForSigs, AssembledTx, CurrentState, DumpState, Init, Internal, Listen, Node, Query, StopListening, TryAssemble}
import xyz.bitml.api.persistence.State
import xyz.bitml.api.serialization.Serializer

case class Client (identity : PrivateKey) extends Actor with LazyLogging{

  private var state : State = State()

  private var msgNode : Option[ActorRef] = Option.empty[ActorRef]
  private var system : Option[ActorSystem] = Option.empty[ActorSystem]

  val ser = new Serializer()
  val conv = new SegwitConverter()
  val sig = new Signer()

  override def receive : Receive = {
    case Init(x) => initState(x)
    case Listen(c,s) => listenMsg(c,s)
    case StopListening() => shutdownMsg()
    case TryAssemble(t) => sender() ! AssembledTx(t, assembleTx(t))
    case AskForSigs(t) => retrieveSigs(t)
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

  // If completable, assemble given tx and return raw serialized form. TODO: proper try/success/failure?
  def assembleTx(txName : String) : String = {
    val canComplete = txPendingList(txName)
    if (canComplete.nonEmpty) {
      logger.error("Cannot assemble tx "+txName+": Missing datapoints from "+canComplete.map(_.name))
      // Automatically start a retrieveSigs on failure. TODO: evaluate if we should.
      retrieveSigs(txName)
      logger.debug("Waiting for responses...")
      Thread.sleep(1000) // TODO: class-defined default timeout variable
      if (txPendingList(txName).nonEmpty) {
        logger.error("Cannot assemble tx "+txName+": Missing datapoints from "+canComplete.map(_.name))
        return ""
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

    assembled.toString()
  }

  // TODO: A bunch of strategy-related logic

}
