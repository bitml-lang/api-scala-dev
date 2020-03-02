package xyz.bitml.api

import java.io.File

import akka.actor.{ActorRef, ActorSystem, CoordinatedShutdown, Props}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import fr.acinq.bitcoin.Crypto.PrivateKey
import xyz.bitml.api.messaging.{Node, Query}
import xyz.bitml.api.persistence.State
import xyz.bitml.api.serialization.Serializer

case class Client (private var state : State = State(), identity : PrivateKey) extends LazyLogging{

  private var msgNode : ActorRef = _
  private var system : ActorSystem = _

  val ser = new Serializer()
  val conv = new SegwitConverter()
  val sig = new Signer()

  // Load a state from an external json string, convert to witness and fill with signatures from our identity.
  def initState(jsonState : String) : Unit = {

    // Load an input state and convert into witness-based equivalent if not already converted
    val tmpState = ser.loadState(jsonState)
    conv.convertTree(tmpState.metadb, tmpState.txdb)

    // Scroll through the meta info and fill in the signatures dependent on our own identity.
    for (t <- tmpState.metadb.dump()) sig.fillEntry(tmpState.txdb.fetch(t._1).get, t._2, identity)

    // Save processed state as this client's new state.
    state = tmpState
  }

  // Start an ActorSystem with a predetermined config and the current state objects.
  def listenMsg(configPath : String, systemName : String): Unit ={
    // Setup network
    val configFile = getClass.getClassLoader.
      getResource(configPath).getFile
    val config = ConfigFactory.parseFile(new File(configFile))
    system = ActorSystem(systemName , config)
    msgNode = system.actorOf(Props(classOf[Node], state.metadb, state.txdb), name = "ContractNode")
  }

  // Stop the current ActorSystem
  def shutdownMsg(): Unit = {
    CoordinatedShutdown(system).run(CoordinatedShutdown.clusterLeavingReason)
  }

  // Ask any participant involved in the tx to share their signatures.
  def retrieveSigs(txName : String): Unit = {
    val partList = txPendingList(txName)
    for (p <- partList) {
      msgNode ! Query(p.endpoint.toString, txName)
    }
  }

  // Retrieve a list of participants whose data is missing to complete a certain tx.
  def txPendingList(txName : String) : Seq[Participant] = {
    val meta = state.metadb.fetch(txName).get
    val pubs = meta.indexData.flatMap(_._2.chunkData.filter(_.data.isEmpty).map(_.owner.get))
    pubs.map(x => state.partdb.fetch(x.toString()).get).toSeq
  }

  // If completable, assemble given tx and return raw serialized form.
  def assembleTx(txName : String) : String = {
    val canComplete = txPendingList(txName)
    if (canComplete.nonEmpty) {
      logger.error("Cannot assemble tx "+txName+": transaction data incomplete. ")
      return ""
    }
    sig.assembleTx(state.txdb.fetch(txName).get, state.metadb.fetch(txName).get).toString()
  }

  // TODO: A bunch of strategy-related logic

}
