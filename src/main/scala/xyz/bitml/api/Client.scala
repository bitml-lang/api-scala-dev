package xyz.bitml.api

import java.io.File

import akka.actor.{ActorRef, ActorSystem, CoordinatedShutdown, Props}
import com.typesafe.config.ConfigFactory
import fr.acinq.bitcoin.Crypto.PrivateKey
import xyz.bitml.api.messaging.Node
import xyz.bitml.api.persistence.State
import xyz.bitml.api.serialization.Serializer

case class Client (private var state : State = State(), identity : PrivateKey) {

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

  // TODO: Ask any participant involved in the tx to share their signatures.
  def retrieveSigs(txName : String): Unit = {
  }

  // TODO: Verify if a certain tx is completable
  def checkTx(txName : String) : Boolean = {
    false
  }

  // TODO: if completable, assemble given tx and return raw serialized form.
  def assembleTx(txName : String) : String = {
    "placeholder"
  }

  // TODO: A bunch of strategy-related logic

}
