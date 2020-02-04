package xyz.bitml.api.messaging

import akka.actor.{Actor, Address, Props}
import xyz.bitml.api.persistence.MetaStorage
import xyz.bitml.api.serialization.Serializer



class Node  (val metaStorage : MetaStorage) extends Actor{

  override def receive: Receive = {
    case Heartbeat(endpoint) => heartbeat(endpoint)
    case Query(endpoint, txMeta) => remoteQuery(endpoint, txMeta)
    case Ping() => sender() ! Pong()
    case Pong() => println("Heartbeat response from " + sender().toString())
    case Request(txMeta) =>
      // try to retrieve the appropriate TxEntry from the storage associated with this server
      val res = metaStorage.fetch(txMeta)
      if (res.nonEmpty) {
        val ser = new Serializer
        sender() ! Data(ser.serializeTxEntry(res.get))
      } else {
        // TODO: proper logging here and in Signer
        println("Unable to find data for tx "+txMeta+" requested by remote actor "+sender().toString())
      }

    case Data(serializedTx) =>
      val ser = new Serializer
      val resData = ser.deserializeTxEntry(serializedTx)
      // TODO: verify signatures and save any previously unknown data into our own TxEntry and back into MetaStorage

    case _ => println("placeholder " + sender().toString())
  }

  // Send a ping towards an expected remote endpoint. If a server is running, our server will receive a Pong()
  def heartbeat(endpoint : String): Unit = {
    val addr = endpoint + context.self.path.toStringWithoutAddress
    println(addr)
    context.actorSelection(addr) ! Ping()
  }

  // Send a request for a remote actor's version of a certain TxEntry. If the remote actor is listening, our server will receive a Data()
  def remoteQuery(endpoint : String, txEntry : String): Unit = {
    val addr = endpoint + context.self.path.toStringWithoutAddress
    println(addr)

    context.actorSelection(addr) ! Request(txEntry)
  }
}
