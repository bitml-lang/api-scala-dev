package xyz.bitml.api.messaging

import akka.actor.Actor
import com.typesafe.scalalogging.LazyLogging
import xyz.bitml.api.persistence.{MetaStorage, TxStorage}
import xyz.bitml.api.serialization.Serializer


class Node  (val metaStorage : MetaStorage, val txStorage : TxStorage) extends Actor with LazyLogging{

  override def receive: Receive = {
    case Heartbeat(endpoint) => heartbeat(endpoint)
    case Query(endpoint, txMeta) => remoteQuery(endpoint, txMeta)
    case Ping() => sender() ! Pong()
    case Pong() => logger.info("Heartbeat response from " + sender().toString())
    case Request(txMeta) =>
      // try to retrieve the appropriate TxEntry from the storage associated with this server
      val res = metaStorage.fetch(txMeta)
      if (res.nonEmpty) {
        val ser = new Serializer
        sender() ! Data(ser.serializeTxEntry(res.get))
      } else {
        logger.error("Unable to find data for tx "+txMeta+" requested by remote actor "+sender().toString())
      }

    case Data(serializedTx) =>
      val ser = new Serializer
      val resData = ser.deserializeTxEntry(serializedTx)
      val txName = resData.name
      val baseTx = txStorage.fetch(txName)
      if (baseTx.nonEmpty) {
        metaStorage.update(resData ,baseTx.get)
      }else{
        logger.error("Unable to find tx "+txName+" referenced by remote actor "+sender().toString())
      }
    case Authorize(txName) => null // TODO: Auth handling
    case _ => logger.warn("Unexpected message from " + sender().toString())
  }

  // Send a ping towards an expected remote endpoint. If a server is running, our server will receive a Pong()
  def heartbeat(endpoint : String): Unit = {
    val addr = endpoint + context.self.path.toStringWithoutAddress
    logger.debug("Sending ping to "+addr)
    context.actorSelection(addr) ! Ping()
  }

  // Send a request for a remote actor's version of a certain TxEntry. If the remote actor is listening, our server will receive a Data()
  def remoteQuery(endpoint : String, txEntry : String): Unit = {
    val addr = endpoint + context.self.path.toStringWithoutAddress
    logger.debug("Asking " + addr + " for info on tx " +  txEntry)
    context.actorSelection(addr) ! Request(txEntry)
  }
}
