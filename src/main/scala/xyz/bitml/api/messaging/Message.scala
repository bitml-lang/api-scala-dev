package xyz.bitml.api.messaging

import akka.actor.{ActorRef, Address}

sealed trait Event
case class Ping() extends Event
case class Pong() extends Event
case class Request(txMeta: String) extends Event
case class Data(serializedTx: String) extends Event