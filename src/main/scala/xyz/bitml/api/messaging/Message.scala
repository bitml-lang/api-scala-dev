package xyz.bitml.api.messaging

import akka.actor.{ActorRef, Address}

sealed trait Event

// TODO: flag these 2 as PossiblyHarmful once we activate untrusted-mode
case class Heartbeat(endpoint : String) extends Event
case class Query(endpoint : String, txMeta: String) extends Event

case class Ping() extends Event
case class Pong() extends Event
case class Request(txMeta: String) extends Event
case class Data(serializedTx: String) extends Event