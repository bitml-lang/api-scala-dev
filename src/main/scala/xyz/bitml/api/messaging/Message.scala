package xyz.bitml.api.messaging

import akka.actor.PossiblyHarmful

sealed trait Event

case class Heartbeat(endpoint : String) extends Event with PossiblyHarmful
case class Query(endpoint : String, txMeta: String) extends Event with PossiblyHarmful

case class Ping() extends Event
case class Pong() extends Event
case class Request(txMeta: String) extends Event
case class Data(serializedTx: String) extends Event