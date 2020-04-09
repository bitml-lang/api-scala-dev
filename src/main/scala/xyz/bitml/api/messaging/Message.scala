package xyz.bitml.api.messaging

import akka.actor.PossiblyHarmful

sealed trait Event

case class Ping() extends Event
case class Pong() extends Event
case class Request(txMeta: String) extends Event
case class Data(serializedTx: String) extends Event

sealed trait Internal extends PossiblyHarmful

case class Heartbeat(endpoint : String) extends Internal
case class Query(endpoint : String, txMeta: String) extends Internal

case class Init(jsonState: String) extends Internal
case class Listen(config: String, systemName: String) extends Internal
case class StopListening() extends Internal
case class DumpState() extends Internal

case class TryAssemble(txName: String) extends Internal
case class AskForSigs(txName: String) extends Internal

case class preInit() extends Internal
case class askAuth(txName: String) extends Internal
case class grantAuth(txName: String) extends Internal

sealed trait Response extends PossiblyHarmful

case class AssembledTx(txName: String, serializedTx : String) extends Response
case class CurrentState(state: String) extends Response