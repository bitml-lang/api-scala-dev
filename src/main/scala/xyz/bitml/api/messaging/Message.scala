package xyz.bitml.api.messaging

import akka.actor.PossiblyHarmful
import fr.acinq.bitcoin.Crypto.PrivateKey

sealed trait Event

case class Ping() extends Event
case class Pong() extends Event
case class Request(txMeta: String) extends Event
case class Data(serializedTx: String) extends Event

sealed trait Internal extends PossiblyHarmful

case class Heartbeat(endpoint : String) extends Internal
case class Query(endpoint : String, txMeta: String) extends Internal

case class Init(identity: PrivateKey, jsonState: String) extends Internal
case class Listen(config: String, systemName: String) extends Internal
case class StopListening() extends Internal
case class DumpState() extends Internal

case class TryAssemble(txName: String, autoPublish : Boolean = false) extends Internal
case class AskForSigs(txName: String) extends Internal

case class PreInit() extends Internal
case class Authorize(txName: String) extends Internal

sealed trait Response extends PossiblyHarmful

case class AssembledTx(txName: String, serializedTx : String) extends Response
case class CurrentState(state: String) extends Response