package xyz.bitml.api.serialization

import org.json4s.Formats
import org.json4s.native.Serialization
import xyz.bitml.api.TxEntry
import xyz.bitml.api.persistence.State

class Serializer {

  implicit val formats: Formats = org.json4s.DefaultFormats + new ChunkEntrySerializer + new SatoshiSerializer + new StateSerializer

  def serializeTxEntry(entry: TxEntry): String = {
    Serialization.write(entry)
  }

  def deserializeTxEntry(jsonStr : String): TxEntry = {
    Serialization.read[TxEntry](jsonStr)
  }

  def loadState(jsonState : String): State = {
    Serialization.read[State](jsonState)
  }

  def saveState(state : State): String = {
    Serialization.write(state)
  }

  def prettyPrintState(state : State): String = {
    Serialization.writePretty(state)
  }

}
