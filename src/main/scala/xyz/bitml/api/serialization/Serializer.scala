package xyz.bitml.api.serialization

import org.json4s.Formats
import org.json4s.native.Serialization
import xyz.bitml.api.ChunkPrivacy.ChunkPrivacy
import xyz.bitml.api.{ChunkPrivacy, TxEntry}
import xyz.bitml.api.persistence.State

class Serializer(secrecy : ChunkPrivacy = ChunkPrivacy.PUBLIC) {

  implicit val formats: Formats = org.json4s.DefaultFormats + new ChunkEntrySerializer(secrecy) + new SatoshiSerializer + new StateSerializer(secrecy)

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
