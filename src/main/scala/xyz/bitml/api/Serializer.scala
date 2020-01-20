package xyz.bitml.api

import org.json4s.Formats
import org.json4s.native.Serialization

class Serializer {

  implicit val formats: Formats = org.json4s.DefaultFormats + new org.json4s.ext.EnumSerializer(ChunkType)

  def serializeTxEntry(entry: TxEntry): String = {
    Serialization.write(entry)
  }

  def deserializeTxEntry(JsonStr : String): TxEntry = {
    Serialization.read[TxEntry](JsonStr)
  }

}
