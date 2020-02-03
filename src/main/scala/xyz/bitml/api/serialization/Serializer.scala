package xyz.bitml.api.serialization

import org.json4s.Formats
import org.json4s.native.Serialization
import xyz.bitml.api.{ChunkType, TxEntry}

class Serializer {

  implicit val formats: Formats = org.json4s.DefaultFormats + new org.json4s.ext.EnumSerializer(ChunkType) + new ByteVectorSerializer() + new SatoshiSerializer()

  def serializeTxEntry(entry: TxEntry): String = {
    Serialization.write(entry)
  }

  def deserializeTxEntry(JsonStr : String): TxEntry = {
    Serialization.read[TxEntry](JsonStr)
  }

}
