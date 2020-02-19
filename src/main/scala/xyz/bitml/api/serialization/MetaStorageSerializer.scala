package xyz.bitml.api.serialization

import org.json4s.{CustomSerializer, Extraction, Formats}
import org.json4s.JsonAST.{JField, JObject, JValue}
import xyz.bitml.api.{ChunkType, TxEntry}
import xyz.bitml.api.persistence.MetaStorage


class MetaStorageSerializer extends CustomSerializer[MetaStorage](format =>(

  {
    case JObject(l : List[(String, JValue)]) =>
      implicit val formats: Formats = org.json4s.DefaultFormats + new org.json4s.ext.EnumSerializer(ChunkType) + new ByteVectorSerializer() + new SatoshiSerializer()
      val l1 = l.map(a => (a._1, Extraction.extract[TxEntry](a._2)))
      new MetaStorage(l1.toMap)
  },
  {
    case x: MetaStorage => {
      implicit val formats: Formats = org.json4s.DefaultFormats + new org.json4s.ext.EnumSerializer(ChunkType) + new ByteVectorSerializer() + new SatoshiSerializer()
      val x1 = x.dump().toList
      val f = (a : (String, TxEntry)) => JField(a._1, Extraction.decompose(a._2))
      val l1 = x1.map(f)
      JObject(l1)
    }
  }
))