package xyz.bitml.api.serialization

import fr.acinq.bitcoin.Crypto.PublicKey
import org.json4s.JsonAST.{JField, JNull, JObject}
import org.json4s.{CustomSerializer, Extraction, Formats}
import scodec.bits.ByteVector
import xyz.bitml.api.ChunkType.ChunkType
import xyz.bitml.api.{ChunkEntry, ChunkType}


class ChunkEntrySerializer extends CustomSerializer[ChunkEntry](format =>(
  {
    case JObject(l : List[JField])=>
      implicit val formats: Formats = org.json4s.DefaultFormats + new org.json4s.ext.EnumSerializer(ChunkType) + new ByteVectorSerializer
      val lm = l.toMap
      val desType = Extraction.extract[ChunkType](lm("type"))
      val desPos = Extraction.extract[Int](lm("index"))
      val desOwn = Extraction.extractOpt[PublicKey](lm.getOrElse("owner", JNull))
      val desVal = Extraction.extract[ByteVector](lm("data"))
      ChunkEntry(desType, desPos, desOwn, desVal)
  },
  {
    case x: ChunkEntry => {
      val secretive = true
      implicit val formats: Formats = org.json4s.DefaultFormats + new org.json4s.ext.EnumSerializer(ChunkType) + new ByteVectorSerializer

      val serType = JField("type", Extraction.decompose(x.chunkType))
      val serPos = JField("index", Extraction.decompose(x.chunkIndex))
      val serOwn = JField("owner", Extraction.decompose(x.owner))
      val serVal = JField("data", if (secretive && (x.chunkType == ChunkType.SECRET_IN || x.chunkType == ChunkType.SECRET_WIT))
        Extraction.decompose(ByteVector.empty) else Extraction.decompose(x.data))
      val obj = List(serType, serPos, serOwn, serVal)
      JObject(obj)
    }
  }
))