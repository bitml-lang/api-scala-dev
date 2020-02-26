package xyz.bitml.api.serialization

import org.json4s.CustomSerializer
import org.json4s.JsonAST.{JField, JObject, JString}
import scodec.bits.ByteVector

class ByteVectorSerializer extends CustomSerializer[ByteVector](format=>(
  {
    case JObject(List(JField("hex",JString(a: String)))) => ByteVector.fromValidHex(a)
  },
  {
    case x: ByteVector => JObject(List(JField("hex", JString(x.toHex))))
  }
))