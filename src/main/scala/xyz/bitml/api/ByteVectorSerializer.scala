package xyz.bitml.api

import org.json4s.CustomSerializer
import org.json4s.JsonAST.{JArray, JField, JInt, JObject}
import scodec.bits.ByteVector

class ByteVectorSerializer extends CustomSerializer[ByteVector](format=>(
  {
    case JObject(List(JField("arr", JArray(a: List[JInt])))) => ByteVector.apply(a.map(i => i.num.toByte))
  },
  {
    case x: ByteVector => JObject(List(JField(name = "arr", value = JArray(x.toArray.map(b => JInt(b.toInt)).toList))))
  }
))