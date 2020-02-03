package xyz.bitml.api.serialization

import fr.acinq.bitcoin.Satoshi
import org.json4s.CustomSerializer
import org.json4s.JsonAST.{JField, JInt, JObject}

class SatoshiSerializer extends CustomSerializer[Satoshi] (format =>(
  {
    case JObject(List(JField("sat", JInt(x)))) => Satoshi(x.toLong)
  },
  {
    case x: Satoshi => JObject(List(JField("sat", JInt(x.toLong))))
  }
))
