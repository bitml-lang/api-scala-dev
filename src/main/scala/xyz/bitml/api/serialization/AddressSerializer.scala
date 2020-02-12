package xyz.bitml.api.serialization

import akka.actor.Address
import org.json4s.CustomSerializer
import org.json4s.JsonAST.{JField, JInt, JObject, JString}

class AddressSerializer extends CustomSerializer[Address](format =>(
  {
    case JObject(List(
    JField("protocol", prot : JString),
    JField("host", host : JString),
    JField("port", port : JInt),
    JField("system", sys: JString)
    )) => Address(prot.s, sys.s, host.s match {
      case "" => Option.empty
      case x => Option(x)
    }, port.num.toInt match {
      case 0 => Option.empty
      case x => Option(x)
    })
  },
  {
    case x : Address => JObject(List(
      JField("protocol", JString(x.protocol)),
      JField("host", JString(x.host.getOrElse(""))),
      JField("port", JInt(BigInt(x.port.getOrElse(0)))),
      JField("system", JString(x.system))
    ))
  }
))
