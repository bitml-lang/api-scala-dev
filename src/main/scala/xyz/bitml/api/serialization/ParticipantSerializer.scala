package xyz.bitml.api.serialization

import akka.actor.Address
import fr.acinq.bitcoin.Crypto.PublicKey
import org.json4s.JsonAST.{JField, JObject, JString, JValue}
import org.json4s.{CustomSerializer, Extraction, Formats}
import scodec.bits.ByteVector
import xyz.bitml.api.Participant

class ParticipantSerializer extends CustomSerializer[Participant](format => (

  {
    case JObject(List(JField("name", name:JString), JField("pubkey", pubkey: JString), JField("endpoint", endpoint : JValue))) =>{
      implicit val formats : Formats = org.json4s.DefaultFormats + new AddressSerializer

      new Participant(name.s, PublicKey(ByteVector.fromValidHex(pubkey.s)), Extraction.extract[Address](endpoint))
    }

  },{
  case x : Participant => {
    implicit val formats : Formats = org.json4s.DefaultFormats + new AddressSerializer

    JObject(List(
      JField("name", JString(x.name)),
      JField("pubkey" , JString(x.pubkey.toString())),
      JField("endpoint", Extraction.decompose(x.endpoint))))
  }
}
))

