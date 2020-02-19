package xyz.bitml.api.serialization

import org.json4s.JsonAST.{JField, JObject, JValue}
import org.json4s.{CustomSerializer, Extraction, Formats}
import xyz.bitml.api.Participant
import xyz.bitml.api.persistence.ParticipantStorage

class PartStorageSerializer extends CustomSerializer[ParticipantStorage](format =>(
  {
    case JObject(l : List[(String, JValue)]) => {

      implicit val formats : Formats = org.json4s.DefaultFormats + new ParticipantSerializer()
      val f = ((s : String, v : JValue) => (s, Extraction.extract[Participant](v)))
      val l1 = l.map(a => f(a._1, a._2)).toMap
      new ParticipantStorage(l1)
    }
  },
  {
    case x : ParticipantStorage =>

      implicit val formats : Formats = org.json4s.DefaultFormats + new ParticipantSerializer()
      val x1 = x.dump().toList
      val f = (a : (String, Participant)) => JField(a._1, Extraction.decompose(a._2))
      val l1 = x1.map(a => f(a))
      JObject(l1)
  }
))
