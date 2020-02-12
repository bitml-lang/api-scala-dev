package xyz.bitml.api.serialization

import fr.acinq.bitcoin.Transaction
import org.json4s.{CustomSerializer, Extraction, Formats}
import org.json4s.JsonAST.{JField, JObject, JString, JValue}
import xyz.bitml.api.persistence.TxStorage


class TxStorageSerializer extends  CustomSerializer[TxStorage](format =>(
  {
    case JObject(l : List[(String, JValue)]) =>
      implicit  val formats : Formats = org.json4s.DefaultFormats + new TxSerializer
      val f = (name: String, ser : JValue) => (name, Extraction.extract[Transaction](ser))
      new TxStorage(l.map(a => f(a._1, a._2)).toMap)
  },
  {
    case x: TxStorage => {
      implicit  val formats : Formats = org.json4s.DefaultFormats + new TxSerializer
      val x1 = x.dump().toList
      val f = (a : (String, Transaction)) => JField(a._1, Extraction.decompose(a._2))
      val l1 = x1.map(f)
      JObject(l1)
    }
  }
))
