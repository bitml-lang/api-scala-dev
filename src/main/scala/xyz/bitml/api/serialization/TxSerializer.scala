package xyz.bitml.api.serialization

import fr.acinq.bitcoin.Transaction
import org.json4s.CustomSerializer
import org.json4s.JsonAST.{JField, JObject, JString}
import xyz.bitml.api.persistence.TxStorage


class TxSerializer extends  CustomSerializer[TxStorage](format =>(
  {
    case JObject(l : List[(String, JString)]) => new TxStorage(l.map(jf => {jf._1 -> Transaction.read(jf._2.s)}).toMap)
  },
  {
    case x: TxStorage => {
      val x1 = x.dump().toList
      val f = (a : (String, Transaction)) => JField(a._1, JString(Transaction.write(a._2).toHex))
      val l1 = x1.map(f)
      JObject(l1)
    }
  }
))
