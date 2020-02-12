package xyz.bitml.api.serialization

import fr.acinq.bitcoin.Transaction
import org.json4s.CustomSerializer
import org.json4s.JsonAST.JString

class TxSerializer extends CustomSerializer[Transaction](format => (
  {
    case hex : JString => Transaction.read(hex.s)
  },
  {
    case x : Transaction => JString(Transaction.write(x).toHex)
}
))