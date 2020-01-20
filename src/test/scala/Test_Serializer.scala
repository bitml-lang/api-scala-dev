import fr.acinq.bitcoin.Crypto.PrivateKey
import fr.acinq.bitcoin.{Base58, Transaction}
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector
import xyz.bitml.api.{ChunkEntry, ChunkType, Serializer, TxEntry}

class Test_Serializer extends AnyFunSuite {
  test ("Serialization of TxEntry without any chunk data.") {
    val empty = new TxEntry(name="TEST",
      data =Transaction.read("0100000001b021a77dcaad3a2da6f1611d2403e1298a902af8567c25d6e65073f6b52ef12d000000006a473044022056156e9f0ad7506621bc1eb963f5133d06d7259e27b13fcb2803f39c7787a81c022056325330585e4be39bcf63af8090a2deff265bc29a3fb9b4bf7a31426d9798150121022dfb538041f111bb16402aa83bd6a3771fa8aa0e5e9b0b549674857fafaf4fe0ffffffff0210270000000000001976a91415c23e7f4f919e9ff554ec585cb2a67df952397488ac3c9d1000000000001976a9148982824e057ccc8d4591982df71aa9220236a63888ac00000000"),
      chunks= Map() )
    println(new Serializer().serializeTxEntry(empty))
  }

  test ("Serialization of TxEntry with chunk data.") {

    val privateKey = PrivateKey.fromBase58("cRp4uUnreGMZN8vB7nQFX6XWMHU5Lc73HMAhmcDEwHfbgRS66Cqp", Base58.Prefix.SecretKeyTestnet)._1
    val publicKey = privateKey.publicKey

    val chunks = Map((0, Seq(new ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkIndex = 0, owner = Option(publicKey), data = ByteVector.empty))))
    val empty = new TxEntry(name="TEST",
      data =Transaction.read("0100000001b021a77dcaad3a2da6f1611d2403e1298a902af8567c25d6e65073f6b52ef12d000000006a473044022056156e9f0ad7506621bc1eb963f5133d06d7259e27b13fcb2803f39c7787a81c022056325330585e4be39bcf63af8090a2deff265bc29a3fb9b4bf7a31426d9798150121022dfb538041f111bb16402aa83bd6a3771fa8aa0e5e9b0b549674857fafaf4fe0ffffffff0210270000000000001976a91415c23e7f4f919e9ff554ec585cb2a67df952397488ac3c9d1000000000001976a9148982824e057ccc8d4591982df71aa9220236a63888ac00000000"),
      chunks= chunks )
    val ser = new Serializer()
    println(ser.serializeTxEntry(empty))

  }

  test ("Deserialization of JSON string with no chunk data") {
    println(new Serializer().deserializeTxEntry("""{"name":"TEST","data":{"version":1,"txIn":[{"outPoint":{"hash":{"bytes":{"bytes":{"at":{"arr":[-80,33,-89,125,-54,-83,58,45,-90,-15,97,29,36,3,-31,41,-118,-112,42,-8,86,124,37,-42,-26,80,115,-10,-75,46,-15,45]},"offset":0,"size":32}}},"index":0},"signatureScript":{"bytes":{"at":{"arr":[71,48,68,2,32,86,21,110,-97,10,-41,80,102,33,-68,30,-71,99,-11,19,61,6,-41,37,-98,39,-79,63,-53,40,3,-13,-100,119,-121,-88,28,2,32,86,50,83,48,88,94,75,-29,-101,-49,99,-81,-128,-112,-94,-34,-1,38,91,-62,-102,63,-71,-76,-65,122,49,66,109,-105,-104,21,1,33,2,45,-5,83,-128,65,-15,17,-69,22,64,42,-88,59,-42,-93,119,31,-88,-86,14,94,-101,11,84,-106,116,-123,127,-81,-81,79,-32]},"offset":0,"size":106}},"sequence":4294967295,"witness":{"stack":[]}}],"txOut":[{"amount":{},"publicKeyScript":{"bytes":{"at":{"arr":[118,-87,20,21,-62,62,127,79,-111,-98,-97,-11,84,-20,88,92,-78,-90,125,-7,82,57,116,-120,-84]},"offset":0,"size":25}}},{"amount":{},"publicKeyScript":{"bytes":{"at":{"arr":[118,-87,20,-119,-126,-126,78,5,124,-52,-115,69,-111,-104,45,-9,26,-87,34,2,54,-90,56,-120,-84]},"offset":0,"size":25}}}],"lockTime":0},"chunks":{}}"""))
  }

  test ("Deserialization of JSON string with chunk data") {
    println(new Serializer().deserializeTxEntry("""{"name":"TEST","data":{"version":1,"txIn":[{"outPoint":{"hash":{"bytes":{"bytes":{"at":{"arr":[-80,33,-89,125,-54,-83,58,45,-90,-15,97,29,36,3,-31,41,-118,-112,42,-8,86,124,37,-42,-26,80,115,-10,-75,46,-15,45]},"offset":0,"size":32}}},"index":0},"signatureScript":{"bytes":{"at":{"arr":[71,48,68,2,32,86,21,110,-97,10,-41,80,102,33,-68,30,-71,99,-11,19,61,6,-41,37,-98,39,-79,63,-53,40,3,-13,-100,119,-121,-88,28,2,32,86,50,83,48,88,94,75,-29,-101,-49,99,-81,-128,-112,-94,-34,-1,38,91,-62,-102,63,-71,-76,-65,122,49,66,109,-105,-104,21,1,33,2,45,-5,83,-128,65,-15,17,-69,22,64,42,-88,59,-42,-93,119,31,-88,-86,14,94,-101,11,84,-106,116,-123,127,-81,-81,79,-32]},"offset":0,"size":106}},"sequence":4294967295,"witness":{"stack":[]}}],"txOut":[{"amount":{},"publicKeyScript":{"bytes":{"at":{"arr":[118,-87,20,21,-62,62,127,79,-111,-98,-97,-11,84,-20,88,92,-78,-90,125,-7,82,57,116,-120,-84]},"offset":0,"size":25}}},{"amount":{},"publicKeyScript":{"bytes":{"at":{"arr":[118,-87,20,-119,-126,-126,78,5,124,-52,-115,69,-111,-104,45,-9,26,-87,34,2,54,-90,56,-120,-84]},"offset":0,"size":25}}}],"lockTime":0},"chunks":{"0":[{"chunkType":1,"chunkIndex":0,"owner":{"value":{"bytes":{"at":{"arr":[3,20,77,67,78,-123,20,13,65,9,-127,74,-57,-124,-111,-1,-22,-29,-124,-63,-114,34,37,-70,16,-102,-46,95,-16,-28,110,-17,101]},"offset":0,"size":33}}},"data":{"bytes":{"at":{},"offset":0,"size":0}}}]}}"""))
  }

}
