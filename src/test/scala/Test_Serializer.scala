import fr.acinq.bitcoin.Crypto.PrivateKey
import fr.acinq.bitcoin.{Base58, Transaction}
import org.json4s.Formats
import org.json4s.native.Serialization
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector
import xyz.bitml.api.{ByteVectorSerializer, ChunkEntry, ChunkType, Serializer, TxEntry, TxStorage}

import scala.collection.immutable.HashMap

class Test_Serializer extends AnyFunSuite {

  test ("Serialization and deserialization of ByteVector with custom serializer") {
    val testarr = Array.apply(1.toByte, 2.toByte, 3.toByte)
    val testVector = ByteVector.apply(testarr)

    implicit val formats: Formats = org.json4s.DefaultFormats + new ByteVectorSerializer()
    val resultCheck = Serialization.read[ByteVector](Serialization.write(testVector))
    assert(testVector.equals(resultCheck))
  }

  test ("Serialization/Deserialization of TxEntry without any chunk data.") {

    val txdb = new TxStorage(inMemoryDb = new HashMap[String,Transaction]())
    txdb.save("TEST",Transaction.read("0100000001b021a77dcaad3a2da6f1611d2403e1298a902af8567c25d6e65073f6b52ef12d000000006a473044022056156e9f0ad7506621bc1eb963f5133d06d7259e27b13fcb2803f39c7787a81c022056325330585e4be39bcf63af8090a2deff265bc29a3fb9b4bf7a31426d9798150121022dfb538041f111bb16402aa83bd6a3771fa8aa0e5e9b0b549674857fafaf4fe0ffffffff0210270000000000001976a91415c23e7f4f919e9ff554ec585cb2a67df952397488ac3c9d1000000000001976a9148982824e057ccc8d4591982df71aa9220236a63888ac00000000"))

    val empty = new TxEntry(name="TEST",
      chunks= Map() )
    val ser = new Serializer()

    val serialized = ser.serializeTxEntry(empty)
    println(serialized)
    val deserializeCheck = ser.deserializeTxEntry(serialized)
    assert(deserializeCheck.equals(empty))
  }

  test ("Serialization/Deserialization of TxEntry with chunk data.") {

    val privateKey = PrivateKey.fromBase58("cRp4uUnreGMZN8vB7nQFX6XWMHU5Lc73HMAhmcDEwHfbgRS66Cqp", Base58.Prefix.SecretKeyTestnet)._1
    val publicKey = privateKey.publicKey

    val txdb = new TxStorage(inMemoryDb = new HashMap[String,Transaction]())
    txdb.save("TEST",Transaction.read("0100000001b021a77dcaad3a2da6f1611d2403e1298a902af8567c25d6e65073f6b52ef12d000000006a473044022056156e9f0ad7506621bc1eb963f5133d06d7259e27b13fcb2803f39c7787a81c022056325330585e4be39bcf63af8090a2deff265bc29a3fb9b4bf7a31426d9798150121022dfb538041f111bb16402aa83bd6a3771fa8aa0e5e9b0b549674857fafaf4fe0ffffffff0210270000000000001976a91415c23e7f4f919e9ff554ec585cb2a67df952397488ac3c9d1000000000001976a9148982824e057ccc8d4591982df71aa9220236a63888ac00000000"))

    val chunks = Map((0, Seq(new ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkIndex = 0, owner = Option(publicKey), data = ByteVector.empty))))

    val empty = new TxEntry(name="TEST",
      chunks= chunks )
    val ser = new Serializer()

    val serialized = ser.serializeTxEntry(empty)
    println(serialized)
    val deserializeCheck = ser.deserializeTxEntry(serialized)
    assert(deserializeCheck.equals(empty))
  }
}
