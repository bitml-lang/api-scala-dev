import akka.actor.Address
import fr.acinq.bitcoin.Crypto.PrivateKey
import fr.acinq.bitcoin.{Base58, Btc, Satoshi, Transaction}
import org.json4s.Formats
import org.json4s.native.Serialization
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector
import xyz.bitml.api.{ChunkEntry, ChunkPrivacy, ChunkType, IndexEntry, Participant, TxEntry}
import xyz.bitml.api.persistence.{MetaStorage, ParticipantStorage, State, TxStorage}
import xyz.bitml.api.serialization.{ByteVectorSerializer, MetaStorageSerializer, PartStorageSerializer, SatoshiSerializer, Serializer, StateSerializer, TxStorageSerializer}

import scala.collection.immutable.HashMap

class Test_Serializer extends AnyFunSuite {

  test ("Serialization and deserialization of ByteVector with custom serializer") {
    val testarr = Array.apply(1.toByte, 2.toByte, 3.toByte)
    val testVector = ByteVector(testarr)

    implicit val formats: Formats = org.json4s.DefaultFormats + new ByteVectorSerializer()
    val serialized = Serialization.write(testVector)
    val resultCheck = Serialization.read[ByteVector](serialized)
    assert(testVector.equals(resultCheck))
  }

  test ("Serialization and deserialization of Satoshi with custom serializer") {
    val testSat = Satoshi(1000)

    implicit val formats: Formats = org.json4s.DefaultFormats + new SatoshiSerializer()
    val resultCheck = Serialization.read[Satoshi](Serialization.write(testSat))
    assert(testSat.equals(resultCheck))
  }

  test ("Serialization and deserialization of TxStorage") {
    val tx = Transaction.read("0100000001b021a77dcaad3a2da6f1611d2403e1298a902af8567c25d6e65073f6b52ef12d000000006a473044022056156e9f0ad7506621bc1eb963f5133d06d7259e27b13fcb2803f39c7787a81c022056325330585e4be39bcf63af8090a2deff265bc29a3fb9b4bf7a31426d9798150121022dfb538041f111bb16402aa83bd6a3771fa8aa0e5e9b0b549674857fafaf4fe0ffffffff0210270000000000001976a91415c23e7f4f919e9ff554ec585cb2a67df952397488ac3c9d1000000000001976a9148982824e057ccc8d4591982df71aa9220236a63888ac00000000")
    val testStorage = new TxStorage(HashMap("TEST" -> tx, "TEST2" -> tx))


    implicit val formats: Formats = org.json4s.DefaultFormats + new TxStorageSerializer()
    val written = Serialization.write(testStorage)
    val resultCheck = Serialization.read[TxStorage](written)
    assert(testStorage.equals(resultCheck))
  }

  test ("Serialization/Deserialization of ParticipantStorage") {
    val priv1 = PrivateKey.fromBase58("QRY5zPUH6tWhQr2NwFXNpMbiLQq9u2ztcSZ6RwMPjyKv36rHP2xT", Base58.Prefix.SecretKeySegnet)._1
    val pub1 = priv1.publicKey
    val p = new Participant(name = "test", pubkey = List(pub1), endpoint = Address("akka", "test", "10.11.12.13", 1234) )
    val pstor = new ParticipantStorage(inMemoryDb = HashMap(pub1.toString() -> p))

    implicit val formats : Formats = org.json4s.DefaultFormats + new PartStorageSerializer
    val written = Serialization.write(pstor)
    val resultCheck = Serialization.read[ParticipantStorage](written)
    assert(pstor.equals(resultCheck))
  }

  test ("Serialization/Deserialization of TxEntry without any chunk data.") {

    val txdb = new TxStorage(inMemoryDb = new HashMap[String,Transaction]())
    txdb.save("TEST",Transaction.read("0100000001b021a77dcaad3a2da6f1611d2403e1298a902af8567c25d6e65073f6b52ef12d000000006a473044022056156e9f0ad7506621bc1eb963f5133d06d7259e27b13fcb2803f39c7787a81c022056325330585e4be39bcf63af8090a2deff265bc29a3fb9b4bf7a31426d9798150121022dfb538041f111bb16402aa83bd6a3771fa8aa0e5e9b0b549674857fafaf4fe0ffffffff0210270000000000001976a91415c23e7f4f919e9ff554ec585cb2a67df952397488ac3c9d1000000000001976a9148982824e057ccc8d4591982df71aa9220236a63888ac00000000"))

    val empty = new TxEntry(name="TEST",
      indexData= Map() )
    val ser = new Serializer()

    val serialized = ser.serializeTxEntry(empty)
    val deserializeCheck = ser.deserializeTxEntry(serialized)
    assert(deserializeCheck.equals(empty))
  }

  test ("Serialization/Deserialization of TxEntry with chunk data.") {

    val privateKey = PrivateKey.fromBase58("cRp4uUnreGMZN8vB7nQFX6XWMHU5Lc73HMAhmcDEwHfbgRS66Cqp", Base58.Prefix.SecretKeyTestnet)._1
    val publicKey = privateKey.publicKey

    val txdb = new TxStorage(inMemoryDb = new HashMap[String,Transaction]())
    txdb.save("TEST",Transaction.read("0100000001b021a77dcaad3a2da6f1611d2403e1298a902af8567c25d6e65073f6b52ef12d000000006a473044022056156e9f0ad7506621bc1eb963f5133d06d7259e27b13fcb2803f39c7787a81c022056325330585e4be39bcf63af8090a2deff265bc29a3fb9b4bf7a31426d9798150121022dfb538041f111bb16402aa83bd6a3771fa8aa0e5e9b0b549674857fafaf4fe0ffffffff0210270000000000001976a91415c23e7f4f919e9ff554ec585cb2a67df952397488ac3c9d1000000000001976a9148982824e057ccc8d4591982df71aa9220236a63888ac00000000"))

    val chunks =  Seq(new ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(publicKey), data = ByteVector.empty))
    val indexInfo = new IndexEntry(Satoshi(0), chunks)

    val empty = new TxEntry(name="TEST",
      indexData= HashMap(0 -> indexInfo))
    val ser = new Serializer()

    val serialized = ser.serializeTxEntry(empty)
    val deserializeCheck = ser.deserializeTxEntry(serialized)
    println(deserializeCheck)
    assert(deserializeCheck.equals(empty))
  }

  test ("Serialization/Deserialization of MetaStorage.") {

    val privateKey = PrivateKey.fromBase58("cRp4uUnreGMZN8vB7nQFX6XWMHU5Lc73HMAhmcDEwHfbgRS66Cqp", Base58.Prefix.SecretKeyTestnet)._1
    val publicKey = privateKey.publicKey

    val chunks =  Seq(new ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(publicKey), data = ByteVector.empty))
    val indexInfo = new IndexEntry(Satoshi(0), chunks)
    val empty = new TxEntry(name="TEST",
      indexData= HashMap(0 -> indexInfo))
    val meta = new MetaStorage(HashMap(empty.name -> empty))

    implicit val formats : Formats = org.json4s.DefaultFormats + new MetaStorageSerializer

    val serialized = Serialization.write(meta)
    val deserializeCheck = Serialization.read[MetaStorage](serialized)
    assert(deserializeCheck.equals(meta))
  }

  test ("Client state serialization/deserialization") {

    //Dummy data

    // Setup part storage
    val privateKey = PrivateKey.fromBase58("cRp4uUnreGMZN8vB7nQFX6XWMHU5Lc73HMAhmcDEwHfbgRS66Cqp", Base58.Prefix.SecretKeyTestnet)._1
    val publicKey = privateKey.publicKey
    val dummyEndpoint = new Address(protocol = "akka", system = "TestA", host = "127.0.0.1", port = 25520)
    val part1 = Participant("A", List(publicKey), dummyEndpoint)
    val partdb = new ParticipantStorage()
    partdb.save(part1)

    // Setup tx storage
    val balzac_t_blank = Transaction.read("02000000013b4bb256e1b6f045b778016c5c63d4b082deab49f9b6067ffcae209dbdc4505d00000000060004766b5187ffffffff0100ca9a3b0000000017a91453c3f130b2e0f8d9a3a5b6aaf71804543076d4568700000000")
    val balzac_t1_blank = Transaction.read("0200000001d758caddefba9ea104643d0cf7acad94c880c29162e963ecb421e3c127cefd9000000000070005766b012a87ffffffff0100ca9a3b0000000017a9146e99e54647eb6588d8a9de2be4d2dd016a1a741a8700000000")
    val txdb = new TxStorage()
    txdb.save("t", balzac_t_blank)
    txdb.save("t1", balzac_t1_blank)

    // Setup meta storage
    val t_chunks = Seq(ChunkEntry(chunkType = ChunkType.OTHER, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option.empty, data = ByteVector(1)))
    val t1_chunks = Seq(ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.AUTH, chunkIndex = 0, owner = Option.empty, data = ByteVector(42))) // This will be zeroed out on serialization
    val t_entry = new TxEntry(name = "t", indexData = Map(0 -> IndexEntry(amt = Btc(10).toSatoshi ,chunkData = t_chunks)))
    val t1_entry = new TxEntry(name = "t1", indexData = Map(0 -> IndexEntry(amt = Btc(10).toSatoshi ,chunkData = t1_chunks)))
    val metadb = new MetaStorage()
    metadb.save(t_entry)
    metadb.save(t1_entry)

    // Create state
    val state = State(partdb, txdb, metadb)

    implicit val formats : Formats = org.json4s.DefaultFormats + new StateSerializer

    val serialized = Serialization.writePretty(state)
    println(serialized)
    val deserializeCheck = Serialization.read[State](serialized)
    assert(deserializeCheck.partdb.equals(state.partdb))
    assert(deserializeCheck.txdb.equals(state.txdb))
    assert(deserializeCheck.metadb.fetch("t").equals(state.metadb.fetch("t")))
    assert(!deserializeCheck.metadb.fetch("t1").equals(state.metadb.fetch("t1")))

  }
}
