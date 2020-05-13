import akka.actor.{ActorSystem, Address, CoordinatedShutdown, Props}
import akka.util.Timeout
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.{Base58, Base58Check, Crypto, OP_0, OP_PUSHDATA, Satoshi, Script, Transaction}
import akka.pattern.ask
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector
import xyz.bitml.api.{ChunkEntry, ChunkPrivacy, ChunkType, Client, IndexEntry, Participant, TxEntry}
import xyz.bitml.api.messaging.{AssembledTx, Init, Listen, StopListening, TryAssemble}
import xyz.bitml.api.persistence.{MetaStorage, ParticipantStorage, State, TxStorage}
import xyz.bitml.api.serialization.Serializer

import scala.concurrent.duration._
import scala.concurrent.Await

class Test_Publish extends AnyFunSuite with BeforeAndAfterAll{

  private var testSystem : ActorSystem = _

  override def beforeAll(): Unit = {
    testSystem = ActorSystem(name = "internalTestSystem")
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    CoordinatedShutdown(testSystem).run(CoordinatedShutdown.unknownReason)
    Thread.sleep(500)
    super.afterAll()
  }

  test("Regtest: insert input txids, then publish a test TX ") {
    // Generate initial state JSON with our own internal serialization



    val tinit = Transaction.read("02000000027be5fa01cf6465d71c0335c5f34c53a1d2a7b29d567d442e1e98f0e64e15a06a0000000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff9d26302a3e82b2475a6ba09a0e0e0c6cf4626125ff012232180a41415be1e5a00100000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff01b15310000000000017a9147a06737efe61a6d916abdc59b7c099ae570c39ca8700000000")
    val t1 = Transaction.read("020000000164206af5b72934c9b198dce52f65e2a1e3d87110ebe458ddd87e048c0d7b60d4000000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff0180de0f00000000001976a914ce07ee1448bbb80b38ae0c03b6cdeff40ff326ba88ac00000000")
    val txdb = new TxStorage()
    txdb.save("Tinit", tinit)
    txdb.save("T1", t1)

    val a_priv = PrivateKey.fromBase58("cVKvdCzgFKF5F2EYn64ekMYVV3Qn1AuWGf763tCMmV1QLi4wqM8R", Base58.Prefix.SecretKeyTestnet)._1
    val a_pub = a_priv.publicKey
    println(a_pub)

    // Build the necessary P2SH address.
    println(Base58Check.encode(Base58.Prefix.ScriptAddressTestnet, Crypto.hash160(Script.write(OP_0 :: OP_PUSHDATA(a_pub.hash160)  :: Nil))))

    // Outside the test, send 0.01 and 0.001 BTC to the address above.


    // Tinit with patched out inputs (txid and pubkeys) to our regtest input transactions.
    // Balzac refuses to compile if we just add P2SH/P2WPKH tx's directly.
    // This was manually edited. It can also be done with string replacement, or parsing and switching.

    // Old pubkey: 02a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2ab
    // old txa0 id: 6aa0154ee6f0981e2e447d569db2a7d2a1534cf3c535031cd76564cf01fae57b
    // old txfee id: a0e5e15b41410a18322201ff256162f46c0c0e0e9aa06b5a47b2823e2a30269d
    // ...but txids are the hashes with reversed endianness
    // LE old tx1: 7be5fa01cf6465d71c0335c5f34c53a1d2a7b29d567d442e1e98f0e64e15a06a
    // LE old tx2: 9d26302a3e82b2475a6ba09a0e0e0c6cf4626125ff012232180a41415be1e5a0

    //NEW NEW pubkey: 03a0fb3a0a175fa64518762a46d07d1261d66b89d455145a0ec830199420402bd5
    //NEW LE TxA0: f02c9c2420ff08e0cad79634e8e411fe630bba5b56628ceae5952f51afa77ede1//NEW LE TxFee: 1d7c5e6e95e2d35d10ddfa4366694ba9d74f43b6e6365c9509f8b20418196bfd

    val newTinit = Transaction.read("0200000002f02c9c2420ff08e0cad79634e8e411fe630bba5b56628ceae5952f51afa77ede0100000023002103a0fb3a0a175fa64518762a46d07d1261d66b89d455145a0ec830199420402bd5ffffffff1d7c5e6e95e2d35d10ddfa4366694ba9d74f43b6e6365c9509f8b20418196bfd0000000023002103a0fb3a0a175fa64518762a46d07d1261d66b89d455145a0ec830199420402bd5ffffffff01b15310000000000017a9147a06737efe61a6d916abdc59b7c099ae570c39ca8700000000")
    txdb.save("Tinit", newTinit)

    val a_p = Participant("A", List(a_pub), Address("akka", "test", "127.0.0.1", 25000))

    val b_pub = PublicKey(ByteVector.fromValidHex("03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e"))
    val b_p = Participant("B", List(b_pub), Address("akka", "test", "127.0.0.1", 25001))

    val partdb = new ParticipantStorage()
    partdb.save(a_p)
    partdb.save(b_p)

    val tinit0_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2PKH, chunkPrivacy = ChunkPrivacy.AUTH, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty))
    val tinit1_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2PKH, chunkPrivacy = ChunkPrivacy.AUTH, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty))
    val t1_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy = ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy = ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(a_pub), data = ByteVector.empty))
    val tinit_entry = TxEntry(name = "Tinit", indexData = Map(
      0 -> IndexEntry(amt = Satoshi(1000000), chunkData = tinit0_chunks),
      1 -> IndexEntry(amt = Satoshi(100000), chunkData = tinit1_chunks)))
    val t1_entry = TxEntry(name = "T1", indexData = Map(0 -> IndexEntry(amt = Satoshi(1070001), chunkData = t1_chunks)))

    val metadb = new MetaStorage()
    metadb.save(tinit_entry)
    metadb.save(t1_entry)

    val initialState = State(partdb, txdb, metadb)
    val stateJson = new Serializer().prettyPrintState(initialState)
    //println(stateJson)

    // Start an actor
    val alice = testSystem.actorOf(Props[Client])

    // Initialize Alice with the state information.
    alice ! Init(a_priv, stateJson)
    // Start the network interface (even though in this test we won't communicate with B)
    alice ! Listen("test_application.conf", a_p.endpoint.system)

    // AFter letting A initialize, see if it can assemble Tinit on its own
    Thread.sleep(2000)
    implicit val timeout: Timeout = 5 seconds
    val future3 = alice ? TryAssemble("Tinit", autoPublish = false)
    val res3 = Await.result(future3, timeout.duration).asInstanceOf[AssembledTx].serializedTx
    // The node has produced a transaction.
    assert(res3.length != 0)


    println(Transaction.read(res3).txid)
    println(Transaction.read(res3).txIn(0).witness)


    alice ! StopListening()

  }
}