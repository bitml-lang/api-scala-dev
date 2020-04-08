
import java.io.File

import scala.concurrent.duration._
import akka.testkit.javadsl.TestKit
import akka.pattern.ask
import akka.actor.{ActorRef, ActorSystem, Address, CoordinatedShutdown, Props}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import fr.acinq.bitcoin.Crypto.PrivateKey
import fr.acinq.bitcoin.{Base58, Btc, Transaction}
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll}
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector
import xyz.bitml.api.{ChunkEntry, ChunkPrivacy, ChunkType, IndexEntry, Signer, TxEntry}
import xyz.bitml.api.messaging.{Heartbeat, Node, Ping, Pong, Query}
import xyz.bitml.api.persistence.{MetaStorage, TxStorage}

import scala.collection.immutable.HashMap
import scala.concurrent.Await


class Test_Network extends AnyFunSuite with BeforeAndAfterAll {

  var nodeA: ActorRef = _
  var nodeB: ActorRef = _
  var remoteEndpointA : Address = _
  var systemA : ActorSystem = _
  var systemB : ActorSystem = _

  // Setup placeholder storage
  val metadbA = new MetaStorage(new HashMap[String, TxEntry])
  val metadbB = new MetaStorage(new HashMap[String, TxEntry])
  val txdb = new TxStorage(new HashMap[String, Transaction])

  override def beforeAll {
    // Setup network A
    val configFileA = getClass.getClassLoader.
      getResource("test_application.conf").getFile
    val configA = ConfigFactory.parseFile(new File(configFileA))
    systemA = ActorSystem("TestA" , configA)
    nodeA = systemA.actorOf(Props(classOf[Node], metadbA, txdb), name = "HeartbeatNode")
    val addrA = nodeA.path.address // Local address A

    remoteEndpointA =  new Address(protocol = "akka", system = "TestA", host = "127.0.0.1", port = 25000)

    // Setup network B
    val configFileB = getClass.getClassLoader.
      getResource("test_application_b.conf").getFile
    val configB = ConfigFactory.parseFile(new File(configFileB))
    systemB = ActorSystem("TestB" , configB)
    nodeB = systemB.actorOf(Props(classOf[Node], metadbB, txdb), name = "HeartbeatNode")
    val addrB = nodeB.path.address // Local address B

    // TODO: debug properly
    // setting up TestKIt instances writes down all debug messages, including routing information.
    val testKitA = new TestKit(systemA)
    val testKitB = new TestKit(systemB)
  }


  override def afterAll {
    // Shutdown remote nodes. The test suite doesn't take care of this on their own.
    CoordinatedShutdown(systemA).run(CoordinatedShutdown.unknownReason)
    CoordinatedShutdown(systemB).run(CoordinatedShutdown.unknownReason)
    Thread.sleep(500)
  }



  test("Ping/Pong remote test") {

    nodeB ! Ping() // the sender of a non-actor message is deadLetters. This will log an error when it tries to Pong()
    nodeB ! Pong() // On the other hand this is perfectly legitimate, even if the sender is deadLetters.

    // "Proper" version of the Ping() test above, that catches and prints the response message.
    implicit val timeout: Timeout = Timeout(1 second)
    val future = nodeB.ask(Ping())
    assert(Await.result(
      (future), timeout.duration) == Pong()) // This will return a "Pong()" message.
  }

  test("Heartbeat remote test") {
    // Heartbeat() is asynchronous. This is hard to test, but it will show up on the debug log after a certain timeout.
    nodeB ! Heartbeat(remoteEndpointA.toString)
    Thread.sleep(500)
    // the debug log should have a few lines about message serialization and shipping back and forth,
    // then a Pong() "Heartbeat..." message with akka://TestA@127.0.0.1:5150/user/HeartbeatNode... as sender.
    // TODO: find better way to test these. Maybe return the string instead of printing from Node.receive()?

  }

  test("Query message remote test") {

    // Send a request with no matching underlying tx data. This will print an error on the nodeA side.
    nodeB ! Query(remoteEndpointA.toString, "TEST0")
    Thread.sleep(500)

    val txEntry = new TxEntry("TEST1", new HashMap[Int, IndexEntry]())
    metadbA.save(txEntry)

    // Send a request with no matching base tx on the validation side. This will print an error on the nodeB side.
    nodeB ! Query(remoteEndpointA.toString, "TEST1")
    Thread.sleep(500)
  }

  test("<Balzac in a nutshell: Oracle>-based signature exchanging simulation") {
    /**
     * Small change to the T1 transaction: its base, starting form will contain only shared data.
     *
     * transaction T1(sigB, sigO) {
     *   input = Alice.T: sigB sigO
     *   output = 1 BTC: fun(x). versig(kB; x)
     * }
     *
     * participant B will be nodeB, participant O will be nodeA.
     * Because the signer creates the expected script for PKH-based signatures, we won't need any data from Alice.T to verify.
     *
     * The data generated on each participant by this transaction is:
     * txdb["T1"] -> Transaction object from T1(_,_)
     * metadb["T1"] -> TxEntry("T1", { 0 -> IndexEntry(1 BTC,
     *                   [chunkEntry(SIG_P2SH, 0, B.pubkey, []), chunkEntry(SIG_P2SH, 1, O.pubkey, []), ]
     *                 )})
     *
     * The states of B and O's metadb then diverge: B's first chunkentry will become
     *     chunkEntry(SIG_P2SH, 0, B.pubkey, sigB)
     * And O's second chunkentry will become
     *     chunkEntry(SIG_P2SH, 1, O.pubkey, sigO)
     *
     * This is where the signature exchange, and this test, takes place.
     */

    // Insert serialized T1(_,_) into node A and B's txdb.
    val t1 = Transaction.read("0200000001ed229658f9b7f09a2205eceaba8990b577d0afd3fcf23a329d3666c684c7fff9000000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e210237c53ebef2992c5b3f0efca8b849f4969095b31e597bdab292385bb132c30f3e52aeffffffff0100e1f505000000001976a914ba91ed34ad92a7c2aa2d764c73cd0f31a18df68088ac00000000")
    txdb.save("T1", t1)

    // Build credential pairs.
    val kb = PrivateKey.fromBase58("cQmSz3Tj3usor9byskhpCTfrmCM5cLetLU9Xw6y2csYhxSbKDzUn", Base58.Prefix.SecretKeyTestnet)._1
    val bpub = kb.publicKey
    val ko = PrivateKey.fromBase58("cTyxEAoUSKcC9NKFCjxKTaXzP8i1ufEKtwVVtY6AsRPpRgJTZQRt", Base58.Prefix.SecretKeyTestnet)._1
    val opub = ko.publicKey

    val signer = new Signer()

    // Start off from two blank TxEntry structures.
    val bTxEntry = new TxEntry("T1", HashMap[Int, IndexEntry](0 -> new IndexEntry(amt = Btc(1).toSatoshi, chunkData = Seq(
      new ChunkEntry(ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, 0, Option(bpub), data = ByteVector.empty ),
      new ChunkEntry(ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, 1, Option(opub), data = ByteVector.empty )
    ))))
    val oTxEntry = bTxEntry.copy()

    // Insert signatures from each participant into the respective network node.
    signer.fillEntry(t1, bTxEntry, kb)
    signer.fillEntry(t1, oTxEntry, ko)
    metadbA.save(oTxEntry)
    metadbB.save(bTxEntry)

    // nodeB(b participant) will ask nodeA(o participant) for his copy of T1's meta.
    println("Exchange start")
    nodeB ! Query(remoteEndpointA.toString, bTxEntry.name)
    Thread.sleep(1000)
    println("Exchange end")

    // O has successfully shared his signature with B
    assert(metadbB.fetch("T1").get.indexData(0).chunkData(1).data == metadbA.fetch("T1").get.indexData(0).chunkData(1).data)
    // B was never asked for his own signature, so O's chunk 0 data is empty.
    assert(metadbA.fetch("T1").get.indexData(0).chunkData(0).data.isEmpty)

  }
}
