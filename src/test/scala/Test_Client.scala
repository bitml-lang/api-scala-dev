

import scala.concurrent.duration._
import akka.actor.{ActorSystem, Address, CoordinatedShutdown, Props}
import akka.pattern.ask
import akka.util.Timeout
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.{Base58, Btc, OP_0, Satoshi, Script, ScriptElt, Transaction}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector
import xyz.bitml.api.messaging.{AskForSigs, AssembledTx, CurrentState, DumpState, Init, Internal, Listen, Ping, Pong, PreInit, StopListening, TryAssemble}
import xyz.bitml.api.{ChunkEntry, ChunkPrivacy, ChunkType, Client, IndexEntry, Participant, TxEntry}
import xyz.bitml.api.persistence.{MetaStorage, ParticipantStorage, State, TxStorage}
import xyz.bitml.api.serialization.Serializer

import scala.concurrent.{Await, Future}

class Test_Client extends AnyFunSuite with BeforeAndAfterAll{

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


  test("BitML compiler example"){
    /*
    #lang bitml

(participant "A" "03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3")
(participant "B" "03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e")

(debug-mode)

(define (txA) "tx:02000000000101000225925157edb93dda00c3765d5f6aee486b67737662cf46cb436471c645f918000000171600145093f95239942f953d077e72c495583ed601cc95feffffff0240420f00000000001976a914ded135b86a7ff97aece531c8b97dc8a3cb3ddc7488ac75d513000000000017a91451329a63924dcc4876c6e94c6ad4957cedd115b88702463043022040fc9549ec6b98027dd7f73373317b05dabb2c528f081a00b4e25ed34ca6e91f021f319d47780bb90a61b4e5257f47ccbb52eee736f4897d5b4d5a19b3df5f3ed8012102df8bd0680cb7ecf1f70eef399f9359a025b96fb776055a2150cbc973def82c116d191700@0")
(define (txFee) "tx:02000000000101adbcf28818d2556fb85ce7f6068775a6a4fd4befe650d3d7d120b609e5af1e920100000017160014a5d12120913a41cdd3be9ef88b60838b8c0db3b7feffffff028ac710000000000017a914664180e7578033f9cef5bc82b3112855f775f02587a0860100000000001976a914ded135b86a7ff97aece531c8b97dc8a3cb3ddc7488ac024730440220290f9526ed5e22d4ae72c66702f5f70dff4c5ea72445cd20112782da1986332e02201d872a0a53fa13b34a9273776dfcd0ea7385e449fec1e95263bdde96fda084e10121021215eb7fabd9bb0c1f1441bf35bade28d9e64dc798a666eb4eaf47e134a74b446d191700@1")

(contract
 (pre (deposit "A" 0.01 (ref (txA)))
      (fee "A" 0.001 (ref (txFee))))

 (withdraw "B"))

const pubkeyA2 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
const pubkeyB1 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e

const pubkeyB = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyA = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3

const sigA0 : signature = _ //add signature for output tx:02000000000101000225925157edb93dda00c3765d5f6aee486b67737662cf46cb436471c645f918000000171600145093f95239942f953d077e72c495583ed601cc95feffffff0240420f00000000001976a914ded135b86a7ff97aece531c8b97dc8a3cb3ddc7488ac75d513000000000017a91451329a63924dcc4876c6e94c6ad4957cedd115b88702463043022040fc9549ec6b98027dd7f73373317b05dabb2c528f081a00b4e25ed34ca6e91f021f319d47780bb90a61b4e5257f47ccbb52eee736f4897d5b4d5a19b3df5f3ed8012102df8bd0680cb7ecf1f70eef399f9359a025b96fb776055a2150cbc973def82c116d191700@0
const sigAFee : signature = _ //add signature for output tx:02000000000101adbcf28818d2556fb85ce7f6068775a6a4fd4befe650d3d7d120b609e5af1e920100000017160014a5d12120913a41cdd3be9ef88b60838b8c0db3b7feffffff028ac710000000000017a914664180e7578033f9cef5bc82b3112855f775f02587a0860100000000001976a914ded135b86a7ff97aece531c8b97dc8a3cb3ddc7488ac024730440220290f9526ed5e22d4ae72c66702f5f70dff4c5ea72445cd20112782da1986332e02201d872a0a53fa13b34a9273776dfcd0ea7385e449fec1e95263bdde96fda084e10121021215eb7fabd9bb0c1f1441bf35bade28d9e64dc798a666eb4eaf47e134a74b446d191700@1

const privkeyA = key:cUnBMKCcvtpuVcfWajJBEF9uQaeNJmcRM6Vasw1vj3ZkiaoAGEuH

transaction Tinit {
 input = [ tx:02000000000101000225925157edb93dda00c3765d5f6aee486b67737662cf46cb436471c645f918000000171600145093f95239942f953d077e72c495583ed601cc95feffffff0240420f00000000001976a914ded135b86a7ff97aece531c8b97dc8a3cb3ddc7488ac75d513000000000017a91451329a63924dcc4876c6e94c6ad4957cedd115b88702463043022040fc9549ec6b98027dd7f73373317b05dabb2c528f081a00b4e25ed34ca6e91f021f319d47780bb90a61b4e5257f47ccbb52eee736f4897d5b4d5a19b3df5f3ed8012102df8bd0680cb7ecf1f70eef399f9359a025b96fb776055a2150cbc973def82c116d191700@0:sigA0; tx:02000000000101adbcf28818d2556fb85ce7f6068775a6a4fd4befe650d3d7d120b609e5af1e920100000017160014a5d12120913a41cdd3be9ef88b60838b8c0db3b7feffffff028ac710000000000017a914664180e7578033f9cef5bc82b3112855f775f02587a0860100000000001976a914ded135b86a7ff97aece531c8b97dc8a3cb3ddc7488ac024730440220290f9526ed5e22d4ae72c66702f5f70dff4c5ea72445cd20112782da1986332e02201d872a0a53fa13b34a9273776dfcd0ea7385e449fec1e95263bdde96fda084e10121021215eb7fabd9bb0c1f1441bf35bade28d9e64dc798a666eb4eaf47e134a74b446d191700@1:sigAFee ]
 output = 0.01070001 BTC : fun(sB, sA) .
 (( versig(pubkeyB1, pubkeyA2; sB, sA) ))
}

const sigBT1 : signature = _
const sigAT1 : signature = _

transaction T1 {
 input = [ Tinit@0:  sigBT1 sigAT1 ]
 output = 0.0104 BTC : fun(x) . versig(pubkeyB; x)

}

eval Tinit, T1


PARTICIPANTS (with our test endpoints)
(participant name:"A" pubkey:"03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3" endpoint:"localhost:25000")
(participant name:"B" pubkey:"03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e" endpoint:"localhost:25000")

RAW TXS:
Tinit (with sigA0, sigFee blank) : "02000000027be5fa01cf6465d71c0335c5f34c53a1d2a7b29d567d442e1e98f0e64e15a06a0000000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff9d26302a3e82b2475a6ba09a0e0e0c6cf4626125ff012232180a41415be1e5a00100000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff01b15310000000000017a9147a06737efe61a6d916abdc59b7c099ae570c39ca8700000000"
T1 (with sigBT1, sigAT1 blank) : "020000000164206af5b72934c9b198dce52f65e2a1e3d87110ebe458ddd87e048c0d7b60d4000000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff0180de0f00000000001976a914ce07ee1448bbb80b38ae0c03b6cdeff40ff326ba88ac00000000"

META:
Tinit:[0:[0:(P2PKH on from A)], 1:[(p2pkh from A)]]
T1:[0:[0:(P2PKH from B, P2PKH from A)]]

     */

    // Generate initial state JSON with our own internal serialization
    val tinit = Transaction.read("02000000027be5fa01cf6465d71c0335c5f34c53a1d2a7b29d567d442e1e98f0e64e15a06a0000000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff9d26302a3e82b2475a6ba09a0e0e0c6cf4626125ff012232180a41415be1e5a00100000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff01b15310000000000017a9147a06737efe61a6d916abdc59b7c099ae570c39ca8700000000")
    val t1 = Transaction.read("020000000164206af5b72934c9b198dce52f65e2a1e3d87110ebe458ddd87e048c0d7b60d4000000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff0180de0f00000000001976a914ce07ee1448bbb80b38ae0c03b6cdeff40ff326ba88ac00000000")
    val txdb = new TxStorage()
    txdb.save("Tinit", tinit)
    txdb.save("T1", t1)

    val a_pub = PublicKey(ByteVector.fromValidHex("03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3"))
    val a_p = Participant("A", List(a_pub), Address("akka", "test", "127.0.0.1", 25000))
    val b_pub = PublicKey(ByteVector.fromValidHex("03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e"))
    val b_p = Participant("B", List(b_pub), Address("akka", "test", "127.0.0.1", 25001))
    val partdb = new ParticipantStorage()
    partdb.save(a_p)
    partdb.save(b_p)

    val tinit0_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2PKH, chunkPrivacy= ChunkPrivacy.AUTH, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty))
    val tinit1_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2PKH, chunkPrivacy= ChunkPrivacy.AUTH, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty))
    val t1_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(a_pub), data = ByteVector.empty))
    val tinit_entry = TxEntry(name = "Tinit", indexData = Map(
      0 -> IndexEntry(amt = Satoshi(1000000) ,chunkData = tinit0_chunks),
      1 -> IndexEntry(amt = Satoshi(100000) ,chunkData = tinit1_chunks)))
    val t1_entry = TxEntry(name = "T1", indexData = Map(0 -> IndexEntry(amt = Satoshi(1070001) ,chunkData = t1_chunks)))

    val metadb = new MetaStorage()
    metadb.save(tinit_entry)
    metadb.save(t1_entry)

    val initialState = State(partdb, txdb, metadb)
    val stateJson = new Serializer().prettyPrintState(initialState)
    println(stateJson)

    // The compiler example doesn't actually follow the normal contract flow, but just produces the first Tinit from A's private key.

    val a_priv = PrivateKey.fromBase58("cUnBMKCcvtpuVcfWajJBEF9uQaeNJmcRM6Vasw1vj3ZkiaoAGEuH", Base58.Prefix.SecretKeyTestnet)._1

    // Start an actor
    val alice = testSystem.actorOf(Props[Client])

    // Initialize Alice with the state information.
    alice ! Init(a_priv, stateJson)
    // Start the network interface (even though in this test we won't communicate with B)
    alice ! Listen("test_application.conf", a_p.endpoint.system)

    // AFter letting A initialize, see if it can assemble Tinit on its own
    Thread.sleep(2000)
    implicit val timeout : Timeout = 1 second
    val future3 = alice ? TryAssemble("Tinit")
    val res3 = Await.result(future3, timeout.duration).asInstanceOf[AssembledTx].serializedTx
    // The node has produced a transaction.
    assert(res3.length != 0)
    println(Transaction.read(res3).txIn)


    alice ! StopListening()
  }

  test("Two Player Lottery"){
    /*
    #lang bitml

(debug-mode)

(define (txA) "tx:02000000000101124327402f588c4b46cfa8b1670495bd9f6f57b969212af5b8afe5da191e349f0000000017160014ca98e2fc277b25dfe48db007419b4b6f7eff7cb2feffffff0205f717000000000017a914ffe4b939f7384b08ec04b2f605b0dca4413af16a87e0930400000000001976a914ded135b86a7ff97aece531c8b97dc8a3cb3ddc7488ac024730440220197c12bf078c2bbc8f86ce93cb42042e3d528ee62de5647c1827229fe9b809ef02205e6faf5a1af59aefe493055e2cdc9d435e3524bba1cc9179e343aa8ae311de30012102a0a9937b3273031c28c1c1c4f87d7d89e4d6f973bdb00e6447a708d2c91991b2cd271700@1")
(define (txB) "tx:02000000000101bb536c381e14e1edf2d460d2e0a9ed649da2b61733d0a5d101489c5ba7fba8400100000017160014023b9558d3736f47b3ff16dcb66800ae89fc681dfeffffff025c8c3e000000000017a9140cd0faeac9fd6f23f57e206d170cd9df909e9ac987e0930400000000001976a914ce07ee1448bbb80b38ae0c03b6cdeff40ff326ba88ac02473044022059ed91550240d9da58e3cef4dabc2b2719ce36c5e05a7af35c6c321fd914c5e70220149e461c53c155706ad6b27bf1f6b08f40a2ad3a2f4c23d41481df840caafce7012102407baf142709a99a67a19c6e9ea8af329e5b1cd6ba1d178f0a5fce3a94db8eb9e1271700@1")
(define (txFee) "tx:02000000000101cc1a7d72cd7c5f64d2e0f34a0f929532b11e18a0802a2cd9d2503fd60b19585e00000000171600149e7b7e6acb6c7d0b613bb3c72f55afc723686683feffffff0240420f00000000001976a914ce07ee1448bbb80b38ae0c03b6cdeff40ff326ba88acfedd33000000000017a914677fd79b9ab537dea966e328afa6fb27d8e9aa3b870247304402201bf5adf5fdea7f1939798fb5acd8a5e75aecddee47a0d101f1113ba5f4a28a3e02205461cd71f3e757d92a0d0635937a13e219508c1be7464473b717e92cf622d642012103fa6e338afbb1bd9ffe0abc107dc15eb38811babac4d2a67fa6b78a2bd38a0809e1271700@0")

(participant "A" "03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3")
(participant "B" "03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e")

(contract (pre
           (deposit "A" 0.003 (ref (txA)))(secret "A" a "b472a266d0bd89c13706a4132ccfb16f7c3b9fcb")
           (deposit "B" 0.003 (ref (txB)))(secret "B" b "c51b66bced5e4491001bd702669770dccf440982")
           (fee "B" 0.01 (ref (txFee))))

          (split
           (0.002 -> (choice
                  (revealif (b) (pred (between b 0 1)) (withdraw "B"))
                  (after 1500000 (withdraw "A"))))
           (0.002 -> (choice
                  (reveal (a) (withdraw "A"))
                  (after 1500000 (withdraw "B"))))
           (0.002 -> (choice
                  (revealif (a b) (pred (= a b)) (withdraw "A"))
                  (revealif (a b) (pred (!= a b)) (withdraw "B"))
                  (after 1500000 (split (0.001 -> (withdraw "A")) (0.001 -> (withdraw "B")))))))

          (auto-generate-secrets)
          (check-liquid))

const privA = _ // removed
const privB = _ // removed

const sec_a:string = "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
const sec_b:string = "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"


const pubkeyB3 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyA18 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
const pubkeyA14 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
const pubkeyB1 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyA2 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
const pubkeyB17 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyA12 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
const pubkeyA10 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
const pubkeyB9 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyA8 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
const pubkeyA6 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
const pubkeyB7 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyB19 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyA20 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
const pubkeyA4 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
const pubkeyA16 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
const pubkeyB5 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyB13 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyB15 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyB11 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e

const pubkeyB = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyA = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3

transaction Tinit {
 input = [
 tx:02000000000101bb536c381e14e1edf2d460d2e0a9ed649da2b61733d0a5d101489c5ba7fba8400100000017160014023b9558d3736f47b3ff16dcb66800ae89fc681dfeffffff025c8c3e000000000017a9140cd0faeac9fd6f23f57e206d170cd9df909e9ac987e0930400000000001976a914ce07ee1448bbb80b38ae0c03b6cdeff40ff326ba88ac02473044022059ed91550240d9da58e3cef4dabc2b2719ce36c5e05a7af35c6c321fd914c5e70220149e461c53c155706ad6b27bf1f6b08f40a2ad3a2f4c23d41481df840caafce7012102407baf142709a99a67a19c6e9ea8af329e5b1cd6ba1d178f0a5fce3a94db8eb9e1271700@1:sig(privB);
 tx:02000000000101124327402f588c4b46cfa8b1670495bd9f6f57b969212af5b8afe5da191e349f0000000017160014ca98e2fc277b25dfe48db007419b4b6f7eff7cb2feffffff0205f717000000000017a914ffe4b939f7384b08ec04b2f605b0dca4413af16a87e0930400000000001976a914ded135b86a7ff97aece531c8b97dc8a3cb3ddc7488ac024730440220197c12bf078c2bbc8f86ce93cb42042e3d528ee62de5647c1827229fe9b809ef02205e6faf5a1af59aefe493055e2cdc9d435e3524bba1cc9179e343aa8ae311de30012102a0a9937b3273031c28c1c1c4f87d7d89e4d6f973bdb00e6447a708d2c91991b2cd271700@1:sig(privA);
 tx:02000000000101cc1a7d72cd7c5f64d2e0f34a0f929532b11e18a0802a2cd9d2503fd60b19585e00000000171600149e7b7e6acb6c7d0b613bb3c72f55afc723686683feffffff0240420f00000000001976a914ce07ee1448bbb80b38ae0c03b6cdeff40ff326ba88acfedd33000000000017a914677fd79b9ab537dea966e328afa6fb27d8e9aa3b870247304402201bf5adf5fdea7f1939798fb5acd8a5e75aecddee47a0d101f1113ba5f4a28a3e02205461cd71f3e757d92a0d0635937a13e219508c1be7464473b717e92cf622d642012103fa6e338afbb1bd9ffe0abc107dc15eb38811babac4d2a67fa6b78a2bd38a0809e1271700@0:sig(privB) ]
 output = 0.01569999 BTC : fun(sB, sA) . (( versig(pubkeyB1, pubkeyA2; sB, sA) ))
}

transaction T1 {
 input = [ Tinit@0: sig(privB) sig(privA) ]
 output = [ 0.00513333 BTC : fun(b:string, sB, sA) . (((between((size(b) - 128),0,2) && hash160(b) == hash:c51b66bced5e4491001bd702669770dccf440982 && size(b) >= 128 && versig(pubkeyB3, pubkeyA4; sB, sA)) ||
 versig(pubkeyB5, pubkeyA6; sB, sA)));
        0.00513333 BTC : fun(a:string, sB, sA) . (((hash160(a) == hash:b472a266d0bd89c13706a4132ccfb16f7c3b9fcb && size(a) >= 128 && versig(pubkeyB7, pubkeyA8; sB, sA)) ||
 versig(pubkeyB9, pubkeyA10; sB, sA)));
        0.00513333 BTC : fun(a:string, b:string, sB, sA) . (((size(a) == size(b) && hash160(a) == hash:b472a266d0bd89c13706a4132ccfb16f7c3b9fcb && size(a) >= 128 && hash160(b) == hash:c51b66bced5e4491001bd702669770dccf440982 && size(b) >= 128 && versig(pubkeyB11, pubkeyA12; sB, sA)) ||
 (size(a) != size(b) && hash160(a) == hash:b472a266d0bd89c13706a4132ccfb16f7c3b9fcb && size(a) >= 128 && hash160(b) == hash:c51b66bced5e4491001bd702669770dccf440982 && size(b) >= 128 && versig(pubkeyB13, pubkeyA14; sB, sA)) ||
 versig(pubkeyB15, pubkeyA16; sB, sA))) ]
}

transaction T2 {
 input = [ T1@0:sec_b  sig(privB) sig(privA) ]
 output = 0.00483333 BTC : fun(sB, sA) . versig(pubkeyB17, pubkeyA18; sB, sA)
}

const sigBT3 : signature = _
const sigAT3 : signature = _

transaction T3 {
 input = [ T2@0:   sig(privB) sig(privA) ]
 output = 0.00453333 BTC : fun(x) . versig(pubkeyB; x)

}

transaction T4 {
 input = [ T1@0: ""  sig(privB) sig(privA) ]
 output = 0.00483333 BTC : fun(x) . versig(pubkeyA; x)
 absLock = block 1500000
}

transaction T5 {
 input = [ T1@1:sec_a  sig(privB) sig(privA) ]
 output = 0.00483333 BTC : fun(sB, sA) . versig(pubkeyB19, pubkeyA20; sB, sA)
}

const sigBT6 : signature = _
const sigAT6 : signature = _

transaction T6 {
 input = [ T5@0:  sig(privB) sig(privA) ]
 output = 0.00453333 BTC : fun(x) . versig(pubkeyA; x)

}

transaction T7 {
 input = [ T1@1: "" sig(privB) sig(privA) ]
 output = 0.00483333 BTC : fun(x) . versig(pubkeyB; x)
 absLock = block 1500000
}

transaction T8 {
 input = [ T1@2:sec_a sec_b  sig(privB) sig(privA) ]
 output = 0.00483333 BTC : fun(sB, sA) . versig(pubkeyB19, pubkeyA20; sB, sA)
}

transaction T9 {
 input = [ T8@0:   sig(privB) sig(privA) ]
 output = 0.00453333 BTC : fun(x) . versig(pubkeyA; x)

}

transaction T10 {
 input = [ T1@2:sec_a sec_b  sig(privB) sig(privA) ]
 output = 0.00483333 BTC : fun(sB, sA) . versig(pubkeyB17, pubkeyA18; sB, sA)
}

transaction T11 {
 input = [ T10@0:  sig(privB) sig(privA) ]
 output = 0.00453333 BTC : fun(x) . versig(pubkeyB; x)

}

transaction T12 {
 input = [ T1@2: "" "" sig(privB) sig(privA) ]
 output = [ 0.00241666 BTC : fun(sB, sA) . ((versig(pubkeyB19, pubkeyA20; sB, sA)));
        0.00241666 BTC : fun(sB, sA) . ((versig(pubkeyB17, pubkeyA18; sB, sA))) ]
 absLock = block 1500000
}

transaction T13 {
 input = [ T12@0:  sig(privB) sig(privA) ]
 output = 0.00211666 BTC : fun(x) . versig(pubkeyA; x)

}

transaction T14 {
 input = [ T12@1:  sig(privB) sig(privA)]
 output = 0.00211666 BTC : fun(x) . versig(pubkeyB; x)

}

eval Tinit, T1, T2, T3, T5, T6, T8, T9

TODO: build initial state

We will switch out the public keys with ones we have the corresponding private key for, or else our actors can't create the sigs.

Participants:
A,B

TXs:
Tinit, T1, T2, ... T14 (all without any baked-in signatures or secrets)

Meta:
Tinit : P2PKH sigs from A for txA, B for txB and txFee (chunks 0 of indexes 0,1,2)
T1: P2SH (Tinit) sigs from A and B (chunks 0,1 of index 0)
T2: P2SH (T1) sigs from A and B (chunks 0,1 of index 0)
  ...
     */


  }

  test("Timed commitment") {
    /*
    #lang bitml

(participant "A" "03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3")
(participant "B" "03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e")

(define (txA) "tx:0200000001c75e1b501f7a1691b16d06398b4235ab35e11ccda3c3f9160d68739c84d435ed00000000e4483045022100ad5f0022e6ae8e789a97ca9497b8d307690b96ddbfcdf822711b1983b328d26702204f276374584292322c1ad33dc7b67600673ace464e9c60990de7a0123933803c014730440220055c42ae93321b4061055c782be11d3392c84ff34b1d4fbbe3a9e208f63518170220231d7712a4d36e5397264bfc8db89fd1d13d64937ee886fb9872f260bf979760014c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff01d5ea0600000000001976a914ded135b86a7ff97aece531c8b97dc8a3cb3ddc7488ac00000000@0")
(define (txFee) "tx:02000000013ea7dd4d036b9a3048992e9c7e4b8c054e7949d08d233005aa79c50ee92ff0a800000000e3483045022100f956e4b07562a209662b42ab0b6d26784de59470d992a542c207e74bf03776d5022071a5089744aa25316d29cb9d1e9bd28f5f50eba6c2c5b57177bf0a17c35308a601463043021f26ce5a6c343fcb5edf3a06dbb95006cbf063393ec7b5beebd16e2c8120c059022015c4afec46a1c04d1dcfbf8e414d9f83d0a008c91b1a96f6b499edec2b8d1d48014c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff01d5ea0600000000001976a914ded135b86a7ff97aece531c8b97dc8a3cb3ddc7488ac00000000@0")

(debug-mode)

(contract
 (pre (deposit "A" 0.00453333 (ref (txA)))
      (fee "A" 0.00453333 (ref (txFee)))
      (secret "A" a "b472a266d0bd89c13706a4132ccfb16f7c3b9fcb"))

 (choice (reveal (a) (withdraw "A"))
      (after 1550000 (withdraw "B")))

 (check-liquid))

 const privA = _ removed

const sec_a:string = "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"

const pubkeyA6 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
const pubkeyB1 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyB3 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyB5 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyA2 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
const pubkeyA4 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3

const pubkeyB = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyA = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3

transaction Tinit {
 input = [ tx:0200000001c75e1b501f7a1691b16d06398b4235ab35e11ccda3c3f9160d68739c84d435ed00000000e4483045022100ad5f0022e6ae8e789a97ca9497b8d307690b96ddbfcdf822711b1983b328d26702204f276374584292322c1ad33dc7b67600673ace464e9c60990de7a0123933803c014730440220055c42ae93321b4061055c782be11d3392c84ff34b1d4fbbe3a9e208f63518170220231d7712a4d36e5397264bfc8db89fd1d13d64937ee886fb9872f260bf979760014c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff01d5ea0600000000001976a914ded135b86a7ff97aece531c8b97dc8a3cb3ddc7488ac00000000@0:sig(privA);
 tx:02000000013ea7dd4d036b9a3048992e9c7e4b8c054e7949d08d233005aa79c50ee92ff0a800000000e3483045022100f956e4b07562a209662b42ab0b6d26784de59470d992a542c207e74bf03776d5022071a5089744aa25316d29cb9d1e9bd28f5f50eba6c2c5b57177bf0a17c35308a601463043021f26ce5a6c343fcb5edf3a06dbb95006cbf063393ec7b5beebd16e2c8120c059022015c4afec46a1c04d1dcfbf8e414d9f83d0a008c91b1a96f6b499edec2b8d1d48014c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff01d5ea0600000000001976a914ded135b86a7ff97aece531c8b97dc8a3cb3ddc7488ac00000000@0:sig(privA) ]
 output = 0.00876666 BTC : fun(a:string, sB, sA) . (( (hash160(a) == hash:9f3df038eeadc0c240fb7f82e31fdfe46804fc7c && size(a) >= 128 && versig(pubkeyB1, pubkeyA2; sB, sA)) ||
 versig(pubkeyB3, pubkeyA4; sB, sA) ))
}

const sigBT1 : signature = _
//received from Bob

transaction T1 {
 input = [ Tinit@0:sec_a sigBT1 sig(privA) ]
 output = 0.00846666 BTC : fun(sB, sA) . versig(pubkeyB5, pubkeyA6; sB, sA)
}

const sigBT2 : signature = _
//received from Bob

transaction T2 {
 input = [ T1@0:  sigBT2 sig(privA) ]
 output = 0.00816666 BTC : fun(x) . versig(pubkeyA; x)

}

const sigBT3 : signature = _
//received from Bob
const sigAT3 : signature = _

transaction T3 {
 input = [ Tinit@0: "0" sigBT3 sig(privA) ]
 output = 0.00846666 BTC : fun(x) . versig(pubkeyB; x)
 absLock = block 1550000
}

// Switched pubkeys:
// A_PUB: 03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
// B_PUB: 03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e

eval Tinit, T1, T2, T3

    Build state
    //TODO: switch a,b pubkeys with the ones from known private keys (we need to sign the transactions in the test itself)
    //TODO: build a common balzac intermediate representation that matches both participants' views.
    // That would be: sigBT* empty, no secret, etc.

    Edited public state

    const sec_a:string = _

const pubkeyA6 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
const pubkeyB1 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyB3 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyB5 = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyA2 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
const pubkeyA4 = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3

const pubkeyB = pubkey:03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e
const pubkeyA = pubkey:03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3

const sigA0 : signature = _
//received from Alice

const sigAFee : signature = _
//received from Alice

transaction Tinit {
 input = [ tx:0200000001c75e1b501f7a1691b16d06398b4235ab35e11ccda3c3f9160d68739c84d435ed00000000e4483045022100ad5f0022e6ae8e789a97ca9497b8d307690b96ddbfcdf822711b1983b328d26702204f276374584292322c1ad33dc7b67600673ace464e9c60990de7a0123933803c014730440220055c42ae93321b4061055c782be11d3392c84ff34b1d4fbbe3a9e208f63518170220231d7712a4d36e5397264bfc8db89fd1d13d64937ee886fb9872f260bf979760014c516b6b006c766c766b7c6b5221034a7192e922118173906555a39f28fa1e0b65657fc7f403094da4f85701a5f809210339bd7fade9167e09681d68c5fc80b72166fe55bbb84211fd12bde1d57247fbe152aeffffffff01d5ea0600000000001976a914ded135b86a7ff97aece531c8b97dc8a3cb3ddc7488ac00000000@0:sigA0;
 tx:02000000013ea7dd4d036b9a3048992e9c7e4b8c054e7949d08d233005aa79c50ee92ff0a800000000e3483045022100f956e4b07562a209662b42ab0b6d26784de59470d992a542c207e74bf03776d5022071a5089744aa25316d29cb9d1e9bd28f5f50eba6c2c5b57177bf0a17c35308a601463043021f26ce5a6c343fcb5edf3a06dbb95006cbf063393ec7b5beebd16e2c8120c059022015c4afec46a1c04d1dcfbf8e414d9f83d0a008c91b1a96f6b499edec2b8d1d48014c516b6b006c766c766b7c6b5221034a7192e922118173906555a39f28fa1e0b65657fc7f403094da4f85701a5f809210339bd7fade9167e09681d68c5fc80b72166fe55bbb84211fd12bde1d57247fbe152aeffffffff01d5ea0600000000001976a914ded135b86a7ff97aece531c8b97dc8a3cb3ddc7488ac00000000@0:sigAFee ]
 output = 0.00876666 BTC : fun(a:string, sB, sA) . (( (hash160(a) == hash:9f3df038eeadc0c240fb7f82e31fdfe46804fc7c && size(a) >= 128 && versig(pubkeyB1, pubkeyA2; sB, sA)) ||
 versig(pubkeyB3, pubkeyA4; sB, sA) ))
}

const sigAT1 : signature = _
const sigBT1 : signature = _
//received from Bob

transaction T1 {
 input = [ Tinit@0:sec_a sigBT1 sigAT1 ]
 output = 0.00846666 BTC : fun(sB, sA) . versig(pubkeyB5, pubkeyA6; sB, sA)
}

const sigAT2 : signature = _
const sigBT2 : signature = _
//received from Bob

transaction T2 {
 input = [ T1@0:  sigBT2 sigAT2 ]
 output = 0.00816666 BTC : fun(x) . versig(pubkeyA; x)

}

const sigBT3 : signature = _
//received from Bob
const sigAT3 : signature = _

transaction T3 {
 input = [ Tinit@0: "0" sigBT3 sigAT3 ]
 output = 0.00846666 BTC : fun(x) . versig(pubkeyB; x)
 absLock = block 1550000
}

// A_PUB: 03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3
// B_PUB: 03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e

eval Tinit, T1, T2, T3

  eval results:

Tinit
Transaction{939783b5de6357c52de66658d75b3f939d6e1e4f3aafc357c6f2059e34b8d00f
weight: 776 wu, 194 bytes
version 2
purpose: UNKNOWN
   in   0[] PUSHDATA(33)[02a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2ab]
        P2PKH addr:n1q6vo5VabL1BpqVKyHjvh4vaHrAq5NcYR  outpoint:f7097c3523aecc62dc97c790f1b276db9fc662f7a4b13fd548093ff11a09d33d:0
   in   0[] PUSHDATA(33)[02a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2ab]
        P2PKH addr:n1q6vo5VabL1BpqVKyHjvh4vaHrAq5NcYR  outpoint:08f24c84628641fecf0f33f013dbdc24c507530282cfb3c5eff97c6c2d502e59:0
   out  HASH160 PUSHDATA(20)[e2fc356d35e4759c282c9c49a39a3e8fd6f756d8] EQUAL  0.00876666 BTC
        P2SH addr:2NDwQqQTByo5XZp2dgtXzCA5eQWRDqamZaQ
}
02000000023dd3091af13f0948d53fb1a4f762c69fdb76b2f190c797dc62ccae23357c09f70000000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff592e502d6c7cf9efc5b3cf82025307c524dcdb13f0330fcffe418662844cf2080000000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff017a600d000000000017a914e2fc356d35e4759c282c9c49a39a3e8fd6f756d88700000000


T1
Transaction{43dc1a8327bb2585b69ccc9806c5cb32da30c77311d732f789c8218f191d62d0
weight: 1228 wu, 307 bytes
version 2
purpose: UNKNOWN
   in   0[] 0[] 0[] PUSHDATA1[6b6b766ba9149f3df038eeadc0c240fb7f82e31fdfe46804fc7c87636c766b827c75028000a267006863006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae670068635167006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae68]
        P2SH addr:2NDwQqQTByo5XZp2dgtXzCA5eQWRDqamZaQ  outpoint:939783b5de6357c52de66658d75b3f939d6e1e4f3aafc357c6f2059e34b8d00f:0
   out  HASH160 PUSHDATA(20)[2e38f05e636989a28f71fe9be443f82e9ce3453e] EQUAL  0.00846666 BTC
        P2SH addr:2MwTdHJ4thrcqJLUuhKCnUoajqKCiuFsEyU
}
02000000010fd0b8349e05f2c657c3af3a4f1e6e9d933f5bd75866e62dc55763deb583979300000000e00000004cdb6b6b766ba9149f3df038eeadc0c240fb7f82e31fdfe46804fc7c87636c766b827c75028000a267006863006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae670068635167006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae68ffffffff014aeb0c000000000017a9142e38f05e636989a28f71fe9be443f82e9ce3453e8700000000


T2
Transaction{72e376fa9e8f852ff43c806bccfb3abfe67cc1c4c36c5192c8d09d016f7f6513
weight: 680 wu, 170 bytes
version 2
purpose: UNKNOWN
   in   0[] 0[] PUSHDATA1[6b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae]
        P2SH addr:2MwTdHJ4thrcqJLUuhKCnUoajqKCiuFsEyU  outpoint:43dc1a8327bb2585b69ccc9806c5cb32da30c77311d732f789c8218f191d62d0:0
   out  DUP HASH160 PUSHDATA(20)[448f9bd84d520fb00adb83f26d8a78ddc5403c89] EQUALVERIFY CHECKSIG  0.00816666 BTC
        P2PKH addr:mmmUHDB4rQasyk5FszofhjfEW9vHuZM5Lz
}
0200000001d0621d198f21c889f732d71173c730da32cbc50698cc9cb68525bb27831adc43000000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff011a760c00000000001976a914448f9bd84d520fb00adb83f26d8a78ddc5403c8988ac00000000


T3
Transaction{d480203d072f885c8cee3414288baaab98f544b2fd2061f5ba1549b4a048f661
weight: 1240 wu, 310 bytes
version 2
time locked until block 1550000
purpose: UNKNOWN
   in   PUSHDATA(1)[30] 0[] 0[] PUSHDATA1[6b6b766ba9149f3df038eeadc0c240fb7f82e31fdfe46804fc7c87636c766b827c75028000a267006863006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae670068635167006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae68]
        P2SH addr:2NDwQqQTByo5XZp2dgtXzCA5eQWRDqamZaQ  outpoint:939783b5de6357c52de66658d75b3f939d6e1e4f3aafc357c6f2059e34b8d00f:0
        sequence:fffffffe
   out  DUP HASH160 PUSHDATA(20)[ba91ed34ad92a7c2aa2d764c73cd0f31a18df680] EQUALVERIFY CHECKSIG  0.00846666 BTC
        P2PKH addr:mxXSnteng7Jkg3sc7xps6Nnw8jxEgLEBky
}
02000000010fd0b8349e05f2c657c3af3a4f1e6e9d933f5bd75866e62dc55763deb583979300000000e1013000004cdb6b6b766ba9149f3df038eeadc0c240fb7f82e31fdfe46804fc7c87636c766b827c75028000a267006863006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae670068635167006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae68feffffff014aeb0c00000000001976a914ba91ed34ad92a7c2aa2d764c73cd0f31a18df68088acb0a61700


     */
    implicit val timeout : Timeout = 1 second


    val a_priv = PrivateKey.fromBase58("cSthBXr8YQAexpKeh22LB9PdextVE1UJeahmyns5LzcmMDSy59L4", Base58.Prefix.SecretKeyTestnet)._1
    val a_pub = a_priv.publicKey
    println(a_pub)
    val alice_p = Participant("Alice", List(a_pub), Address("akka", "test", "127.0.0.1", 25000))
    val b_priv = PrivateKey.fromBase58("cQmSz3Tj3usor9byskhpCTfrmCM5cLetLU9Xw6y2csYhxSbKDzUn", Base58.Prefix.SecretKeyTestnet)._1
    val b_pub = b_priv.publicKey
    println(b_pub)
    val bob_p = Participant("Bob", List(b_pub), Address("akka", "test", "127.0.0.1", 25001))

    val partdb = new ParticipantStorage()
    partdb.save(alice_p)
    partdb.save(bob_p)

    val tinit_raw = Transaction.read("02000000023dd3091af13f0948d53fb1a4f762c69fdb76b2f190c797dc62ccae23357c09f70000000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff592e502d6c7cf9efc5b3cf82025307c524dcdb13f0330fcffe418662844cf2080000000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff017a600d000000000017a914e2fc356d35e4759c282c9c49a39a3e8fd6f756d88700000000")
    val t1_raw = Transaction.read("02000000010fd0b8349e05f2c657c3af3a4f1e6e9d933f5bd75866e62dc55763deb583979300000000e00000004cdb6b6b766ba9149f3df038eeadc0c240fb7f82e31fdfe46804fc7c87636c766b827c75028000a267006863006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae670068635167006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae68ffffffff014aeb0c000000000017a9142e38f05e636989a28f71fe9be443f82e9ce3453e8700000000")
    val t2_raw = Transaction.read("0200000001d0621d198f21c889f732d71173c730da32cbc50698cc9cb68525bb27831adc43000000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff011a760c00000000001976a914448f9bd84d520fb00adb83f26d8a78ddc5403c8988ac00000000")
    val t3_raw = Transaction.read("02000000010fd0b8349e05f2c657c3af3a4f1e6e9d933f5bd75866e62dc55763deb583979300000000e1013000004cdb6b6b766ba9149f3df038eeadc0c240fb7f82e31fdfe46804fc7c87636c766b827c75028000a267006863006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae670068635167006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae68feffffff014aeb0c00000000001976a914ba91ed34ad92a7c2aa2d764c73cd0f31a18df68088acb0a61700")

    val txdb = new TxStorage()
    txdb.save("Tinit", tinit_raw)
    txdb.save("T1", t1_raw)
    txdb.save("T2", t2_raw)
    txdb.save("T3", t3_raw)

    val tinit0_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2PKH, chunkPrivacy= ChunkPrivacy.AUTH, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty))
    val tinit1_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2PKH, chunkPrivacy= ChunkPrivacy.AUTH, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty))
    val t1_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PRIVATE, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(a_pub), data = ByteVector.empty))
    val t2_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(a_pub), data = ByteVector.empty))
    val t3_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(a_pub), data = ByteVector.empty))

    val tinit_entry = TxEntry(name = "Tinit", indexData = Map(
      0 -> IndexEntry(amt = Satoshi(453333) ,chunkData = tinit0_chunks),
      1 -> IndexEntry(amt = Satoshi(453333) ,chunkData = tinit1_chunks)))
    val t1_entry = TxEntry(name = "T1", indexData = Map(0 -> IndexEntry(amt = Satoshi(876666) ,chunkData = t1_chunks)))
    val t2_entry = TxEntry(name = "T2", indexData = Map(0 -> IndexEntry(amt = Satoshi(846666) ,chunkData = t2_chunks)))
    val t3_entry = TxEntry(name = "T3", indexData = Map(0 -> IndexEntry(amt = Satoshi(876666) ,chunkData = t3_chunks)))

    val metadb = new MetaStorage()
    metadb.save(tinit_entry)
    metadb.save(t1_entry)
    metadb.save(t2_entry)
    metadb.save(t3_entry)

    val blankState = new Serializer().prettyPrintState(State(partdb, txdb, metadb))

    // Edit chunk with secret information for test convenience. In a real use case this would either be interactive or baked into the state JSON

    val t1_chunks_secret = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PRIVATE, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.fromValidHex("303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303031")),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(a_pub), data = ByteVector.empty))
    val t1_entry_secret = TxEntry(name = "T1", indexData = Map(0 -> IndexEntry(amt = Satoshi(846666) ,chunkData = t1_chunks_secret)))
    metadb.save(t1_entry_secret)

    val state_alice_view = new Serializer().prettyPrintState(State(partdb, txdb, metadb))

    val alice = testSystem.actorOf(Props(classOf[Client]))
    val bob = testSystem.actorOf(Props(classOf[Client]))

    // Initialize and convert actor state
    alice ! Init(jsonState = state_alice_view, identity = a_priv)
    bob ! Init(jsonState = blankState, identity = b_priv)

    // Start network node.
    alice ! Listen("test_application.conf", alice_p.endpoint.system)
    bob ! Listen("test_application_b.conf", bob_p.endpoint.system)

    // If Bob tries to assemble Tinit now, he'll be missing the authorization from Alice. This will be reflected in the logging.
    bob ! TryAssemble("Tinit")

    // Ensure we have all public chunks before we authorize Tinit.
    // The first time this is called, it will fail and then try fetching every missing chunk.
    alice ! PreInit()
    // If he wants, Bob can also use this to receive all public chunks, even if he doesn't have to authorize any chunk.
    // However, until Alice authorizes Tinit, he won't receive the signatures to sigA0 and sigAFee and can't publish Tinit on his own.
    bob ! PreInit()

    Thread.sleep(2000)
    // If alice doesn't have any empty public chunks anymore, it should be safe to share the signatures sigA0 and sigAFee.
    alice ! PreInit()

    // Now Alice should be able to assemble Tinit, T1 and T2.
    alice ! TryAssemble("Tinit")
    /*
    // TODO: Fix several errors in the logic itself.
    // After inserting the signatures, the Tinit hash changes.
    // With that the outpoints of T1 and t3, and their hashes, also change. and because of T1, then T2 also changes.
    // Aren't all signatures shared before publishing Tinit pointless?
    alice ! TryAssemble("T1")
    alice ! TryAssemble("T2")


    // In the same way, Bob will now be able to fetch Alice's signatures for Tinit and assemble it.
    // He will not be able to assemble T1, since that chunk is marked as PRIVATE and can't be shared at all.
    // He should be able to assemble T3.
    bob ! TryAssemble("Tinit") // Still missing the signatures at the time this is called, but it will make the query inside.
    bob ! TryAssemble("T3") // Can be assembled with just the public chunks, but is still subject to the timelock for publication.




    alice ! StopListening()
    bob ! StopListening()

    for (participant <- Seq(alice, bob)) {
      println(participant.path)
      val futState = participant ? DumpState()
      println(Await.result(futState, timeout.duration).asInstanceOf[CurrentState].state)
    }
    */

  }


    /* This test broke while updating the code to follow BitML spec. It will stay disabled for the moment.
  test("Signature exchange and transaction assembly based on Balzac.Oracle"){
    /**
     * Edited version of Oracle script:
     *
     *
    // tx with Alice's funds, redeemable with Alice's private key
transaction A_funds {
    input = _
    output = 1 BTC: fun(x). versig(Alice.kApub; x)
}

participant Alice {
    // Alice's private key
    private const kA = key:cSthBXr8YQAexpKeh22LB9PdextVE1UJeahmyns5LzcmMDSy59L4
    // Alice's public key
    const kApub = kA.toPubkey

    transaction T(sigA) {
        input = A_funds: sigA
        output = 1 BTC: fun(sigB, sigO). versig(Bob.kBpub, Oracle.kOpub; sigB, sigO)
    }
}

participant Bob {
    // Bob's private key
    private const kB = key:cQmSz3Tj3usor9byskhpCTfrmCM5cLetLU9Xw6y2csYhxSbKDzUn
    // Bob's public key
    const kBpub = kB.toPubkey

    transaction T1(sigB, sigO) {
        input = Alice.T(_): sigB sigO
        output = 1 BTC: fun(x). versig(kB; x)
    }
}

participant Oracle {
    // Oracle's private key
    private const kO = key:cTyxEAoUSKcC9NKFCjxKTaXzP8i1ufEKtwVVtY6AsRPpRgJTZQRt
    // Oracle's public key
    const kOpub = kO.toPubkey

    const sigO = sig(kO) of Bob.T1(_,_)
}


eval Alice.T(_), Bob.T1(_,_)
     *
     * Participants: Alice(priv:cSth...), Bob(priv:cQmS...), Oracle(priv:cTyx...)
     * Transactions: Alice.T, Bob.T1
     * Meta:
     * - T{index 0:{chunk 0:sig_pkh[Alice]}} // Redeem from A_funds, not convertible into witness script.
     * - T1{index 0:{chunk 0:sig_sh[Bob], chunk 1:sig_sh[Oracle]}} // Redeem A.0 script.
     */

    val oracle_t_blank = Transaction.read("02000000017875f000ee77e9adac3224ad43c977b22b02f65339b7c69e1d7780f92e2e7fcb0000000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff0100e1f5050000000017a91459f8b912203e01527f5feed3dfd6740773c8022d8700000000")
    val oracle_t1_2blank = Transaction.read("0200000001ad765fa02c697d04b393f012c6ea0b9dc471c60ca5832cc9622c591aecabc925000000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e210237c53ebef2992c5b3f0efca8b849f4969095b31e597bdab292385bb132c30f3e52aeffffffff0100e1f505000000001976a914ba91ed34ad92a7c2aa2d764c73cd0f31a18df68088ac00000000")
    val txdb = new TxStorage()
    txdb.save("T", oracle_t_blank)
    txdb.save("T1", oracle_t1_2blank)

    val a_priv = PrivateKey.fromBase58("cSthBXr8YQAexpKeh22LB9PdextVE1UJeahmyns5LzcmMDSy59L4", Base58.Prefix.SecretKeyTestnet)._1
    val a_pub = a_priv.publicKey
    val alice_p = Participant("Alice", List(a_pub), Address("akka", "test", "127.0.0.1", 25000))
    val b_priv = PrivateKey.fromBase58("cQmSz3Tj3usor9byskhpCTfrmCM5cLetLU9Xw6y2csYhxSbKDzUn", Base58.Prefix.SecretKeyTestnet)._1
    val b_pub = b_priv.publicKey
    val bob_p = Participant("Bob", List(b_pub), Address("akka", "test", "127.0.0.1", 25001))
    val o_priv = PrivateKey.fromBase58("cTyxEAoUSKcC9NKFCjxKTaXzP8i1ufEKtwVVtY6AsRPpRgJTZQRt", Base58.Prefix.SecretKeyTestnet)._1
    val o_pub = o_priv.publicKey
    val oracle_p = Participant("Oracle", List(o_pub), Address("akka", "test", "127.0.0.1", 25002))
    val partdb = new ParticipantStorage()
    partdb.save(alice_p)
    partdb.save(bob_p)
    partdb.save(oracle_p)

    val t_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2PKH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty))
    val t1_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(o_pub), data = ByteVector.empty))
    val t_entry = TxEntry(name = "T", indexData = Map(0 -> IndexEntry(amt = Btc(1).toSatoshi ,chunkData = t_chunks)))
    val t1_entry = TxEntry(name = "T1", indexData = Map(0 -> IndexEntry(amt = Btc(1).toSatoshi ,chunkData = t1_chunks)))

    val metadb = new MetaStorage()
    metadb.save(t_entry)
    metadb.save(t1_entry)

    val ser = new Serializer()
    val startingState = ser.prettyPrintState(State(partdb, txdb, metadb))
    // println(startingState)

    // Start 3 Client objects each with their participant's private key and the initial state json.

    val testSystem = ActorSystem(name = "internalTestSystem")


    val alice = testSystem.actorOf(Props(classOf[Client],  a_priv))
    val bob = testSystem.actorOf(Props(classOf[Client],  b_priv))
    val oracle = testSystem.actorOf(Props(classOf[Client],  o_priv))

    // Initializing the state will automatically convert the internal transactions into segwit.
    alice ! Init(startingState)
    bob ! Init(startingState)
    oracle ! Init(startingState)

    // Start their network interfaces.
    alice ! Listen("test_application.conf", alice_p.endpoint.system)
    bob ! Listen("test_application_b.conf", bob_p.endpoint.system)
    oracle ! Listen("test_application_o.conf", oracle_p.endpoint.system)

    // Verify T's TxOut 0 and the matching TxIn has independently been "modernized" into a p2wsh by each participant.
    implicit val timeout : Timeout = Timeout(2000 milliseconds)
    val future = alice ? DumpState()
    val res = ser.loadState(Await.result((future), timeout.duration).asInstanceOf[CurrentState].state)
    // These have been correctly converted. Test_converter tests more on this separately.
    assert(res.metadb.fetch("T1").get.indexData(0).chunkData(0).chunkType == ChunkType.SIG_P2WSH)
    assert(res.metadb.fetch("T1").get.indexData(0).chunkData(1).chunkType == ChunkType.SIG_P2WSH)
    assert(res.txdb.fetch("T").get.txOut(0).publicKeyScript.length == 34) // P2WSH: OP_0 :: PUSHDATA(32 byte sha256)
    // This should be left as is as we can't change the pubKeyScript associated.
    assert(res.metadb.fetch("T").get.indexData(0).chunkData(0).chunkType == ChunkType.SIG_P2PKH)

    // Verify Alice can already assemble T on her own.
    // This creates a problem of its own: if the assembled tx is non-segwit,
    // the Signer can't update the txid referred by everyone else.
    // Alice will receive a warning and then update the references to T.
    //
    val future2 = alice ? TryAssemble("T")
    val res2 = Await.result(future2, timeout.duration).asInstanceOf[AssembledTx].serializedTx
    // The node has produced a transaction.
    assert(res2.length != 0)
    // The signature has been produced and placed.
    assert(Script.parse(Transaction.read(res2).txIn(0).signatureScript)(0) != Seq(OP_0)(0))

    // Alice has already started asking for signatures from the first TryAssemble.
    // alice ! AskForSigs("T1")

    // Let Bob and Oracle exchange signatures to each other when prompted.
    bob ! AskForSigs("T1")
    oracle ! AskForSigs("T1")
    Thread.sleep(3000) // this is completely non-deterministic. TODO: event driven state reporting? totally doable with akka.

    // Verify all 3 have all necessary info to assemble T1.
    for (participant <- Seq(alice, bob, oracle)){
      val future3 = participant ? TryAssemble("T1")
      val res3 = Await.result(future3, timeout.duration).asInstanceOf[AssembledTx].serializedTx
      // The node has produced a transaction.
      assert(res3.length != 0)
      // The signature has been produced and placed.
      assert(Transaction.read(res3).txIn(0).witness.stack(0) != ByteVector.empty)
      assert(Transaction.read(res3).txIn(0).witness.stack(1) != ByteVector.empty)
    }
    // debug: dump final state of each participant into json
    for (participant <- Seq(alice, bob, oracle)) {
      println(participant.path)
      val futState = participant ? DumpState()
      println(Await.result(futState, timeout.duration).asInstanceOf[CurrentState].state)

      participant ! StopListening()
    }
    CoordinatedShutdown(testSystem).run(CoordinatedShutdown.unknownReason)
    Thread.sleep(500)
  }
  */
}
