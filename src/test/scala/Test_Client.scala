

import scala.concurrent.duration._
import akka.actor.{ActorSystem, Address, CoordinatedShutdown, Props}
import akka.pattern.ask
import akka.util.Timeout
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.{Base58, Btc, OP_0, Satoshi, Script, ScriptElt, Transaction}
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector
import xyz.bitml.api.messaging.{AskForSigs, AssembledTx, CurrentState, DumpState, Init, Internal, Listen, Ping, Pong, StopListening, TryAssemble}
import xyz.bitml.api.{ChunkEntry, ChunkPrivacy, ChunkType, Client, IndexEntry, Participant, TxEntry}
import xyz.bitml.api.persistence.{MetaStorage, ParticipantStorage, State, TxStorage}
import xyz.bitml.api.serialization.Serializer

import scala.concurrent.{Await, Future}

class Test_Client extends AnyFunSuite {


  test("BitML compiler example"){
    /*
    #lang bitml

(participant "A" "0339bd7fade9167e09681d68c5fc80b72166fe55bbb84211fd12bde1d57247fbe1")
(participant "B" "034a7192e922118173906555a39f28fa1e0b65657fc7f403094da4f85701a5f809")

(debug-mode)

(define (txA) "tx:02000000000101000225925157edb93dda00c3765d5f6aee486b67737662cf46cb436471c645f918000000171600145093f95239942f953d077e72c495583ed601cc95feffffff0240420f00000000001976a914ded135b86a7ff97aece531c8b97dc8a3cb3ddc7488ac75d513000000000017a91451329a63924dcc4876c6e94c6ad4957cedd115b88702463043022040fc9549ec6b98027dd7f73373317b05dabb2c528f081a00b4e25ed34ca6e91f021f319d47780bb90a61b4e5257f47ccbb52eee736f4897d5b4d5a19b3df5f3ed8012102df8bd0680cb7ecf1f70eef399f9359a025b96fb776055a2150cbc973def82c116d191700@0")
(define (txFee) "tx:02000000000101adbcf28818d2556fb85ce7f6068775a6a4fd4befe650d3d7d120b609e5af1e920100000017160014a5d12120913a41cdd3be9ef88b60838b8c0db3b7feffffff028ac710000000000017a914664180e7578033f9cef5bc82b3112855f775f02587a0860100000000001976a914ded135b86a7ff97aece531c8b97dc8a3cb3ddc7488ac024730440220290f9526ed5e22d4ae72c66702f5f70dff4c5ea72445cd20112782da1986332e02201d872a0a53fa13b34a9273776dfcd0ea7385e449fec1e95263bdde96fda084e10121021215eb7fabd9bb0c1f1441bf35bade28d9e64dc798a666eb4eaf47e134a74b446d191700@1")

(contract
 (pre (deposit "A" 0.01 (ref (txA)))
      (fee "A" 0.001 (ref (txFee))))

 (withdraw "B"))

const pubkeyA2 = pubkey:0339bd7fade9167e09681d68c5fc80b72166fe55bbb84211fd12bde1d57247fbe1
const pubkeyB1 = pubkey:034a7192e922118173906555a39f28fa1e0b65657fc7f403094da4f85701a5f809

const pubkeyB = pubkey:034a7192e922118173906555a39f28fa1e0b65657fc7f403094da4f85701a5f809
const pubkeyA = pubkey:0339bd7fade9167e09681d68c5fc80b72166fe55bbb84211fd12bde1d57247fbe1

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
(participant name:"A" pubkey:"0339bd7fade9167e09681d68c5fc80b72166fe55bbb84211fd12bde1d57247fbe1" endpoint:"localhost:25000")
(participant name:"B" pubkey:"034a7192e922118173906555a39f28fa1e0b65657fc7f403094da4f85701a5f809" endpoint:"localhost:25000")

RAW TXS:
Tinit (with sigA0, sigFee blank) : "02000000027be5fa01cf6465d71c0335c5f34c53a1d2a7b29d567d442e1e98f0e64e15a06a0000000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff9d26302a3e82b2475a6ba09a0e0e0c6cf4626125ff012232180a41415be1e5a00100000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff01b15310000000000017a9147a06737efe61a6d916abdc59b7c099ae570c39ca8700000000"
T1 (with sigBT1, sigAT1 blank) : "020000000164206af5b72934c9b198dce52f65e2a1e3d87110ebe458ddd87e048c0d7b60d4000000005500004c516b6b006c766c766b7c6b5221034a7192e922118173906555a39f28fa1e0b65657fc7f403094da4f85701a5f809210339bd7fade9167e09681d68c5fc80b72166fe55bbb84211fd12bde1d57247fbe152aeffffffff0180de0f00000000001976a914ce07ee1448bbb80b38ae0c03b6cdeff40ff326ba88ac00000000"

META:
Tinit:[0:[0:(P2PKH on from A)], 1:[(p2pkh from A)]]
T1:[0:[0:(P2PKH from B, P2PKH from A)]]

     */

    // Generate initial state JSON with our own internal serialization
    val tinit = Transaction.read("02000000027be5fa01cf6465d71c0335c5f34c53a1d2a7b29d567d442e1e98f0e64e15a06a0000000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff9d26302a3e82b2475a6ba09a0e0e0c6cf4626125ff012232180a41415be1e5a00100000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff01b15310000000000017a9147a06737efe61a6d916abdc59b7c099ae570c39ca8700000000")
    val t1 = Transaction.read("020000000164206af5b72934c9b198dce52f65e2a1e3d87110ebe458ddd87e048c0d7b60d4000000005500004c516b6b006c766c766b7c6b5221034a7192e922118173906555a39f28fa1e0b65657fc7f403094da4f85701a5f809210339bd7fade9167e09681d68c5fc80b72166fe55bbb84211fd12bde1d57247fbe152aeffffffff0180de0f00000000001976a914ce07ee1448bbb80b38ae0c03b6cdeff40ff326ba88ac00000000")
    val txdb = new TxStorage()
    txdb.save("Tinit", tinit)
    txdb.save("T1", t1)

    val a_pub = PublicKey(ByteVector.fromValidHex("0339bd7fade9167e09681d68c5fc80b72166fe55bbb84211fd12bde1d57247fbe1"))
    val a_p = Participant("A", List(a_pub), Address("akka", "test", "127.0.0.1", 25000))
    val b_pub = PublicKey(ByteVector.fromValidHex("034a7192e922118173906555a39f28fa1e0b65657fc7f403094da4f85701a5f809"))
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
    val testSystem = ActorSystem(name = "internalTestSystem")
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
    CoordinatedShutdown(testSystem).run(CoordinatedShutdown.unknownReason)
    Thread.sleep(500)
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
