import akka.actor.Address
import fr.acinq.bitcoin.Crypto.PrivateKey
import fr.acinq.bitcoin.{Base58, Btc, Transaction}
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector
import xyz.bitml.api.{ChunkEntry, ChunkType, IndexEntry, Participant, TxEntry}
import xyz.bitml.api.persistence.{MetaStorage, ParticipantStorage, State, TxStorage}
import xyz.bitml.api.serialization.Serializer

class Test_Client extends AnyFunSuite {

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

    // TODO: Build common state -> json string


    val oracle_t_blank = Transaction.read("02000000017875f000ee77e9adac3224ad43c977b22b02f65339b7c69e1d7780f92e2e7fcb0000000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff0100e1f5050000000017a91459f8b912203e01527f5feed3dfd6740773c8022d8700000000")
    val oracle_t1_2blank = Transaction.read("0200000001ad765fa02c697d04b393f012c6ea0b9dc471c60ca5832cc9622c591aecabc925000000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e210237c53ebef2992c5b3f0efca8b849f4969095b31e597bdab292385bb132c30f3e52aeffffffff0100e1f505000000001976a914ba91ed34ad92a7c2aa2d764c73cd0f31a18df68088ac00000000")
    val txdb = new TxStorage()
    txdb.save("T", oracle_t_blank)
    txdb.save("T1", oracle_t1_2blank)

    val a_priv = PrivateKey.fromBase58("cSthBXr8YQAexpKeh22LB9PdextVE1UJeahmyns5LzcmMDSy59L4", Base58.Prefix.SecretKeyTestnet)._1
    val a_pub = a_priv.publicKey
    val alice = Participant("Alice", a_pub, Address("akka", "test", "127.0.0.1", 25520))
    val b_priv = PrivateKey.fromBase58("cQmSz3Tj3usor9byskhpCTfrmCM5cLetLU9Xw6y2csYhxSbKDzUn", Base58.Prefix.SecretKeyTestnet)._1
    val b_pub = b_priv.publicKey
    val bob = Participant("Bob", b_pub, Address("akka", "test", "127.0.0.1", 25521))
    val o_priv = PrivateKey.fromBase58("cTyxEAoUSKcC9NKFCjxKTaXzP8i1ufEKtwVVtY6AsRPpRgJTZQRt", Base58.Prefix.SecretKeyTestnet)._1
    val o_pub = o_priv.publicKey
    val oracle = Participant("Oracle", o_pub, Address("akka", "test", "127.0.0.1", 25522))
    val partdb = new ParticipantStorage()
    partdb.save(alice)
    partdb.save(bob)
    partdb.save(oracle)

    val t_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2PKH, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty))
    val t1_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkIndex = 0, owner = Option(o_pub), data = ByteVector.empty))
    val t_entry = new TxEntry(name = "T", indexData = Map(0 -> IndexEntry(amt = Btc(1).toSatoshi ,chunkData = t_chunks)))
    val t1_entry = new TxEntry(name = "T1", indexData = Map(0 -> IndexEntry(amt = Btc(1).toSatoshi ,chunkData = t1_chunks)))

    val metadb = new MetaStorage()
    metadb.save(t_entry)
    metadb.save(t1_entry)

    val ser = new Serializer()
    val startingState = ser.prettyPrintState(State(partdb, txdb, metadb))
    println(startingState)

    // TODO: Start 3 Client objects each with their participant's private key and the initial state json.

    // TODO: Start their network interfaces.

    // TODO: Verify T's TxOut 0 has independently been "modernized" into a p2wsh by each participant.

    // TODO: Verify Alice can already assemble T on her own.

    // TODO: Let Alice exchange signatures with both Bob and Oracle if prompted.

    // TODO: Let Bob and Oracle exchange signatures to each other when prompted.

    // TODO: Verify all 3 have all necessary info to assemble T1.

    // TODO: (debug dump state of each participant into json?)
  }
}
