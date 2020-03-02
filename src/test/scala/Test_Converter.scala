import fr.acinq.bitcoin.{Base58, Base58Check, Btc, ByteVector32, Crypto, OP_0, OP_PUSHDATA, Protocol, Satoshi, Script, ScriptWitness, Transaction}
import fr.acinq.bitcoin.Crypto.PrivateKey
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector
import xyz.bitml.api.{ChunkEntry, ChunkType, IndexEntry, SegwitConverter, TxEntry}
import xyz.bitml.api.persistence.{MetaStorage, TxStorage}

import scala.collection.immutable.HashMap

class Test_Converter extends AnyFunSuite {

  test("Testing conversion from non-segwit into witness based scripts to keep txid's consistent.") {

    val pversion = Protocol.PROTOCOL_VERSION
    val tx1 = Transaction.read("010000000240b4f27534e9d5529e67e52619c7addc88eff64c8e7afc9b41afe9a01c6e2aea010000006b48304502210091aed7fe956c4b2471778bfef5a323a96fee21ec6d73a9b7f363beaad135c5f302207f63b0ffc08fd905cdb87b109416c2d6d8ec56ca25dd52529c931aa1154277f30121037cb5789f1ca6c640b6d423ef71390e0b002da81db8fad4466bf6c2fdfb79a24cfeffffff6e21b8c625d9955e48de0a6bbcd57b03624620a93536ddacabc19d024c330f04010000006a47304402203fb779df3ae2bf8404e6d89f83af3adee0d0a0c4ec5a01a1e88b3aa4313df6490220608177ca82cf4f7da9820a8e8bf4266ccece9eb004e73926e414296d0635d7c1012102edc343e7c422e94cca4c2a87a4f7ce54594c1b68682bbeefa130295e471ac019feffffff0280f0fa02000000001976a9140f66351d05269952302a607b4d6fb69517387a9788ace06d9800000000001976a91457572594090c298721e8dddcec3ac1ec593c6dcc88ac205a0000", pversion)

    val priv1 = PrivateKey.fromBase58("QRY5zPUH6tWhQr2NwFXNpMbiLQq9u2ztcSZ6RwMPjyKv36rHP2xT", Base58.Prefix.SecretKeySegnet)._1
    val pub1 = priv1.publicKey
    val address1 = Base58Check.encode(Base58.Prefix.PubkeyAddressSegnet, Crypto.hash160(pub1.value))

    println(Script.pay2pkh(pub1)) // from this
    println(Script.pay2wpkh(pub1)) // to this?

    val orig_pkh = Script.parse(tx1.txOut(0).publicKeyScript)
    println(orig_pkh)
    println(Script.isPayToScript(orig_pkh)) // false

    val redeemScript = Script.createMultiSigMofN(1, Seq(pub1))
    println(redeemScript)

    val orig_p2sh = Script.pay2sh(redeemScript)
    println(Script.isPayToScript(orig_p2sh)) // true

    println(orig_p2sh) // from this
    println(Script.pay2wsh(redeemScript)) // to this?

    val in = Script.write(OP_0 :: OP_0 :: OP_PUSHDATA(Script.write(redeemScript)) :: Nil)
    val wit = ScriptWitness(Seq[ByteVector](ByteVector.empty, ByteVector.empty, Script.write(redeemScript)))
    println(in) // From this
    println(wit) // to this?

    /** P2PKH -> P2WPKH
     * Output: Extract pkh.  pkh= OP_DUP :: OP_HASH160 :: OP_PUSHDATA(pkh):: OP_EQUALVERIFY :: OP_CHECKSIG -> OP_0 :: pkh :: Nil
     * The library has Script.pay2wpkh which accepts ByteVectors, and Script.publicKeyHash that extracts it from chunk 2 for us.
     *    Script.pay2wpkh(Script.publicKeyHash(orig_pkh))
     * Redeem: Just strip the pushdata op. Seq(OP_PUSHDATA(x)) -> ScriptWitness(Seq(x))
     * Script.parse produces the pushdata sequence, then writing them one at a time and popping the op byte should save the chunk structure.
     */
    // One-liner for output conversion
    println(Script.pay2wpkh(Script.publicKeyHash(orig_pkh)))
    // One-liner for redeem conversion. This demo uses a p2sh script, but the script is processed the same way (seq(pushdata(x)) -> seq(x))
    println(ScriptWitness(Script.parse(in).map(x => Script.write(Seq(x)).drop(1))))

    /** P2SH -> P2WSH
     * Output : Can't be generated on its own. We go from a 20 byte hash160 to a 32 byte sha256.
     * We can easily generate an equivalent p2wsh with Script.pay2wsh(redeemScript) if we have the last chunk from the redeeming tx.
     * Redeem: should be exactly the same as pkh -> wpkh.
     * The witness stack will have a sequence of chunks without the pushdata op.
     */
    // One-liner for output generation.
    // We can't convert it directly from just the pubKeyScript information, but in a smart contract we'll always have the redeemScript
    println(Script.pay2wsh(redeemScript))
    // One-liner for redeem conversion.
    println(ScriptWitness(Script.parse(in).map(x => Script.write(Seq(x)).drop(1))))

    /** Propagating txid changes
     * Changing input and output fields breaks the references between both the output tx and potential redeemers,
     * and the redeemers with all their own following tx's.
     * Since we hold a registry of all possible contract tx's, we could just search and replace for the previous outpoint.
     */
  }

  test ("Switch a p2pkh input into a p2wpkh and produce a matching pubkeyscript.") {
    val pversion = Protocol.PROTOCOL_VERSION
    val tx1 = Transaction.read("010000000240b4f27534e9d5529e67e52619c7addc88eff64c8e7afc9b41afe9a01c6e2aea010000006b48304502210091aed7fe956c4b2471778bfef5a323a96fee21ec6d73a9b7f363beaad135c5f302207f63b0ffc08fd905cdb87b109416c2d6d8ec56ca25dd52529c931aa1154277f30121037cb5789f1ca6c640b6d423ef71390e0b002da81db8fad4466bf6c2fdfb79a24cfeffffff6e21b8c625d9955e48de0a6bbcd57b03624620a93536ddacabc19d024c330f04010000006a47304402203fb779df3ae2bf8404e6d89f83af3adee0d0a0c4ec5a01a1e88b3aa4313df6490220608177ca82cf4f7da9820a8e8bf4266ccece9eb004e73926e414296d0635d7c1012102edc343e7c422e94cca4c2a87a4f7ce54594c1b68682bbeefa130295e471ac019feffffff0280f0fa02000000001976a9140f66351d05269952302a607b4d6fb69517387a9788ace06d9800000000001976a91457572594090c298721e8dddcec3ac1ec593c6dcc88ac205a0000", pversion)

    val conv = new SegwitConverter()

    println(tx1.txIn(0))
    val res = conv.switchInput(tx1, 0, isP2SH = false)
    println(res._1.txIn(0))
    print(Script.parse(res._2))
  }

  test ("Switch a p2sh input into a p2wsh and produce a matching pubkeyscript") {
    val pversion = Protocol.PROTOCOL_VERSION
    // Balzac basic example script. x==42
    val tx1 = Transaction.read("0200000001f04e806edaf54a87964e1688e9d86295080fd9bf848bd36a12ab03111c39804e0000000008012a05766b012a87ffffffff0100ca9a3b0000000017a9146e99e54647eb6588d8a9de2be4d2dd016a1a741a8700000000", pversion)

    val conv = new SegwitConverter()

    println(tx1.txIn(0))
    val res = conv.switchInput(tx1, 0, isP2SH = true)
    println(res._1.txIn(0))
    print(Script.parse(res._2))
  }

  test ("Navigate through transaction tree and switch p2sh into pw2sh") {
    /** Modified version of balzac's basic example:
     * transaction CB {
     * input = _
     * output = 10 BTC: fun(x) . x==1
     * }
     *
     * transaction T (y:int) {
     * input = CB : y
     * output = 10 BTC: fun(x) . x == 42
     * }
     *
     *
     * transaction T1 (x:int){
     * input = T(_) : x
     * output = 10 BTC: fun(x) . x != 0
     * }
     *
     * eval CB, T(_), T1(_)
     *
     * CB is not part of our contract data.
     * T has this txEntry: { 0 : {amt=10 BTC, chunks=[SECRET_IN, < owner irrelevant >, 1]}}
     * T1 has this txEntry: { 0 : {amt=10 BTC, chunks=[SECRET_IN, < owner irrelevant >, 42]}}
     *
     * The participant list is irrelevant/empty.
     * The transaction list contains the 3 evals.
     * the meta list contains the 2 txEntries above.
     *
     * Our goal is to convert T1's input 0 and T's output 0. We shouldn't touch indexes that connect transactions to the outside.
     * */

    val balzac_cb = Transaction.read("02000000010000000000000000000000000000000000000000000000000000000000000000ffffffff02012affffffff0100ca9a3b0000000017a914ff28698ee4e6fd6a40016d28911de02aa5b948468700000000")
    val balzac_t_blank = Transaction.read("02000000013b4bb256e1b6f045b778016c5c63d4b082deab49f9b6067ffcae209dbdc4505d00000000060004766b5187ffffffff0100ca9a3b0000000017a91453c3f130b2e0f8d9a3a5b6aaf71804543076d4568700000000")
    val balzac_t1_blank = Transaction.read("0200000001d758caddefba9ea104643d0cf7acad94c880c29162e963ecb421e3c127cefd9000000000070005766b012a87ffffffff0100ca9a3b0000000017a9146e99e54647eb6588d8a9de2be4d2dd016a1a741a8700000000")

    // Set up tx storage
    val txdb = new TxStorage()
    txdb.save("t", balzac_t_blank)
    txdb.save("t1", balzac_t1_blank)

    val t_chunks = Seq(ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkIndex = 0, owner = Option.empty, data = ByteVector(1)))
    val t1_chunks = Seq(ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkIndex = 0, owner = Option.empty, data = ByteVector(42)))
    val t_entry = new TxEntry(name = "t", indexData = Map(0 -> IndexEntry(amt = Btc(10).toSatoshi ,chunkData = t_chunks)))
    val t1_entry = new TxEntry(name = "t1", indexData = Map(0 -> IndexEntry(amt = Btc(10).toSatoshi ,chunkData = t1_chunks)))

    val metadb = new MetaStorage()
    metadb.save(t_entry)
    metadb.save(t1_entry)

    val conv = new SegwitConverter()
    conv.convertTree(metadb, txdb)

    assert(metadb.fetch("t1").get.indexData(0).chunkData(0).chunkType == ChunkType.SECRET_WIT) // This has been correctly converted into one.
    assert(txdb.fetch("t").get.txOut(0).publicKeyScript.length == 34) // OP_0 :: OP_PUSHDATA(32 byte sha256)
    assert(txdb.fetch("t1").get.txIn(0).signatureScript.isEmpty) // The sigScript is now empty.
    assert(txdb.fetch("t1").get.txIn(0).witness.stack.nonEmpty) // The witness contains the new script.

    val scriptsha256 = (txdb.fetch("t").get.txOut(0).publicKeyScript).drop(2) // drop op_0 and op_pushdata
    assert(ByteVector32(scriptsha256) == Crypto.sha256(txdb.fetch("t1").get.txIn(0).witness.stack.last)) // The WSH hash is the correct one.

  }
}
