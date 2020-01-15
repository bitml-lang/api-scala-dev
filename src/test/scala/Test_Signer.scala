import fr.acinq.bitcoin
import fr.acinq.bitcoin._
import fr.acinq.bitcoin.Crypto.PrivateKey
import fr.acinq.bitcoin.{Base58, Base58Check, OutPoint, Satoshi, Transaction, TxIn, TxOut}
import xyz.bitml.api.Signer
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector

class Test_Signer extends AnyFunSuite {
  // From bitcoin-lib's p2pkh example
  test("Generate a valid p2pkh signature from a private key and transaction."){
    // simple pay to PK tx

    // we have a tx that was sent to a public key that we own
    val to = "mi1cMMSL9BZwTQZYpweE1nTmwRxScirPp3"
    val (Base58.Prefix.PubkeyAddressTestnet, pubkeyHash) = Base58Check.decode(to)
    val amount = Satoshi(10000)

    val privateKey = PrivateKey.fromBase58("cRp4uUnreGMZN8vB7nQFX6XWMHU5Lc73HMAhmcDEwHfbgRS66Cqp", Base58.Prefix.SecretKeyTestnet)._1
    val publicKey = privateKey.publicKey

    val previousTx = Transaction.read("0100000001b021a77dcaad3a2da6f1611d2403e1298a902af8567c25d6e65073f6b52ef12d000000006a473044022056156e9f0ad7506621bc1eb963f5133d06d7259e27b13fcb2803f39c7787a81c022056325330585e4be39bcf63af8090a2deff265bc29a3fb9b4bf7a31426d9798150121022dfb538041f111bb16402aa83bd6a3771fa8aa0e5e9b0b549674857fafaf4fe0ffffffff0210270000000000001976a91415c23e7f4f919e9ff554ec585cb2a67df952397488ac3c9d1000000000001976a9148982824e057ccc8d4591982df71aa9220236a63888ac00000000")

    // create a transaction where the sig script is the pubkey script of the tx we want to redeem
    // the pubkey script is just a wrapper around the pub key hash
    // what it means is that we will sign a block of data that contains txid + from + to + amount

    // step  #1: creation a new transaction that reuses the previous transaction's output pubkey script
    val tx1 = Transaction(
      version = 1L,
      txIn = List(
        TxIn(OutPoint(previousTx, 0), signatureScript = Nil, sequence = 0xFFFFFFFFL)
      ),
      txOut = List(
        TxOut(amount = amount, publicKeyScript = OP_DUP :: OP_HASH160 :: OP_PUSHDATA(pubkeyHash) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil)
      ),
      lockTime = 0L
    )

    val signer = new Signer
    // step #2: sign the tx
    val sig = signer.signP2PKH(privateKey, tx1, 0, Satoshi(0))
    val tx2 = tx1.updateSigScript(0, OP_PUSHDATA(sig) :: OP_PUSHDATA(publicKey) :: Nil)

    // redeem the tx
    Transaction.correctlySpends(tx2, Seq(previousTx), ScriptFlags.MANDATORY_SCRIPT_VERIFY_FLAGS)
  }

  test("Try to sign a P2PKH TX with an improper signature (incorrect identity)"){
    // simple pay to PK tx

    // we have a tx that was sent to a public key that we own
    val to = "mi1cMMSL9BZwTQZYpweE1nTmwRxScirPp3"
    val (Base58.Prefix.PubkeyAddressTestnet, pubkeyHash) = Base58Check.decode(to)
    val amount = Satoshi(10000)

    val privateKey = PrivateKey.fromBase58("cRp4uUnreGMZN8vB7nQFX6XWMHU5Lc73HMAhmcDEwHfbgRS66Cqp", Base58.Prefix.SecretKeyTestnet)._1
    val publicKey = privateKey.publicKey

    val previousTx = Transaction.read("0100000001b021a77dcaad3a2da6f1611d2403e1298a902af8567c25d6e65073f6b52ef12d000000006a473044022056156e9f0ad7506621bc1eb963f5133d06d7259e27b13fcb2803f39c7787a81c022056325330585e4be39bcf63af8090a2deff265bc29a3fb9b4bf7a31426d9798150121022dfb538041f111bb16402aa83bd6a3771fa8aa0e5e9b0b549674857fafaf4fe0ffffffff0210270000000000001976a91415c23e7f4f919e9ff554ec585cb2a67df952397488ac3c9d1000000000001976a9148982824e057ccc8d4591982df71aa9220236a63888ac00000000")

    // create a transaction where the sig script is the pubkey script of the tx we want to redeem
    // the pubkey script is just a wrapper around the pub key hash
    // what it means is that we will sign a block of data that contains txid + from + to + amount

    // step  #1: creation a new transaction that reuses the previous transaction's output pubkey script
    val tx1 = Transaction(
      version = 1L,
      txIn = List(
        TxIn(OutPoint(previousTx, 0), signatureScript = Nil, sequence = 0xFFFFFFFFL)
      ),
      txOut = List(
        TxOut(amount = amount, publicKeyScript = OP_DUP :: OP_HASH160 :: OP_PUSHDATA(pubkeyHash) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil)
      ),
      lockTime = 0L
    )

    val signer = new Signer
    // step #2: sign the tx WITH THE INCORRECT USER

    val badpriv = PrivateKey.fromBase58("QRY5zPUH6tWhQr2NwFXNpMbiLQq9u2ztcSZ6RwMPjyKv36rHP2xT", Base58.Prefix.SecretKeySegnet)._1

    val sig = signer.signP2PKH(badpriv, tx1, 0, Satoshi(0))
    val tx2 = tx1.updateSigScript(0, OP_PUSHDATA(sig) :: OP_PUSHDATA(publicKey) :: Nil)

    assertThrows[IllegalArgumentException](Transaction.correctlySpends(tx2, Seq(previousTx), ScriptFlags.MANDATORY_SCRIPT_VERIFY_FLAGS))
  }

  // The example at bitcoin-lib for P2SH is actually a P2WSH, so this is slightly altered.
  test("Sign a P2SH transaction. Example from the bitcoin-lib github."){
    val priv1 = PrivateKey.fromBase58("QRY5zPUH6tWhQr2NwFXNpMbiLQq9u2ztcSZ6RwMPjyKv36rHP2xT", Base58.Prefix.SecretKeySegnet)._1
    val pub1 = priv1.publicKey
    val address1 = Base58Check.encode(Base58.Prefix.PubkeyAddressSegnet, Crypto.hash160(pub1.value))

    assert(address1 == "D6YX7dpieYu8j1bV8B4RgksNmDk3sNJ4Ap")

    val pversion = Protocol.PROTOCOL_VERSION

    val priv2 = PrivateKey.fromBase58("QUpr3G5ia7K7txSq5k7QpgTfNy33iTQWb1nAUgb77xFesn89xsoJ", Base58.Prefix.SecretKeySegnet)._1
    val pub2 = priv2.publicKey

    val priv3 = PrivateKey.fromBase58("QX3AN7b3WCAFaiCvAS2UD7HJZBsFU6r5shjfogJu55411hAF3BVx", Base58.Prefix.SecretKeySegnet)._1
    val pub3 = priv3.publicKey

    // this is a standard tx that sends 0.5 BTC to D6YX7dpieYu8j1bV8B4RgksNmDk3sNJ4Ap
    val tx1 = Transaction.read("010000000240b4f27534e9d5529e67e52619c7addc88eff64c8e7afc9b41afe9a01c6e2aea010000006b48304502210091aed7fe956c4b2471778bfef5a323a96fee21ec6d73a9b7f363beaad135c5f302207f63b0ffc08fd905cdb87b109416c2d6d8ec56ca25dd52529c931aa1154277f30121037cb5789f1ca6c640b6d423ef71390e0b002da81db8fad4466bf6c2fdfb79a24cfeffffff6e21b8c625d9955e48de0a6bbcd57b03624620a93536ddacabc19d024c330f04010000006a47304402203fb779df3ae2bf8404e6d89f83af3adee0d0a0c4ec5a01a1e88b3aa4313df6490220608177ca82cf4f7da9820a8e8bf4266ccece9eb004e73926e414296d0635d7c1012102edc343e7c422e94cca4c2a87a4f7ce54594c1b68682bbeefa130295e471ac019feffffff0280f0fa02000000001976a9140f66351d05269952302a607b4d6fb69517387a9788ace06d9800000000001976a91457572594090c298721e8dddcec3ac1ec593c6dcc88ac205a0000", pversion)

    // now let's create a simple tx that spends tx1 and send 0.5 BTC to a P2WSH output
    val tx2 = {
      // our script is a 2-of-2 multisig script
      val redeemScript = Script.createMultiSigMofN(2, Seq(pub2, pub3))
      val tmp = Transaction(version = 1,
        txIn = TxIn(OutPoint(tx1.hash, 0), sequence = 0xffffffffL, signatureScript = ByteVector.empty) :: Nil,
        txOut = TxOut(0.49 btc, Script.pay2sh(redeemScript)) :: Nil,
        lockTime = 0
      )
      val sig = Transaction.signInput(tmp, 0, tx1.txOut(0).publicKeyScript, SIGHASH_ALL, 0 sat, SigVersion.SIGVERSION_BASE, priv1)
      tmp.updateSigScript(0, OP_PUSHDATA(sig) :: OP_PUSHDATA(priv1.publicKey) :: Nil)
      //Transaction.sign(tmp, Seq(SignData(tx1.txOut(0).publicKeyScript, priv1)))
    }
    Transaction.correctlySpends(tx2, Seq(tx1), ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)

    // bitcoin-lib creates a P2WSH instead of a P2SH. We'll fix it here.
    val tx3 = {

      // Insert op_0 stand-ins for the signatures.
      var dummySeq = Seq[ScriptElt](OP_0, OP_0 , OP_0)
      dummySeq = dummySeq :+ OP_PUSHDATA(Script.write(Script.createMultiSigMofN(2, Seq(pub2, pub3))))

      val tmp: Transaction = Transaction(version = 1,
        txIn = TxIn(OutPoint(tx2.hash, 0), sequence = 0xffffffffL, signatureScript = dummySeq) :: Nil,
        txOut = TxOut(0.48 btc, Script.pay2wpkh(pub1)) :: Nil,
        lockTime = 0
      )

      val signer = new Signer()
      val sig2_test = signer.signP2SH(priv2, tmp, 0, tx2.txOut(0).amount)
      val sig3_test = signer.signP2SH(priv3, tmp, 0, tx2.txOut(0).amount)

      // This test only covers producing the correct signature. Injecting into the existing script is another matter.
      val redeemScript = Script.write(Script.createMultiSigMofN(2, Seq(pub2, pub3 )))
      val sigScript = OP_0 :: OP_PUSHDATA(sig2_test) :: OP_PUSHDATA(sig3_test) :: OP_PUSHDATA(redeemScript) :: Nil

      tmp.updateSigScript(0, sigScript)
    }

    Transaction.correctlySpends(tx3, Seq(tx2), ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)
  }

  test("bitcoin-lib test for p2wpkh") {

  }
}
