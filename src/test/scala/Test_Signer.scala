import fr.acinq.bitcoin._
import fr.acinq.bitcoin.Crypto.PrivateKey
import fr.acinq.bitcoin.{Base58, Base58Check, OutPoint, Satoshi, Transaction, TxIn, TxOut}

import xyz.bitml.api.Signer

import org.scalatest.funsuite.AnyFunSuite

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

  test("Same as before but with an improper signature (incorrect identity)"){
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
}
