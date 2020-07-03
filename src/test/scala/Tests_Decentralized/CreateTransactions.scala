package Tests_Decentralized

import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin._
import scodec.bits.ByteVector

object CreateTransactions {

  def main(args: Array[String]): Unit = {
    createTransaction()
  }

  def createTransaction() : Unit = {
    //regtest addresses of the partecipants
    val alice_addr = "bcrt1qpvya0fx4a3rny37jt8twqe4kt95qwcvagvn7ed"
    val bob_addr = "bcrt1q6hhz23me8gqumprwzdrm4yrcy9yntp7l53nzt6"
    val oracle_addr = "bcrt1qp2g8a0ele0ywv9tc9ywutv58j0prhqznxvltn0"

    //private keys
    val a_priv = PrivateKey.fromBase58("cVbFzgZSpnuKvNT5Z3DofF9dV4Dr1zFQJw9apGZDVaG73ULqM7XS", Base58.Prefix.SecretKeyTestnet)._1
    val b_priv = PrivateKey.fromBase58("cPU3AmQFsBxvrBgTWc1j3pS6T7m4bYWMFQyPnR9Qp3o3UTCBwspZ", Base58.Prefix.SecretKeyTestnet)._1
    val o_priv = PrivateKey.fromBase58("cQAEMfAQwbVDSUDT3snYu9QVfbdBTVMrm36zoArizBkAaPYTtLdH", Base58.Prefix.SecretKeyTestnet)._1

    //public keys
    val a_pub = a_priv.publicKey
    val b_pub = b_priv.publicKey
    val o_pub = o_priv.publicKey

    println(a_pub)
    println(b_pub)
    println(o_pub)

    println(Bech32.encodeWitnessAddress("bcrt",0,Crypto.hash160(a_pub.value)))

    // funds transaction generated on regtest with sendtoaddress bcrt1qpvya0fx4a3rny37jt8twqe4kt95qwcvagvn7ed
    val funds = Transaction.read("02000000000101b6f1b81b09b740d13a52165f4f2395980f04ea5bc1a119b96e68b507e61ce0db0000000000fdffffff02424d24180100000016001412ecd583a5e432aaf852a9b6c06b0cb744abc32300e1f505000000001600140b09d7a4d5ec473247d259d6e066b6596807619d0247304402200ca11c78fd7da7e0a8ed056bef330b04dc4a2d763d89f58f07701523282a2e870220143c9324a05675847571c875ac44f496f42cb342cced0f88f2b18bef2178c648012103f9f38c3db4bd62fcfce44b4e44f86639bc6f6f38d8ae8843db6d53ebbdb4ad20ef000000")

    /*
      transaction T {
        input = A_funds: sig(kA)
        output = 1 BTC: fun(sigB, sigO). versig(Bob.kBpub, Oracle.kOpub; sigB, sigO)
      }
     */
    val t = {
      // our script is a 2-of-2 multisig script
      val redeemScript = Script.createMultiSigMofN(2, Seq(b_pub, o_pub))
      val tmp: Transaction = Transaction(version = 2,
        txIn = TxIn(OutPoint(funds.hash, 1), sequence = 0xffffffffL, signatureScript = ByteVector.empty, witness = ScriptWitness.empty) :: Nil,
        txOut = TxOut(0.99 btc, Script.pay2wsh(redeemScript)) :: Nil,
        lockTime = 0
      )
      // mind this: the pubkey script used for signing is not the prevout pubscript (which is just a push
      // of the pubkey hash), but the actual script that is evaluated by the script engine, in this case a PAY2PKH script
        val pubKeyScript = Script.pay2pkh(a_pub)
        val sig = Transaction.signInput(tmp, 0, pubKeyScript, SIGHASH_ALL, funds.txOut(1).amount, SigVersion.SIGVERSION_WITNESS_V0, a_priv)
        val witness = ScriptWitness(Seq(sig, a_pub.value))
        tmp.updateWitness(0, witness)
    }
    //check that the transaction is spendable
    Transaction.correctlySpends(t, Seq(funds), ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)

    /*
      transaction T1(sigO) {
        input = Alice.T: sig(kB) sigO
        output = 1 BTC: fun(x). versig(kB; x)
     }
     */
    val t1 = {
      val tmp: Transaction = Transaction(version = 2,
        txIn = TxIn(OutPoint(t.hash, 0), sequence = 0xffffffffL, signatureScript = ByteVector.empty) :: Nil,
        txOut = TxOut(0.999 btc, Script.pay2wpkh(b_pub)) :: Nil,
        lockTime = 0
      )
      //sign the multisig witness input
      val pubKeyScript = Script.write(Script.createMultiSigMofN(2, Seq(b_pub, o_pub)))
      val sig2 = Transaction.signInput(tmp, 0, pubKeyScript, SIGHASH_ALL, t.txOut(0).amount, SigVersion.SIGVERSION_WITNESS_V0, b_priv)
      val sig3 = Transaction.signInput(tmp, 0, pubKeyScript, SIGHASH_ALL, t.txOut(0).amount, SigVersion.SIGVERSION_WITNESS_V0, o_priv)
      val witness = ScriptWitness(Seq(ByteVector.empty, sig2, sig3, pubKeyScript))
      tmp.updateWitness(0, witness)
    }
    //check that the transaction is spendable
    Transaction.correctlySpends(t1, Seq(funds, t), ScriptFlags.STANDARD_SCRIPT_VERIFY_FLAGS)

    //dump the serialized transactions
    println("t "+t.toString())
    println("t1 "+t1.toString())
  }

}
