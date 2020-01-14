package xyz.bitml.api

import fr.acinq.bitcoin
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.{Crypto, OP_CHECKSIG, OP_DUP, OP_EQUALVERIFY, OP_HASH160, OP_PUSHDATA, Satoshi, Script, ScriptWitness, SigVersion, Transaction}
import scodec.bits.ByteVector

class Signer {

  // Generate signature from "dummy" P2PKH redeemScript
  def signP2PKH(priv : PrivateKey, toSign : Transaction, inputIndex : Int, amt : Satoshi) : ByteVector = {
    Transaction.signInput(toSign, inputIndex, genP2PKHDummy(priv.publicKey), bitcoin.SIGHASH_ALL, amt, SigVersion.SIGVERSION_BASE, priv)
  }

  // Generate signature from a P2SH input script.
  def signP2SH(priv : PrivateKey, toSign : Transaction, inputIndex : Int, amt : Satoshi) : ByteVector = {
    // Assume P2SH redeem script is the very last chunk in our half finished input tx
    val redeemScript = Script.write(Script.parse(toSign.txIn(inputIndex).signatureScript).last :: Nil)
    Transaction.signInput(toSign, inputIndex, redeemScript, bitcoin.SIGHASH_ALL, amt, SigVersion.SIGVERSION_BASE, priv)
  }

  // Generate signature from "dummy" P2WPKH redeem script. Practically identical, except for the SigVersion flag.
  def signP2WPKH(priv : PrivateKey, toSign : Transaction, inputIndex : Int, amt : Satoshi) : ByteVector = {
    Transaction.signInput(toSign, inputIndex, genP2PKHDummy(priv.publicKey), bitcoin.SIGHASH_ALL, amt, SigVersion.SIGVERSION_WITNESS_V0, priv)
  }

  // Generate signature from a P2WSH input script.
  def signP2WSH(priv : PrivateKey, toSign : Transaction, inputIndex : Int, amt : Satoshi) : ByteVector = {
    // Assume P2WSH redeem script is the very last chunk in our half finished witness at inputIndex of our tx
    val redeemScript = ScriptWitness.unapply(toSign.txIn(inputIndex).witness).get.last
    Transaction.signInput(toSign, inputIndex, redeemScript, bitcoin.SIGHASH_ALL, amt, SigVersion.SIGVERSION_WITNESS_V0, priv)
  }

  // Generate standard P2PKH redeem script from expected public key
  def genP2PKHDummy(pub : PublicKey) : ByteVector = {
    val pkh = Crypto.hash160(pub.value)
    val redeemScript = OP_DUP :: OP_HASH160 :: OP_PUSHDATA(pkh) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil
    Script.write(redeemScript)
  }
}
