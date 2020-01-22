package xyz.bitml.api

import fr.acinq.bitcoin
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.{Crypto, OP_CHECKSIG, OP_DUP, OP_EQUALVERIFY, OP_HASH160, OP_PUSHDATA, Satoshi, Script, ScriptElt, ScriptWitness, SigVersion, Transaction}
import scodec.bits.ByteVector

class Signer {

  // Generate and fill signature into ChunkEntry object if we have the correct private key.
  def fillSig(txData : Transaction, inputIndex : Int, chunkEntry: ChunkEntry, identity : PrivateKey, amt : Option[Satoshi]) : Boolean = {
    if (chunkEntry.owner.isEmpty || (chunkEntry.owner.get != identity.publicKey)){
      false
    } else {
      println("Signature "+chunkEntry.chunkIndex+" can be filled by "+identity.publicKey)
      val produced = chunkEntry.chunkType match {
        case ChunkType.SIG_P2PKH => signP2PKH(identity, txData, inputIndex, amt.getOrElse(Satoshi(0)))
        case ChunkType.SIG_P2SH => signP2SH(identity, txData, inputIndex, amt.getOrElse(Satoshi(0)))
        case ChunkType.SIG_P2WPKH => signP2WPKH(identity, txData, inputIndex, amt.getOrElse(Satoshi(0)))
        case ChunkType.SIG_P2WSH => signP2WSH(identity, txData, inputIndex, amt.getOrElse(Satoshi(0)))
        case _ => return false
      }
      // Insert into original object
      chunkEntry.data = produced
      true
    }
  }

  // Run through TxEntry, looking for chunks that can be signed with the provided private key
  def fillEntry(txData: Transaction, txEntry: TxEntry, identity: PrivateKey): Unit = {
    println("Browsing through chunks in transaction "+ txEntry.name)
    val signer = new Signer()

    for (indexChunks <- txEntry.indexData) {
      val txIndex = indexChunks._1
      for (chunkEntry <- indexChunks._2.chunkData) {
        signer.fillSig(txData, txIndex, chunkEntry, identity, Option(indexChunks._2.amt))
      }
    }

  }

  // Insert ByteVector as pushdata into script at chunk #index.
  def injectPushdataIn(script: Seq[ScriptElt], data: ByteVector, index : Int): Seq[ScriptElt] = {
    script.updated(index, OP_PUSHDATA(data))
  }

  // Generate signature from "dummy" P2PKH redeemScript
  def signP2PKH(priv : PrivateKey, toSign : Transaction, inputIndex : Int, amt : Satoshi) : ByteVector = {
    val redeemScript = genP2PKHDummy(priv.publicKey)
    Transaction.signInput(toSign, inputIndex, redeemScript, bitcoin.SIGHASH_ALL, amt, SigVersion.SIGVERSION_BASE, priv)
  }

  // Generate signature from a P2SH input script.
  def signP2SH(priv : PrivateKey, toSign : Transaction, inputIndex : Int, amt : Satoshi) : ByteVector = {
    // Assume P2SH redeem script is the very last chunk in our half finished input tx. Remember to remove the PUSHDATA op!
    val redeemScript = Script.write(Seq(Script.parse(toSign.txIn(inputIndex).signatureScript).last)).drop(1)
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

  // Generic substitution in ByteVector sequence
  def injectAt(value_in: ByteVector,index : Int,  dest: Seq[ByteVector]) : Seq[ByteVector] = {
    dest.updated(index, value_in)
  }

  // Generate standard P2PKH redeem script from expected public key
  def genP2PKHDummy(pub : PublicKey) : ByteVector = {
    val pkh = Crypto.hash160(pub.value)
    val redeemScript = OP_DUP :: OP_HASH160 :: OP_PUSHDATA(pkh) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil
    Script.write(redeemScript)
  }
}
