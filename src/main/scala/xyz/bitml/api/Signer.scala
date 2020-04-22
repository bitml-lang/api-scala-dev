package xyz.bitml.api

import com.typesafe.scalalogging.LazyLogging
import fr.acinq.bitcoin
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.{Crypto, OP_CHECKSIG, OP_DUP, OP_EQUALVERIFY, OP_HASH160, OP_PUSHDATA, Satoshi, Script, ScriptElt, ScriptWitness, SigVersion, Transaction}
import scodec.bits.ByteVector

class Signer extends LazyLogging{

  // Generate and fill signature into ChunkEntry object if we have the correct private key.
  def fillSig(txData : Transaction, inputIndex : Int, chunkEntry: ChunkEntry, identity : PrivateKey, amt : Option[Satoshi]) : Boolean = {
    if (chunkEntry.owner.isEmpty || (chunkEntry.owner.get != identity.publicKey)){
      false
    } else {
      logger.info("Filling [%d] with signature from  %s" format (chunkEntry.chunkIndex, identity.publicKey))
      val produced = chunkEntry.chunkType match {
        case ChunkType.SIG_P2PKH => signP2PKH(identity, txData, inputIndex, amt.getOrElse(Satoshi(0)))
        case ChunkType.SIG_P2SH => signP2SH(identity, txData, inputIndex, amt.getOrElse(Satoshi(0)))
        case ChunkType.SIG_P2WPKH => signP2WPKH(identity, txData, inputIndex, amt.getOrElse(Satoshi(0)))
        case ChunkType.SIG_P2WSH => signP2WSH(identity, txData, inputIndex, amt.getOrElse(Satoshi(0)))
        case _ => return false
      }
      // Insert into original object
      chunkEntry.data = produced
      logger.debug("tx:%s\nsig:%s\npk:%s\n sanity:%s" format (txData, produced, chunkEntry.owner.get, identity.publicKey == chunkEntry.owner.get))
      true
    }
  }
  // Run through TxEntry, looking for chunks that can be signed with the provided private key
  def fillEntry(txData: Transaction, txEntry: TxEntry, identity: PrivateKey): Unit = {
    logger.debug("Filling transaction %s" format (txEntry.name))
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

  def validateSig(toSign : Transaction, inputIndex : Int, amt : Satoshi, localEntry: ChunkEntry, sig : ByteVector) : Boolean = {
    // Recreate signing hash
    if (localEntry.owner.isEmpty ){
      logger.warn("No owner!"); false
    } else {
      logger.debug("Verifying signature [%d]" format (localEntry.chunkIndex))
      val redeemScript = localEntry.chunkType match {
        case ChunkType.SIG_P2PKH | ChunkType.SIG_P2WPKH => genP2PKHDummy(localEntry.owner.get)
        case ChunkType.SIG_P2SH => Script.write(Seq(Script.parse(toSign.txIn(inputIndex).signatureScript).last)).drop(1)
        case ChunkType.SIG_P2WSH => ScriptWitness.unapply(toSign.txIn(inputIndex).witness).get.last
        case _ => logger.warn("Unexpected sig type!");return false
      }
      val sigVersion = localEntry.chunkType match {
        case ChunkType.SIG_P2PKH | ChunkType.SIG_P2SH => SigVersion.SIGVERSION_BASE
        case ChunkType.SIG_P2WPKH | ChunkType.SIG_P2WSH => SigVersion.SIGVERSION_WITNESS_V0
        case _ => logger.warn("Unexpected sig type!");return false
      }

      val signData = Transaction.hashForSigning(toSign, inputIndex, redeemScript, bitcoin.SIGHASH_ALL, amt, sigVersion)
      //Convert back der+type flag to compact signature
      val compactSig = Crypto.der2compact(sig)
      // Return signature validation
      Crypto.verifySignature(signData, compactSig, localEntry.owner.get)
    }
  }

  def assembleTx(tx : Transaction, data : TxEntry): Transaction = {
    //Create a deep copy by deserializing the serialized tx.
    var cp = Transaction.read(Transaction.write(tx).toHex)

    for (id <- data.indexData) {
      for (chunk <- id._2.chunkData) {
        if (chunk.data.isEmpty) {
          logger.warn("Warning: chunk [%d] appears to be empty!" format (chunk.chunkIndex))
        } else {
          chunk.chunkType match {
            case ChunkType.SIG_P2PKH | ChunkType.SIG_P2SH | ChunkType.SECRET_IN => {
              cp = cp.updateSigScript(id._1, injectPushdataIn(Script.parse(cp.txIn(id._1).signatureScript), chunk.data, chunk.chunkIndex))
              logger.debug("Added chunk to  %s@%d[%d] sigScript" format (data.name, id._1, chunk.chunkIndex))
              logger.warn("TXID CHANGE: %s -> %s"  format ( tx.txid, cp.txid))
              // TODO: propagate in transactions referencing the original txid as outpoint.
            }
            case ChunkType.SIG_P2WPKH | ChunkType.SIG_P2WSH | ChunkType.SECRET_WIT => {
              val witstack = cp.txIn(id._1).witness.stack
              cp = cp.updateWitness(id._1, ScriptWitness(injectAt(chunk.data, chunk.chunkIndex, witstack)))
              logger.debug("added chunk to %s@%d[%d] witness stack" format (data.name, id._1, chunk.chunkIndex))
            }
            case _ => logger.warn("Warning: chunk type is unknown/incompatible with auto-insertion")
          }
        }
      }
    }
    cp
  }
}
