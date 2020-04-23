package xyz.bitml.api

import com.typesafe.scalalogging.LazyLogging
import fr.acinq.bitcoin.Crypto.PublicKey
import fr.acinq.bitcoin.{ByteVector32, OutPoint, Script, ScriptWitness, Transaction, TxIn, TxOut}
import scodec.bits.ByteVector
import xyz.bitml.api.persistence.{MetaStorage, TxStorage}

import scala.collection.mutable

class SegwitConverter extends LazyLogging{
  // Generate a P2WPKH output from a P2PKH one
  def convertOutputP2PKH(script : ByteVector) : ByteVector = {
    Script.write(Script.pay2wpkh(Script.publicKeyHash(script)))
  }
  // Generate a redeem stack from the corresponding redeem script. Valid for both PKH and SH. Does not work with multisig!
  def convertRedeem(script : ByteVector) : ScriptWitness = {
    ScriptWitness(Script.parse(script).map(x => Script.write(Seq(x)).drop(1)))
  }
  // Retrieve the names of any transaction referencing a certain txid. Useful for tracking the tx tree forward.
  def searchRedeemer(txdb : TxStorage, id : ByteVector32) : Seq[String] = {
    txdb.dump().filter(x => x._2.txIn.exists(_.outPoint.txid == id)).keys.toSeq
  }
  // Search in our db a transaction referenced by txid in an OutPoint. Useful for tracking the tx tree backwards.
  def searchOutpoint(txdb : TxStorage, id : ByteVector32) : Seq[String] = {
    val ref = txdb.dump().filter(_._2.hash == id).keys.toSeq
    if (ref.isEmpty) {
      logger.warn("No transaction found. Root tx?")
    } else if (ref.length > 1) {
      logger.warn("More than one transaction with the same txid found.")
    }
    ref
  }
  // Find the set of leaves in our transaction tree from which we can walk backwards and convert every non-segwit in the contract.
  def searchLeaves(txdb : TxStorage) : Set[String] = {
    txdb.dump().filter(n => searchRedeemer(txdb, n._2.txid).isEmpty).keys.toSet
  }

  // Build a new transaction with the added script.
  def switchOutput(tx : Transaction, index : Int, newPKS : ByteVector) : Transaction = {
    val newEntry = tx.txOut.toList(index)
    val txOut =  new TxOut(newEntry.amount, newPKS)
    val newTxOut = tx.txOut.updated(index, txOut)
    // Rebuild the transaction with the new TxOut list.
    Transaction (
      version=tx.version,
      txIn= tx.txIn,
      txOut= newTxOut,
      lockTime= tx.lockTime
    )
  }

  // Generate new transaction by converting an input index into a witness based one. Also produces compatible PubKeyScript.
  def switchInput(tx: Transaction, index : Int, isP2SH : Boolean) : (Transaction, ByteVector) = {
    val ss = tx.txIn(index).signatureScript
    val wit = convertRedeem(ss)
    val resTx = tx.updateSigScript(index, ByteVector.empty).updateWitness(index, wit)

    // Produce the corresponding pubKeyScript
    var pubKeyScript = ByteVector.empty
    if (isP2SH) {
      pubKeyScript = Script.write(Script.pay2wsh(wit.stack.last))
    }
    else {
      val pk = PublicKey(wit.stack.last)
      pubKeyScript = Script.write(Script.pay2wpkh(pk))
    }

    (resTx, pubKeyScript)
  }



  // Move backwards through the tree and convert every non-segwit transaction into one.
  def convertTree(metadb : MetaStorage, txdb : TxStorage): Unit = {


    // propagate the changes following our metadata.
    for (m <- metadb.dump()) {
      // create txid -> name map
      val oldIdMap = txdb.dump().map(x => (x._2.txid -> x._1))

      // Go through the inputData.
      // If we find a signature or secret specifying this is a P2PKH or P2SH, go through the conversion.
      val toConvert = m._2.indexData.filter(_._2.chunkData.exists(_.chunkType match {
        case ChunkType.SIG_P2PKH | ChunkType.SIG_P2SH | ChunkType.SECRET_IN => true
        case _ => false
      }))

      // Copy current indexData structure
      var metaCopy = m._2.copy().indexData

      for (i <- toConvert) {
        // Create a deep copy that we can overwrite and save.
        var cp = Transaction.read(Transaction.write(txdb.fetch(m._1).get).toHex)

        // Update the redeemScript and produce the matching PubKeyScript
        val isP2SH = i._2.chunkData.exists(_.chunkType match {
          case ChunkType.SIG_P2PKH => false
          case ChunkType.SIG_P2SH | ChunkType.SECRET_IN => true
          case _ => logger.error("Unexpected processing of non-base index type"); true
        })

        val res = switchInput(cp, i._1, isP2SH)
        cp = res._1
        // Save partial conversion so that txdb can apply the txid changes
        txdb.save(m._1, cp)

        // If the sigScript references one of our own transactions,  we can safely change it
        //  and update the referenced tx with the modified pubKeyScript.
        logger.debug("Searching for tx with hash %s " format (cp.txIn(i._1).outPoint.hash))
        val searchStr = oldIdMap.get(cp.txIn(i._1).outPoint.hash.reverse)
        if (searchStr.nonEmpty) {
          val pks = res._2
          val prevStr = searchStr.get
          logger.debug("Retrieving tx with id %s " format (prevStr))
          val prevTx = txdb.fetch(prevStr)
          // Edit and save referenced Transaction with converted pubkeyscript
          val newPrev = switchOutput(prevTx.get, cp.txIn(i._1).outPoint.index.toInt, pks)
          txdb.save(prevStr, newPrev)
        } else {
          // There is only one situation in which we'd like to switch only the input, and that's Tinit inputs.
          // Anything else is almost certainly an error and should at least be logged.
          logger.error("Transaction %s modified without control of output" format (m._1))
        }
        // Refresh cp so that we receive the txdb txid updates.
        cp = Transaction.read(Transaction.write(txdb.fetch(m._1).get).toHex)

        // Convert our own info and update our IndexData
        val newChunks =  i._2.chunkData.map(f => f.copy(
          chunkType = f.chunkType match {
            case ChunkType.SECRET_IN => ChunkType.SECRET_WIT
            case ChunkType.SIG_P2SH => ChunkType.SIG_P2WSH
            case ChunkType.SIG_P2PKH => ChunkType.SIG_P2WPKH
            case _ => logger.error("Unexpected processing of non-base index type"); f.chunkType
          },
          data = f.chunkType match {
            case ChunkType.SECRET_WIT | ChunkType.SECRET_IN => f.data // The script rewrite should preserve the expected value.
            case _ => ByteVector.empty // Any signature (especially with sighash_all) will have to be remade.
          }
        ))
        metaCopy += (i._1 -> i._2.copy(chunkData = newChunks))
      }
      metadb.save( m._2.copy(indexData = metaCopy))
    }
  }
}