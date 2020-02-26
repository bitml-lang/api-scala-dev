package xyz.bitml.api

import com.typesafe.scalalogging.LazyLogging
import fr.acinq.bitcoin.{ByteVector32, OutPoint, Script, ScriptWitness, Transaction, TxIn, TxOut}
import scodec.bits.ByteVector
import xyz.bitml.api.persistence.{MetaStorage, TxStorage}

import scala.collection.mutable

class SegwitConverter extends LazyLogging{
  // Generate a P2WPKH output from a P2PKH one
  def convertOutputP2PKH(script : ByteVector) : ByteVector = {
    Script.write(Script.pay2wpkh(Script.publicKeyHash(script)))
  }
  // Generate a redeem stack from the corresponding redeem script. Valid for both PKH and SH.
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
      pubKeyScript = Script.write(Script.pay2wpkh(Script.publicKey(Script.parse(ss))))
    }

    (resTx, pubKeyScript)
  }



  // Move backwards through the tree and convert every non-segwit transaction into one.
  def convertTree(metadb : MetaStorage, txdb : TxStorage): Unit = {
    // build txid -> name map
    val oldIdMap = txdb.dump().map(x => (x._2.txid -> x._1))


    // propagate the changes following our metadata.
    for (m <- metadb.dump()) {

      val tx = txdb.fetch(m._1).get

      // Create a deep copy that we can overwrite and save at the end.
      var cp = Transaction.read(Transaction.write(tx).toHex)

      // Go through the inputData.
      // If we find a signature or secret specifying this is a P2PKH or P2SH, go through the conversion.
      val toConvert = m._2.indexData.filter(_._2.getChunks.exists(_.chunkType match {
        case ChunkType.SIG_P2PKH | ChunkType.SIG_P2SH | ChunkType.SECRET_IN => true
        case _ => false
      }))

      for (i <- toConvert) {
        // If the sigScript references one of our own transactions,  we can safely change it
        //  and update the referenced tx with the modified pubKeyScript
        val prevStr = oldIdMap(cp.txIn(i._1).outPoint.hash)
        val prevTx = txdb.fetch(prevStr)
        if (prevTx.nonEmpty) {

          // Update the redeemScript and produce the matching PubKeyScript
          val isP2SH = i._2.getChunks.exists(_.chunkType match {
            case ChunkType.SIG_P2PKH => true
            case ChunkType.SIG_P2SH | ChunkType.SECRET_IN => false
            case _ => logger.error("Unexpected processing of non-base index type " + classOf[_]); true
          })
          val res = switchInput(cp, i._1, isP2SH)
          cp = res._1
          val pks = res._2

          // Edit and save referenced Transaction with converted pubkeyscript
          txdb.save(prevStr, switchOutput(prevTx.get, cp.txIn(i._1).outPoint.index.toInt, pks))

          // Convert our own info and update our IndexData
          val newChunks =  i._2.getChunks.map(f => new ChunkEntry(
            chunkType = f.chunkType match {
              case ChunkType.SECRET_IN => ChunkType.SECRET_WIT
              case ChunkType.SIG_P2SH => ChunkType.SIG_P2WSH
              case ChunkType.SIG_P2PKH => ChunkType.SIG_P2WPKH
              case _ => logger.error("Unexpected processing of non-base index type " + classOf[_]); f.chunkType
            },
            chunkIndex = f.chunkIndex,
            owner = f.owner,
            data = ByteVector.empty // Even if we do have something in here it's most definitely wrong now.
          ))
          m._2.indexData(i._1).setChunks(newChunks)

        }
      }
      // Save the new transaction.
      txdb.save(m._1, cp)
    }


    // build name -> txid map, then build an old id -> new id map
    val newIdMap = txdb.dump().map(x => (x._1 -> x._2.txid))
    val txidSub = oldIdMap.map(f => (f._1 -> newIdMap(f._2)))

    txdb.dump().map(x => (x._1 -> {
      // Scroll through the entire TxIn list and switch out outdated OutPoints.
      val newTxIn = x._2.txIn.map(f => new TxIn(
        signatureScript = f.signatureScript,
        sequence = f.sequence,
        outPoint = { // Switch txid if it's between the ones we tracked.
          if (oldIdMap.keys.exists(_ == f.outPoint.txid)) new OutPoint(hash = txidSub(f.outPoint.txid), index = f.outPoint.index) else f.outPoint
        }
      ))

      // Return the new transaction.
      Transaction(
        version = x._2.version,
        txOut = x._2.txOut,
        lockTime = x._2.lockTime,
        txIn = newTxIn
      )
    }))
  }

}