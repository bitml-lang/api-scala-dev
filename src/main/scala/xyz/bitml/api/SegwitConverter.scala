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
  // Move backwards through the tree and convert every non-segwit transaction into one.
  def convertTree(metadb : MetaStorage, txdb : TxStorage): Unit = {
    // build txid -> name map
    val oldIdMap = txdb.dump().map(x => (x._2.txid -> x._1))

    val leaves = searchLeaves(txdb).map(x => (x,List[(Int, ByteVector)]()))
    val queue = mutable.Queue() ++= leaves
    // Starting state : leaf nodes

    // propagate the changes backwards from the leaves, stopping either when we reference an external transaction (roots)
    // or there are no non-witness sigScripts (already processed)
    while (queue.nonEmpty) {
      val leafName = queue.dequeue
      val tx = txdb.fetch(leafName._1).get
      val meta = metadb.fetch(leafName._1).get

      // Create a deep copy that we can overwrite and save at the end.
      var cp = Transaction.read(Transaction.write(tx).toHex)

      // Go through the Output data. Switch every pubkeyScript with the ones included in the map.
      for (x <- leafName._2) {
        val newEntry = cp.txOut.toList(x._1)
        val txOut =  new TxOut(newEntry.amount, x._2)
        val newTxOut = cp.txOut.updated(x._1, txOut)
        // Rebuild the transaction with the new TxOut list.
        cp = Transaction (
          version=cp.version,
          txIn= cp.txIn,
          txOut= newTxOut,
          lockTime= cp.lockTime
        )
      }


      // Go through the inputData.
      // If we find a signature or secret specifying this is a P2PKH or P2SH, go through the conversion.
      val toConvert = meta.indexData.filter(_._2.chunkData.exists(_.chunkType match {
        case ChunkType.SIG_P2PKH | ChunkType.SIG_P2SH | ChunkType.SECRET_IN => true
        case _ => false
      }))


      for (i <- toConvert) {
        // If the sigScript references one of our own transactions,  we can safely change it
        //  and queue to update the referenced tx with the modified pubKeyScript
        if (oldIdMap.keys.exists(_ == cp.txIn(i._1).outPoint.txid)) {

          // Convert the signatureScript
          val ss = cp.txIn(i._1).signatureScript
          val wit = convertRedeem(ss)
          cp = cp.updateSigScript(i._1, ByteVector.empty).updateWitness(i._1, wit)

          // Produce the corresponding pubKeyScript
          var pubKeyScript = ByteVector.empty
          if (i._2.chunkData.exists(_.chunkType == ChunkType.SIG_P2PKH)) {
            pubKeyScript = Script.write(Script.pay2wpkh(Script.publicKey(Script.parse(ss))))
          }
          // If it's using either p2sh signatures or secrets it's a p2sh and we need to retrieve the redeemScript to proceed.
          else {
            pubKeyScript = Script.write(Script.pay2wsh(wit.stack.last))
          }

          val prevStr = oldIdMap(cp.txIn(i._1).outPoint.hash)
          queue.enqueue((prevStr, List(
            (cp.txIn(i._1).outPoint.index.toInt, pubKeyScript))))

          // Convert our own info and update our IndexData
          val newI = new IndexEntry(i._2.amt, i._2.chunkData.map(f => new ChunkEntry(
            chunkType = f.chunkType match {
              case ChunkType.SECRET_IN => ChunkType.SECRET_WIT
              case ChunkType.SIG_P2SH => ChunkType.SIG_P2WSH
              case ChunkType.SIG_P2PKH => ChunkType.SIG_P2WPKH
              case _ => f.chunkType
            },
            chunkIndex = f.chunkIndex,
            owner = f.owner,
            data = ByteVector.empty // Even if we do have something in here it's most definitely wrong now.
          )))
          meta.indexData.updated(i._1, newI)
        }
      }
      // Save the new transaction.
      txdb.save(leafName._1, cp)
    }
    // build new name -> txid map
    val newIdMap = txdb.dump().map(x => (x._1 -> x._2.txid))

    // TODO: map old txid -> new txid, then scroll through every tx and replace them.
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