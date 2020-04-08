package xyz.bitml.api.persistence

import fr.acinq.bitcoin.{ByteVector32, OutPoint, Transaction, TxIn}

import scala.collection.immutable.HashMap

class TxStorage (private var inMemoryDb : Map[String, Transaction] = new HashMap[String, Transaction]){

  def fetch(name: String): Option[Transaction] = {
    inMemoryDb.get(name)
  }

  def save(name: String, data: Transaction): Unit = {
    if (inMemoryDb.keySet.contains(name) && inMemoryDb(name).txid!= data.txid){
      val prev = inMemoryDb(name).txid
      inMemoryDb = inMemoryDb.updated(name, data)
      propagateSingleTxID(prev, data.txid)
    }else{
      inMemoryDb = inMemoryDb.updated(name, data)
    }
  }

  def dump() : Map[String, Transaction] = {
    inMemoryDb
  }

  // Recursively propagate any txid change and associated references forward.
  def propagateSingleTxID(from: ByteVector32, to: ByteVector32) {

    var newSubs = Map[ByteVector32, ByteVector32]()
    if (!this.dump().exists(_._2.txIn.exists(_.outPoint.txid == from))) {
      return
    }

    val newdb = this.dump().map(x => (x._1 -> {
      // Scroll through the entire TxIn list and switch out outdated OutPoints.
      val newTxIn = x._2.txIn.map(f => new TxIn(
        signatureScript = f.signatureScript,
        sequence = f.sequence,
        outPoint = { // Switch txid if it's between the ones we tracked.
          if ((from == f.outPoint.hash.reverse)) new OutPoint(hash = to.reverse, index = f.outPoint.index) else f.outPoint
        },
        witness = f.witness
      ))

      // Return the new transaction.
      val newTx = Transaction(
        version = x._2.version,
        txOut = x._2.txOut,
        lockTime = x._2.lockTime,
        txIn = newTxIn
      )
      // Add to swap
      if (x._2.txid != newTx.txid) newSubs = newSubs + (x._2.txid -> newTx.txid)
      newTx
    }))
    // Apply changes.
    for (i <- newdb) inMemoryDb = inMemoryDb.updated(i._1, i._2)
    for (k <- newSubs.keys) propagateSingleTxID(k, newSubs(k))
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[TxStorage]

  override def equals(other: Any): Boolean = other match {
    case that: TxStorage =>
      (that canEqual this) &&
        inMemoryDb == that.inMemoryDb
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(inMemoryDb)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
