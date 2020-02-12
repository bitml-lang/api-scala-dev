package xyz.bitml.api.persistence

import fr.acinq.bitcoin.Transaction

class TxStorage (private var inMemoryDb : Map[String, Transaction]){

  def fetch(name: String): Option[Transaction] = {
    inMemoryDb.get(name)
  }

  def save(name: String, data: Transaction): Unit = {
    inMemoryDb = inMemoryDb.updated(name, data)
  }

  def dump() : Map[String, Transaction] = {
    inMemoryDb
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
