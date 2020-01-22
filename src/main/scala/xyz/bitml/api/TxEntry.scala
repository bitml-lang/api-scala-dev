package xyz.bitml.api

import fr.acinq.bitcoin.Transaction

class TxEntry (val name : String, val chunks : Map[Int, Seq[ChunkEntry]]){

  def canEqual(other: Any): Boolean = other.isInstanceOf[TxEntry]

  override def equals(other: Any): Boolean = other match {
    case that: TxEntry =>
      (that canEqual this) &&
        name == that.name &&
        chunks == that.chunks
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name, chunks)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
