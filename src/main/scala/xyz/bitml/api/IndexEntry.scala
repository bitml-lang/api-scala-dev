package xyz.bitml.api

import fr.acinq.bitcoin.Satoshi

class IndexEntry(val amt: Satoshi, val chunkData: Seq[ChunkEntry]){
  def canEqual(other: Any): Boolean = other.isInstanceOf[IndexEntry]

  override def equals(other: Any): Boolean = other match {
    case that: IndexEntry =>
      (that canEqual this) &&
        amt == that.amt &&
        chunkData == that.chunkData
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(amt, chunkData)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
