package xyz.bitml.api

import fr.acinq.bitcoin.Crypto.PublicKey
import scodec.bits.ByteVector
import xyz.bitml.api.ChunkType.ChunkType


class ChunkEntry (val chunkType : ChunkType, val chunkIndex : Int, val owner : Option[PublicKey], var data: ByteVector){

  def canEqual(other: Any): Boolean = other.isInstanceOf[ChunkEntry]

  override def equals(other: Any): Boolean = other match {
    case that: ChunkEntry =>
      (that canEqual this) &&
        chunkType == that.chunkType &&
        chunkIndex == that.chunkIndex &&
        owner == that.owner &&
        data == that.data
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(chunkType, chunkIndex, owner, data)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
