package xyz.bitml.api

import fr.acinq.bitcoin.Satoshi

case class IndexEntry(amt: Satoshi, chunkData: Seq[ChunkEntry]){

  def setChunks(newChunkList : Seq[ChunkEntry]): IndexEntry = {
    copy(amt, newChunkList)
  }

}
