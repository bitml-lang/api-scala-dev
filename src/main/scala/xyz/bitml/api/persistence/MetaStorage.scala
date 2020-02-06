package xyz.bitml.api.persistence

import fr.acinq.bitcoin.Transaction
import xyz.bitml.api.{Signer, TxEntry}

class MetaStorage (private var inMemoryDb : Map[String, TxEntry]){
  def fetch(name: String): Option[TxEntry] = {
    inMemoryDb.get(name)
  }

  def save(name: String, data: TxEntry): Unit = {
    inMemoryDb = inMemoryDb.updated(name, data)
  }

  def dump() : Map[String, TxEntry] = {
    inMemoryDb
  }

  // Find new data from incoming tx chunk info, validate it and add it to our own.
  def update(name : String, data : TxEntry, matchingTx : Transaction): Unit  = {
    val localCopy = fetch(name).getOrElse({
      println("Error validating data: No meta available for "+name)
      return
    })
    val signer = new Signer()
    // We will simply discard anything but the actual data, and validate with our own copy. TODO: proper logging+error handling
    for (k <- localCopy.indexData.keys) {
      val localIndex = localCopy.indexData(k)
      val remoteIndex = data.indexData(k)
      for (i <- localIndex.chunkData.indices) {
        val localChunk = localIndex.chunkData(i)
        val remoteChunk = remoteIndex.chunkData(i)
        // If the chunk holds new info and properly validates, then we can add its data to ours.
        if (localChunk.data.isEmpty && remoteChunk.data.nonEmpty && signer.validateSig(matchingTx, k, localIndex.amt, localChunk, remoteChunk.data) ) {
          localChunk.data = remoteChunk.data
        }
      }
    }
    // No need to propagate the changes as localCopy is the actual record.
  }

}
