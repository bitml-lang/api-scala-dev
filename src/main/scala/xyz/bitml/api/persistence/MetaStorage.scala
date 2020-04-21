package xyz.bitml.api.persistence

import com.typesafe.scalalogging.LazyLogging
import fr.acinq.bitcoin.Transaction
import scodec.bits.ByteVector
import xyz.bitml.api.{Signer, TxEntry}

import scala.collection.immutable.HashMap

class MetaStorage (private var inMemoryDb : Map[String, TxEntry] = new HashMap[String, TxEntry]) extends LazyLogging{
  def fetch(name: String): Option[TxEntry] = {
    inMemoryDb.get(name)
  }

  def save(data: TxEntry): Unit = {
    inMemoryDb = inMemoryDb.updated(data.name, data)
  }

  def dump() : Map[String, TxEntry] = {
    inMemoryDb
  }

  // Find new data from incoming tx chunk info, validate it and add it to our own.
  def update(data : TxEntry, matchingTx : Transaction): Unit  = {
    val name = data.name
    val localCopy = fetch(name).getOrElse({
      logger.error("Error validating data: No meta available for %" format (name))
      return
    })
    val signer = new Signer()
    // We will simply discard anything but the actual data, and validate with our own copy.
    for (k <- localCopy.indexData.keys) {
      val localIndex = localCopy.indexData(k)
      val remoteIndex = data.indexData(k)
      for (i <- localIndex.chunkData.indices) {
        val localChunk = localIndex.chunkData(i)
        val remoteChunk = remoteIndex.chunkData(i)
        // If the chunk holds new info and properly validates (if it is a signature), then we can add its data to ours.
        if (localChunk.data.isEmpty && remoteChunk.data.nonEmpty && (!localChunk.isSig() || signer.validateSig(matchingTx, k, localIndex.amt, localChunk, remoteChunk.data) )) {
          localChunk.data = remoteChunk.data
          logger.info("Added signature from %s to %s@%d[%d]" format (localChunk.owner.get, name, k, localChunk.chunkIndex))
        }else{
          if (localChunk.data.nonEmpty || remoteChunk.data.isEmpty)
            logger.warn("Rejected chunk for tx %s@%d[%d] cause: FULL" format (name, k, localChunk.chunkIndex))
          else
            logger.warn("Rejected chunk for tx %s@%d[%d] cause: Validation error" format (name, k, localChunk.chunkIndex))
        }
      }
    }
    // No need to propagate the changes as localCopy is the actual record.
  }

  // Empty any local chunks that do not validate against
  def validateAll(txdb : TxStorage): Unit = {
    val signer = new Signer()
    for (txe <- inMemoryDb.values){
      val matchingTx = txdb.fetch(txe.name).get
      for (ie <- txe.indexData){
        for (chk <- ie._2.chunkData){
          if (chk.isSig() && chk.data.nonEmpty && !signer.validateSig(matchingTx, ie._1, ie._2.amt, chk, chk.data)){
            logger.info("Emptying incorrect signature in tx %s" format (txe.name))
            chk.data = ByteVector.empty
          }
        }
      }
    }
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[MetaStorage]

  override def equals(other: Any): Boolean = other match {
    case that: MetaStorage =>
      (that canEqual this) &&
        inMemoryDb == that.inMemoryDb
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(inMemoryDb)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
