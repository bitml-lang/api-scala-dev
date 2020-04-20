package xyz.bitml.api

import fr.acinq.bitcoin.Crypto.PublicKey
import scodec.bits.ByteVector
import xyz.bitml.api.ChunkPrivacy.ChunkPrivacy
import xyz.bitml.api.ChunkType.ChunkType


case class ChunkEntry (chunkType : ChunkType,  chunkPrivacy: ChunkPrivacy, chunkIndex : Int, owner : Option[PublicKey], var data: ByteVector){

  def isSig(): Boolean ={
    chunkType match {
      case ChunkType.SIG_P2WSH | ChunkType.SIG_P2SH | ChunkType.SIG_P2WPKH | ChunkType.SIG_P2PKH  => true
      case _ => false
    }
  }

}
