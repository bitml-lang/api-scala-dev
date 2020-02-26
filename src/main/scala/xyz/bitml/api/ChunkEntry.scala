package xyz.bitml.api

import fr.acinq.bitcoin.Crypto.PublicKey
import scodec.bits.ByteVector
import xyz.bitml.api.ChunkType.ChunkType


case class ChunkEntry (chunkType : ChunkType, chunkIndex : Int, owner : Option[PublicKey], var data: ByteVector){

}
