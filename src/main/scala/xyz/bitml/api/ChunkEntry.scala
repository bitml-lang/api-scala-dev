package xyz.bitml.api

import fr.acinq.bitcoin.Crypto.PublicKey
import scodec.bits.ByteVector
import xyz.bitml.api.ChunkPrivacy.ChunkPrivacy
import xyz.bitml.api.ChunkType.ChunkType


case class ChunkEntry (chunkType : ChunkType,  chunkPrivacy: ChunkPrivacy, chunkIndex : Int, owner : Option[PublicKey], var data: ByteVector){
}
