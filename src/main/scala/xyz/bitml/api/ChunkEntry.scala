package xyz.bitml.api

import fr.acinq.bitcoin.Crypto.PublicKey
import scodec.bits.ByteVector
import xyz.bitml.api.ChunkType.ChunkType


class ChunkEntry (val chunkType : ChunkType, val chunkIndex : Int, val owner : Option[PublicKey], var data: ByteVector){
}
