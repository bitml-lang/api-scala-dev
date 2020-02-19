package xyz.bitml.api

object ChunkType extends Enumeration {
  type ChunkType = Value
  val SIG_P2PKH, SIG_P2SH, SIG_P2WPKH, SIG_P2WSH, SECRET_IN, SECRET_WIT, OTHER = Value
}
