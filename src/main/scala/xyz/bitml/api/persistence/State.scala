package xyz.bitml.api.persistence

case class State(partdb : ParticipantStorage, txdb : TxStorage, metadb : MetaStorage) {
}
