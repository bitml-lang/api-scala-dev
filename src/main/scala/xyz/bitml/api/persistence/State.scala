package xyz.bitml.api.persistence

case class State(partdb : ParticipantStorage = new ParticipantStorage(), txdb : TxStorage = new TxStorage(), metadb : MetaStorage = new MetaStorage()) {
}
