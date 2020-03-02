package xyz.bitml.api.persistence

import xyz.bitml.api.Participant

import scala.collection.immutable.HashMap

class ParticipantStorage (private var inMemoryDb : Map[String, Participant] = new HashMap[String, Participant]){

  def fetch(pubString : String): Option[Participant] = {
    inMemoryDb.get(pubString)
  }

  def save(data: Participant): Unit = {
    inMemoryDb = inMemoryDb.updated(data.pubkey.toString(), data)
  }

  def dump() : Map[String, Participant] = {
    inMemoryDb
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[ParticipantStorage]

  override def equals(other: Any): Boolean = other match {
    case that: ParticipantStorage =>
      (that canEqual this) &&
        inMemoryDb == that.inMemoryDb
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(inMemoryDb)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
