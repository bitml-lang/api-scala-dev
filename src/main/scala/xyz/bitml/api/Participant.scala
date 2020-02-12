package xyz.bitml.api

import akka.actor.Address
import fr.acinq.bitcoin.Crypto.PublicKey

class Participant(val name : String, val pubkey : PublicKey, val endpoint : Address) {
  def canEqual(other: Any): Boolean = other.isInstanceOf[Participant]

  override def equals(other: Any): Boolean = other match {
    case that: Participant =>
      (that canEqual this) &&
        name == that.name &&
        pubkey == that.pubkey &&
        endpoint == that.endpoint
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name, pubkey, endpoint)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
