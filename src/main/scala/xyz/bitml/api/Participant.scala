package xyz.bitml.api

import akka.actor.Address
import fr.acinq.bitcoin.Crypto.PublicKey

case class Participant(name : String, pubkey : List[PublicKey], endpoint : Address) {
}
