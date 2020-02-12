package xyz.bitml.api

import akka.actor.Address
import fr.acinq.bitcoin.Crypto.PublicKey

class Participant(val name : String, val pubkey : PublicKey, val endpoint : Address) {
}
