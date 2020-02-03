package xyz.bitml.api.persistence

import fr.acinq.bitcoin.Transaction

class TxStorage (private var inMemoryDb : Map[String, Transaction]){

  def fetch(name: String): Option[Transaction] = {
    inMemoryDb.get(name)
  }

  def save(name: String, data: Transaction): Unit = {
    inMemoryDb = inMemoryDb.updated(name, data)
  }

  def dump() : Map[String, Transaction] = {
    inMemoryDb
  }

}
