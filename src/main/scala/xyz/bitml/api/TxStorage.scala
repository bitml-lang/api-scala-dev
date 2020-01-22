package xyz.bitml.api

import fr.acinq.bitcoin.Transaction

class TxStorage (val inMemoryDb : Map[String, Transaction]){

  def fetch(name: String): Option[Transaction] = {
    inMemoryDb.get(name)
  }

  def save(name: String, data: Transaction): Unit = {
    inMemoryDb.updated(name, data)
  }

  def dump() : Map[String, Transaction] = {
    return inMemoryDb
  }

}
