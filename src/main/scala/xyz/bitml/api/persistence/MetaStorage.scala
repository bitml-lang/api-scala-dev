package xyz.bitml.api.persistence

import xyz.bitml.api.TxEntry

class MetaStorage (private var inMemoryDb : Map[String, TxEntry]){
  def fetch(name: String): Option[TxEntry] = {
    inMemoryDb.get(name)
  }

  def save(name: String, data: TxEntry): Unit = {
    inMemoryDb = inMemoryDb.updated(name, data)
  }

  def dump() : Map[String, TxEntry] = {
    inMemoryDb
  }

}
