package xyz.bitml.api

class MetaStorage (private var inMemoryDb : Map[String, TxEntry]){
  def fetch(name: String): Option[TxEntry] = {
    inMemoryDb.get(name)
  }

  def save(name: String, data: TxEntry) = {
    inMemoryDb = inMemoryDb.updated(name, data)
  }

  def dump() : Map[String, TxEntry] = {
    return inMemoryDb
  }

}
