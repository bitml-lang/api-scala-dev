package xyz.bitml.api

class TxEntry (val name : String, val indexData : Map[Int, IndexEntry]){

  def canEqual(other: Any): Boolean = other.isInstanceOf[TxEntry]

  override def equals(other: Any): Boolean = other match {
    case that: TxEntry =>
      (that canEqual this) &&
        name == that.name &&
        indexData == that.indexData
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name, indexData)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
