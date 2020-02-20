package xyz.bitml.api

import com.typesafe.scalalogging.LazyLogging
import fr.acinq.bitcoin.{ByteVector32, OutPoint, Script, ScriptWitness, Transaction}
import scodec.bits.ByteVector
import xyz.bitml.api.persistence.TxStorage

class SegwitConverter extends LazyLogging{
  // Generate a P2WPKH output from a P2PKH one
  def convertOutputP2PKH(script : ByteVector) : ByteVector = {
    Script.write(Script.pay2wpkh(Script.publicKeyHash(script)))
  }
  // Generate a redeem stack from the corresponding redeem script. Valid for both PKH and SH.
  def convertRedeem(script : ByteVector) : ScriptWitness = {
    ScriptWitness(Script.parse(script).map(x => Script.write(Seq(x)).drop(1)))
  }
  // Retrieve the names of any transaction referencing a certain txid+vout combo. Useful for tracking non-segwit forward.
  def searchRedeemer(txdb : TxStorage, op : OutPoint) : Seq[String] = {
    txdb.dump().filter(x => x._2.txIn.exists(p => p.outPoint == op)).keys.toSeq
  }
  // Search in our db a transaction referenced by txid in an OutPoint. Useful for tracking non-segwit trees backwards.
  def searchOutpoint(txdb : TxStorage, id : ByteVector32) : String = {
    val ref = txdb.dump().filter(x => x._2.hash == id).keys.toSeq
    if (ref.isEmpty) {
      throw NoSuchFieldException // TODO: more appropriate exception?
    } else if (ref.length > 1) {
      logger.warn("More than one transaction with the same txid found?")
    }
    ref.head
  }

}