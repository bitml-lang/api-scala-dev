package xyz.bitml.api

import fr.acinq.bitcoin.Transaction

class TxEntry (val name : String, val data : Transaction, val chunks : Map[Int, Seq[ChunkEntry]]){
}
