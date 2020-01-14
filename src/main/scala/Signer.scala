import fr.acinq.bitcoin
import fr.acinq.bitcoin.{Crypto, OP_CHECKSIG, OP_DUP, OP_EQUALVERIFY, OP_HASH160, OP_PUSHDATA, Satoshi, Script, SigVersion, Transaction}
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import scodec.bits.ByteVector

class Signer {

  // Generate signature from "dummy" P2PKH redeemScript
  def signP2PKH(priv : PrivateKey, toSign : Transaction, inputIndex : Int) : ByteVector = {
    Transaction.signInput(toSign, inputIndex, genP2PKHDummy(priv.publicKey),bitcoin.SIGHASH_ALL, Satoshi(0),SigVersion.SIGVERSION_BASE,priv)
  }

  // Generate standard P2PKH redeem script from expected public key
  def genP2PKHDummy(pub : PublicKey) : ByteVector = {
    val pkh = Crypto.hash160(pub.value)
    val redeemScript = OP_DUP :: OP_HASH160 :: OP_PUSHDATA(pkh) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil
    Script.write(redeemScript)
  }
}
