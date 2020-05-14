import akka.actor.Address
import fr.acinq.bitcoin.Crypto.{PrivateKey, PublicKey}
import fr.acinq.bitcoin.{Base58, Satoshi, Transaction}
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector
import xyz.bitml.api.{ChunkEntry, ChunkPrivacy, ChunkType, IndexEntry, Participant, TxEntry}
import xyz.bitml.api.persistence.{MetaStorage, ParticipantStorage, State, TxStorage}
import xyz.bitml.api.serialization.Serializer

class Example_States extends AnyFunSuite {
  test("Compiler test example") {

    // Generate initial state JSON with our own internal serialization
    val tinit = Transaction.read("02000000027be5fa01cf6465d71c0335c5f34c53a1d2a7b29d567d442e1e98f0e64e15a06a0000000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff9d26302a3e82b2475a6ba09a0e0e0c6cf4626125ff012232180a41415be1e5a00100000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff01b15310000000000017a9147a06737efe61a6d916abdc59b7c099ae570c39ca8700000000")
    val t1 = Transaction.read("020000000164206af5b72934c9b198dce52f65e2a1e3d87110ebe458ddd87e048c0d7b60d4000000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff0180de0f00000000001976a914ce07ee1448bbb80b38ae0c03b6cdeff40ff326ba88ac00000000")
    val txdb = new TxStorage()
    txdb.save("Tinit", tinit)
    txdb.save("T1", t1)

    val a_pub = PublicKey(ByteVector.fromValidHex("03ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c3"))
    val a_p = Participant("A", List(a_pub), Address("akka", "test", "127.0.0.1", 25000))
    val b_pub = PublicKey(ByteVector.fromValidHex("03859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e"))
    val b_p = Participant("B", List(b_pub), Address("akka", "test", "127.0.0.1", 25001))
    val partdb = new ParticipantStorage()
    partdb.save(a_p)
    partdb.save(b_p)

    val tinit0_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2PKH, chunkPrivacy= ChunkPrivacy.AUTH, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty))
    val tinit1_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2PKH, chunkPrivacy= ChunkPrivacy.AUTH, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty))
    val t1_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(a_pub), data = ByteVector.empty))
    val tinit_entry = TxEntry(name = "Tinit", indexData = Map(
      0 -> IndexEntry(amt = Satoshi(1000000) ,chunkData = tinit0_chunks),
      1 -> IndexEntry(amt = Satoshi(100000) ,chunkData = tinit1_chunks)))
    val t1_entry = TxEntry(name = "T1", indexData = Map(0 -> IndexEntry(amt = Satoshi(1070001) ,chunkData = t1_chunks)))

    val metadb = new MetaStorage()
    metadb.save(tinit_entry)
    metadb.save(t1_entry)

    val initialState = State(partdb, txdb, metadb)
    val stateJson = new Serializer().prettyPrintState(initialState)
    println(stateJson)
  }

  test("Two player lottery: Blank state, B view and A view") {
    val a_priv = PrivateKey.fromBase58("cSthBXr8YQAexpKeh22LB9PdextVE1UJeahmyns5LzcmMDSy59L4", Base58.Prefix.SecretKeyTestnet)._1
    val a_pub = a_priv.publicKey
    val alice_p = Participant("Alice", List(a_pub), Address("akka", "test", "127.0.0.1", 25000))
    val b_priv = PrivateKey.fromBase58("cQmSz3Tj3usor9byskhpCTfrmCM5cLetLU9Xw6y2csYhxSbKDzUn", Base58.Prefix.SecretKeyTestnet)._1
    val b_pub = b_priv.publicKey
    val bob_p = Participant("Bob", List(b_pub), Address("akka", "test", "127.0.0.1", 25001))

    val partdb = new ParticipantStorage()
    partdb.save(alice_p)
    partdb.save(bob_p)

    val tinit_raw = Transaction.read("02000000034ebd9948a3b44846de67815e6cdf32edf2924dad4c03f62276fcced295f997f90100000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffffe46349e58df3e00db58938cd5f9ffcda1fad16afb23f908f2eee9ac93d78a1480100000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffffcf7c7752c144dfc8093918ba01c4d3d8a13107583f5f4502d211a651266dbcc30000000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff01cff417000000000017a9142e38f05e636989a28f71fe9be443f82e9ce3453e8700000000")
    val t1_raw = Transaction.read("0200000001018e004e075be6692fa53694c6cc0811b9c54d1f7d2392c1eb35ceeb9fb92a5e000000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff0335d507000000000017a9145d47e7c94f98c7462298a1fd74438e2edc9bdc488735d507000000000017a9145c77a7884016e20b07be95bcfab9f63a5926e92e8735d507000000000017a914d5944b405abb7dc532047245952eca7bd9116a6e8700000000")
    val t2_raw = Transaction.read("0200000001e8da9a5415f883258e156d96c0e758e66c55c55245838878f431050915d1a61600000000f10000004cec6b6b766b827c75028000940052a5636c766ba914c51b66bced5e4491001bd702669770dccf44098287670068636c766b827c75028000a267006863006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae670068635167006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae68ffffffff01056007000000000017a9142e38f05e636989a28f71fe9be443f82e9ce3453e8700000000")
    val t3_raw = Transaction.read("0200000001fa6c2e1e1402a9399518baff5936f31cb6ab13ed6b2ed247bf0a301ef9e123bd000000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff01d5ea0600000000001976a914ba91ed34ad92a7c2aa2d764c73cd0f31a18df68088ac00000000")
    val t4_raw = Transaction.read("0200000001e8da9a5415f883258e156d96c0e758e66c55c55245838878f431050915d1a61600000000f10000004cec6b6b766b827c75028000940052a5636c766ba914c51b66bced5e4491001bd702669770dccf44098287670068636c766b827c75028000a267006863006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae670068635167006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae68feffffff0105600700000000001976a914448f9bd84d520fb00adb83f26d8a78ddc5403c8988ac60e31600")
    val t5_raw = Transaction.read("0200000001e8da9a5415f883258e156d96c0e758e66c55c55245838878f431050915d1a61601000000e00000004cdb6b6b766ba914b472a266d0bd89c13706a4132ccfb16f7c3b9fcb87636c766b827c75028000a267006863006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae670068635167006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae68ffffffff01056007000000000017a9142e38f05e636989a28f71fe9be443f82e9ce3453e8700000000")
    val t6_raw = Transaction.read("0200000001e03c982aab19cdeb209277f1031a09961240508af78245bc3ec86d08070cca76000000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff01d5ea0600000000001976a914448f9bd84d520fb00adb83f26d8a78ddc5403c8988ac00000000")
    val t7_raw = Transaction.read("0200000001e8da9a5415f883258e156d96c0e758e66c55c55245838878f431050915d1a61601000000e00000004cdb6b6b766ba914b472a266d0bd89c13706a4132ccfb16f7c3b9fcb87636c766b827c75028000a267006863006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae670068635167006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae68feffffff0105600700000000001976a914ba91ed34ad92a7c2aa2d764c73cd0f31a18df68088ac60e31600")
    val t8_raw = Transaction.read("0200000001e8da9a5415f883258e156d96c0e758e66c55c55245838878f431050915d1a61602000000fd0702000000004d00026b6b6b766b827c756c6c766b7c6b827c7587636c766ba914b472a266d0bd89c13706a4132ccfb16f7c3b9fcb87670068636c766b827c75028000a2670068636c6c766b7c6ba914c51b66bced5e4491001bd702669770dccf44098287670068636c6c766b7c6b827c75028000a267006863006c6c6c766b7c6b7c6c6c6c766b7c6b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae6700686351676c766b827c756c6c766b7c6b827c758791636c766ba914b472a266d0bd89c13706a4132ccfb16f7c3b9fcb87670068636c766b827c75028000a2670068636c6c766b7c6ba914c51b66bced5e4491001bd702669770dccf44098287670068636c6c766b7c6b827c75028000a267006863006c6c6c766b7c6b7c6c6c6c766b7c6b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae67006868635167006c6c6c766b7c6b7c6c6c6c766b7c6b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae68ffffffff01056007000000000017a9142e38f05e636989a28f71fe9be443f82e9ce3453e8700000000")
    val t9_raw = Transaction.read("0200000001ccacc8726092c9b2fddc9a082c40aff468eaa42f89b4f438933f702403a40f05000000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff01d5ea0600000000001976a914448f9bd84d520fb00adb83f26d8a78ddc5403c8988ac00000000")
    val t10_raw = Transaction.read("0200000001e8da9a5415f883258e156d96c0e758e66c55c55245838878f431050915d1a61602000000fd0702000000004d00026b6b6b766b827c756c6c766b7c6b827c7587636c766ba914b472a266d0bd89c13706a4132ccfb16f7c3b9fcb87670068636c766b827c75028000a2670068636c6c766b7c6ba914c51b66bced5e4491001bd702669770dccf44098287670068636c6c766b7c6b827c75028000a267006863006c6c6c766b7c6b7c6c6c6c766b7c6b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae6700686351676c766b827c756c6c766b7c6b827c758791636c766ba914b472a266d0bd89c13706a4132ccfb16f7c3b9fcb87670068636c766b827c75028000a2670068636c6c766b7c6ba914c51b66bced5e4491001bd702669770dccf44098287670068636c6c766b7c6b827c75028000a267006863006c6c6c766b7c6b7c6c6c6c766b7c6b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae67006868635167006c6c6c766b7c6b7c6c6c6c766b7c6b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae68ffffffff01056007000000000017a9142e38f05e636989a28f71fe9be443f82e9ce3453e8700000000")
    val t11_raw = Transaction.read("0200000001ccacc8726092c9b2fddc9a082c40aff468eaa42f89b4f438933f702403a40f05000000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff01d5ea0600000000001976a914ba91ed34ad92a7c2aa2d764c73cd0f31a18df68088ac00000000")
    val t12_raw = Transaction.read("0200000001e8da9a5415f883258e156d96c0e758e66c55c55245838878f431050915d1a61602000000fd0702000000004d00026b6b6b766b827c756c6c766b7c6b827c7587636c766ba914b472a266d0bd89c13706a4132ccfb16f7c3b9fcb87670068636c766b827c75028000a2670068636c6c766b7c6ba914c51b66bced5e4491001bd702669770dccf44098287670068636c6c766b7c6b827c75028000a267006863006c6c6c766b7c6b7c6c6c6c766b7c6b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae6700686351676c766b827c756c6c766b7c6b827c758791636c766ba914b472a266d0bd89c13706a4132ccfb16f7c3b9fcb87670068636c766b827c75028000a2670068636c6c766b7c6ba914c51b66bced5e4491001bd702669770dccf44098287670068636c6c766b7c6b827c75028000a267006863006c6c6c766b7c6b7c6c6c6c766b7c6b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae67006868635167006c6c6c766b7c6b7c6c6c6c766b7c6b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae68feffffff0202b003000000000017a9142e38f05e636989a28f71fe9be443f82e9ce3453e8702b003000000000017a9142e38f05e636989a28f71fe9be443f82e9ce3453e8760e31600")
    val t13_raw = Transaction.read("0200000001d150840b51b1a7c5048e1be26e44e85c289c31d9f9d32d3c3d335c319257f8c4000000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff01d23a0300000000001976a914448f9bd84d520fb00adb83f26d8a78ddc5403c8988ac00000000")
    val t14_raw = Transaction.read("0200000001d150840b51b1a7c5048e1be26e44e85c289c31d9f9d32d3c3d335c319257f8c4010000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff01d23a0300000000001976a914ba91ed34ad92a7c2aa2d764c73cd0f31a18df68088ac00000000")

    val txdb = new TxStorage()
    txdb.save("Tinit", tinit_raw)
    txdb.save("T1", t1_raw)
    txdb.save("T2", t2_raw)
    txdb.save("T3", t3_raw)
    txdb.save("T4", t4_raw)
    txdb.save("T5", t5_raw)
    txdb.save("T6", t6_raw)
    txdb.save("T7", t7_raw)
    txdb.save("T8", t8_raw)
    txdb.save("T9", t9_raw)
    txdb.save("T10", t10_raw)
    txdb.save("T11", t11_raw)
    txdb.save("T12", t12_raw)
    txdb.save("T13", t13_raw)
    txdb.save("T14", t14_raw)

    val tinit0_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2PKH, chunkPrivacy= ChunkPrivacy.AUTH, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty))
    val tinit1_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2PKH, chunkPrivacy= ChunkPrivacy.AUTH, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty))
    val tinit2_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2PKH, chunkPrivacy= ChunkPrivacy.AUTH, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty))
    val t1_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(a_pub), data = ByteVector.empty)
    )
    // First split: B secret
    val t2_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PRIVATE, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 2, owner = Option(a_pub), data = ByteVector.empty)
    )
    val t3_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(a_pub), data = ByteVector.empty)
    )
    val t4_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PRIVATE, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.fromValidHex("00")),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 2, owner = Option(a_pub), data = ByteVector.empty)
    )
    // Second split: A secret
    val t5_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PRIVATE, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 2, owner = Option(a_pub), data = ByteVector.empty)
    )
    val t6_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(a_pub), data = ByteVector.empty)
    )
    val t7_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PRIVATE, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.fromValidHex("00")),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 2, owner = Option(a_pub), data = ByteVector.empty)
    )
    // Third split: match sec_a sec_b.
    // TODO: Should each participant grab the opposite participant's secret from T2/T5 respectively? (currently unimplemented RPC lookup)
    //  Or can we trust to just ask them? (switch private blocks in t8 and t10 to public)
    // Temporarily implemented the latter
    val t8_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 2, owner = Option(a_pub), data = ByteVector.empty)
    )
    val t9_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(a_pub), data = ByteVector.empty)
    )
    val t10_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 2, owner = Option(a_pub), data = ByteVector.empty)
    )
    val t11_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(a_pub), data = ByteVector.empty)
    )
    val t12_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.fromValidHex("00")),
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.fromValidHex("00")),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 2, owner = Option(a_pub), data = ByteVector.empty)
    )
    val t13_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(a_pub), data = ByteVector.empty)
    )
    val t14_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(a_pub), data = ByteVector.empty)
    )

    val tinit_entry = TxEntry(name = "Tinit", indexData = Map(
      0 -> IndexEntry(amt = Satoshi(300000) ,chunkData = tinit0_chunks),
      1 -> IndexEntry(amt = Satoshi(300000) ,chunkData = tinit1_chunks),
      2 -> IndexEntry(amt = Satoshi(1000000) ,chunkData = tinit2_chunks)))
    val t1_entry = TxEntry(name = "T1", indexData = Map(0 -> IndexEntry(amt = Satoshi(1569999) ,chunkData = t1_chunks)))
    val t2_entry = TxEntry(name = "T2", indexData = Map(0 -> IndexEntry(amt = Satoshi(513333) ,chunkData = t2_chunks)))
    val t3_entry = TxEntry(name = "T3", indexData = Map(0 -> IndexEntry(amt = Satoshi(483333) ,chunkData = t3_chunks)))
    val t4_entry = TxEntry(name = "T4", indexData = Map(0 -> IndexEntry(amt = Satoshi(513333) ,chunkData = t4_chunks)))
    val t5_entry = TxEntry(name = "T5", indexData = Map(0 -> IndexEntry(amt = Satoshi(513333) ,chunkData = t5_chunks)))
    val t6_entry = TxEntry(name = "T6", indexData = Map(0 -> IndexEntry(amt = Satoshi(483333) ,chunkData = t6_chunks)))
    val t7_entry = TxEntry(name = "T7", indexData = Map(0 -> IndexEntry(amt = Satoshi(513333) ,chunkData = t7_chunks)))
    val t8_entry = TxEntry(name = "T8", indexData = Map(0 -> IndexEntry(amt = Satoshi(513333) ,chunkData = t8_chunks)))
    val t9_entry = TxEntry(name = "T9", indexData = Map(0 -> IndexEntry(amt = Satoshi(483333) ,chunkData = t9_chunks)))
    val t10_entry = TxEntry(name = "T10", indexData = Map(0 -> IndexEntry(amt = Satoshi(513333) ,chunkData = t10_chunks)))
    val t11_entry = TxEntry(name = "T11", indexData = Map(0 -> IndexEntry(amt = Satoshi(483333) ,chunkData = t11_chunks)))
    val t12_entry = TxEntry(name = "T12", indexData = Map(0 -> IndexEntry(amt = Satoshi(513333) ,chunkData = t12_chunks)))
    val t13_entry = TxEntry(name = "T13", indexData = Map(0 -> IndexEntry(amt = Satoshi(241666) ,chunkData = t13_chunks)))
    val t14_entry = TxEntry(name = "T14", indexData = Map(0 -> IndexEntry(amt = Satoshi(241666) ,chunkData = t14_chunks)))

    val metadb = new MetaStorage()
    metadb.save(tinit_entry)
    metadb.save(t1_entry)
    metadb.save(t2_entry)
    metadb.save(t3_entry)
    metadb.save(t4_entry)
    metadb.save(t5_entry)
    metadb.save(t6_entry)
    metadb.save(t7_entry)
    metadb.save(t8_entry)
    metadb.save(t9_entry)
    metadb.save(t10_entry)
    metadb.save(t11_entry)
    metadb.save(t12_entry)
    metadb.save(t13_entry)
    metadb.save(t14_entry)

    val stateSerializer =  new Serializer(ChunkPrivacy.PRIVATE)
    val blankState = stateSerializer.prettyPrintState(State(partdb, txdb, metadb))
    println("Blank state:\n%s" format blankState)

    // Build B view
    val b_secret = ByteVector.fromValidHex("3030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303031")

    val t2_b = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PRIVATE, chunkIndex = 0, owner = Option(b_pub), data = b_secret),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 2, owner = Option(a_pub), data = ByteVector.empty)
    )
    // T8 and T10 might look different depending on the note above.
    val t8_b = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = b_secret),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 2, owner = Option(a_pub), data = ByteVector.empty)
    )
    val t10_b = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = b_secret),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 2, owner = Option(a_pub), data = ByteVector.empty)
    )
    val t2_entry_b = TxEntry(name = "T2", indexData = Map(0 -> IndexEntry(amt = Satoshi(513333) ,chunkData = t2_b)))
    val t8_entry_b = TxEntry(name = "T8", indexData = Map(0 -> IndexEntry(amt = Satoshi(513333) ,chunkData = t8_b)))
    val t10_entry_b = TxEntry(name = "T10", indexData = Map(0 -> IndexEntry(amt = Satoshi(513333) ,chunkData = t10_b)))
    metadb.save(t2_entry_b)
    metadb.save(t8_entry_b)
    metadb.save(t10_entry_b)

    val b_view = stateSerializer.prettyPrintState(State(partdb, txdb, metadb))
    println("B view:\n%s" format b_view)

    // Build A view
    val a_secret = ByteVector.fromValidHex("3030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030")

    val t5_a = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PRIVATE, chunkIndex = 0, owner = Option(a_pub), data = a_secret),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 2, owner = Option(a_pub), data = ByteVector.empty)
    )
    // T8 and T10 might look different depending on the note above.
    val t8_a = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(a_pub), data = a_secret),
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 2, owner = Option(a_pub), data = ByteVector.empty)
    )
    val t10_a = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(a_pub), data = a_secret),
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 2, owner = Option(a_pub), data = ByteVector.empty)
    )
    val t5_entry_a = TxEntry(name = "T5", indexData = Map(0 -> IndexEntry(amt = Satoshi(513333) ,chunkData = t5_a)))
    val t8_entry_a = TxEntry(name = "T8", indexData = Map(0 -> IndexEntry(amt = Satoshi(513333) ,chunkData = t8_a)))
    val t10_entry_a = TxEntry(name = "T10", indexData = Map(0 -> IndexEntry(amt = Satoshi(513333) ,chunkData = t10_a)))
    // Revert content of t2 and save content of new t5-8-10
    metadb.save(t2_entry)
    metadb.save(t5_entry_a)
    metadb.save(t8_entry_a)
    metadb.save(t10_entry_a)

    val a_view = stateSerializer.prettyPrintState(State(partdb, txdb, metadb))
    println("A view:\n%s" format a_view)
  }

  test("Timed committment, blank/B view and A view") {
    val a_priv = PrivateKey.fromBase58("cSthBXr8YQAexpKeh22LB9PdextVE1UJeahmyns5LzcmMDSy59L4", Base58.Prefix.SecretKeyTestnet)._1
    val a_pub = a_priv.publicKey
    val alice_p = Participant("Alice", List(a_pub), Address("akka", "test", "127.0.0.1", 25000))
    val b_priv = PrivateKey.fromBase58("cQmSz3Tj3usor9byskhpCTfrmCM5cLetLU9Xw6y2csYhxSbKDzUn", Base58.Prefix.SecretKeyTestnet)._1
    val b_pub = b_priv.publicKey
    val bob_p = Participant("Bob", List(b_pub), Address("akka", "test", "127.0.0.1", 25001))

    val partdb = new ParticipantStorage()
    partdb.save(alice_p)
    partdb.save(bob_p)

    val tinit_raw = Transaction.read("02000000023dd3091af13f0948d53fb1a4f762c69fdb76b2f190c797dc62ccae23357c09f70000000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff592e502d6c7cf9efc5b3cf82025307c524dcdb13f0330fcffe418662844cf2080000000023002102a6d35321c8930c1da17df79edebaf13192ee3e39c9abcea6d8dd9c5f3640e2abffffffff017a600d000000000017a914e2fc356d35e4759c282c9c49a39a3e8fd6f756d88700000000")
    val t1_raw = Transaction.read("02000000010fd0b8349e05f2c657c3af3a4f1e6e9d933f5bd75866e62dc55763deb583979300000000e00000004cdb6b6b766ba9149f3df038eeadc0c240fb7f82e31fdfe46804fc7c87636c766b827c75028000a267006863006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae670068635167006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae68ffffffff014aeb0c000000000017a9142e38f05e636989a28f71fe9be443f82e9ce3453e8700000000")
    val t2_raw = Transaction.read("0200000001d0621d198f21c889f732d71173c730da32cbc50698cc9cb68525bb27831adc43000000005500004c516b6b006c766c766b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352aeffffffff011a760c00000000001976a914448f9bd84d520fb00adb83f26d8a78ddc5403c8988ac00000000")
    val t3_raw = Transaction.read("02000000010fd0b8349e05f2c657c3af3a4f1e6e9d933f5bd75866e62dc55763deb583979300000000e1013000004cdb6b6b766ba9149f3df038eeadc0c240fb7f82e31fdfe46804fc7c87636c766b827c75028000a267006863006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae670068635167006c6c766b7c6c6c766b7c6b7c6b522103859a0f601cf485a72ec097fddd798c694b0257f69f0229506f8ea923bc600c5e2103ff41f23b70b1c83b01914eb223d7a97a6c2b24e9a9ef2762bf25ed1c1b83c9c352ae68feffffff014aeb0c00000000001976a914ba91ed34ad92a7c2aa2d764c73cd0f31a18df68088acb0a61700")

    val txdb = new TxStorage()
    txdb.save("Tinit", tinit_raw)
    txdb.save("T1", t1_raw)
    txdb.save("T2", t2_raw)
    txdb.save("T3", t3_raw)

    val tinit0_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2PKH, chunkPrivacy= ChunkPrivacy.AUTH, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty))
    val tinit1_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2PKH, chunkPrivacy= ChunkPrivacy.AUTH, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty))
    val t1_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PRIVATE, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 2, owner = Option(a_pub), data = ByteVector.empty))
    val t2_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 0, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(a_pub), data = ByteVector.empty))
    val t3_chunks = Seq(
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 2, owner = Option(a_pub), data = ByteVector.empty))

    val tinit_entry = TxEntry(name = "Tinit", indexData = Map(
      0 -> IndexEntry(amt = Satoshi(453333) ,chunkData = tinit0_chunks),
      1 -> IndexEntry(amt = Satoshi(453333) ,chunkData = tinit1_chunks)))
    val t1_entry = TxEntry(name = "T1", indexData = Map(0 -> IndexEntry(amt = Satoshi(876666) ,chunkData = t1_chunks)))
    val t2_entry = TxEntry(name = "T2", indexData = Map(0 -> IndexEntry(amt = Satoshi(846666) ,chunkData = t2_chunks)))
    val t3_entry = TxEntry(name = "T3", indexData = Map(0 -> IndexEntry(amt = Satoshi(876666) ,chunkData = t3_chunks)))

    val metadb = new MetaStorage()
    metadb.save(tinit_entry)
    metadb.save(t1_entry)
    metadb.save(t2_entry)
    metadb.save(t3_entry)

    val blankState = new Serializer(ChunkPrivacy.PRIVATE).prettyPrintState(State(partdb, txdb, metadb))
    println("Blank state:\n%s" format blankState)

    // Edit chunk with secret information for test convenience. In a real use case this would either be interactive or baked into the state JSON
    val t1_chunks_secret = Seq(
      ChunkEntry(chunkType = ChunkType.SECRET_IN, chunkPrivacy= ChunkPrivacy.PRIVATE, chunkIndex = 0, owner = Option(a_pub), data = ByteVector.fromValidHex("303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303031")),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 1, owner = Option(b_pub), data = ByteVector.empty),
      ChunkEntry(chunkType = ChunkType.SIG_P2SH, chunkPrivacy= ChunkPrivacy.PUBLIC, chunkIndex = 2, owner = Option(a_pub), data = ByteVector.empty))
    val t1_entry_secret = TxEntry(name = "T1", indexData = Map(0 -> IndexEntry(amt = Satoshi(876666) ,chunkData = t1_chunks_secret)))
    metadb.save(t1_entry_secret)

    // If we don't specify the higher visibility access, the secret is stripped anyway.
    val a_view = new Serializer(ChunkPrivacy.PRIVATE).prettyPrintState(State(partdb, txdb, metadb))
    println("A view:\n%s" format a_view)
  }
}