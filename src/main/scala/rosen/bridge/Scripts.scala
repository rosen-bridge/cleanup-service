package rosen.bridge

object Scripts {

  lazy val WatcherBankScript: String =
    s"""{
       |  val GuardToken = fromBase64("GUARD_TOKEN");
       |  if(INPUTS(1).tokens(0)._1 == GuardToken){
       |    sigmaProp(true)
       |  } else {
       |    val lockScriptHash = fromBase64("LOCK_SCRIPT_HASH");
       |    val bankOut = OUTPUTS(0)
       |    val bank = SELF
       |    val bankListSize = bank.R5[Coll[Long]].get.size
       |    val bankOutListSize = bankOut.R5[Coll[Long]].get.size
       |    val bankReplication = allOf(
       |      Coll(
       |        bankOut.propositionBytes == bank.propositionBytes,
       |        bankOut.R6[Coll[Long]].get == bank.R6[Coll[Long]].get,
       |        bankOut.tokens(0)._1 == bank.tokens(0)._1,
       |        bankOut.tokens(1)._1 == bank.tokens(1)._1,
       |        bankOut.tokens(2)._1 == bank.tokens(2)._1,
       |      )
       |    )
       |    if(bank.tokens(1)._2 > bankOut.tokens(1)._2){
       |      // Locking RSN.
       |      // [Bank, UserInputs] => [Bank, LockBox, UserUTP]
       |      val lock = OUTPUTS(1)
       |      val UTP = OUTPUTS(2)
       |      val EWRTokenOut = bank.tokens(1)._2 - bankOut.tokens(1)._2
       |      sigmaProp(
       |        allOf(
       |          Coll(
       |            bankReplication,
       |            bankOut.R4[Coll[Coll[Byte]]].get.size == bankListSize + 1,
       |            bankOut.R4[Coll[Coll[Byte]]].get.slice(0, bankOutListSize - 1) == bank.R4[Coll[Coll[Byte]]].get,
       |            bankOut.R4[Coll[Coll[Byte]]].get(bankOutListSize - 1) == bank.id,
       |            bankOut.R5[Coll[Long]].get.size == bankListSize + 1,
       |            bankOut.R5[Coll[Long]].get.slice(0, bankOutListSize - 1) == bank.R5[Coll[Long]].get,
       |            bankOut.R5[Coll[Long]].get(bankOutListSize - 1) == EWRTokenOut,
       |            EWRTokenOut * bank.R6[Coll[Long]].get(0) == bankOut.tokens(2)._2 - bank.tokens(2)._2,
       |            lock.tokens(0)._2 == EWRTokenOut,
       |            blake2b256(lock.propositionBytes) == lockScriptHash,
       |            lock.R4[Coll[Coll[Byte]]].get == Coll(bank.id),
       |            UTP.tokens(0)._1 == bank.id
       |          )
       |        )
       |      )
       |    }else{
       |      // Unlock RSN
       |      // [Bank, EWR] => [Bank]
       |      val locked = INPUTS(1)
       |      val EWRTokenIn = bankOut.tokens(1)._2 - bank.tokens(1)._2
       |      val UTPIndex = bankOut.R7[Int].get
       |      val lockedSize = bank.R5[Coll[Long]].get.size
       |      val UTPCheckInBank = if(bank.R5[Coll[Long]].get(UTPIndex) > EWRTokenIn) {
       |        allOf(
       |          Coll(
       |            bank.R5[Coll[Long]].get(UTPIndex) == bankOut.R5[Coll[Long]].get(UTPIndex) + EWRTokenIn,
       |            bank.R4[Coll[Coll[Byte]]].get == bankOut.R4[Coll[Coll[Byte]]].get
       |          )
       |        )
       |      }else{
       |        allOf(
       |          Coll(
       |            bank.R5[Coll[Long]].get(UTPIndex) == EWRTokenIn,
       |            bank.R4[Coll[Coll[Byte]]].get.slice(0, UTPIndex) == bankOut.R4[Coll[Coll[Byte]]].get.slice(0, UTPIndex),
       |            bank.R4[Coll[Coll[Byte]]].get.slice(UTPIndex + 1, lockedSize) == bankOut.R4[Coll[Coll[Byte]]].get.slice(UTPIndex, lockedSize - 1),
       |            bank.R5[Coll[Long]].get.slice(0, UTPIndex) == bankOut.R5[Coll[Long]].get.slice(0, UTPIndex),
       |            bank.R5[Coll[Long]].get.slice(UTPIndex + 1, lockedSize) == bankOut.R5[Coll[Long]].get.slice(UTPIndex, lockedSize - 1)
       |          )
       |        )
       |      }
       |      val UTP = bank.R4[Coll[Coll[Byte]]].get(UTPIndex)
       |      sigmaProp(
       |        allOf(
       |          Coll(
       |            bankReplication,
       |            Coll(UTP) == locked.R4[Coll[Coll[Byte]]].get,
       |            EWRTokenIn * bank.R6[Coll[Long]].get(0) == bank.tokens(2)._2 - bankOut.tokens(2)._2,
       |            UTPCheckInBank
       |          )
       |        )
       |      )
       |    }
       |  }
       |}""".stripMargin

  lazy val WatcherLockScript: String =
    s"""{
       |  val BankNFT = fromBase64("BANK_NFT");
       |  val CommitmentScriptHash = fromBase64("COMMITMENT_SCRIPT_HASH");
       |  val OutputWithToken = OUTPUTS.slice(2, OUTPUTS.size).filter { (box: Box) => box.tokens.size > 0 }
       |  val OutputWithEWR = OutputWithToken.exists { (box: Box) => box.tokens.exists { (token: (Coll[Byte], Long)) => token._1 == SELF.tokens(0)._1 } }
       |  val SecondBoxHasEWR = OUTPUTS(1).tokens.exists { (token: (Coll[Byte], Long)) => token._1 == SELF.tokens(0)._1 }
       |  if(OUTPUTS(0).tokens(0)._1 == BankNFT){
       |    // Lock or unlock operation. [Bank, Lock, UTP] => [Bank, Lock(optional)]
       |    val SecondOutputLock = if(SecondBoxHasEWR){
       |      allOf(
       |        Coll(
       |          OUTPUTS(1).tokens(0)._1 == SELF.tokens(0)._1,
       |          OUTPUTS(1).propositionBytes == SELF.propositionBytes,
       |        )
       |      )
       |    }else{
       |      true
       |    }
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          OutputWithEWR == false,
       |          INPUTS(2).tokens(0)._1 == SELF.R4[Coll[Coll[Byte]]].get(0),
       |          SecondOutputLock,
       |        )
       |      )
       |    )
       |  }else{
       |    val SecondOutputCommitment = if(SecondBoxHasEWR){
       |      allOf(
       |        Coll(
       |          OUTPUTS(1).tokens(0)._1 == SELF.tokens(0)._1,
       |          blake2b256(OUTPUTS(1).propositionBytes) == CommitmentScriptHash,
       |          OUTPUTS(1).R4[Coll[Coll[Byte]]].get == SELF.R4[Coll[Coll[Byte]]].get,
       |          OUTPUTS(1).tokens(0)._2 == 1,
       |        )
       |      )
       |    }else{
       |      true
       |    }
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          OutputWithEWR == false,
       |          OUTPUTS(0).propositionBytes == SELF.propositionBytes,
       |          OUTPUTS(0).R4[Coll[Coll[Byte]]].get == SELF.R4[Coll[Coll[Byte]]].get,
       |          INPUTS(1).tokens(0)._1 == SELF.R4[Coll[Coll[Byte]]].get(0),
       |          SecondOutputCommitment,
       |        )
       |      )
       |    )
       |  }
       |}""".stripMargin

  lazy val WatcherCommitmentScript: String =
    s"""{
       |  val BankNFT = fromBase64("BANK_NFT");
       |
       |  sigmaProp(
       |    allOf(
       |      Coll(
       |        true
       |      )
       |    )
       |  )
       |}""".stripMargin

  lazy val WatcherTriggerEventScript: String =
    s"""{
       |  val BankNFT = fromBase64("BANK_NFT");
       |  sigmaProp(
       |    allOf(
       |      Coll(
       |        true
       |      )
       |    )
       |  )
       |}""".stripMargin

  lazy val WatcherFraudLockScript: String =
    s"""{
       |  val BankNFT = fromBase64("BANK_NFT");
       |  sigmaProp(
       |    allOf(
       |      Coll(
       |        true
       |      )
       |    )
       |  )
       |}""".stripMargin

}
