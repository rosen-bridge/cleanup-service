package rosen.bridge

object Scripts {

  lazy val WatcherBankScript: String =
    s"""{
       |  // R4: Coll[Coll[Byte]] = first element in R4 is chain id bytes and the rest is watchers UTP
       |  // R5: Coll[Long] = first element is zero and every element indicates EWR tokens count for UTP index i
       |  // R6: Coll[Long] = [RSN to EWR Factor, watcher approve percent, constant value for approve, maximum approve count] approve formula is min(R6[3], R6[1] * (len(R4) - 1) / 100 + R6[2])
       |  // R7: Int = in case of unlock indicates UTP index in R4 otherwise not used
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
       |  // R4: Coll[Coll[Byte]] = only one element display user UTP id.
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
       |          OUTPUTS(1).R5[Coll[Coll[Byte]]].isDefined,
       |          OUTPUTS(1).R6[Coll[Byte]].isDefined,
       |          OUTPUTS(1).R7[Coll[Byte]].get == blake2b256(SELF.propositionBytes),
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
       |  // R4: Coll[Coll[Byte]] = Only one element indicates user UTP
       |  // R5: Coll[Coll[Byte]] = Request ID = Hash(TxId)
       |  // R6: Coll[Byte] = Commitment Hash
       |  // R7: Coll[Byte] = Lock Script Address Hash
       |  val BankNFT = fromBase64("BANK_NFT");
       |  val TriggerEventHash = fromBase64("TRIGGER_EVENT_SCRIPT_HASH");
       |  val event = if (blake2b256(INPUTS(0).propositionBytes) == TriggerEventHash) INPUTS(0) else OUTPUTS(0)
       |  val myUTP = SELF.R4[Coll[Coll[Byte]]].get
       |  val UTPs = event.R4[Coll[Coll[Byte]]].get
       |  val paddedCommitment = if(event.R5[Coll[Coll[Byte]]].isDefined) {
       |    event.R5[Coll[Coll[Byte]]].get.fold(Coll(0.toByte), { (a: Coll[Byte], b: Coll[Byte]) => a ++ b } )
       |  }else{
       |    Coll(0.toByte)
       |  }
       |  val commitment = paddedCommitment.slice(1, paddedCommitment.size)
       |  if(blake2b256(INPUTS(0).propositionBytes) == TriggerEventHash){
       |    // admin payment of fee
       |    val userLockBox = OUTPUTS.filter {(box:Box) => if(box.R4[Coll[Coll[Byte]]].isDefined) box.R4[Coll[Coll[Byte]]].get == SELF.R4[Coll[Coll[Byte]]].get else false }(0)
       |    val UTPExists =  UTPs.exists {(UTP: Coll[Byte]) => myUTP == Coll(UTP)}
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          blake2b256(userLockBox.propositionBytes) == SELF.R7[Coll[Byte]].get,
       |          userLockBox.tokens(0)._1 == SELF.tokens(0)._1,
       |          // check for duplicates
       |          UTPExists == false,
       |          // validate commitment
       |          blake2b256(commitment ++ myUTP(0)) == SELF.R6[Coll[Byte]].get
       |        )
       |      )
       |    )
       |
       |  } else if (blake2b256(OUTPUTS(0).propositionBytes) == TriggerEventHash){
       |    // merge events
       |    // [Commitment1, Commitment2, Commitment3, ...,] + [Bank(DataInput)] => [TriggerEvent]
       |    val commitmentBoxes = INPUTS.filter { (box: Box) => SELF.propositionBytes == box.propositionBytes }
       |    val myUTPCommitments = commitmentBoxes.filter{ (box: Box) => box.R4[Coll[Coll[Byte]]].get == myUTP }
       |    val myUTPExists = UTPs.exists{ (UTP: Coll[Byte]) => Coll(UTP) == myUTP }
       |    val bank = CONTEXT.dataInputs(0)
       |    val requestId = if(event.R5[Coll[Coll[Byte]]].isDefined) {
       |      blake2b256(event.R5[Coll[Coll[Byte]]].get(0))
       |    } else {
       |      Coll(0.toByte)
       |    }
       |    val bankR6 = bank.R6[Coll[Long]].get
       |    val maxCommitment = bankR6(3)
       |    val requiredCommitmentFromFormula: Long = bankR6(2) + bankR6(1) * (bank.R4[Coll[Coll[Byte]]].get.size - 1L) / 100L
       |    val requiredCommitment = if(maxCommitment < requiredCommitmentFromFormula) {
       |      maxCommitment
       |    } else {
       |      requiredCommitmentFromFormula
       |    }
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          myUTPCommitments.size == 1,
       |          myUTPExists,
       |          event.R6[Coll[Byte]].get == SELF.R7[Coll[Byte]].get,
       |          UTPs.size == commitmentBoxes.size,
       |          // TODO verify commitment to be correct
       |          blake2b256(commitment ++ myUTP(0)) == SELF.R6[Coll[Byte]].get,
       |          // check event id
       |          SELF.R5[Coll[Coll[Byte]]].get == Coll(requestId),
       |          // check commitment count
       |          commitmentBoxes.size > requiredCommitment,
       |        )
       |      )
       |    )
       |  } else {
       |    // token has been redeem
       |    // [Commitment, UTP] => [Lock]
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          SELF.id == INPUTS(0).id,
       |          OUTPUTS(0).tokens(0)._1 == SELF.tokens(0)._1,
       |          OUTPUTS(0).tokens(0)._2 == SELF.tokens(0)._2, // not required because only one token available
       |          // check UTP copied
       |          OUTPUTS(0).R4[Coll[Coll[Byte]]].get == SELF.R4[Coll[Coll[Byte]]].get,
       |          // check user UTP
       |          Coll(INPUTS(1).tokens(0)._1) == SELF.R4[Coll[Coll[Byte]]].get,
       |          // check lock contract address
       |          blake2b256(OUTPUTS(0).propositionBytes) == SELF.R7[Coll[Byte]].get
       |        )
       |      )
       |    )
       |  }
       |}""".stripMargin

  lazy val WatcherTriggerEventScript: String =
    s"""{
       |  // R4: Coll[Coll[Byte]] a list contain UTP of merged events
       |  // R5: Coll[Coll[Byte]] event data
       |  // R6: Coll[Byte] lock contract script hash
       |  // [TriggerEvent, CleanupToken(if fraud)] => [Fraud1, Fraud2, ...]
       |  val cleanupNFT = fromBase64("CLEANUP_NFT");
       |  val guardNFT = fromBase64("GUARD_NFT");
       |  val cleanupConfirmation = CLEANUP_CONFIRMATION;
       |  val FraudScriptHash = fromBase64("FRAUD_SCRIPT_HASH");
       |  val fraudScriptCheck = if(blake2b256(OUTPUTS(0).propositionBytes) == FraudScriptHash) {
       |    allOf(
       |      Coll(
       |        INPUTS(1).tokens(0)._1 == cleanupNFT,
       |        HEIGHT - cleanupConfirmation >= SELF.creationInfo._1
       |      )
       |    )
       |  } else {
       |    allOf(
       |      Coll(
       |        INPUTS(1).tokens(0)._1 == guardNFT,
       |        blake2b256(OUTPUTS(0).propositionBytes) == SELF.R6[Coll[Byte]].get
       |      )
       |    )
       |  }
       |  val UTPs: Coll[Coll[Byte]] = SELF.R4[Coll[Coll[Byte]]].get
       |  val mergeBoxes = OUTPUTS.slice(0, UTPs.size)
       |  val checkAllUTPs = UTPs.zip(mergeBoxes).forall {
       |    (data: (Coll[Byte], Box)) => {
       |      Coll(data._1) == data._2.R4[Coll[Coll[Byte]]].get && data._2.propositionBytes == OUTPUTS(0).propositionBytes
       |    }
       |  }
       |  sigmaProp(
       |    allOf(
       |      Coll(
       |        UTPs.size == mergeBoxes.size,
       |        checkAllUTPs,
       |        fraudScriptCheck,
       |      )
       |    )
       |  )
       |}""".stripMargin

  lazy val WatcherFraudLockScript: String =
    s"""{
       |  // R4: Coll[Coll[Byte]] = only one element display user UTP id. used to update configuration box
       |  // [Bank, Fraud, Cleanup] => [Bank]
       |  val BankNFT = fromBase64("BANK_NFT");
       |  val CleanupNFT = fromBase64("CLEANUP_NFT");
       |  val OutputWithToken = OUTPUTS.slice(1, OUTPUTS.size).filter { (box: Box) => box.tokens.size > 0 }
       |  val OutputWithEWR = OutputWithToken.exists { (box: Box) => box.tokens.exists { (token: (Coll[Byte], Long)) => token._1 == SELF.tokens(0)._1 } }
       |  // Lock or unlock operation. [Bank, Lock, UTP] => [Bank, Lock(optional)]
       |  sigmaProp(
       |    allOf(
       |      Coll(
       |        OutputWithEWR == false,
       |        INPUTS(0).tokens(0)._1 == BankNFT,
       |        INPUTS(2).tokens(0)._1 == CleanupNFT,
       |      )
       |    )
       |  )
       |}""".stripMargin

}
