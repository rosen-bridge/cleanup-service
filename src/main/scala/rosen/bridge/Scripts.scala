package rosen.bridge

object Scripts {

  lazy val RwtRepoScript: String =
    s"""{
       |  // ----------------- REGISTERS
       |  // R4: Coll[Coll[Byte]] = [Chain id, WID_0, WID_1, ...] (Stores Chain id and related watcher ids)
       |  // R5: Coll[Long] = [0, X-RWT_0, X-RWT_1, ...] (The first element is zero and the rest indicates X-RWT count for watcher i)
       |  // R6: Coll[Long] = [Commitment RWT count, Watcher quorum percentage, minimum needed approval, maximum needed approval, Collateral Erg amount, Collateral Rsn Amount]
       |  // (Minimum number of commitments needed for an event is: min(R6[3], R6[1] * (len(R4) - 1) / 100 + R6[2]) )
       |  // R7: Int = Watcher index (only used in returning or extending permits)
       |  // ----------------- TOKENS
       |  // 0: X-RWT Repo NFT
       |  // 1: X-RWT
       |  // 2: RSN
       |
       |  val GuardNFT = fromBase64("GUARD_NFT");
       |  val watcherCollateralScriptHash = fromBase64("WATCHER_COLLATERAL_SCRIPT_HASH");
       |  if(OUTPUTS(0).tokens(0)._1 == GuardNFT){
       |    // RWT Repo Update transaction
       |    sigmaProp(true)
       |  } else {
       |    val permitScriptHash = fromBase64("PERMIT_SCRIPT_HASH");
       |    val repoOut = OUTPUTS(0)
       |    val repo = SELF
       |    val widListSize = repo.R5[Coll[Long]].get.size
       |    val widOutListSize = repoOut.R5[Coll[Long]].get.size
       |    val repoReplication = allOf(
       |      Coll(
       |        repoOut.propositionBytes == repo.propositionBytes,
       |        repoOut.R6[Coll[Long]].get == repo.R6[Coll[Long]].get,
       |        repoOut.tokens(0)._1 == repo.tokens(0)._1,
       |        repoOut.tokens(0)._2 == repo.tokens(0)._2,
       |        repoOut.tokens(1)._1 == repo.tokens(1)._1,
       |        repoOut.tokens(2)._1 == repo.tokens(2)._1,
       |      )
       |    )
       |    if(repo.tokens(1)._2 > repoOut.tokens(1)._2){
       |      // Getting Watcher Permit
       |      val WIDIndex = repoOut.R7[Int].getOrElse(-1)
       |      val permit = OUTPUTS(1)
       |      val outWIDBox = OUTPUTS(2)
       |      val RWTOut = repo.tokens(1)._2 - repoOut.tokens(1)._2
       |      val permitCreation = allOf(
       |        Coll(
       |          repoReplication,
       |          RWTOut == repoOut.tokens(2)._2 - repo.tokens(2)._2,
       |          permit.tokens(0)._2 == RWTOut,
       |          blake2b256(permit.propositionBytes) == permitScriptHash,
       |        )
       |      )
       |      if(WIDIndex == -1){
       |        // Getting initial permit
       |        // [Repo, UserInputs] => [Repo, watcherPermit, WIDBox, watcherCollateral]
       |        val watcherCollateral = OUTPUTS(3)
       |        sigmaProp(
       |          allOf(
       |            Coll(
       |              permitCreation,
       |              repoOut.R4[Coll[Coll[Byte]]].get.size == widListSize + 1,
       |              repoOut.R4[Coll[Coll[Byte]]].get.slice(0, widOutListSize - 1) == repo.R4[Coll[Coll[Byte]]].get,
       |              repoOut.R4[Coll[Coll[Byte]]].get(widOutListSize - 1) == repo.id,
       |              repoOut.R5[Coll[Long]].get.size == widListSize + 1,
       |              repoOut.R5[Coll[Long]].get.slice(0, widOutListSize - 1) == repo.R5[Coll[Long]].get,
       |              repoOut.R5[Coll[Long]].get(widOutListSize - 1) == RWTOut,
       |              permit.R4[Coll[Coll[Byte]]].get == Coll(repo.id),
       |              outWIDBox.tokens(0)._1 == repo.id,
       |              blake2b256(watcherCollateral.propositionBytes) == watcherCollateralScriptHash,
       |              watcherCollateral.R4[Coll[Byte]].get == repo.id,
       |              watcherCollateral.value >= repo.R6[Coll[Long]].get(4),
       |              if(repo.R6[Coll[Long]].get(5) > 0){
       |                allOf(
       |                  Coll(
       |                    watcherCollateral.tokens(0)._1 == repo.tokens(2)._1,
       |                    watcherCollateral.tokens(0)._2 >= repo.R6[Coll[Long]].get(5)
       |                  )
       |                )
       |              }else{
       |                true
       |              }
       |            )
       |          )
       |        )
       |      } else {
       |        // Extending Permit
       |        // [Repo, WIDBox] => [Repo, watcherPermit, WIDBox]
       |        val WID = repo.R4[Coll[Coll[Byte]]].get(WIDIndex)
       |        val currentRWT = repo.R5[Coll[Long]].get(WIDIndex)
       |        val WIDBox = INPUTS(1)
       |        sigmaProp(
       |          allOf(
       |            Coll(
       |              permitCreation,
       |              WID == WIDBox.tokens(0)._1,
       |              repoOut.R4[Coll[Coll[Byte]]].get == repo.R4[Coll[Coll[Byte]]].get,
       |              repoOut.R5[Coll[Long]].get(WIDIndex) == currentRWT + RWTOut,
       |              repoOut.R5[Coll[Long]].get.slice(0, WIDIndex) == repo.R5[Coll[Long]].get.slice(0, WIDIndex),
       |              repoOut.R5[Coll[Long]].get.slice(WIDIndex + 1, widOutListSize) == repo.R5[Coll[Long]].get.slice(WIDIndex + 1, widOutListSize),
       |              permit.R4[Coll[Coll[Byte]]].get == Coll(WID),
       |              outWIDBox.tokens(0)._1 == WID,
       |            )
       |          )
       |        )
       |      }
       |    }else{
       |      // Returning Watcher Permit
       |      val permit = INPUTS(1)
       |      val RWTIn = repoOut.tokens(1)._2 - repo.tokens(1)._2
       |      val WIDIndex = repoOut.R7[Int].get
       |      val watcherCount = repo.R5[Coll[Long]].get.size
       |      val WIDCheckInRepo = if(repo.R5[Coll[Long]].get(WIDIndex) > RWTIn) {
       |        // Returning some RWTs
       |        // [repo, Permit, WIDToken] => [repo, Permit(Optional), WIDToken(+userChange)]
       |        // [repo, Fraud, Cleanup] => [repo, Cleanup]
       |        allOf(
       |          Coll(
       |            repo.R5[Coll[Long]].get(WIDIndex) == repoOut.R5[Coll[Long]].get(WIDIndex) + RWTIn,
       |            repo.R4[Coll[Coll[Byte]]].get == repoOut.R4[Coll[Coll[Byte]]].get,
       |            repo.R5[Coll[Long]].get.slice(0, WIDIndex) == repoOut.R5[Coll[Long]].get.slice(0, WIDIndex),
       |            repo.R5[Coll[Long]].get.slice(WIDIndex + 1, watcherCount) == repoOut.R5[Coll[Long]].get.slice(WIDIndex + 1, watcherCount)
       |          )
       |        )
       |      }else{
       |        // Returning the permit
       |        // [repo, Permit, WIDToken, watcherCollateral] => [repo, WIDToken(+userChange)]
       |        val watcherCollateral = INPUTS(3);
       |        allOf(
       |          Coll(
       |            repo.R5[Coll[Long]].get(WIDIndex) == RWTIn,
       |            repo.R4[Coll[Coll[Byte]]].get.slice(0, WIDIndex) == repoOut.R4[Coll[Coll[Byte]]].get.slice(0, WIDIndex),
       |            repo.R4[Coll[Coll[Byte]]].get.slice(WIDIndex + 1, watcherCount) == repoOut.R4[Coll[Coll[Byte]]].get.slice(WIDIndex, watcherCount - 1),
       |            repo.R5[Coll[Long]].get.slice(0, WIDIndex) == repoOut.R5[Coll[Long]].get.slice(0, WIDIndex),
       |            repo.R5[Coll[Long]].get.slice(WIDIndex + 1, watcherCount) == repoOut.R5[Coll[Long]].get.slice(WIDIndex, watcherCount - 1),
       |            blake2b256(watcherCollateral.propositionBytes) == watcherCollateralScriptHash,
       |          )
       |        )
       |      }
       |      val WID = repo.R4[Coll[Coll[Byte]]].get(WIDIndex)
       |      sigmaProp(
       |        allOf(
       |          Coll(
       |            repoReplication,
       |            Coll(WID) == permit.R4[Coll[Coll[Byte]]].get,
       |            RWTIn == repo.tokens(2)._2 - repoOut.tokens(2)._2,
       |            WIDCheckInRepo
       |          )
       |        )
       |      )
       |    }
       |  }
       |}""".stripMargin

  lazy val WatcherPermitScript: String =
    s"""{
       |  // ----------------- REGISTERS
       |  // R4: Coll[Coll[Byte]] = [WID]
       |  // ----------------- TOKENS
       |  // 0: X-RWT
       |
       |  val repoNFT = fromBase64("REPO_NFT");
       |  val commitmentScriptHash = fromBase64("COMMITMENT_SCRIPT_HASH");
       |  val WID = SELF.R4[Coll[Coll[Byte]]].get
       |  val outputWithToken = OUTPUTS.slice(2, OUTPUTS.size).filter { (box: Box) => box.tokens.size > 0 }
       |  val outputWithRWT = outputWithToken.exists { (box: Box) => box.tokens.exists { (token: (Coll[Byte], Long)) => token._1 == SELF.tokens(0)._1 } }
       |  val secondBoxHasRWT = OUTPUTS(1).tokens.exists { (token: (Coll[Byte], Long)) => token._1 == SELF.tokens(0)._1 }
       |  if(OUTPUTS(0).tokens(0)._1 == repoNFT){
       |    // Updating Permit (Return or receive more tokens)
       |    // [Repo, Permit(SELF), WID] => [Repo, Permit(optional), WID(+userChange)]
       |    val outputPermitCheck = if(secondBoxHasRWT){
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
       |          outputWithRWT == false,
       |          INPUTS(2).tokens(0)._1 == WID(0),
       |          outputPermitCheck,
       |        )
       |      )
       |    )
       |  }else{
       |    // Event Commitment Creation
       |    // [Permit(s), WID] => [Permit, Commitment, WID]
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          OUTPUTS(1).tokens(0)._1 == SELF.tokens(0)._1,
       |          blake2b256(OUTPUTS(1).propositionBytes) == commitmentScriptHash,
       |          OUTPUTS(1).R5[Coll[Coll[Byte]]].isDefined,
       |          OUTPUTS(1).R6[Coll[Byte]].isDefined,
       |          OUTPUTS(1).R7[Coll[Byte]].get == blake2b256(SELF.propositionBytes),
       |          OUTPUTS(1).R4[Coll[Coll[Byte]]].get == WID,
       |          outputWithRWT == false,
       |          OUTPUTS(0).propositionBytes == SELF.propositionBytes,
       |          OUTPUTS(0).R4[Coll[Coll[Byte]]].get == WID,
       |          OUTPUTS(2).tokens(0)._1 == WID(0),
       |        )
       |      )
       |    )
       |  }
       |}""".stripMargin

  lazy val CommitmentScript: String =
    s"""{
       |  // ----------------- REGISTERS
       |  // R4: Coll[Coll[Byte]] = [WID]
       |  // R5: Coll[Coll[Byte]] = [Request ID (Hash(TxId))]
       |  // R6: Coll[Byte] = Event Data Digest
       |  // R7: Coll[Byte] = Permit Script Digest
       |  // ----------------- TOKENS
       |  // 0: X-RWT
       |
       |  val eventTriggerHash = fromBase64("EVENT_TRIGGER_SCRIPT_HASH");
       |  val repoNFT = fromBase64("REPO_NFT");
       |  val event = if (blake2b256(INPUTS(0).propositionBytes) == eventTriggerHash) INPUTS(0) else OUTPUTS(0)
       |  val myWID = SELF.R4[Coll[Coll[Byte]]].get
       |  val WIDs = event.R4[Coll[Coll[Byte]]].get
       |  val paddedData = event.R5[Coll[Coll[Byte]]].get.fold(Coll(0.toByte), { (a: Coll[Byte], b: Coll[Byte]) => a ++ b } )
       |  val eventData = paddedData.slice(1, paddedData.size)
       |  if(blake2b256(INPUTS(0).propositionBytes) == eventTriggerHash){
       |    // Reward Distribution (for missed commitments)
       |    // [EventTrigger, Commitments[], BridgeWallet] => [WatcherPermits[], BridgeWallet]
       |    val permitBox = OUTPUTS.filter {(box:Box) =>
       |      box.R4[Coll[Coll[Byte]]].isDefined &&
       |      box.R4[Coll[Coll[Byte]]].get == myWID
       |    }(0)
       |    val WIDExists =  WIDs.exists {(WID: Coll[Byte]) => myWID == Coll(WID)}
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          blake2b256(permitBox.propositionBytes) == SELF.R7[Coll[Byte]].get,
       |          permitBox.tokens(0)._1 == SELF.tokens(0)._1,
       |          permitBox.tokens(0)._2 == SELF.tokens(0)._2,
       |          // check for duplicates
       |          WIDExists == false,
       |          // validate commitment
       |          blake2b256(eventData ++ myWID(0)) == SELF.R6[Coll[Byte]].get
       |        )
       |      )
       |    )
       |
       |  } else if (blake2b256(OUTPUTS(0).propositionBytes) == eventTriggerHash){
       |    // Event Trigger Creation
       |    // [Commitments[]] + [Repo(DataInput)] => [EventTrigger]
       |    val commitmentBoxes = INPUTS.filter { (box: Box) => SELF.propositionBytes == box.propositionBytes }
       |    val myWIDCommitments = commitmentBoxes.filter{ (box: Box) => box.R4[Coll[Coll[Byte]]].get == myWID }
       |    val EventBoxErgs = commitmentBoxes.map { (box: Box) => box.value }.fold(0L, { (a: Long, b: Long) => a + b })
       |    val myWIDExists = WIDs.exists{ (WID: Coll[Byte]) => Coll(WID) == myWID }
       |    val repo = CONTEXT.dataInputs(0)
       |    val requestId = blake2b256(event.R5[Coll[Coll[Byte]]].get(0))
       |    val repoR6 = repo.R6[Coll[Long]].get
       |    val maxCommitment = repoR6(3)
       |    val requiredCommitmentFromFormula: Long = repoR6(2) + repoR6(1) * (repo.R4[Coll[Coll[Byte]]].get.size - 1L) / 100L
       |    val requiredCommitment = if(maxCommitment < requiredCommitmentFromFormula) {
       |      maxCommitment
       |    } else {
       |      requiredCommitmentFromFormula
       |    }
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          //check repo
       |          repo.tokens(0)._1 == repoNFT,
       |          repo.tokens(1)._1 == SELF.tokens(0)._1,
       |
       |          OUTPUTS(0).value >= EventBoxErgs,
       |          myWIDCommitments.size == 1,
       |          myWIDExists,
       |          event.R6[Coll[Byte]].get == SELF.R7[Coll[Byte]].get,
       |          WIDs.size == commitmentBoxes.size,
       |          // verify commitment to be correct
       |          blake2b256(eventData ++ myWID(0)) == SELF.R6[Coll[Byte]].get,
       |          // check event id
       |          SELF.R5[Coll[Coll[Byte]]].get == Coll(requestId),
       |          // check commitment count
       |          commitmentBoxes.size > requiredCommitment,
       |          // Check required RWT
       |          SELF.tokens(0)._2 == repoR6(0),
       |          event.tokens(0)._2 == repoR6(0) * commitmentBoxes.size,
       |          event.tokens(0)._1 == SELF.tokens(0)._1
       |        )
       |      )
       |    )
       |  } else {
       |    // Commitment Redeem
       |    // [Commitment, WID] => [Permit, WID]
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          SELF.id == INPUTS(0).id,
       |          OUTPUTS(0).tokens(0)._1 == SELF.tokens(0)._1,
       |          OUTPUTS(0).tokens(0)._2 == SELF.tokens(0)._2,
       |          // check WID copied
       |          OUTPUTS(0).R4[Coll[Coll[Byte]]].get == myWID,
       |          // check user WID
       |          INPUTS(1).tokens(0)._1 == myWID(0),
       |          // check permit contract address
       |          blake2b256(OUTPUTS(0).propositionBytes) == SELF.R7[Coll[Byte]].get
       |        )
       |      )
       |    )
       |  }
       |}""".stripMargin

  lazy val EventTriggerScript: String =
    s"""{
       |  // ----------------- REGISTERS
       |  // R4: Coll[Coll[Byte]] [WID[]]
       |  // R5: Coll[Coll[Byte]] Event data
       |  // R6: Coll[Byte] Permit contract script digest
       |  // ----------------- TOKENS
       |  // 0: RWT
       |
       |  // In case of fraud: [TriggerEvent, CleanupToken] => [Fraud1, Fraud2, ...]
       |  // In case of payment: [TriggerEvent, Commitments(if exists), LockBox](dataInput: GuardNFTBox) => [Permit1, ..., changeBox]
       |  val cleanupNFT = fromBase64("CLEANUP_NFT");
       |  val cleanupConfirmation = CLEANUP_CONFIRMATION;
       |  val LockScriptHash = fromBase64("LOCK_SCRIPT_HASH");
       |  val FraudScriptHash = fromBase64("FRAUD_SCRIPT_HASH");
       |  val GuardLockExists = INPUTS.exists { (box: Box) => blake2b256(box.propositionBytes) == LockScriptHash}
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
       |        GuardLockExists,
       |        blake2b256(OUTPUTS(0).propositionBytes) == SELF.R6[Coll[Byte]].get
       |      )
       |    )
       |  }
       |  val WIDs: Coll[Coll[Byte]] = SELF.R4[Coll[Coll[Byte]]].get
       |  val mergeBoxes = OUTPUTS.slice(0, WIDs.size)
       |  val checkAllWIDs = WIDs.zip(mergeBoxes).forall {
       |    (data: (Coll[Byte], Box)) => {
       |      Coll(data._1) == data._2.R4[Coll[Coll[Byte]]].get &&
       |      data._2.propositionBytes == OUTPUTS(0).propositionBytes &&
       |      data._2.tokens(0)._1 == SELF.tokens(0)._1 &&
       |      data._2.tokens(0)._2 == SELF.tokens(0)._2 / WIDs.size
       |    }
       |  }
       |  sigmaProp(
       |    allOf(
       |      Coll(
       |        WIDs.size == mergeBoxes.size,
       |        checkAllWIDs,
       |        fraudScriptCheck,
       |      )
       |    )
       |  )
       |}""".stripMargin

  lazy val FraudScript: String =
    s"""{
       |  // ----------------- REGISTERS
       |  // R4: Coll[Coll[Byte]] = [WID]
       |  // ----------------- TOKENS
       |  // 0: RWT
       |
       |  val repoNFT = fromBase64("REPO_NFT");
       |  val cleanupNFT = fromBase64("CLEANUP_NFT");
       |  val outputWithToken = OUTPUTS.slice(1, OUTPUTS.size).filter { (box: Box) => box.tokens.size > 0 }
       |  val outputWithRWT = outputWithToken.exists { (box: Box) => box.tokens.exists { (token: (Coll[Byte], Long)) => token._1 == SELF.tokens(0)._1 } }
       |  // RSN Slash
       |  // [Repo, Fraud, Cleanup] => [Repo, Cleanup, Slashed]
       |  sigmaProp(
       |    allOf(
       |      Coll(
       |        outputWithRWT == false,
       |        INPUTS(0).tokens(0)._1 == repoNFT,
       |        INPUTS(2).tokens(0)._1 == cleanupNFT,
       |      )
       |    )
       |  )
       |}""".stripMargin

  val watcherCollateral: String =
    s"""{
       |  // ----------------- REGISTERS
       |  // R4: Coll[Byte] = Owner WID
       |  // ----------------- TOKENS
       |  // [repo, Permit, WIDToken, Collateral] => [repo, WIDToken(+userChange)]
       |  val repoNFT = fromBase64("REPO_NFT");
       |  val repo = INPUTS(0);
       |  val repoOut = OUTPUTS(0);
       |  val widBox = INPUTS(2);
       |  val watcherIndex = repoOut.R7[Int].getOrElse(-1);
       |  val watcherWID = SELF.R4[Coll[Byte]].get;
       |  sigmaProp(
       |    allOf(
       |      Coll(
       |        widBox.tokens(0)._1 == watcherWID,
       |        repo.R4[Coll[Coll[Byte]]].get(watcherIndex) == watcherWID,
       |        repoOut.R4[Coll[Coll[Byte]]].get.size == watcherIndex || repoOut.R4[Coll[Coll[Byte]]].get(watcherIndex) != watcherWID,
       |        repo.tokens(0)._1 == repoNFT
       |      )
       |    )
       |  )
       |}""".stripMargin

  val lockScript: String =
    s"""
       |{
       |  val GuardNFT = fromBase64("GUARD_NFT");
       |  val GuardBox = CONTEXT.dataInputs(0);
       |  val paymentSignCount = GuardBox.R5[Coll[Int]].get(0);
       |  val signedColl = GuardBox.R4[Coll[Coll[Byte]]].get.map { (row: Coll[Byte]) => proveDlog(decodePoint(row)) };
       |  val verifyGuard = GuardBox.tokens.exists { (token: (Coll[Byte], Long)) => token._1 == GuardNFT };
       |  sigmaProp(
       |    allOf(
       |      Coll(
       |        verifyGuard,
       |        atLeast(paymentSignCount, signedColl)
       |      )
       |    )
       |  )
       |}
       |""".stripMargin
}
