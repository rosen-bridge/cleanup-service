package rosen.bridge

object Scripts {

  lazy val RwtRepoScript: String =
    s"""{
       |  // ----------------- REGISTERS
       |  // R4: Coll[Byte] = Chain id
       |  // R5: Long = total watchers
       |  // ----------------- TOKENS
       |  // 0: X-RWT Repo NFT
       |  // 1: X-RWT
       |  // 2: RSN
       |  // 3: X-AWC NFT
       |
       |  val repoConfigNft = fromBase64("REPO_CONFIG_NFT");
       |  val watcherCollateralScriptHash = fromBase64("WATCHER_COLLATERAL_SCRIPT_HASH");
       |  if(OUTPUTS(0).tokens(0)._1 == repoConfigNft){
       |    // RWT Repo Update transaction
       |    sigmaProp(true)
       |  } else {
       |    val permitScriptHash = fromBase64("PERMIT_SCRIPT_HASH");
       |    val repoOut = OUTPUTS(0)
       |    val repo = SELF
       |    val repoReplication = allOf(
       |      Coll(
       |        repoOut.propositionBytes == repo.propositionBytes,
       |        repoOut.value >= repo.value,
       |        repoOut.tokens(0)._1 == repo.tokens(0)._1,
       |        repoOut.tokens(0)._2 == repo.tokens(0)._2,
       |        repoOut.tokens(1)._1 == repo.tokens(1)._1,
       |        repoOut.tokens(2)._1 == repo.tokens(2)._1,
       |        repoOut.tokens(3)._1 == repo.tokens(3)._1,
       |        repoOut.R4[Coll[Byte]].get == repo.R4[Coll[Byte]].get,
       |      )
       |    )
       |    if(repo.tokens(1)._2 > repoOut.tokens(1)._2){
       |      // Getting Watcher Permit
       |      val outCollateral = OUTPUTS(1)
       |      val permit = OUTPUTS(2)
       |      val outWIDBox = OUTPUTS(3)
       |      val RWTOut = repo.tokens(1)._2 - repoOut.tokens(1)._2
       |      val permitCreation = allOf(
       |        Coll(
       |          repoReplication,
       |          RWTOut == repoOut.tokens(2)._2 - repo.tokens(2)._2,
       |          permit.tokens(0)._2 == RWTOut,
       |          permit.tokens(0)._1 == SELF.tokens(1)._1,
       |          blake2b256(permit.propositionBytes) == permitScriptHash,
       |        )
       |      )
       |      if(repoOut.tokens(3)._2 == repo.tokens(3)._2 - 1){
       |        // Getting initial permit
       |        // [Repo, UserInputs] + [(DataInput) RepoConfig] => [Repo, Collateral, watcherPermit, WIDBox]
       |        val repoConfigBox = CONTEXT.dataInputs(0)
       |        val repoConfig = repoConfigBox.R4[Coll[Long]]
       |        sigmaProp(
       |          allOf(
       |            Coll(
       |              repoOut.R5[Long].get == repo.R5[Long].get + 1,
       |              // Permit and WID checks
       |              permitCreation,
       |              permit.R4[Coll[Byte]].get == repo.id,
       |              outWIDBox.tokens(0)._1 == repo.id,
       |              outWIDBox.tokens(0)._2 >= 3,
       |              // Repo config checks
       |              repoConfigBox.tokens(0)._1 == repoConfigNft,
       |              // Collateral checks
       |              blake2b256(outCollateral.propositionBytes) == watcherCollateralScriptHash,
       |              outCollateral.R4[Coll[Byte]].get == repo.id,
       |              outCollateral.value >= repoConfig.get(4),
       |              outCollateral.R5[Long].get == RWTOut,
       |              outCollateral.tokens(0)._1 == repo.tokens(3)._1,
       |              if(repoConfig.get(5) > 0){
       |                allOf(
       |                  Coll(
       |                    outCollateral.tokens(1)._1 == repo.tokens(2)._1,
       |                    outCollateral.tokens(1)._2 >= repoConfig.get(5)
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
       |        // [Repo, Collateral, WIDBox] => [Repo, Collateral, watcherPermit, WIDBox]
       |        val collateral = INPUTS(1)
       |        val WID = outCollateral.R4[Coll[Byte]].get
       |        sigmaProp(
       |          allOf(
       |            Coll(
       |              // Permit check
       |              permitCreation,
       |              permit.R4[Coll[Byte]].get == WID,
       |              // Rwt repo check
       |              repoOut.tokens(3)._2 == repo.tokens(3)._2,
       |              repoOut.R5[Long].get == repo.R5[Long].get,
       |              // Collateral check
       |              outCollateral.tokens(0)._1 == repo.tokens(3)._1,
       |              collateral.R5[Long].get + RWTOut == outCollateral.R5[Long].get
       |            )
       |          )
       |        )
       |      }
       |    }else{
       |      // Returning Watcher Permit
       |      val permit = INPUTS(2)
       |      val RWTIn = repoOut.tokens(1)._2 - repo.tokens(1)._2
       |      val collateral = INPUTS(1)
       |      val validateUpdates = if(collateral.R5[Long].get > RWTIn) {
       |        // two scenarios:
       |        //   - partial return permit
       |        //   [Repo, Collateral, Permit, WIDBox, Permits(Optional)] => [Repo, Collateral, Permit(Optional), WIDBox]
       |        //   - slash
       |        //   [Repo, Collateral, Fraud, Cleanup] => [Repo, Collateral, Cleanup]
       |        val outCollateral = OUTPUTS(1)
       |        allOf(
       |          Coll(
       |            repo.tokens(3)._2 == repoOut.tokens(3)._2,
       |            repoOut.R5[Long].get == repo.R5[Long].get,
       |            collateral.R5[Long].get - RWTIn == outCollateral.R5[Long].get,
       |          )
       |        )
       |      }else{
       |        // Returning total permit
       |        // [Repo, Collateral, Permit, WIDBox] => [Repo, UserChange(+Collateral)]
       |        allOf(
       |          Coll(
       |            repoOut.tokens(3)._2 == repo.tokens(3)._2 + 1,
       |            repoOut.R5[Long].get == repo.R5[Long].get - 1,
       |          )
       |        )
       |      }
       |      val WID = collateral.R4[Coll[Byte]].get
       |      sigmaProp(
       |        allOf(
       |          Coll(
       |            repoReplication,
       |            permit.R4[Coll[Byte]].get == WID,
       |            permit.tokens(0)._1 == repo.tokens(1)._1,
       |            RWTIn == repo.tokens(2)._2 - repoOut.tokens(2)._2,
       |            validateUpdates,
       |            collateral.tokens(0)._1 == repo.tokens(3)._1,
       |          )
       |        )
       |      )
       |    }
       |  }
       |}""".stripMargin

  lazy val WatcherPermitScript: String =
    s"""{
       |  // ----------------- REGISTERS
       |  // R4: Coll[Byte] = WID
       |  // ----------------- TOKENS
       |  // 0: X-RWT
       |
       |  val repoNFT = fromBase64("REPO_NFT");
       |  val commitmentScriptHash = fromBase64("COMMITMENT_SCRIPT_HASH");
       |  val WID = SELF.R4[Coll[Byte]].get
       |  val inputPermitsRwt = INPUTS.filter{
       |    (box:Box) =>
       |      box.tokens.size > 0 &&
       |      box.tokens(0)._1 == SELF.tokens(0)._1 &&
       |      box.propositionBytes == SELF.propositionBytes
       |    }
       |    .map{(box:Box) => box.tokens(0)._2}
       |    .fold(0L, { (a: Long, b: Long) => a + b })
       |  if(OUTPUTS(0).tokens(0)._1 == repoNFT){
       |    // Returning Permit
       |    // [Repo, Collateral, Permit(SELF), WID] => [Repo, Collateral, Permit(optional), WID(+userChange)]
       |    val transferedRwt = OUTPUTS(0).tokens(1)._2 - INPUTS(0).tokens(1)._2
       |    val hasOuputPermit = OUTPUTS(2).tokens.size > 0 && OUTPUTS(2).tokens(0)._1 == SELF.tokens(0)._1
       |    val outputPermitCheck = if(hasOuputPermit){
       |      allOf(
       |        Coll(
       |          inputPermitsRwt - transferedRwt == OUTPUTS(2).tokens(0)._2,
       |          OUTPUTS(2).propositionBytes == SELF.propositionBytes,
       |          SELF.R4[Coll[Byte]].get == OUTPUTS(2).R4[Coll[Byte]].get
       |        )
       |      )
       |    }else{
       |      inputPermitsRwt == transferedRwt
       |    }
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          INPUTS(3).tokens(0)._1 == WID,
       |          INPUTS(3).tokens(0)._2 >= 2,
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
       |          OUTPUTS(0).tokens(0)._1 == SELF.tokens(0)._1,
       |          OUTPUTS(1).tokens(0)._2 == inputPermitsRwt - OUTPUTS(0).tokens(0)._2,
       |          OUTPUTS(1).tokens(0)._1 == SELF.tokens(0)._1,
       |          blake2b256(OUTPUTS(1).propositionBytes) == commitmentScriptHash,
       |          OUTPUTS(1).R5[Coll[Byte]].isDefined,
       |          OUTPUTS(1).R6[Coll[Byte]].isDefined,
       |          OUTPUTS(1).R7[Coll[Byte]].get == blake2b256(SELF.propositionBytes),
       |          OUTPUTS(1).R4[Coll[Byte]].get == WID,
       |          OUTPUTS(0).propositionBytes == SELF.propositionBytes,
       |          OUTPUTS(0).R4[Coll[Byte]].get == WID,
       |          OUTPUTS(2).tokens(0)._1 == WID,
       |        )
       |      )
       |    )
       |  }
       |}
       |""".stripMargin

  lazy val CommitmentScript: String =
    s"""{
       |  // ----------------- REGISTERS
       |  // R4: Coll[Byte] = WID
       |  // R5: Coll[Byte] = Event ID (Hash(TxId))
       |  // R6: Coll[Byte] = Event Data Digest
       |  // R7: Coll[Byte] = Permit Script Digest
       |  // ----------------- TOKENS
       |  // 0: X-RWT
       |
       |  val eventTriggerHash = fromBase64("EVENT_TRIGGER_SCRIPT_HASH");
       |  val repoNFT = fromBase64("REPO_NFT");
       |  val repoConfigNft = fromBase64("REPO_CONFIG_NFT");
       |  val trigger = if (blake2b256(INPUTS(0).propositionBytes) == eventTriggerHash) INPUTS(0) else OUTPUTS(0)
       |  val myWID = SELF.R4[Coll[Byte]].get
       |  val eventData = trigger.R5[Coll[Coll[Byte]]].get.fold(Coll[Byte](), {(a: Coll[Byte], b: Coll[Byte]) => a ++ b })
       |  if(blake2b256(INPUTS(0).propositionBytes) == eventTriggerHash){
       |    // Reward Distribution (for missed commitments)
       |    // [EventTrigger, Commitments[], BridgeWallet] => [WatcherPermits[], BridgeWallet]
       |    val WIDs = OUTPUTS.filter{(box:Box)
       |        => box.tokens.size > 0 && box.tokens(0)._1 == SELF.tokens(0)._1
       |      }
       |      .slice(0, trigger.R7[Int].get)
       |      .map{(box:Box) => box.R4[Coll[Byte]].get}
       |    val commitmentsWithMyWid = INPUTS.filter{(box:Box) =>
       |        box.tokens.size > 0 &&
       |        box.tokens(0)._1 == SELF.tokens(0)._1 &&
       |        box.propositionBytes == SELF.propositionBytes &&
       |        box.R4[Coll[Byte]].get == myWID
       |      }
       |    val permitBox = OUTPUTS.filter {(box:Box) =>
       |      box.R4[Coll[Byte]].isDefined &&
       |      box.R4[Coll[Byte]].get == myWID
       |    }(0)
       |    val WIDExists =  WIDs.exists {(WID: Coll[Byte]) => myWID == WID}
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          blake2b256(permitBox.propositionBytes) == SELF.R7[Coll[Byte]].get,
       |          permitBox.tokens(0)._1 == SELF.tokens(0)._1,
       |          permitBox.tokens(0)._2 == SELF.tokens(0)._2,
       |          // check for duplicates
       |          WIDExists == false,
       |          commitmentsWithMyWid.size == 1,
       |          // validate commitment
       |          blake2b256(eventData ++ myWID) == SELF.R6[Coll[Byte]].get
       |        )
       |      )
       |    )
       |
       |  } else if (blake2b256(OUTPUTS(0).propositionBytes) == eventTriggerHash){
       |    // Event Trigger Creation
       |    // [Commitments[]] + [(DataInput) RepoConfigBox + Repo] => [EventTrigger]
       |    val commitmentBoxes = INPUTS.filter{
       |      (box: Box) =>
       |        SELF.propositionBytes == box.propositionBytes &&
       |        box.tokens.size > 0 &&
       |        box.tokens(0)._1 == SELF.tokens(0)._1
       |      }
       |    val WIDs = commitmentBoxes.map{(box:Box) => box.R4[Coll[Byte]].get}
       |    val widListDigest = blake2b256(WIDs.fold(Coll[Byte](), {(a: Coll[Byte], b: Coll[Byte]) => a++b}))
       |    val myWIDCommitments = commitmentBoxes.filter{ (box: Box) => box.R4[Coll[Byte]].get == myWID }
       |    val EventBoxErgs = commitmentBoxes.map { (box: Box) => box.value }.fold(0L, { (a: Long, b: Long) => a + b })
       |    val repoConfigBox = CONTEXT.dataInputs(0)
       |    val repoConfig = repoConfigBox.R4[Coll[Long]].get
       |    val repo = CONTEXT.dataInputs(1)
       |    val watcherCount = repo.R5[Long].get
       |    val eventId = blake2b256(trigger.R5[Coll[Coll[Byte]]].get(0))
       |    val maxCommitment = repoConfig(3)
       |    val requiredCommitmentFromFormula: Long = repoConfig(2) + repoConfig(1) * watcherCount / 100L
       |    val requiredCommitment = if(maxCommitment < requiredCommitmentFromFormula) {
       |      maxCommitment
       |    } else {
       |      requiredCommitmentFromFormula
       |    }
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          //check repo
       |          repoConfigBox.tokens(0)._1 == repoConfigNft,
       |          repo.tokens(0)._1 == repoNFT,
       |          repo.tokens(1)._1 == SELF.tokens(0)._1,
       |          // prevent duplicate commitments
       |          myWIDCommitments.size == 1,
       |          // verify trigger params
       |          trigger.value >= EventBoxErgs,
       |          trigger.R6[Coll[Byte]].get == SELF.R7[Coll[Byte]].get,
       |          trigger.R7[Int].get == commitmentBoxes.size,
       |          trigger.R4[Coll[Byte]].get == widListDigest,
       |          // verify commitment to be correct
       |          blake2b256(eventData ++ myWID) == SELF.R6[Coll[Byte]].get,
       |          // check event id
       |          SELF.R5[Coll[Byte]].get == eventId,
       |          // check commitment count
       |          commitmentBoxes.size > requiredCommitment,
       |          // Check required RWT
       |          SELF.tokens(0)._2 == repoConfig(0),
       |          trigger.tokens(0)._2 == repoConfig(0) * commitmentBoxes.size,
       |          trigger.tokens(0)._1 == SELF.tokens(0)._1
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
       |          OUTPUTS(0).R4[Coll[Byte]].get == myWID,
       |          // check user WID
       |          OUTPUTS(1).tokens(0)._1 == myWID,
       |          // check permit contract address
       |          blake2b256(OUTPUTS(0).propositionBytes) == SELF.R7[Coll[Byte]].get
       |        )
       |      )
       |    )
       |  }
       |}
       |""".stripMargin

  lazy val EventTriggerScript: String =
    s"""{
       |  // ----------------- REGISTERS
       |  // R4: Coll[Byte] = WID list digest
       |  // R5: Coll[Coll[Byte]] = Event data
       |  // R6: Coll[Byte] = Permit contract script digest
       |  // R7: Int = Commitment Count
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
       |  val commitmentCount = SELF.R7[Int].get
       |  val rewards = OUTPUTS.filter{(box:Box)
       |      => box.tokens.size > 0 && box.tokens(0)._1 == SELF.tokens(0)._1
       |    }
       |    .slice(0, commitmentCount)
       |  val WIDs = rewards.map{(box:Box) => box.R4[Coll[Byte]].get}
       |  val widListDigest = blake2b256(WIDs.fold(Coll[Byte](), {(a: Coll[Byte], b: Coll[Byte]) => a++b}))
       |  val checkAllWIDs = rewards.forall {
       |    (data: Box) => {
       |      data.propositionBytes == OUTPUTS(0).propositionBytes &&
       |      data.tokens(0)._1 == SELF.tokens(0)._1 &&
       |      data.tokens(0)._2 == SELF.tokens(0)._2 / commitmentCount
       |    }
       |  }
       |  sigmaProp(
       |    allOf(
       |      Coll(
       |        rewards.size == commitmentCount,
       |        SELF.R4[Coll[Byte]].get == widListDigest,
       |        checkAllWIDs,
       |        fraudScriptCheck,
       |      )
       |    )
       |  )
       |}
       |""".stripMargin


  lazy val newEventTriggerScript: String =
    s"""{
       |  // ----------------- REGISTERS
       |  // R4: Coll[Byte] = WID list digest
       |  // R5: Coll[Coll[Byte]] = Event data
       |  // R6: Coll[Byte] = Permit contract script digest
       |  // R7: Int = Commitment Count
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
//       |        HEIGHT - cleanupConfirmation >= SELF.creationInfo._1
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
       |  val commitmentCount = SELF.R7[Int].get
       |  val rewards = OUTPUTS.filter{(box:Box)
       |      => box.tokens.size > 0 && box.tokens(0)._1 == SELF.tokens(0)._1
       |    }
       |    .slice(0, commitmentCount)
       |  val WIDs = rewards.map{(box:Box) => box.R4[Coll[Byte]].get}
       |  val widListDigest = blake2b256(WIDs.fold(Coll[Byte](), {(a: Coll[Byte], b: Coll[Byte]) => a++b}))
       |  val checkAllWIDs = rewards.forall {
       |    (data: Box) => {
       |      data.propositionBytes == OUTPUTS(0).propositionBytes &&
       |      data.tokens(0)._1 == SELF.tokens(0)._1 &&
       |      data.tokens(0)._2 == SELF.tokens(0)._2 / commitmentCount
       |    }
       |  }
       |  sigmaProp(
       |    allOf(
       |      Coll(true,
       |        rewards.size == commitmentCount,
       |        SELF.R4[Coll[Byte]].get == widListDigest,
       |        checkAllWIDs,
       |        fraudScriptCheck,
       |      )
       |    )
       |  )
       |}
       |""".stripMargin

  lazy val FraudScript: String =
    s"""{
       |  // ----------------- REGISTERS
       |  // R4: Coll[Byte] = WID
       |  // ----------------- TOKENS
       |  // 0: X-RWT
       |
       |  val repoNFT = fromBase64("REPO_NFT");
       |  val cleanupNFT = fromBase64("CLEANUP_NFT");
       |  // RSN Slash
       |  // [Repo, Collateral, Fraud, Cleanup] => [Repo, Collateral, Cleanup, Slashed]
       |  val transferedRwt = OUTPUTS(0).tokens(1)._2 - INPUTS(0).tokens(1)._2
       |  sigmaProp(
       |    allOf(
       |      Coll(
       |        SELF.tokens(0)._2 == transferedRwt,
       |        SELF.id == INPUTS(2).id,
       |        INPUTS(0).tokens(0)._1 == repoNFT,
       |        INPUTS(3).tokens(0)._1 == cleanupNFT,
       |      )
       |    )
       |  )
       |}
       |""".stripMargin

  val watcherCollateral: String =
    s"""{
       |  // ----------------- REGISTERS
       |  // R4: Coll[Byte] = Owner WID
       |  // R5: Long = locked RSN
       |  // ----------------- TOKENS
       |  // 0: X-AWC NFT
       |  // 1: RSN collateral
       |
       |  val repoNFT = fromBase64("REPO_NFT");
       |  val repo = INPUTS(0);
       |  val repoOut = OUTPUTS(0);
       |  val WID = SELF.R4[Coll[Byte]].get;
       |  val transferedRwt = repoOut.tokens(1)._2 - repo.tokens(1)._2
       |  if(transferedRwt < SELF.R5[Long].get){
       |    // two scenarios:
       |    //   - extend or partial return permit
       |    //   [Repo, Collateral, Permit(Optional), WIDBox, Permits(Optional)] => [Repo, Collateral, Permit(Optional), WIDBox]
       |    //   - slash
       |    //   [Repo, Collateral, Fraud, Cleanup] => [Repo, Collateral, Cleanup]
       |    val outCollateral = OUTPUTS(1)
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          SELF.id == INPUTS(1).id,
       |          repo.tokens(0)._1 == repoNFT,
       |          repo.tokens(3)._1 == SELF.tokens(0)._1,
       |          outCollateral.value == SELF.value,
       |          outCollateral.tokens(0)._1 == SELF.tokens(0)._1,
       |          if (SELF.tokens.size > 1) {
       |            // RSN collateral check
       |            outCollateral.tokens(1)._1 == SELF.tokens(1)._1 &&
       |            outCollateral.tokens(1)._2 == SELF.tokens(1)._2
       |          } else {
       |            true
       |          },
       |          outCollateral.R4[Coll[Byte]].get == WID,
       |          outCollateral.R5[Long].get + transferedRwt == SELF.R5[Long].get,
       |          if(transferedRwt < 0) {
       |              // WID check in extend permit
       |              OUTPUTS(3).tokens(0)._1 == WID &&
       |              OUTPUTS(3).tokens(0)._2 >= 2
       |          } else {
       |            true
       |          }
       |        )
       |      )
       |    )
       |  }
       |  else {
       |    val widBox = INPUTS(3);
       |    // Compelete Return
       |    // [Repo, Collateral, Permit, WIDBox] => [Repo, UserChange(+Collateral)]
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          widBox.tokens(0)._1 == WID,
       |          widBox.tokens(0)._2 >= 2,
       |          repoOut.R5[Long].get + 1 == repo.R5[Long].get,
       |          repo.tokens(0)._1 == repoNFT,
       |          repo.tokens(3)._1 == SELF.tokens(0)._1,
       |        )
       |      )
       |    )
       |  }
       |}
       |""".stripMargin

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
