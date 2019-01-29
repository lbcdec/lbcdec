r0_rt.CreateSocialContext = function(r0_pr6, r1_pr6, r2_pr6, r3_pr6) -- proto 6
   return {
      data = r0_pr6:BuildSocialData(r1_pr6),
      initiator = r2_pr6 or r0_pr6.simA,
      animSync = ConditionSyncCreate(2),
      exitSync = ConditionSyncCreate(2),
      results = r3_pr6
   }
end