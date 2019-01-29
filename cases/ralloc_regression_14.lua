r0_rt.ANIMATE_ENTRY = function(r0_pr8, r1_pr8, r2_pr8, r3_pr8) -- proto 8
   local r4_pr8 = BlockingResult.Succeeded
   local r5_pr8 = 0
   local r6_pr8 = false
   local r7_pr8 = BlockingResult.Succeeded
   if r3_pr8.PreAnimCallback then
      local r8_pr8 = r3_pr8.PreAnimCallback(r0_pr8, r1_pr8, r2_pr8)
      if r8_pr8 == BlockingResult.Failed then
         r6_pr8 = true
      else
         if r8_pr8 == nil then
            r8_pr8 = BlockingResult.Succeeded
         end
      end
   end
   if r7_pr8 == BlockingResult.Succeeded then
      if r3_pr8.sequence ~= nil then
         r4_pr8, r5_pr8 = r0_pr8:ANIMATE_SEQUENCE(r1_pr8, r2_pr8, r3_pr8)
      else
         if r3_pr8.anims ~= nil or 0 < #r3_pr8 then
            local r8_pr8 = r3_pr8.anims or r3_pr8
            local r9_pr8 = true
            local r10_pr8 = Common:SelectRandomWeightedWithTest(r8_pr8, r0_pr8, r1_pr8, r2_pr8)
            r10_pr8, r9_pr8 = r0_pr8:ProcessStackAnim(r10_pr8)
            r6_pr8 = not r9_pr8
            r4_pr8, r5_pr8 = r0_pr8:ANIMATE_ENTRY(r1_pr8, r2_pr8, r10_pr8)
         else
            r4_pr8, r5_pr8 = r0_pr8:ANIMATE_ONE(r1_pr8, r2_pr8, r3_pr8)
         end
      end
      if r3_pr8.spawnTuning ~= nil or r3_pr8.spawnEligibility ~= nil then
         local r8_pr8 = r1_pr8 ~= Universe:GetPlayerGameObject() and r3_pr8.spawnProbabilityNPC or r3_pr8.spawnProbability
         r0_pr8:SpawnResources(r1_pr8, r2_pr8, r8_pr8, r3_pr8.spawnEligibility, r3_pr8.spawnTuning)
      end
      if r3_pr8.PostAnimCallback then
         r3_pr8.PostAnimCallback(r0_pr8, r1_pr8, r2_pr8, r4_pr8, r5_pr8)
      end
   end
   if r4_pr8 == BlockingResult.Canceled and r3_pr8.AnimCanceledCallback then
      r3_pr8.AnimCanceledCallback(r0_pr8, r1_pr8, r2_pr8)
   end
   return r4_pr8, r5_pr8, r6_pr8
end
