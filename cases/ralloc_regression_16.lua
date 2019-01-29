r0_rt.RealFakeAutonomy = function(r0_pr26) -- proto 26
   if DebugMenu:GetValue("EnableRealFakeAutonomy") and r0_pr26.autonomyEnabled then
      local r1_pr26 = r0_pr26.containingWorld
      local r2_pr26 = r1_pr26:CreateTable(r0_pr26, DebugMenu:GetValue("RealFakeAutonomyDistance"))
      local r3_pr26 = nil
      local r4_pr26 = 0
      local r5_pr26 = {}
      for r9_pr26 in pairs(r2_pr26) do
         -- backwards jump barrier
         if InteractionUtils:IsObjectInteractable(r9_pr26) then
            for r13_pr26 in pairs(r9_pr26.interactionSet) do
               -- backwards jump barrier
               if InteractionUtils:InteractionTest(r0_pr26, r9_pr26, r13_pr26, true) then
                  r5_pr26[#r5_pr26 + 1] = {
                     object = r9_pr26,
                     key = r13_pr26
                  }
               end
            end
         end
      end
      if 0 < #r5_pr26 then
         local r6_pr26 = r5_pr26[math.random(#r5_pr26)]
         r0_pr26:PushInteraction(r6_pr26.object, r6_pr26.key, nil, nil, nil, Constants.InteractionPriorities.Autonomy)
         return true
      end
   end
   return false
end
