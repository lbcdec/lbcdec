r0_rt.ChangedCallback = function(r0_pr3) -- proto 3
   local r1_pr3 = r0_pr3:GetSearchCombos()
   local r2_pr3 = r0_pr3:AreListsSimilar(r0_pr3.currentSearchCombos, r1_pr3)
   if not r2_pr3 then
      local r3_pr3, r4_pr3 = r0_pr3:BuildComboDifferences(r1_pr3, r0_pr3.currentSearchCombos)
      r0_pr3.currentSearchCombos = r1_pr3
   end
   if r0_pr3.changeListeners ~= nil then
      for r6_pr3, r7_pr3 in pairs(r0_pr3.changeListeners) do
         -- backwards jump barrier
         r7_pr3.func(r7_pr3.context, r6_pr3, r0_pr3, r1_pr3, not r2_pr3)
      end
   else
      Interest_CommonCode:ClearBuildableRegion()
   end
end