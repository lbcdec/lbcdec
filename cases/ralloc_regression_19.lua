Common.RecurseUnlocks = function(r0_pr27, r1_pr27, r2_pr27) -- proto 27
   if r1_pr27[1] == Luattrib:ConvertStringToUserdataKey("unlock") then
      local r3_pr27 = Luattrib:ReadAttribute(r1_pr27[1], r1_pr27[2], "Unlocks")
      local r4_pr27 = Luattrib:ReadAttribute(r1_pr27[1], r1_pr27[2], "RewardDialogMessage")
      local r5_pr27 = Luattrib:ReadAttribute(r1_pr27[1], r1_pr27[2], "DisplayRefSpec")
      if r5_pr27 == nil or r4_pr27 == nil then
         for r9_pr27, r10_pr27 in pairs(r3_pr27) do
            -- backwards jump barrier
            r2_pr27[#r2_pr27 + 1] = r10_pr27
         end
         return true, nil, nil
      else
         return false, r5_pr27, r4_pr27
      end
   else
      return false, nil, nil
   end
end
