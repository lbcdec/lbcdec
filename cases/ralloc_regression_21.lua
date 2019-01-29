Common.ParseResourceAndWeightsListForUnlocksAndScrolls = function(r0_pr35, r1_pr35, r2_pr35) -- proto 35
   local r3_pr35 = {}
   local r4_pr35 = {}
   local r5_pr35 = {}
   local r6_pr35 = {}
   for r10_pr35, r11_pr35 in ipairs(r1_pr35) do
      -- backwards jump barrier
      if Classes.ResourceBase:ResourceIsValidToSpawn(r11_pr35[1], r11_pr35[2]) then
         local r12_pr35 = Luattrib:ReadAttribute(r11_pr35[1], r11_pr35[2], "ResourceType")
         if r12_pr35 == Constants.ResourceTypes.Unlockable then
            r4_pr35[#r4_pr35 + 1] = r11_pr35
         else
            if r12_pr35 == Constants.ResourceTypes.QuestItem and Luattrib:ReadAttribute(r11_pr35[1], r11_pr35[2], "TaskToOpen") ~= nil then
               r5_pr35[#r5_pr35 + 1] = r11_pr35
            else
               r3_pr35[#r3_pr35 + 1] = r11_pr35
               if r2_pr35 ~= nil then
                  r6_pr35[#r6_pr35 + 1] = r2_pr35[r10_pr35]
               end
            end
         end
      end
   end
   return r3_pr35, r4_pr35, r5_pr35, r6_pr35
end