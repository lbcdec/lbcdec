Common.StandardResourceSpawn = function(r0_pr31, r1_pr31, r2_pr31, r3_pr31, r4_pr31, r5_pr31, r6_pr31) -- proto 31
   local r7_pr31 = Universe:GetPlayerGameObject()
   if r7_pr31 == nil or not r7_pr31.isValid then
      return
   end
   if r3_pr31 == nil or not r3_pr31.isValid or r3_pr31.containingWorld ~= r7_pr31.containingWorld then
      return
   end
   if r1_pr31 ~= nil then
      if not r4_pr31 then
         r4_pr31 = 180
      end
      if not r5_pr31 then
         r5_pr31 = {
            x = 0,
            y = 0.699999988079071,
            z = 0,
            rotY = 0
         }
      end
      for r11_pr31, r12_pr31 in pairs(r1_pr31) do
         -- backwards jump barrier
         if Classes.ResourceBase:ResourceIsValidToSpawn(r12_pr31[1][1], r12_pr31[1][2]) then
            r13_pr31 = Luattrib:ReadAttribute(r12_pr31[1][1], r12_pr31[1][2], "ResourceType")
            local r14_pr31 = r12_pr31[2]
            if r13_pr31 == Constants.ResourceTypes.WandPower then
               r14_pr31 = r7_rt(r12_pr31[1][1], r12_pr31[1][2], r14_pr31)
            end
            if r13_pr31 == Constants.ResourceTypes.Unlockable then
            end
            local r15_pr31 = Classes.Job_PropellResource:Spawn(r2_pr31, r3_pr31, r12_pr31[1][1], r12_pr31[1][2], r14_pr31, r4_pr31, r5_pr31, nil, nil, {
               r2_pr31,
               r3_pr31
            })
            r15_pr31:ExecuteAsIs()
         end
      end
   end
end