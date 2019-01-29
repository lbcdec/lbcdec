r0_rt.ROUTE_AWAY = function(r0_pr7, r1_pr7, r2_pr7, r3_pr7) -- proto 7
   if not r3_pr7.allowPlayer and r1_pr7 == Universe:GetPlayerGameObject() then
      return
   end
   if r0_pr7.routeSlotNum == nil then
      return
   end
   local r4_pr7 = BlockingResult.Succeeded
   local r5_pr7 = 0
   local r6_pr7 = r3_pr7.routeSpeed
   local r7_pr7 = r0_pr7.locoOverrides or r0_pr7.stateSpec.locoOverrides or r3_pr7.locoOverrides
   local r8_pr7 = r2_pr7.containingWorld
   if r3_pr7.worldName then
      r8_pr7 = Universe:GetWorld(r3_pr7.worldName)
   end
   if r8_pr7 ~= r1_pr7.containingWorld then
      local r9_pr7 = Classes.Job_RouteToWorld:Spawn(r1_pr7, r8_pr7)
      r9_pr7:SetRouteSpeed(r6_pr7)
      r9_pr7:SetLocoOverrides(r7_pr7)
      r9_pr7:Execute(r0_pr7)
      r4_pr7, r5_pr7 = r9_pr7:BlockOn()
   end
   if r4_pr7 == BlockingResult.Succeeded and not r0_pr7.bSkipRoute then
      local r9_pr7 = r3_pr7.distance
      if type(r9_pr7) == "table" then
         r9_pr7 = r9_pr7.min + math.random() * r9_pr7.max - r9_pr7.min
      end
      local r10_pr7, r11_pr7 = Common:GetRelativePosition(0, r9_pr7, 0, 0, math.random(0, 360))
      local r12_pr7 = Classes.Job_Wander:Spawn(r1_pr7, r9_pr7, r10_pr7, r11_pr7)
      r12_pr7:SetRouteSpeed(r6_pr7)
      r12_pr7:SetLocoOverrides(r7_pr7)
      r4_pr7, r5_pr7 = r0_pr7:BlockingJob(r12_pr7)
   end
   r4_pr7 = r4_pr7 ~= BlockingResult.Succeeded or (r0_pr7:Test(r1_pr7, r2_pr7, nil, r0_pr7.params) and BlockingResult.Succeeded) or BlockingResult.Failed
   return r4_pr7, r5_pr7
end
