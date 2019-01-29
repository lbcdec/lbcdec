Common.DebugEffect = function(r0_pr1, r1_pr1, r2_pr1) -- proto 1
   local r3_pr1 = GetPlayerGameObject()
   local r4_pr1, r5_pr1, r6_pr1, r7_pr1 = r3_pr1:GetPositionRotation()
   Common:SpawnEffect(r3_pr1.containingWorld, nil, r1_pr1, r2_pr1 or 10, {
      x = r4_pr1,
      y = r5_pr1 + 2,
      z = r6_pr1,
      rotY = r7_pr1
   })
end
