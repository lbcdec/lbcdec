Common.GetCurveInterpolation = function(r0_pr16, r1_pr16, r2_pr16) -- proto 16
   local r3_pr16 = 0
   for r7_pr16 = 1, #r2_pr16 - 1, 1 do
      -- backwards jump barrier
      if r1_pr16 <= r2_pr16[r7_pr16].coord and r1_pr16 <= r2_pr16[r7_pr16 + 1].coord then
         r3_pr16 = Common:LinearInterp(r2_pr16[r7_pr16].coord, r2_pr16[r7_pr16 + 1].coord, r2_pr16[r7_pr16].value, r2_pr16[r7_pr16 + 1].value, r1_pr16)
         break
      end
   end
   return r3_pr16
end