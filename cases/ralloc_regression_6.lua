Common.ExecuteSequence = function(r0_pr3, r1_pr3, r2_pr3, r3_pr3, ...) -- proto 3
   local r5_pr3 = {}
   for r9_pr3, r10_pr3 in ipairs(r2_pr3) do
      -- backwards jump barrier
      r5_pr3 = {
         r1_pr3(r10_pr3, ...)
      }
      if r3_pr3 ~= r5_pr3[1] then
         break
      end
   end
   return unpack(r5_pr3)
end
