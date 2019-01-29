local r0_rt = {}
local r1_rt = function(r0_pr0, r1_pr0) -- proto 0
   if r0_rt[r0_pr0] then
      r1_pr0:StopSound(r0_rt[r0_pr0])
      r0_rt[r0_pr0] = nil
      r0_pr0:SetCallbackOnDestruction("fiddlesound", nil)
   end
end
local r2_rt = function(r0_pr1, r1_pr1) -- proto 1
   r0_rt[r0_pr1] = r1_pr1:PlaySound("fiddle_play")
   r0_pr1:SetCallbackOnDestruction("fiddlesound", r1_rt)
end
