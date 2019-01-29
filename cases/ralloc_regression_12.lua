initFunc = function(r0_pr0, r1_pr0, r2_pr0) -- proto 0
   local r3_pr0 = Classes.Job_PlayAnimation_CutsceneTalk:Spawn(r0_pr0, {
      "c-crab-appear",
      count = 1,
      idles = {
         {
            "c-crab-idle-breathe"
         }
      }
   })
   r3_pr0:Execute(r1_pr0)
   r1_pr0.trackedOps[r2_pr0] = r3_pr0
end
