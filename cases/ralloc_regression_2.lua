r0_rt.EnterTriggerCallback = function(r0_pr0, r1_pr0, r2_pr0) -- proto 0
   Classes.HerdableScriptObjectBase.EnterTriggerCallback(r0_pr0, r1_pr0, r2_pr0)
   local r3_pr0 = r0_pr0.containingWorld
   if r3_pr0 == Universe:GetWorld("candy_01") then
      r0_pr0:StopSteering()
      if r0_pr0.behaviorAlarm then
         r0_pr0.behaviorAlarm:Kill()
         r0_pr0.behaviorAlarm = nil
      end
      if r0_pr0.animJob ~= nil then
         r0_pr0.animJob:Signal(BlockingResult.Canceled, 0)
      end
      r0_pr0.animJob = nil
      r0_pr0.animJob = r0_pr0:GetPlayAnimationJob("c-crab-disappear", 1)
      r0_pr0.animJob:RegisterForJobCompletedCallback(r0_pr0, r0_pr0.Disappear)
      r0_pr0.animJob:Execute(r0_pr0)
      if r0_pr0:TestAllHerded() and Task:IsTaskRevealed(r0_pr0.specifiedTaskToComplete) then
         Task:CompleteTask(r0_pr0.specifiedTaskToComplete, Task:GetNPCFromTaskId(r0_pr0.specifiedTaskToComplete))
      end
   end
end
