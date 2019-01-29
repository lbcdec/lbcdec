local function x(r0_pr0, r1_pr0, r2_pr0) -- proto 0
   if r1_pr0 == r2_pr0 then
      Task:CompleteTask(r0_pr0.AssociatedTaskId, Task:GetNPCFromTaskId(r0_pr0.AssociatedTaskId))
   end
   local r3_pr0 = r0_pr0:GetAttribute("AmbientCrabRefSpec")
   local r4_pr0 = Classes.Job_SpawnObject:Spawn(r3_pr0[1], r3_pr0[2], Universe:GetWorld(), 0, 0, 0, 0)
   r4_pr0:Execute(Universe:GetWorld())
end