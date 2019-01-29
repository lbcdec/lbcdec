local function x()
    local x = 1
    repeat
        print(x)
        x = x + 1
    until x > 10 and y
end

local function y()
    repeat print(5) until (x or y and z) or m
end

local function z()
    repeat
        local x = 4
        print(5)
    until x or a()
end

local function w()
    repeat
        local x = 4
        print(5)
    until x or a()
    b()
end

-- Regression: MultiExpression nodes not counted as statements
local function q(r0_pr1) -- proto 1
    local r1_pr1 = Classes.Job_EnterMetaState:Spawn(MetaStates.Mining, r0_pr1)
    r1_pr1:Execute(r0_pr1)
    r1_pr1:BlockOn()
    local r2_pr1 = r1_pr1:BlockOn(Clock.Game, 0, 0, 0, 30)
    if r2_pr1 ~= BlockingResult.Succeeded then
       return
    end
    r0_pr1.bMetaStateEntered = true
    r0_pr1.requestControlJob = r0_pr1.player:RequestControl(r0_pr1)
    local r3_pr1, r4_pr1 = nil, nil
    r3_pr1, r4_pr1, r0_pr1.returnControlJob = r0_pr1.requestControlJob:BlockOn()
    r0_pr1.requestControlJob = nil
    if r3_pr1 ~= BlockingResult.Succeeded then
       return
    end
    r0_pr1.miningState = r0_pr1.MiningStates.Start
    local r5_pr1 = {
       [LocoAnimTypes.kIdle] = "a2o-mining-loop-breathe"
    }
    r0_pr1.player:OverrideLocoAnims(r5_pr1)
    -- backwards jump barrier
    repeat
        r0_pr1:ActorLoop(r0_pr1)
    until r0_pr1.miningState == r0_pr1.MiningStates.Destroy
end

-- Regression: Free mark and future required base not taken into account when creating scope
r0_rt.MainLoop = function(r0_pr17) -- proto 17
    while true do
       -- backwards jump barrier
       r0_pr17:ProcessControlRequest()
       local r1_pr17 = BlockingResult.Failed
       if 0 < r0_pr17:GetQueuedInteractionCount() then
          r1_pr17 = r0_pr17:DoNextInteraction()
       else
          if r0_pr17.schedule ~= nil and DebugMenu:GetValue("EnableScheduleAutonomy") then
             r1_pr17 = r0_pr17.schedule:ProcessSchedule()
          end
       end
       if r1_pr17 ~= BlockingResult.Succeeded then
          Yield()
       end
    end
 end
