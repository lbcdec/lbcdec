r0_rt.Begin = function(r0_pr4) -- proto 4
    local r1_pr4 = Tutorial:GetTutorialController()
    if Task:IsTaskComplete("NPC_Marlin_TaskTwo") then
        r0_pr4:Complete()
        return true
    end
    local r2_pr4 = r1_pr4:Construction_IsBasetypeInHand("block", "go_roxie_teddybear") or r1_pr4:Construction_IsBasetypeInHand("block", "go_poppy_bunny") or r1_pr4:Construction_IsBasetypeInHand("block", "go_roxie_monkey")
    if r2_pr4 then
        UI:Spawn("UITutorialScreen", {
            target = "dpad",
            text = "STRING_TUTORIAL_CONSTRUCTION_ROTATEBLOCK_TOOLTIP",
            timer = 180
        })
        r0_pr4.UI = UI:Spawn("UITutorialScreen", {
           text = "STRING_TUTORIAL_CONSTRUCTION_ROTATEBLOCK_TOOLTIP"
        })
        r0_pr4.currentState = r0_pr4.WaitForTask4Complete
        return true
    end
    return false
end
