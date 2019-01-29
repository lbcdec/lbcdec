local r0_rt = Classes.AmbientCritter:Inherit("AmbientBird")
r0_rt.airAnims = {
   {
      anim = "c-bird-idle-hover",
      weight = 50,
      speed = 0
   },
   {
      anim = "c-bird-walk",
      weight = 50,
      speed = 1
   },
   {
      anim = "c-bird-idle-land-start",
      weight = 50,
      speed = 0,
      nextState = Classes.AmbientCritter.states.idle
   }
}
r0_rt.groundAnims = {
   {
      anim = "c-bird-idle-land-breathe",
      weight = 50
   },
   {
      anim = "c-bird-idle-land-loop01",
      weight = 10
   },
   {
      anim = "c-bird-idle-land-loop02",
      weight = 10
   },
   {
      anim = "c-bird-idle-land-loop03",
      weight = 10
   },
   {
      anim = "c-bird-idle-land-stop",
      weight = 20,
      speed = 0,
      nextState = Classes.AmbientCritter.states.moving
   }
}
r0_rt.stateAnims = {
   [Classes.AmbientCritter.states.idle] = r0_rt.groundAnims,
   [Classes.AmbientCritter.states.moving] = r0_rt.airAnims,
   [Classes.AmbientCritter.states.disappear] = "c-bird-disappear",
   [Classes.AmbientCritter.states.appear] = "c-bird-appear"
}
r0_rt.Disappear = nil
-- function(r0_pr0) -- proto 0
--    if r0_pr0:GetAttribute("NoDisappear") ~= true then
--       r0_pr0:SetMotionPathSpeed(0)
--       r0_pr0.state = r0_pr0.states.moving
--       r0_pr0.hideTimer = r0_pr0:CreateTimer(Clock.Game, 0, 0, 0, 5)
--       r0_pr0:PlayAnimation(r0_pr0.stateAnims[r0_pr0.states.disappear], 1, 0, nil)
--    end
-- end
