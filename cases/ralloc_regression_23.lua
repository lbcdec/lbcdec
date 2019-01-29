r0_rt.ChangeSong = function(r0_pr4) -- proto 4
   if r0_pr4.bOn then
      if r0_pr4.hSound then
         r0_pr4:StopSound(r0_pr4.hSound)
         r0_pr4.hSound = nil
      end
      local r1_pr4 = math.random(#r0_rt.SoundLoops)
      r1_pr4 = r1_pr4 ~= r0_pr4.soundIndex or (r1_pr4 < #r0_rt.SoundLoops and r1_pr4 + 1) or 1
      r0_pr4.soundIndex = r1_pr4
      local r2_pr4 = r0_rt.SoundLoops[r0_pr4.soundIndex]
      r0_pr4.hSound = r0_pr4:PlaySound(r2_pr4)
      r0_pr4:ChangeSpeakerSong(r2_pr4)
   end
end
