for i, v in pairs(table) do
    local foo = v * v
    print(i, v, foo)
end

for r5_pr10, r6_pr10 in pairs(r0_pr10.scheduleBlockTimers) do
    -- backwards jump barrier
    r6_pr10 = r6_pr10 - 1
    if r6_pr10 <= 0 then
        r6_pr10 = nil
    end
    r0_pr10.scheduleBlockTimers[r5_pr10] = r6_pr10
end
