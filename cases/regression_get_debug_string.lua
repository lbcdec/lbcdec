function BuildableRegion:GetDebugString(unknown)
    local out = {}
    local interestMap = {}
    local brInterests = self:GetBRInterests()
    for i, v in ipairs(brInterests) do
        interestMap[v[1]] = (interestMap[v[1]] or 0) + v[2]
    end
    for interest, count in pairs(interestMap) do
        -- Skips Chair interest (interest 6) :shrug:
        if interest == 0 then
            out[#out + 1] = "[Cute".." = "..count.."]"
        elseif interest == 1 then
            out[#out + 1] = "[Fun".." = "..count.."]"
        elseif interest == 2 then
            out[#out + 1] = "[Nature".." = "..count.."]"
        elseif interest == 3 then
            out[#out + 1] = "[Spooky".." = "..count.."]"
        elseif interest == 4 then
            out[#out + 1] = "[Tech".." = "..count.."]"
        elseif interest == 5 then
            out[#out + 1] = "[Elegant".." = "..count.."]"
        elseif interest == 7 then
            out[#out + 1] = "[Food".." = "..count.."]"
        elseif interest == 8 then
            out[#out + 1] = "[Domestic".." = "..count.."]"
        elseif interest == 9 then
            out[#out + 1] = "[Sculpture".." = "..count.."]"
        else
            out[#out + 1] = "[Paint".." = "..count.."]"
        end
    end
    return out
end
