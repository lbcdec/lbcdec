local r2 = 1

local function xyz()
    return r2
end

if not _FINAL then
    System:RegisterGeneralPostLoadInit(r2)
end
