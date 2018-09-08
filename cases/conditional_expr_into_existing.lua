local m = print
local x = false
local y = 123
x = abc.def or false
local z = 126
if z == y then
    m(y, x)
end
m(z)
