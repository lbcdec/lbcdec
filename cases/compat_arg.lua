local arg = 4
function x(...)
    if false then
        return ...
    end
    return arg
end
print(x())
