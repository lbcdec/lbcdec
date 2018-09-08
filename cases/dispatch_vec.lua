function vec()
  local x, y = 0, 0
  return function(m, ...)
    if m == "x" then return x
    elseif m == "y" then return y
    elseif m == "set_x" then x = ...
    elseif m == "set_y" then y = ...
    end
  end
end
