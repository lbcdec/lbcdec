function vec(x, y) return function() return x, y end end
function vecadd(a, b) return function() local ax, ay = a() local bx, by = b() return ax + bx, ay + by end end
