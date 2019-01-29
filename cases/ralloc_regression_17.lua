local x = 123
do
    local foo = bar
    if foo then
        print(foo, bar)
    end
end
if x then
    local a, b, c, d = foo.bar:qux()
    local e = {}
    print(a, b, c, d, e)
end
