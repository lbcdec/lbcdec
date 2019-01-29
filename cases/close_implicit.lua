do
    local abc = 123
    def(abc)
end
abc(123)

do
    local abc = 123
    print(abc)
end
def.abc:qux("foo")
