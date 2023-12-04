--[[
	Additional Luau lexer tests
]]

--[==[
]]
print()
]==]

type foo<A="foo", B=A, C=true> = ((A, B) -> C) -> foo<A, B, C>

local continue, type, export = 1, 2, 3
print(continue, type, export)
for i: number = 1, 10 do
	continue
end

local bar = function(...)
	return ...
end
bar = bar;
("a"):upper()

local baz = {table = {a = {b = {c = {d = {e = 1}}}}}}
print(baz.table.a.b.c.d.e)
print((baz).table.a.b.c.d.e)

table.create(1)

print("\
a\z



--b --[[\
c")

print((nil :: any) :: foo<foo<>, foo<() -> ()>>, true)

print(`{({` { {`baz`} } `})}`, nil)