--[[
	Additional Luau lexer tests
]]

--[==[
]]
print()
]==]

export type foo<A="foo", B=A, C=true> = ((A, B) -> C) -> foo<A, B, C>

local continue, type, export = 1, 2, 3
print(continue, type, export)
for i: number = 1, 10 do
	continue
end
type, export = export, type

local exporttype = 2
exporttype ^= 4
print(exporttype)

local bar = function(...)
	return ...
end
bar = bar;
("a"):upper()

local baz = {table = {a = {b = {c = {d = {e = 1}}}}}}
print(baz.table.a.b.c.d.e)
print((baz).table.a.b.c.d.e)

table.create(1)

export--[[aaa

]]type  --[[]]
--

foobar = (foo | foo) & foo

print"Hello world!"
print{"Hello world 2!"}
print --
	--[=[ ]==] ]=]
	'Hello world 3!'

print("\
a\z



--b --[[\
c")

print((nil :: any) :: foo<foo<>, foo<() -> ()>>, true)

print(`{({` { {`baz`} } `})}`, nil)