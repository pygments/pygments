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

print(if math.random() >= 0.5 then "yes" elseif math.random() >= 0.75 then "maybe" else type("a"))

export--[[aaa

]]type  --[[]]
--

foobar = (foo | foo) & foo

print(12345)
print(1.2345)
print(1._2_____345_____)
print(1______23_____._45)
print(.12345)
print(15e10)
print(1__.5__e__1__2___)
print(1.e5)
print(2e-_1__)
print(({} :: any)._12345)

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