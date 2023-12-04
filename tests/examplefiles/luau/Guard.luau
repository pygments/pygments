--[[
	Copyright 2023 The Redblox Authors

	Permission is hereby granted, free of charge, to any person obtaining a
	copy of this software and associated documentation files (the “Software”),
	to deal in the Software without restriction, including without limitation
	the rights to use, copy, modify, merge, publish, distribute, sublicense,
	and/or sell copies of the Software, and to permit persons to whom the Software
	is furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
	INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
	PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
	HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
	OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
	SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
]]

export type Check<T> = (Value: unknown) -> T

-- Luau Primitive Types

local Any: Check<any> = function(Value)
	return Value
end

local Boolean: Check<boolean> = function(Value)
	assert(type(Value) == "boolean")

	return Value
end

local Thread: Check<thread> = function(Value)
	assert(type(Value) == "thread")

	return Value
end

local Nil: Check<nil> = function(Value)
	assert(Value == nil)

	return Value :: any
end

local Number: Check<number> = function(Value)
	assert(type(Value) == "number")
	assert(Value == Value)

	return Value
end

local String: Check<string> = function(Value)
	assert(type(Value) == "string")

	return Value
end

-- Combination Types

local function Or<T, U>(Left: Check<T>, Right: Check<U>): Check<T | U>
	return function(Value)
		local LeftSuccess = pcall(Left, Value)

		if LeftSuccess then
			return Value :: any
		end

		local RightSuccess = pcall(Right, Value)

		if RightSuccess then
			return Value :: any
		end

		error("Union check failed")
	end
end

local function And<T, U>(Left: Check<T>, Right: Check<U>): Check<T & U>
	return function(Value)
		local LeftSuccess = pcall(Left, Value)

		if not LeftSuccess then
			error("Intersection check failed")
		end

		local RightSuccess = pcall(Right, Value)

		if not RightSuccess then
			error("Intersection check failed")
		end

		return Value :: any
	end
end

-- Complex Types

local function Optional<T>(Check: Check<T>): Check<T | nil>
	return function(Value)
		if Value == nil then
			return nil
		else
			return Check(Value)
		end
	end
end

local function Literal<T>(Literal: T): Check<T>
	return function(Value)
		assert(Value == Literal)

		return Value :: any
	end
end

local function Map<K, V>(KeyCheck: Check<K>, ValueCheck: Check<V>): Check<{ [K]: V }>
	return function(Value)
		assert(type(Value) == "table")

		for k, v in Value :: any do
			KeyCheck(k)
			ValueCheck(v)
		end

		return Value :: any
	end
end

local function Set<T>(Check: Check<T>): Check<{ [T]: boolean }>
	return Map(Check, Literal(true))
end

local function List<T>(Check: Check<T>): Check<{ T }>
	return function(Value)
		assert(type(Value) == "table")

		for i = 1, table.maxn(Value :: any) do
			Check((Value :: any)[i])
		end

		return Value :: any
	end
end

-- Number Types

local Integer: Check<number> = function(Value)
	assert(type(Value) == "number")
	assert(Value % 1 == 0)

	return Value
end

local function NumberMin(Min: number): Check<number>
	return function(Value)
		assert(type(Value) == "number")
		assert(Value >= Min)

		return Value
	end
end

local function NumberMax(Max: number): Check<number>
	return function(Value)
		assert(type(Value) == "number")
		assert(Value <= Max)

		return Value
	end
end

local function NumberMinMax(Min: number, Max: number): Check<number>
	return function(Value)
		assert(type(Value) == "number")
		assert(Value > Min)
		assert(Value < Max)

		return Value
	end
end

-- Roblox Types

local CFrame: Check<CFrame> = function(Value)
	assert(typeof(Value) == "CFrame")
	assert(Value == Value)

	return Value
end

local Color3: Check<Color3> = function(Value)
	assert(typeof(Value) == "Color3")
	assert(Value == Value)

	return Value
end

local DateTime: Check<DateTime> = function(Value)
	assert(typeof(Value) == "DateTime")

	return Value
end

local Instance: Check<Instance> = function(Value)
	assert(typeof(Value) == "Instance")

	return Value
end

local Vector2: Check<Vector2> = function(Value)
	assert(typeof(Value) == "Vector2")
	assert(Value == Value)

	return Value
end

local Vector2int16: Check<Vector2int16> = function(Value)
	assert(typeof(Value) == "Vector2int16")

	return Value
end

local Vector3: Check<Vector3> = function(Value)
	assert(typeof(Value) == "Vector3")
	assert(Value == Value)

	return Value
end

local Vector3int16: Check<Vector3int16> = function(Value)
	assert(typeof(Value) == "Vector3int16")

	return Value
end

-- Check

local function Check<T>(Check: Check<T>): (Value: unknown) -> (boolean, T)
	return function(Value)
		return pcall(Check, Value)
	end
end

return {
	Any = Any,

	Boolean = Boolean,
	Thread = Thread,
	Nil = Nil,
	Number = Number,
	String = String,

	Optional = Optional,
	Literal = Literal,

	Or = Or,
	And = And,

	Map = Map,
	Set = Set,
	List = List,

	Integer = Integer,
	NumberMin = NumberMin,
	NumberMax = NumberMax,
	NumberMinMax = NumberMinMax,

	CFrame = CFrame,
	Color3 = Color3,
	DateTime = DateTime,
	Instance = Instance,
	Vector2 = Vector2,
	Vector2int16 = Vector2int16,
	Vector3 = Vector3,
	Vector3int16 = Vector3int16,

	Check = Check,
}