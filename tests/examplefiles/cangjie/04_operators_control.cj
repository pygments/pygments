// Operators and control flow

package example.operators

import std.collection.*

// Arithmetic operators
let add = 1 + 2
let subtract = 10 - 3
let multiply = 4 * 5
let divide = 15 / 3
let modulo = 17 % 5
let exponent = 2 ** 10

// Bitwise operators
let bitwiseAnd = 0xFF & 0x0F
let bitwiseOr = 0xF0 | 0x0F
let bitwiseXor = 0xFF ^ 0x0F
let bitwiseNot = ~0xFF
let leftShift = 1 << 8
let rightShift = 256 >> 4

// Comparison operators
let isEqual = 5 == 5
let isNotEqual = 5 != 3
let isLess = 3 < 5
let isGreater = 5 > 3
let isLessOrEqual = 5 <= 5
let isGreaterOrEqual = 5 >= 3

// Logical operators
let logicalAnd = true && false
let logicalOr = true || false
let logicalNot = !true

// Assignment operators
var assign = 10
assign += 5
assign -= 3
assign *= 2
assign /= 4
assign %= 3
assign **= 2
assign <<= 1
assign >>= 1
assign &= 0xFF
assign |= 0x0F
assign ^= 0x05
assign &&= true
assign ||= false

// Increment/decrement
var counter = 0
counter++
counter--
let preInc = ++counter
let preDec = --counter

// Pipeline operators
let pipelineResult = getData()
    |> processData()
    |> transformOutput()
    ~> saveResult()

// Composition operator
let composed = funcA ~> funcB

// Range operators
let openRange = 1..10
let closedRange = 1..=10
let halfOpenRange = 0..<10

// Null coalescing operators
let nullable: ?Int64 = None
let coalesced = nullable ?? 0
let chainedCoalescing = nullable ?? anotherNullable ?? 0

// Safe navigation operators
let safeAccess = obj?.property
let safeMethod = obj?.method()
let safeIndex = array?[index]
let safeCall = nullable?(42)
let safeBlock = nullable?{ => compute() }

// Elvis operator
let elvis = value ?: defaultValue

// Spread operator
let spread = [...array1, ...array2]

// Type operators
let isInt = value is Int64
let casted = value as Float64
let subtyping = type <: ParentType

// Member access
let member = obj.property
let methodCall = obj.method()
let staticMember = Type.staticProperty
let namespaceAccess = std.collection.ArrayList

// if-else statements
if (condition) {
    doSomething()
}

if (condition) {
    doThis()
} else {
    doThat()
}

if (condition1) {
    branch1()
} else if (condition2) {
    branch2()
} else {
    defaultBranch()
}

// if with let binding
if (let Some(value) = optionalValue) {
    useValue(value)
}

// while loop
var i = 0
while (i < 10) {
    println(i)
    i++
}

// do-while loop
var j = 0
do {
    println(j)
    j++
} while (j < 10)

// for loop with range
for (i in 1..10) {
    println(i)
}

// for loop with closed range
for (i in 1..=10) {
    println(i)
}

// for loop with array
for (item in array) {
    println(item)
}

// for loop with let binding
for (let i in 1..10) {
    println(i * 2)
}

// for loop with where clause
for (i in 1..100 where i % 2 == 0) {
    println(i)
}

// for loop with step
for (i in 1..10 step 2) {
    println(i)
}

// Nested for loops
for (i in 1..5) {
    for (j in 1..5) {
        println("${i}, ${j}")
    }
}

// Break and continue
for (i in 1..100) {
    if (i == 50) {
        break
    }
    if (i % 2 == 0) {
        continue
    }
    println(i)
}

// Labeled loops
outer: for (i in 1..10) {
    for (j in 1..10) {
        if (i + j > 15) {
            break outer
        }
    }
}

// Match expression
let result = match (value) {
    case 0 => "zero"
    case 1 | 2 | 3 => "small"
    case n where n > 10 => "large"
    case _ => "unknown"
}

// Match with patterns
let patternResult = match (data) {
    case Some(x) => x
    case None => 0
}

// Match with struct pattern
let structMatch = match (point) {
    case Point { x: 0.0, y: 0.0 } => "origin"
    case Point { x: _, y: 0.0 } => "on x-axis"
    case Point { x: 0.0, y: _ } => "on y-axis"
    case _ => "somewhere"
}

// Match with enum
let enumMatch = match (direction) {
    case Direction.North => 0
    case Direction.South => 180
    case Direction.East => 90
    case Direction.West => 270
}

// Match with tuple pattern
let tupleMatch = match (coordinates) {
    case (0.0, 0.0) => "origin"
    case (x, 0.0) => "x-axis at ${x}"
    case (0.0, y) => "y-axis at ${y}"
    case (x, y) => "point at (${x}, ${y})"
}

// Match with array pattern
let arrayMatch = match (arr) {
    case [] => "empty"
    case [x] => "single: ${x}"
    case [x, y] => "pair: ${x}, ${y}"
    case [first, ...rest] => "first: ${first}, rest: ${rest}"
}

// Match block (without parenthesized expression)
match {
    case pattern1 => action1()
    case pattern2 => action2()
    case _ => defaultAction()
}

// Where clause in match
let complexMatch = match (value) {
    case x where x > 0 && x < 10 => "small positive"
    case x where x >= 10 => "large positive"
    case x where x < 0 => "negative"
    case _ => "zero"
}

// In operator
let contains = 5 in [1, 2, 3, 4, 5]
let notContains = 6 !in [1, 2, 3]

// With statement
with (resource) {
    useResource()
}

// Spawn (concurrency)
spawn {
    concurrentTask()
}

// Synchronized block
synchronized (lock) {
    criticalSection()
}

// Mut modifier
mut var mutableVar = 10
mut func mutableFunc(self: MutableStruct) {
    self.field = 42
}

// Static modifier
static func staticMethod(): Unit {}
static let staticField: Int64 = 42
static var staticVar: String = "static"