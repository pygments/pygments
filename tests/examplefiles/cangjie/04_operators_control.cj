// Operators and control flow

package example.operators

import std.collection.*

// Arithmetic operators
main() {
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
    let bitwiseNot = !0xFF
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

    // Increment/decrement (postfix only)
    var counter = 0
    counter++
    counter--

    // Range operators
    let openRange = 1..10
    let closedRange = 1..=10

    // Null coalescing operators
    let nullable: ?Int64 = None
    let coalesced = nullable ?? 0

    // Safe navigation operators
    let obj: Option<Point> = Some(Point(1.0, 2.0))
    let safeAccess = obj?.x
    let safeMethod = obj?.toString()

    // Type operators
    let value: Any = 42
    let isInt = value is Int64
    let casted = value as Int64

    // Member access
    let point = Point(1.0, 2.0)
    let member = point.x
    let staticMember = Math.PI

    // Control flow
    let condition = true
    
    // if-else statements
    if (condition) {
        println("true")
    }

    if (condition) {
        println("true")
    } else {
        println("false")
    }

    if (condition) {
        println("first")
    } else if (!condition) {
        println("second")
    } else {
        println("other")
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
    let array = [1, 2, 3, 4, 5]
    for (item in array) {
        println(item)
    }

    // for loop with where clause
    for (i in 1..100 where i % 2 == 0) {
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

    // Match expression
    let matchValue: Int64 = 5
    let result = match (matchValue) {
        case 0 => "zero"
        case 1 | 2 | 3 => "small"
        case n where n > 10 => "large"
        case _ => "unknown"
    }

    // Match with Option
    let optValue: Option<Int64> = Some(42)
    let patternResult = match (optValue) {
        case Some(x) => x
        case None => 0
    }

    // Match with struct pattern
    let pointMatch = Point(0.0, 0.0)
    let structMatch = match (pointMatch) {
        case Point(0.0, 0.0) => "origin"
        case Point(x, 0.0) => "on x-axis at ${x}"
        case Point(0.0, y) => "on y-axis at ${y}"
        case _ => "somewhere"
    }

    // Match with tuple pattern
    let coordinates = (0.0, 0.0)
    let tupleMatch = match (coordinates) {
        case (0.0, 0.0) => "origin"
        case (x, 0.0) => "x-axis at ${x}"
        case (0.0, y) => "y-axis at ${y}"
        case (x, y) => "point at (${x}, ${y})"
    }

    // Where clause in match
    let complexMatch = match (matchValue) {
        case x where x > 0 && x < 10 => "small positive"
        case x where x >= 10 => "large positive"
        case x where x < 0 => "negative"
        case _ => "zero"
    }

    // Spawn (concurrency)
    spawn {
        println("concurrent")
    }

    // Unsafe block
    unsafe {
        println("unsafe operation")
    }
}

struct Point {
    let x: Float64
    let y: Float64
}

class Math {
    static let PI: Float64 = 3.14159
}

enum Direction {
    | North
    | South
    | East
    | West
}

// ============================================================================
// REGRESSION TESTS for bugs #10-16 (discovered in stdlib testing)
// ============================================================================

// Bug #10: <= operator splitting in function bodies
// Problem: <= was split into < (Punctuation) + = (Error) in function bodies
// Fix: Removed < pattern from func_params, added explicit generic params pattern
func compareValues(a: Int64, b: Int64): Bool {
    // Test <= operator after member access (this was failing in stdlib)
    return a <= b && b <= 100 && a <= b
}

// Bug #11: << operator splitting in function bodies  
// Problem: << was split into < (Punctuation) + < (Error) 
// Fix: Prevented generic params < pattern from matching <<
func shiftOperations(x: Int64): Int64 {
    return x << 8 | (x << 4) & 0xFF << 2
}

// Bug #12: <- operator splitting in if-let
// Problem: <- was split into < (Punctuation) + - (Error) in nested functions
// Fix: Prevented generic params < pattern from matching <-
func nestedIfLetExample(): Unit {
    func innerFunc(opt: Option<Int64>): Unit {
        // Nested function with <- operator
        if (let Some(v) <- opt) {
            println(v)
        }
    }
    innerFunc(Some(42))
}

// Bug #13: Generic params handling at function declarations
// Problem: Generic functions with <T> had parsing issues
// Fix: Added explicit patterns for func name< vs func name(
func genericFunction<T>(value: T): T {
    return value
}

func genericWithMultiple<T, U>(a: T, b: U): Tuple<T, U> {
    return (a, b)
}

// Bug #14 is tested in 08_macros_basics.cj (requires macro package)

// Bug #15: } in compact function bodies  
// Problem: } was Error in single-line functions
// Fix: Fixed { pattern in func_params to push to block state
func compactBody(x: Int64): Int64 { x * 2 }

func compactBodyWithGeneric<T>(x: T): T { x }

// Bug #16: Number literals with underscores
// Problem: 1_000_000_000.0 was parsed as 1 + _000_000_000 + . + 0
// Fix: Added patterns for \d[_\d]* and \d[_\d]*\.\d[_\d]*  
func numberWithUnderscores(): Float64 {
    let largeInt = 1_000_000_000
    let largeFloat = 1_000_000_000.0
    let scientificFloat = 1_234e+5_000
    let hexWithUnderscores = 0xFFFF_FFFF
    let binaryWithUnderscores = 0b1111_0000_1111_0000
    return largeFloat / 1_000_000.0
}

// Additional regression: operators in complex expressions
func complexOperatorExpr(a: Int64, b: Int64, c: Int64): Bool {
    // Mix of comparison operators after various expressions
    return (a << 2) <= (b << 2) && 
           (a >> 1) >= (b >> 1) && 
           a + b <= c && 
           a * b >= c << 3
}