// Function definitions and declarations

package example.functions

import std.collection.*

// Basic function definition
func add(a: Int64, b: Int64): Int64 {
    return a + b
}

// Function without return type (Unit)
func printMessage(msg: String): Unit {
    println(msg)
}

// Function with default return (implicit Unit)
func greet(name: String) {
    println("Hello, ${name}!")
}

// Generic function
func identity<T>(value: T): T {
    return value
}

// Generic function with multiple type parameters
func pair<T, U>(first: T, second: U): (T, U) {
    return (first, second)
}

// Generic function with constraints
func compare<T>(a: T, b: T): Int64 where T <: Comparable<T> {
    return a.compareTo(b)
}

// Multiple constraints using where clause
func process<T, U>(value: T): U where T <: Serializable, U <: Equatable<T> {
    return value.deserialize<U>()
}

// Function with optional parameter
func greetWithTitle(name: String, title!: String = "Mr"): String {
    return "${title} ${name}"
}

// Function with var/let parameter modifiers
func modifyArray(var arr: ArrayList<Int64>) {
    arr.add(100)
}

func readOnlyArray(let arr: ArrayList<Int64>): Int64 {
    return arr.size
}

// Operator function
operator func +(a: Point, b: Point): Point {
    return Point(a.x + b.x, a.y + b.y)
}

operator func [](arr: Array<Int64>, index: Int64): Int64 {
    return arr[index]
}

// Main function
main() {
    let result = add(1, 2)
    println(result)
}

// Lambda expressions
let square = { x: Int64 => x * x }
let addLambda = { a: Int64, b: Int64 => a + b }
let noParamLambda = { => 42 }

// Lambda with explicit return type
let typedLambda: (Int64) -> Int64 = { x => x * x }

// Closure capturing variables
var counter = 0
let increment = { => counter++; counter }

// Lambda as function parameter
func applyFunction(value: Int64, f: (Int64) -> Int64): Int64 {
    return f(value)
}

// Nested function
func outer(x: Int64): Int64 {
    func inner(y: Int64): Int64 {
        return y * 2
    }
    return inner(x) + 1
}

// Recursive function
func factorial(n: Int64): Int64 {
    if (n <= 1) {
        return 1
    }
    return n * factorial(n - 1)
}

// Function with throwing annotation
func riskyOperation(data: String): Int64 throwing {
    if (data.isEmpty()) {
        throw IllegalArgumentException("Empty data")
    }
    return data.size
}

// Const function
const func computeConstant(): Int64 {
    return 42 * 42
}

// Static function in struct/class context
struct Calculator {
    static func add(a: Int64, b: Int64): Int64 {
        return a + b
    }
}

// Mut function (modifies receiver)
mut func increment(self: Counter): Unit {
    self.value++
}

// Init constructors
struct Point {
    let x: Float64
    let y: Float64
    
    init(x: Float64, y: Float64) {
        this.x = x
        this.y = y
    }
    
    init(origin: Float64) {
        this.x = origin
        this.y = origin
    }
}

// Destructor (~init)
class Resource {
    let handle: Int64
    
    init(handle: Int64) {
        this.handle = handle
    }
    
    ~init() {
        releaseHandle(this.handle)
    }
}

// Public/protected/private function modifiers
class Visibility {
    public func publicMethod(): Unit {}
    protected func protectedMethod(): Unit {}
    private func privateMethod(): Unit {}
    internal func internalMethod(): Unit {}
}

// Override and redef
class Child extends Parent {
    override func method(): Unit {
        super.method()
    }
    
    redef func reimplemented(): Int64 {
        return 42
    }
}

// Abstract function declaration
abstract class AbstractBase {
    abstract func mustImplement(): Int64
}

// Open class with open function
open class OpenClass {
    open func canOverride(): Unit {}
}

// Sealed class with functions
sealed class SealedClass {
    func sealedMethod(): Unit {}
}