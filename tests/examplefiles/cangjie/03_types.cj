// Type definitions: struct, class, interface, enum, extend, prop

package example.types

import std.collection.*

// Basic struct
struct Point {
    let x: Float64
    let y: Float64
    
    init(x: Float64, y: Float64) {
        this.x = x
        this.y = y
    }
}

// Struct with methods
struct Rectangle {
    let width: Float64
    let height: Float64
    
    init(width: Float64, height: Float64) {
        this.width = width
        this.height = height
    }
    
    func area(): Float64 {
        return this.width * this.height
    }
    
    func perimeter(): Float64 {
        return 2.0 * (this.width + this.height)
    }
}

// Struct with visibility modifiers
struct PrivateFields {
    private let secret: Int64
    protected let family: String
    public let everyone: Bool
    
    init(secret: Int64, family: String, everyone: Bool) {
        this.secret = secret
        this.family = family
        this.everyone = everyone
    }
}

// Basic class
class Person {
    let name: String
    var age: Int64
    
    init(name: String, age: Int64) {
        this.name = name
        this.age = age
    }
    
    func greet(): String {
        return "Hello, I'm ${this.name}"
    }
}

// Abstract class
abstract class Shape {
    func area(): Float64 { return 0.0 }
    func perimeter(): Float64 { return 0.0 }
    
    func describe(): String {
        return "Shape with area ${this.area()}"
    }
}

// Abstract with sealed modifier
sealed abstract class FinalShape {
    func compute(): Int64 { return 0 }
}

// Sealed class
sealed class SealedType {
    func sealedMethod(): Unit {}
}

// Open class (can be extended)
open class OpenBase {
    open func canOverride(): Unit {}
    
    func cannotOverride(): Unit {}
}

// Class inheritance
class Circle <: Shape {
    let radius: Float64
    
    init(radius: Float64) {
        this.radius = radius
    }
    
    override func area(): Float64 {
        return 3.14159 * this.radius * this.radius
    }
    
    override func perimeter(): Float64 {
        return 2.0 * 3.14159 * this.radius
    }
}

// Multiple interface implementation
class Concrete <: InterfaceA & InterfaceB {
    init() {}
    
    func methodA(): Unit {}
    func methodB(): Unit {}
}

// Interface definition
interface Serializable {
    func serialize(): String
    func deserialize<T>(data: String): T
}

// Interface with default implementation
interface Countable {
    func count(): Int64
    
    func isEmpty(): Bool {
        return this.count() == 0
    }
}

// Sealed interface
sealed interface SealedInterface {
    func sealedMethod(): Unit
}

// Open interface
open interface OpenInterface {
    func openMethod(): Unit
}

// Interface with properties
interface HasProperties {
    prop name: String
    prop age: Int64
}

// Enum definition
enum Direction {
    | North
    | South
    | East
    | West
}

// Enum with associated values
enum Result<T, E> {
    | Success(T)
    | Error(E)
}

// Enum with methods
enum Color {
    | Red
    | Green
    | Blue
    
    func toHex(): String {
        match (this) {
            case Red => "#FF0000"
            case Green => "#00FF00"
            case Blue => "#0000FF"
        }
    }
    
    static func fromHex(hex: String): Option<Color> {
        match (hex) {
            case "#FF0000" => Some(Red)
            case "#00FF00" => Some(Green)
            case "#0000FF" => Some(Blue)
            case _ => None
        }
    }
}

// Enum with complex associated data
enum Message {
    | Text(String)
    | Binary(Array<UInt8>)
    | Metadata(String, Int64, HashMap<String, String>)
}

// Extend (extension mechanism)
extend String <: Printable {
    func print(): Unit {
        println(this)
    }
}

extend Int64 <: Comparable<Int64> {
    func compareTo(other: Int64): Int64 {
        return this - other
    }
}

extend<T> Array<T> where T <: Printable {
    func printAll(): Unit {
        for (item in this) {
            item.print()
        }
    }
}

// Type alias
type Coordinate = (Float64, Float64)
type StringMap = HashMap<String, String>
type Processor = (String) -> Int64

// Property (prop)
class PropertiesExample {
    prop readOnlyProp: String {
        get() {
            return this.internalValue
        }
    }
    
    prop writeOnlyProp: Int64 {
        set(value) {
            this.internalInt = value
        }
    }
    
    prop fullProp: Float64 {
        get() {
            return this.internalFloat
        }
        set(newValue) {
            this.internalFloat = newValue
        }
    }
    
    private let internalValue: String = "value"
    private var internalInt: Int64 = 0
    private var internalFloat: Float64 = 0.0
}

// Generic types
struct GenericStruct<T, U> {
    let first: T
    let second: U
    
    init(first: T, second: U) {
        this.first = first
        this.second = second
    }
}

class GenericClass<T> where T <: Equatable<T> {
    var value: T
    
    init(value: T) {
        this.value = value
    }
    
    func equals(other: T): Bool {
        return this.value.equals(other)
    }
}

// VArray (fixed-size array)
struct FixedArray {
    let data: VArray<Int64, $10>
    
    init() {
        this.data = VArray<Int64, $10>()
    }
}

// Modifier keywords for classes and functions
sealed class SealedClass {
    func baseMethod(): Unit {}
}

sealed abstract class SealedAbstractClass {
    func abstractMethod(): Unit
}

open class OpenClass {
    public open func baseMethod(): Unit {}
}

abstract class AbstractClass {
    func abstractMethod(): Unit
}

sealed interface SealedInterface {
    func requiredMethod(): Unit
}

open interface OpenInterface {
    func optionalMethod(): Unit
}

// Override and redef modifiers
class DerivedClass <: OpenClass {
    public override func baseMethod(): Unit {}
    public redef func newMethod(): Unit {}
}

// Byte type test
let byteValue: Byte = 0b10101010

// Regression tests for bugs discovered during HTML review

// Bug #6: String literals after member access (was Error token)
class StringTest {
    let value: String = "test"
    
    func checkValue(): Bool {
        // String literal after member access chain
        if (this.value == "expected") {
            return true
        }
        // Multiple member access with string comparison
        let obj = StringTest()
        if (obj.value == "test") {
            return false
        }
        return this.value.contains("partial")
    }
}

// Bug #7: Comma/paren in member access (was Error token)
class FunctionArgsTest {
    let x: Int64 = 10
    let y: Int64 = 20
    
    func testComma(): Unit {
        // Comma after member access in function call
        let result = this.compute(this.x, this.y)
        let pos = Position(this.x, this.y)
    }
    
    func compute(a: Int64, b: Int64): Int64 {
        return a + b
    }
}

// Bug #8: Visibility keywords (was Name.Variable)
public class PublicClass {
    public func publicMethod(): Unit {}
}

private class PrivateClass {
    private func privateMethod(): Unit {}
}

protected class ProtectedClass {
    protected func protectedMethod(): Unit {}
}

internal class InternalClass {
    internal func internalMethod(): Unit {}
}

// Visibility keywords in expressions (should still be Keyword)
let publicKeywordTest = "public"
let privateKeywordTest = "private"
let protectedKeywordTest = "protected"
let internalKeywordTest = "internal"

// Combined modifiers with visibility
public sealed abstract class PublicSealedAbstractClass {}
private open class PrivateOpenClass {}
protected abstract class ProtectedAbstractClass {}
// Note: sealed class must be abstract and is implicitly public