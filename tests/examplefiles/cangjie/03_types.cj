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
    abstract func area(): Float64
    abstract func perimeter(): Float64
    
    func describe(): String {
        return "Shape with area ${this.area()}"
    }
}

// Abstract with sealed modifier
sealed abstract class FinalShape {
    abstract func compute(): Int64
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
class Circle extends Shape {
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

// Multiple inheritance simulation via interfaces
class Concrete extends BaseClass implements InterfaceA, InterfaceB {
    init() {}
    
    override func methodA(): Unit {}
    override func methodB(): Unit {}
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
extend String implements Printable {
    func print(): Unit {
        println(this)
    }
}

extend Int64 implements Comparable<Int64> {
    func compareTo(other: Int64): Int64 {
        return this - other
    }
}

extend Array<T> where T <: Printable {
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
        set(value: Int64) {
            this.internalInt = value
        }
    }
    
    prop fullProp: Float64 {
        get() {
            return this.internalFloat
        }
        set(newValue: Float64) {
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
    let data: VArray<Int64, 10>
    
    init() {
        this.data = VArray<Int64, 10>()
    }
}