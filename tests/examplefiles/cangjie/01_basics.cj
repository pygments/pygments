// Basic Cangjie syntax examples
// Package and import declarations

package example.basic

import std.collection.*
import std.io.File
import std.math.{sqrt, pow, log}
import std.concurrent as conc
public import std.format

// Variable declarations with type annotations
let immutableValue: Int64 = 100
var mutableValue: Float64 = 3.14159
const MAX_SIZE: Int64 = 1024

// Type inference
let inferredInt = 42
var inferredFloat = 2.5
const inferredString = "hello"

// Numeric literals
let decimalInt: Int64 = 12345
let hexInt: UInt64 = 0xDEADBEEF
let binaryInt: Int8 = 0b10101010
let octalInt: Int64 = 0o755

// Float literals with type suffixes
let float16: Float16 = 1.5f16
let float32: Float32 = 3.14f32
let float64: Float64 = 2.71828
let scientific: Float64 = 1.5e-10
let scientificSuffix: Float32 = 2.5e3f32

// Integer literals with type suffixes
let int8Val: Int8 = 127i8
let int16Val: Int16 = 32000i16
let int32Val: Int32 = 2000000000i32
let int64Val: Int64 = 9000000000000i64
let uint8Val: UInt8 = 255u8
let uint16Val: UInt16 = 65000u16
let uint32Val: UInt32 = 4000000000u32
let uint64Val: UInt64 = 18000000000000u64

// Boolean and unit literals
let isTrue: Bool = true
let isFalse: Bool = false
let unitValue: Unit = ()

// Rune literals
let letterA: Rune = 'A'
let unicodeChar: Rune = '\u{4E2D}'
let escapedRune: Rune = '\n'
let rawByte: Rune = r'\n'

// Comments
// Single line comment

/*
 * Multi-line comment
 * with multiple lines
 */

/* Nested /* comments */ are supported */

// Built-in types
let intNative: IntNative = 42
let uintNative: UIntNative = 100
let nothingType: Nothing
let anyType: Any = 42

// Optional types
let optionalInt: Option<Int64> = Some(42)
let optionalNone: Option<Int64> = None
let nullableString: ?String = "value"
let nullValue: ?String = null

// Array and collection types
let array: Array<Int64> = [1, 2, 3, 4, 5]
let varray: VArray<Int64, 5> = [1, 2, 3, 4, 5]
let arrayList: ArrayList<String> = ArrayList<String>()
let hashMap: HashMap<String, Int64> = HashMap<String, Int64>()
let hashSet: HashSet<Int64> = HashSet<Int64>()

// Range types
let intRange: Range<Int64> = 1..10
let closedRange: Range<Int64> = 1..=10

// Duration type
let duration: Duration = 1000ms