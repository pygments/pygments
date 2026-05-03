// Foreign Function Interface (FFI), unsafe operations, and CPointer

package example.ffi

import std.collection.*
import std.ffi.*

// Foreign function declaration
foreign func externalFunc(a: Int32, b: Int32): Int32

// Foreign function with no return
foreign func voidFunc(): Unit

// Foreign function with pointer return
foreign func allocMemory(size: UInt64): CPointer<UInt8>

// Foreign function with CPointer parameter
foreign func processPointer(ptr: CPointer<Int64>): Unit

// Foreign class declaration
foreign class ForeignClass {
    foreign func method(): Int64
    foreign func methodWithParam(p: Int64): Int64
}

// Foreign struct declaration
foreign struct ForeignStruct {
    let field1: Int32
    let field2: Float64
}

// @FastNative annotation for optimization
@FastNative
foreign func optimizedCall(): Int64

// @Frozen annotation for stable implementation
@Frozen
foreign func stableForeignFunc(): CPointer<Nothing>

// CPointer types
let intPtr: CPointer<Int32> = CPointer<Int32>()
let floatPtr: CPointer<Float64> = CPointer<Float64>()
let voidPtr: CPointer<Nothing> = CPointer<Nothing>()
let bytePtr: CPointer<UInt8> = CPointer<UInt8>()

// CPointer allocation
let allocated: CPointer<Int64> = CPointer<Int64>.alloc(10)

// CPointer dereference
let value: Int64 = ptr.item
ptr.item = 42

// CPointer array access
let arrayValue: Int64 = ptr[0]
ptr[1] = 100

// CPointer arithmetic
let offsetPtr: CPointer<Int64> = ptr + 5
let nextPtr: CPointer<Int64> = ptr.next()

// CPointer size and capacity
let ptrSize: Int64 = ptr.size
let ptrCapacity: Int64 = ptr.capacity

// CPointer conversion
let fromArray: CPointer<Int64> = array.toCPointer()
let toCArray: CArray<Int64, 10> = ptr.toCArray()

// Unsafe block
unsafe {
    let rawValue = ptr.item
    ptr.item = rawValue + 1
}

// Unsafe function
unsafe func unsafeOperation(ptr: CPointer<Int64>): Int64 {
    return ptr.item
}

// Unsafe with multiple operations
unsafe {
    let p1: CPointer<Int32> = allocateInt32(5)
    let p2: CPointer<Int32> = allocateInt32(10)
    let result = p1.item + p2.item
    free(p1)
    free(p2)
}

// Unsafe with error handling
try {
    unsafe {
        dangerousPointerOp()
    }
} catch (e: NullPointerException) {
    handleNullPointer()
}

// CArray (fixed-size array for FFI)
let cArray: CArray<Int64, 10> = CArray<Int64, 10>()
cArray[0] = 1
cArray[1] = 2

// CArray to CPointer conversion
let cPtr: CPointer<Int64> = cArray.toCPointer()

// Native memory management
unsafe func allocateBuffer(size: UInt64): CPointer<UInt8> {
    return CPointer<UInt8>.alloc(size)
}

unsafe func freeBuffer(ptr: CPointer<UInt8>): Unit {
    ptr.free()
}

// String conversion for FFI
let cString: CString = nativeString.toCString()
let fromCString: String = cString.toString()

// Callback for foreign function
foreign func registerCallback(cb: (Int64) -> Unit): Unit

// Register callback
registerCallback({ x => println("Callback received: ${x}") })

// Function pointer
let funcPtr: CPointer<Nothing> = getFunctionPointer()

// CPointer null check
if (ptr.isNull()) {
    handleNullPointer()
}

// CPointer comparison
let areSame: Bool = ptr1 == ptr2
let isAfter: Bool = ptr1 < ptr2

// Foreign function with varargs (pseudo)
foreign func printf(format: CString, ...): Int32

// Unsafe memory copy
unsafe func copyMemory(dest: CPointer<UInt8>, src: CPointer<UInt8>, size: UInt64): Unit {
    for (i in 0..size) {
        dest[i] = src[i]
    }
}

// Foreign function with complex types
foreign func processStruct(s: ForeignStruct): ForeignStruct

// Calling foreign function
let result: Int32 = externalFunc(10, 20)
let structResult: ForeignStruct = processStruct(ForeignStruct { field1: 1, field2: 2.0 })

// Nested unsafe blocks
unsafe {
    let innerPtr = getPointer()
    unsafe {
        innerPtr.item = processValue(innerPtr.item)
    }
}

// Unsafe with resource management
unsafe {
    let resource: CPointer<Resource> = acquire()
    try {
        useResource(resource)
    } finally {
        release(resource)
    }
}

// CPointer type conversion
unsafe func reinterpretCast<T, U>(ptr: CPointer<T>): CPointer<U> {
    return ptr.reinterpret<U>()
}

// FFI with error propagation
foreign func mayFail(): Int32 throwing

try {
    let result = mayFail()
} catch (e: ForeignException) {
    println("Foreign call failed: ${e}")
}

// Foreign function with string parameters
foreign func takesString(s: CString): CString
let strResult: CString = takesString("hello".toCString())

// Platform-specific FFI
@When[os == "linux"]
foreign func linuxSpecific(): Int64

@When[os == "windows"]
foreign func windowsSpecific(): Int64

// Foreign interface
foreign interface ForeignInterface {
    foreign func interfaceMethod(): Int64
}

// CPointer as generic type argument
let genericPtr: GenericContainer<CPointer<Int64>> = GenericContainer<CPointer<Int64>>(ptr)

// Memory layout annotation
@Aligned[16]
foreign func alignedOperation(ptr: CPointer<Float64>): Unit

// Unsafe pointer arithmetic
unsafe {
    let base: CPointer<Int8> = buffer
    let offset: CPointer<Int8> = base + 100
    let item: Int8 = offset.item
}

// CPointer size calculation
unsafe func calculateSize<T>(ptr: CPointer<T>): Int64 {
    return sizeof<T>() * ptr.size
}

// Foreign struct initialization
foreign struct Point3D {
    let x: Float64
    let y: Float64  
    let z: Float64
}

let point3d: Point3D = Point3D { x: 1.0, y: 2.0, z: 3.0 }

// Foreign enum
foreign enum ForeignEnum {
    | ValueA
    | ValueB
    | ValueC
}

let foreignEnumVal: ForeignEnum = ForeignEnum.ValueA