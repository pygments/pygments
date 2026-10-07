// Foreign Function Interface (FFI), unsafe operations, and CPointer

package example.ffi

import std.collection.*
import std.ffi.*

// Foreign function declarations (top-level, no body)
foreign func externalFunc(a: Int32, b: Int32): Int32

foreign func voidFunc(): Unit

foreign func allocMemory(size: UInt64): CPointer<UInt8>

foreign func processPointer(ptr: CPointer<Int64>): Unit

foreign func registerCallback(cb: CFunc<(Int64) -> Unit>): Unit

foreign func takesString(s: CString): CString

foreign func printf(format: CString, ...): Int32

// @FastNative annotation for optimization
@FastNative
foreign func optimizedCall(): Int64

// @Frozen annotation for stable implementation
@Frozen
foreign func stableForeignFunc(): CPointer<Byte>

// Foreign struct declaration (allowed)
foreign struct ForeignStruct {
    let field1: Int32
    let field2: Float64
}

foreign struct Point3D {
    let x: Float64
    let y: Float64  
    let z: Float64
}

// Foreign enum (allowed)
foreign enum ForeignEnum {
    | ValueA
    | ValueB
    | ValueC
}

// Foreign interface (allowed)
foreign interface ForeignInterface {
    foreign func interfaceMethod(): Int64
}

// Platform-specific FFI
@When[os == "linux"]
foreign func linuxSpecific(): Int64

@When[os == "windows"]
foreign func windowsSpecific(): Int64

// Memory layout annotation
@Aligned[16]
foreign func alignedOperation(ptr: CPointer<Float64>): Unit

// CPointer types (declarations at top level are OK)
let intPtr: CPointer<Int32> = CPointer<Int32>()
let floatPtr: CPointer<Float64> = CPointer<Float64>()
let bytePtr: CPointer<UInt8> = CPointer<UInt8>()
let voidPtr: CPointer<Byte> = CPointer<Byte>()

main() {
    // CPointer allocation
    let allocated: CPointer<Int64> = CPointer<Int64>.alloc(10)
    
    // Unsafe block for pointer operations
    unsafe {
        let rawValue = allocated.item
        allocated.item = rawValue + 1
    }
    
    // Unsafe function call
    let result = unsafeOperation(allocated)
    
    // Unsafe with multiple operations
    unsafe {
        let p1: CPointer<Int32> = CPointer<Int32>.alloc(5)
        let p2: CPointer<Int32> = CPointer<Int32>.alloc(10)
        p1.free()
        p2.free()
    }
    
    // CArray (fixed-size array for FFI)
    let cArray: VArray<Int64, $10> = VArray<Int64, $10>(repeat: 0)
    
    // String conversion for FFI
    let nativeString = "hello"
    let cString: CString = nativeString.toCString()
    let fromCString: String = cString.toString()
    
    // Register callback
    registerCallback(CFunc<(Int64) -> Unit>({ x => println("Callback: ${x}") }))
    
    // CPointer null check
    if (intPtr.isNull()) {
        println("Null pointer")
    }
    
    // CPointer comparison
    let ptr1 = CPointer<Int64>()
    let ptr2 = CPointer<Int64>()
    let areSame: Bool = ptr1 == ptr2
    
    // Calling foreign function
    let funcResult: Int32 = externalFunc(10, 20)
    let strResult: CString = takesString("hello".toCString())
    
    // Foreign struct initialization
    let point3d: Point3D = Point3D(x: 1.0, y: 2.0, z: 3.0)
    
    // Foreign enum usage
    let foreignEnumVal: ForeignEnum = ForeignEnum.ValueA
    
    // Unsafe memory operations
    unsafe {
        let base: CPointer<Int8> = CPointer<Int8>.alloc(100)
        let offset: CPointer<Int8> = base + 10
        base.free()
    }
    
    // Unsafe with error handling
    try {
        unsafe {
            dangerousPointerOp()
        }
    } catch (e: Exception) {
        println("Error: ${e}")
    }
    
    // Unsafe with resource management
    unsafe {
        let resource: CPointer<Byte> = CPointer<Byte>.alloc(1)
        try {
            println("Using resource")
        } finally {
            resource.free()
        }
    }
    
    // Nested unsafe blocks
    unsafe {
        let innerPtr = CPointer<Int64>.alloc(1)
        unsafe {
            innerPtr.item = 42
        }
        innerPtr.free()
    }
}

// Unsafe function definition
unsafe func unsafeOperation(ptr: CPointer<Int64>): Int64 {
    return ptr.item
}

unsafe func allocateBuffer(size: UInt64): CPointer<UInt8> {
    return CPointer<UInt8>.alloc(size)
}

unsafe func freeBuffer(ptr: CPointer<UInt8>): Unit {
    ptr.free()
}

unsafe func copyMemory(dest: CPointer<UInt8>, src: CPointer<UInt8>, size: UInt64): Unit {
    for (i in 0..size) {
        dest[i] = src[i]
    }
}

unsafe func dangerousPointerOp(): Unit {
    let ptr = CPointer<Int64>.alloc(1)
    ptr.free()
}