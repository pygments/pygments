// Odin Programming Language Example
package example

import "core:fmt"
import "core:mem"

// Constants and types
BUFFER_SIZE :: 1024
PI :: 3.14159

Vec3 :: struct {
    x, y, z: f32,
}

Color :: enum u8 {
    RED   = 0,
    GREEN = 1,
    BLUE  = 2,
}

// Union types
Shape :: union {
    Circle:    struct { radius: f32 },
    Rectangle: struct { width, height: f32 },
    Triangle:  struct { base, height: f32 },
}

// Generic struct
Generic_Stack :: struct($T: typeid) {
    items: [dynamic]T,
    count: int,
}

// Procedures (functions)
main :: proc() {
    fmt.println("Hello, Odin!")
    
    // Variable declarations
    number := 42
    floating := 3.14159
    boolean := true
    character := 'A'
    emoji: rune = '🚀'
    
    // Maps
    scores := map[string]int{"Alice" = 95, "Bob" = 87}
    defer delete(scores)
    
    // Bitwise operations
    bit_result := 0b1010 & 0b1100   // AND
    bit_xor := 5 ~ 3                // XOR
    
    // Arrays and slices
    array: [5]int = {1, 2, 3, 4, 5}
    dynamic_array: [dynamic]int
    append(&dynamic_array, 10, 20, 30)
    
    // String literals
    regular_string := "This is a regular string"
    raw_string := `This is a raw string with "quotes"`
    
    // Control flow
    if number > 0 {
        fmt.printf("Number is positive: %d\n", number)
    } else {
        fmt.println("Number is zero or negative")
    }
    
    // Loops
    for i in 0..<5 {
        fmt.printf("Loop iteration: %d\n", i)
    }
    
    // Switch statement
    switch Color.RED {
    case .RED:
        fmt.println("Color is red")
    case .GREEN:
        fmt.println("Color is green")  
    case .BLUE:
        fmt.println("Color is blue")
    }
    
    // Procedure call
    result := add_numbers(10, 20)
    fmt.printf("Result: %d\n", result)
    
    // Struct usage
    vec := Vec3{x = 1.0, y = 2.0, z = 3.0}
    fmt.printf("Vector: (%f, %f, %f)\n", vec.x, vec.y, vec.z)
    
    // Union usage with pattern matching
    shape := Shape(Circle{{radius = 5.0}})
    switch s in shape {
    case Circle:
        fmt.printf("Circle with radius %f\n", s.radius)
    case Rectangle:
        fmt.printf("Rectangle %f x %f\n", s.width, s.height)
    }
    
    // Generic usage
    stack: Generic_Stack(int)
    append(&stack.items, 42)
    
    // Memory allocation
    ptr := new(int)
    ptr^ = 100
    defer free(ptr)
}

add_numbers :: proc(a, b: int) -> int {
    return a + b
}

// Number literals and bit operations
test_numbers :: proc() {
    // Different number bases
    decimal := 255
    hex := 0xFF
    octal := 0o377
    binary := 0b11111111
    dozenal := 0z9B  // Base 12
    
    // Floating point literals
    float_val := 3.14159
    scientific := 1.23e-4
    imaginary := 2.5i
}

// Foreign procedure declaration
foreign import libc "system:c"

@(default_calling_convention="c")
foreign libc {
    printf :: proc(format: cstring, #c_vararg args: ..any) -> i32 ---
}