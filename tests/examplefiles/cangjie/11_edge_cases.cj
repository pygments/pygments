// Edge cases: deeply nested structures, complex interpolation, tricky syntax

package example.edge_cases

import std.collection.*

// Type pattern test classes (must be at top-level)
class Base {}
class Derived <: Base {}

// Enum for pattern matching (must be at top-level)
enum Result<T, E> { | Ok(T) | Err(E) }

// DEEPLY NESTED STRING INTERPOLATION

// Interpolation inside interpolation (3 levels)
main() {
    let value = 42
    let outer: String = "Outer ${"middle ${"inner ${value}"}"}"
    
    // Interpolation with nested method calls and lambdas
    let items = ["a", "b", "c"]
    let complexInterp: String = "Result: ${items.map({ x => x }).join(", ")}"
    
    // Interpolation with array literal
    let arrayInInterp: String = "Array: ${[1, 2, 3].map({ x => x * 2 }).join(", ")}"
    
    // Interpolation with match expression
    let num = 5
    let matchInterp: String = "Type: ${match (num) { case 0 => "zero" case _ => "other" }}"
    
    // Multi-line string with interpolation
    let name = "Alice"
    let age = 30
    let multilineInterp: String = """
Name: ${name}
Age: ${age}
"""
    
    // MULTI-HASH RAW STRING EDGE CASES
    
    // Raw string containing what looks like interpolation (but is literal)
    let fakeInterp: String = #"This is literal: ${notInterpolated}"#
    
    // Raw string with quote and hash combinations
    let trickyRaw1: String = ##"Contains "# inside"##
    let trickyRaw2: String = ###"Has ##" and "# too"###
    
    // Raw string with backslashes (literal, not escapes)
    let rawBackslash: String = #"C:\Users\name\path\to\file"# 
    let rawRegex: String = ##"\d+\.\d+|[a-z]+"##
    
    // Raw single-quote strings
    let rawSingleQuote: String = #'Contains text'# 
    
    // Multi-line raw string
    let multilineRaw: String = ###"
func example() {
    let s = "nested string"
}
"###
    
    // COMPLEX OPERATOR CHAINS
    
    // Chained null-safe operators
    let opt: Option<String> = Some("value")
    let chainedNullSafe: Int64 = opt?.size ?? 0
    
    // Complex operator precedence with parens
    let a = 1
    let b = 2
    let c = 3
    let d = 4
    let precedence = (a + b) * (c - d)
    
    // Operators in interpolation
    let opInterp = "Calc: ${(a + b) * c}"
    
    // Range operators
    let rangeCombo = [1, 2, 3, 4, 5][0..3]
    
    // COMPLEX MATCH PATTERNS (type patterns with :)
    
    let obj: Base = Derived()
    
    // Type pattern with :
    let typeMatch = match (obj) {
        case d: Derived => "derived"
        case b: Base => "base"
    }
    
    // Match with tuple patterns
    let x = 1
    let y = 2
    let z = 3
    let tupleMatch = match ((x, y, z)) {
        case (0, 0, 0) => "all zero"
        case (a, b, c) where a == b && b == c => "all equal"
        case (a, _, _) where a > 0 => "positive first"
        case _ => "mixed"
    }
    
    // Match with enum patterns
    let result: Result<Int64, String> = Ok(42)
    let enumMatch = match (result) {
        case Ok(value) => "success: ${value}"
        case Err(msg) => "error: ${msg}"
    }
    
    // COMPLEX LAMBDA NESTING
    
    // Lambda returning lambda
    let curried = { x: Int64 => { y: Int64 => x + y } }
    let sum = curried(10)(20)
    
    // Lambda with nested match
    let lambdaMatch = { input: Option<Int64> => 
        match (input) {
            case Some(v) => v * 2
            case None => 0
        }
    }
    
    // Nested lambdas in method call
    let transformed = items.map({ x => x.upper() }).filter({ s => s.size > 1 })
    
    // COMPLEX GENERICS
    
    // Nested generic types
    let nestedGeneric: HashMap<String, ArrayList<Int64>> = HashMap<String, ArrayList<Int64>>()
    
    // Generic with function type parameter
    let funcGeneric: ((Int64, String) -> Bool) = { n, s => n > 0 && s.size > 0 }
    
    // Generic with tuple type
    let tupleGeneric: Option<(Int64, String, Bool)> = Some((1, "test", true))
    
    // DEEPLY NESTED BRACKETS
    
    // Nested function calls
    let nestedCall = println(toString(compute(process(validate(input)))))
    
    // Nested array/map literals
    let nestedData = [
        ["a", "b"],
        ["c", "d"],
        ["e", "f"]
    ]
    
    // Nested generics in types
    let complexType: Option<HashMap<String, ArrayList<Option<Int64>>>> = None<HashMap<String, ArrayList<Option<Int64>>>>()
    
    // TRIPLE QUOTE STRINGS
    
    // Empty triple-quoted string
    let emptyTriple = """
"""
    
    // Triple-quoted with only whitespace
    let whitespaceTriple = """
   
"""
    
    // Triple-quoted with special characters
    let specialTriple = """
Special: $ { } \n \t " ' #
"""
    
    // IDENTIFIER EDGE CASES
    
    // Quoted identifiers
    let `class` = "not a keyword"
    let `func` = "also not keyword"
    let `if` = "identifier here"
    
    // Long identifier
    let veryLongIdentifierNameThatTestsTheLexerHandlingOfLongNames: Int64 = 100
    
    // Underscore patterns
    let _ = 42  // wildcard, allowed
    let _unused = 100
    
    // NUMBER EDGE CASES
    
    // Large numbers
    let largeInt: Int64 = 9223372036854775807
    let smallInt: Int64 = -9223372036854775808
    
    // Float edge cases
    let pi: Float64 = 3.14159265358979323846
    let scientific: Float64 = 1.23e10
    let smallFloat: Float64 = 1.23e-10
    let negativeFloat: Float64 = -3.14
    
    // Binary, octal, hex
    let binary: Int64 = 0b101010
    let octal: Int64 = 0o755
    let hex: Int64 = 0xFF
    
    // COMMENT EDGE CASES
    
    // Very long comment
    // This is a very long comment that tests the lexer's handling of long single-line comments that might span most of the line width
    
    /* Multi-line comment
       with multiple lines
       and various content
       including special chars: $ { } \n \t
    */
    
    /* Nested /* multi-line */ comment */
    
    // ANNOTATION EDGE CASES
    
    // Multiple annotations stacked
    @Test
    @Benchmark
    func stackedAnnotations(): Unit {}
    
    // Annotation with complex argument
    @Test[suite: "EdgeCaseTests", timeout: 5000]
    func complexAnnotation(): Unit {}
    
    // Annotation with array argument
    @TestCase["a", "b", "c"]
    func arrayAnnotation(): Unit {}
    
    // ESCAPE SEQUENCE EDGE CASES
    
    // All escape sequences
    let allEscapes: String = "\n\t\r\b\f\v\\\"\$\u{41}\u{4E2D}\0"
    
    // Unicode escape variations
    let unicode1: String = "\u{0}"  // null
    let unicode2: String = "\u{FFFF}"  // max basic
    let unicode3: String = "\u{10FFFF}"  // max unicode
    
    // Rune edge cases
    let rune1: Rune = '\n'
    let rune2: Rune = '\t'
    let rune3: Rune = '\u{4E2D}'
    let rune4: Rune = r'\n'  // raw rune
    
    // EXPRESSION EDGE CASES
    
    // Very nested parentheses
    let deeplyNested = ((((1 + 2) * 3) - 4) / 5)
    
    // Chained method calls
    let chained = "hello".upper().trim().split("").join("-")
    
    // Complex if expression
    let complexIf = if (a > 0) {
        if (b > 0) { "both positive" } else { "a positive, b not" }
    } else {
        if (b > 0) { "a not, b positive" } else { "neither positive" }
    }
    
    // VArray EDGE CASES
    
    // VArray with different sizes
    let arr5: VArray<Int64, $5> = VArray<Int64, $5>(repeat: 0)
    let arr100: VArray<Int64, $100> = VArray<Int64, $100>(repeat: 1)
    
    // VArray nested
    let nestedVArray: VArray<VArray<Int64, $3>, $2> = VArray<VArray<Int64, $3>, $2>(repeat: VArray<Int64, $3>(repeat: 0))
    
    // synchronized block (requires mutex)
    let mutex = Mutex()
    synchronized(mutex) {
        println("synchronized block")
    }
    
    // unsafe blocks
    unsafe {
        let ptr = CPointer<Int64>.alloc(1)
        ptr.free()
    }
    
    // spawn expression
    spawn {
        println("spawned thread")
    }
}