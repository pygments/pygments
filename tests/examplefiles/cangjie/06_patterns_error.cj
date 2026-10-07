// Pattern matching and error handling

package example.patterns

import std.collection.*

// Enum declarations at top-level
enum Result<T, E> { | Ok(T) | Err(E) }
enum Color { | Red(UInt8) | Green(UInt8) | Blue(UInt8) }
enum Expr { | Num(Int64) | Add(Expr, Expr) }

// Class declarations at top-level
class Base {}
class Derived <: Base {}

// Custom exception
class CustomException <: Exception {
    let code: Int64
    init(code: Int64, message: String) {
        this.code = code
    }
}

// Pattern matching basics
main() {
    let value = 1
    
    // Simple match expression
    let simpleMatch = match (value) {
        case 0 => "zero"
        case 1 => "one"
        case _ => "other"
    }
    
    // Pattern alternatives with |
    let altMatch = match (value) {
        case 1 | 2 | 3 => "small"
        case 4 | 5 | 6 => "medium"
        case _ => "large"
    }
    
    // Wildcard pattern
    let wildcardMatch = match (value) {
        case _ => "matches everything"
    }
    
    // Variable binding pattern
    let bindingMatch = match (value) {
        case x => "value is ${x}"
    }
    
    // Tuple pattern matching
    let pair = (1, 2)
    let tupleMatch = match (pair) {
        case (0, 0) => "origin"
        case (x, 0) => "x-axis"
        case (0, y) => "y-axis"
        case (x, y) => "point (${x}, ${y})"
    }
    
    // Tuple with nested patterns
    let triple = (1, 2, 3)
    let nestedTuple = match (triple) {
        case (0, _, _) => "first is zero"
        case (_, 0, _) => "second is zero"
        case (_, _, 0) => "third is zero"
        case (a, b, c) => "(${a}, ${b}, ${c})"
    }
    
    // Enum pattern matching
    let result: Result<Int64, String> = Ok(42)
    let enumMatch = match (result) {
        case Ok(value) => "success: ${value}"
        case Err(err) => "error: ${err}"
    }
    
    // Option pattern matching
    let optional: Option<Int64> = Some(100)
    let optionMatch = match (optional) {
        case Some(value) => "value: ${value}"
        case None => "no value"
    }
    
    // Nested Option pattern
    let nestedOpt: Option<Option<Int64>> = Some(Some(42))
    let nestedMatch = match (nestedOpt) {
        case Some(Some(x)) => "double nested: ${x}"
        case Some(None) => "inner is None"
        case None => "outer is None"
    }
    
    // Guard patterns with where
    let number = 5
    let guardMatch = match (number) {
        case n where n > 0 => "positive"
        case n where n < 0 => "negative"
        case _ => "zero"
    }
    
    // Multiple guards
    let multiGuard = match (number) {
        case n where n > 0 && n < 10 => "small positive"
        case n where n >= 10 && n < 100 => "medium positive"
        case n where n >= 100 => "large positive"
        case _ => "not positive"
    }
    
    // Pattern with type check (subtyping)
    let obj: Base = Derived()
    let typeMatch = match (obj) {
        case d: Derived => "derived"
        case b: Base => "base"
    }
    
    // Match without value (boolean conditions)
    let x = 10
    match {
        case x > 0 => println("positive")
        case x < 0 => println("negative")
        case _ => println("zero")
    }
    
    // Try-catch-finally
    try {
        println("try block")
    } catch (e: IllegalArgumentException) {
        println("Illegal argument: ${e}")
    } finally {
        println("cleanup")
    }
    
    // Try with multiple catch blocks
    try {
        println("complex operation")
    } catch (e: OverflowException) {
        println("overflow")
    } catch (e: NoneValueException) {
        println("null")
    } catch (e: Exception) {
        println("generic error")
    } finally {
        println("cleanup")
    }
    
    // Catch with pattern alternatives
    try {
        println("operation")
    } catch (e: IllegalArgumentException | OverflowException) {
        println("Specific error: ${e}")
    } catch (_: Exception) {
        println("Generic error")
    }
    
    // Catch with wildcard
    try {
        println("risky")
    } catch (_) {
        println("Something went wrong")
    }
    
    // Try with resource
    try (resource = "resource") {
        println("using ${resource}")
    } catch (e: Exception) {
        println("error: ${e}")
    } finally {
        println("cleanup")
    }
    
    // Throwing exceptions
    if (value < 0) {
        throw IllegalArgumentException("Invalid input")
    }
    
    // Throw custom exception
    try {
        throw CustomException(404, "Not found")
    } catch (e: CustomException) {
        println("Error code: ${e.code}")
    }
    
    // Nested try-catch
    try {
        println("outer")
        try {
            println("inner")
        } catch (e: Exception) {
            println("inner error")
        }
    } catch (e: Exception) {
        println("outer error")
    }
    
    // Try expression (returns value)
    let result2 = try {
        42
    } catch (e: Exception) {
        0
    }
    
    // Safe navigation avoiding exceptions
    let opt: Option<String> = Some("value")
    let safe = opt?.size ?? 0
    
    // if-let pattern
    if (let Some(v) <- optional) {
        println("got value: ${v}")
    }
    
    // for-let pattern (tuple destructuring)
    let map = HashMap<String, Int64>()
    for ((key, value) in map.entrySet()) {
        println("${key}: ${value}")
    }
    
    // Color enum pattern
    let c = Color.Red(255)
    match (c) {
        case Red(r) => println("red: ${r}")
        case Green(g) => println("green: ${g}")
        case Blue(b) => println("blue: ${b}")
    }
    
    // Recursive enum pattern
    let expr = Add(Num(1), Num(2))
    match (expr) {
        case Num(n) => println("num: ${n}")
        case Add(Num(a), Num(b)) => println("add: ${a} + ${b}")
        case Add(left, right) => println("complex add")
    }
}