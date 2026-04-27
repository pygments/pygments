// Pattern matching and error handling

package example.patterns

import std.collection.*

// Pattern matching basics
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
let wildcardMatch = match (anything) {
    case _ => "matches everything"
}

// Variable binding pattern
let bindingMatch = match (value) {
    case x => "value is ${x}"
}

// Nested pattern matching
let nestedMatch = match (option) {
    case Some(Some(x)) => "double nested: ${x}"
    case Some(None) => "inner is None"
    case None => "outer is None"
}

// Struct pattern matching
let structPattern = match (point) {
    case Point { x: 0, y: 0 } => "origin"
    case Point { x, y: 0 } => "on x-axis at ${x}"
    case Point { x: 0, y } => "on y-axis at ${y}"
    case Point { x, y } => "at (${x}, ${y})"
}

// Partial struct pattern
let partialMatch = match (data) {
    case Person { name } => "named ${name}"
    case _ => "unknown"
}

// Tuple pattern matching
let tupleMatch = match (pair) {
    case (0, 0) => "origin"
    case (x, 0) => "x-axis"
    case (0, y) => "y-axis"
    case (x, y) => "point (${x}, ${y})"
}

// Tuple with nested patterns
let nestedTuple = match (triple) {
    case (0, _, _) => "first is zero"
    case (_, 0, _) => "second is zero"
    case (_, _, 0) => "third is zero"
    case (a, b, c) => "(${a}, ${b}, ${c})"
}

// Array pattern matching
let arrayPattern = match (arr) {
    case [] => "empty"
    case [x] => "single element: ${x}"
    case [x, y] => "two elements: ${x}, ${y}"
    case [x, y, z] => "three elements: ${x}, ${y}, ${z}"
    case [first, ...rest] => "first: ${first}, rest size: ${rest.size}"
}

// Array with spread pattern
let spreadPattern = match (arr) {
    case [..., last] => "last: ${last}"
    case [first, ..., last] => "first: ${first}, last: ${last}"
    case [first, middle, ..., last] => "with middle"
}

// Enum pattern matching
let enumMatch = match (result) {
    case Result.Success(value) => "success: ${value}"
    case Result.Error(err) => "error: ${err}"
}

// Enum with complex associated values
let complexEnumMatch = match (message) {
    case Message.Text(text) => "text: ${text}"
    case Message.Binary(data) => "binary size: ${data.size}"
    case Message.Metadata(name, version, attrs) => "metadata: ${name} v${version}"
}

// Guard patterns with where
let guardMatch = match (number) {
    case n where n > 0 => "positive"
    case n where n < 0 => "negative"
    case _ => "zero"
}

// Multiple guards
let multiGuard = match (value) {
    case n where n > 0 && n < 10 => "small positive"
    case n where n >= 10 && n < 100 => "medium positive"
    case n where n >= 100 => "large positive"
    case _ => "not positive"
}

// Pattern with type check
let typeMatch = match (obj) {
    case s is String => "string: ${s}"
    case n is Int64 => "int: ${n}"
    case _ => "other type"
}

// Option pattern matching
let optionMatch = match (optional) {
    case Some(value) => useValue(value)
    case None => handleNone()
}

// Option in conditions
if (let Some(x) = optional) {
    println(x)
}

// Pattern matching in for loop
for ((key, value) in hashMap.entrySet()) {
    println("${key}: ${value}")
}

// Try-catch-finally
try {
    riskyOperation()
} catch (e: IllegalArgumentException) {
    println("Illegal argument: ${e}")
} finally {
    cleanup()
}

// Try with multiple catch blocks
try {
    complexOperation()
} catch (e: OverflowException) {
    handleOverflow()
} catch (e: NoneValueException) {
    handleNull()
} catch (e: Exception) {
    handleGeneric()
} finally {
    cleanup()
}

// Catch with pattern
try {
    operation()
} catch (e: IllegalArgumentException | OverflowException) {
    println("Specific error: ${e}")
} catch (_: Exception) {
    println("Generic error")
}

// Catch with wildcard
try {
    risky()
} catch (_) {
    println("Something went wrong")
}

// Try with resource
try (let resource = acquireResource()) {
    useResource(resource)
} catch (e: Exception) {
    handleError(e)
} finally {
    cleanup()
}

// Throwing exceptions
func throwingFunction(): Int64 throwing {
    if (invalidCondition) {
        throw IllegalArgumentException("Invalid input")
    }
    return computeResult()
}

// Throw statement
func validation(input: String): Unit {
    if (input.isEmpty()) {
        throw IllegalArgumentException("Input cannot be empty")
    }
}

// Throwing function annotation
func riskyFunc(): Unit throwing {
    mightThrow()
}

// Nested try-catch
try {
    outerOperation()
    try {
        innerOperation()
    } catch (e: InnerException) {
        handleInner(e)
    }
} catch (e: OuterException) {
    handleOuter(e)
}

// Exception propagation
func propagates(): Unit throwing {
    try {
        mightThrow()
    } catch (e: RecoverableException) {
        recover()
    }
    throw UnrecoverableException("Failed")
}

// Custom exception
class CustomException <: Exception {
    let code: Int64
    let message: String
    
    init(code: Int64, message: String) {
        this.code = code
        this.message = message
    }
}

// Throw custom exception
throw CustomException(404, "Not found")

// Error type
func errorHandling(): Unit {
    let result = match (compute()) {
        case Ok(value) => println(value)
        case Err(error) => println("Error: ${error}")
    }
}

// Perform and handle (effect system)
handle {
    perform(Action.Request())
} with {
    case Action.Response(data) => process(data)
}

// Resume in effect handling
handle {
    let result = perform(Ask.Value())
    resume(result)
}

// Throwing with multiple types
func multiThrow(): Unit throwing IOException, NetworkException {
    networkOperation()
}

// Common/specific pattern
common interface CommonInterface {
    func commonMethod(): Unit
}

specific interface SpecificInterface {
    func specificMethod(): Unit
}

features interface Features {
    func featureA(): Unit
    func featureB(): Unit
}

// Exception chaining
try {
    highLevel()
} catch (e: LowLevelException) {
    throw HighLevelException("Context: ${e.message}", e)
}

// Try expression
let result = try {
    computeValue()
} catch (e: Exception) {
    defaultValue
}

// Safe navigation avoiding exceptions
let safe = value?.property ?? defaultValue