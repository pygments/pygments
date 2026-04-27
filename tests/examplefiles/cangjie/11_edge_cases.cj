// Edge cases: deeply nested structures, complex interpolation, tricky syntax

package example.edge_cases

import std.collection.*
import std.ast.*

// DEEPLY NESTED STRING INTERPOLATION

// Interpolation inside interpolation (3 levels)
let outer: String = "Outer ${"middle ${"inner ${value}"}"}"

// Interpolation with nested method calls and lambdas
let complexInterp: String = "Result: ${items.map({ x => x.process({ y => y.transform() }) }).join(", ")}"

// Interpolation with ternary inside
let ternaryInterp: String = "Status: ${if (active) "active (${count})" else "inactive"}"

// Interpolation with match expression
let matchInterp: String = "Type: ${match (obj) {
    case s is String => "string: ${s}"
    case n is Int64 => "int: ${n}"
    case _ => "unknown"
}}"

// Interpolation with quote inside
let quoteInInterp: String = "Generated: ${quote(func generated() { return 42 }).toString()}"

// Nested braces in interpolation
let bracesInInterp: String = "Data: ${{
    let temp = compute()
    temp * 2
}}"

// Interpolation with array literal
let arrayInInterp: String = "Array: ${[1, 2, 3].map({ x => x * 2 })}"

// Interpolation with struct construction
let structInInterp: String = "Point: ${Point { x: 10, y: 20 }}"

// MULTI-HASH RAW STRING EDGE CASES

// Raw string containing what looks like interpolation (but is literal)
let fakeInterp: String = #"This is literal: ${notInterpolated}"#

// Raw string with quote and hash combinations
let trickyRaw1: String = ##"Contains "# inside"##
let trickyRaw2: String = ###"Has ##" and "# too"###

// Raw string that looks like closing delimiter but isn't
let almostClosed: String = ####"Almost closes: ###" but continues"####

// Multi-line raw string preserving everything literally
let multilineRaw: String = ###"
func example() {
    let s = "nested string with ${interpolation}"
    let raw = #"fake ${interp}"#
}
"###

// Raw string with backslashes (literal, not escapes)
let rawBackslash: String = #"C:\Users\name\path\to\file"#
let rawRegex: String = ##"\d+\.\d+|[a-z]+"##

// Raw single-quote strings
let rawSingleQuote: String = #'Contains \'escaped\' but literal'# 

// DEEPLY NESTED QUOTE EXPRESSIONS

// Quote inside quote (2 levels)
let nestedQuote1 = quote(
    let inner = quote(x + 1)
    return inner
)

// Quote inside quote inside quote (3 levels)
let nestedQuote2 = quote(
    macro package nested_macro
    public macro deep(): Tokens {
        return quote(
            let deeper = quote(final)
            return deeper
        )
    }
)

// Interpolation in nested quotes
let interpInNestedQuote = quote(
    let val = ${input}
    return quote($(val) + 1)
)

// Quote with $() interpolation containing quote
let dollarQuote = quote(
    let tokens = $(quote(inner tokens))
    return tokens
)

// Quote with complex bracket matching
let complexBrackets = quote({
    if (condition) {
        match (value) {
            case Some(x) => { x.process() }
            case None => { defaultAction() }
        }
    }
})

// COMPLEX ANNOTATIONS

// Annotation with nested brackets and typed arguments
@Validate[
    type: String,
    constraints: [
        minLength: 5,
        maxLength: 100,
        pattern: "[a-zA-Z]+"
    ],
    messages: (error: "Invalid", warning: "Check")
]
let validatedField: String = "test"

// Annotation with function call argument
@Config[env: getEnv("APP_MODE"), fallback: "default"]
let configValue: String = environmentSetting

// Annotation with array argument
@Permissions["read", "write", "admin"]
let accessLevel: String = "user"

// Annotation with nested annotation
@Outer(@Inner[value: 42])
let nestedAnnotField: Int64 = 0

// Multiple annotations stacked
@LogCall
@CacheResult[ttl: 3600]
@Validate[type: Int64]
func stackedAnnotations(x: Int64): Int64 { return x * 2 }

// Annotation on import
@When[os == "linux"]
import std.os.linux.*

@When[!debug]
import std.debug.*

// Annotation on foreign declaration
@When[platform == "native"]
foreign func nativeAlloc(size: Int64): CPointer<Int64>

// COMPLEX GENERICS

// Nested generic types
let nestedGeneric: HashMap<String, ArrayList<Option<Result<Int64, Error>>>> = HashMap<String, ArrayList<Option<Result<Int64, Error>>>>()

// Generic with function type parameter
let funcGeneric: FuncWrapper<(Int64, String) -> Bool> = FuncWrapper<(Int64, String) -> Bool>({ (n, s) => n > 0 })

// Generic with tuple type
let tupleGeneric: Container<(Int64, String, Bool)> = Container<(Int64, String, Bool)>((1, "test", true))

// Generic with where clause
class ConstrainedGeneric<T, U> where T <: Comparable, U <: Serializable {
    let first: T
    let second: U
}

// Generic function with complex constraints
func processGeneric<T, U, V>(a: T, b: U, c: V): V where T <: Number, U <: Convertible<T>, V <: Result<T, Error> {
    return c
}

// Multiple where clauses
class MultiConstraint<A, B, C> where A <: Addable, A <: Multipliable, B <: Comparable<A>, C <: Container<B> {
    func compute(): A { return zeroValue }
}

// COMPLEX OPERATOR CHAINS

// Chained null-safe operators
let chainedNullSafe = obj?.property?.nested?.value ?? defaultValue ?? fallback ?? "final"

// Chained pipeline operators
let piped = inputValue |> process |> validate |> transform |> format

// Complex operator precedence with parens
let precedence = (a + b) * (c - d) / (e ?? f) |> (g && h) || i

// Operators in interpolation
let opInterp = "Calc: ${(a + b) * c / d ?? 0}"

// Spread and range operators combined
let rangeCombo = arr[0..<5] + arr[10..] + arr[..3]

// COMPLEX MATCH PATTERNS

// Match with nested patterns
let nestedMatch = match (complexValue) {
    case Point { x: 0, y: 0 } => "origin"
    case Point { x: n, y: n } where n > 0 => "diagonal positive"
    case Point { x: _, y: y } where y < 0 => "below x-axis"
    case _ => "other"
}

// Match with enum patterns and guards
let enumMatch = match (status) {
    case Status.Active | Status.Pending => "in progress"
    case Status.Completed(result: Some(r)) => "done: ${r}"
    case Status.Completed(result: None) => "done: no result"
    case Status.Error(code: c, msg: m) where c > 500 => "server error: ${m}"
    case Status.Error(code: _, msg: m) => "error: ${m}"
}

// Match with tuple patterns
let tupleMatch = match ((x, y, z)) {
    case (0, 0, 0) => "all zero"
    case (a, b, c) where a == b && b == c => "all equal"
    case (a, _, _) where a > 0 => "positive first"
    case (_, b, _) where b < 0 => "negative middle"
    case (_, _, c) where c == 0 => "zero last"
    case _ => "mixed"
}

// Match with array patterns
let arrayMatch = match (arr) {
    case [] => "empty"
    case [first] => "single: ${first}"
    case [first, second] => "pair: ${first}, ${second}"
    case [first, ...rest] => "first: ${first}, rest: ${rest.size}"
    case [first, middle, ..., last] => "first, middle, last"
}

// COMPLEX LAMBDA NESTING

// Lambda returning lambda
let curried = { x: Int64 => { y: Int64 => { z: Int64 => x + y + z } } }

// Lambda with nested match
let lambdaMatch = { input: Option<Int64> => 
    match (input) {
        case Some(v) => v * 2
        case None => 0
    }
}

// Lambda with complex closure capture
let capturingLambda = {
    let captured = outerValue
    { x: Int64 => captured + x + anotherCapture }
}

// Lambda in lambda in interpolation
let lambdaInInterp = "Result: ${{
    let processor = { x => x * 2 }
    let validator = { x => x > 0 }
    processor(10)
}}"

// COMPLEX TRY-CATCH-FINALLY

// Multiple catch with patterns
func multiCatch(): Int64 {
    try {
        riskyOperation()
    } catch (e: IOException) {
        handleIO(e)
    } catch (e: NetworkException where e.code == 404) {
        handleNotFound(e)
    } catch (e: ValidationException | ParseException) {
        handleInputError(e)
    } catch (e: Exception where e.isRecoverable()) {
        recoverFrom(e)
    } catch (_) {
        genericHandler()
    } finally {
        cleanup()
    }
}

// Try with resource and nested try
func nestedTry(): Result<Data, Error> {
    try (resource = openResource()) {
        try {
            processData(resource)
        } catch (inner: ProcessingError) {
            rollback(resource)
            throw inner
        }
    } catch (outer: ResourceError) {
        return Err(outer)
    }
}

// Throw in expression context
let throwExpr = if (invalid) { throw ValidationError() } else { validValue }

// Throwing function type
func throwingFunc(): Int64 throwing IOException, NetworkException {
    return networkCall()
}

// COMPLEX MACRO INTERPOLATION

// $() with complex expressions
let complexDollar = quote(
    let result = $(parseExpr(quote(complex + expression)))
    return $(processNode(result))
)

// $() with method calls
let dollarMethod = quote($(tokens.map({ t => t.upper() }).join(" ")))

// $name vs $() in same expression
let mixedInterp = quote(
    let simple = $simpleVar
    let complex = $(computeComplex())
)

// Quote with escape sequences for special chars
let escapedQuote = quote(
    let special = \$(literal)
    let paren = \()
)

// Macro generating code with all interpolation types
public macro generateComplete(name: Tokens, type: Tokens, init: Tokens): Tokens {
    return quote(
        let $nameVar = $(init)
        var $(name): $(type) = $nameVar
        prop $(name)Computed: $(type) {
            get() = $(name) * 2
        }
    )
}

// FFI EDGE CASES

// Foreign with complex types
foreign func complexFFI(
    callback: CPointer<(Int64, CString) -> Int64>,
    data: CPointer<Array<CPointer<Int64>>>,
    opts: CPointer<ForeignOptions>
): CPointer<Result<CString, ForeignError>>

// Foreign struct with nested foreign types
foreign struct ForeignComplex {
    foreign let nested: ForeignNested
    foreign func process(input: CPointer<Int64>): CPointer<ForeignResult>
}

// Foreign interface
foreign interface ForeignCallback {
    foreign func onEvent(type: Int64, data: CString): Unit
    foreign func onError(code: Int64, message: CString): Unit
}

// Unsafe block with complex operations
func unsafeComplex(): Int64 {
    unsafe {
        let ptr: CPointer<Int64> = foreignAlloc(100)
        let deref: Int64 = ptr.pointee
        let offset: CPointer<Int64> = ptr.offset(10)
        offset.pointee = deref * 2
        return offset.pointee
    }
}

// COMPLEX CONDITIONAL COMPILATION

// @When on various declarations
@When[os == "linux" && arch == "x86_64"]
foreign func linuxX86(): Int64

@When[os == "windows" || os == "macos"]
foreign func desktopOnly(): Int64

@When[!debug && !test]
func productionOnly(): Int64 { return 42 }

@When[feature == "experimental"]
class ExperimentalFeature {
    @When[version >= 2]
    func newMethod(): Int64 { return 1 }
}

// Nested @When conditions
@When[platform == "mobile"]
class MobileSpecific {
    @When[os == "android"]
    func androidOnly(): Unit {}
    
    @When[os == "ios"]
    func iosOnly(): Unit {}
}

// COMPLEX STRING ESCAPE SEQUENCES

// All escape types in one string
let allEscapes: String = "Escapes: \n \t \r \\ \" \' \0 \$ \u{4E2D} \u{0041}"

// Unicode escapes with various lengths
let unicode4: String = "\u{0041}"
let unicode8: String = "\u{00000041}"
let unicodeChinese: String = "\u{4E2D}\u{6587}"

// Escape in interpolation
let escapeInInterp: String = "Line1\nLine2: ${value}\nLine3"

// Quote expression string content
let quoteString = quote("string in quote with ${interp}")

// NESTED FUNCTIONS (LOCAL FUNCTIONS)

// Basic nested function
func outerWithNested(x: Int64): Int64 {
    func innerHelper(y: Int64): Int64 {
        return y * 2
    }
    return innerHelper(x) + 1
}

// Nested function capturing outer variables
func capturingNested(input: Int64): Int64 {
    let multiplier = 10
    func scaleAndAdd(n: Int64): Int64 {
        return n * multiplier + input
    }
    return scaleAndAdd(5)
}

// Multiple nested functions at same level
func multipleNested(a: Int64, b: Int64): Int64 {
    func add(x: Int64, y: Int64): Int64 { return x + y }
    func mul(x: Int64, y: Int64): Int64 { return x * y }
    func square(n: Int64): Int64 { return mul(n, n) }
    return add(square(a), square(b))
}

// Deeply nested functions (3 levels)
func threeLevelNested(x: Int64): Int64 {
    func level1(n: Int64): Int64 {
        func level2(m: Int64): Int64 {
            func level3(k: Int64): Int64 {
                return k + 1
            }
            return level2(m) * level3(n)
        }
        return level1(n) + level2(x)
    }
    return level1(x)
}

// Nested function with same name as outer (shadowing)
func shadowingExample(value: Int64): Int64 {
    func process(v: Int64): Int64 { return v * 2 }
    func process(v: String): Int64 { return v.size }  // overloaded local
    return process(value)
}

// Nested generic function
func outerGeneric<T>(items: Array<T>): Int64 where T <: Hashable {
    func localHash(item: T): Int64 {
        return hash(item)
    }
    var total: Int64 = 0
    for (item in items) {
        total += localHash(item)
    }
    return total
}

// Nested function with complex signature
func outerComplex(): Int64 {
    func innerComplex(
        data: HashMap<String, ArrayList<Int64>>,
        processor: (Int64) -> Int64,
        validator: (Int64) -> Bool
    ): Result<Int64, Error> {
        let sum = data.values()
            .flatten()
            .map(processor)
            .filter(validator)
            .reduce({ a, b => a + b }, initial!: 0)
        return Ok(sum)
    }
    return innerComplex(HashMap<String, ArrayList<Int64>>(), { x => x }, { x => x > 0 }).unwrapOr(0)
}

// Nested function throwing errors
func outerThrowing(): Int64 {
    func innerThrowing(n: Int64): Int64 throwing ValidationException {
        if (n < 0) {
            throw ValidationException("negative value")
        }
        return n * 2
    }
    try {
        return innerThrowing(10)
    } catch (e: ValidationException) {
        return 0
    }
}

// Nested function returning Option
func outerOption(): Option<Int64> {
    func innerOption(s: String): Option<Int64> {
        if (s.isEmpty()) {
            return None
        }
        return Some(s.size)
    }
    return innerOption("hello")
}

// Nested function in class method
class WithNestedFunc {
    func compute(input: Int64): Int64 {
        func helper(n: Int64): Int64 {
            return n * this.factor
        }
        return helper(input) + this.offset
    }
    
    let factor: Int64 = 2
    let offset: Int64 = 10
}

// NAMED PARAMETERS WITH COMPLEX DEFAULT ARGUMENTS

// Default argument with function call
func withFuncDefault(
    value: Int64 = computeDefault(),
    name: String = generateName()
): Int64 {
    return value + name.size
}

// Default argument with method call
func withMethodDefault(
    data: ArrayList<Int64> = ArrayList<Int64>(),
    config: Config = getDefaultConfig()
): Int64 {
    return data.size
}

// Default argument with constructor
func withConstructorDefault(
    point: Point = Point { x: 0, y: 0 },
    range: Range<Int64> = 0..<100
): Int64 {
    return point.x + range.start
}

// Default argument with lambda
func withLambdaDefault(
    processor: (Int64) -> Int64 = { x => x * 2 },
    validator: (Int64) -> Bool = { x => x > 0 }
): Int64 {
    return processor(10)
}

// Default argument with match expression
func withMatchDefault(
    mode: String = match (getEnv("MODE")) {
        case "prod" => "production"
        case "dev" => "development"
        case _ => "default"
    }
): String {
    return mode
}

// Default argument with ternary
func withTernaryDefault(
    enabled: Bool = isFeatureEnabled() ? true : false,
    count: Int64 = items.size > 0 ? items.size : 10
): Int64 {
    return count
}

// Default argument with interpolation
func withInterpDefault(
    message: String = "Default: ${getDefaultName()} with count ${getDefaultCount()}",
    template: String = "${prefix}: ${value}"
): String {
    return message
}

// Default argument with array literal
func withArrayDefault(
    items: Array<Int64> = [1, 2, 3, 4, 5],
    empty: Array<String> = []
): Int64 {
    return items.reduce({ a, b => a + b }, initial!: 0)
}

// Default argument with tuple
func withTupleDefault(
    pair: (Int64, String) = (0, "default"),
    triple: (Int64, String, Bool) = (1, "one", true)
): Int64 {
    return pair.0
}

// Default argument with Option
func withOptionDefault(
    maybeValue: Option<Int64> = Some(42),
    noValue: Option<String> = None
): Int64 {
    return maybeValue.unwrapOr(0)
}

// Default argument with Result
func withResultDefault(
    result: Result<Int64, Error> = Ok(100),
    errorResult: Result<String, Error> = Err(DefaultError())
): Int64 {
    return result.unwrapOr(0)
}

// Default argument with range operators
func withRangeDefault(
    range1: Range<Int64> = 0..<10,
    range2: Range<Int64> = 0..=10,
    range3: RangeFrom<Int64> = 5..,
    range4: RangeTo<Int64> = ..5
): Int64 {
    return range1.size
}

// Default argument with generic type
func withGenericDefault<T>(
    container: ArrayList<T> = ArrayList<T>(),
    mapper: (T) -> T = { x => x }
): ArrayList<T> where T <: Cloneable {
    return container
}

// Multiple complex defaults
func multiComplexDefaults(
    a: Int64 = compute({ x => x + 1 })(10),
    b: String = "${getPrefix()}-${getId()}",
    c: ArrayList<Int64> = ArrayList<Int64>([1, 2, 3]),
    d: Option<Int64> = Some(match (status) {
        case "ok" => 1
        case _ => 0
    }),
    e: (Int64) -> Int64 = { n => if (n > 0) { n * 2 } else { 0 } }
): Int64 {
    return a + b.size + c.size + d.unwrapOr(0)
}

// Named argument calls with complex expressions
func callWithNamedArgs(): Int64 {
    return multiComplexDefaults(
        a: computeComplex() + extraValue,
        b: "${dynamicPrefix}-${dynamicId}",
        c: ArrayList<Int64>(dynamicItems.map({ x => x })),
        d: Some(computedOption ?? fallbackValue),
        e: { n => processNested({ m => m * 2 }, n) }
    )
}

// Named argument mixing with positional
func mixedCall(): Int64 {
    return multiComplexDefaults(
        positionalValue,
        b: namedString,
        anotherPositional,
        d: namedOption
    )
}

// Named argument with spread
func spreadNamedCall(items: Array<Int64>): Int64 {
    return multiComplexDefaults(
        a: items.reduce({ a, b => a + b }, initial!: 0),
        c: ArrayList<Int64>(...items),
        d: Some(items.size),
        ...otherArgs
    )
}

// Default argument referencing other parameters
func withParamRefDefault(
    base: Int64,
    computed: Int64 = base * 2,
    message: String = "Value is ${computed}"
): Int64 {
    return base + computed
}

// Recursive default (not allowed but syntax test)
func recursiveDefault(
    n: Int64 = recursiveDefault(0)  // would error at runtime
): Int64 {
    return n + 1
}

// Nested function with default arguments
func outerWithNestedDefault(): Int64 {
    func innerWithDefaults(
        x: Int64 = computeInner(),
        y: Int64 = x + 1
    ): Int64 {
        return x + y
    }
    return innerWithDefaults()
}

// Operator function with named arguments
operator func +(a: Int64, b: Int64 = 10): Int64 {
    return a + b
}

let opResult = 5 + (b: 20)  // named argument in operator call

// TRAILING LAMBDA (TRAILING CLOSURE)

// Basic trailing lambda
func withCallback(value: Int64, fn: (Int64) -> Int64): Int64 {
    return fn(value)
}

// Normal call with lambda inside parentheses
let normalCall = withCallback(10, { x => x * 2 })

// Trailing lambda syntax (lambda after parentheses)
let trailingLambda = withCallback(10) { x => x * 2 }

// Trailing lambda with no other arguments
func onlyLambda(fn: () -> Int64): Int64 {
    return fn()
}

let trailingNoArgs = onlyLambda() { 42 }

// Trailing lambda with multiple parameters
func withMultiLambda(a: Int64, b: Int64, fn: (Int64, Int64) -> Int64): Int64 {
    return fn(a, b)
}

let trailingMulti = withMultiLambda(5, 10) { x, y => x + y }

// Trailing lambda with complex body
let trailingComplex = withCallback(100) { x =>
    let temp = x * 2
    let result = temp + 1
    return result
}

// Trailing lambda with nested lambdas
func nestedLambdaCall(fn: (Int64) -> Int64): Int64 {
    return fn(0)
}

let trailingNested = nestedLambdaCall() { n =>
    { inner => inner + n }(10)
}

// Trailing lambda in chained calls
let chainedTrailing = items
    .filter() { x => x > 0 }
    .map() { x => x * 2 }
    .reduce() { a, b => a + b }

// Trailing lambda with named arguments before
func mixedArgs(name: String, count: Int64, processor: (Int64) -> Int64): Int64 {
    return processor(count)
}

let trailingMixed = mixedArgs(name: "test", count: 5) { x => x + 1 }

// Trailing lambda with explicit parameter types
let trailingTyped = withCallback(10) { (x: Int64): Int64 => x * 2 }

// Trailing lambda with return type annotation
let trailingReturn = withCallback(10) { x: Int64 -> Int64 => x * 2 }

// Trailing lambda capturing variables
let captureValue = 100
let trailingCapture = withCallback(5) { x => x + captureValue }

// Trailing lambda with match inside
let trailingMatch = withCallback(10) { x =>
    match (x) {
        case 0 => "zero"
        case n where n > 0 => "positive"
        case _ => "negative"
    }.size
}

// Trailing lambda with try-catch
func errorHandlingLambda(fn: () -> Int64 throwing): Result<Int64, Error> {
    try {
        return Ok(fn())
    } catch (e: Exception) {
        return Err(e)
    }
}

let trailingThrowing = errorHandlingLambda() { =>
    if (unsafeCondition) {
        throw ValidationException()
    }
    return 42
}

// Trailing lambda with generics
func genericLambda<T>(input: T, transform: (T) -> T): T {
    return transform(input)
}

let trailingGeneric = genericLambda("hello") { s => s.upper() }

// Trailing lambda on method call
class Processor {
    func apply(value: Int64, fn: (Int64) -> Int64): Int64 {
        return fn(value)
    }
}

let proc = Processor()
let methodTrailing = proc.apply(10) { x => x * 2 }

// Trailing lambda on static method call
class StaticHelper {
    static func process(input: Int64, fn: (Int64) -> Int64): Int64 {
        return fn(input)
    }
}

let staticTrailing = StaticHelper.process(5) { x => x + 1 }

// Trailing lambda on generic method
class GenericProcessor<T> {
    func transform(input: T, mapper: (T) -> T): T {
        return mapper(input)
    }
}

let genericMethodTrailing = GenericProcessor<Int64>().transform(100) { n => n / 2 }

// Multiple trailing lambdas (error in real code but syntax test)
// func twoLambdas(a: (Int64)->Int64, b: (Int64)->Int64): Int64 { ... }
// twoLambdas() { x => x } { y => y }  // Would error - only one trailing lambda allowed

// Trailing lambda after empty parentheses
func noParamsButLambda(fn: () -> Unit): Unit {
    fn()
}

noParamsButLambda() { println("executed") }

// Trailing lambda with default argument function
func withDefaultLambda(
    value: Int64,
    fn: (Int64) -> Int64 = { x => x }
): Int64 {
    return fn(value)
}

let useDefault = withDefaultLambda(10)  // uses default lambda
let overrideDefault = withDefaultLambda(10) { x => x * 3 }  // trailing lambda overrides default

// Trailing lambda in builder pattern
class Builder {
    private var steps: ArrayList<(Builder) -> Builder> = ArrayList<(Builder) -> Builder>()
    
    func step(fn: (Builder) -> Builder): Builder {
        steps.add(fn)
        return this
    }
}

let built = Builder()
    .step() { b => b.configure() }
    .step() { b => b.validate() }
    .step() { b => b.finalize() }

// Trailing lambda with spread in other args
func withSpreadAndLambda(items: Array<Int64>, processor: (Array<Int64>) -> Int64): Int64 {
    return processor(items)
}

let spreadTrailing = withSpreadAndLambda([1, 2, 3, 4, 5]) { arr =>
    arr.reduce({ a, b => a + b }, initial!: 0)
}

// MACRO/ANNOTATION WITH ARBITRARY TOKENS

// Macro with escaped brackets (literal bracket characters)
@Bar[\[\]random\[]
func escapedBrackets() {}

// Macro with lambda containing braces
@A[{ => (3,4)[0] }()]
func lambdaInAnnotation() {}

// Macro with complex lambda and try-catch
@!CustomAnnotation[{ a:Int,b:Int => try {
    let c=a+b 
    c
} catch (_:OverflowException) {
    a 
}}(1,2)]
func complexLambdaAnnotation() {}

// Macro with arbitrary operators and expressions
@MyMacro[x < - > y .. z |> ~ 1]
func arbitraryOperators() {}

// Macro with nested array indexing and method calls
@Config[data[0].value, settings["key"].nested[1]]
func nestedIndexingMacro() {}

// Macro with match expression inside
@Validator[match (mode) {
    case "strict" => true
    case "relaxed" => false
    case _ => defaultEnabled
}]
func matchInAnnotation() {}

// Macro with if-else expression
@Conditional[if (enabled) { "active" } else { "disabled" }]
func ifElseAnnotation() {}

// Macro with quote expression
@Generator[quote(let x = 1 + 2)]
func quoteInAnnotation() {}

// Macro with function definition inside
@Define[func helper() { return 42 }]
func funcDefInAnnotation() {}

// Macro with class definition inside
@Schema[class TempStruct { let x: Int64 }]
func classDefInAnnotation() {}

// Macro with all escape sequences
@EscapeTest[\[ \] \{ \} \( \) \\]
func escapeSequences() {}

// Macro with nested brackets and braces
@Nested[[1, 2, 3], { x => x * 2 }, [[a, b], [c, d]]]
func nestedBracketsBraces() {}

// Macro with complex expression chain
@Chain[arr.map({ x => x * 2 }).filter({ x => x > 0 }).reduce({ a, b => a + b })]
func chainExpressionMacro() {}

// Macro with throw expression
@ErrorHandling[throw ValidationException("invalid")]
func throwInAnnotation() {}

// Macro with spawn expression
@Concurrency[spawn { concurrentTask() }]
func spawnInAnnotation() {}

// Macro with synchronized block
@ThreadSafe[synchronized { criticalSection() }]
func synchronizedAnnotation() {}

// Macro with unsafe block
@UnsafeOp[unsafe { ptr.pointee = 42 }]
func unsafeAnnotation() {}

// Macro with range operators
@RangeConfig[0..<100, 50..=200, start.., ..end]
func rangeOperatorsMacro() {}

// Macro with pipeline operators
@Pipeline[data |> process |> validate |> format |> output]
func pipelineOperatorsMacro() {}

// Macro with null-safe operators
@NullSafe[obj?.method()?.nested ?? fallback ?? "default"]
func nullSafeOperatorsMacro() {}

// Macro with type expressions
@TypeExpr[Array<HashMap<String, ArrayList<Option<Int64>>>>]
func typeExpressionMacro() {}

// Macro with generic instantiation
@GenericInst[Container<HashMap<String, Int64>>(data)]
func genericInstMacro() {}

// Macro with tuple expressions
@TupleExpr[(1, "two", true), (a, b, c).0, (x, y).1]
func tupleExpressionMacro() {}

// Macro with spread operator
@SpreadMacro[...items, ...other, *[1, 2, 3]]
func spreadOperatorMacro() {}

// Macro with raw identifier
@RawId[`keyword`, `func`, `let`, `class`]
func rawIdentifierMacro() {}

// Macro with interpolation-like syntax (literal in annotation)
@LiteralInterp[${notInterpolated}, $(notParsed)]
func literalInterpMacro() {}

// Macro with regex-like patterns (using raw strings for literal backslash)
@Pattern[#"\\d+\\.?\\d*"#, #"\\w+"#]
func patternMacro() {}

// Macro with complex operators combination
@ComplexOps[a ?? b |> c ~> d :: e <: T :> U ++ f -- g]
func complexOpsMacro() {}

// Macro with nested annotations (parentheses syntax)
@Outer(@Inner[value: 42])
func nestedAnnotationsParens() {}

// Multiple macros with various token patterns
@Macro1[x + y * z]
@Macro2[arr[0..10].map({ x => x })]
@Macro3[match (v) { case _ => 1 }]
func stackedMacros() {}