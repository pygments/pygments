// Macro basics: package declaration, quote expressions, interpolation

// Macro package declaration (required for macros)
macro package example.macros

import std.ast.*

// Basic macro definition
public macro dprint(input: Tokens): Tokens {
    let inputStr = input.toString()
    let result = quote(
        print($(inputStr) + " = ")
        println($(input))
    )
    return result
}

// Macro invocation
@dprint(x + y)

// Macro before declaration
@dprint
let value = compute()

// Macro before function
@LogCall
func process(data: String): Int64 {
    return data.size
}

// Macro before class
@Serialize
class DataContainer {
    let id: Int64
    let name: String
}

// Attribute macro (with arguments in brackets)
public macro WithModifier(attr: Tokens, input: Tokens): Tokens {
    return attr + input
}

@WithModifier[public]
struct PublicStruct {
    let field: Int64
}

@WithModifier[protected static]
var protectedVar: Int64 = 42

// Quote expression basics
let exprTokens = quote(1 + 2)
let funcTokens = quote(func foo() { return 42 })
let classTokens = quote(class Bar { let x: Int64 })

// Quote with interpolation
let name = "myVar"
let value = 100
let varDecl = quote(let $(name) = $(value))

// Quote with multiple interpolations
let a = "x"
let b = "y"
let combined = quote($(a) + $(b))

// Quote with expression interpolation
let expr = computeExpr()
let wrapped = quote(result = $(expr))

// Quote with complex interpolation
let items = [1, 2, 3]
let arrayExpr = quote(arr = $(items))

// Quote interpolation with parentheses (recommended)
let nodeName = parseExpr(quote(x + 1))
let withParens = quote($(nodeName))

// Quote interpolation without parentheses (single identifier)
let simpleInterp = quote($name)

// Escaping in quote expressions
let escapedParen = quote(\()
let escapedDollar = quote(\$(literal))
let escapedBackslash = quote(\\)

// Quote with matched parentheses (no escaping needed)
let matchedParens = quote((x))
let nestedParens = quote(((a + b) * c))

// Quote with nested interpolation
let innerExpr = parseExpr(quote(innerValue))
let outerExpr = quote(outer = $(quote(inner = $(innerExpr))))

// $() interpolation syntax
let tokenList = $(input.tokens)
let nodeInterp = $(parseExpr(input))

// Dollar identifier ($name)
let dollarVar: Tokens = $inputTokens
let dollarExpr: Tokens = $exprTokens

// Tokens and Token construction
let tk1 = Token(TokenKind.ADD)
let tk2 = Token(TokenKind.FUNC)
let tk3 = Token(TokenKind.IDENTIFIER, "myVar")
let tk4 = Token(TokenKind.INTEGER_LITERAL, "42")
let tk5 = Token(TokenKind.STRING_LITERAL, "hello")

// Tokens array construction
let tks = Tokens([
    Token(TokenKind.INTEGER_LITERAL, "1"),
    Token(TokenKind.ADD),
    Token(TokenKind.INTEGER_LITERAL, "2")
])

// Tokens operations
let combinedTokens = tks + anotherTokens
let tokenCount: Int64 = tks.size
let firstToken: Token = tks[0]
let tokensStr: String = tks.toString()
let debugStr: String = tks.dump()

// Macro with Tokens parameter
public macro transform(input: Tokens): Tokens {
    let parsed = parseExpr(input)
    let modified = modifyExpr(parsed)
    return modified.toTokens()
}

// Macro returning Tokens
public macro generateConst(name: Tokens, value: Tokens): Tokens {
    return quote(const $(name) = $(value))
}

@generateConst[MAX_SIZE, 1024]

// Quote with block expression
let blockExpr = quote({
    let x = 1
    let y = 2
    x + y
})

// Quote with match expression
let matchExpr = quote(match (value) {
    case 0 => "zero"
    case _ => "other"
})

// Quote with if expression
let ifExpr = quote(if (condition) {
    doThis()
} else {
    doThat()
})

// Quote with function definition
let funcDef = quote(func add(a: Int64, b: Int64): Int64 {
    return a + b
})

// Quote with struct definition
let structDef = quote(struct Point {
    let x: Float64
    let y: Float64
})

// Quote with class definition
let classDef = quote(class Container<T> {
    let value: T
    init(value: T) { this.value = value }
})

// Macro package visibility
public macro package public_macros
protected macro package protected_macros  
internal macro package internal_macros

// Macro with visibility modifier
public macro publicMacro(input: Tokens): Tokens { return input }
protected macro protectedMacro(input: Tokens): Tokens { return input }
private macro privateMacro(input: Tokens): Tokens { return input }

// Macro with multiple parameters
public macro multiParam(name: Tokens, type: Tokens, value: Tokens): Tokens {
    return quote(let $(name): $(type) = $(value))
}

@multiParam[counter, Int64, 0]

// Macro before var declaration
@InitZero
var counter: Int64

// Macro before enum
@Derive[ToString]
enum Status {
    | Active
    | Inactive
}

// Macro before interface
@GenerateProxy
interface Service {
    func execute(): Int64
}

// Macro before extend
@AddMethods
extend String implements Printable {}

// Macro before prop
@Lazy
prop cachedValue: String {
    get() { return compute() }
}

// Macro before type alias
@ValidateType
type SafeInt = Int64