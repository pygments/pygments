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

// Macro invocation (from another package)
// @dprint(x + y)

// Attribute macro (with arguments in brackets)
public macro WithModifier(attr: Tokens, input: Tokens): Tokens {
    return attr + input
}

// Quote expression basics
main() {
    let exprTokens = quote(1 + 2)
    let funcTokens = quote(func foo() { return 42 })
    let classTokens = quote(class Bar { let x: Int64 })
    
    // Quote with interpolation (inside quote())
    let name = "myVar"
    let value = quote(100)
    let varDecl = quote(let $(name) = $(value))
    
    // Quote with multiple interpolations
    let a = quote(x)
    let b = quote(y)
    let combined = quote($(a) + $(b))
    
    // Quote with expression interpolation
    let expr = parseExpr(quote(x + 1))
    let wrapped = quote(result = $(expr))
    
    // Quote with complex interpolation
    let items = quote([1, 2, 3])
    let arrayExpr = quote(arr = $(items))
    
    // Quote interpolation with parentheses (recommended)
    let nodeName = parseExpr(quote(x + 1))
    let withParens = quote($(nodeName))
    
    // Quote interpolation without parentheses (single identifier)
    let simpleInterp = quote($nodeName)
    
    // Escaping in quote expressions
    // (matched parentheses tested elsewhere)
    let escapedDollar = quote(\$(literal))
    
    // Quote with matched parentheses (no escaping needed)
    let matchedParens = quote((x))
    let nestedParens = quote(((a + b) * c))
    
    // Quote with nested interpolation
    let innerExpr = parseExpr(quote(innerValue))
    let outerExpr = quote(outer = $(quote(inner = $(innerExpr))))
    
    // Dollar identifier inside quote (valid)
    let dollarVarInQuote = quote($inputTokens)
    
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
    let anotherTokens = Tokens([Token(TokenKind.INTEGER_LITERAL, "3")])
    let combinedTokens = tks + anotherTokens
    let tokenCount: Int64 = tks.size
    let firstToken: Token = tks[0]
    let tokensStr: String = tks.toString()
    let debugStr: String = tks.dump()
    
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
    
    // Macro with Tokens parameter
    func transform(input: Tokens): Tokens {
        let parsed = parseExpr(input)
        return parsed.toTokens()
    }
    
    // Macro returning Tokens with interpolation
    let constName = quote(MAX_SIZE)
    let constValue = quote(1024)
    let generated = quote(const $(constName) = $(constValue))
}

// Macro with visibility modifier (macros must be public)
public macro publicMacro(input: Tokens): Tokens { return input }

// Macro with escaped keyword parameter name
public macro withEscapedKeyword(input: Tokens): Tokens {
    let `type` = quote(Int64)
    return quote(let x: $(`type`) = $(input))
}

// Macro before enum
public macro Derive(input: Tokens): Tokens { return input }

// Macro before interface
public macro GenerateProxy(input: Tokens): Tokens { return input }

// Macro before extend
public macro AddMethods(input: Tokens): Tokens { return input }

// Macro before type alias
public macro ValidateType(input: Tokens): Tokens { return input }

// ============================================================================
// REGRESSION TEST for bug #14 (discovered in stdlib testing)
// ============================================================================

// Bug #14: $member in quote expressions after member access
// Problem: $member was Error after . in quote expressions  
// Fix: Added $member pattern to member_access and member_after_dot states
public macro testMemberInterpolation(member: Tokens): Tokens {
    quote(this.$member == other.$member)
}

public macro testMultipleMemberInterpolation(field1: Tokens, field2: Tokens): Tokens {
    quote(obj.$field1 == other.$field1 && obj.$field2 != other.$field2)
}