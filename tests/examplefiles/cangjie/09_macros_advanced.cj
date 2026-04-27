// Advanced macros: parsing, syntax nodes, nested macros

macro package example.advanced_macros

import std.ast.*

// Parsing functions
public macro parseAndTransform(input: Tokens): Tokens {
    let expr: Expr = parseExpr(input)
    let transformed: Expr = transformExpr(expr)
    return transformed.toTokens()
}

// parseExprFragment for partial parsing
public macro parsePartial(input: Tokens): Tokens {
    let (expr, endPos) = parseExprFragment(input, startFrom!: 0)
    let remaining = input[endPos..input.size]
    return expr.toTokens() + remaining
}

// parseDecl for declarations
public macro parseDeclaration(input: Tokens): Tokens {
    let decl: Decl = parseDecl(input)
    let processed = processDecl(decl)
    return processed.toTokens()
}

// parseType for type expressions
public macro parseTypeExpr(input: Tokens): Tokens {
    let typeNode: TypeNode = parseType(input)
    return typeNode.toTokens()
}

// parsePattern for pattern matching
public macro parsePatternExpr(input: Tokens): Tokens {
    let pattern: Pattern = parsePattern(input)
    return pattern.toTokens()
}

// Node hierarchy: Expr subclasses
public macro analyzeExpr(input: Tokens): Tokens {
    let expr = parseExpr(input)
    
    match (expr) {
        case e is BinaryExpr => {
            let left = e.leftExpr
            let op = e.op
            let right = e.rightExpr
            return quote(Binary: $(left.toTokens()) ${op} $(right.toTokens()))
        }
        case e is CallExpr => {
            let funcName = e.funcExpr
            let args = e.args
            return quote(Call: $(funcName.toTokens()))
        }
        case e is LambdaExpr => {
            let params = e.params
            let body = e.body
            return quote(Lambda: ${params})
        }
        case _ => quote(Unknown expression type)
    }
}

// Node hierarchy: Decl subclasses
public macro analyzeDecl(input: Tokens): Tokens {
    let decl = parseDecl(input)
    
    match (decl) {
        case d is FuncDecl => {
            let name = d.identifier
            let params = d.funcParams
            let retType = d.declType
            let body = d.block
            return quote(Func: ${name} returns ${retType})
        }
        case d is StructDecl => {
            let name = d.identifier
            let fields = d.fields
            return quote(Struct: ${name})
        }
        case d is ClassDecl => {
            let name = d.identifier
            return quote(Class: ${name})
        }
        case _ => quote(Unknown declaration type)
    }
}

// Node modification
public macro modifyBinary(input: Tokens): Tokens {
    let expr = parseExpr(input)
    
    if (expr is BinaryExpr) {
        let binExpr = expr as BinaryExpr
        binExpr.leftExpr = BinaryExpr(quote(a + b))
        binExpr.op = Token(TokenKind.MUL)
        return binExpr.toTokens()
    }
    return input
}

// Creating nodes from quote
public macro createNode(): Tokens {
    let binExpr = BinaryExpr(quote(1 + 2))
    let funcDecl = FuncDecl(quote(func f1(x: Int64) { return x + 1 }))
    return quote($(binExpr) and $(funcDecl))
}

// Node list interpolation
public macro nodeListInterp(input: Tokens): Tokens {
    var nodes = ArrayList<Node>()
    nodes.add(parseExpr(quote(x += 1)))
    nodes.add(parseExpr(quote(y *= 2)))
    nodes.add(parseDecl(quote(let z = 0)))
    
    let result = quote(func foo() {
        $(nodes)
    })
    return result
}

// Nested quote expressions
public macro nestedQuote(input: Tokens): Tokens {
    let inner = quote($(input))
    let outer = quote(wrapper($(inner)))
    return outer
}

// Quote containing quote
public macro quoteInQuote(): Tokens {
    let innerQuote = quote(1 + 2)
    let wrapper = quote(
        macro package nested
        public macro inner(): Tokens {
            return quote($(innerQuote))
        }
    )
    return wrapper
}

// Deeply nested quotes
public macro deepNested(): Tokens {
    let level1 = quote(x)
    let level2 = quote($(level1))
    let level3 = quote($(level2))
    return level3
}

// Macro generating another macro
public macro generateMacro(name: Tokens, body: Tokens): Tokens {
    return quote(
        macro package generated
        public macro $(name)(input: Tokens): Tokens {
            return $(body)
        }
    )
}

// Nested macro expansion order
@OuterMacro
struct NestedExample {
    @InnerMacro var field1: Int64
    @InnerMacro var field2: String
}

// Message passing between nested macros
public macro InnerMacro(input: Tokens): Tokens {
    assertParentContext("OuterMacro")
    setItem("field", input.toString())
    return input
}

public macro OuterMacro(input: Tokens): Tokens {
    let messages = getChildMessages("InnerMacro")
    
    var fields = ArrayList<Tokens>()
    for (m in messages) {
        let fieldName = m.getString("field")
        fields.add(Token(TokenKind.IDENTIFIER, fieldName))
    }
    
    return quote(struct Modified {
        $(fields)
    })
}

// Complex node construction
public macro buildFunc(name: Tokens, params: Tokens, body: Tokens): Tokens {
    let funcDecl = FuncDecl(quote(
        func $(name)($(params)) {
            $(body)
        }
    ))
    
    funcDecl.identifier = Token(TokenKind.IDENTIFIER, name.toString())
    return funcDecl.toTokens()
}

// Error reporting in macros
public macro reportError(input: Tokens): Tokens {
    diagReport(
        DiagReportLevel.ERROR,
        input,
        "Invalid syntax in input",
        "Check the structure of the provided tokens"
    )
    return input
}

// Warning reporting
public macro reportWarning(input: Tokens): Tokens {
    diagReport(
        DiagReportLevel.WARNING,
        input,
        "Potentially problematic input",
        "Consider revising the structure"
    )
    return input
}

// Built-in compilation tags in macros
public macro withSourceInfo(input: Tokens): Tokens {
    let pkg = @sourcePackage()
    let file = @sourceFile()
    let line = @sourceLine()
    
    return quote(
        // Source: ${pkg}/${file}:${line}
        $(input)
    )
}

// Conditional compilation in macros
@When[os == "linux"]
public macro linuxOnly(input: Tokens): Tokens {
    return input + quote(// Linux-specific)
}

@When[os == "windows"]
public macro windowsOnly(input: Tokens): Tokens {
    return input + quote(// Windows-specific)
}

// Macro with Token construction
public macro constructTokens(): Tokens {
    let tokens = Tokens([
        Token(TokenKind.LET),
        Token(TokenKind.IDENTIFIER, "x"),
        Token(TokenKind.ASSIGN),
        Token(TokenKind.INTEGER_LITERAL, "42")
    ])
    return tokens
}

// Macro concatenating tokens
public macro concatTokens(a: Tokens, b: Tokens): Tokens {
    return a + Token(TokenKind.ADD) + b
}

@concatTokens[x, y]

// Macro with type node parsing
public macro parseAndModifyType(typeTokens: Tokens): Tokens {
    let typeNode = parseType(typeTokens)
    
    match (typeNode) {
        case t is GenericTypeNode => {
            let name = t.identifier
            let args = t.typeArgs
            return quote(${name}<${args}>)
        }
        case t is TupleTypeNode => {
            return quote(tuple type)
        }
        case _ => typeNode.toTokens()
    }
}

// Macro with pattern parsing
public macro analyzePattern(patternTokens: Tokens): Tokens {
    let pattern = parsePattern(patternTokens)
    
    match (pattern) {
        case p is VarPattern => quote(var pattern: ${p.variable})
        case p is StructPattern => quote(struct pattern)
        case p is EnumPattern => quote(enum pattern: ${p.constructor})
        case _ => quote(unknown pattern)
    }
}

// Accessing node properties
public macro extractFuncInfo(funcTokens: Tokens): Tokens {
    let decl: FuncDecl = parseDecl(funcTokens)
    
    let name: Token = decl.identifier
    let params: ArrayList<FuncParam> = decl.funcParams
    let retType: TypeNode = decl.declType
    let body: Block = decl.block
    
    return quote(
        Function name: ${name}
        Parameters: ${params.size}
        Returns: ${retType.toTokens()}
    )
}

// Modifying function body
public macro injectCode(funcTokens: Tokens, extra: Tokens): Tokens {
    let decl: FuncDecl = parseDecl(funcTokens)
    
    let originalBody: Tokens = decl.block.toTokens()
    let modifiedBody: Tokens = quote({
        ${extra}
        ${originalBody}
    })
    
    decl.block = Block(modifiedBody)
    return decl.toTokens()
}

// Macro debugging
public macro debugTokens(input: Tokens): Tokens {
    input.dump()
    return input
}

// Recursive macro logic
public macro expandRecursive(input: Tokens, depth: Int64): Tokens {
    if (depth <= 0) {
        return input
    }
    
    let processed = processOnce(input)
    return expandRecursive(processed, depth - 1)
}