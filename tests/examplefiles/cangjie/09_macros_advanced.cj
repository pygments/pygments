// Advanced macros: parsing, syntax nodes

macro package example.advanced_macros

import std.ast.*

// Parsing functions
public macro parseAndTransform(input: Tokens): Tokens {
    let expr: Expr = parseExpr(input)
    return expr.toTokens()
}

// parseExprFragment for partial parsing
public macro parsePartial(input: Tokens): Tokens {
    let result = parseExprFragment(input, startFrom: 0)
    let expr = result[0]
    let endPos = result[1]
    let remaining = input[endPos..input.size]
    return expr.toTokens() + remaining
}

// parseDecl for declarations
public macro parseDeclaration(input: Tokens): Tokens {
    let decl: Decl = parseDecl(input)
    return decl.toTokens()
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

// Node hierarchy: Expr subclasses (type pattern with :)
public macro analyzeExpr(input: Tokens): Tokens {
    let expr = parseExpr(input)
    
    match (expr) {
        case e: BinaryExpr => processBinary(e)
        case e: CallExpr => processCall(e)
        case e: LambdaExpr => processLambda(e)
        case _ => quote(Unknown expression type)
    }
}

func processBinary(e: BinaryExpr): Tokens {
    let left = e.leftExpr
    let right = e.rightExpr
    return quote(Binary: $(left.toTokens()) + $(right.toTokens()))
}

func processCall(e: CallExpr): Tokens {
    let funcName = e.funcExpr
    return quote(Call: $(funcName.toTokens()))
}

func processLambda(e: LambdaExpr): Tokens {
    return quote(Lambda expression)
}

// Node hierarchy: Decl subclasses (type pattern with :)
public macro analyzeDecl(input: Tokens): Tokens {
    let decl = parseDecl(input)
    
    match (decl) {
        case d: FuncDecl => processFuncDecl(d)
        case d: StructDecl => processStructDecl(d)
        case d: ClassDecl => processClassDecl(d)
        case _ => quote(Unknown declaration type)
    }
}

func processFuncDecl(d: FuncDecl): Tokens {
    let name = d.identifier
    return quote(Func: $(name))
}

func processStructDecl(d: StructDecl): Tokens {
    let name = d.identifier
    return quote(Struct: $(name))
}

func processClassDecl(d: ClassDecl): Tokens {
    let name = d.identifier
    return quote(Class: $(name))
}

// Node modification
public macro modifyBinary(input: Tokens): Tokens {
    let expr = parseExpr(input)
    
    match (expr) {
        case _: BinaryExpr => quote(modified binary)
        case _ => input
    }
}

// Creating nodes from quote
public macro createNode(input: Tokens): Tokens {
    let binExpr = BinaryExpr(quote(1 + 2))
    let funcDecl = FuncDecl(quote(func f1(x: Int64) { return x + 1 }))
    return quote($(binExpr) and $(funcDecl))
}

// Node list interpolation
public macro nodeListInterp(input: Tokens): Tokens {
    var nodes = ArrayList<Node>()
    nodes.add(parseExpr(quote(x += 1)))
    nodes.add(parseExpr(quote(y *= 2)))
    
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

// Deeply nested quotes
public macro deepNested(input: Tokens): Tokens {
    let level1 = quote(x)
    let level2 = quote($(level1))
    let level3 = quote($(level2))
    return level3
}

// Macro generating another macro
public macro generateMacro(input: Tokens): Tokens {
    return quote(
        macro package generated
        public macro gen(input: Tokens): Tokens {
            return $(input)
        }
    )
}

// Complex node construction
public macro buildFunc(input: Tokens): Tokens {
    let funcDecl = FuncDecl(quote(
        func generated() {
            $(input)
        }
    ))
    
    return funcDecl.toTokens()
}

// Error reporting in macros
public macro reportError(input: Tokens): Tokens {
    diagReport(
        DiagReportLevel.ERROR,
        input,
        "Invalid syntax in input",
        "Check the structure"
    )
    return input
}

// Warning reporting
public macro reportWarning(input: Tokens): Tokens {
    diagReport(
        DiagReportLevel.WARNING,
        input,
        "Potentially problematic",
        "Consider revising"
    )
    return input
}

// Built-in compilation tags in macros
public macro withSourceInfo(input: Tokens): Tokens {
    let pkg: String = @sourcePackage()
    let file: String = @sourceFile()
    let line: Int64 = @sourceLine()
    
    return quote(
        // Source: ${pkg}/${file}:${line}
        $(input)
    )
}

// Conditional compilation in macros
@When[os == "linux"]
public macro linuxOnly(input: Tokens): Tokens {
    return input
}

@When[os == "windows"]
public macro windowsOnly(input: Tokens): Tokens {
    return input
}

// Macro with Token construction
public macro constructTokens(input: Tokens): Tokens {
    let tokens = Tokens([
        Token(TokenKind.LET),
        Token(TokenKind.IDENTIFIER, "x"),
        Token(TokenKind.ASSIGN),
        Token(TokenKind.INTEGER_LITERAL, "42")
    ])
    return tokens
}

// Macro concatenating tokens
public macro concatTokens(input: Tokens): Tokens {
    return input + Token(TokenKind.ADD) + input
}

// Macro with type node parsing
public macro parseAndModifyType(typeTokens: Tokens): Tokens {
    let typeNode = parseType(typeTokens)
    
    match (typeNode) {
        case t: GenericTypeNode => processGeneric(t)
        case t: TupleTypeNode => quote(tuple type)
        case _ => typeNode.toTokens()
    }
}

func processGeneric(t: GenericTypeNode): Tokens {
    let name = t.identifier
    return quote($(name))
}

// Macro with pattern parsing
public macro analyzePattern(patternTokens: Tokens): Tokens {
    let pattern = parsePattern(patternTokens)
    
    match (pattern) {
        case p: VarPattern => quote(var pattern)
        case p: EnumPattern => quote(enum pattern)
        case _ => quote(unknown pattern)
    }
}

// Accessing node properties
public macro extractFuncInfo(funcTokens: Tokens): Tokens {
    let decl: FuncDecl = parseDecl(funcTokens)
    
    let name: Token = decl.identifier
    let params: ArrayList<FuncParam> = decl.funcParams
    let retType: TypeNode = decl.declType
    
    return quote(
        Function: $(name)
        Params: $(params.size)
    )
}

// Modifying function body
public macro injectCode(funcTokens: Tokens, extra: Tokens): Tokens {
    let decl: FuncDecl = parseDecl(funcTokens)
    
    let originalBody: Tokens = decl.block.toTokens()
    let modifiedBody: Tokens = quote({
        $(extra)
        $(originalBody)
    })
    
    decl.block = Block(modifiedBody)
    return decl.toTokens()
}

// Macro debugging
public macro debugTokens(input: Tokens): Tokens {
    input.dump()
    return input
}

// @Frozen annotation for stable macro
@Frozen
public macro stableMacro(input: Tokens): Tokens {
    return input
}

// @FastNative for foreign func (example)
@FastNative
foreign func externalHelper(): Int64