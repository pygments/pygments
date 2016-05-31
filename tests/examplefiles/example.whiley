/**
 * Example Whiley program, taken from the Whiley benchmark suite.
 * https://github.com/Whiley/WyBench/blob/master/src/101_interpreter/Main.whiley
 */

import whiley.lang.System
import whiley.lang.Int
import whiley.io.File
import string from whiley.lang.ASCII
import char from whiley.lang.ASCII

// ====================================================
// A simple calculator for expressions
// ====================================================

constant ADD is 0
constant SUB is 1
constant MUL is 2
constant DIV is 3

// binary operation
type BOp is (int x) where ADD <= x && x <= DIV
type BinOp is { BOp op, Expr lhs, Expr rhs } 

// variables
type Var is { string id }

// list access
type ListAccess is { 
    Expr src, 
    Expr index
} 

// expression tree
type Expr is int |  // constant
    Var |              // variable
    BinOp |            // binary operator
    Expr[] |           // array constructor
    ListAccess         // list access

// values
type Value is int | Value[]

// stmts
type Print is { Expr rhs }
type Set is { string lhs, Expr rhs }
type Stmt is Print | Set

// ====================================================
// Expression Evaluator
// ====================================================

type RuntimeError is { string msg }
type Environment is [{string k, Value v}]

// Evaluate an expression in a given environment reducing either to a
// value, or a runtime error.  The latter occurs if evaluation gets
// "stuck" (e.g. expression is // not well-formed)
function evaluate(Expr e, Environment env) -> Value | RuntimeError:
    //
    if e is int:
        return e
    else if e is Var:
        return env[e.id]
    else if e is BinOp:
        Value|RuntimeError lhs = evaluate(e.lhs, env)
        Value|RuntimeError rhs = evaluate(e.rhs, env)
        // check if stuck
        if !(lhs is int && rhs is int):
            return {msg: "arithmetic attempted on non-numeric value"}
        // switch statement would be good
        if e.op == ADD:
            return lhs + rhs
        else if e.op == SUB:
            return lhs - rhs
        else if e.op == MUL:
            return lhs * rhs
        else if rhs != 0:
            return lhs / rhs
        return {msg: "divide-by-zero"}
    else if e is Expr[]:
        [Value] r = []
        for i in e:
            Value|RuntimeError v = evaluate(i, env)
            if v is RuntimeError:
                return v
            else:
                r = r ++ [v]
        return r
    else if e is ListAccess:
        Value|RuntimeError src = evaluate(e.src, env)
        Value|RuntimeError index = evaluate(e.index, env)
        // santity checks
        if src is [Value] && index is int && index >= 0 && index < |src|:
            return src[index]
        else:
            return {msg: "invalid list access"}
    else:
        return 0 // dead-code

// ====================================================
// Expression Parser
// ====================================================

type State is { string input, int pos }
type SyntaxError is { string msg, int start, int end }

function SyntaxError(string msg, int start, int end) -> SyntaxError:
    return { msg: msg, start: start, end: end }

// Top-level parse method
function parse(State st) -> (Stmt,State)|SyntaxError:
    //
    Var keyword, Var v
    Expr e
    int start = st.pos
    //
    keyword,st = parseIdentifier(st)
    switch keyword.id:
        case "print":
            any r = parseAddSubExpr(st)
            if !(r is SyntaxError):
                e,st = r
                return {rhs: e},st
            else:
                return r // error case
        case "set":
            st = parseWhiteSpace(st)
            v,st = parseIdentifier(st)
            any r = parseAddSubExpr(st)
            if !(r is SyntaxError):
                e,st = r
                return {lhs: v.id, rhs: e},st
            else:
                return r // error case
        default:
            return SyntaxError("unknown statement",start,st.pos-1)

function parseAddSubExpr(State st) -> (Expr, State)|SyntaxError:    
    //
    Expr lhs, Expr rhs      
    // First, pass left-hand side 
    any r  = parseMulDivExpr(st)
    //
    if r is SyntaxError:
        return r
    //    
    lhs,st = r
    st = parseWhiteSpace(st)
    // Second, see if there is a right-hand side
    if st.pos < |st.input| && st.input[st.pos] == '+':
        // add expression
        st.pos = st.pos + 1
        r = parseAddSubExpr(st)        
        if !(r is SyntaxError):
            rhs,st = r
            return {op: ADD, lhs: lhs, rhs: rhs},st
        else:
            return r
    else if st.pos < |st.input| && st.input[st.pos] == '-':
        // subtract expression
        st.pos = st.pos + 1
        r = parseAddSubExpr(st)        
        if !(r is SyntaxError):
            rhs,st = r
            return {op: SUB, lhs: lhs, rhs: rhs},st
        else:
            return r    
    // No right-hand side
    return (lhs,st)

function parseMulDivExpr(State st) -> (Expr, State)|SyntaxError:    
    // First, parse left-hand side
    Expr lhs, Expr rhs
    any r  = parseTerm(st)
    if r is SyntaxError:
        return r
    //
    lhs,st = r
    st = parseWhiteSpace(st)
    // Second, see if there is a right-hand side
    if st.pos < |st.input| && st.input[st.pos] == '*':
        // add expression
        st.pos = st.pos + 1
        r = parseMulDivExpr(st)   
        if !(r is SyntaxError):
            rhs,st = r
            return {op: MUL, lhs: lhs, rhs: rhs}, st
        else:
            return r
    else if st.pos < |st.input| && st.input[st.pos] == '/':
        // subtract expression
        st.pos = st.pos + 1
        r = parseMulDivExpr(st)        
        if !(r is SyntaxError):
            rhs,st = r
            return {op: DIV, lhs: lhs, rhs: rhs}, st
        else:
            return r
    // No right-hand side
    return (lhs,st)

function parseTerm(State st) -> (Expr, State)|SyntaxError:
    //
    st = parseWhiteSpace(st)        
    if st.pos < |st.input|:
        if ASCII.isLetter(st.input[st.pos]):
            return parseIdentifier(st)
        else if ASCII.isDigit(st.input[st.pos]):
            return parseNumber(st)
        else if st.input[st.pos] == '[':
            return parseList(st)
    //
    return SyntaxError("expecting number or variable",st.pos,st.pos)

function parseIdentifier(State st) -> (Var, State):
    //
    string txt = ""
    // inch forward until end of identifier reached
    while st.pos < |st.input| && ASCII.isLetter(st.input[st.pos]):
        txt = txt ++ [st.input[st.pos]]
        st.pos = st.pos + 1
    return ({id:txt}, st)

function parseNumber(State st) -> (Expr, State)|SyntaxError:    
    // inch forward until end of identifier reached
    int start = st.pos
    while st.pos < |st.input| && ASCII.isDigit(st.input[st.pos]):
        st.pos = st.pos + 1    
    //
    int|null iv = Int.parse(st.input[start..st.pos])
    if iv == null:
        return SyntaxError("Error parsing number",start,st.pos)
    else:
        return iv, st

function parseList(State st) -> (Expr, State)|SyntaxError:    
    //
    st.pos = st.pos + 1 // skip '['
    st = parseWhiteSpace(st)
    [Expr] l = [] // initial list
    bool firstTime = true
    while st.pos < |st.input| && st.input[st.pos] != ']':
        if !firstTime && st.input[st.pos] != ',':
            return SyntaxError("expecting comma",st.pos,st.pos)
        else if !firstTime:
            st.pos = st.pos + 1 // skip ','
        firstTime = false
        any r = parseAddSubExpr(st)
        if r is SyntaxError:
            return r
        else:
            Expr e
            e,st = r
            // perform annoying error check    
            l = l ++ [e]
            st = parseWhiteSpace(st)
    st.pos = st.pos + 1
    return l,st
 
// Parse all whitespace upto end-of-file
function parseWhiteSpace(State st) -> State:
    while st.pos < |st.input| && ASCII.isWhiteSpace(st.input[st.pos]):
        st.pos = st.pos + 1
    return st

// ====================================================
// Main Method
// ====================================================

public method main(System.Console sys):
    if(|sys.args| == 0):
        sys.out.println("no parameter provided!")
    else:
        File.Reader file = File.Reader(sys.args[0])
        string input = ASCII.fromBytes(file.readAll())
        
        Environment env = Environment()
        State st = {pos: 0, input: input}
        while st.pos < |st.input|:
            Stmt s
            any r = parse(st)
            if r is SyntaxError:
                sys.out.println("syntax error: " ++ r.msg)  
                return
            s,st = r
            Value|RuntimeError v = evaluate(s.rhs,env)
            if v is RuntimeError:
                sys.out.println("runtime error: " ++ v.msg) 
                return
            if s is Set:
                env[s.lhs] = v
            else:
                sys.out.println(r)
            st = parseWhiteSpace(st)
            
