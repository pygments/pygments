// String literals and interpolation

package example.strings

import std.collection.*

// Basic strings (declarations at top level are OK)
let simpleString: String = "Hello, World!"
let emptyString: String = ""

// Escape sequences in strings
let escapedNewline: String = "Line1\nLine2"
let escapedTab: String = "Tab\tseparated"
let escapedQuote: String = "Quote: \""
let escapedBackslash: String = "Backslash: \\"
let escapedUnicode: String = "Unicode: \u{4E2D}"
let escapedUnicode2: String = "Unicode A: \u{41}"

// Triple-quoted strings (multi-line)
let multilineString: String = """
This is a
multi-line
string
"""

// String interpolation
let name: String = "Alice"
let greeting: String = "Hello, ${name}!"
let expressionInterp: String = "Result: ${1 + 2}"
let nestedInterp: String = "Outer ${"inner ${value}"}"

// Triple-quoted string with interpolation
let multilineInterp: String = """
Name: ${name}
Age: ${age}
"""

// Raw strings with # delimiter
let rawString: String = #"No escaping here: \n \t "#

// Raw string can contain quotes
let rawWithQuotes: String = #"Contains "quotes" inside"# 

// Raw strings with interpolation (literal in raw strings)
let rawWithInterp: String = #"Raw ${name}"#

// Raw single-quoted strings
let rawSingle: String = #'Raw single: \n'#

// Multi-hash raw strings
let rawDouble: String = ##"Contains "# inside"##
let rawTriple: String = ###"Contains "## inside"###

// Raw strings for regex-like patterns
let regexPattern: String = ##"\d+.\d+"##
let pathPattern: String = #"C:\Users\name"# 

// Rune literals (single characters)
let runeA: Rune = 'A'
let runeUnicode: Rune = '\u{4E2D}'
let runeEscape: Rune = '\n'
let runeTab: Rune = '\t'

// Raw rune (byte literal)
let rawRune: Rune = r'\n'

// Triple single-quoted strings (multi-line)
let tripleSingle: String = '''
Also multi-line
with single quotes
'''

// Raw string with escaped dollar (literal $)
let literalDollar: String = #"Price: \$100"#

// Raw string containing interpolation syntax (literal, not interpolated)
let literalInterpSyntax: String = #"This is literal: ${notInterpolated}"#

// Class for nested property access test
class Point { let x: Int64; let y: Int64; init(x: Int64, y: Int64) { this.x = x; this.y = y } }

// Function for method chain test
func compute(): Int64 { return 42 }

main() {
    let name = "Alice"
    let age = 30
    
    // String methods
    let string = "Hello"
    let length: Int64 = string.size
    let upper: String = string.upper()
    let lower: String = string.lower()
    let trimmed: String = string.trim()
    
    // Empty string checks
    let isEmpty: Bool = string.isEmpty()
    
    // String to number conversion
    let fromInt: String = 42.toString()
    let toInt: Option<Int64> = "42".toInt64()
    
    // String splitting
    let parts: Array<String> = "a,b,c".split(",")
    
    // String joining
    let joined: String = ["a", "b", "c"].join(",")
    
    // String contains and search
    let contains: Bool = "hello".contains("ell")
    let startsWith: Bool = "hello".startsWith("hel")
    let endsWith: Bool = "hello".endsWith("lo")
    
    // String comparison
    let str1 = "abc"
    let str2 = "def"
    let areEqual: Bool = str1 == str2
    
    // String as collection
    for (ch in "hello") {
        println(ch)
    }
    
    // String builder for efficient concatenation
    let builder: StringBuilder = StringBuilder()
    builder.append("Hello")
    builder.append(" ")
    builder.append("World")
    let result: String = builder.toString()
    
    // Multi-line raw string preserving everything
    let codeTemplate: String = #"func ${name}() { return ${expr} }"# 
    
    // Multi-line raw string with multiple hash delimiters
    let multilineRaw: String = ###"
func example() {
    return 42
}
"###
    
    // String with complex interpolation expressions
    let items = ["a", "b", "c"]
    let complex: String = "Items: ${items.join(", ")}"
    
    // Property access in interpolation
    let arr = [1, 2, 3]
    let methodInterp: String = "Size: ${arr.size}"
    
    // Nested property access in interpolation
    let point = Point(1, 2)
    let complexInterp: String = "Point: (${point.x}, ${point.y})"
    
    // Method chain in interpolation
    let nestedMethod: String = "Result: ${compute().toString()}"
    
    // String templates with expressions
    let total = 100
    let template: String = """
Summary:
- Total: ${total}
- Average: ${total / 10}
"""
}