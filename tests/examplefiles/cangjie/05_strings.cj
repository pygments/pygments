// String literals and interpolation

package example.strings

import std.collection.*

// Basic strings
let simpleString: String = "Hello, World!"
let emptyString: String = ""

// Escape sequences in strings
let escapedNewline: String = "Line1\nLine2"
let escapedTab: String = "Tab\tseparated"
let escapedQuote: String = "Quote: \""
let escapedBackslash: String = "Backslash: \\"
let escapedUnicode: String = "Unicode: \u{4E2D}"
let escapedHex: String = "Hex: \x41"

// Triple-quoted strings (multi-line)
let multilineString: String = """
This is a
multi-line
string
"""

let tripleSingle: String = '''
Also multi-line
with single quotes
'''

// String interpolation
let name: String = "Alice"
let greeting: String = "Hello, ${name}!"
let expressionInterp: String = "Result: ${1 + 2}"
let methodInterp: String = "Size: ${array.size}"
let complexInterp: String = "Point: (${point.x}, ${point.y})"

// Nested interpolation
let nestedInterp: String = "Outer ${"inner ${value}"}"

// Triple-quoted string with interpolation
let multilineInterp: String = """
Name: ${name}
Age: ${age}
Location: ${city}
"""

// Raw strings with # delimiter
let rawString: String = #"No escaping here: \n \t "#

// Raw string can contain quotes
let rawWithQuotes: String = #"Contains "quotes" inside"#

// Raw strings with interpolation (literal in raw strings)
let rawWithInterp: String = #"Raw ${name}"#

// Raw single-quoted strings
let rawSingle: String = #'Raw single: \n'#

// Multi-hash raw strings (allow # and quotes in content)
let rawDouble: String = ##"Contains "# inside"##
let rawTriple: String = ###"Contains "## inside"###

// Raw strings for regex-like patterns
let regexPattern: String = ##"\d+.\d+"##
let pathPattern: String = #"C:\Users\name"#

// J-string (Java-style string for FFI)

// J-string (Java-style string for FFI)
let jstring: JString = J"Java compatible string"

// Raw strings with unmatched brackets
let rawUnmatched: String = #"Unmatched: ( ) ]"#

// Rune literals (single characters)
let runeA: Rune = 'A'
let runeUnicode: Rune = '\u{4E2D}'
let runeEscape: Rune = '\n'
let runeTab: Rune = '\t'

// Raw rune (byte literal)
let rawRune: Rune = r'\n'

// String concatenation
let concatenated: String = "Hello" + " " + "World"

// String methods
let length: Int64 = string.size
let substring: String = string[0..5]
let upper: String = string.upper()
let lower: String = string.lower()
let trimmed: String = string.trim()
let replaced: String = string.replace("old", "new")

// Empty string checks
let isEmpty: Bool = string.isEmpty()
let isNotEmpty: Bool = !string.isEmpty()

// String to number conversion
let fromInt: String = 42.toString()
let fromFloat: String = 3.14.toString()
let toInt: Option<Int64> = "42".toInt64()
let toFloat: Option<Float64> = "3.14".toFloat64()

// String splitting
let parts: Array<String> = "a,b,c".split(",")
let lines: Array<String> = multiline.split("\n")

// String joining
let joined: String = ["a", "b", "c"].join(",")

// String contains and search
let contains: Bool = "hello".contains("ell")
let startsWith: Bool = "hello".startsWith("hel")
let endsWith: Bool = "hello".endsWith("lo")
let indexOf: Option<Int64> = "hello".indexOf("l")

// Format strings
let formatted: String = "${name} is ${age} years old"
let formatWithMethod: String = String.format("%s: %d", name, age)

// String comparison
let areEqual: Bool = str1 == str2
let compareTo: Int64 = str1.compareTo(str2)

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
func ${name}() {
    return ${expr}
}
"###

// String with complex interpolation expressions
let complex: String = "Items: ${items.map({ x => x.name }).join(", ")}"
let nestedMethod: String = "Result: ${compute().toString()}"
let ternaryInterp: String = "Status: ${if (active) "active" else "inactive"}"

// String templates with expressions
let template: String = """
Summary:
- Total: ${data.total}
- Average: ${data.average()}
- Max: ${data.max}
- Min: ${data.min}
"""

// Escaped dollar sign in raw strings
let literalDollar: String = #"Price: \$100"#

// Raw string containing interpolation syntax (literal)
let literalInterpSyntax: String = #"This is literal: ${notInterpolated}"#