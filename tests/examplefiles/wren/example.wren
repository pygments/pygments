#!/bin/wren

/* IMPORTS */
import "random" for Random as Rand

/* COMMENTS */

//  single line comment

/*
    multiline comment
    /*
        nested multiline comment
    */
*/

/* CLASSES & ATTRIBUTES */

#!type = "parent"
class Parent {
    #method
    static setField (field) {
        __field = field
    }

    construct new(parent) {
        _parent = parent
        return
    }

    parent { _parent }

    foreign method()
}

#!type = "child"
#group(
    multiple,
    lines = true
)
class Child is Parent {
    construct new(parent, child) {
        super(parent)
        _child = child
    }

    child { _child }

    toString { this.parent }
}

/* VARIABLES & STRINGS */

var rand = Rand.new()
var name = """David"""
var fullName = "%(name) Smith"
var firstChild = Child.new("Philip Smith", fullName)
var age = 21
var weight = 70.25
var male = true
var sex = male ? "M" : "F"
var address = """
    "House name" 12 Any Street
    Some Town
    \t %("Some Country") "
"""

/* LOOPS & CONDITIONALS */
for (i in 1..5) {
    if (i == 2) {
        continue
    } else if (i == 4) {
        break
    }
    System.print(i)
}
var j = 6
while (j <= 1e+1) {
    if (j == 8) break
    System.print(j)
    j = j + 1
}

/* ARITHMETIC OPERATORS */
var a = 1
var b = 2
var c = [-a, a + b, a - b, a * b, a / b, a % b]
var add = c[1]

/* BITWISE OPERATORS */
var d = 3
var e = 4
var f = [~d, d & e, d | e, d ^ e, d << 2, e >> 1]

/* COMPARISON OPERATORS */
var g = 5
var h = 6
var i = [a == b, a != b, a < b, a <= b, a > b, a >= b]
var k = firstChild is Parent

/* FUNCTIONS */
var func = Fn.new { |param|
    var z = "this"
    System.print(z + " " + param)
}
func.call("function")

/* MISCELLANEOUS */

var hex = 0x12ac
var nul = null
var l = false
var m = true
var n = l && m
var o = l || m
var esc = "\\ \% \" \0 \a \b \t \f \n \r \v \e \x01 \uabcd \Uabcdef01"
var uni = "£ é 😀  ‎🎷"
var map = {"a": 1, "b": 2}
var iex = "%(map["a"] + map[("b")])"
var odd = (1...h).where { |i| i % 2 == 1 }
                 .toList
var emp = ""
