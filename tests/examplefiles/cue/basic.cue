// Basic CUE example showcasing key language features
package example

import "strings"

// Basic value definitions
name:   strings.ToUpper("John")
age:    30
active: true

// Constraints and validation
score: >50 & <100
score: 75
email: =~"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
email: "cueckoo@cuelang.org"

// Structs and nested structures
#person: {
	name!: string
	age?:  int & >=0 & <=120
	address?: {
		street?: string
		city?:   string
		zip?:    =~"^[0-9]{5}(-[0-9]{4})?$"
	}
}

// Lists and comprehensions
users: [
	{name: "Alice", role: "admin"},
	{name: "Bob", role: "user"},
]

// For comprehension
userNames: [for u in users {u.name}]

// Conditionals
if age > 100 {
	status: "pending"
}

// Templates and definitions
#User: {
	name: string
	age:  int & >=0
	role: "admin" | "user" | "guest"
}

// String interpolation
greeting: "Hello, \(name)! You are \(age) years old."

// Functions
doubleAge: age * 2

// Let expressions
let x = 10
result: x * 2

// Hidden fields
_internal: "secret"

// Optional fields
optional?: string

// Disjunctions
status: *"active" | "inactive" | "pending"

// Numbers with units
memory: 8Gi
