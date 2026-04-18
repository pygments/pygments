package kubernetes

import (
	strs "strings"
)

// Multi-line strings
description: """
	This is a multi-line string
	that spans multiple lines.
	It supports interpolation: \(name)
	"""

// Advanced number formats
hexValue:           0xFF
octalValue:         0o777
binaryValue:        0b1010
scientificNotation: 1.23e-4
floatWithUnits:     3Ki

interpolation: true

// String variations
singleQuoted:    'single quoted string'
doubleQuoted:    "double quoted string"
multiLineString: """
	multi-line string with \(interpolation)
	"""
sharpString:     #"string with # delimiter"#
multilineSharp:  #"""
	Multi-line sharp string
	with \#(interpolation)
	"""#

// Complex attribute syntax
@tag(go="json,omitempty")
@deprecated("use newField instead")
field?: string

name: "prod"

// Advanced pattern matching
switch: {
	if name == "prod" {
		replicas: 3
	}
	if name == "dev" {
		replicas: 1
	}
}

// Recursive definitions
#Tree: {
	value:  string
	left?:  #Tree
	right?: #Tree
}

// Complex comprehensions
config: {
	for env in ["dev", "staging", "prod"] {
		"\(env)": {
			resources: {
				if env == "prod" {
					cpu:    "2000m"
					memory: "4Gi"
				}
				if env != "prod" {
					cpu:    "500m"
					memory: "1Gi"
				}
			}
		}
	}
}

// Nullable and bottom values
nullable:     null
bottomValue?: _|_
topValue?:    _

// Function calls
userNames: ["a", "b"]
processedData: len(userNames)
combined:      strs.Join(userNames, ", ")

// Complex validation
#validatedConfig: {
	name:     string & =~"^[a-z][a-z0-9-]*[a-z0-9]$"
	port:     int & >0 & <=65535
	protocol: *"TCP" | "UDP"

	// Conditional validation
	if protocol == "TCP" {
		keepAlive?: bool
	}
}
