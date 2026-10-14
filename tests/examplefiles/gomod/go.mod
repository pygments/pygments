// A module with single-line and grouped directives.
module example.com/project

go 1.26.0

toolchain go1.27.1

godebug default=go1.26
godebug (
	panicnil=1
	asynctimerchan=0
)

require (
	example.com/library v1.2.3 // indirect
	example.com/another/v2 v2.0.0-rc.1
	example.com/legacy v3.1.0+incompatible
	example.com/snapshot v0.0.0-20260102030405-abcdef123456
)

exclude example.com/library v1.2.2

replace (
	example.com/library v1.2.3 => example.com/fork v1.3.0
	example.com/another/v2 => "../local module"
	example.com/legacy => ../legacy
)

retract (
	v0.1.0 // Published in error.
	[v0.2.0, v0.2.3]
)

tool (
	example.com/project/cmd/check
	example.com/project/cmd/generate
)

ignore (
	module
	go
	./node_modules
)
