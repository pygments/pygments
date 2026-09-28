#!/usr/bin/env nu

# A small Nushell script exercising the lexer.

# Module-level constants and environment.
const APP = "example"
$env.LOG_LEVEL = "info"

# A custom command with a typed signature and flags.
def "build report" [
    dir: path            # directory to scan
    --limit (-n): int    # keep only the largest N files
    --json                # emit JSON instead of a table
] {
    let files = (
        ls $dir
        | where type == file and size > 4kb
        | sort-by size --reverse
        | first $limit
    )

    if $json {
        $files | to json
    } else {
        $files | select name size modified
    }
}

# Strings: raw, single, double and interpolated.
let raw = r#'C:\Users\nu\no-escapes'#
let name = 'nushell'
let greeting = $"Hello from ($name) v(version | get version)!"
print $greeting

# Numbers, durations and file sizes.
let sizes = [1kb 3.5mb 2gib]
let timeout = 30sec
let mask = 0o755
let flags = 0xff
let ratio = 1_000 / 3.0

# Records, lists and closures.
let config = {
    host: "localhost",
    port: 8080,
    retries: 3,
    tags: [alpha beta gamma],
}

let doubled = [1 2 3 4] | each { |x| $x * 2 } | reduce --fold 0 { |it, acc| $acc + $it }

# Ranges, pipelines and boolean operators.
for i in 0..<($config.retries) {
    if ($i mod 2) == 0 and not ($i in [0]) {
        print $"attempt ($i)"
    }
}

# Error handling.
try {
    open nonexistent.toml | from toml
} catch { |err|
    print --stderr $"failed: ($err.msg)"
}

build report ~/downloads --limit 5 --json
