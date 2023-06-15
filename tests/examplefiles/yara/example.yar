import pe
/*
    Test multi-line comment
*/

rule SyntaxExample
{
    meta:
        // Useful meta information examples to add
        description = "Test"
        author = "Test"
        reference = "https://github.com/infosec-intern/vscode-yara/wiki"
        os = "mswindows"
        filetype = "pe"
        maltype = "trojan"
        date = "2016-06"
    strings:
        $true = true
        $false = false
        $hex_string = { E2 34 ?? C8 A? FB [2-4] }
        $hex_string2 = { F4 23 ( 62 B4 | 56 ) 45 }
        $dstring = "double # string" wide nocase fullword
        $reg_ex = /md5: [0-9a-zA-Z]{32}/    // greedy test comment
    condition:
        $hex_string
        for any of ($a,$b,$c) : ( $ )
        17 or none of them
        filesize < 200MB
        #dstring == 8
        @dstring[1]
        $hex_*
}

// test single-line comment
rule RuleReferenceExample
{
    meta:
        description = "Rule Reference Example {this should not be here}"
        author = "Test"
        reference = "https://github.com/infosec-intern/vscode-yara/wiki"
    strings:
        $hex_string = "test"    /*greedy comment*/
    condition:
        SyntaxExample and $hex_string
}

rule Yara4Example
{
    meta:
        description = "Example rule to test features added in version 4.0"
        author = "test /test/"
    strings:
        $b64name = "string" base64 # some more comments
        $b64wname = "string" base64wide
    condition:
        any of them
        and pe.pdb_path == "C:\\fake_pdb_path"
        and pe.dll_name == "library.dll"
        and pe.export_timestamp == 000000000
        and pe.exports_index(40)
}
# another comment
