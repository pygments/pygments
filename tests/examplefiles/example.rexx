/* REXX example. */

/* Some basic constructs. */
almost_pi = 0.1415 + 3
if almost_pi < 3 then
   say 'huh?'
else do
   say 'ok, almost_pi=' almost_pi || " - done"
end
x = '"' || "'" || '''' || """" /* quotes */

/* A comment
   spawning multiple
   lines. */

half: procedure
    parse arg some
    return some / 2

some_label: /* ... ready to go to. */

/* Print a text file on MVS. */
ADDRESS TSO
"ALLOC F(TEXTFILE) DSN('some.text.dsn') SHR REU"
"EXECIO * DISKR TEXTFILE ( FINIS STEM LINES."
"FREE F(TEXTFILE)"
I = 1
DO WHILE I <= LINES.0
   SAY ' LINE ' I ' : ' LINES.I
   I = I + 1
END
