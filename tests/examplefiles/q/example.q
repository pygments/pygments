/ shebang
#!/bin/q

/ preprocessor
\d .namespace

/ keywords such as flip and prd should not be highlighted in comments
"keywords such as flip and prd should not be highlighted in strings"

/ keep syntax highlighting for timing code
\t 1+1
\ts 1+1

/ highlight until comment but not next line
\c 20 40 / comment
a:1 / this is no longer part of the system command

/ 2-digit system commands run til end of line
\cd /foo/bar

/ prompt
q)1+2

/
this is a
multi-line comment
\

 /
this is not a
multi-line comment
\

"string with escapted quote \" and random escape \\"

``foo`bar                       / symbols
`/not/a/symbol                  / not a symbol
`:`:/path/to/file               / file symbols
'`length                        / exception

2000.01m                        / month
2000.01.01                      / date
2000.01.01D                     / timestamp
2000.01.01D00                   / timestamp
2000.01.01D00:00                / timestamp
2000.01.01D00:00:00             / timestamp
2000.01.01D00:00:00.000         / timestamp
2000.01.01D00:00:00.000000      / timestamp

2000.01.01T                     / datetime
2000.01.01T00                   / datetime
2000.01.01T00:00                / datetime
2000.01.01T00:00:00             / datetime
2000.01.01T00:00:00.000         / datetime

00:00                           / time
00:00:00                        / time 
00:00:00.000                    / time

8c6b8b64-6815-6084-0a3e-178401251b68 / guid

101010b                         / booleans
(0w;0N;0n;0Wp)                  / null/infinities
0x01fe                          / octal

(1;1j;1n;1p)                    / long integers
(1c;1h;1i;1t;1u;1v)             / integers
(1e;1f;1.;.1;1.0;1.0f)          / floats
(.1e8;1.e8;1e-8f;1E-8)          / floats

/ ascii operators
(-;=;+;*;#;$;%;@;!;~;^;&;:;.;,;<;>;';\;|;/;?;_)

/ k keywords
(abs; acos; asin; atan; avg; bin; binr; by cor; cos; cov; dev;
 delete; div; do enlist; exec; exit; exp; from; getenv; hopen if;
 in; insert; last; like; log; max; min; prd select; setenv; sin;
 sqrt; ss; sum tan; update; var wavg; while; within; wsum; xexp)

/ q operators
(aj; aj0; ajf; ajf0; all; and; any; asc; asof; attr; avgs; ceiling;
 cols; count; cross; csv; cut; deltas; desc; differ; distinct; dsave;
 each; ej; ema; eval; except; fby; fills; first; fkeys; flip; floor;
 get; group; gtime; hclose; hcount; hdel; hsym; iasc; idesc; ij; ijf;
 inter; inv; key; keys; lj; ljf; load; lower; lsq; ltime; ltrim; mavg;
 maxs; mcount; md5; mdev; med; meta; mins; mmax; mmin; mmu; mod; msum;
 neg; next; not; null; or; over; parse; peach; pj; prds; prior; prev;
 rand; rank; ratios; raze; read0; read1; reciprocal; reval; reverse;
 rload; rotate; rsave; rtrim; save; scan; scov; sdev; set; show;
 signum; ssr; string; sublist; sums; sv; svar; system; tables; til;
 trim; txf; type; uj; ujf; ungroup; union; upper; upsert; value; view;
 views; vs; where; wj; wj1; ww; xasc; xbar; xcol; xcols; xdesc;
 xgroup; xkey; xlog; xprev; xrank)

.foo.bar : {[x;y]x+y}           / function declaration
.foo.bar : {x+y}                / function declaration
foo.bar : "string"              / variable declaration
foo.bar ,: "amend"              / variable amend


{x+y}/[1 2]                     / anonymous function

f:{[x;y]                        / multiline function
 x:`foo;
 y:`bar;
 z:x,y;
 z}
