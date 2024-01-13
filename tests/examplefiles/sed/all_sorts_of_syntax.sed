#!/usr/bin/sed -f

819,$ {
	p; x
	z
	a Hello World!
	i \
	Testy \
	test
}

/START/,/END/ {
	10 y_123456789_abcdefghi_
}

\@[0-9]+@s/[aeiou]*/-\
&\
-/pegw output.txt
