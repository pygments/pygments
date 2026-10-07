package main

import "fmt"

func main() {
	_ = []rune{'\a', '\b', '\f', '\n', '\r', '\t', '\v', '\\', '\''}
	_ = []rune{'\000', '\007', '\377', '\x07', '\xff', '\u12e4', '\U00101234'}
	_ = []rune{'a', 'ä', '本', '"', 'ß', '€', '@'}
	_ = "simple aä本'ß€@"
	_ = "\a \b \f \n \r \t \v \\ \" \000 \007 \377 \x07 \xff \u12e4 \U00101234 aä本'ß€@"
	fmt.Println(`\a \b \f \n \r \t \v \\ \" \000 \007 \377 \x07 \xff \u12e4 \U00101234 aä本'ß€@ newline->
	<-tab`)
}
