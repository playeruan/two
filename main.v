module main

import two
import os

fn main() {
	content := os.read_file("source.two") or {return}
	mut l := two.Lexer{}
	ts := l.lex_input(content)
	println(ts)
	mut p := two.Parser{}
	ast, table := p.parse_program(ts)
	println(ast)
	mut c := two.Checker{table: table}
	c.check(ast)
	println("all types are correct :D")
}
