module main

import two
import os

fn main() {
	content := os.read_file("examples/source.two") or {return}
	mut l := two.Lexer{}
	ts := l.lex_input(content)
	println(ts)
	mut p := two.Parser{}
	ast, table := p.parse_program(ts)
	println(ast)
	mut c := two.Checker{table: table}
	c.check(ast)
	println("all types are correct :D")
	mut qg := two.QbeGen{}
  qg.gen_program(ast, table)
	os.write_file("examples/out.qbe", qg.buf.str()) or {panic('error writing out.qbe')}
	os.execute("qbe examples/out.qbe -o examples/out.s")
	os.execute("cc examples/out.s -o examples/out")

	//println(table)
}
