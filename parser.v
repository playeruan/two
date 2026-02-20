module two

type Expr = VoidExpr | UnaryExpr | BinaryExpr | IntegerLiteral | FloatLiteral | StringLiteral | BoolLiteral | VarExpr | TypeExpr | ParenExpr | RefExpr | DerefExpr
type LiteralExpr = IntegerLiteral | FloatLiteral | StringLiteral | BoolLiteral

struct VoidExpr {}

struct UnaryExpr {
	expr Expr
	op string
	prefix bool
}

struct BinaryExpr {
	left Expr
	right Expr
	op string
}

struct IntegerLiteral {
	val i64
}

struct FloatLiteral {
	val f64
}

struct StringLiteral {
	val string
}

struct BoolLiteral {
	val bool
}

struct VarExpr {
	name string
}

struct TypeExpr {
	name string
mut:
	ptr_depth int
}

struct ParenExpr {
	expr Expr
}

struct RefExpr {
	expr Expr
}

struct DerefExpr {
	expr Expr
}

fn (e Expr) is_literal() bool {
	return match e {
		IntegerLiteral, FloatLiteral,
		BoolLiteral, StringLiteral {true}
		else {false}
	}
}

fn (te TypeExpr) str() string {
	return ("@".repeat(te.ptr_depth))+te.name
}

// ---------------------------------

struct DeclFlags {
	const bool
	mutable bool
	public bool
	abstract bool
}

fn (f DeclFlags) str() string {
	mut s := ""
	if f.const {
		s+="const "
	}
	if f.mutable {
		s+="mutable "
	}
	if f.public {
		s+="public "
	}
	if f.abstract {
		s+="abstract "
	}
	if s=="" {
		s+="none"
	}
	return s
}

type Stmt = VarDecl | FuncDecl | ClassDecl | ArgDecl |
						ExprStmt | Member | Block

struct VarDecl {
	name string
	type TypeExpr
	value Expr
	flags DeclFlags
}

struct FuncDecl {
	name string
	type TypeExpr
	args []ArgDecl
	block Block
	flags DeclFlags
}

struct ClassDecl {
	name string
	members []Member
	flags DeclFlags
}

struct ArgDecl {
	name string
	type TypeExpr
	flags DeclFlags
}

struct ExprStmt {
	expr Expr
}

struct Member {
	name string
	type TypeExpr
	flags DeclFlags
}

struct Block {
	stmts []Stmt
	scope &Scope
}

// ---------------------------------

pub struct Parser {
mut:
	ts []Token
	symbols SymbolTable
	pos int
	line int
	stmts []Stmt
}

fn (mut p Parser) peek() Token {
	return p.ts[p.pos]
}

fn (mut p Parser) advance() Token {
	t := p.peek()
	p.pos++
	return t
}

fn (mut p Parser) expect(kind TokKind) {
	if p.peek().kind != kind {
		panic("expected ${kind}, got ${p.peek().kind} at line ${p.line}")
	}
	p.advance()
}

// --- parsing exprs
fn (mut p Parser) parse_expr(prec Precedence) Expr {
	mut expr := p.parse_primary()

	for int(prec) < int(p.peek().kind.get_prec()) {
		op_tok := p.advance()

		expr = match op_tok.kind {
			.dot {p.parse_access(expr)}
			.increment, .decrement {UnaryExpr{expr, op_tok.lit, false}}
			else {p.parse_binary(expr, op_tok.lit, op_tok.kind.get_prec())}
		}
	}

	return expr
}

fn (mut p Parser) parse_primary() Expr {
	t := p.advance()

	return match t.kind {
		.integer_lit {IntegerLiteral{t.lit.i64()}}
		.float_lit   {FloatLiteral{t.lit.f64()}}
		.string_lit  {StringLiteral{t.lit}}
		.key_true, .key_false {BoolLiteral{t.kind == .key_true}}
		.identifier  {p.parse_ident(t.lit)}
		.key_addr    {p.parse_addr()}
		.key_deref   {p.parse_deref()}
		else {panic("error here")}
	}
}

fn (mut p Parser) parse_addr() RefExpr {
	p.expect(.leftparen)
  expr := p.parse_expr(.prefix)
	if expr.is_literal() {
		panic("cannot reference a literal value")
	}
	p.expect(.rightparen)
	return RefExpr{expr}
}

fn (mut p Parser) parse_deref() DerefExpr {
	p.expect(.leftparen)
	expr := p.parse_expr(.prefix)
	if expr.is_literal() {
		panic("cannot dereference a literal value")
	}
	p.expect(.rightparen)
	return DerefExpr{expr}
}

fn (mut p Parser) parse_ident(ident string) Expr {
	return match p.peek().kind {
		.leftparen {p.parse_call(ident)}
		else       {VarExpr{name: ident}}
	}
}

fn (mut p Parser) parse_call(callee string) Expr {
	panic("todo")
	return Expr{}
}

fn (mut p Parser) parse_access(expr Expr) Expr {return Expr{}}
fn (mut p Parser) parse_unary_l(expr Expr) Expr {return Expr{}}

fn (mut p Parser) parse_binary(left Expr, op string, prec Precedence) BinaryExpr {
	right := p.parse_expr(prec)
	return BinaryExpr{left, right, op}
}

fn (mut p Parser) parse_type() TypeExpr {
	mut t := p.peek()
	if t.kind == .at {
		p.advance()
		mut type := p.parse_type()
		type.ptr_depth++
		return type
	}
	t = p.advance()
	if t.kind.is_primitive_type() {
		return TypeExpr{t.lit, 0}
	}
	// class types
	return TypeExpr{'void', 0}
}

// --- parsing stmts

fn (mut p Parser) parse_stmt() Stmt {
	match p.peek().kind {
		.key_let, .key_mut {return p.parse_var_decl()}
		.key_fn            {return p.parse_fn_decl()}
		.leftbrace         {return p.parse_block()}
		else               {return p.parse_expr_stmt()}
	}
}

fn (mut p Parser) parse_var_decl() VarDecl {
	mut_ := p.peek().kind == .key_mut
	p.advance()
	name_tok := p.advance()
	if name_tok.kind != .identifier {
		panic("expected identifier after let, got ${name_tok.lit}")
	}

	p.expect(.colon)
	type_expr := p.parse_type()
	p.expect(.eq)
	val := p.parse_expr(.lowest)
	p.expect(.semicolon)

	flags := DeclFlags{
		mutable: mut_
	}

	p.symbols.define_var(name_tok.lit, type_expr, flags)

	return VarDecl{
		name: name_tok.lit
		value: val
		type: type_expr
		flags: flags
	}
}

fn (mut p Parser) parse_fn_decl() FuncDecl {
	p.advance()
	name_tok := p.advance()
	if name_tok.kind != .identifier {
		panic("expected identifier after fn, got ${name_tok.lit}")
	}

	p.expect(.leftparen)

	p.symbols.enter_scope()

	mut args := []ArgDecl{}
	for p.peek().kind != .rightparen {
		mut is_const:= false
		if p.peek().kind == .key_const {
			is_const = true
			p.advance()
		}
		ident_tok := p.peek()
		p.expect(.identifier)
		p.expect(.colon)
		type_expr := p.parse_type()
		if p.peek().kind != .rightparen {
			p.expect(.comma)
		}
		flags := DeclFlags {const: is_const}
		p.symbols.define_var(ident_tok.lit, type_expr, flags)
		args << ArgDecl{
			name: ident_tok.lit
			type: type_expr
			flags: flags
		}
	}

	p.expect(.rightparen)
	p.expect(.colon)

	ret_type := p.parse_type()
	block := p.parse_block()
	p.symbols.exit_scope()

	return FuncDecl {
		name_tok.lit
		ret_type
		args
		block
		DeclFlags{}
	}

}

fn (mut p Parser) parse_block() Block {
	p.expect(.leftbrace)
	mut stmts := []Stmt{}

	p.symbols.enter_scope()

	for p.peek().kind != .rightbrace && p.peek().kind != .eof {
		stmts << p.parse_stmt()
	}

	p.expect(.rightbrace)

	scope := p.symbols.current_scope or {panic("no scope for block")}

	p.symbols.exit_scope()

	return Block {
		stmts: stmts
		scope: scope
	}
}

fn (mut p Parser) parse_expr_stmt() ExprStmt {
	expr := p.parse_expr(.lowest)
	p.expect(.semicolon)
	return ExprStmt{
		expr: expr
	}
}

pub fn (mut p Parser) parse_program(ts []Token) ([]Stmt, SymbolTable){
	p.ts = ts
	p.symbols.enter_scope()
	for p.peek().kind != .eof {
		p.stmts << p.parse_stmt()
	}
	p.symbols.exit_scope()
	return p.stmts, p.symbols
}
