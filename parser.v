module two

type Expr = VoidExpr | UnaryExpr | BinaryExpr | IntegerLiteral | FloatLiteral | StringLiteral | BoolLiteral | VarExpr | TypeExpr | ParenExpr | RefExpr | DerefExpr | FnCall | ClassInstantiation | NullptrExpr | LambdaExpr
type LiteralExpr = IntegerLiteral | FloatLiteral | StringLiteral | BoolLiteral | ClassInstantiation | NullptrExpr

struct VoidExpr {}
struct NullptrExpr{}

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
	is_fn bool
	arg_types []TypeExpr
	ret_type ?&TypeExpr
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

struct FnCall {
	callee Expr
	args []Expr
}

struct LambdaExpr {
	args []ArgDecl
	ret_type TypeExpr
	block Block
mut:
	internal_name string
}

struct ClassInstantiation {
	name string
	args []Expr
}

fn (e Expr) is_literal() bool {
	return match e {
		IntegerLiteral, FloatLiteral,
		BoolLiteral, StringLiteral,
		ClassInstantiation, NullptrExpr,
		VoidExpr {true}
		else {false}
	}
}

fn (te TypeExpr) str() string {
	if te.is_fn {
		mut s := "@".repeat(te.ptr_depth)+"fn("
		for t in te.arg_types {
			s += t.str()
		}
		ret_t := *te.ret_type or {&TypeExpr{'void', 0, false, [], none}}
		s += ") -> " + ret_t.str()
		return s
	}
	return ("@".repeat(te.ptr_depth))+te.name
}

// ---------------------------------

struct DeclFlags {
	const bool
	mutable bool
	public bool
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
	if s=="" {
		s+="none"
	}
	return s
}

type Stmt = VarDecl | FuncDecl | ClassDecl | ArgDecl |
						ExprStmt | Member | Block | ReturnStmt

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
	scope_idx int
}

struct ReturnStmt {
	expr Expr
}

fn (b Block) get_all_returns() []ReturnStmt {
	mut returns := []ReturnStmt{}
	for stmt in b.stmts {
		if stmt is ReturnStmt {
			returns << stmt
		} else if stmt is Block {
			returns << stmt.get_all_returns()
		}
	}
	return returns
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
			.leftparen {
				p.parse_call(expr)
			}
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
		.key_nullptr {NullptrExpr{}}
		.key_true, .key_false {BoolLiteral{t.kind == .key_true}}
		.identifier  {p.parse_ident(t.lit)}
		.key_addr    {p.parse_addr()}
		.key_deref   {p.parse_deref()}
		.key_fn      {p.parse_lambda()}
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

fn (mut p Parser) parse_lambda() LambdaExpr {
	p.expect(.leftparen)
  mut args := []ArgDecl{}
  for p.peek().kind != .rightparen {
		arg_name := p.peek().lit
		p.expect(.identifier)
		p.expect(.colon)
    arg_type := p.parse_type()
    args << ArgDecl{name: arg_name, type: arg_type}

  	if p.peek().kind != .rightparen {
			p.expect(.comma)
		}
  }
  p.expect(.rightparen)
  p.expect(.arrow)
  ret_type := p.parse_type()
  block := p.parse_block()

  return LambdaExpr{args: args, ret_type: ret_type, block: block}
}

fn (mut p Parser) parse_ident(ident string) Expr {

	if p.symbols.lookup_class(ident) != none && p.peek().kind == .leftbrace {
		return p.parse_class_instantiation(ident)
	}

	return match p.peek().kind {
		.leftparen {
			p.advance()
			p.parse_call(VarExpr{name: ident})
		}
		else       {VarExpr{name: ident}}
	}
}

fn (mut p Parser) parse_call(left_expr Expr) FnCall {

	mut args := []Expr{}
	for p.peek().kind != .rightparen {
		args << p.parse_expr(.lowest)
		if p.peek().kind != .rightparen {
			p.expect(.comma)
		}
	}
	p.expect(.rightparen)

	return FnCall{
		callee: left_expr
		args: args
	}
}

fn (mut p Parser) parse_class_instantiation(name string) ClassInstantiation {
	p.expect(.leftbrace)
	mut args := []Expr{}
	for p.peek().kind != .rightbrace {
		args << p.parse_expr(.lowest)
		if p.peek().kind != .rightbrace {
			p.expect(.comma)
		}
	}
	p.expect(.rightbrace)

	return ClassInstantiation{
		name: name
		args: args
	}
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
		mut type_ := p.parse_type()
		type_.ptr_depth++
		return type_
	} else if t.kind == .key_fn {
		return p.parse_fn_type()
	}
	t = p.advance()
	if t.kind.is_primitive_type() {
		return TypeExpr{t.lit, 0, false, [], none}
	}

	// class types
	csym := p.symbols.lookup_class(t.lit)
	if csym == none {
		panic("unknown type ${t.lit}")
	}

	return TypeExpr{t.lit, 0, false, [], none}
}

fn (mut p Parser) parse_fn_type() TypeExpr {
	p.expect(.key_fn)
	p.expect(.leftparen)
	mut args := []TypeExpr{}

	for p.peek().kind != .rightparen {
		args << p.parse_type()
		if p.peek().kind != .rightparen {
			p.expect(.comma)
		}
	}

	p.expect(.rightparen)
	p.expect(.arrow)
	ret := p.parse_type()
	return TypeExpr{is_fn: true, arg_types: args, ret_type: &ret}
}

// --- parsing stmts

fn (mut p Parser) parse_stmt() Stmt {
	match p.peek().kind {
		.key_let, .key_mut {return p.parse_var_decl()}
		.key_fn            {return p.parse_fn_decl()}
		.key_class         {return p.parse_class_decl()}
		.key_return        {return p.parse_return()}
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
	p.expect(.arrow)

	ret_type := p.parse_type()

	p.symbols.declare_func(name_tok.lit, args)

	block := p.parse_block()
	p.symbols.exit_scope()

	p.symbols.define_func(name_tok.lit, ret_type, DeclFlags{}, block, args)

	return FuncDecl {
		name_tok.lit
		ret_type
		args
		block
		DeclFlags{}
	}
}

fn (mut p Parser) parse_class_decl() ClassDecl {
	p.expect(.key_class)
	ident_tok := p.peek()
	p.expect(.identifier)

	p.expect(.leftbrace)

	p.symbols.declare_class(ident_tok.lit)

	mut members := []Member{}
	for p.peek().kind != .rightbrace && p.peek().kind != .eof {
		members << p.parse_member()
	}

	p.expect(.rightbrace)

	p.symbols.define_class(ident_tok.lit, members)

	return ClassDecl {
		ident_tok.lit
		members
		DeclFlags {}
	}
}

fn (mut p Parser) parse_member() Member {
	ident_tok := p.peek()
	p.expect(.identifier)

	p.expect(.colon)
	type_expr := p.parse_type()
	p.expect(.semicolon)

	return Member {
		ident_tok.lit
		type_expr
		DeclFlags {}
	}
}

fn (mut p Parser) parse_return() ReturnStmt {
	p.expect(.key_return)
	if p.peek().kind == .semicolon {
		p.advance()
		return ReturnStmt{VoidExpr{}}
	}
	expr := p.parse_expr(.lowest)
	p.expect(.semicolon)
	return ReturnStmt{expr}
}

fn (mut p Parser) parse_block() Block {
	p.expect(.leftbrace)
	mut stmts := []Stmt{}

	p.symbols.enter_scope()

	for p.peek().kind != .rightbrace && p.peek().kind != .eof {
		stmts << p.parse_stmt()
	}

	p.expect(.rightbrace)

	p.symbols.exit_scope()

	return Block {
		stmts: stmts
		scope_idx: p.symbols.current_scope_idx
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
