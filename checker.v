module two

pub struct Checker {
pub mut:
	table SymbolTable
}

fn assert_types_match(t1 TypeExpr, t2 TypeExpr) {
	if t1.str() != t2.str() {
		panic("Type mismatch between ${t1.str()} and ${t2.str()}")
	}
}

pub fn (mut c Checker) check(stmts []Stmt) {
	c.table.jump_to_scope(c.table.scopes[0])
	for stmt in stmts {
		c.check_stmt(stmt)
	}
}

fn (mut c Checker) check_expr(expr Expr) TypeExpr {
	return match expr {
		IntegerLiteral {TypeExpr{'i32',  0}}
		FloatLiteral   {TypeExpr{'f32',  0}}
		BoolLiteral    {TypeExpr{'bool', 0}}
		StringLiteral  {TypeExpr{'string', 0}}
		TypeExpr       {expr}
		VarExpr        {
			sym := c.table.lookup_var(expr.name) or {
				panic("undefined variable \"${expr.name}\"")
			}
			sym.type
		}
		BinaryExpr    {c.check_binary_expr(expr)}
		UnaryExpr     {c.check_expr(expr.expr)}
		ParenExpr     {c.check_expr(expr.expr)}
		VoidExpr      {TypeExpr{'void', 0}}
		RefExpr       {
			mut t := c.check_expr(expr.expr)
			t.ptr_depth++
			t
		}
		DerefExpr     {
			mut t := c.check_expr(expr.expr)
			t.ptr_depth--
			t
		}
		FnCall        {c.check_fn_call(expr)}
	}
}

const assignment_ops = [
	"=", "++", "--", "+=", "-="
]
fn (mut c Checker) check_binary_expr(bin BinaryExpr) TypeExpr {
	left_type := c.check_expr(bin.left)
	right_type := c.check_expr(bin.right)

	/*if assignment_ops.contains(bin.op) && bin.left.flags {

	}*/
	println("todo: check not to assign to const or immutable")
	dump(left_type)
	dump(right_type)
	assert_types_match(left_type, right_type)

	return left_type
}

fn (mut c Checker) check_fn_call(expr FnCall) TypeExpr {
	sym := c.table.lookup_func(expr.name) or {
		panic("undefined function \"${expr.name}\"")
	}
	if expr.args.len != sym.args.len {
		panic("function ${expr.name}() expects ${sym.args.len} \
		arguments, got ${expr.args.len}")
	}
	mut i := 0
	for i < expr.args.len {
		argtype := c.check_expr(expr.args[i])
		reqtype:= sym.args.values()[i].type
		if argtype != reqtype {
			panic("argument ${i+1} for function ${expr.name}() \
			should be $reqtype, got $argtype")
		}
		i++
	}
	return sym.type
}

fn (mut c Checker) check_stmt(stmt Stmt) {
	match stmt {
		VarDecl  {c.check_var_decl(stmt)}
		FuncDecl {c.check_func_decl(stmt)}
		ClassDecl{c.check_class_decl(stmt)}
		Block    {c.check_block(stmt)}
		ExprStmt {c.check_expr(stmt.expr)}
		else     {panic("unhandled")}
	}
}

fn (mut c Checker) check_var_decl(decl VarDecl) {
	val_type := c.check_expr(decl.value)

	assert_types_match(decl.type, val_type)
}

fn (mut c Checker) check_func_decl(decl FuncDecl) {
	c.check_block(decl.block)
}

fn (mut c Checker) check_class_decl(decl ClassDecl) {
	for member in decl.members {
		if member.type.name == decl.name &&
				member.type.ptr_depth < 1 {
			panic("Class recursion is only allowed with pointers")
		}
	}
}

fn (mut c Checker) check_block(block Block) {
	prev_scope := c.table.current_scope or {panic("nonexitent scope")}
	c.table.jump_to_scope(block.scope)
	for stmt in block.stmts {
		c.check_stmt(stmt)
	}
	c.table.jump_to_scope(prev_scope)
}

