module two

pub struct Checker {
pub mut:
	table SymbolTable
}

pub fn (mut c Checker) check(stmts []Stmt) {
	c.table.jump_to_scope(c.table.scopes[0])
	for stmt in stmts {
		c.check_stmt(stmt)
	}
}

fn (mut c Checker) check_expr(expr Expr) TypeExpr {
	return match expr {
		IntegerLiteral {TypeExpr{'i32'}}
		FloatLiteral   {TypeExpr{'f32'}}
		BoolLiteral    {TypeExpr{'bool'}}
		StringLiteral   {TypeExpr{'string'}}
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
		VoidExpr      {TypeExpr{'void'}}
	}
}

fn (mut c Checker) check_binary_expr(bin BinaryExpr) TypeExpr {
	left_type := c.check_expr(bin.left)
	right_type := c.check_expr(bin.right)

	if left_type.name != right_type.name {
		panic('Binary operator "${bin.op}" cannot be applied to ${left_type.name} and ${right_type.name}')
	}

	return left_type
}

fn (mut c Checker) check_stmt(stmt Stmt) {
	match stmt {
		VarDecl  {c.check_var_decl(stmt)}
		FuncDecl {c.check_func_decl(stmt)}
		Block    {c.check_block(stmt)}
		ExprStmt {c.check_expr(stmt.expr)}
		else     {panic("unhandled")}
	}
}

fn (mut c Checker) check_var_decl(decl VarDecl) {
	val_type := c.check_expr(decl.value)

	if decl.type.name != val_type.name {
		panic("Type miismatch in assignment for ${decl.name}: expected ${decl.type.name}, got ${val_type.name}")
	}
}

fn (mut c Checker) check_func_decl(decl FuncDecl) {

}

fn (mut c Checker) check_block(block Block) {
	prev_scope := c.table.current_scope or {panic("nonexitent scope")}
	c.table.jump_to_scope(block.scope)
	for stmt in block.stmts {
		c.check_stmt(stmt)
	}
	c.table.jump_to_scope(prev_scope)
}

