module two

pub struct Checker {
pub mut:
	table SymbolTable
	lambda_count int
	inside_func ?FuncDecl
}

fn assert_types_match(t1 TypeExpr, t2 TypeExpr) {
	if !do_types_match(t1, t2) {
		panic("Type mismatch between ${t1.str()} and ${t2.str()}")
	}
}

fn do_types_match(t1 TypeExpr, t2 TypeExpr) bool {
	if is_nullptr_match(t1, t2) {
		return true
	}
	if t1.str() != t2.str() {
		return false
	}
	return true
}

fn is_nullptr_match(t1 TypeExpr, t2 TypeExpr) bool {
	if (t1.ptr_depth > 0 && t2.name == 'nullptr') ||
		 (t2.ptr_depth > 0 && t1.name == 'nullptr') {
		return true
	}
	return false
}

pub fn (mut c Checker) check(stmts []Stmt) {
	c.table.jump_to_scope(0)
	for stmt in stmts {
		c.check_stmt(stmt)
	}
}

fn (mut c Checker) check_expr(expr Expr) TypeExpr {
	return match expr {
		IntegerLiteral {TypeExpr{'i32',     0, false, [], none}}
		FloatLiteral   {TypeExpr{'f32',     0, false, [], none}}
		BoolLiteral    {TypeExpr{'bool',    0, false, [], none}}
		StringLiteral  {TypeExpr{'string',  0, false, [], none}}
		NullptrExpr    {TypeExpr{'nullptr', 0, false, [], none}}
		TypeExpr       {expr}
		LambdaExpr      {c.check_lambda(expr)}
		VarExpr        {
			sym := c.table.lookup_var(expr.name) or {
				panic("undefined variable \"${expr.name}\"")
			}
			sym.type
		}
		BinaryExpr    {c.check_binary_expr(expr)}
		UnaryExpr     {c.check_expr(expr.expr)}
		ParenExpr     {c.check_expr(expr.expr)}
		VoidExpr      {TypeExpr{'void', 0, false, [], none}}
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
		ClassInstantiation {c.check_class_inst(expr)}
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
	assert_types_match(left_type, right_type)

	return left_type
}

fn (mut c Checker) check_lambda(l LambdaExpr) TypeExpr {
	//l.internal_name = "lambda_${c.lambda_count++}"

	c.table.define_func("__lambda_${c.lambda_count++}", l.ret_type, DeclFlags{}, l.block, l.args)
	mut arg_types := []TypeExpr{}
	for arg in l.args {
		arg_types << arg.type
	}

	return TypeExpr{ is_fn: true, arg_types: arg_types, ret_type: &l.ret_type}
}

fn (mut c Checker) check_fn_call(expr FnCall) TypeExpr {
	callee_type := c.check_expr(expr.callee)

	if !callee_type.is_fn {
		name := if expr.callee is VarExpr { (expr.callee as VarExpr).name } else { "expression" }
		panic("Cannot call $name: it is a ${callee_type.str()}, not a function")
	}

	if expr.args.len != callee_type.arg_types.len {
		panic("Function expects ${callee_type.arg_types.len} arguments, but got ${expr.args.len}")
	}

	for i, arg_expr in expr.args {
		passed_type := c.check_expr(arg_expr)
		expected_type := callee_type.arg_types[i]

		if !do_types_match(passed_type, expected_type) {
			panic("Argument ${i+1} mismatch: expected ${expected_type.str()}, got ${passed_type.str()}")
		}
	}

	return *(callee_type.ret_type or {&TypeExpr{'void', 0, false, [], none}})

	/*sym := c.table.lookup_func(expr.name) or {
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
		if !do_types_match(argtype, reqtype) {
			panic("argument ${i+1} for function ${expr.name}() \
			should be $reqtype, got $argtype")
		}
		i++
	}
	return sym.type*/
}

fn (mut c Checker) check_class_inst(expr ClassInstantiation) TypeExpr {
	sym := c.table.lookup_class(expr.name) or {
		panic("undefined class \"${expr.name}\"")
	}
	if expr.args.len != sym.members.len {
		panic("class ${expr.name}() expects ${sym.members.len} \
		members, got ${expr.args.len}")
	}
	mut i := 0
	for i < expr.args.len {
		argtype := c.check_expr(expr.args[i])
		reqtype:= sym.members.values()[i].type
		if !do_types_match(argtype, reqtype) {
			panic("member ${i+1} for class ${expr.name}() \
			should be $reqtype, got $argtype")
		}
		i++
	}
	return TypeExpr {
		name: expr.name
		ptr_depth: 0
	}
}

fn (mut c Checker) check_stmt(stmt Stmt) {
	match stmt {
		VarDecl  {c.check_var_decl(stmt)}
		FuncDecl {c.check_func_decl(stmt)}
		ClassDecl{c.check_class_decl(stmt)}
		Block    {c.check_block(stmt)}
		ExprStmt {c.check_expr(stmt.expr)}
		ReturnStmt {}
		else     {panic("unhandled")}
	}
}

fn (mut c Checker) check_var_decl(decl VarDecl) {
	val_type := c.check_expr(decl.value)

	assert_types_match(decl.type, val_type)
}

fn (mut c Checker) check_func_decl(decl FuncDecl) {
	c.inside_func = decl
	c.check_block(decl.block)
	rtrns := decl.block.get_all_returns()
	for rt in rtrns {
		if !do_types_match(decl.type, c.check_expr(rt.expr)) {
			panic("Function ${decl.name} must return ${decl.type.str()}\
						, got ${c.check_expr(rt.expr).str()}")
		}
	}
	c.inside_func = none
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
	prev_scope_idx := c.table.current_scope_idx
	c.table.jump_to_scope(block.scope_idx)
	for stmt in block.stmts {
		c.check_stmt(stmt)
	}
	c.table.jump_to_scope(prev_scope_idx)
}

