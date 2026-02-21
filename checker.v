module two

pub struct Checker {
pub mut:
	table SymbolTable
	lambda_count int
	inside_func bool
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

fn (mut c Checker) is_expr_mutable(expr Expr) bool {
	unsafe {
		return match expr {
			VarExpr {
				sym := c.table.lookup_var(expr.name) or {panic('undefined variable ${expr.name}')}
				(!sym.flags.const && sym.flags.mutable)
			}
			ParenExpr {c.is_expr_mutable(expr.expr)}
			DerefExpr {
				inner_type := c.check_expr(expr.expr)
				dump("${inner_type} ${inner_type.pointed_flags}")
				(!inner_type.pointed_flags.const && inner_type.pointed_flags.mutable)
			}
			IndexExpr {c.is_expr_mutable(expr.left)}
			AccessExpr {
				left_type := c.check_expr(expr.left)
				sym := c.table.classes[left_type.name].members[expr.member_name] or {panic('undefined member ${expr.member_name}')}
				(!sym.flags.const && sym.flags.mutable)
			}
			else {false}
		}
	}
}

fn (mut c Checker) get_expr_flags(expr Expr) DeclFlags {
	unsafe {
		return match expr {
			VarExpr {
				sym := c.table.lookup_var(expr.name) or {panic('undefined variable ${expr.name}')}
				sym.flags
			}
			DerefExpr {
				inner_type := c.check_expr(expr.expr)
				inner_type.pointed_flags
			}
			ParenExpr, RefExpr {c.get_expr_flags(expr.expr)}
			IndexExpr {c.get_expr_flags(expr.left)}
			AccessExpr {
				left_type := c.check_expr(expr.left)
				sym := c.table.classes[left_type.name].members[expr.member_name] or {panic('undefined member ${expr.member_name}')}
				sym.flags
			}
			else {DeclFlags{}}
		}
	}
}

fn can_cast(from TypeExpr, to TypeExpr) bool {
	if from.str() == to.str() {
		return true
	}

	if from.ptr_depth > 0 && to.ptr_depth > 0 {
		return true
	}

	if from.ptr_depth > 0 && is_integer_type(to.name) {
		return true
	}
	if to.ptr_depth > 0 && is_integer_type(from.name) {
		return true
	}

	if is_numeric_type(from.name) && is_numeric_type(to.name) {
		return true
	}

	return false
}

fn is_integer_type(name string) bool {
	return name in ['i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64']
}

fn is_float_type(name string) bool {
	return name in ['f32', 'f64']
}

fn is_numeric_type(name string) bool {
	return is_integer_type(name) || is_float_type(name)
}

fn resolve_generic(t TypeExpr, concrete TypeExpr) TypeExpr {
	if t.name == '__generic__' {
		mut resolved := concrete
		resolved.ptr_depth += t.ptr_depth
		return resolved
	}
	mut resolved := t
	resolved.arg_types = t.arg_types.map(resolve_generic(it, concrete))
	if rt := t.ret_type {
		resolved_ret := resolve_generic(*rt, concrete)
		resolved.ret_type = &resolved_ret
		if resolved.ret_type != none {
			unsafe {(*resolved.ret_type).ptr_depth += t.ptr_depth}
		}
	}
	return resolved
}

pub fn (mut c Checker) check(stmts []Stmt) {
	c.table.jump_to_scope(0)
	for stmt in stmts {
		c.check_stmt(stmt)
	}
}

fn (mut c Checker) check_expr(expr Expr) TypeExpr {
	return match expr {
		IntegerLiteral {TypeExpr{name: 'i32'}}
		FloatLiteral   {TypeExpr{name: 'f32'}}
		BoolLiteral    {TypeExpr{name: 'bool'}}
		StringLiteral  {TypeExpr{name: 'string'}}
		ArrayLiteral   {
			mut elem_t := c.check_expr(expr.vals[0])
			for val in expr.vals {
				if !do_types_match(c.check_expr(val), elem_t) {
					panic('arrays must contains values of the same type. found ${c.check_expr(val)} in []${elem_t}')
				}
			}
			TypeExpr{
				name: 'array'
				is_array: true
				elem_type: &elem_t
			}
		}
		NullptrExpr    {TypeExpr{name: 'nullptr'}}
		TypeExpr       {expr}
		LambdaExpr     {c.check_lambda(expr)}
		IndexExpr      {
			leftex := c.check_expr(expr.left)
			if leftex.is_array {
				*leftex.elem_type or {&TypeExpr{name: 'void'}}
			} else {
				panic("tried to index from non-array expression of type ${expr.left.str()}")
			}
		}
		VarExpr        {
			sym := c.table.lookup_var(expr.name) or {
				panic("undefined variable \"${expr.name}\"")
			}
			sym.type
		}
		AccessExpr    {c.check_access_expr(expr)}
		BinaryExpr    {c.check_binary_expr(expr)}
		UnaryExpr     {c.check_unary_expr(expr)}
		ParenExpr     {c.check_expr(expr.expr)}
		VoidExpr      {TypeExpr{name: 'void'}}
		RefExpr       {
			mut t := c.check_expr(expr.expr)
			t.ptr_depth++
			t.pointed_flags = c.get_expr_flags(expr.expr)
			t
		}
		DerefExpr     {
			mut t := c.check_expr(expr.expr)
			t.ptr_depth--
			t
		}
		FnCall        {c.check_fn_call(expr)}
		ClassInstantiation {c.check_class_inst(expr)}
		CastExpr      {c.check_cast_expr(expr)}
	}
}

const assignment_ops = [
	"=", "++", "--", "+=", "-=", "*=", "/="
]

fn (mut c Checker) check_unary_expr(un UnaryExpr) TypeExpr {
	left_type := c.check_expr(un.expr)

	if assignment_ops.contains(un.op) && !c.is_expr_mutable(un.expr) {
		panic('expression is immutable')
	}

	return left_type
}

fn (mut c Checker) check_binary_expr(bin BinaryExpr) TypeExpr {
	left_type := c.check_expr(bin.left)
	right_type := c.check_expr(bin.right)

	if assignment_ops.contains(bin.op) && !c.is_expr_mutable(bin.left) {
		panic('expression is immutable')
	}
	assert_types_match(left_type, right_type)

	return left_type
}

fn (mut c Checker) check_cast_expr(cast CastExpr) TypeExpr {

	from := c.check_expr(cast.expr)
	if !can_cast(from, cast.to) {
		panic("cannot cast from ${from.str()} to ${cast.to.str()}")
	}
	return cast.to
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

fn (mut c Checker) check_access_expr(expr AccessExpr) TypeExpr {
  left_type := c.check_expr(expr.left)

  mut class_name := left_type.name
  if left_type.is_array {
    class_name = 'array'
  }

  class_sym := c.table.lookup_class(class_name) or {
    panic("Type ${left_type.str()} has no members")
   }

	// concrete is the actual type that substitutes __generic__
	// for arrays it's the element type, otherwise it's the
	// type of what the method/member is being called on
	concrete := if left_type.is_array {
		*(left_type.elem_type or { &TypeExpr{name: 'void'} })
	} else {
		left_type
	}

	unsafe {
		if expr.member_name in class_sym.members {
			return resolve_generic(class_sym.members[expr.member_name].type, concrete)
		}

		if expr.member_name in class_sym.methods {
			method := class_sym.methods[expr.member_name]
			mut method_type := resolve_generic(method.type, concrete)
			if method_type.arg_types.len > 0 {
				method_type.arg_types = method_type.arg_types[1..] // strip this
			}
			return method_type
		}
	}

  panic("Member ${expr.member_name} not found on type ${left_type.str()}")
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

		if !can_cast(passed_type, expected_type) {
			panic("Argument ${i+1} mismatch: expected ${expected_type.str()}, got ${passed_type.str()}")
		}
	}

	return *(callee_type.ret_type or {&TypeExpr{name: 'void'}})
}

fn (mut c Checker) check_class_inst(expr ClassInstantiation) TypeExpr {
	sym := c.table.lookup_class(expr.name) or {
		panic("undefined class \"${expr.name}\"")
	}
	if expr.args.len != sym.members.len {
		panic("class instantiation ${expr.name}{} expects ${sym.members.len} \
		members, got ${expr.args.len}")
	}
	mut i := 0
	for i < expr.args.len {
		argtype := c.check_expr(expr.args[i])
		reqtype:= sym.members.values()[i].type
		if !can_cast(argtype, reqtype) {
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
		MethodDecl {c.check_method_decl(stmt)}
		ClassDecl{c.check_class_decl(stmt)}
		Block    {c.check_block(stmt)}
		ExprStmt {c.check_expr(stmt.expr)}
		ReturnStmt {}
		else     {panic("unhandled")}
	}
}

fn (mut c Checker) check_var_decl(decl VarDecl) {
	val_type := c.check_expr(decl.value)

	can_cast(decl.type, val_type)

	mut stored_type := decl.type
	stored_type.pointed_flags = val_type.pointed_flags
	c.table.update_var_type(decl.name, stored_type)
}

fn (mut c Checker) check_func_decl(decl FuncDecl) {
	c.inside_func = true
	c.check_block(decl.block)

	prev_scope := c.table.current_scope_idx
  c.table.jump_to_scope(decl.block.scope_idx)

	rtrns := decl.block.get_all_returns()
	if decl.type != TypeExpr{name: 'void', ptr_depth: 0} && rtrns.len <= 0 {
		panic("Funciton ${decl.name} is expected to return ${decl.type.str()}")
	}

	for rt in rtrns {
		if !can_cast(decl.type, c.check_expr(rt.expr)) {
			panic("Function ${decl.name} must return ${decl.type.str()}\
						, got ${c.check_expr(rt.expr).str()}")
		}
	}

	c.table.jump_to_scope(prev_scope)
	c.inside_func = false
}

fn (mut c Checker) check_method_decl(decl MethodDecl) {

	c.inside_func = true

	if c.table.lookup_class(decl.class_name) == none {
		panic("defining method ${decl.name} for nonexistant class ${decl.class_name}")
	}

	c.check_block(decl.block)

	prev_scope := c.table.current_scope_idx
  c.table.jump_to_scope(decl.block.scope_idx)

	rtrns := decl.block.get_all_returns()
	for rt in rtrns {
		if !do_types_match(decl.type, c.check_expr(rt.expr)) {
			panic("Function ${decl.name} must return ${decl.type.str()}\
						, got ${c.check_expr(rt.expr).str()}")
		}
	}

	c.table.jump_to_scope(prev_scope)
	c.inside_func = false
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

