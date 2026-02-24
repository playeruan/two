module two

import strings

pub struct QbeGen {
pub mut:
	buf            strings.Builder
	tmp_ctr        int
	lbl_ctr        int
	vars_temp_values map[string]GenVal
	block_terminated bool
	symbols        SymbolTable
}

struct GenVal {
	val string // "%t2" or "5" for example
	typ string // "w", "s" etc.
	is_addr bool
	two_typ string // 'i32', 'u64' ...
}
const voidgenval := GenVal{'', '', false, ''}

fn (mut g QbeGen) twotype_to_qbetype(t string) string {
	return match t {
		'i32', 'u32' {'w'}
		'i64', 'u64' {'l'}
		'f32'  {'s'}
		'f64'  {'d'}
		'bool' {'w'}
		'void' {''}
		else         {panic('unhandled type "${t}"')}
	}
}

fn (mut g QbeGen) qbetype_bytesize(t string) u8 {
	return match t {
		'w', 's'     {4}
		'l', 'd'     {8}
		else         {panic('unhandled')}
	}
}

fn (mut g QbeGen) is_type_signed(name string) bool {
	return ![
		"u8", "u16", "u32", "u64"
	].contains(name)
}

fn (mut g QbeGen) instr_from_op(op string, src_type string) string {
	is_float    := src_type in ['f32', 'f64']
	is_unsigned := !is_float && !g.is_type_signed(src_type)
	cmp_prefix  := if is_float { '' } else if is_unsigned { 'u' } else { 's' }
	return match op {
		'+'  { 'add' }
		'-'  { 'sub' }
		'*'  { 'mul' }
		'/'  { if is_unsigned { 'udiv' } else { 'div' } }
		'==' { 'c${cmp_prefix}eq' }
		'!=' { 'c${cmp_prefix}ne' }
		'<'  { 'c${cmp_prefix}lt' }
		'>'  { 'c${cmp_prefix}gt' }
		'<=' { 'c${cmp_prefix}le' }
		'>=' { 'c${cmp_prefix}ge' }
		else { panic('unhandled op ${op}') }
	}
}

fn (mut g QbeGen) new_tmp() string {
	g.tmp_ctr++
	return '%t${g.tmp_ctr}'
}

fn (mut g QbeGen) new_label() string {
	g.lbl_ctr++
	return '@lbl_${g.lbl_ctr}'
}

fn (mut g QbeGen) emit(line string) {
	g.buf.write_string('\t${line}\n')
}

fn (mut g QbeGen) emit_label(line string) {
	g.buf.write_string('${line}\n')
}

fn (mut g QbeGen) gen_expr(expr Expr, expected_t TypeExpr) GenVal {
	return match expr {
		VoidExpr {voidgenval}
		IntegerLiteral {
			t := g.twotype_to_qbetype('i32')
			GenVal{expr.val.str(), t, false, 'i32'}
		}
		FloatLiteral {
			t := g.twotype_to_qbetype('f32')
			GenVal{t+"_"+expr.val.str(), t, false, 'f32'}
		}
		BoolLiteral {
			t := g.twotype_to_qbetype('bool')
			lit := if expr.val {"1"} else {"0"}
			GenVal{lit, t, false, 'bool'}
		}
		VarExpr        {
			t := g.vars_temp_values[expr.name]
			if t.is_addr {
				tmp := g.new_tmp()
				g.emit("${tmp} =${t.typ} load${t.typ} ${t.val}")
				GenVal{tmp, t.typ, true, t.two_typ}
			} else {
				t
			}
		}
		UnaryExpr {
			if expr.op == '-' {
      	operand := g.gen_expr(expr.expr, expected_t)
      	tmp := g.new_tmp()
      	g.emit('${tmp} =${operand.typ} sub 0, ${operand.val}')
      	return GenVal{tmp, operand.typ, false, operand.two_typ}
    	} else if expr.op in ['++', '--'] {
    		left_name := match expr.expr {
      	  VarExpr { expr.expr.name }
      	  else    { panic('operand of ++ / -- must be a variable') }
    		}
    		slot := g.vars_temp_values[left_name]
    		cur := g.new_tmp()
    		g.emit('${cur} =${slot.typ} load${slot.typ} ${slot.val}')
    		result := g.new_tmp()
  			op := if expr.op == '++' { 'add' } else { 'sub' }
    		g.emit('${result} =${slot.typ} ${op} ${cur}, 1')
    		g.emit('store${slot.typ} ${result}, ${slot.val}')
    		GenVal{cur, slot.typ, false, slot.two_typ}
			} else {
				panic('unexpected prefix unary operator')
			}
		}
		BinaryExpr {
			if expr.op in ['=', '+=', '-=', '*=', '/='] {
				left_name := match expr.left {
					VarExpr { expr.left.name }
					else    { panic('left side of assignment must be a variable') }
				}
				lhs := g.vars_temp_values[left_name]

				if expr.op == '=' {
        	rhs := g.gen_expr(expr.right, expected_t)
        	g.emit('store${lhs.typ} ${rhs.val}, ${lhs.val}')
        	return GenVal{rhs.val, lhs.typ, false, lhs.two_typ}
    		}

    		// load current value
    		cur := g.new_tmp()
    		g.emit('${cur} =${lhs.typ} load${lhs.typ} ${lhs.val}')

    		result := g.new_tmp()
      	rhs := g.gen_expr(expr.right, expected_t)
        base_op := expr.op.replace('=', '')
				g.emit('${result} =${lhs.typ} ${g.instr_from_op(base_op, lhs.two_typ)} ${cur}, ${rhs.val}')
    		g.emit('store${lhs.typ} ${result}, ${lhs.val}')
    		GenVal{result, lhs.typ, false, lhs.two_typ}
			} else {
				l := g.gen_expr(expr.left, expected_t)
				r := g.gen_expr(expr.right, expected_t)
				tmp := g.new_tmp()
	  		is_cmp := expr.op in ['==', '!=', '<', '>', '<=', '>=']
	  		res_typ := if is_cmp { 'w' } else { l.typ }
	  		opsufx  := if is_cmp { l.typ } else { '' }
				g.emit('${tmp} =${res_typ} ${g.instr_from_op(expr.op, l.two_typ)}${opsufx} ${l.val}, ${r.val}')
				GenVal{tmp, res_typ, false, l.two_typ}
			}
		}
		FnCall {
			mut arg_vals := []GenVal{}
			for arg in expr.args {
				arg_vals << g.gen_expr(arg, expected_t)
			}
			tmp := g.new_tmp()
			callee_name := match expr.callee {
				VarExpr {expr.callee.name}
				else   {panic('unhandled callee')}
			}
			sym := g.symbols.lookup_func(callee_name) or {
        panic('undefined function ${callee_name}')
    	}
    	ret_type := sym.type
    	ret_qbe  := if ret_type.name == 'void' { '' } else { g.twotype_to_qbetype(ret_type.str()) }
			mut argsstr := "("
			for arg_v in arg_vals {
				argsstr += "${arg_v.typ} ${arg_v.val}"
				if arg_v != arg_vals[arg_vals.len-1] {argsstr+=", "}
			}
			argsstr += ")"
			if ret_qbe == '' {
    	g.emit("call \$${callee_name}${argsstr}")
    		voidgenval
			} else {
    		g.emit("${tmp} =${ret_qbe} call \$${callee_name}${argsstr}")
    		GenVal{tmp, ret_qbe, false, ret_type.str()}
			}
		}
		else {panic('unhandled')}
	}
}

fn (mut g QbeGen) gen_func(decl FuncDecl) {
	g.vars_temp_values.clear()
	mut has_returned := false
	mut ret_t := g.twotype_to_qbetype(decl.type.str())
	if ret_t != "" {
		ret_t += " "
	}
	g.buf.write_string('${"export ".repeat(int(decl.flags.export))}function ${ret_t}\$${decl.name}(')
	for arg in decl.args {
		arg_t := g.twotype_to_qbetype(arg.type.str())
		g.buf.write_string('${arg_t} %${arg.name}_arg')
		if arg != decl.args[decl.args.len - 1] {
			g.buf.write_string(', ')
		}
    g.vars_temp_values[arg.name] = GenVal{'%${arg.name}_arg', arg_t, true, arg.type.str()}
	}
	g.buf.write_string(') {\n')
	g.emit_label('@start')
	for stmt in decl.block.stmts {
		g.gen_stmt(stmt, decl.type)
		if stmt is ReturnStmt {has_returned = true}
	}
	if !has_returned {
		g.emit('ret')
	}
	g.buf.write_string('}\n')
}

fn (mut g QbeGen) gen_stmt(stmt Stmt, expected_t TypeExpr) {
	match stmt {
		VarDecl    { g.gen_local_var(stmt) }
		ReturnStmt {
			g.block_terminated = true
			g.gen_return(stmt, expected_t)
		}
		IfChain    { g.gen_if_chain(stmt, expected_t) }
		Block      {
			prevscope := g.symbols.current_scope_idx
			g.symbols.jump_to_scope(stmt.scope_idx)
			for s in stmt.stmts {
				if g.block_terminated {
					g.block_terminated = false
					break
				}
				g.gen_stmt(s, expected_t)
			}
			g.symbols.jump_to_scope(prevscope)
		}
		ExprStmt   {g.gen_expr(stmt.expr, expected_t)}
		FuncDecl   {g.gen_func(stmt)}
		else       { panic('unhandled') }
	}
}

fn (mut g QbeGen) gen_local_var(decl VarDecl) {
	value := g.gen_expr(decl.value, decl.type)
	newvar := g.new_tmp()+"_ptr"
	qbetype := g.twotype_to_qbetype(decl.type.str())
	size := g.qbetype_bytesize(qbetype)
	g.emit('${newvar} =l alloc${size} ${size}')
	g.vars_temp_values[decl.name] = GenVal{newvar, qbetype, true, decl.type.str()}
	g.emit('store${qbetype} ${value.val}, ${newvar}')
}

fn (mut g QbeGen) gen_return(ret ReturnStmt, expected_t TypeExpr) {
	var := g.gen_expr(ret.expr, expected_t)
	if var == voidgenval {
		g.emit('ret')
		return
	}
	g.emit('ret ${var.val}')
}

fn (mut g QbeGen) gen_if_chain(chain IfChain, expected_t TypeExpr) {

	mut needs_endif_lbl := false
	iflbl := g.new_label()+"_if"
	mut eliflbls := []string{}
	mut elifbodylbls := []string{}
	for _ in chain.elifs {
			l := g.new_label() + "_elif"
	    eliflbls << l      // condition check label
	    elifbodylbls << l+"_body"  // body label
	}
	elselbl := if chain.else != none { g.new_label()+"_else" } else { '' }
	endiflbl := g.new_label() + "_endif"

	guardval := g.gen_expr(chain.if.guard, expected_t)
	if eliflbls.len > 0{
		g.emit("jnz ${guardval.val}, ${iflbl}, ${eliflbls[0]}")
	} else if elselbl != "" {
		g.emit("jnz ${guardval.val}, ${iflbl}, ${elselbl}")
	} else {
		needs_endif_lbl = true
		g.emit("jnz ${guardval.val}, ${iflbl}, ${endiflbl}")
	}

	g.emit_label(iflbl)
	g.gen_stmt(chain.if.block, expected_t)
	if g.block_terminated {
		g.block_terminated = false
	} else {
		needs_endif_lbl = true
		g.emit("jmp ${endiflbl}")
	}

	mut i := 0
	for i < chain.elifs.len {
		elifguardval := g.gen_expr(chain.elifs[i].guard, expected_t)
		g.emit_label(eliflbls[i])
		if i < chain.elifs.len-1 {
			g.emit("jnz ${elifguardval.val}, ${elifbodylbls[i]}, ${eliflbls[i+1]}")
		} else if elselbl != "" {
			g.emit("jnz ${elifguardval.val}, ${elifbodylbls[i]}, ${elselbl}")
		} else {
			needs_endif_lbl = true
			g.emit("jnz ${elifguardval.val}, ${elifbodylbls[i]}, ${endiflbl}")
		}
		g.emit_label(elifbodylbls[i])
		g.gen_stmt(chain.elifs[i].block, expected_t)
		if g.block_terminated {
			g.block_terminated = false
		} else {
			g.emit("jmp ${endiflbl}")
		}
		i++
	}

	if chain.else != none {
		g.emit_label(elselbl)
		g.gen_stmt(chain.else.block, expected_t)
	}
	if needs_endif_lbl {
		g.emit_label(endiflbl)
	}
}

pub fn (mut g QbeGen) gen_program(ast []Stmt, table SymbolTable) {
	g.symbols= table
	for stmt in ast {
		g.gen_stmt(stmt, TypeExpr{})
	}
}
