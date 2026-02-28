module two

import strings

pub struct QbeGen {
pub mut:
	buf            strings.Builder
	data_buf       strings.Builder
	tmp_ctr        int
	lbl_ctr        int
	global_ctr     int
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

fn (mut g QbeGen) twotype_to_abi_type(t string) string {
    return match t {
        'i8', 'u8', 'i16', 'u16' { 'w' }
        else { g.twotype_to_qbetype(t) }
    }
}

fn (mut g QbeGen) twotype_to_qbetype(t string) string {
	if t.contains("@") {return 'l'}
	return match t {
		'i8', 'u8' {'b'}
		'i16', 'u16' {'h'}
		'i32', 'u32', 'bool' {'w'}
		'i64', 'u64', 'string' {'l'}
		'f32'  {'s'}
		'f64'  {'d'}
		'void' {''}
		else         {panic('unhandled type "${t}"')}
	}
}



fn (mut g QbeGen) qbetype_bytesize(t string) u8 {
	return match t {
		'b' {1}
		'h' {2}
		'w', 's'     {4}
		'l', 'd'     {8}
		else         {panic('unhandled')}
	}
}

fn (mut g QbeGen) load_instr(two_typ string) string {
	if two_typ.contains("@") {return 'loadl'}
  return match two_typ {
      'i8'  { 'loadsb' }
      'u8'  { 'loadub' }
      'i16' { 'loadsh' }
      'u16' { 'loaduh' }
      'i32', 'u32', 'bool' { 'loadw' }
      'i64', 'u64', 'string' { 'loadl' }
      'f32' { 'loads' }
      'f64' { 'loadd' }
      else  { panic('unhandled type ${two_typ}') }
  }
}

fn (mut g QbeGen) coerce(val GenVal, to TypeExpr) GenVal {
	if val.two_typ.starts_with('@') || to.name.starts_with('@') {
		return val
	}
	if twotype_bytesize(val.two_typ) < twotype_bytesize(to.name) {
		if is_twotype_float(val.two_typ) != is_twotype_float(to.name) {
			panic("Cannot implicitly cast value from ${val.two_typ} to ${to.name}!")
		}
		new := g.new_tmp()
		to_qbe := g.twotype_to_abi_type(to.name)
		if val.typ == to_qbe {
			// already the same QBE type, no instruction needed
			return GenVal{val.val, to_qbe, false, to.name}
		}
		if !is_twotype_float(to.name) {
			sign := if is_twotype_signed(val.two_typ) { 's' } else { 'u' }
			g.emit('${new} =${to_qbe} ext${sign}${val.typ} ${val.val}')
		} else {
			g.emit('${new} =${to_qbe} exts ${val.val}')
		}
		return GenVal{new, to_qbe, false, to.name}
	}
	return val
}

fn (mut g QbeGen) instr_from_op(op string, src_type string) string {
	is_float    := src_type in ['f32', 'f64']
	is_unsigned := !is_float && !is_twotype_signed(src_type)
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

fn (mut g QbeGen) new_global() string {
	g.global_ctr++
	return '\$glb_${g.global_ctr}'
}

fn (mut g QbeGen) emit(line string) {
	g.buf.write_string('\t${line}\n')
}

fn (mut g QbeGen) emit_label(line string) {
	g.buf.write_string('${line}\n')
}

fn (mut g QbeGen) emit_global(line string) {
	g.data_buf.write_string("${line}\n")
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
		StringLiteral {
			glb := g.new_global()
			g.emit_global("data ${glb} = { b \"${expr.val}\", b 0 }")
			// the 0 terminator is for compatibility with C functions like printf
			// might remove later on idk yet

			if expected_t.ptr_depth > 0 && expected_t.name == 'i8' {
				// used directly as @i8, return raw pointer
				return GenVal{glb, 'l', false, '@i8'}
			}

			// 16 bytes {data: l, len: l}
			struct_ptr := g.new_tmp()
			g.emit('${struct_ptr} =l alloc8 16')

			data_field := g.new_tmp()
			g.emit('${data_field} =l add ${struct_ptr}, 0')
			g.emit('storel ${glb}, ${data_field}')

			// len at offset 8
			len_field := g.new_tmp()
			g.emit('${len_field} =l add ${struct_ptr}, 8')
			g.emit('storel ${expr.val.len}, ${len_field}')

			GenVal{struct_ptr, 'l', false, 'string'}
		}
		VarExpr        {
			t := g.vars_temp_values[expr.name]
			if t.is_addr {
				tmp := g.new_tmp()
				if t.two_typ == 'string' {
					// load data pointer from offset 0 of string struct
					// so the user doesn't have to do s.data because
					// it's ugly asf :D
					data_field := g.new_tmp()
					g.emit('${data_field} =l add ${t.val}, 0')
					g.emit('${tmp} =l loadl ${data_field}')
					return GenVal{tmp, 'l', false, '@i8'}
				}
				abi_typ := g.twotype_to_abi_type(t.two_typ)
				g.emit("${tmp} =${abi_typ} ${g.load_instr(t.two_typ)} ${t.val}")
				GenVal{tmp, t.typ, true, t.two_typ}
			} else {
				t
			}
		}
		RefExpr {
			name := match expr.expr {
				VarExpr {expr.expr.name}
				else    {panic("RefExpr can only take address of a variable")}
			}
			slot := g.vars_temp_values[name]
			GenVal{slot.val, 'l', true, "@"+slot.two_typ}
		}
		DerefExpr {
			ex := g.gen_expr(expr.expr, expected_t)
			tmp := g.new_tmp()
			new_twotype := ex.two_typ.replace_once('@', '')
			abi_typ := g.twotype_to_abi_type(new_twotype)
			g.emit('${tmp} =${abi_typ} ${g.load_instr(new_twotype)} ${ex.val}')
			GenVal{tmp, abi_typ, new_twotype.contains('@'), new_twotype}
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
				abi_typ := g.twotype_to_abi_type(slot.two_typ)
				cur := g.new_tmp()
				g.emit('${cur} =${abi_typ} ${g.load_instr(slot.two_typ)} ${slot.val}')
				result := g.new_tmp()
				op := if expr.op == '++' { 'add' } else { 'sub' }
				g.emit('${result} =${abi_typ} ${op} ${cur}, 1')
				g.emit('store${slot.typ} ${result}, ${slot.val}')
				GenVal{cur, abi_typ, false, slot.two_typ}

			} else {
				panic('unexpected prefix unary operator')
			}
		}
		BinaryExpr {
			if ['=', '+=', '-=', '*=', '/='].contains(expr.op) {
				_ := "se non metto questa riga entra dentro a questo if branch anche se\
							la condizione sopra risulta falsa. prova a togliere questa riga e compilare
							un programma che fa qualcosa del tipo \"return f32(25) + 1.5\""
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
				abi_typ := g.twotype_to_abi_type(lhs.two_typ)
				g.emit('${cur} =${abi_typ} load${lhs.typ} ${lhs.val}')
				result := g.new_tmp()
				rhs := g.gen_expr(expr.right, expected_t)
				base_op := expr.op.replace('=', '')
				g.emit('${result} =${abi_typ} ${g.instr_from_op(base_op, lhs.two_typ)} ${cur}, ${rhs.val}')
				g.emit('store${lhs.typ} ${result}, ${lhs.val}')
				GenVal{result, abi_typ, false, lhs.two_typ}
			} else {
				l := g.coerce(g.gen_expr(expr.left, expected_t), expected_t)
				r := g.coerce(g.gen_expr(expr.right, expected_t), expected_t)
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
			callee_name := match expr.callee {
				VarExpr {expr.callee.name}
				else   {panic('unhandled callee')}
			}
			sym := g.symbols.lookup_func(callee_name) or {
				panic('undefined function ${callee_name}')
			}
			param_types := sym.args.values().map(it.type)
			for i, arg in expr.args {
				pt := if i < param_types.len { param_types[i] } else { expected_t }
				arg_vals << g.gen_expr(arg, pt)
			}
			tmp := g.new_tmp()
    	ret_type := sym.type
    	ret_qbe  := if ret_type.name == 'void' { '' } else { g.twotype_to_abi_type(ret_type.str()) }
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
		CastExpr {
			src := g.gen_expr(expr.expr, expected_t)
			new := g.new_tmp()
			to_name := expr.to.str()
			from_name := src.two_typ
			to_qbe := g.twotype_to_abi_type(to_name)

			is_src_float := from_name in ['f32', 'f64']
			is_dst_float := to_name in ['f32', 'f64']

			if !is_src_float && !is_dst_float {
				if twotype_bytesize(to_name) == twotype_bytesize(from_name) {
					return GenVal{src.val, to_qbe, false, to_name}
				}
				// int → int
				if twotype_bytesize(from_name) < twotype_bytesize(to_name) {
					// widening
					sign := if is_twotype_signed(from_name) { 's' } else { 'u' }
					g.emit('${new} =${to_qbe} ext${sign}${src.typ} ${src.val}')
				} else {
				// narrowing or same size — reinterpret
				return GenVal{src.val, to_qbe, false, to_name}
				}
			} else if !is_src_float && is_dst_float {
				// int → float
				sign := if is_twotype_signed(from_name) { 's' } else { 'u' }
				size := if from_name in ['i64', 'u64'] { 'l' } else { 'w' }
				g.emit('${new} =${to_qbe} ${sign}${size}tof ${src.val}')
			} else if is_src_float && !is_dst_float {
				// float → int
				sign := if is_twotype_signed(to_name) { 's' } else { 'u' }
				src_prefix := if from_name == 'f32' { 's' } else { 'd' }
				g.emit('${new} =${to_qbe} ${src_prefix}to${sign}i ${src.val}')
			} else {
				// float → float
				if from_name == 'f32' && to_name == 'f64' {
					g.emit('${new} =d exts ${src.val}')
				} else if from_name == 'f64' && to_name == 'f32' {
				g.emit('${new} =s truncd ${src.val}')
				} else {
					return GenVal{src.val, to_qbe, false, to_name}
				}
			}
			GenVal{new, to_qbe, false, to_name}
		}
		AccessExpr {
			slot := match expr.left {
				VarExpr {g.vars_temp_values[expr.left.name]}
				else {panic("access on non-variable not yet supported")}
			}
			left_t := slot.two_typ
			class_sym := g.symbols.lookup_class(left_t) or {
				panic("cannot access member of non-class type ${left_t}")
			}
			mut offset := 0
			for name in class_sym.member_order {
				if name == expr.member_name {break}
				unsafe {offset += twotype_bytesize(class_sym.members[name].type.str())}
			}

			mut genval := GenVal{}
			unsafe {
				member_type := class_sym.members[expr.member_name].type
				member_two_typ := member_type.name
				member_abi := g.twotype_to_abi_type(member_two_typ)

				field_ptr := g.new_tmp()
				tmp := g.new_tmp()
				g.emit('${field_ptr} =l add ${slot.val}, $offset')
				g.emit('$tmp =$member_abi ${g.load_instr(member_two_typ)} $field_ptr')
				genval = GenVal{tmp, member_abi, false, member_two_typ}
			}
			genval
		}
		else {panic('unhandled')}
	}
}

fn (mut g QbeGen) gen_func(decl FuncDecl) {
	if decl.flags.extern {
		return
	}
	g.vars_temp_values.clear()
	mut has_returned := false
	mut ret_t := g.twotype_to_abi_type(decl.type.str())
	if ret_t != "" {
		ret_t += " "
	}
	g.buf.write_string('${"export ".repeat(int(decl.flags.export))}function ${ret_t}\$${decl.name}(')
	for arg in decl.args {
		arg_t := g.twotype_to_abi_type(arg.type.str())
		g.buf.write_string('${arg_t} %${arg.name}_arg')
		if arg != decl.args[decl.args.len - 1] {
			g.buf.write_string(', ')
		}
    g.vars_temp_values[arg.name] = GenVal{'%${arg.name}_arg', arg_t, false, arg.type.str()}
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
	value := g.coerce(g.gen_expr(decl.value, decl.type), decl.type)
	if decl.type.name == 'string' {
		// value.val is already a pointer to the 16-byte struct
		g.vars_temp_values[decl.name] = GenVal{value.val, 'l', true, 'string'}
		return
	}
	newvar := g.new_tmp()+"_"+decl.name+"_ptr"
	qbetype := g.twotype_to_qbetype(decl.type.str())
	size := g.qbetype_bytesize(qbetype)
	g.emit('${newvar} =l alloc${size} ${size}')
	g.vars_temp_values[decl.name] = GenVal{newvar, qbetype, true, decl.type.str()}
	g.emit('store${qbetype} ${value.val}, ${newvar}')
}

fn (mut g QbeGen) gen_return(ret ReturnStmt, expected_t TypeExpr) {
	var := g.coerce(g.gen_expr(ret.expr, expected_t), expected_t)
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
	final := g.data_buf.str() + g.buf.str()
	g.buf = strings.new_builder(final.len)
	g.buf.write_string(final)
}
