module two

type Symbol = VarSymbol | FuncSymbol | ClassSymbol

struct VarSymbol {
	name string
	flags DeclFlags
mut:
	type TypeExpr
}

struct FuncSymbol {
	name string
	type TypeExpr
	flags DeclFlags
	block ?Block
	args map[string]VarSymbol
mut:
	only_declared bool
}

struct ClassSymbol {
	name string
mut:
	members map[string]VarSymbol
	methods map[string]FuncSymbol
	only_declared bool
}

struct Scope {
	parent_idx int = -1
mut:
	vars map[string]VarSymbol
}

fn (s Scope) to_str(t SymbolTable) string {
	mut st := "{"
	for sym in s.vars.values() {
		st+=" ("+sym.type.str()+" "+sym.name+")"
	}
	st+=" }"
	if s.parent_idx != -1 {
		st+=" <- "+(t.scopes[s.parent_idx]).to_str(t)
	}
	return st
}

struct SymbolTable {
mut:
	scopes  []Scope
	current_scope_idx int = -1
	funcs   map[string]FuncSymbol
	classes map[string]ClassSymbol
}

fn (mut t SymbolTable) register_builtins() {
	t.classes['string'] = ClassSymbol {
		name: 'string'
		members: {
			'len': VarSymbol{name: 'len', type: TypeExpr{name: 'u64'}}
		}
	}

	t.classes['array'] = ClassSymbol {
    name: 'array'
    members: {
      'len': VarSymbol{name: 'len', type: TypeExpr{name: 'i32'}}
    }
    methods: {
      'append': FuncSymbol{
      	name: 'append'
				args: {
					'this': VarSymbol{name: 'this', type: TypeExpr{name: 'array', ptr_depth: 1, is_array: true}, flags: DeclFlags{mutable: true}}
					'what': VarSymbol{name: 'what', type: TypeExpr{name: '__generic__'}}
				}
				type: TypeExpr{
					is_fn: true
					arg_types: [
						TypeExpr{name: 'array', ptr_depth: 1, is_array: true},
						TypeExpr{name: '__generic__'}
					]
					ret_type: &TypeExpr{name: 'void'}
				}
    	}
			'pop': FuncSymbol {
				name: 'pop'
				args: {
					'this': VarSymbol{name: 'this', type: TypeExpr{name: 'array', ptr_depth: 1, is_array: true}, flags: DeclFlags{mutable: true}}
				}
				type: TypeExpr {
					is_fn: true
					arg_types: [TypeExpr{name: 'array', ptr_depth: 1, is_array: true}]
					ret_type: &TypeExpr{name: '__generic__'}
				}
			}
  	}
  }
}

fn (mut t SymbolTable) jump_to_scope(idx int) {
	unsafe {t.current_scope_idx = idx}
}

fn (mut t SymbolTable) enter_scope() {
	t.scopes << Scope{parent_idx: t.current_scope_idx}
	t.current_scope_idx = t.scopes.len-1
}

fn (mut t SymbolTable) exit_scope() {
	if t.current_scope_idx == -1 {panic("no scope to exit from")}
	else {
		t.current_scope_idx = t.scopes[t.current_scope_idx].parent_idx
	}
}

fn (mut t SymbolTable) define_var(name string, type TypeExpr, flags DeclFlags) {
	if t.scopes.len == 0 {panic("no scope available")}

	if t.lookup_var(name) != none {
		panic("variable redefinition")
	}

	t.scopes[t.current_scope_idx].vars[name] = VarSymbol {type: type, flags: flags, name: name}
}

fn (mut t SymbolTable) update_var_type(name string, type TypeExpr) {
	mut idx := t.current_scope_idx
	for idx != -1 {
		if name in t.scopes[idx].vars {
			unsafe { t.scopes[idx].vars[name].type = type }
			return
		}
		idx = t.scopes[idx].parent_idx
	}
}

fn (mut t SymbolTable) declare_func(name string, args[]ArgDecl) {

	mut arg_syms := map[string]VarSymbol{}
	for arg in args {
		arg_syms[arg.name] = VarSymbol {
			name: arg.name
			type: arg.type
			flags: arg.flags
		}
	}
	unsafe {
		t.funcs[name] = FuncSymbol {
			name: name
			args: arg_syms
			only_declared: true
		}
	}

  t.scopes[0].vars[name] = VarSymbol {
  	name: name
    type: TypeExpr {
					name: name,
					is_fn: true
				}
    flags: DeclFlags{}
  }
}

fn (mut t SymbolTable) define_func(name string, type TypeExpr, flags DeclFlags, block Block, args []ArgDecl) {
	unsafe {
		if name in t.funcs && !(t.funcs[name].only_declared){
			panic("Function redefinition for ${name}")
		}
	}

	mut arg_syms := map[string]VarSymbol{}
	for arg in args {
		arg_syms[arg.name] = VarSymbol {
			name: arg.name
			type: arg.type
			flags: arg.flags
		}
	}

	unsafe {
		t.funcs[name] = FuncSymbol {
			name: name
			type: type
			flags: flags
			block: block
			args: arg_syms
			only_declared: false
		}
	}

	mut f_arg_types := []TypeExpr{}
	for arg in args {
		f_arg_types << arg.type
	}

  t.scopes[0].vars[name] = VarSymbol {
  	name: name
    type: TypeExpr {
					name: name,
					ptr_depth: type.ptr_depth
					is_fn: true
					arg_types: f_arg_types,
					ret_type: &type
				}
    flags: flags
  }
}

fn (mut t SymbolTable) declare_method(class_name string, name string, args[]ArgDecl) {

	mut arg_syms := map[string]VarSymbol{}
	for arg in args {
		arg_syms[arg.name] = VarSymbol {
			name: arg.name
			type: arg.type
			flags: arg.flags
		}
	}
	unsafe {
		t.classes[class_name].methods[name] = FuncSymbol {
			name: name
			args: arg_syms
			only_declared: true
		}
	}
}

fn (mut t SymbolTable) define_method(class_name string, name string, type TypeExpr, flags DeclFlags, block Block, args []ArgDecl) {
	mut arg_syms := map[string]VarSymbol{}
  for arg in args {
    arg_syms[arg.name] = VarSymbol{name: arg.name, type: arg.type, flags: arg.flags}
  }

  mut f_arg_types := []TypeExpr{}
  for arg in args {
    f_arg_types << arg.type
  }

  func_sym := FuncSymbol{
    name: name
    type: TypeExpr{
    	is_fn: true
    	arg_types: f_arg_types
    	ret_type: &type
    }
    flags: flags
    block: block
    args: arg_syms
  }

  unsafe {
    t.classes[class_name].methods[name] = func_sym
  }
}

fn (mut t SymbolTable) declare_class(name string) {
	unsafe{
		t.classes[name] = ClassSymbol {
			name: name,
			only_declared: true
		}
	}
}

fn (mut t SymbolTable) define_class(name string, members []Member) {
	unsafe {
		if name in t.classes && !(t.classes[name].only_declared) {
			panic("Class ${name} is already defined")
		}
	}

	mut members_syms := map[string]VarSymbol{}
	for member in members {
		members_syms[member.name] = VarSymbol {
			name: member.name
			type: member.type,
			flags: member.flags,
		}
	}

	unsafe {
		t.classes[name] = ClassSymbol {
			name: name,
			members: members_syms,
			only_declared: false
		}
	}
}

fn (mut t SymbolTable) lookup_var(name string) ?VarSymbol {
	mut idx := t.current_scope_idx
	for idx != -1 {
		scope := t.scopes[idx]
		if name in scope.vars {
			unsafe {return scope.vars[name]}
		}
		idx = scope.parent_idx
	}
	return none
}

fn (mut t SymbolTable) lookup_func(name string) ?FuncSymbol {
	if name in t.funcs {
		unsafe{return t.funcs[name]}
	}
	return none
}

fn (mut t SymbolTable) lookup_class(name string) ?ClassSymbol {
	if name in t.classes {
		unsafe{return t.classes[name]}
	}
	return none
}
