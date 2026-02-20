module two

type Symbol = VarSymbol | FuncSymbol | ClassSymbol

struct VarSymbol {
	name string
	type TypeExpr
	flags DeclFlags
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
	parent ?&Scope
mut:
	vars map[string]VarSymbol
}

fn (s Scope) str() string {
	mut st := "{"
	for sym in s.vars.values() {
		st+=" ("+sym.type.str()+" "+sym.name+")"
	}
	st+=" }"
	if s.parent != none {
		st+=" <- "+(*s.parent).str()
	}
	return st
}

struct SymbolTable {
mut:
	scopes  []Scope
	current_scope ?&Scope
	funcs   map[string]FuncSymbol
	classes map[string]ClassSymbol
}

fn (mut t SymbolTable) jump_to_scope(scope &Scope) {
	unsafe {t.current_scope = scope}
}

fn (mut t SymbolTable) enter_scope() {
	t.scopes << Scope{parent: t.current_scope}
	t.current_scope = &t.scopes[t.scopes.len-1]
}

fn (mut t SymbolTable) exit_scope() {
	if t.current_scope == none {panic("no scope to exit from")}
	else {
		t.current_scope = t.current_scope.parent
	}
}

fn (mut t SymbolTable) define_var(name string, type TypeExpr, flags DeclFlags) {
	if t.scopes.len == 0 {panic("no scope available")}

	if t.lookup_var(name) != none {
		panic("variable redefinition")
	}

	scope := t.current_scope or {panic("nonexistant scope")}
	(*scope).vars[name] = VarSymbol {type: type, flags: flags, name: name}
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
	if t.scopes.len == 0 {panic("no scope available")}
	mut scope := *t.current_scope or {panic("nonexistant scope")}
	for true {
		if name in scope.vars {return scope.vars[name]}
		if scope.parent != none {
			scope = *scope.parent
		} else {
			break
		}
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
