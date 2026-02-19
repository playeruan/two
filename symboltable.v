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
	block Block
}

struct ClassSymbol {
	name string
mut:
	members map[string]VarSymbol
	methods map[string]FuncSymbol
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
fn (mut t SymbolTable) define_func(name string, type TypeExpr, flags DeclFlags, block Block)
fn (mut t SymbolTable) define_class(name string)

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

fn (mut t SymbolTable) lookup_func(name string) ?FuncSymbol
fn (mut t SymbolTable) lookup_class(name string) ?ClassSymbol
