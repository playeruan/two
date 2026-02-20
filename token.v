module two

enum TokKind as u8 {
	key_let
	key_mut
	key_const
	key_class
	key_fn
	key_i8
	key_i16
	key_i32
	key_i64
	key_u8
	key_u16
	key_u32
	key_u64
	key_f32
	key_f64
	key_bool
	key_string
	key_true
	key_false
	key_addr
	key_deref
	integer_lit
	float_lit
	string_lit
	identifier
	eq
	lt
	gt
	eqeq
	lteq
	gteq
	plus
	minus
	star
	slash
	dot
	comma
	colon
	semicolon
	at
	increment
	decrement
	leftparen
	rightparen
	leftsquare
	rightsquare
	leftbrace
	rightbrace
  eof
}

enum Precedence as u8 {
	lowest
	assigment
	comparison
	sum
	product
	prefix
	suffix
	call
	access
}

struct Token {
  kind TokKind
  lit string
  line int
}

fn (ts []Token) str() string {
	mut s := ""
	for t in ts {
		s += t.str() + "\n"
	}
	return s
}

fn (t Token) str() string {
  return "${t.kind.str()} (${t.lit})"
}

fn (tk TokKind) get_prec() Precedence {
	return match tk {
		.eq {.assigment}
		.eqeq, .lteq, .gteq, .lt, .gt {.comparison}
		.plus, .minus {.sum}
		.star, .slash {.product}
		.dot {.access}
		.increment, .decrement {.suffix}
		else {.lowest}
	}
}

fn (tk TokKind) is_primitive_type() bool {
	return [
		TokKind.key_i8, .key_u8, .key_i16, .key_u16,
		.key_i32, .key_u32, .key_i64, .key_u64,
		.key_f32, .key_f64, .key_bool, .key_string
	].contains(tk)
}

fn get_kind_if_delimiter(s string) ?TokKind {
	return match s {
		"="  {.eq}
		"<"  {.lt}
		">"  {.gt}
		"==" {.eqeq}
		"<=" {.lteq}
		">=" {.gteq}
		"+"  {.plus}
		"-"  {.minus}
		"*"  {.star}
		"/"  {.slash}
		"."  {.dot}
		","  {.comma}
		"++" {.increment}
		"--" {.decrement}
		"("  {.leftparen}
		")"  {.rightparen}
		"["  {.leftsquare}
		"]"  {.rightsquare}
		"{"  {.leftbrace}
		"}"  {.rightbrace}
		":"  {.colon}
		";"  {.semicolon}
		"@"  {.at}
		else {none}
	}
}

fn get_kind_if_key(s string) ?TokKind {
	return match s {
		"mut"   {.key_mut}
 		"let"   {.key_let}
		"const" {.key_const}
		"class" {.key_class}
		"i8"    {.key_i8}
		"i16"   {.key_i16}
		"i32"   {.key_i32}
		"i64"   {.key_i64}
		"u8"    {.key_u8}
		"u16"   {.key_u16}
		"u32"   {.key_u32}
		"u64"   {.key_u64}
		"f32"   {.key_f32}
		"f64"   {.key_f64}
		"bool"  {.key_bool}
		"false" {.key_false}
		"true"  {.key_true}
		"string"{.key_string}
		"addr"  {.key_addr}
		"deref" {.key_deref}
		"fn"    {.key_fn}
		else  {none}
	}
}
