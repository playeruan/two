module two

pub struct Lexer {
mut:
	source string
	pos int
	line int
	tokens []Token
	is_in_num bool
	is_in_string bool
	is_in_comment bool
}

fn (l Lexer) tok(k TokKind, lit string) Token {
	return Token{k, lit, l.line}
}

@[noreturn]
fn (l Lexer) error(s string) {
	panic("Error: $s")
}

fn (mut l Lexer) peek() u8 {
	return l.source[l.pos]
}

fn (mut l Lexer) peek_next() u8 {
	return l.source[l.pos+1]
}

fn (mut l Lexer) advance() u8 {
	c := l.source[l.pos]
	l.pos++
	return c
}

fn (mut l Lexer) skip_whitespace() {
	for !l.is_at_end() && l.peek().is_space() {
		if l.peek() == `\n` {
			l.line++
		}
		l.advance()
	}
}

fn (mut l Lexer) is_at_end() bool {
	return l.pos >= l.source.len-1 || l.source[l.pos].ascii_str() == '\0'
}

fn (mut l Lexer) lex_string() Token {
	l.advance() // assume current is "
	start := l.pos
	for l.peek() != `"` && !l.is_at_end() {
		if l.peek() == `\n` { l.line++ }
		// escapes such as \"
		if l.peek() == `\\` && l.peek_next() == `"` {
			l.advance()
		}
		l.advance()
	}

	if l.is_at_end() { l.error('Unterminated string') }

	lit := l.source[start..l.pos]
	l.advance() // skip closing "
	return l.tok(.string_lit, lit)
}

fn (mut l Lexer) lex_number() Token {
	start := l.pos
	mut seen_point := false
	for !l.is_at_end() &&
			(l.peek().is_digit() || (!seen_point && l.peek() == `.`)) {
		if l.peek() == `.` {
			seen_point = true
		}
		l.advance()
	}
	return match seen_point {
		false {l.tok(.integer_lit, l.source[start..l.pos])}
		true  {l.tok(.float_lit, l.source[start..l.pos])}
	}
}

fn (mut l Lexer) lex_ident() Token {
	start := l.pos
	for !l.is_at_end() && l.peek().is_alnum() {
		l.advance()
	}

	lit := l.source[start..l.pos]
	kind := get_kind_if_key(lit) or {TokKind.identifier}
	return l.tok(kind, lit)
}

const delimiters := ",;.:+-*/%#()[]{}<>=|&^|@\n\" "
fn (mut l Lexer) lex_delimiter() Token {
	start := l.pos
	l.advance()
	for !l.is_at_end() && get_kind_if_delimiter(l.source[start..l.pos+1]) != none {
		l.advance()
	}

	lit := l.source[start..l.pos]
	kind := get_kind_if_delimiter(lit) or {l.error("invalid token ${lit}")}

	return l.tok(kind, lit)
}

fn (mut l Lexer) next_tok() Token {
	l.skip_whitespace()

	if l.is_at_end() {
		return l.tok(.eof, "")
	}

	c := l.peek()

	if c == `"` {
		return l.lex_string()
	}

	if c.is_digit() {
		return l.lex_number()
	}

	if c.is_letter() || c == `_` {
		return l.lex_ident()
	}

	if delimiters.contains(c.ascii_str()) {
		return l.lex_delimiter()
	}

	return l.tok(.eof, "invalid")

}


pub fn (mut l Lexer) lex_input(input string) []Token {
	l.source = input
	l.line = 1
	mut ts := []Token{}
	mut t := Token{}
	for t.kind != .eof {
		t = l.next_tok()
		ts << t
	}
	return ts
}
