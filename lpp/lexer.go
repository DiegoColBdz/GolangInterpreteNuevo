package lpp

import (
	"regexp"
)

type Lexer struct {
	source       string
	character    string
	position     int
	readPosition int
}

func NewLexer(source string) *Lexer {
	lexer := &Lexer{source: source}
	lexer.readCharacter()
	return lexer
}

func isLetter(character string) bool {
	return regexp.MustCompile(`^[a-zA-Z]$`).MatchString(character)
}

func isNumber(character string) bool {
	return regexp.MustCompile(`^\d$`).MatchString(character)
}

func (l *Lexer) NextToken() Token {
	l.skipWhitespace()

	var token Token

	switch l.character {
	case "=":
		if l.peekCharacter() == "=" {
			token = l.makeTwoCharacterToken(EQ)
		} else {
			token = Token{TokenType: ASSIGN, Literal: l.character}
		}
	case ">":
		if l.peekCharacter() == "=" {
			token = l.makeTwoCharacterToken(GTE)
		} else {
			token = Token{TokenType: GT, Literal: l.character}
		}
	case "+":
		token = Token{TokenType: PLUS, Literal: l.character}
	case "":
		token = Token{TokenType: EOF, Literal: l.character}
	default:
		if isLetter(l.character) {
			literal := l.readIdentifier()
			tokenType := LookupTokenType(literal)
			return Token{TokenType: tokenType, Literal: literal}
		} else if isNumber(l.character) {
			literal := l.readNumber()
			token = Token{TokenType: INT, Literal: literal}
		} else {
			token = Token{TokenType: ILLEGAL, Literal: l.character}
		}
	}

	l.readCharacter()
	return token
}

func (l *Lexer) makeTwoCharacterToken(tokenType TokenType) Token {
	prefix := l.character
	l.readCharacter()
	suffix := l.character

	return Token{TokenType: tokenType, Literal: prefix + suffix}
}

func (l *Lexer) peekCharacter() string {
	if l.readPosition >= len(l.source) {
		return ""
	}
	return string(l.source[l.readPosition])
}

func (l *Lexer) readCharacter() {
	if l.readPosition >= len(l.source) {
		l.character = ""
	} else {
		l.character = string(l.source[l.readPosition])
	}
	l.position = l.readPosition
	l.readPosition++
}

func (l *Lexer) readIdentifier() string {
	initialPosition := l.position
	for isLetter(l.character) {
		l.readCharacter()
	}
	return l.source[initialPosition:l.position]
}

func (l *Lexer) readNumber() string {
	initialPosition := l.position
	for isNumber(l.character) {
		l.readCharacter()
	}
	return l.source[initialPosition:l.position]
}

func (l *Lexer) skipWhitespace() {
	for regexp.MustCompile(`^\s$`).MatchString(l.character) {
		l.readCharacter()
	}
}
