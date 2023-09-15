package lpp

import "fmt"

type TokenType int

const (
	ASSIGN TokenType = iota
	COMMA
	DIF
	DIVISION
	ELSE
	ELSEIF
	EQ
	EOF
	FALSE
	FOR
	FUNCTION
	GT
	GTE
	IDENT
	IF
	ILLEGAL
	INT
	LBRACE
	LET
	LPAREN
	LT
	LTE
	MINUS
	MULTIPLICATION
	NEGATION
	NEQ
	PLUS
	RBRACE
	RPAREN
	RETURN
	SEMICOLON
	TRUE
)

type Token struct {
	TokenType TokenType
	Literal   string
}

func (t Token) String() string {
	return fmt.Sprintf("Type %v, Literal %v", t.TokenType, t.Literal)
}

var keywords = map[string]TokenType{
	"variable":  LET,
	"funcion":   FUNCTION,
	"para":      FOR,
	"regresa":   RETURN,
	"si":        IF,
	"si_no":     ELSE,
	"si_no_si":  ELSEIF,
	"verdadero": TRUE,
	"falso":     FALSE,
}

func LookupTokenType(literal string) TokenType {
	return keywords[literal]
}
