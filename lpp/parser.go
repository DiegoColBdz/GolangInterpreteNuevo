package lpp

import (
	"fmt"
	"strconv"
)

type Precedence int

const (
	LOWEST Precedence = iota + 1
	EQUALS
	LESSGREATER
	SUM
	PRODUCT
	PREFIX
	CALL
)

var precedences = map[TokenType]Precedence{
	EQ:             EQUALS,
	DIF:            EQUALS,
	LT:             LESSGREATER,
	GT:             LESSGREATER,
	PLUS:           SUM,
	MINUS:          SUM,
	DIVISION:       PRODUCT,
	MULTIPLICATION: PRODUCT,
	LPAREN:         CALL,
}

type PrefixParseFn func() Expression
type InfixParseFn func(left Expression) Expression

type Parser struct {
	lexer          *Lexer
	currentToken   Token
	peekToken      Token
	errors         []string
	prefixParseFns map[TokenType]PrefixParseFn
	infixParseFns  map[TokenType]InfixParseFn
}

func NewParser(lexer *Lexer) *Parser {
	parser := &Parser{
		lexer:          lexer,
		errors:         []string{},
		prefixParseFns: make(map[TokenType]PrefixParseFn),
		infixParseFns:  make(map[TokenType]InfixParseFn),
	}

	parser.registerPrefixFns()
	parser.registerInfixFns()

	parser.nextToken()
	parser.nextToken()

	return parser
}

func (p *Parser) ParseProgram() *Program {
	program := &Program{Statements: []Statement{}}

	for !p.currentTokenIs(EOF) {
		statement := p.parseStatement()
		if statement != nil {
			program.Statements = append(program.Statements, statement)
		}
		p.nextToken()
	}

	return program
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) registerPrefixFns() {
	p.prefixParseFns[IDENT] = p.parseIdentifier
	p.prefixParseFns[INT] = p.parseIntegerLiteral
	p.prefixParseFns[MINUS] = p.parsePrefixExpression
	p.prefixParseFns[NEGATION] = p.parsePrefixExpression
	p.prefixParseFns[TRUE] = p.parseBoolean
	p.prefixParseFns[FALSE] = p.parseBoolean
	p.prefixParseFns[LPAREN] = p.parseGroupedExpression
	p.prefixParseFns[IF] = p.parseIfExpression
	p.prefixParseFns[FUNCTION] = p.parseFunctionLiteral
}

func (p *Parser) registerInfixFns() {
	p.infixParseFns[PLUS] = p.parseInfixExpression
	p.infixParseFns[MINUS] = p.parseInfixExpression
	p.infixParseFns[DIVISION] = p.parseInfixExpression
	p.infixParseFns[MULTIPLICATION] = p.parseInfixExpression
	p.infixParseFns[EQ] = p.parseInfixExpression
	p.infixParseFns[DIF] = p.parseInfixExpression
	p.infixParseFns[LT] = p.parseInfixExpression
	p.infixParseFns[GT] = p.parseInfixExpression
	p.infixParseFns[LPAREN] = p.parseCallExpression
}

func (p *Parser) nextToken() {
	p.currentToken = p.peekToken
	p.peekToken = p.lexer.NextToken()
}

func (p *Parser) currentTokenIs(tokenType TokenType) bool {
	return p.currentToken.TokenType == tokenType
}

func (p *Parser) peekTokenIs(tokenType TokenType) bool {
	return p.peekToken.TokenType == tokenType
}

func (p *Parser) expectPeek(tokenType TokenType) bool {
	if p.peekTokenIs(tokenType) {
		p.nextToken()
		return true
	} else {
		p.peekError(tokenType)
		return false
	}
}

func (p *Parser) peekError(tokenType TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead", tokenType, p.peekToken.TokenType)
	p.errors = append(p.errors, msg)
}

func (p *Parser) parseStatement() Statement {
	switch p.currentToken.TokenType {
	case LET:
		return p.parseLetStatement()
	case RETURN:
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

func (p *Parser) parseLetStatement() *LetStatement {
	statement := &LetStatement{Token: p.currentToken}

	if !p.expectPeek(IDENT) {
		return nil
	}

	statement.Name = &Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	if !p.expectPeek(ASSIGN) {
		return nil
	}

	// TODO: Skip expressions until a semicolon is found
	for !p.currentTokenIs(SEMICOLON) {
		p.nextToken()
	}

	return statement
}

func (p *Parser) parseReturnStatement() *ReturnStatement {
	statement := &ReturnStatement{Token: p.currentToken}

	p.nextToken()

	// TODO: Skip expressions until a semicolon is found
	for !p.currentTokenIs(SEMICOLON) {
		p.nextToken()
	}

	return statement
}

func (p *Parser) parseExpressionStatement() *ExpressionStatement {
	statement := &ExpressionStatement{Token: p.currentToken}

	statement.Expression = p.parseExpression(LOWEST)

	if p.peekTokenIs(SEMICOLON) {
		p.nextToken()
	}

	return statement
}

func (p *Parser) parseExpression(precedence Precedence) Expression {
	prefix := p.prefixParseFns[p.currentToken.TokenType]
	if prefix == nil {
		p.noPrefixParseFnError(p.currentToken.TokenType)
		return nil
	}

	leftExp := prefix()

	for !p.peekTokenIs(SEMICOLON) && precedence < p.peekPrecedence() {
		infix := p.infixParseFns[p.peekToken.TokenType]
		if infix == nil {
			return leftExp
		}

		p.nextToken()

		leftExp = infix(leftExp)
	}

	return leftExp
}

func (p *Parser) parseIdentifier() Expression {
	return &Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
}

func (p *Parser) parseIntegerLiteral() Expression {
	literal := &IntegerLiteral{Token: p.currentToken}

	value, err := strconv.ParseInt(p.currentToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.currentToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}

	literal.Value = value
	return literal
}

func (p *Parser) parsePrefixExpression() Expression {
	expression := &PrefixExpression{
		Token:    p.currentToken,
		Operator: p.currentToken.Literal,
	}

	p.nextToken()

	expression.Right = p.parseExpression(PREFIX)

	return expression
}

func (p *Parser) parseInfixExpression(left Expression) Expression {
	expression := &InfixExpression{
		Token:    p.currentToken,
		Operator: p.currentToken.Literal,
		Left:     left,
	}

	//expression.Precedence = p.currentPrecedence()
	//p.nextToken()
	//expression.Right = p.parseExpression(expression.Precedence)

	precedence := p.currentPrecedence()
	p.nextToken()
	expression.Right = p.parseExpression(precedence) //bueno

	return expression
}

func (p *Parser) parseBoolean() Expression {
	return &Boolean{Token: p.currentToken, Value: p.currentTokenIs(TRUE)}
}

func (p *Parser) parseGroupedExpression() Expression {
	p.nextToken()

	expression := p.parseExpression(LOWEST)

	if !p.expectPeek(RPAREN) {
		return nil
	}

	return expression
}

func (p *Parser) parseIfExpression() Expression {
	expression := &IfExpression{Token: p.currentToken}

	if !p.expectPeek(LPAREN) {
		return nil
	}

	p.nextToken()
	expression.Condition = p.parseExpression(LOWEST)

	if !p.expectPeek(RPAREN) {
		return nil
	}

	if !p.expectPeek(LBRACE) {
		return nil
	}

	expression.Consequence = p.parseBlockStatement()

	if p.peekTokenIs(ELSE) {
		p.nextToken()

		if !p.expectPeek(LBRACE) {
			return nil
		}

		expression.Alternative = p.parseBlockStatement()
	}

	return expression
}

func (p *Parser) parseFunctionLiteral() Expression {
	lit := &FunctionLiteral{Token: p.currentToken}

	if !p.expectPeek(LPAREN) {
		return nil
	}

	lit.Parameters = p.parseFunctionParameters()

	if !p.expectPeek(LBRACE) {
		return nil
	}

	lit.Body = p.parseBlockStatement()

	return lit
}

func (p *Parser) parseFunctionParameters() []*Identifier {
	identifiers := []*Identifier{}

	if p.peekTokenIs(RPAREN) {
		p.nextToken()
		return identifiers
	}

	p.nextToken()

	ident := &Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
	identifiers = append(identifiers, ident)

	for p.peekTokenIs(COMMA) {
		p.nextToken()
		p.nextToken()
		ident := &Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
		identifiers = append(identifiers, ident)
	}

	if !p.expectPeek(RPAREN) {
		return nil
	}

	return identifiers
}

func (p *Parser) parseBlockStatement() *BlockStatement {
	block := &BlockStatement{Token: p.currentToken}
	block.Statements = []Statement{}

	p.nextToken()

	for !p.currentTokenIs(RBRACE) && !p.currentTokenIs(EOF) {
		statement := p.parseStatement()
		if statement != nil {
			block.Statements = append(block.Statements, statement)
		}
		p.nextToken()
	}

	return block
}

func (p *Parser) currentPrecedence() Precedence {
	if p, ok := precedences[p.currentToken.TokenType]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) peekPrecedence() Precedence {
	if p, ok := precedences[p.peekToken.TokenType]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) noPrefixParseFnError(t TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s found", t)
	p.errors = append(p.errors, msg)
	p.nextToken()
}

func (p *Parser) parseCallExpression(function Expression) Expression {
	exp := &CallExpression{Token: p.currentToken, Function: function}
	exp.Arguments = p.parseExpressionList(RPAREN)
	return exp
}

func (p *Parser) parseExpressionList(end TokenType) []Expression {
	list := []Expression{}

	if p.peekTokenIs(end) {
		p.nextToken()
		return list
	}

	p.nextToken()
	list = append(list, p.parseExpression(LOWEST))

	for p.peekTokenIs(COMMA) {
		p.nextToken()
		p.nextToken()
		list = append(list, p.parseExpression(LOWEST))
	}

	if !p.expectPeek(end) {
		return nil
	}

	return list
}
