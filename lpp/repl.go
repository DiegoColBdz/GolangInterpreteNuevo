package lpp

import (
	"bufio"
	"fmt"
	"os"
)

func StartREPL() {
	scanner := bufio.NewScanner(os.Stdin)

	for {
		fmt.Print(">> ")
		scanned := scanner.Scan()

		if !scanned {
			return
		}

		line := scanner.Text()

		if line == "salir()" {
			return
		}

		l := NewLexer(line)
		p := NewParser(l)
		program := p.ParseProgram()

		if len(p.Errors()) > 0 {
			printParserErrors(p.Errors())
			continue
		}

		fmt.Println(program.String())
	}
}

func printParserErrors(errors []string) {
	fmt.Println("Se encontraron errores de análisis:")
	for _, msg := range errors {
		fmt.Println("\t" + msg)
	}
}
