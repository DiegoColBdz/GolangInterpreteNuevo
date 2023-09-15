package main

import (
	"fmt"

	"github.com/DiegoColBdz/GolangInterprete/lpp"
)

func imprimirBienvenida() {
	bienvenida := []string{
		"  ***     *   *   ***   *   *     *** ",
		" *   *   *   *  *   *  *   *    *   *",
		" *       *****  *      *****    *",
		" *       *   *  *      *   *     *",
		" *   *   *   *  *   *  *   *    *   *",
		"  ***    *   *   ***   *   *     *** ",
	}
	for _, linea := range bienvenida {
		fmt.Println(linea)
	}
}

func main() {
	imprimirBienvenida()
	lpp.StartREPL()
}
