package main

import "fmt"

func plus(a int, b int) int {
	return a + b
}

func main() {
	fmt.Printf("%v", plus(1111111111111111111, plus(2222222222222222222, plus(2222222222222222222, plus(2222222222222222222, 2222222222222222222)))))
	fmt.Println("hello world")
}
