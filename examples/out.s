.text
test:
	pushq %rbp
	movq %rsp, %rbp
	imull $2, %edi, %eax
	leave
	ret
.type test, @function
.size test, .-test
/* end function test */

.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	movl $5, %edi
	callq test
	leave
	ret
.type main, @function
.size main, .-main
/* end function main */

.section .note.GNU-stack,"",@progbits
