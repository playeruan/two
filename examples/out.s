.text
randint:
	pushq %rbp
	movq %rsp, %rbp
	imull $3, %edi, %eax
	leave
	ret
.type randint, @function
.size randint, .-randint
/* end function randint */

.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	movl $1, %edi
	callq randint
	leave
	ret
.type main, @function
.size main, .-main
/* end function main */

.section .note.GNU-stack,"",@progbits
