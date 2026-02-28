.data
.balign 8
glb_1:
	.ascii "uan"
	.byte 0
/* end data */

.data
.balign 8
glb_2:
	.ascii "Hello, %s!\n"
	.byte 0
/* end data */

.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	leaq glb_1(%rip), %rsi
	leaq glb_2(%rip), %rdi
	callq printf
	movl $3, %eax
	leave
	ret
.type main, @function
.size main, .-main
/* end function main */

.section .note.GNU-stack,"",@progbits
