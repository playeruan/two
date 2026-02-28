.data
.balign 8
glb_1:
	.ascii "Hello, %d!\n"
	.byte 0
/* end data */

.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	movl $12, %esi
	leaq glb_1(%rip), %rdi
	callq printf
	movl $0, %eax
	leave
	ret
.type main, @function
.size main, .-main
/* end function main */

.section .note.GNU-stack,"",@progbits
