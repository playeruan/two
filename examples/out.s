.data
.balign 8
glb_1:
	.int 174440041
/* end data */

.data
.balign 8
glb_2:
	.ascii "%d\n"
	.byte 0
/* end data */

.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	movl glb_1(%rip), %esi
	leaq glb_2(%rip), %rdi
	callq printf
	movl $0, %eax
	leave
	ret
.type main, @function
.size main, .-main
/* end function main */

.section .note.GNU-stack,"",@progbits
