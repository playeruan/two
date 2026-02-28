.data
.balign 8
glb_1:
	.ascii "What is your name? "
	.byte 0
/* end data */

.data
.balign 8
glb_2:
	.ascii ""
	.byte 0
/* end data */

.data
.balign 8
glb_3:
	.ascii ""
	.byte 0
/* end data */

.data
.balign 8
glb_4:
	.ascii "%s"
	.byte 0
/* end data */

.data
.balign 8
glb_5:
	.ascii "Hello %s!\n"
	.byte 0
/* end data */

.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	leaq glb_2(%rip), %rsi
	leaq glb_1(%rip), %rdi
	callq printf
	leaq glb_3(%rip), %rsi
	leaq glb_4(%rip), %rdi
	callq scanf
	leaq glb_3(%rip), %rsi
	leaq glb_5(%rip), %rdi
	callq printf
	movl $0, %eax
	leave
	ret
.type main, @function
.size main, .-main
/* end function main */

.section .note.GNU-stack,"",@progbits
