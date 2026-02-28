.data
.balign 8
glb_1:
	.ascii "%s %d\n"
	.byte 0
/* end data */

.data
.balign 8
glb_2:
	.ascii "Hello, World!"
	.byte 0
/* end data */

.text
foo:
	pushq %rbp
	movq %rsp, %rbp
	movq (%rdi), %rax
	movq (%rax), %rsi
	movq 8(%rax), %rdx
	leaq glb_1(%rip), %rdi
	callq printf
	leave
	ret
.type foo, @function
.size foo, .-foo
/* end function foo */

.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	leaq glb_2(%rip), %rax
	movq %rax, -16(%rbp)
	movq $13, -8(%rbp)
	leaq -16(%rbp), %rdi
	callq foo
	movl $0, %eax
	leave
	ret
.type main, @function
.size main, .-main
/* end function main */

.section .note.GNU-stack,"",@progbits
