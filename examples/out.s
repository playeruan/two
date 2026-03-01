.data
.balign 8
glb_1:
	.ascii "%s\n"
	.byte 0
/* end data */

.data
.balign 8
glb_2:
	.ascii "uan"
	.byte 0
/* end data */

.text
bar:
	pushq %rbp
	movq %rsp, %rbp
	leave
	ret
.type bar, @function
.size bar, .-bar
/* end function bar */

.text
foo:
	pushq %rbp
	movq %rsp, %rbp
	movq (%rdi), %rsi
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
	movq $3, -8(%rbp)
	leaq -16(%rbp), %rdi
	callq foo
	movl $2, %edi
	callq bar
	movl $0, %eax
	leave
	ret
.type main, @function
.size main, .-main
/* end function main */

.section .note.GNU-stack,"",@progbits
