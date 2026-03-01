.data
.balign 8
glb_1:
	.ascii "%d\n"
	.byte 0
/* end data */

.text
is_prime:
	pushq %rbp
	movq %rsp, %rbp
	movl $2, %ecx
.Lbb2:
	movl $2, %esi
	movl %edi, %eax
	cltd
	idivl %esi
	cmpl %eax, %ecx
	jae .Lbb6
	movl %edi, %eax
	cltd
	idivl %ecx
	movl %edx, %eax
	cmpl $0, %eax
	jz .Lbb5
	addl $1, %ecx
	jmp .Lbb2
.Lbb5:
	movl $0, %eax
	jmp .Lbb7
.Lbb6:
	movl $1, %eax
.Lbb7:
	leave
	ret
.type is_prime, @function
.size is_prime, .-is_prime
/* end function is_prime */

.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	movl $174440041, %edi
	callq is_prime
	movl %eax, %esi
	leaq glb_1(%rip), %rdi
	callq printf
	movl $0, %eax
	leave
	ret
.type main, @function
.size main, .-main
/* end function main */

.section .note.GNU-stack,"",@progbits
