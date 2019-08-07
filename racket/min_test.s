	.globl	_main                   ## -- Begin function main
_main:                                  ## @main
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, -4(%rbp)
	movl	$42, %eax
	popq	%rbp
	retq
