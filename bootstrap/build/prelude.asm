	.file	"prelude.ll"
	.text
	.globl	printString                     # -- Begin function printString
	.p2align	4
	.type	printString,@function
printString:                            # @printString
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$snl, %edi
	callq	printf@PLT
	movb	$1, %al
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	printString, .Lfunc_end0-printString
	.cfi_endproc
                                        # -- End function
	.globl	printChar                       # -- Begin function printChar
	.p2align	4
	.type	printChar,@function
printChar:                              # @printChar
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$cnl, %edi
	callq	printf@PLT
	movb	$1, %al
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	printChar, .Lfunc_end1-printChar
	.cfi_endproc
                                        # -- End function
	.globl	exit_success                    # -- Begin function exit_success
	.p2align	4
	.type	exit_success,@function
exit_success:                           # @exit_success
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	xorl	%edi, %edi
	callq	exit@PLT
	movb	$1, %al
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end2:
	.size	exit_success, .Lfunc_end2-exit_success
	.cfi_endproc
                                        # -- End function
	.globl	exit_failure                    # -- Begin function exit_failure
	.p2align	4
	.type	exit_failure,@function
exit_failure:                           # @exit_failure
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$1, %edi
	callq	exit@PLT
	movb	$1, %al
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end3:
	.size	exit_failure, .Lfunc_end3-exit_failure
	.cfi_endproc
                                        # -- End function
	.globl	printInt                        # -- Begin function printInt
	.p2align	4
	.type	printInt,@function
printInt:                               # @printInt
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$dnl, %edi
	callq	printf@PLT
	movb	$1, %al
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end4:
	.size	printInt, .Lfunc_end4-printInt
	.cfi_endproc
                                        # -- End function
	.type	snl,@object                     # @snl
	.section	.rodata,"a",@progbits
snl:
	.asciz	"%s"
	.size	snl, 3

	.type	cnl,@object                     # @cnl
cnl:
	.asciz	"%c"
	.size	cnl, 3

	.type	dnl,@object                     # @dnl
dnl:
	.asciz	"%d"
	.size	dnl, 3

	.section	".note.GNU-stack","",@progbits
