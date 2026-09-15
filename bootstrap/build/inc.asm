	.file	"inc.ll"
	.text
	.globl	inc                             # -- Begin function inc
	.p2align	4
	.type	inc,@function
inc:                                    # @inc
	.cfi_startproc
# %bb.0:                                # %entry
	movq	%rsi, -8(%rsp)
	movq	$1, -16(%rsp)
	leaq	1(%rsi), %rax
	retq
.Lfunc_end0:
	.size	inc, .Lfunc_end0-inc
	.cfi_endproc
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
