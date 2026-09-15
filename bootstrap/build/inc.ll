declare i1 @printInt(ptr, i8)

declare i1 @exit_failure()

declare i1 @exit_success()

declare i1 @printChar(ptr, i8*)

declare i1 @printString(ptr, i8*)

declare ptr @malloc(i64)

declare i32 @printf(ptr, ...)

declare void @exit(i64)

define i64 @inc(ptr %env, i64 %x$1.arg) {
entry:
    %x$1 = alloca i64
    store i64 %x$1.arg, i64* %x$1
    ; Let expression
    %named_0 = alloca i64
    ; Lit expression
    store i64 1, i64* %named_0
    ; Return expression
    ; BinOp expression
    ; Var expression
    %_0 = load i64, i64* %x$1
    ; Var expression
    %_1 = load i64, i64* %named_0
    %_2 = add i64 %_0, %_1
    ret i64 %_2
}