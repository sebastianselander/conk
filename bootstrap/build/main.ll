declare i1 @printInt(ptr, i8)

declare i1 @exit_failure()

declare i1 @exit_success()

declare i1 @printChar(ptr, i8*)

declare i1 @printString(ptr, i8*)

declare ptr @malloc(i64)

declare i32 @printf(ptr, ...)

declare void @exit(i64)

define i1 @main() {
entry:
    ; Let expression
    %named_0 = alloca i64
    ; Lit expression
    store i64 68, i64* %named_0
    ; Let expression
    %named_1 = alloca i64
    ; App expression
    ; Var expression
    ; Lit expression
    ; Var expression
    %_0 = load i64, i64* %named_0
    %_1 = call i64 @inc(ptr null, i64 %_0)
    store i64 %_1, i64* %named_1
    ; Let expression
    %named_2 = alloca i1
    ; App expression
    ; Var expression
    ; Lit expression
    ; Var expression
    %_2 = load i64, i64* %named_1
    %_3 = call i1 @printInt(ptr null, i64 %_2)
    store i1 %_3, i1* %named_2
    ret i1 0
}