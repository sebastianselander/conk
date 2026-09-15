
declare void @exit(i64)
declare i32 @printf(ptr, ...)
declare ptr @malloc(i64)

@snl = internal constant [3 x i8] c"%s\00"
define i1 @printString(ptr %env, i8* %x) {
    %t0 = getelementptr [3 x i8], [3 x i8]* @snl, i32 0, i32 0
	call i32 @printf(i8* %t0, i8* %x)
	ret i1 1
}


@cnl = internal constant [3 x i8] c"%c\00"
define i1 @printChar(ptr %env, i8 %x) {
    %t0 = getelementptr [3 x i8], [3 x i8]* @cnl, i32 0, i32 0
	call i32 @printf(i8* %t0, i8 %x)
	ret i1 1
}


define i1 @exit_success() {
    call void @exit(i64 0)
    ret i1 1
}


define i1 @exit_failure() {
    call void @exit(i64 1)
    ret i1 1
}


@dnl = internal constant [3 x i8] c"%d\00"
define i1 @printInt(ptr %env, i64 %x) {
    %t0 = getelementptr [3 x i8], [3 x i8]* @dnl, i32 0, i32 0
	call i32 @printf(i8* %t0, i64 %x)
	ret i1 1
}
