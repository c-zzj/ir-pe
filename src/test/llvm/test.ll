; Copied directly from the documentation
; Declare the string constant as a global constant.
@.str = private unnamed_addr constant [13 x i8] c"hello world\0A\00"
@.str2 = global [13 x i8] c"hello world\0A\00"
@.int = global i8 100

; External declaration of the puts function
declare i32 @puts(i8* nocapture) nounwind
declare i32 @putchar(i8 nocapture) nounwind

; Definition of main function
define i32 @main () { ; i32()*
    ; Convert [13 x i8]* to i8  *...
    %cast210 = getelementptr [13 x i8],i8* @.str, i64 0, i64 0

    ;%ptr = alloca i32                               ; yields ptr
    ;store i32 3, ptr %ptr                           ; yields void
    ;%val = load i32, ptr %ptr                       ; yields i32:val = i32 3

    %agg1 = load [13 x i8], ptr %cast210
    %agg2 = insertvalue [13 x i8] %agg1, i8 65, 0                ; yields {i32 1, float undef}

    store [13 x i8] %agg2, ptr @.str2

    call i32 @puts(i8* @.str2)

    store i8 101, ptr @.int

    ; Call puts function to write out the string to stdout.
    call i32 @puts(i8* @.str)
    call void @goo()

    %Offset = getelementptr {i8,i32*}, ptr null, i32 0, i32 0
    %OffsetI = ptrtoint i32** %Offset to i8
    %x = add i8 %OffsetI, 48
    %foores = call i8 @foo()
    call i32 @putchar(i8 %foores)

    ret i32 0
thisisalabel:
    ret i32 0
}

define void @goo() {
    ret void
}

define i8 @foo () {
    %res = load i8, ptr @.int2
    ret i8 %res
}

@.int2 = global i8 103

; Named metadata
!0 = !{i32 42, null, !"string"}
!foo = !{!0}