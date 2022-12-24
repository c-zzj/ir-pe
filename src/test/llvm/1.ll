declare  void @putchar(i8 nocapture) nounwind
declare  void @puts(i8* nocapture) nounwind
declare  i8* @itoa(i32 nocapture) nounwind
declare  i32 @atoi(i8* nocapture) nounwind
define  void @printi(i32 %_1)  {
	%_2 = icmp sgt i32 %_1, 9
	br i1 %_2, label %LABEL_0, label %LABEL_1
	LABEL_0:
	%_3 = sdiv i32 %_1, 10
	call void @printi(i32 %_3)
	br label %LABEL_2
	LABEL_1:
	br label %LABEL_2
	LABEL_2:
	%_4 = srem i32 %_1, 10
	%_5 = trunc i32 %_4 to i8
	%_6 = add i8 %_5, 48
	call void @putchar(i8 %_6)
	ret void
	unreachable
}

define  i32 @fibonacci(i32 %_7)  {
	%_8 = icmp eq i32 %_7, 1
	%_9 = icmp eq i32 %_7, 2
	%_10 = or i1 %_8, %_9
	br i1 %_10, label %LABEL_3, label %LABEL_4
	LABEL_3:
	ret i32 1
	br label %LABEL_5
	LABEL_4:
	%_11 = sub i32 %_7, 1
	%_12 = call i32 @fibonacci(i32 %_11)
	%_13 = sub i32 %_7, 2
	%_14 = call i32 @fibonacci(i32 %_13)
	%_15 = add i32 %_12, %_14
	ret i32 %_15
	br label %LABEL_5
	LABEL_5:
	unreachable
}

define  i32 @main()  {
	%_16 = call i32 @fibonacci(i32 11)
	call void @printi(i32 %_16)
	ret i32 0
	unreachable
}

