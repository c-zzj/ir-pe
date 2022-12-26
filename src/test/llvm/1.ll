declare  void @putchar(i8 nocapture) nounwind
declare  void @puts(i8* nocapture) nounwind
declare  i8* @itoa(i32 nocapture) nounwind
declare  i32 @atoi(i8* nocapture) nounwind
declare  i8* @malloc(i32 nocapture) nounwind
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

define  i32 @main()  {
	%_7 = getelementptr { void (i32)*, i32 }, ptr null, i32 1
	%_8 = ptrtoint ptr %_7 to i32

	%_9 = call ptr @malloc(i32 %_8)
	%_10 = getelementptr ptr, ptr %_9, i32 0
	store void (i32)* @printi, ptr %_10
	%_11 = getelementptr ptr, ptr %_9, i32 1
	store i32 123, ptr %_11
	%_12 = getelementptr ptr, ptr %_9, i32 0
	%_13 = load void (i32)*, ptr %_12
	%_14 = getelementptr ptr, ptr %_9, i32 1
	%_15 = load i32, ptr %_14
	call void %_13(i32 %_15)
	ret i32 0
	unreachable
}

