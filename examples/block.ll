; ModuleID = 'main_module'
source_filename = "main_module"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"

@str = private unnamed_addr constant [15 x i8] c"\22Hello, World\22\00", align 1
@str.1 = private unnamed_addr constant [13 x i8] c"\22Hello, Bee\22\00", align 1

define i32 @main() {
entry:
  %message = alloca ptr, align 8
  store ptr @str, ptr %message, align 8
  %response = alloca ptr, align 8
  store ptr @str.1, ptr %response, align 8
  %age = alloca i32, align 4
  store i32 44, ptr %age, align 4
  %year = alloca i32, align 4
  store i32 93389, ptr %year, align 4
  %age1 = alloca i32, align 4
  store i32 72, ptr %age1, align 4
  %other_age = alloca i32, align 4
  %age2 = load i32, ptr %age1, align 4
  store i32 %age2, ptr %other_age, align 4
  store i32 44, ptr %age1, align 4
  ret i32 0
}
