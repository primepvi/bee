; ModuleID = 'main_module'
source_filename = "main_module"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"

@message = internal global ptr @str
@str = private unnamed_addr constant [15 x i8] c"\22Hello, World\22\00", align 1
@response = internal global ptr @str.1
@str.1 = private unnamed_addr constant [13 x i8] c"\22Hello, Bee\22\00", align 1
@age = internal global i32 44

define i32 @main() {
entry:
  %year = alloca i32, align 4
  store i32 93389, ptr %year, align 4
  %age = alloca i32, align 4
  store i32 72, ptr %age, align 4
  %other_age = alloca i32, align 4
  %age1 = load i32, ptr %age, align 4
  store i32 %age1, ptr %other_age, align 4
  store i32 44, ptr %age, align 4
  ret i32 0
}
