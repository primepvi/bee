#ifndef BEE_TYPES_H
#define BEE_TYPES_H

// signed integers
typedef signed char i8;
typedef signed short i16;
typedef signed int i32;
typedef signed long long i64;

_Static_assert(sizeof(i8) == 1, "i8 must have 1 byte.");
_Static_assert(sizeof(i16) == 2, "i16 must have 2 bytes.");
_Static_assert(sizeof(i32) == 4, "i32 must have 4 bytes.");
_Static_assert(sizeof(i64) == 8, "i64 must have 8 bytes.");

// unsigned integers
typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;

_Static_assert(sizeof(u8) == 1, "u8 must have 1 byte.");
_Static_assert(sizeof(u16) == 2, "u16 must have 2 bytes.");
_Static_assert(sizeof(u32) == 4, "u32 must have 4 bytes.");
_Static_assert(sizeof(u64) == 8, "u64 must have 8 bytes.");

// floats
typedef float f32;
typedef double f64;
_Static_assert(sizeof(f32) == 4, "f32 must have 4 bytes.");
_Static_assert(sizeof(f64) == 8, "f64 must have 8 bytes.");

// booleans
#define false ((b8)0)
#define true ((b8)1)

typedef unsigned char b8;
typedef unsigned int b32;

_Static_assert(sizeof(b8) == 1, "b8 must have 1 byte.");
_Static_assert(sizeof(b32) == 4, "b32 must have 4 bytes.");

#endif // BEE_TYPES_H
