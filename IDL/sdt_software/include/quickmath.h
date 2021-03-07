/*
 * some macros to speed up math computations
 */

#ifndef Quickmath_h
#define Quickmath_h

#define SCCSID_Quickmath_h "@(#)quickmath.h	1.1 12/09/94 UCB SSL"



#define SQUARE(x) ((x)*(x))
#define CUBE(x) ((x)*(x)*(x))
#define FOURTH(x) ((x)*(x)*(x)*(x))

#define MAX(A, B) (((A) > (B)) ? (A) : (B))
#define MIN(A, B) (((A) < (B)) ? (A) : (B))

#define ABS(x) (((x) < 0) ? (-(x)) : (x))

#endif
