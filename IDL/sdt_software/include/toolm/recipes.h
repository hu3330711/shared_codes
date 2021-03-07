/* recipes.h */

#ifndef RECIPES_H
#define RECIPES_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* For SCCS */
#define SccsId_recipes_h "@(#)recipes.h	1.2, 08/18/97"

/* note : MAX_DIMENSION must be same as array dimensions for coordinate
	  transforms. */
#define  TINY             1.0e-20
#define  MAX_DIMENSION    3

#ifdef ANSI_STD_C
int ludcmp (float a[MAX_DIMENSION][MAX_DIMENSION], int n, int *indx,
    float *d) ;
void lubksb (float a[MAX_DIMENSION][MAX_DIMENSION], int n, int *indx,
    float *b)  ;
int invert_mtrx (float a[MAX_DIMENSION][MAX_DIMENSION], int n,
    float b[MAX_DIMENSION][MAX_DIMENSION]) ;
#else
int ludcmp () ;
void lubksb () ;
int invert_mtrx () ;
#endif

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* RECIPES_H */

