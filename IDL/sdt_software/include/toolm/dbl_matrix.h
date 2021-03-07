/* dbl_matrix.h */

#ifndef DBL_MATRIX_WAS_INCLUDED
#define DBL_MATRIX_WAS_INCLUDED

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* For SCCS */
#define SccsId_dbl_matrix_h "@(#)dbl_matrix.h	1.2, 08/18/97"

#include <math.h>

#define  DBL_MAX_DIMENSION  20
#define  DBL_TINY           1.0e-20

/* ----------------------------------------------------------------- */
#ifdef ANSI_STD_C

int dludcmp (double a[DBL_MAX_DIMENSION][DBL_MAX_DIMENSION],
    int n, int *indx, double *d) ;

void dlubksb (double a[DBL_MAX_DIMENSION][DBL_MAX_DIMENSION],
    int n, int *indx, double *b) ; 

int dinvert_mtrx (double a[DBL_MAX_DIMENSION][DBL_MAX_DIMENSION],
    int n, double b[DBL_MAX_DIMENSION][DBL_MAX_DIMENSION]) ;

double **dmatrix_alloc (int nrows, int ncols) ;

void dmatrix_free (double **m, int nrows, int ncols) ;

#else

int dludcmp () ;
void dlubksb () ;
int dinvert_mtrx () ;
double **dmatrix_alloc () ;
void dmatrix_free () ;

#endif /* ANSI_STD_C */

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* DBL_MATRIX_WAS_INCLUDED */
