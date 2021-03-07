/* poly_fit.h */

#ifndef POLY_FIT_WAS_INCLUDED
#define POLY_FIT_WAS_INCLUDED

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* For SCCS */
#define SccsId_poly_fit_h "@(#)poly_fit.h	1.5, 08/18/97"

#include "dbl_matrix.h"

#ifdef ANSI_STD_C

extern double *poly_fit_dbl_dbl (double *ix, double *iy, int npts,
    int ndegree, double *yfit, double *yband, double *sigma,
    double aret[DBL_MAX_DIMENSION][DBL_MAX_DIMENSION]) ;

extern double *poly_fit_dbl_flt (double *ix, float *iy, int npts,
    int ndegree, double *yfit, double *yband, double *sigma,
    double aret[DBL_MAX_DIMENSION][DBL_MAX_DIMENSION]) ;

extern double *poly_fit_flt_dbl (float *ix, double *iy, int npts,
    int ndegree, double *yfit, double *yband, double *sigma,
    double aret[DBL_MAX_DIMENSION][DBL_MAX_DIMENSION]) ;

extern double *poly_fit_flt_flt (float *ix, float *iy, int npts,
    int ndegree, double *yfit, double *yband, double *sigma,
    double aret[DBL_MAX_DIMENSION][DBL_MAX_DIMENSION]) ;

#else

extern double *poly_fit_dbl_dbl () ;
extern double *poly_fit_dbl_flt () ;
extern double *poly_fit_flt_dbl () ;
extern double *poly_fit_flt_flt () ;

#endif

/* -------------------------------------------------------------------- */

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* POLY_FIT_WAS_INCLUDED */
