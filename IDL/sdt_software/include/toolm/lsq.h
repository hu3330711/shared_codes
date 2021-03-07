/* lsq.h */

#ifndef LSQ_WAS_INCLUDED
#define LSQ_WAS_INCLUDED

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* For SCCS */
#define SccsId_lsq_h "@(#)lsq.h	1.2, 08/18/97"

#ifdef ANSI_STD_C

extern void LSQfit(float *x, float *y, float *sig, int ndata, float *a,
   int ma, float *chisq, void (*funcs)(float, float *, void *), 
   void *ptr, float **covar);

/* Singular Value Decomposition least squares fit
   given a set of points x[1..ndata] and y[1..ndata], and standard deviations
   sig[1..ndata], use chi squared minimization to select the coefficients
   a[1..ma] of y = Sum( ai * funcs( xi) from 1 to ma.
   The value of chi squared is also returned. The degrees of freedom is
   ndata - ma - 1.

   you may also obtain the covariance matrix of the fitted parameters by
   passing in a non-NULL pointer to covar[1..ma][a..ma], in num.rec's
   float ** array format.
 */

#else

extern void LSQfit();

#endif

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* LSQ_WAS_INCLUDED */
