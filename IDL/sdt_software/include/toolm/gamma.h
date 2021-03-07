/* gamma.h */

#ifndef GAMMA_WAS_INCLUDED
#define GAMMA_WAS_INCLUDED

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* For SCCS */
#define SccsId_gamma_h "@(#)gamma.h	1.2, 08/18/97"

#ifdef ANSI_STD_C

extern float GAMMAq(float a, float x);
/* return the value of the incomplete gamma function
   Q(a, x) for x >= 0 and a > 0
   Q(a, x) = 1/gamma( a) * integral( exp(-t) t**(a-1) dt) from x to infinity
 */

extern float GAMMAchi(float chi, int df);
/* return the value of the chi squared function for df degrees of freedom
   this is the probability that the observed chi square will exceed chi even
   for a correct model.
 */

#else
extern float GAMMAchi();
extern float GAMMAq()
#endif

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* GAMMA_WAS_INCLUDED */

