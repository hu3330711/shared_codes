/* lq.h */

#ifndef LQ_WAS_INCLUDED
#define LQ_WAS_INCLUDED

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* For SCCS */
#define SccsId_lq_h "@(#)lq.h	1.2, 08/18/97"

/* hidden type */
typedef char  *LQrec;

extern boolean LQinit( LQrec **lq);
   /* set lq = NUll the first tim, so that memory will be allocated.
      return FASLE if allocation fails
      calculation is initialized.
   */

extern void LQexit( LQrec *lq);
   /* free up the memory for lq when all done */

extern void LQaccum( LQrec *lq, float y, float x1, float x2);
   /* accumulate the x, y data.
      weights are assumed uniform */

extern boolean LQsolve( LQrec *lq, float *a, float *b);
/* solve for least squares fit y = a + bx.
   return FALSE if problem is ill-conditioned */

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* LQ_WAS_INCLUDED */
