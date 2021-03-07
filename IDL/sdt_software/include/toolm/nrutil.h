/* nrutil.h */

#ifndef NRU_WAS_INCLUDED
#define NRU_WAS_INCLUDED

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* For SCCS */
#define SccsId_nrutil_h "@(#)nrutil.h	1.2, 08/18/97"

#ifdef ANSI_STD_C

extern float **NRUmatrix( int nrl, int nrh, int ncl, int nch);
   /* allocate a float matrix m[ nrl..nrh][ ncl..nch] */

extern void NRUfree_matrix( float **m, int nrl, int nrh, int ncl, int nch);
   /* free the float matrix m[ nrl..nrh][ ncl..nch] */

extern float *NRUvector(int nl, int nh);
   /* allocate a vector v[ nl..nh] */

extern void NRUfree_vector(float *v, int nl, int nh);
   /* deallocate a vector v[ nl..nh] */

extern double **NRUdmatrix( int nrl, int nrh, int ncl, int nch);
   /* allocate a double matrix m[ nrl..nrh][ ncl..nch] */

extern void NRUfree_dmatrix( double **m, int nrl, int nrh, int ncl, int nch);
   /* free the double matrix m[ nrl..nrh][ ncl..nch] */

extern double *NRUdvector(int nl, int nh);
   /* allocate a vector v[ nl..nh] */

extern void NRUfree_dvector(double *v, int nl, int nh);
   /* deallocate a vector v[ nl..nh] */

#else

extern float **NRUmatrix( );
extern float **NRUfree_matrix( );

extern float *NRUvector();
extern float *NRUfree_vector();

#endif

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* NRU_WAS_INCLUDED */
