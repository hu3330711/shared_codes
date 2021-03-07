/* eigen.h */

#ifndef EIGEN_WAS_INCLUDED
#define EIGEN_WAS_INCLUDED

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* For SCCS */
#define SccsId_eigen_h "@(#)eigen.h	1.2, 08/18/97"

#ifdef ANSI_STD_C

/* Find eigenvectors and eigenvalues of a real, symmetric matrix.
   mat[1..n][1..n] is input using numrec's float ** array representation.
   mat is replaced by the eigenvectors that were found,
   eval are the eigenvalues that were found,
   eval[k] corresponding to the kth column of mat[1..n][k]
   
   The returned eigenvectors are normalized to 1.0, and are sorted in order 
   of ascending eigenvalue.

   return FALSE if the routine failed.
 */

boolean  EIGENreal_sym( float **mat, int n, float *eval);


#else

boolean  EIGENreal_sym( );

#endif

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* EIGEN_WAS_INCLUDED */

