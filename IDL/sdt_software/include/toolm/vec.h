/* vec.h */

#ifndef VEC_WAS_INCLUDED
#define VEC_WAS_INCLUDED

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* For SCCS */
#define SccsId_vec_h "@(#)vec.h	1.2, 08/18/97"

typedef struct VECtor_
   {  float x;
      float y;
      float z;
   } VECtor;

typedef float VECarray[3][3]; /* index: a[i][j]  i=col, j=row. Thus an array
                                 of 3 vectors VECtor a[3] are column vectors
                               */

extern void VECangle ( float *result, VECtor *v1, VECtor *v2);
/* calculate angle between two vectors = acos( (v1 dot v2) / |v1||v2|) */


extern void VECcross ( VECtor *result, VECtor *v1, VECtor *v2);
/* return the cross product of two vectors; 
   the result vector must be different strorage from the two inputs */

extern  float VECdot( VECtor *v1, VECtor *v2);
/* return the dot product of two vectors */

extern  float VECmag( VECtor *v1);
/* return the magnitude of a vector */

extern  VECtor *VECmultArray( VECtor *result, float (*a)[3], VECtor *v1);
/* right multiply a vector by an array: result = a * v1
   the array must be indexed by a(col, row)
   return pointer to result */

extern  VECtor *VECmultScalar( VECtor *result, VECtor *v1, float f);
/* multiply a vector by a scalar: result = f * v1
   return pointer to result */

extern  float VECnormalize( VECtor *result, VECtor *v1);
/* normalize v1 to be a unit vector: result = v1 / |v1|
   return |v1| */

extern  VECtor *VECsub( VECtor *result, VECtor *v1, VECtor *v2);
/* subtract two vectors: result = v1 - v2
   return pointer to result */

extern  void VECtranspArray(float (*result)[3],float (*a)[3]);
/* get transpose of an array 
   result must have separate storage from a 
   return pointer to result */

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* VEC_WAS_INCLUDED */
