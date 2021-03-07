
#ifndef NRUtil_h
#define NRUtil_h

/* For SCCS */
#define SccsId_NRUtil_h "@(#)NRUtil.h	1.3, 05/27/04"

/* NRUtil.h */

float *vector(int nl, int nh);
float **matrix(int nrl, int nrh, int ncl, int nch);
float **convert_matrix( float *a, int nrl, int nrh, int ncl, int nch);
double *dvector(int nl, int nh);
double **dmatrix(int nrl, int nrh, int ncl, int nch);
int *ivector(int nl, int nh);
int **imatrix(int nrl, int nrh, int ncl, int nch);
float **submatrix(float **a, int oldrl, int oldrh, int oldcl, int oldch, int newrl, int newcl);
void free_vector( float *v, int nl, int nh);
void free_dvector( double *v, int nl, int nh);
void free_ivector( int *v, int nl, int nh);
void free_matrix( float **m, int nrl, int nrh, int ncl, int nch);
void free_dmatrix( double **m, int nrl, int nrh, int ncl, int nch);
void free_imatrix( int **m, int nrl, int nrh, int ncl, int nch);
void free_submatrix( float **b, int nrl, int nrh, int ncl, int nch);
void free_convert_matrix( float **b, int nrl, int nrh, int ncl, int nch);
void nrerror( char error_text[]);

#endif /* NRUtil_h */
