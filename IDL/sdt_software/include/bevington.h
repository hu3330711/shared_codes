/* some useful routines from Bevington, modified some for use in science 
 * modules
 */

#ifndef bevington_h
#define bevington_h

#define SCCSID_bevington_h "@(#)bevington.h	1.2 05/19/95 UCB SSL"


#include "quickmath.h"

#ifdef	__cplusplus
extern "C" {
#endif

#define MAXSIZE  10 /* max size for all matrices */

typedef struct polyfit_struct {
  int nterms;               /* this must be set to the number of terms */
  int mode;		    /* this must be set to desired mode */
  double a[MAXSIZE] ;	    /* the coefficient results */
  double chisqr;	    /* the chisquare results */

  double sumx[2*MAXSIZE-1]; /* for internal use */
  double sumy[MAXSIZE];	    /* for internal use */
  int nmax;
  int npts;		    /* number of data points */
} polyfit;   

typedef struct lsfit_struct {
  int nterms;                  /* number of terms (coefficients) */
  double a[MAXSIZE];           /* the coefficients */
  double sigma;                /* sqrt of reduced chisquare */

  double X[MAXSIZE][MAXSIZE];  /* the X matrix */
  double Y[MAXSIZE];           /* the y column matrix */
  double yy;                   /* used in calculating chisqr */
} lsfit ;

#define PTS_PER_FIT 64

typedef struct spinfit_struct {
  int norig;                   /* # of points started out with */
  int x[PTS_PER_FIT] ;         /* array of indices into sine & cosine array */
  double y[PTS_PER_FIT] ;      /* array of y values */
  int iteration ;	       /* iterations of spinfit_throw_away_points() */
  lsfit fit;
} spinfit ;

typedef struct spinresults_struct {
  float time;
  double A, B, C;
  double sigma;
  int n, norig;
} spinresults ;

void lsfit_init(int nterms, lsfit *lsfit);
void lsfit_calculate_coeff(lsfit *lsfit);

void spinfit_gentable(void);
void spinfit_init(spinfit *spinfit);
void spinfit_build_sums(int x, double y, spinfit *spinfit);
int spinfit_throw_away_points(spinfit *spinfit);

void polyfit_init(int nterms, int mode, polyfit *polyfit);
void polyfit_build_sums(double x, double y, double sigmay, polyfit *polyfit);
void polyfit_calculate_coeff(polyfit *polyfit);

double polyfit_expand(double x, double *a, int nterms);

double determ(double matrix[MAXSIZE][MAXSIZE], int order);
 
#ifdef	__cplusplus
}
#endif

#endif

