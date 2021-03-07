#ifndef _GRADLSH_
#define _GRADLSH_

#if     !defined(lint)
static char sccsidApidMnem[] = "@(#)gradls.h	1.3 01/11/00";
#endif

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int gradls(double * x, double * y, double * sigmay, 
	   long npts, int nterms, int mode, double ** a, 
	   double * deltaa, double * yfit, 
	   double * chisqr, double * gr_red_by,
	   double (*func)(double *, long, double **), 
	   double (*fchisq)(double *,double *,long,long,int,double *),
	   FILE * fout) ;

int gradls_md(double * x, double * y, double * sigmay, 
	      long npts, int ndepdims, int nterms, int mode, double ** a, 
	      double * deltaa, double * yfit,
	      double * chisqr, double * gr_red_by,
	      double *(*func)(double *, long, double **), 
	      double (*fchisq)(double *,double *,long,long,int,double *),
	      FILE * fout) ;

#endif  /* _GRADLSH_ */
