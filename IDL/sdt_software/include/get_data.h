#ifndef get_data_h
#define get_data_h

static char sccsidGetDataH[] = "@(#)get_data.h	1.7 10/28/93 UCB SSL";

/* FAKEIT defined if want to pretend we are writing to normal linear buffer 
 * for a quick and dirty test */
/* COREFLOAT defined if core cannot handle double precision data values */
 
/* get_data.h */

#include <unistd.h>
#include <math.h>

/* #include <Error.h> */

#ifdef	__cplusplus
extern "C" {
#endif


typedef struct {
	       int AppId ;
	       double StartTime ;
	       double EndTime ;
	       int PhysicalLength ;
	       int *Length ;
	       int *Location ;
#ifdef COREFLOAT
	       float *Time ;
	       float *Value ;
#else
	       double *Time ;
	       double *Value ;
#endif
	       } AppIdDesc ;

#ifndef get_data_c
extern int get_data( int AppIdDescCount, AppIdDesc *AID) ;
#endif get_data_c

#ifdef	__cplusplus
}
#endif

#endif get_data_h

