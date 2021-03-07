#ifndef JCS_Error_h
#define JCS_Error_h
/* Error1.h */

#define SccsId_Error1_h "@(#)Error1.h	1.1, 02/04/07"

/* NOTE - 2007/02/04:  This file replaces "Error.h".
 * This is required because of the case-insensitiviey of
 * the MacOSX file system names.  Thus, we need to distinguish
 * this file from the "error.h" from "src/ipc_lib".
 */

#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <Boolean.h>
#include <General.h>

#ifdef JCS_Error_cc
char JCS_ERROR_comp_file[ MAX_LINE_LENGTH] ;
int JCS_ERROR_comp_line ;
int JCS_ERROR_i ;
#else /* JCS_Error_cc */
extern char JCS_ERROR_comp_file[ MAX_LINE_LENGTH] ;
extern int JCS_ERROR_comp_line ;
extern int JCS_ERROR_i ;
#endif /* JCS_Error_cc */

#define LOG_IT for(strcpy( JCS_ERROR_comp_file, __FILE__),\
	           JCS_ERROR_comp_line = __LINE__,\
		   JCS_ERROR_i = 1; JCS_ERROR_i==1; JCS_ERROR_i--)\
	          Log_It

void Log_It( int level,  char *fmt, ...) ;

#endif /* JCS_Error_h */

