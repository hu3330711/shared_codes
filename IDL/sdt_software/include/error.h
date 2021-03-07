/*
 * header file for the error handling routines form the Stevens book. 
 * (Unix Network Programming, 1990, Prentice Hall)
 *
 */
#ifndef _ERRORH_
#define _ERRORH_

#include	<stdarg.h>

#ifdef	__cplusplus
extern "C" {
#endif

void myPerror(void) ;
void errInit(char *ident) ;
void errQuit(char*,...) ;
void errSys(char*,...) ;
void errRet(char*,...) ;
void errDump(char*,...) ;
char * sysErrStr(void) ;

#ifdef	__cplusplus
}
#endif

#endif   /* _ERRORH_ */
