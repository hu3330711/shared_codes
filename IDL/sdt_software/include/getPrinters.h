
/*********************************************************************
 * header for getPrinters.c
 */

#if ! defined (_GETPRINTERSH_)
#define _GETPRINTERSH_

#include "systemMT.h"

typedef struct 
{
    int		numPrinters ;
    char **	printerNames ;
} printerInfo ;

printerInfo * getPrinters (printerInfo *) ;
void freePrinterInfo (printerInfo *) ;
    
#endif   /* _GETPRINTERSH_ */
