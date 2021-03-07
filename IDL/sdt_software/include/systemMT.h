
/*********************************************************************
 * header for systemMT.c
 */

#if ! defined (_SYSTEMMTH_)
#define _SYSTEMMTH_

#include <sys/types.h>

int systemMT(char *) ;
pid_t forkexecMT(char * cmd) ;


#endif   /* _SYSTEMMTH_ */
