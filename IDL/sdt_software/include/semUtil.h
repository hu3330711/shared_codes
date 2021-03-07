/* Header file for the simple semaphore interface "semUtil.c" from the
 * Stevens book:
 * (Unix Network Programming, 1990, Prentice Hall)
 *
 */

#ifndef _SEMUTILH_
#define _SEMUTILH_

#ifdef	__cplusplus
extern "C" {
#endif

#include	<sys/types.h>
#include 	<sys/ipc.h>
#include 	<sys/sem.h>
#include	<errno.h>

#define MAX_SEM_VALUE 32765


int semCreate (key_t, int) ;
int semOpen (key_t) ;
void semClose (int) ;
void semWait (int) ;
void semSignal (int) ;
void semOp (int, int) ;
void semRm (int) ;
int semValue (int) ;
void semSet (int, int) ;

#ifdef	__cplusplus
}
#endif

#endif      /* _SEMUTILH_ */
