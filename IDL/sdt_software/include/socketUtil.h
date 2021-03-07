/* Header for s ocket utility functions, for the stevens Book:  (Unix Network 
 * Programming, 1990, Prentice Hall)
 */

#ifndef _SOCKUTILH_
#define _SOCKUTILH_

#ifdef	__cplusplus
extern "C" {
#endif

int readn(register int, register char *, register int) ;
int nonblockreadn(register int, register char *, register int, 
		  register int *) ;
int writen(register int, register char *, register int) ; 
int nonblockwriten(register int	fd, register char *ptr, register int nbytes,
		   register int *quit) ;
int readline(register int, register char *, register int) ;

#ifdef	__cplusplus
}
#endif

#endif    /* _SOCKUTILH_ */
