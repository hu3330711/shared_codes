#ifndef _APIDMNEMH_
#define _APIDMNEMH_

#if     !defined(lint)
static char sccsidApidMnem[] = "@(#)ApidMnem.h	1.4 11/21/06";
#endif
/* ---------------------------  ApidMnem.h ---------------------------------
 * Apid Mnemonics.
 */
 
#include <sys/types.h>

#ifdef LINUX_A
#include <TDefsForLinux.h>
#endif

/*---------------------------------------------------------------------------
 * some data structure constants
 */

#define APIDMNEMFILE "ApidMnem"
#define MAX_APPID_MNEM_LEN 9 /* max length of app ID mnemonics */

/* appid mnemonic struct */

struct appIdMnemInfo
{
  int		appId ;             /* actual app ID */
  char *	mnem ;              /* mnemonic to display */
  char *	longMnem ;          /* full description of app ID */
} ;

/* struct containing all app ID mnemonics */

struct appIdMnemonics
{
  int				numMnems ;
  struct appIdMnemInfo *	mnemInfo ;
} ;
   
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 * function prototypes.
 */

int ParseApidMnem (struct appIdMnemonics *appIdMnems) ;

boolean_t hashAppIds (struct appIdMnemonics * appIdMnems, int min, int max) ;
                                      /* create a hash table to reference the
				       * non-contiguous app Id list */
int getAppIdHash (int) ;              /* given and appId in range, return hash*/

/* utility functions used in parsing: */
char ** mallocWords (void) ;
int strWords (char **, const char *) ;
void strCompress (char *, char *) ;
void freeWords (char **, int, int, boolean_t) ;

#endif    /* _APIDMNEMH_ */
