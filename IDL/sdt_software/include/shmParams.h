/* ---------------------------  shmParams.h -------------------------------
 * - header containing shm paramerter for FAST data aquisition programs  --*/

#ifndef _SHMPARAMSH_
#define _SHMPARAMSH_
static char sccsidShmParamsH[] = "@(#)shmParams.h	1.11 08/19/93 UCB SSL";

#include "telemParams.h"

/*--------------------------------------------------------------------------
 * shared memory parameters
 */

#define SHM_NUM_BLOCKS 750	    /* number of blocks in shm */
#define SHM_KEY 10000               /* key to shm buffer */

#define SHM_RW_HELPER_KEY 10001     /* shm read/write helper key */

#define MAX_MAX_CLIENTS 10          /* the upper limit on shm clients */

#define FILL_PATTERN 0xaa           /* pattern of fill data */


#endif   /*_SHMPARAMSH_*/
