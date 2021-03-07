/* ---------------------------  shmReaders.h -------------------------------
 * --------- header file for programs that read FAST data from shm ---------*/

#ifndef _SHMWRITERSH_
#define _SHMWRITERSH_
static char sccsidShmWritersH[] = "@(#)shmWriters.h	1.4 08/13/93 UCB SSL";

#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <signal.h>
#include <errno.h>

#include "socketUtil.h"
#include "error.h"
#include "MTShmCirBuf.h"
#include "MTReadWriteHelper.h"
#include "shmParams.h"

/*---------------------------------------------------------------------------
 * Defines
 */

  /* make sure we won't have too many shm clients. */

#define MAX_SHM_CLIENTS 10


#endif    /*  _SHMWRITERSH_  */
