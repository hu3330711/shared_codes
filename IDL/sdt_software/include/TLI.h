#if     !defined(lint)
static char sccsidTLIH[] = "@(#)TLI.h	1.7 11/22/06 UCB SSL";
#endif

#ifndef _TLIH_
#define _TLIH_

#ifndef LINUX_A
#include <netdir.h>
#include <tiuser.h>
#endif

#include <fcntl.h>

#ifndef LINUX_A
#include <netconfig.h>
#endif

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "error.h"
#include "MTShmCirBuf.h"
#include "MTReadWriteHelper.h"

#ifdef	__cplusplus
extern "C" {
#endif

int initUdpBroadcast (char *serverName, char *service[], unsigned int *addrLen,
		      char *addrBuf[], boolean_t server, char** udpErr);
int closeUdpBroadcast (int fd, char **udpErr);
char *name2addr(char *);
char *addr2broadaddr(char *);

#ifdef	__cplusplus
}
#endif

#endif /* _TLIH_ */
