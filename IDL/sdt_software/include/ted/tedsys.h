/*
 * @(#)tedsys.h	1.1, 05/23/00
 *
 * Project:         CLUSTER WEC TED Package
 *
 * Item Name:       src/ted/tedsys.h
 *
 * Item Version:    (see RCS header)
 *
 * Item Type:       c-header
 *
 * Author:          Toby Champion
 *
 * Modified for thread-safeness and SCCS (98/07):  Jack Vernetti SSL/UCB
 *
 */
#define  SccsId_tedsys_h  "@(#)tedsys.h	1.1, 05/23/00"

#ifndef _ted_tedsys_h
#define _ted_tedsys_h

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include <errno.h>
#include <math.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#ifndef TED_NO_FD
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#endif /*!TED_NO_FD*/

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /*!_ted_tedsys_h*/
