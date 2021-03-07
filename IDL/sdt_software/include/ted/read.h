/*
 * @(#)read.h	1.1, 05/23/00
 *
 * Project:         CLUSTER WEC TED Package
 *
 * Item Name:       src/read/read.h
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
#define  SccsId_read_h  "@(#)read.h	1.1, 05/23/00"

#ifndef _read_read_h
#define _read_read_h

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include "ted.h"
#include "tedsys.h"

extern TED_STATUS ted_read_debug(
TedSession *tsess,
int debug_level
);

extern TED_STATUS ted_read_initf(
TedSession *tsess,
FILE *nsd_stream,
FILE *bsd_stream,
FILE *hkd_stream
);

extern TED_STATUS ted_read_init(
TedSession *tsess,
int nsd_fd,
int bsd_fd,
int hkd_fd
);

extern TED_STATUS ted_read_packet(
TedSession *tsess,
unsigned char **nsd_packet,
unsigned char **bsd_packet,
unsigned char **hkd_packet
);

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /*!_read_read_h*/
