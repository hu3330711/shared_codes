/*
 * @(#)misc.h	1.1, 05/23/00
 *
 * Project:         CLUSTER WEC TED Package
 *
 * Item Name:       src/ted/misc.h
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
#define  SccsId_misc_h  "@(#)misc.h	1.1, 05/23/00"

#ifndef _ted_misc_h
#define _ted_misc_h

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include "ted.h"
#include "tedsys.h"
#include "misc.h"
#include "xmacros.h"
#include "unpack.h"

typedef struct {
  char *item;
  int version;
  int revision;
  int patch;
} rcssymrevinfo;

extern TED_STATUS UnixToCCSDS(time_t seconds, TED_TIME *ccsds);
extern TED_STATUS CCSDSToUnix(TED_TIME *ccsds, time_t *seconds);
extern char *CCSDSToString(TED_TIME *ccsds);
extern TED_STATUS PacketToCCSDS(unsigned char *packet, TED_TIME *ccsds);
extern void show_buffer(char *buffer, int length);
extern char *StatusToString(TED_STATUS status);
extern char *DiagnosticToString(TED_UNPACK_DIAG *diagnostic);
extern int ParseRCSSYMREVNAME(char *rcssymrevname, rcssymrevinfo **info);
extern char *VersionToString(int version, int revision, int patch, int userpatch);

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /*!_ted_misc_h*/
