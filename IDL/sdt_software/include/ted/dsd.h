/*
 * @(#)dsd.h	1.1, 05/23/00
 *
 * Project:         CLUSTER WEC TED Package
 *
 * Item Name:       src/dsd/dsd.h
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
#define  SccsId_dsd_h  "@(#)dsd.h	1.1, 05/23/00"

#ifndef _dsd_dsd_h
#define _dsd_dsd_h

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include "ted.h"

enum {
  TED_DSD_SOURCE_EFW = 0,
  TED_DSD_SOURCE_STAFFSA = 1,
  TED_DSD_SOURCE_STAFFMWF = 2,
  TED_DSD_SOURCE_WHISPER = 3,
  TED_DSD_SOURCE_WBD = 4,
  TED_DSD_SOURCE_DWP = 5
};

enum {
  TED_DSD_GROUND_ODENWALD = 1,
  TED_DSD_GROUND_REDU = 2,
  TED_DSD_GROUND_KOUROU = 3,
  TED_DSD_GROUND_PERTH = 4,
  TED_DSD_GROUND_MALINDI = 5,
  TED_DSD_GROUND_CANBERRA = 6,
  TED_DSD_GROUND_GOLDSTONE = 7,
  TED_DSD_GROUND_NA = 15
};

enum {
  TED_DSD_SPACECRAFT_CLUSTER_1 = 1,
  TED_DSD_SPACECRAFT_CLUSTER_2 = 2,
  TED_DSD_SPACECRAFT_CLUSTER_3 = 3,
  TED_DSD_SPACECRAFT_CLUSTER_4 = 4
};

#define TED_DSD_DW_NOHK 0x8000
#define TED_DSD_DW_TRUNC 0x4000
#define TED_DSD_DW_HKTIMESAME 0x2000
#define TED_DSD_DW_HKEXTRAPOLATED 0x1000
#define TED_DSD_DW_HKCONTINGENCY 0x0800
#define TED_DSD_DW_NOHKTIME 0x0400
#define TED_DSD_DW_EW5PRCTLNONZERO 0x0200
#define TED_DSD_DW_EW5SSOFFTEST 0x0100

enum {
  TED_DSD_DEFINITION_SET_TED = 0,
  TED_DSD_DEFINITION_SET_DECOM = 1
};

TED_STATUS ted_dsd_init(TedSession *tsess, int definition_set);

TED_STATUS ted_dsd_build(TedSession *tsess, TED_DSD_ITEMS *items,
    unsigned char *hkblock, unsigned char *sciencedata,
    unsigned char **dsdheader, int *headerlength,
    unsigned char **dsddata);

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /*!_dsd_dsd_h*/
