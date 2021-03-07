/*
 * @(#)unpack.h	1.1, 05/23/00
 *
 * Project:         CLUSTER WEC TED Package
 *
 * Item Name:       src/unpack/unpack.h
 *
 * Item Version:    (see RCS header)
 *
 * Item Type:       c-source
 *
 * Author:          Toby Champion
 *
 * Modified for thread-safeness and SCCS (98/07):  Jack Vernetti SSL/UCB
 *
 */
#define  SccsId_unpack_h  "@(#)unpack.h	1.1, 05/23/00"

#ifndef _unpack_unpack_h
#define _unpack_unpack_h

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include "ted.h"

enum {
  INSTRUMENT_EFW = 1,
  INSTRUMENT_STAFFSA = 2,
  INSTRUMENT_STAFFMWF = 4,
  INSTRUMENT_WHISPER = 8,
  INSTRUMENT_WBD = 16,
  INSTRUMENT_DWP = 32
};

extern TED_STATUS ted_unpack_debug(
TedSession *tsess,
int debug_level
);

extern TED_STATUS ted_unpack_init(
TedSession *tsess,
TED_SPACECRAFT spacecraft,
int instruments,
int ccs_converted,
int model_tag_table[]
);				  

extern TED_STATUS ted_unpack_decom(
TedSession *tsess,
unsigned char *exppacket,
unsigned char *hkpacket,
int *gotscience,
TED_DSD_ITEMS **items,
unsigned char **hkblock,
unsigned char **sciencedata,
int *again
);

extern TED_STATUS ted_unpack_diagnostic(
TedSession *tsess,
TED_UNPACK_DIAG **diagnostic
);

extern TED_STATUS ted_unpack_hk_timestamp(
TedSession *tsess,
unsigned char **timestamp
);

extern TED_STATUS ted_unpack_diagcode2string(
TED_UNPACK_DIAGCODE diagcode,
char **string
);

extern TED_STATUS ted_unpack_diag2string(
TED_UNPACK_DIAG *diag,
char **string
);

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /*!_unpack_unpack_h*/
