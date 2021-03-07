
/* @(#)hkext.h	1.1, 05/23/00
 *
 * HKEXTRACT 
 * Document Reference: 
 * Authors: SJD 
 * Created: Jan 03/95 
 * Modified: 
 *
 * Modified for thread-safeness and SCCS (98/07):  Jack Vernetti SSL/UCB
 */
#define  SccsId_hkext_h  "@(#)hkext.h	1.1, 05/23/00"

#ifndef _hkext_h
#define _hkext_h

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include "ted.h"


typedef void *TED_HKEXTRACT_HPD_ITEM;
typedef void *TED_HKEXTRACT_HPD_TABLE;

typedef struct {
	TED_TIME scet;
	int length;
	int source;
	TED_SPACECRAFT scid;
	TED_GROUND gsid;
	TED_TRANSMISSION stream;
	TED_QUALITY tcal;
	int tasi;
	TED_HKEXTRACT_HPD_TABLE symbols;
} TED_HPD_TABLE;

#ifndef _hktcal_h
#define _hktcal_h
typedef enum {
	TED_HKEXTRACT_CALIBRATED_YES = 0,		/* Value is calibrated */
	TED_HKEXTRACT_CALIBRATED_NO = 1		/* Value is not calibrated */
} TED_HKEXTRACT_CALIBRATED;
#endif

#ifndef _hktvaltyp_h
#define _hktvaltyp_h
typedef enum {
	TED_HKEXTRACT_VALUE_TYPE_UNDEFINED = 0,	/* Undefined Value Type */
	TED_HKEXTRACT_VALUE_TYPE_INTEGER = 1,	/* Symbol Value Integer */
	TED_HKEXTRACT_VALUE_TYPE_REAL = 2,	/* Symbol Value Real */
	TED_HKEXTRACT_VALUE_TYPE_TEXT = 3	/* Symbol Value Text */
} TED_HKEXTRACT_VALUE_TYPE;
#endif


/* CALIBRATED VALUE STRUCTURE */

typedef long INTEGER;
typedef double REAL;
typedef struct {
	TED_HKEXTRACT_CALIBRATED wasCalibrated;
	char pMNEM[25];
	char pUNIT[25];
	char pCATEG[10];
	char pDESCR[256];
	char pCALIB[10240];
	unsigned long raw;
	TED_HKEXTRACT_VALUE_TYPE Type;
	union {
		INTEGER Integer;
		REAL Real;
		char Text[1024];
	} HKEXTRACT_SYMBOL_VALUE_u;
} TED_HKEXTRACT_CALIBRATED_VALUE;
	       
#ifndef _hkext_c

extern
TED_STATUS
ted_hkextract_validate_hpd_header(
unsigned char *				/* I/P DDS PACKET */
);

extern
TED_STATUS
ted_hkextract_validate_hkd_header(
unsigned char *				/* I/P DDS PACKET */
);

extern
TED_STATUS
ted_hkextract_check_corresponence(
unsigned char *,			/* I/P HPD PACKET HEADER */
unsigned char *				/* I/P HKD PACKET HEADER */
);

extern
TED_STATUS
ted_hkextract_lookup_parameter(
TED_HKEXTRACT_HPD_ITEM *,		/* O/P PDE */
TED_HKEXTRACT_HPD_TABLE,		/* I/P PDEs */
int tasi,				/* I/P TASI */
char *pMNEM				/* I/P MNEMONIC */
);

extern
TED_STATUS
ted_hkextract_decode_parameter(
TED_HKEXTRACT_HPD_ITEM,			/* I/P ITEM */
unsigned char *,			/* I/P HK DDSP DATA BLOCK */
unsigned long *				/* O/P RAW VALUE */
);

extern
TED_STATUS
ted_hkextract_calibrate_parameter(
TED_HKEXTRACT_HPD_ITEM,			/* I/P PDE */
unsigned int,				/* I/P RAW VALUE */
TED_HKEXTRACT_CALIBRATED_VALUE *	/* O/P POINTER TO CALIBRATED VALUE */
);

extern
TED_STATUS
ted_hkextract_extract(
unsigned char *,			/* I/P HK DDS PACKET */
char MNEM[],				/* I/P NAME OF PARAMETER TO OBTAIN */
TED_HPD_TABLE *,			/* I/P TABLE OF HPD DEFINITIONS */
TED_HKEXTRACT_CALIBRATED_VALUE * 	/* O/P CALIBRATED VALUE */
);

extern
void
ted_hkextract_freetable(
TED_HPD_TABLE **			/* I/P TABLE */
);

extern
TED_STATUS
ted_hkextract_parse_and_build(
TED_HPD_TABLE **,			/* ADDRESS OF TABLE POINTER */
unsigned char *,			/* I/P HPD PACKET */
char *					/* I/P WEC ALIAS STRING */
);

#endif

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif

