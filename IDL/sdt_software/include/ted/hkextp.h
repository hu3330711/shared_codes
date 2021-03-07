
#define  SccsId_hkextp_h  "@(#)hkextp.h	1.1, 05/23/00"

#include "hkext.h"
#include "hkconst.h"
#include "hkadidh.h" 
#include "hkscidh.h"
#include "hkgsidh.h"
#include "hktcalh.h"
#include "hktasih.h"
#include "hkstrmh.h"
#include "hkstath.h"
#include "hksyath.h"
#include "hksytyh.h"

typedef char *STRING;

typedef struct {
	HkextractSymbolType Type;
	union {
		INTEGER Integer;
		REAL Real;
		char *Text;
		void *Table;
	} hkextractSymbolValue_u;
} HkextractSymbolValue;

typedef struct {
	char *Name;
	HkextractSymbolValue Value;
} HkextractSymbol;

typedef struct {
	int *hashTable;
	HkextractSymbol **symbolTable;
	int tableSize;
	int tableSizeIndex;
	int nextLocation;
} HkextractTable;

typedef struct {
	HkextractSymbolAttrib a;
	HkextractSymbolValue v;
} HkextractSymbolAttribRes;

typedef struct {
	unsigned int days;
	unsigned long ms;
	unsigned int us;
} HkextractTime;

#ifndef _hkext_c

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

extern
FILE *
ted_hkextract_display_hpd_table(
FILE *,					/* I/P OUTPUT STREAM */
TED_HPD_TABLE *				/* I/P HPD TABLE */
);

extern
FILE *
hkextractPrintTable(
FILE *os,				/* I/P I/O STREAM */
HkextractTable *			/* I/P TABLE */
);

extern
HkextractTable *
hkextractNewTable(
void
);

extern
int
hkextractTableHash(
HkextractTable *,			/* I/P TABLE */
char *s					/* I/P STRING */
);

extern
int
hkextractGrowTable(
HkextractTable *,			/* I/P TABLE */
char * (*sname)(HkextractSymbol *)	/* I/P SYMBOL NAME EXTRACTION FUNCTION */
);

extern
int
hkextractTableInsert(
HkextractTable *,			/* I/P TABLE */
HkextractSymbol *,			/* I/P SYMBOL */
char *pch,				/* I/P STRING || NULL */
char * (*sname) (HkextractSymbol *)	/* I/P SYMBOL NAME EXTRACTION FUNCTION */
);

extern
int
hkextractTableSearch(
HkextractTable *,			/* I/P TABLE */
HkextractSymbol **,			/* O/P ADDRESS OF SYMBOL POINTER */
char *Name,				/* I/P NAME */
char * (*sname)(HkextractSymbol *)	/* I/P SYMBOL NAME EXTRACTION FUNCTION */
);

extern
int
hkextractDeleteTable(
HkextractTable **			/* I/P ADDRESS OF TABLE POINTER */
);

extern
int
hkextractInsertSymbol(
HkextractSymbol *,			/* I/P TABLE */
HkextractSymbol *,			/* I/P SYMBOL */
const char *				/* I/P NAME */
);

extern
int
hkextractDeleteSymbol(
HkextractSymbol **			/* I/P ADDRESS OF SYMBOL POINTER */
);

extern
FILE *
ted_hkextract_print_calibrated_value(
FILE *,					/* OUTPUT STREAM */
TED_HKEXTRACT_CALIBRATED_VALUE *	/* CALIBRATED VALUE */
);

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
ted_hkextract_parse_hpd_data(
TED_HPD_TABLE *this,			/* TABLE */
unsigned char *,			/* I/P HPD DATA */
unsigned long				/* I/P DATA LENGTH */
);

extern
TED_STATUS
ted_hkextract_parse_hpd(
TED_HPD_TABLE *,			/* TABLE */
unsigned char *pPacket			/* I/P DDS HPD PACKET */
);

extern
TED_STATUS
ted_hkextract_parse_aliases(
TED_HPD_TABLE *,			/* TABLE */
char *aliases				/* I/P USER PARAMETER ALIASES */
);

extern
TED_STATUS
ted_hkextract_parse_wpd(
TED_HPD_TABLE *,			/* TABLE */
unsigned char *				/* I/P WEC DDS PDE PACKET */
);

extern
void
ted_hkextract_close_table(
TED_HPD_TABLE *				/* I/P TABLE */
);

extern
void
ted_hkextract_init_table(
TED_HPD_TABLE *				/* I/P UNINITALISED TABLE */
);

extern
TED_STATUS
ted_hkextract_validate_dds_header(
unsigned char *				/* I/P DDS PACKET HEADER */
);

extern
TED_STATUS
ted_hkextract_check_corresponence(
unsigned char *,			/* I/P HPD DDS PACKET HEADER */
unsigned char *				/* I/P HK  DDS PACKET HEADER */
);

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif
