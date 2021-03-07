/*  @(#)calinf.h	1.1, 05/23/00
 *
 * HKTOOL 
 * Document Reference: 
 * Authors: SJD 
 * Created: Jan 03/95 
 * Modified: 
 *
 * Modified for thread-safeness and SCCS (98/07):  Jack Vernetti SSL/UCB
 */
#define  SccsId_calinf_h  "@(#)calinf.h	1.1, 05/23/00"

#define _calinf_h

#include "tedsys.h"

typedef enum {
  CALINF_STATUS_NA,
  CALINF_COORD_FOUND,
  CALINF_COORD_NOT_FOUND,
  CALINF_STATUS_FOUND,
  CALINF_STATUS_NOT_FOUND
} CALINF_RESULT;

#ifndef _calpointy_c

extern
void
calPointYaccInit(
unsigned long,			/* RAW PARAMETER */
char *text			/* CALINF TEXT */
);

extern
int
calPointRes(
void
);

extern
double
calPointCoord(
void
);

#endif

#ifndef _calenuml_c

extern
calEnumLexInit(
char *,				/* CALINF TEXT */
int				/* LENGTH of CLAINF TEXT */
);

#endif

#ifndef _calpointl_c

extern
calPointLexInit(
char *,				/* CALINF TEXT */
int				/* LENGTH of CALINF TEXT */
);

#endif

#ifndef _calenumy_c

extern
void
calEnumYaccInit(
unsigned long,			/* RAW VALUE */
char *text			/* CALINF TEXT */
);

extern
int
calEnumRes(
void
);

extern
char *
calEnumStatus(
void
);

#endif

/* Local variables: */
/* mode: c */
/* folded-file: t */
/* end: */
