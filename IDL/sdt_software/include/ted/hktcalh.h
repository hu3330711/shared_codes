
#define  SccsId_hktcalh_h  "@(#)hktcalh.h	1.1, 05/23/00"

#ifndef _hktcalh_h
#define _hktcalh_h
typedef enum {
	HkextractTcalACTUAL = 0,	/* Actual time */
	HkextractTcalEXTRAPOLATED = 1,	/* Extrapolated time */
	HkextractTcalCONTINGENCY = 2	/* Contingency time */
} HkextractTcal;
#endif
