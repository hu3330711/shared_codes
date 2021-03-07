
#define  SccsId_hksytyh_h  "@(#)hksytyh.h	1.1, 05/23/00"

#ifndef _hksymtyp_h
#define _hksymtyp_h
typedef enum {
	HkextractSymbolTypeUndefined = 0,	 /* Undefined Symbol Type */
	HkextractSymbolTypeInteger = 1,	 /* Symbol Type Integer */
	HkextractSymbolTypeReal = 2,	 /* Symbol Type Real */
	HkextractSymbolTypeText = 3,	 /* Symbol Type Text */
	HkextractSymbolTypeTable = 4	 /* Symbol Type Table */
} HkextractSymbolType;
#endif
