
#define  SccsId_hksyath_h  "@(#)hksyath.h	1.1, 05/23/00"

#ifndef _hksymat_h
#define _hksymat_h
typedef enum {
	HkextractSymbolAttribName = 0,	/* Symbol's name */
	HkextractSymbolAttribType = 1,	/* Symbol's type */
	HkextractSymbolAttribValue = 2,	/* Symbol's value */
	HkextractSymbolAttribUndefined = -2	/* undefined attribute */
} HkextractSymbolAttrib;
#endif
