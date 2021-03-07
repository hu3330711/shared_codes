
#define  SccsId_calenum_h  "@(#)calenum.h	1.1, 05/23/00"

typedef union
#ifdef __cplusplus
	CALENUMSTYPE
#endif
 {
  char   STRING[1023];
  int    INT;
} CALENUMSTYPE;
extern CALENUMSTYPE calEnumlval;
# define TSTRING 257
