
#define  SccsId_alias_h  "@(#)alias.h	1.1, 05/23/00"

typedef union
#ifdef __cplusplus
	ALIASSTYPE
#endif
 {
  HkextractSymbol *SYMBOL;
  char   STRING[1023];
  int    INT;
} ALIASSTYPE;
extern ALIASSTYPE aliaslval;
# define TSTRING 257
