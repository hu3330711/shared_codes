
#define  SccsId_calpoint_h  "@(#)calpoint.h	1.1, 05/23/00"

typedef union
#ifdef __cplusplus
	CALPOINTSTYPE
#endif
 {
  long   LONG;
  float  DBL;
  int    INT;
} CALPOINTSTYPE;
extern CALPOINTSTYPE calPointlval;
# define TINTLIT 257
# define TREALIT 258
