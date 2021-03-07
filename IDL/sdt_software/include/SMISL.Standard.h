
// SMISL.Standard.h

#ifndef SMISL_Standard_h_def
#define SMISL_Standard_h_def

#include <SnapOn.ANSI-C.h>
#include <SMISL.General.h>

#define SccsId_SMISL_Standard_h "@(#)SMISL.Standard.h	1.13, 05/30/04"

#define SMISL_STANDARD_DIM 1000

struct standard_struct {
   int Valid ;
   char Name[ SMISL_MAX_CHAR_LENGTH] ;
   char UnitsName[ SMISL_MAX_CHAR_LENGTH] ;
   int Year ;
   int Month ;
   int Day ;
   double Time ;
   int Components ;
   float Value[ SMISL_STANDARD_DIM] ;
   int QualityBit[ SMISL_STANDARD_DIM] ;
   } ;
typedef struct standard_struct StandardStruct ;

int GetStandardData( char *DataName, double Time, int WhichFlag, StandardStruct *Data) ;
int PutStandardData( StandardStruct *Data) ;
void PrintStandardData( FILE *Stream, StandardStruct *Data) ;


#endif // SMISL_Standard_h_def

