
// SMISL.Depth.h

#ifndef SMISL_Depth_h_def
#define SMISL_Depth_h_def

#include <SnapOn.ANSI-C.h>
#include <SMISL.General.h>

#define SccsId_SMISL_Depth_h "@(#)SMISL.Depth.h	1.4, 05/30/04"

#define SMISL_DEPTH_DIM 1000
#define SMISL_DEPTH_DEPTH_DIM 800

struct depth_struct {
   int Valid ;
   char Name[ SMISL_MAX_CHAR_LENGTH] ;
   char UnitsName[ SMISL_MAX_CHAR_LENGTH] ;
   int Year ;
   int Month ;
   int Day ;
   double Time ;
   int Components ;
   int ComponentDepths[ SMISL_DEPTH_DIM] ;
   float Value[ SMISL_DEPTH_DIM][ SMISL_DEPTH_DEPTH_DIM] ;
   } ;
typedef struct depth_struct DepthStruct ;

int GetDepthData( char *DataName, double Time, int WhichFlag, DepthStruct *Data) ;
int PutDepthData( DepthStruct *Data) ;
void PrintDepthData( FILE *Stream, DepthStruct *Data) ;


#endif // SMISL_Depth_h_def

