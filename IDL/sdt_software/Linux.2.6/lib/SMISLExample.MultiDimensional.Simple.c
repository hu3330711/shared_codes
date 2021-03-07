
// @(#)SMISLExample.MultiDimensional.Simple.c	1.7, 08/17/95

#include <SMISL.h>

void SnapOnFillBuffers( void)
   {
   MultiDimensionalStruct Data ;

   Data.Time = 0.0 ;
   int Continue = 1 ;
   while( Continue)
      {
      int GetError = GetMultiDimensionalData( "Sesa Survey",
  		               Data.Time, SMISL_TIME_GREATERTHAN, &Data);

      // do whatever!

      if( Data.Valid)
         PutMultiDimensionalData( &Data) ;

      Continue = (GetError==SMISL_SUCCESS) ;
      }

   if( GetError != SMISL_END_OF_DATA)
      fprintf( stderr, "Abnormal Termination\n") ;

   return ;
   }

