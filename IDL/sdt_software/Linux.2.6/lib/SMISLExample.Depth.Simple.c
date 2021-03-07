
// @(#)SMISLExample.Depth.Simple.c	1.2, 08/17/95

#include <SMISL.h>

void SnapOnFillBuffers( void)
   {
   DepthStruct Data ;

   Data.Time = 0.0 ;
   int Continue = 1 ;
   while( Continue)
      {
      int GetError = GetDepthData( "DataHdr_1032", 
		       Data.Time, SMISL_TIME_GREATERTHAN, &Data);

      // do whatever!

      if( Data.Valid)
	 PutDepthData( &Data) ;

      Continue = (GetError==SMISL_SUCCESS) ;
      }

   if( GetError != SMISL_END_OF_DATA)
      fprintf( stderr, "Abnormal Termination\n") ;

   return ;
   }

