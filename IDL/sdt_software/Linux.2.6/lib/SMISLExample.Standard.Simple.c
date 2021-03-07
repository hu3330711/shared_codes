
// @(#)SMISLExample.Standard.Simple.c	1.8, 08/17/95

#include <SMISL.h>

void SnapOnFillBuffers( void)
   {
   StandardStruct Data ;

   Data.Time = 0.0 ;
   int Continue = 1 ;
   while( Continue)
      {
      int GetError = GetStandardData( "V1-V4_S", 
		       Data.Time, SMISL_TIME_GREATERTHAN, &Data);

      // do whatever!

      if( Data.Valid)
	 PutStandardData( &Data) ;

      Continue = (GetError==SMISL_SUCCESS) ;
      }

   if( GetError != SMISL_END_OF_DATA)
      fprintf( stderr, "Abnormal Termination\n") ;

   return ;
   }

