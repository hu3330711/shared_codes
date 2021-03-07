
// @(#)SMISLExample.Standard.Parameter.c	1.2, 08/17/95

#include <SMISL.h>

void SnapOnFillBuffers( void)
   {

   int Stat = 0 ;
   int Value = 0 ;
   if( (Stat=GetParamInt( 0, &Value)) < 0)
      {
      fprintf( stderr, "GetParamInt( 0) returns %d\n", Stat) ;
      Error() ;
      return ;
      }
   else
      fprintf( stderr, "Got Parameter: Value=%d\n", Value) ;

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

