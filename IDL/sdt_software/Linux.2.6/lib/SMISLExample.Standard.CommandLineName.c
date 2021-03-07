
// @(#)SMISLExample.Standard.CommandLineName.c	1.2, 08/17/95

#include <SMISL.h>

void SnapOnFillBuffers( void)
   {
   int Stat = 0 ;
   char DataName[ 100] ;
   if( (Stat=GetCommandLineDataName( 1, NULL, DataName)) < 0)
      {
      fprintf( stderr, "GetCommandLineDataName returns: %d\n", Stat) ;
      Error() ;
      return ;
      }
   else
      fprintf( stderr, "%s using <%s>\n", GlobalArgv[ 0], DataName) ;

   StandardStruct Data ;

   Data.Time = 0.0 ;
   int Continue = 1 ;
   while( Continue)
      {
      int GetError = GetStandardData( DataName, 
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

