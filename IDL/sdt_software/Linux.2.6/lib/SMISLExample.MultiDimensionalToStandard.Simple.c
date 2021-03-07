
// @(#)SMISLExample.MultiDimensionalToStandard.Simple.c	1.9, 08/17/95

#include <SMISL.h>

void SnapOnFillBuffers( void)
   {
   MultiDimensionalStruct InData ;
   StandardStruct OutData ;

   InData.Time = 0.0 ;
   int Continue = 1 ;
   while( Continue)
      {
      int GetError = GetMultiDimensionalData( "Sesa Survey",
  		               InData.Time, SMISL_TIME_GREATERTHAN, &InData);

      // do whatever!
      OutData.Valid = InData.Valid ;
      sprintf( OutData.Name, "%s Modified", InData.Name) ;
      sprintf( OutData.UnitsName, "%s", InData.UnitsName) ;
      OutData.Time = InData.Time ;
      OutData.Components = 1 ;
      OutData.Value[ 0] = 0.0 ;
      for( int ne=0; ne<InData.nechs; ne++)
      for( int nr=0; nr<InData.nrows; nr++)
      for( int nc=0; nc<InData.ncols; nc++)
         OutData.Value[ 0] += InData.array[ nr][ nc][ ne] ;

      if( OutData.Valid)
         PutStandardData( &OutData) ;

      Continue = (GetError==SMISL_SUCCESS) ;
      }

   if( GetError != SMISL_END_OF_DATA)
      fprintf( stderr, "Abnormal Termination\n") ;

   return ;
   }

