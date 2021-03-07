#ifndef Example_c
#define Example_c

/* SCCS ID string: */
#ifndef SccsId_Example_c_def
#define SccsId_Example_c_def
static char SccsId_Example_c[] = "@(#)Example.c	1.1, 06/09/94" ;
#endif

/* The following Source Code file is located in file $FASTHOME/include/Example.c */
/* ************************************************************************* */

/* #define DEBUGS */

#include <math.h>
#include <SnapOn.ANSI-C.h>

int SnapOnSetUpValues( SetUpValuesResponse *SUVResponse)
   { 
   SUVResponse->Size = GetInputTotal( 0) ;
   SUVResponse->ArraySize = 0 ;

   return( SUVResponse->Size) ;
   }

void SnapOnFillBuffers( void)
   {
   if( GlobalArgc < 4)
      {
      fprintf( stderr, "usage: Example XCompIndex YCompIndex ZCompIndex\n") ;
      Error() ;
      return ;
      }
   
   int XCompIndex = atoi( GlobalArgv[ 1]) ;
   int YCompIndex = atoi( GlobalArgv[ 2]) ;
   int ZCompIndex = atoi( GlobalArgv[ 3]) ;

   float Scale = GetScale() ;
   float Offset = GetOffset() ;

   int Stat = 0 ;
   int SkipPoints = 0 ;
   if( (Stat = GetParamInt( 0, &SkipPoints)) < 0)
      {
      fprintf( stderr, "GetParamInt error: %d\n", Stat) ;
      Error() ;
      return ;
      }
   float TimeOffset = 0 ;
   if( (Stat = GetParamFloat( 0, &TimeOffset)) < 0)
      {
      fprintf( stderr, "GetParamFloat error: %d\n", Stat) ;
      Error() ;
      return ;
      }

   BlockUntilInputsDone() ;

   int Total = GetInputTotal( 0) ;
   int Length = GetInputCurrent( 0) ;

   for( int n=0; !GetDone() && (n<Length) && (n<Total); n+= SkipPoints)
      {
      float Time = 0.0 ;
      if( (Stat = GetInputFloat( 0, 0, n, &Time)) < 0)
         {
         fprintf( stderr, "GetInputFloat error: %d\n", Stat) ;
         Error() ;
         return ;
         }
      Time += TimeOffset ;

      float XValue = 0.0 ;
      if( (Stat = GetInputFloat( 0, XCompIndex, n, &XValue)) < 0)
         {
         fprintf( stderr, "GetInputFloat error: %d\n", Stat) ;
         Error() ;
         return ;
         }
      float YValue = 0.0 ;
      if( (Stat = GetInputFloat( 0, YCompIndex, n, &YValue)) < 0)
         {
         fprintf( stderr, "GetInputFloat error: %d\n", Stat) ;
         Error() ;
         return ;
         }
      float ZValue = 0.0 ;
      if( (Stat = GetInputFloat( 0, ZCompIndex, n, &ZValue)) < 0)
         {
         fprintf( stderr, "GetInput error: %d\n", Stat) ;
         Error() ;
         return ;
         }

      float Magnitude = sqrt( XValue*XValue + YValue*YValue + ZValue*ZValue) ;

      Magnitude -= Offset ;
      Magnitude *= Scale ;

#ifdef DEBUGS
      fprintf( stderr, "n:%d, Time: %f, Magnitude: %f\n", n, Time, Magnitude) ;
#endif

      if( (Stat = SetOutputFloat( 0, n, Time)) < 0)
         {
         fprintf( stderr, "SetOutputFloat error: %d\n", Stat) ;
         Error() ;
         return ;
         }
      if( (Stat = SetOutputFloat( 1, n, Magnitude)) < 0)
         {
         fprintf( stderr, "SetOutputFloat error: %d\n", Stat) ;
         Error() ;
         return ;
         }

      SetCurrent( n+1) ;
      }

   return ;
   }

/* ************************************************************************* */

#endif Example_c
