
// SMISL.Size.h

#ifndef SMISL_Size_h_def
#define SMISL_Size_h_def

#include <SnapOn.ANSI-C.h>

#define SccsId_SMISL_Size_h "@(#)SMISL.Size.h	1.14, 05/30/04"


#define  DEFAULT_ARRAY_SIZE    16384

#define  DEFAULT_ARRAY_DESC_SIZE \
             ComputeArrayDescStorage(  1000,     1,   100, \
                                          0,     0,     0, \
                                          0,     0,     0)


#ifndef SMISL_SIZE_USER_DEFINED

#define DO_DEFAULT_CASE SUVResponse->Size = InputSUVR.Size ; SUVResponse->ArraySize = InputSUVR.ArraySize ; SUVResponse->ArrayDescriptionSize = InputSUVR.ArrayDescriptionSize

int SnapOnSetUpValues(SetUpValuesResponse *SUVResponse)
   {
   int Stat = 0 ;

   SetUpValuesResponse InputSUVR ;
   if( (Stat = GetInputCount()) < 1)
      {
      (void) fprintf(stderr,
                 "\n"
                 "ERROR    ****    SMISL.Size.h\n"
                 "SIZE allocation by SMISL requires at least one\n"
                 "Science Module input; but none were found\n"
                 "\n") ;
      return( ErrorSize()) ;
      }
   if( (Stat = GetInputSetUpValues( 0, &InputSUVR)) < 0)
      {
      LOG_IT( 1, (char *) "GetInputSetUpValues returns ERROR: %d",
	  Stat) ;
      (void) fprintf(stderr,
                 "\n"
                 "ERROR    ****    SMISL.Size.h\n"
                 "SIZE allocation by SMISL failed in accessing the\n"
                 "first Science Module input\n"
                 "\n") ;
      return( ErrorSize()) ;
      }

   // check Science Module Interface Secondary Layer size defines
// same
#ifndef SMISL_SIZE_ALREADY_DEFINED
#ifdef SMISL_SIZE_SAME
#define SMISL_SIZE_ALREADY_DEFINED
   DO_DEFAULT_CASE ;
#endif // SMISL_SIZE_SAME
#endif // SMISL_SIZE_ALREADY_DEFINED

// reduce by percent
#ifndef SMISL_SIZE_ALREADY_DEFINED
#ifdef SMISL_SIZE_REDUCE_BY_PERCENT
#define SMISL_SIZE_ALREADY_DEFINED
   extern double ReduceByPercent( void) ;

   SUVResponse->Size = int( (double)InputSUVR.Size * ReduceByPercent()) ;
   SUVResponse->ArraySize = InputSUVR.ArraySize ;
   SUVResponse->ArrayDescriptionSize = InputSUVR.ArrayDescriptionSize ;
#endif // SMISL_SIZE_REDUCE_BY_PERCENT
#endif // SMISL_SIZE_ALREADY_DEFINED

// set
#ifndef SMISL_SIZE_ALREADY_DEFINED
#ifdef SMISL_SIZE_SET
#define SMISL_SIZE_ALREADY_DEFINED
   extern int SetSize( SetUpValuesResponse InSUVRes, SetUpValuesResponse *OutSUVRes) ;

   SetUpValuesResponse OutSUVRes ;
   int Stat = SetSize( InputSUVR, &OutSUVRes) ;

   if( Stat < 0)
      {
      DO_DEFAULT_CASE ;
      }
   else
      {
      SUVResponse->Size = OutSUVRes.Size ;
      SUVResponse->ArraySize = OutSUVRes.ArraySize ;
      SUVResponse->ArrayDescriptionSize = OutSUVRes.ArrayDescriptionSize ;
      }
#endif // SMISL_SIZE_SET
#endif // SMISL_SIZE_ALREADY_DEFINED

// default case...
#ifndef SMISL_SIZE_ALREADY_DEFINED
#define SMISL_SIZE_ALREADY_DEFINED
   DO_DEFAULT_CASE ;
#endif // SMISL_SIZE_ALREADY_DEFINED

   // handle Standard to MultiDimensional types
   // need to allocate space for arrays and descriptions
   // too much?
   if( SUVResponse->ArraySize == 0)
      {
      SUVResponse->ArraySize = DEFAULT_ARRAY_SIZE ;
      SUVResponse->ArrayDescriptionSize = DEFAULT_ARRAY_DESC_SIZE ;
      }

   return( SUVResponse->Size) ;
   }

#endif // SMISL_SIZE_USER_DEFINED

#endif // SMISL_Size_h_def

