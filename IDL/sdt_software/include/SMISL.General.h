
// SMISL.General.h

#ifndef SMISL_General_h_def
#define SMISL_General_h_def

#define SccsId_SMISL_General_h "@(#)SMISL.General.h	1.13, 12/03/06"


#include <SnapOn.ANSI-C.h>


// defines for Science Module Interface Secondary Layer modules
#define SMISL_NON_FATAL_ERROR		-1024

// return defines
#define SMISL_END_OF_DATA		-1
#define SMISL_SUCCESS			0

#define SMISL_MAX_CHAR_LENGTH		1000

#define SMISL_TIME_GREATERTHAN		1
#define SMISL_TIME_LESSTHAN_OR_EQUAL	2

// general utility routine definitions
int GetStandardValue( int input, int comp, int point, float *thevalue) ;
int GetStandardDoubleValue( int input, int comp, int point, double *thevalue) ;
int PutStandardValue( int comp, int point, float thevalue) ;
int PutStandardDoubleValue( int comp, int point, double thevalue) ;

void WaitForPoints( void) ;
int GetGeneralTimeIndex( int input, int comp, double time,
    int whichflag, int *point) ;
int GetCommandLineDataName( int StartArgv, int *NextArgv, char *TheDataName) ;

#endif // SMISL_General_h_def

