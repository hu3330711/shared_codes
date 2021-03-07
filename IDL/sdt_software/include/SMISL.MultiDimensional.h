
// SMISL.MutliDimensional.h

#ifndef SMISL_MultiDimensional_h_def
#define SMISL_MultiDimensional_h_def

#include <SnapOn.ANSI-C.h>
#include <SMISL.General.h>

#define SccsId_SMISL_MultiDimensional_h "@(#)SMISL.MultiDimensional.h	1.18, 05/30/04"

#define SMISL_ARRAY_LARGEST_SIZE 262144

#define SMISL_ARRAY_X_DIM 1024
#define SMISL_ARRAY_Y_DIM 128  /* this is now obsolete */
#define SMISL_ARRAY_Z_DIM 2    /* this is now obsolete */
#define SMISL_ARRAY_LARGEST_DIM SMISL_ARRAY_X_DIM

#define SMISL_DIMDESC_X_DIM SMISL_ARRAY_LARGEST_DIM
#define SMISL_DIMDESC_Y_DIM 2
#define SMISL_DIMDESC_Z_DIM 3
#define SMISL_DIMDESC_Y_MIN 0
#define SMISL_DIMDESC_Y_MAX 1
#define SMISL_DIMDESC_Z_ROWS 0
#define SMISL_DIMDESC_Z_COLS 1
#define SMISL_DIMDESC_Z_ECHS 2

struct multidimensional_struct {
   int Valid ;
   char Name[ SMISL_MAX_CHAR_LENGTH] ;
   char UnitsName[ SMISL_MAX_CHAR_LENGTH] ;
   int Year ;
   int Month ;
   int Day ;
   double Time ;
   double EndTime ;
   double Integ_T ;
   int array_type ;
   int nrows ;
   int ncols ;
   int nechs ;
   float array[ SMISL_ARRAY_LARGEST_SIZE] ;
   int use_dim_desc ;
   int ndims[3] ;
   float dim_desc[3][ SMISL_DIMDESC_X_DIM][ SMISL_DIMDESC_Y_DIM][ SMISL_DIMDESC_Z_DIM] ;
   int dim_desc_flag[3][ SMISL_DIMDESC_X_DIM][ SMISL_DIMDESC_Z_DIM] ;
   float  xwrap[3] ;
   float  ywrap[3] ;
   float  zwrap[3] ;
   } ;
typedef struct multidimensional_struct MultiDimensionalStruct ;

int GetMultiDimensionalData( char *DataName, double Time, int WhichFlag, MultiDimensionalStruct *Data) ;
int PutMultiDimensionalData( MultiDimensionalStruct *Data) ;
void PrintMultiDimensionalData( FILE *Stream, MultiDimensionalStruct *Data) ;

int MultiDimensionalDataIndex(int irow, int jcol, int kech,
        MultiDimensionalStruct *Data) ;


#endif // SMISL_MultiDimensional_h_def
