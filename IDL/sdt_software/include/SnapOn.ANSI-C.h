#ifndef SnapOn_C_h
#define SnapOn_C_h
#define SccsId_SnapOn_C_h "@(#)SnapOn.ANSI-C.h	1.39, 04/17/07"

/* SnapOn.ANSI-C.h */

/* **************************************************************************
 * this should be pre-defined in the particular snap on module to get output
 * for that snap on in a unique file.  otherwise, ALL snap on errors will be
 * logged into one file called "SnapOn.log".
 * **************************************************************************
 */
#ifndef JCS_ERROR_LOG
#define JCS_ERROR_LOG "SnapOn.log"
#endif /* JCS_ERROR_LOG */
/* **************************************************************************/

#include <stdlib.h>
#include <unistd.h>
#include <rpc/rpc.h>
#include <rpc/types.h>
#include <rpc/pmap_clnt.h>
#include <rpc/svc.h>
#include <math.h>
#include <stddef.h>

#ifdef SOLARIS
#include <rpc/svc_soc.h>
#endif

#include <Error1.h>
#include <SDTInstance.h>
#include <SDTSourceDefs.h>
#include <SnapOn.Types.h>
#include <DQHInterface.h>

#include "SnapOnXDR.h"

#define GOOD 1
#define BAD 0


/*
 *     These symbolic constants are for the array descriptor QFlag,
 *     which tells whether an array bin is reliable or unreliable.
 */
#define  GOOD_ARRAY_BIN        1       /* array bin is reliable */
#define  BAD_ARRAY_BIN         0       /* array bin is unreliable */


#define TIME_NORMAL 0
#define TIME_LONG 1

   struct Inst_struct
      {
      DataQuantityInstanceList *DQIList ;
      DQHRequest *DQHR ;
      } ;
   typedef struct Inst_struct Inst ;

   struct SnapOn_struct
      {
      char *Name ;
      SpaceCraftDescription *SpaceCraft ;
      Inst *ThisQuant ; 
      int InputQuantCount ;
      Inst *InputQuant ;
      ParamList PL ;
      float Scale ;
      float Offset ;

      DataBaseResponse *DBRes ;

      int ErrorStatus ;
      } ;
   typedef struct SnapOn_struct SnapOn ;

   void SnapOnSnapOn( SetUpValuesRequest *GSReq) ;
   void USnapOnSnapOn() ;

/* listed in SDT Science Module Document */
   void BlockUntilInputsDone( void) ;
   int ComputeArrayDescStorage(int XNBins, int XNDims, int XNDesc,
           int YNBins, int YNDims, int YNDesc,
           int ZNBins, int ZNDims, int ZNDesc) ;
   void CrossProduct( double *Vector1, double *Vector2, double *Vector3) ;
   double DotProduct( double *Vector1, double *Vector2) ;
   void Error( void) ;
   int ErrorSize( void) ;
   int GetCircular( void) ;
   int GetCompCount( void) ;
   int GetCompType( int comp) ;
   int GetCompNRepeats( int comp) ;
   int GetCurrent( void) ;
   int GetDone( void) ;
   int GetInputChar( int input, int comp, int point, char *p) ;
   int GetInputShort( int input, int comp, int point, short *p) ;
   int GetInputInt( int input, int comp, int point, int *p) ;
   int GetInputLong( int input, int comp, int point, long *p) ;
   int GetInputFloat( int input, int comp, int point, float *p) ;
   int GetInputDouble( int input, int comp, int point, double *p) ;
   int GetInputUChar( int input, int comp, int point, unsigned char *p) ;
   int GetInputUShort( int input, int comp, int point, unsigned short *p) ;
   int GetInputUInt( int input, int comp, int point, unsigned int *p) ;
   int GetInputArray( int input, int comp, int point, ArrayInfo *p) ;
   int GetInputNChar( int input, int comp, int point, int nrepeats, char *p) ;
   int GetInputNShort( int input, int comp, int point, int nrepeats, short *p) ;
   int GetInputNInt( int input, int comp, int point, int nrepeats, int *p) ;
   int GetInputNLong( int input, int comp, int point, int nrepeats, long *p) ;
   int GetInputNFloat( int input, int comp, int point, int nrepeats, float *p) ;
   int GetInputNDouble( int input, int comp, int point, int nrepeats, double *p) ;
   int GetInputNUChar( int input, int comp, int point, int nrepeats, unsigned char *p) ;
   int GetInputNUShort( int input, int comp, int point, int nrepeats, unsigned short *p) ;
   int GetInputNUInt( int input, int comp, int point, int nrepeats, unsigned int *p) ;
   int GetInputArrayDescription( int input, AddrT *array_desc_id,
       MDimArrDesc **x_array_desc, MDimArrDesc **y_array_desc,
       MDimArrDesc **z_array_desc) ;
   int GetInputCircular( int input) ;
   int GetInputCompCount( int input) ;
   int GetInputCompType( int input, int comp) ;
   int GetInputCompNRepeats( int input, int comp) ;
   int GetInputCount( void) ;
   int GetInputCurrent( int input) ;
   int GetInputDone( int input) ;
   int GetInputUseQualityBits( int input) ;
   int GetInputQualityBit( int input, int comp, int point) ;
   int GetInputSetUpValues( int input, SetUpValuesResponse *SUVResponse) ;
   int GetInputTotal( int input) ;
   float GetOffset( void) ;
   int GetOutputChar( int comp, int point, char *p) ;
   int GetOutputShort( int comp, int point, short *p) ;
   int GetOutputInt( int comp, int point, int *p) ;
   int GetOutputLong( int comp, int point, long *p) ;
   int GetOutputFloat( int comp, int point, float *p) ;
   int GetOutputDouble( int comp, int point, double *p) ;
   int GetOutputUChar( int comp, int point, unsigned char *p) ;
   int GetOutputUShort( int comp, int point, unsigned short *p) ;
   int GetOutputUInt( int comp, int point, unsigned int *p) ;
   int GetOutputArray( int comp, int point, ArrayInfo *p) ;
   int GetOutputNChar( int comp, int point, int nrepeats, char *p) ;
   int GetOutputNShort( int comp, int point, int nrepeats, short *p) ;
   int GetOutputNInt( int comp, int point, int nrepeats, int *p) ;
   int GetOutputNLong( int comp, int point, int nrepeats, long *p) ;
   int GetOutputNFloat( int comp, int point, int nrepeats, float *p) ;
   int GetOutputNDouble( int comp, int point, int nrepeats, double *p) ;
   int GetOutputNUChar( int comp, int point, int nrepeats, unsigned char *p) ;
   int GetOutputNUShort( int comp, int point, int nrepeats, unsigned short *p) ;
   int GetOutputNUInt( int comp, int point, int nrepeats, unsigned int *p) ;
   int GetOutputArrayDescription (AddrT *array_desc_id,
       MDimArrDesc **x_array_desc, MDimArrDesc **y_array_desc,
       MDimArrDesc **z_array_desc) ;
   int GetSetUpValues( SetUpValuesResponse *SUVResponse) ;
   int GetParamChar( int param, char *ptr) ;
   int GetParamAlgorithm( int param, short *ptr) ;
   int GetParamShort( int param, short *ptr) ;
   int GetParamInt( int param, int *ptr) ;
   int GetParamLong( int param, long *ptr) ;
   int GetParamFloat( int param, float *ptr) ;
   int GetParamDouble( int param, double *ptr) ;
   int GetParamUChar( int param, unsigned char *ptr) ;
   int GetParamUShort( int param, unsigned short *ptr) ;
   int GetParamUInt( int param, unsigned int *ptr) ;
   int GetParamCount( void) ;
   int GetParamType( int param) ;
   float GetScale( void) ;
   int GetTimeIndex( int input, int time_comp, int inlength, float time) ;
   TimeSpanDescription *GetTimeSpan( void) ;
   DataBaseResponse *GetDataBaseResponse( void) ;
   int GetTotal( void) ;
   double LinInterp( float xlo, double ylo, float xhi, double yhi, float x) ;
   int Minimum( int A, int B) ;
   int Maximum( int A, int B) ;
   int Rest( int input, int timecomp, float time) ;
   int RestCount( int input, int reference, int more) ;
   int SetCurrent( int current) ;
   void SetCircular( long julian) ;
   int SetOutputChar( int comp, int point, char p) ;
   int SetOutputShort( int comp, int point, short p) ;
   int SetOutputInt( int comp, int point, int p) ;
   int SetOutputLong( int comp, int point, long p) ;
   int SetOutputFloat( int comp, int point, float p) ;
   int SetOutputDouble( int comp, int point, double p) ;
   int SetOutputUChar( int comp, int point, unsigned char p) ;
   int SetOutputUShort( int comp, int point, unsigned short p) ;
   int SetOutputUInt( int comp, int point, unsigned int p) ;
   int SetOutputArray( int comp, int point, ArrayInfo p) ;
   int SetOutputNChar( int comp, int point, int nrepeats, char *p) ;
   int SetOutputNShort( int comp, int point, int nrepeats, short *p) ;
   int SetOutputNInt( int comp, int point, int nrepeats, int *p) ;
   int SetOutputNLong( int comp, int point, int nrepeats, long *p) ;
   int SetOutputNFloat( int comp, int point, int nrepeats, float *p) ;
   int SetOutputNDouble( int comp, int point, int nrepeats, double *p) ;
   int SetOutputNUChar( int comp, int point, int nrepeats, unsigned char *p) ;
   int SetOutputNUShort( int comp, int point, int nrepeats, unsigned short *p) ;
   int SetOutputNUInt( int comp, int point, int nrepeats, unsigned int *p) ;

   int SetOutputArrayDescription( AddrT *array_desc_id,
     int x_ndims, int x_length, MDimVal **x_array_desc, double *x_wrap,
     int y_ndims, int y_length, MDimVal **y_array_desc, double *y_wrap,
     int z_ndims, int z_length, MDimVal **z_array_desc, double *z_wrap) ;

   int GetOutputUseQualityBits( void) ;
   int SetOutputQualityBit( int comp, int point, int value) ;
   char *TimeToString( int type, double time);
   void TransForm( double *VectorIn, double Matrix[ 3][ 3], double *VectorOut) ;

/* **************************************************************************
 * these must be defined in the particular snap on module, i.e. user defined.
 * **************************************************************************/
   int SnapOnSetUpValues( SetUpValuesResponse *SUVResponse) ;
   void SnapOnFillBuffers( void) ;
/* **************************************************************************/

/* internal routines (should not be used by Science Modules) */
   int CreateThis( void) ;
   int GetArraySize( void) ;
   int GetArrDescExists( void) ;
   int GetError( void) ;
   int GetInputArraySize( int input) ;
   int GetInputArrDescExists( int input) ;
   char *GetName( void) ;
   int GetRealCompType( int comp) ;
   int GetRealInputCompType( int input, int comp) ;
   SpaceCraftDescription *GetSpaceCraft( void) ;
   int SetIn( int in) ;
   int SetDone( void) ;

/* internal routines (should not be used by Science Modules) */
/* added for second layer */
   char *GetInputName( int input) ;
   DataQuantityInstance *GetDQI( void) ;
   DataQuantityInstance *GetInputDQI( int input) ;

/* proprietary routines (unsupported: used only for back-support) */
   int GetInputCompChar( int input, int comp, char **ptr) ;
   int GetInputCompShort( int input, int comp, short **ptr) ;
   int GetInputCompInt( int input, int comp, int **ptr) ;
   int GetInputCompLong( int input, int comp, long **ptr) ;
   int GetInputCompFloat( int input, int comp, float **ptr) ;
   int GetInputCompDouble( int input, int comp, double **ptr) ;
   int GetInputCompStandardOneDimensionalHdr( int input, int comp, StandardOneDimensionalHdr **ptr) ;
   int GetInputCompStandardTwoDimensionalHdr( int input, int comp, StandardTwoDimensionalHdr **ptr) ;
   int GetInputCompStandardThreeDimensionalHdr( int input, int comp, StandardThreeDimensionalHdr **ptr) ;
   int GetInputCompUChar( int input, int comp, unsigned char **ptr) ;
   int GetInputCompUShort( int input, int comp, unsigned short **ptr) ;
   int GetInputCompUInt( int input, int comp, unsigned int **ptr) ;
   int GetCompChar( int comp, char **ptr) ;
   int GetCompShort( int comp, short **ptr) ;
   int GetCompInt( int comp, int **ptr) ;
   int GetCompLong( int comp, long **ptr) ;
   int GetCompFloat( int comp, float **ptr) ;
   int GetCompDouble( int comp, double **ptr) ;
   int GetCompStandardOneDimensionalHdr( int comp, StandardOneDimensionalHdr **ptr) ;
   int GetCompStandardTwoDimensionalHdr( int comp, StandardTwoDimensionalHdr **ptr) ;
   int GetCompStandardThreeDimensionalHdr( int comp, StandardThreeDimensionalHdr **ptr) ;
   int GetCompUChar( int comp, unsigned char **ptr) ;
   int GetCompUShort( int comp, unsigned short **ptr) ;
   int GetCompUInt( int comp, unsigned int **ptr) ;
   int GetTimeIndex( float *intime, int inlength, float time) ;
   int GetTimeIndex( double *intime, int inlength, double time) ;
   double Magnitude1( double *Vector) ;
   double Magnitude2( double X, double Y, double Z) ;


   void SnapOnRPCHandler( struct svc_req *rqstp, SVCXPRT *transp) ;

   extern int GlobalArgc ;
   extern char **GlobalArgv ;
   extern SnapOn *SO ;

#endif /* SnapOn_C_h */

