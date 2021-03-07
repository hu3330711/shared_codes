#ifndef SnapOn_CC_h
#define SnapOn_CC_h
#define SccsId_SnapOn_CC_h "@(#)SnapOn.C++.h	1.33, 02/04/07"

// SnapOn.C++.h

// *************************************************************************
// this should be pre-defined in the particular snap on module to get output
// for that snap on in a unique file.  otherwise, ALL snap on errors will be
// logged into one file called "SnapOn.log".
// *************************************************************************
#ifndef JCS_ERROR_LOG
#define JCS_ERROR_LOG "SnapOn.log"
#endif // JCS_ERROR_LOG
// *************************************************************************

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
#include <Inst.h>
#include <SDTInstance.h>
#include <SDTSourceDefs.h>
#include <SnapOn.Types.h>
#include <SnapOnXDR.h>
#include <DQHInterface.h>

#define GOOD 1
#define BAD 0


//     These symbolic constants are for the array descriptor QFlag,
//     which tells whether an array bin is reliable or unreliable.

#define  GOOD_ARRAY_BIN        1       // array bin is reliable
#define  BAD_ARRAY_BIN         0       // array bin is unreliable


class SnapOn
   {
private:
   char *Name ;
   SpaceCraftDescription *SpaceCraft ;
   ExistInstUnit *ThisQuant ; 
   LinkList *InputQuant ;
   DataBaseResponse *TheGDBRes ;
   ParamList PL ;
   float Scale ;
   float Offset ;

   int ErrorStatus ;
public:
   SnapOn( SetUpValuesRequest *GSReq) ;
   ~SnapOn() ;

// listed in SDT Science Module Document
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
   int GetInput( int input, int comp, int point, char *p) ;
   int GetInput( int input, int comp, int point, short *p) ;
   int GetInput( int input, int comp, int point, int *p) ;
   int GetInput( int input, int comp, int point, long *p) ;
   int GetInput( int input, int comp, int point, float *p) ;
   int GetInput( int input, int comp, int point, double *p) ;
   int GetInput( int input, int comp, int point, unsigned char *p) ;
   int GetInput( int input, int comp, int point, unsigned short *p) ;
   int GetInput( int input, int comp, int point, unsigned int *p) ;
   int GetInput( int input, int comp, int point, ArrayInfo *p) ;
   int GetInput( int input, int comp, int point, int nrepeats, char *p) ;
   int GetInput( int input, int comp, int point, int nrepeats, short *p) ;
   int GetInput( int input, int comp, int point, int nrepeats, int *p) ;
   int GetInput( int input, int comp, int point, int nrepeats, long *p) ;
   int GetInput( int input, int comp, int point, int nrepeats, float *p) ;
   int GetInput( int input, int comp, int point, int nrepeats, double *p) ;
   int GetInput( int input, int comp, int point, int nrepeats, unsigned char *p) ;
   int GetInput( int input, int comp, int point, int nrepeats, unsigned short *p) ;
   int GetInput( int input, int comp, int point, int nrepeats, unsigned int *p) ;
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
   int GetInputQualityBit( int input, int comp, int point) ;
   int GetInputSetUpValues( int input, SetUpValuesResponse *SUVResponse) ;
   int GetInputTotal( int input) ;
   float GetOffset( void) ;
   int GetOutput( int comp, int point, char *p) ;
   int GetOutput( int comp, int point, short *p) ;
   int GetOutput( int comp, int point, int *p) ;
   int GetOutput( int comp, int point, long *p) ;
   int GetOutput( int comp, int point, float *p) ;
   int GetOutput( int comp, int point, double *p) ;
   int GetOutput( int comp, int point, unsigned char *p) ;
   int GetOutput( int comp, int point, unsigned short *p) ;
   int GetOutput( int comp, int point, unsigned int *p) ;
   int GetOutput( int comp, int point, ArrayInfo *p) ;
   int GetOutput( int comp, int point, int nrepeats, char *p) ;
   int GetOutput( int comp, int point, int nrepeats, short *p) ;
   int GetOutput( int comp, int point, int nrepeats, int *p) ;
   int GetOutput( int comp, int point, int nrepeats, long *p) ;
   int GetOutput( int comp, int point, int nrepeats, float *p) ;
   int GetOutput( int comp, int point, int nrepeats, double *p) ;
   int GetOutput( int comp, int point, int nrepeats, unsigned char *p) ;
   int GetOutput( int comp, int point, int nrepeats, unsigned short *p) ;
   int GetOutput( int comp, int point, int nrepeats, unsigned int *p) ;
   int GetOutputArrayDescription (AddrT *array_desc_id,
      MDimArrDesc **x_array_desc, MDimArrDesc **y_array_desc,
      MDimArrDesc **z_array_desc) ;
   int GetSetUpValues( SetUpValuesResponse *SUVResponse) ;
   int GetParam( int param, char *ptr) ;
   int GetParam( int param, short *ptr) ;
   int GetParam( int param, int *ptr) ;
   int GetParam( int param, long *ptr) ;
   int GetParam( int param, float *ptr) ;
   int GetParam( int param, double *ptr) ;
   int GetParam( int param, unsigned char *ptr) ;
   int GetParam( int param, unsigned short *ptr) ;
   int GetParam( int param, unsigned int *ptr) ;
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
   int Rest( int input, int reference, int more) ;
   int SetCurrent( int current) ;
   void SetCircular( long julian) ;
   int SetOutput( int comp, int point, char p) ;
   int SetOutput( int comp, int point, short p) ;
   int SetOutput( int comp, int point, int p) ;
   int SetOutput( int comp, int point, long p) ;
   int SetOutput( int comp, int point, float p) ;
   int SetOutput( int comp, int point, double p) ;
   int SetOutput( int comp, int point, unsigned char p) ;
   int SetOutput( int comp, int point, unsigned short p) ;
   int SetOutput( int comp, int point, unsigned int p) ;
   int SetOutput( int comp, int point, ArrayInfo p) ;
   int SetOutput( int comp, int point, int nrepeats, char *p) ;
   int SetOutput( int comp, int point, int nrepeats, short *p) ;
   int SetOutput( int comp, int point, int nrepeats, int *p) ;
   int SetOutput( int comp, int point, int nrepeats, long *p) ;
   int SetOutput( int comp, int point, int nrepeats, float *p) ;
   int SetOutput( int comp, int point, int nrepeats, double *p) ;
   int SetOutput( int comp, int point, int nrepeats, unsigned char *p) ;
   int SetOutput( int comp, int point, int nrepeats, unsigned short *p) ;
   int SetOutput( int comp, int point, int nrepeats, unsigned int *p) ;

   int SetOutputArrayDescription (AddrT *array_desc_id,
      int x_ndims, int x_length, MDimVal **x_array_desc, double *x_wrap,
      int y_ndims, int y_length, MDimVal **y_array_desc, double *y_wrap,
      int z_ndims, int z_length, MDimVal **z_array_desc, double *z_wrap) ;

   int SetOutputQualityBit( int comp, int point, int value) ;
   void TransForm( double *VectorIn, double Matrix[ 3][ 3], double *VectorOut) ;

// *************************************************************************
// these must be defined in the particular snap on module, i.e. user defined.
// *************************************************************************
   int SetUpValues( SetUpValuesResponse *SUVResponse) ;
   void FillBuffers( void) ;
// *************************************************************************

// internal routines (should not be used by Science Modules)
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
   int SetDone( void) ;
   int SetIn( int in) ;

// internal routines (should not be used by Science Modules)
// added for second software layer
   char *GetInputName( int input) ;
   DataQuantityInstance *GetDQI( void) ;
   DataQuantityInstance *GetInputDQI( int input) ;


// proprietary routines (unsupported: used only for back-support)
   int GetInputComp( int input, int comp, char **ptr) ;
   int GetInputComp( int input, int comp, short **ptr) ;
   int GetInputComp( int input, int comp, int **ptr) ;
   int GetInputComp( int input, int comp, long **ptr) ;
   int GetInputComp( int input, int comp, float **ptr) ;
   int GetInputComp( int input, int comp, double **ptr) ;
   int GetInputComp( int input, int comp, StandardOneDimensionalHdr **ptr) ;
   int GetInputComp( int input, int comp, StandardTwoDimensionalHdr **ptr) ;
   int GetInputComp( int input, int comp, StandardThreeDimensionalHdr **ptr) ;
   int GetInputComp( int input, int comp, unsigned char **ptr) ;
   int GetInputComp( int input, int comp, unsigned short **ptr) ;
   int GetInputComp( int input, int comp, unsigned int **ptr) ;
   int GetComp( int comp, char **ptr) ;
   int GetComp( int comp, short **ptr) ;
   int GetComp( int comp, int **ptr) ;
   int GetComp( int comp, long **ptr) ;
   int GetComp( int comp, float **ptr) ;
   int GetComp( int comp, double **ptr) ;
   int GetComp( int comp, StandardOneDimensionalHdr **ptr) ;
   int GetComp( int comp, StandardTwoDimensionalHdr **ptr) ;
   int GetComp( int comp, StandardThreeDimensionalHdr **ptr) ;
   int GetComp( int comp, unsigned char **ptr) ;
   int GetComp( int comp, unsigned short **ptr) ;
   int GetComp( int comp, unsigned int **ptr) ;
   int GetTimeIndex( float *intime, int inlength, float time) ;
   int GetTimeIndex( double *intime, int inlength, double time) ;
   double Magnitude( double *Vector) ;
   double Magnitude( double X, double Y, double Z) ;
   } ;

void SnapOnRPCHandler( struct svc_req *rqstp, SVCXPRT *transp) ;

extern int GlobalArgc ;
extern char **GlobalArgv ;
extern SnapOn *SO ;

#endif // SnapOn_CC_h

