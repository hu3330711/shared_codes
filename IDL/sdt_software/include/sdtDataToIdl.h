#if !defined(SDTDATATOIDL_H)
#define SDTDATATOIDL_H

#if     !defined(lint)
static char sccsIDsdtDataToIdl[] = "@(#)sdtDataToIdl.h	1.29\t12/09/06,      UCB SSL";
#endif
/* -----------------------  sdtDataToIdl.h ---------------------------------
 * include file for the IDL from SDT C function lib
 */

#include <stdlib.h>
#include <SDTDescription.h>
#include <DQHInterface.h>
#include <SDTInstance.h>
#include <export.h>

#ifdef	__cplusplus
extern "C" {
#endif

/* ----------------------------------------------------------------------
 * defines
 */

#define IDL_UNDEFINED	0
#define IDL_INT8	1
#define IDL_INT16	2
#define IDL_INT32 	3
#define IDL_FLOAT	4
#define IDL_DOUBLE 	5

/* these specifies which mode we are getting data in for the
 * getDQISelection () function
 */
    
#define GET_PREV_POINT 0
#define GET_NEXT_POINT 1
#define USE_THIS_POINT 2
#define GET_TIME_SPAN_2_TIMES 3
#define GET_TIME_SPAN_1_TIME_N_SECS 4
#define GET_TIME_SPAN_1_TIME_N_PTS 5
#define GET_DATA_START_ONE_PT_MODE 6
#define GET_DATA_START_TIME_SER_N_SECS 7
#define GET_DATA_START_TIME_SER_N_PTS 8
#define GET_DATA_END_ONE_PT_MODE 9
#define GET_DATA_END_TIME_SER_N_SECS 10
#define GET_DATA_END_TIME_SER_N_PTS 11
#define GET_TIME_SPAN_2_INDICES 12
    
/* ----------------------------------------------------------------------
 * Structures:
 */

/* ----------------------------------------------------------------------
 * Structs used for time zooming/searching
 */

#ifndef vec1_typ
struct  vec1_struct
    {
    float     x ;
    float     y ;
    } ;
typedef  struct  vec1_struct  vec1_typ ;
#endif

#ifndef vec2_typ
struct  vec2_struct
    {
    float     time ;
    float     v[2] ;
    } ;
typedef  struct  vec2_struct  vec2_typ ;
#endif

#ifndef vec3_typ
struct  vec3_struct
    {
    float     time ;
    float     v[3] ;
    } ;
typedef  struct  vec3_struct  vec3_typ ;
#endif



/* ----------------------------------------------------------------------*
 * this struct holds the parameters from the DQI info call.
 * WARNING:  It must match the struct in the idl routine get_dqi_info.pro
 * exactally!
 */

typedef struct DQIInfo_str
{
    int  		sat ;             /* IDL LONG */
    IDL_STRING 		datType ;         /* IDL STRING */
    int 		Year ;            /* IDL LONG */
    int 		Mon ;             /* IDL LONG */
    int 		Day ;             /* IDL LONG */
    double 		stSec ;           /* IDL DOUBLE */
    double 		enSec ;           /* IDL DOUBLE */
    int 		dType ;           /* IDL LONG */
    int 		npts ;            /* IDL LONG */
    short 		done ;            /* IDL INT */
    int			index ;           /* IDL LONG */
    double		rtime ;           /* IDL DOUBLE */
    short		gettingIdxT ;     /* IDL INT */
} DQIInfo ;

/* this struct holds the standard argv's that are used by this library
 * (multidimensional case).
 */
   
typedef struct argsMD
{
    int * 		sat ; 
    IDL_STRING *	datType ;
    int *		year ;
    int *		mon ;
    int	*		day ;
    double *		sec ;
    double *		timeTag ;
    double *		endTime ;
    int	*		dType ;
    int	*		ncomp ;
    int	*		mode ;
    int *		retrievalMode ;
    int	*		ndims ;
    int *		dsize ;
    void * 		values ;
    int *		arrDescDepth ;
    double *		min1 ;
    double *		max1 ;
    double *		min2 ;
    double *		max2 ;
    double *		min3 ;
    double *		max3 ;
    int *		transpose ;
    int *		calibrated ;
    char *		calibratedUnits ;	
    long *		index ;
} OnePtParamsMD ;

/* this struct holds the standard argv's that are used by this library
 * (multidimensional time series case).
 */
   
typedef struct argsMDTS
{
    int * 		sat ; 
    IDL_STRING *	datType ;
    int *		stYear ;
    int *		stMon ;
    int *		stDay ;
    double *		stSec ;
    int *		enYear ;
    int *		enMon ;
    int *		enDay ;
    double *		enSec ;
    double *		timeTags ;
    double *		endTimes ;
    int *		npts ;
    int	*		dType ;
    int	*		ncomp ;
    int	*		mode ;
    int *		retrievalMode ;
    int	*		ndims ;
    int *		dsize ;
    void * 		values ;
    int *		arrDescDepth ;
    double *		min1 ;
    double *		max1 ;
    double *		min2 ;
    double *		max2 ;
    double *		min3 ;
    double *		max3 ;
    int *		transpose ;
    int *		calibrated ;
    char *		calibratedUnits ;
    int *		fstIdx ;
    int *		lstIdx ;
} TimeSeriesParamsMD ;

/* this struct holds the standard argv's that are used by this library
 * (one point of time series case).
 */

typedef struct argsTSOP
{
    int * 		sat ; 
    IDL_STRING *	datType ;
    int *		year ;
    int *		mon ;
    int	*		day ;
    double *		sec ;
    double *		timeTag ;
    int	*		dType ;
    int *		depth ;
    int	*		ncomp ;
    int	*		mode ;
    int *		retrievalMode ;
    void ** 		component ;
    int *		calibrated ;
    char *		calibratedUnits ;	
    long *		index ;
} OnePtParamsTS ;

/* this struct holds the standard argv's that are used by this library
 * (time series case).
 */

typedef struct argsTS
{
    int * 		sat ; 
    IDL_STRING *	datType ;
    int *		stYear ;
    int *		stMon ;
    int *		stDay ;
    double *		stSec ;
    int *		enYear ;
    int *		enMon ;
    int *		enDay ;
    double *		enSec ;
    double *		timeTags ;
    int	*		dType ;
    int *		depth ;
    int	*		ncomp ;
    int *		npts ;
    int	*		mode ;
    int *		retrievalMode ;
    void ** 		component ;
    int *		calibrated ;
    char *		calibratedUnits ;	
    int *		fstIdx ;
    int *		lstIdx ;
    int 		maxNComp ;
} TimeSeriesParams ;

typedef struct SDTZoomParams_struct
{
    int * 		sat ; 
    IDL_STRING *	data_name ;
    double *		zoom_stime ;
    double *		zoom_etime ;
} SDTZoomParams ;

typedef struct SDTFiducialParams_struct
{
    int * 		sat ; 
    IDL_STRING *	data_name ;
    double *		value ;
} SDTFiducialParams ;

#ifdef MULTI_RUN_SDT 

typedef struct SDTCurrentMRuns_struct
{
    int * 	njobs ;  /* The number of current SDTS (Max is 10) */
    int *       Idx0 ;   /* Indices of the SDTs */
    int *       Idx1 ;
    int *       Idx2 ;     
    int *       Idx3 ;
    int *       Idx4 ;
    int *       Idx5 ;
    int *       Idx6 ;
    int *       Idx7 ;
    int *       Idx8 ;
    int *       Idx9 ;
    char *      Desc0 ;  /* Description strings of the SDTs */
    char *      Desc1 ;
    char *      Desc2 ;
    char *      Desc3 ;
    char *      Desc4 ;
    char *      Desc5 ;
    char *      Desc6 ;
    char *      Desc7 ;
    char *      Desc8 ;
    char *      Desc9 ;
} SDTCurrentMRuns ;

#endif /* MULTI_RUN_SDT */

/* ----------------------------------------------------------------------*
 * External declarations:
 */

#ifdef MULTI_RUN_SDT 
extern int LoadBufSdtIdx ;
#endif /* MULTI_RUN_SDT */

/* ----------------------------------------------------------------------*
 * Function declarations:
 */

int  compute_time_axis_indices (DataQuantityInstance *timedqi,
    int time_cmp, int first_index, int last_index,
    int first_query, double zoom_start, double zoom_end,
    int max_points, int idx_in_comp, int *array_size,
    int *new_index, int *done_flag, int *total_data_pts) ;

int  get_zoom_indices_vec1 (vec1_typ *in_ptr, int first_index,
    int last_index, int max_pts_to_report, double start_time, double end_time,
    int *avail_pts, int *next_index) ;

int  get_zoom_indices_vec2 (vec2_typ *in_ptr, int first_index,
    int last_index, int max_pts_to_report, double start_time, double end_time,
    int *avail_pts, int *next_index) ;

int  get_zoom_indices_vec3 (vec3_typ *in_ptr, int first_index,
    int last_index, int max_pts_to_report, double start_time, double end_time,
    int *avail_pts, int *next_index) ;

int  get_zoom_indices_vecn_float (float *in_ptr, int first_index,
    int last_index, int max_pts_to_report, double start_time, double end_time,
    int *avail_pts, int *next_index, int ncomp, int tcmp) ;

int  get_zoom_indices_vecn_double (double *in_ptr, int first_index,
    int last_index, int max_pts_to_report, double start_time, double end_time,
    int *avail_pts, int *next_index, int ncomp, int tcmp) ;

int  get_zoom_indices_array_time_float (float *in_ptr, int first_index,
    int last_index, int max_pts_to_report, double start_time, double end_time,
    int *avail_pts, int *next_index) ;

int  get_zoom_indices_array_time_double (double *in_ptr, int first_index,
    int last_index, int max_pts_to_report, double start_time, double end_time,
    int *avail_pts, int *next_index) ;

int  get_zoom_indices_OneDimensionalArray (
    StandardOneDimensionalHdr *in_ptr, int first_index,
    int last_index, int max_pts_to_report, double start_time,
    double end_time, int *avail_pts, int *next_index) ;

int  get_zoom_indices_TwoDimensionalArray (
    StandardTwoDimensionalHdr *in_ptr, int first_index,
    int last_index, int max_pts_to_report, double start_time,
    double end_time, int *avail_pts, int *next_index) ;

int  get_zoom_indices_ThreeDimensionalArray (
    StandardThreeDimensionalHdr *in_ptr, int first_index,
    int last_index, int max_pts_to_report, double start_time,
    double end_time, int *avail_pts, int *next_index) ;


DataQuantityInstanceList  *QueryDataQuantityAvailability (char*, int, int, int,
							  int, double, int,
							  int, int, double,
							  int*) ;

int getTimeSeriesInstance (TimeSeriesParams p) ;
int getTimeSeriesInstanceMD (TimeSeriesParamsMD p) ;
int getDataInstanceMD (OnePtParamsMD) ;
int getDataInstanceTS (OnePtParamsTS) ;
int getZoomInterval (SDTZoomParams ZoomSpanInfo) ;
int getFiducialValue (SDTFiducialParams FiducialInfo, int ftype) ;

void printPramsMD (OnePtParamsMD) ;
void printPramsTS (OnePtParamsTS) ;

int putSDTDataToIDLData (char * IDLDataPtr, char * SDTDataPtr, int SDTDType,
			 int IDLStepSize, int SDTStepSize,
			 int * dims, int ndims, int transpose) ;

int getDQISelection (int selectionMode, int mode,
		     char * dataType, int satallite,
		     long * fstDataIdx, long * lstDataIdx,
		     int *stYear, int *stMon, int *stday, double *stSec,
		     int *enYear, int *enMon, int *enday, double *enSec,
		     DataQuantityInstance ** dqip,
		     DataQuantityInstanceList ** dqil) ;

int cleanDQISelection (DataQuantityInstance * dqip,
		       DataQuantityInstanceList * dqil) ;

double getDQITimeAtIdx (DataQuantityInstance * dqip, long idx) ;

#ifdef	__cplusplus
}                       /* extern "C" */
#endif

#endif  /* SDTDATATOIDL_H */
