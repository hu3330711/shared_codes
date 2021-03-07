/* SDTDescription.h */

#ifndef SDTDESCRIPTION_H
#define SDTDESCRIPTION_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* SSL Data Tools Decriptions (Widgits) Declaration File: */

/* SCCS ID string: */
#define SccsId_SDTDescription_h "@(#)SDTDescription.h	1.69, 04/17/07"

#include <stdio.h>
#include <rpc/types.h>
#include <SDTType.h>
#include <SDTSpacecraft.h>

/* Types of quantities: */
#define  QUANTITY_TYPE_UNDEFINED	-100
#define  RAW_BYTE_RECORD_DATA		-1
#define  TIME_BASED_DATA		0
#define  FREQUENCY_BASED_DATA		1
#define  X_VRS_Y_BASED_DATA		2
#define  BURST_HEADER_DATA		3
#define  BIAS_SWEEP_HEADER_DATA		4

/* Types of plots: */
#define BLOCK			     0 
#define TIME_SERIES		     1
#define MULTILINE_TIMESERIES	     2
#define LOG_VS_TIME_SERIES	     3
#define X_VS_Y			     4
#define X_VS_LOGY		     5 
#define LOG_VS_LOG	     	     6
#define SPECTRAL		     7
#define CONTOUR_2D	     	     8
#define LEGEND_TIME_SERIES	     9 
#define TEXT_PANEL_PLOT		     10 
#define MODE			     11 
#define X_VS_Y_TRACE 		     12
#define X_VS_Y_SPECTRAL_VSLICE       13
#define SPECTRAL_HSLICE_TIME_SERIES  14

/* Multi-dimensional array type quantities: */
#define STANDARD_ONE_DIMENSIONAL     8 
#define STANDARD_TWO_DIMENSIONAL     9 
#define STANDARD_THREE_DIMENSIONAL   10 

/* These indicate the different ways of storing quantities: */
#define  STORE_AS_SEPARATE_ARRAYS   0
#define  STORE_AS_C_STRUCTURE       1

/* This is whether to plot the panel with lines or with points,
 * or both (both implemented 10/31/2001):
 */
#define GRAPH_MODE_LINE           1
#define GRAPH_MODE_PTS            2
#define GRAPH_MODE_LINE_AND_PTS   3

/* This is whether to plot the panel with error bars and what type */
#define NO_ERROR_BARS	0
#define DATA_ERROR_BARS	1
#define ZERO_ERROR_BARS	2

/* Types of scales: */
#define SDT_TIME_SCALE    -1
#define SDT_LINEAR_SCALE   0
#define SDT_LOG_SCALE      1
#define SDT_HEX_SCALE      2


/* Leading tag for Dqd SubCom strings (from SCM's): */
#define  DQD_SUBCOM_LABEL "DqdSubCom:"

#define  SDT_DM_OV        "SDT_DM_OV"

/* If this string is in the argument list when invoking a
 * decommutator, it indicates that this is a re-invocation,
 * i.e. not the first invocation of the decommutator, in an
 * SDT session.  This is information can be used by the
 * decommutator as it wishes.
 * Added 2005/06/16 - re-invocation allows SDT, for the purposes 
 * of de-fragmenting memory, to occasionally restart individual
 * decommutators, at a time when nothing is happening.  This
 * is particularly useful in "sdt_batch" where, after a certain
 * number of processed TimeSpans, the decommutator can be
 * reinvoked.  This allows for an unlimited number of TimeSpans
 * that can be run in a single batch run.
 */
#define  SDT_REINVOCATION_ARG    "SDT_REINVOKE_DECOM"

/* Maximum number of array descriptions for the bins in a single
 * dimension in a 1, 2, or 3 dimensional quantity:
 */
#define MDIM_ADESC_MAX	3

/* These are the types of data allowed in the ComponentDescription */

/* Note that "DQ_TIMESPAN" was added 98/05/18 in order to facilitate
 * better handling of data set changes when dealing with on-the-fly
 * science modules (e.g. PwrSpectra).
 */
/* Note that "DQ_OPERATIONAL" was added 98/09/14 in order to distinguish
 * between parameters that are changeable via the user interface in SDT
 * (those that are not "literal") and those that need to be set once, at
 * the beginning of on-the-fly science modules (e.g. PwrSpectra), but
 * are thereafter not to be changed for that particular plot.   If you
 * want to change one of these, you have to create, on-the-fly, the
 * plot again (you may or may not delete the first on-the-fly plot -
 * they both can exist).  These types of parameters go into the plot
 * config files, and are used to initialize the corresponding plots'
 * quantities when the config is read into SDT or the batch version,
 * but are not put into the Parameter UI tool for modification by the
 * user.
 */
#define DQ_UNDEFINED	999
#define DQ_LITERAL	998
#define DQ_TIMESPAN     997
#define DQ_OPERATIONAL  996
#define DQ_STORE_CHAR	0
#define DQ_STORE_INT16	1
#define DQ_STORE_INT32	2
#define DQ_STORE_INT64	3
#define DQ_STORE_FLOAT	4
#define DQ_STORE_DOUBLE	5
#define DQ_ONE_DIMENSIONAL_HDR     8 
#define DQ_TWO_DIMENSIONAL_HDR     9 
#define DQ_THREE_DIMENSIONAL_HDR   10 
#define DQ_STORE_UCHAR	21
#define DQ_STORE_UINT16	22
#define DQ_STORE_UINT32	23
#define DQ_STORE_ADDRT  24

/* Added "DQ_STORE_ALGORITHM" on 2007/03/11 to allow for alternate
 * methods of computing DQDs.   This feature first became useful
 * when CAA CLUSTER B-GSE data became available - giving us two
 * ways of computing E/B quantities for CLUSTER (the older method
 * required starting with the raw FGM data and working up).
 * NOTE that a "DQ_STORE_ALGORITHM" paramater will be stored as
 * as "int32".  It is an index into the list of possible methods
 * for computing a DQD.
 */
#define DQ_STORE_ALGORITHM         33

/*#define DQ_MDIM_DESCRIPTION 30*/
#define DQ_MDIM_DESCRIPTION DQ_STORE_UCHAR 

/* The maximum number of dimensions we can handle: */
#define SDT_MAX_DIMENSIONS   3 

/* State constants for error bar output: */
/* Values reassigned 2006/01/10, to match the ERROR_BAR values */
#define SDT_EBAR_NONE            0 
#define SDT_EBAR_DATA_CENTERED   1 
#define SDT_EBAR_BAR_CENTERED    2 

/* State constants for multi-line output: */
#define SDT_MLSTYLE_COLOR        0 
#define SDT_MLSTYLE_SYMBOL       1 

/* State constants for contour output: */
#define SDT_CONTOUR_STYLE_COLOR  0 
#define SDT_CONTOUR_STYLE_LINE   1 

/* State constants for spectrogram output: */
#define SDT_SPECTRA_STYLE_NONE   0
#define SDT_SPECTRA_STYLE_EFLUX  1000 

/* State constants for line style: */
#define SDT_LFONT_SOLID          0 
#define SDT_LFONT_DASHED         1 
#define SDT_LFONT_PHANTOM        2 
#define SDT_LFONT_CENTERLINE     3 

/* State constants for line color: */
/* NOTE:   The order of colors was changed, October 7, 2005, to
 * correspond with the CLUSTER convention:
 *
 *   SC1 = WHITE
 *   SC2 = RED
 *   SC3 = GREEN
 *   SC4 = BLUE
 */
#define SDT_LCOLOR_WHITE         0 
#define SDT_LCOLOR_RED           1 
#define SDT_LCOLOR_GREEN         2 
#define SDT_LCOLOR_BLUE          3 
#define SDT_LCOLOR_ORANGE        4 
#define SDT_LCOLOR_PURPLE        5 
#define SDT_LCOLOR_YELLOW        6 

/* This is the number of available "line" plot colors, as
 * per the WHITE, etc. above:
 */
#define SDT_NMB_LCOLORS          7 

/* The maximum number of packet pathnames in a string for separation by
 * "SDTExtractFileNamesFromString":
 *
 * OBSOLETE as of 2003/04/07 - we no longer have this limitation in
 * order that very long timespans (one or two months) be allowed for
 * viewing FAST HK data.
 *
 */
#define SDT_MAX_FILENAMES_PER_STRING  50

#define SDT_NOT_MONOTONIC             0
#define SDT_MONOTONIC_UP              1
#define SDT_MONOTONIC_DOWN            2

/* The maximum number of spacecraft to allow for a multi-spacecraft
 * mission.
 */
#define MAX_MSCRAFT_IN_MISSION   20

/* For DQD, PQD templates - the max size of the Spacecraft ID
 * part of relevant strings.
 */
#define MAX_MSCRAFT_ID_LEN       12

/* Added 2007/03/14.   The maximum number of "algorthms" from
 * which a DQD can be computed.
 */
#define MAX_ALGORITHMS_FOR_DQD   10

/* -------------------- Array Descriptions ------------------------- */
/*
 * Here is a brief description of the memory storage of multidimensional
 * array descriptions:
 *
 *   The main unit of an MDim array description is the MDimArrDesc
 *   structure which is always aligned on 16-byte boundaries.  It is
 *   stored in "MDIM_ARR_DESC_HSIZ" bytes in memory and contains the
 *   structure defined below.  It describes 1, 2, or 3 "MDimVal" arrays
 *   which immediately follow it in memory.   Note that the number of
 *   following "MDimVal" arrays corresponds to the number of sub-dimensions
 *   which are required to describe the bins.  This is usually ONE, but
 *   for solid angles, as in FAST Teams, there are TWO defining sub-
 *   dimensions:  "phi" and "theta".  As of 96/05/05, there are no
 *   quantities whose bins require 3 sub-dimensions, although provision
 *   is made for it in the "sdtlib" interface.   NOTE:  Because of byte-
 *   usage concerns, the SMISL interface only allows for two defining
 *   sub-dimensions --- this restriction was removed on 1996/08/08, so
 *   that now the SMISL interface allows also up to 3 sub-dimensions.
 *
 *   The first array of "MDimVal"'s starts at "address + MDIM_ARR_DESC_HSIZ"
 *   after the first byte of the "MDimArrDesc", plus whatever number of
 *   bytes < 16 that are required to get the start of the array aligned
 *   on 16-bytes.  In the case of the FAST Teams quantities, this is the
 *   "phi" sub-dimension.
 *
 *   If a second array of "MDimVal"'s is required (as in FAST Teams), then
 *   that array starts immediately after the end of the first array of
 *   "MDimVal"'s, plus whatever number of bytes < 16 that are required
 *   to get the start of the array aligned on 16-bytes.  In the case of
 *   the FAST Teams quantities, this is the "theta" sub-dimension.
 *
 *   If a third array of "MDimVal"'s is required (no cases as of May 96),
 *   then that array starts immediately after the end of the second array
 *   of "MDimVal"'s, plus whatever number of bytes < 16 that are required
 *   to get the start of the array aligned on 16-bytes.
 *
 */

/* This is the number of bytes from the start of the header of an
 * "MDimArrDesc" and the first "MDimVal" in that descriptor in memory.
 * It is IMPORTANT that this be a multiple of 16 AND be larger than
 * (or equal to) sizeof(MDimArrDesc):
 */
#define MDIM_ARR_DESC_HSIZ            80

/* Definition of a Multi-dimensional bin min-max value structure: */
struct MDimVal_struct
    {
    double  mmin ;
    double  mmax ;
    int     QFlag ;   /* 0 -> bin is not reliable,  1 -> bin is okay */
    } ;
typedef struct MDimVal_struct  MDimVal ;

/* Definition of a table containing bin limits information for a
 * Multi-dimensional data quantity.  As of 94/11/06, the actual
 * arrays of "MDimVal"'s start in the first byte following this
 * structure.
 */
struct MDimArrDesc_struct
    {
    int      NBins ;          /* The number of bins in this array
			       * description.
			       */
    AddrT    NextDescOffset ; /* Offset, in bytes, from the start
			       * of the DQI array description component
			       * to the start of the following
			       * MDimArrDesc.  This is used as an
			       * internal aid and will probably never
			       * be used by anything outside of the
			       * source for "libsdtlib.a".
			       */
    int      NDims ;          /* The number of sub-dimensions of min,
			       * maxes which describe each bin.  There
			       * will be this many "MDimVal" arrays
			       * following the contents of this structure
			       * (each with "NBins" elements).  The min,
			       * max of subdimension "j" for bin "i" will
			       * be the min,max fields of the "i"'th
			       * element of "MDimVal" array "j".
			       *
			       * Note that "NDims" must be 1, 2, ... ,
			       * or "MDIM_ADESC_MAX".
			       */

    /* For the following fields, there is one per each "NDims" sub-
     * dimensions.  They help describe the "MDimVal" arrays.
     */

    int      Monotonicity[MDIM_ADESC_MAX] ;
			      /* Flag indicating whether or not the
			       * MDimVals in the description are
			       *
			       *    SDT_MONOTONIC_UP
			       *    SDT_MONOTONIC_DOWN
			       *    SDT_NOT_MONOTONIC
			       */

    int      Contiguous[MDIM_ADESC_MAX] ;
			      /* Only meaningful if the desciption
			       * is monotonic (either up or down):
			       *
			       *   0 -> not contiguous
			       *   1 -> contiguous
			       */

    int      WrapFlag[MDIM_ADESC_MAX] ;
			      /* Flag indicating if the MDimVals
			       * in the description wrap (for instance,
			       * angles wrap to zero at 360.0 degrees):
			       *
			       *  0 -> linear (no wrap to zero)
			       *  1 -> wrap to zero
			       *
			       * If this flag is on, then the
			       * field "WrapValue" indicates the
			       * value that wraps to zero (e.g 360.0
			       * in the case of angles):
			       */

    int      WrapIdx[MDIM_ADESC_MAX] ;
			      /* Only meaningful if "WrapFlag" is 1:
			       * Indicates which index of the array
			       * of MDimVals is where wrap occurs.
			       */

    float    WrapValue[MDIM_ADESC_MAX] ;
			      /* Only meaningful if "WrapFlag" is 1:
			       * the value that wraps to zero.
			       */
    } ;
typedef struct MDimArrDesc_struct  MDimArrDesc ;


/* Definition of a multi-dimension array header with dimension: 1 */
/* This is stored in the 0'th component of the DQI for a
 * "STANDARD_ONE_DIMENSIONAL" quantity (as opposed to a single "time"
 * value for TimeSeries data):
 */
struct StandardOneDimensionalHdr_str
    {
    /* The number of elts in the corresponding one-dim array: */
    unsigned short    nrows ;

    /* The time (secs UT of the current day) at the start of the array: */
    double            time ;

    /* The time delta (secs UT of the current day) between consecutive
     * points in the corresponding array:
     */
    double            delta_t ;

    /* Indexes to the first data element (element:  array[0]): */
    int               d_index ;

    /* Byte offset, into the MDimArrDesc component of the corresponding
     * Data Quantity, of the "MDimArrDesc" correponding to this
     * Multi-dimensional array.  If 0 or less, then there is NO such
     * corresponding "MDimArrDesc" (in which case the bin limits are
     * treated as just the array indices by SDT).
     *
     * NOTE:  When "arr_dsc" > 0, it is ALWAYS assumed that the
     *        corresponding "MDimArrDesc" pointed to by "arr_dsc" has
     *        one associated MDimVal array and contains "nrows" elements.
     *
     *        It is up to the source sofware generating this data
     *        to insure this.  SDT will not attempt to protect against
     *        a mismatch which could cause crashes, etc.
     */
    AddrT            arr_dsc[1] ;
    } ;
typedef struct  StandardOneDimensionalHdr_str  StandardOneDimensionalHdr ;


/* Definition of a multi-dimension array header with dimension: 2 */
/* This is stored in the 0'th component of the DQI for a
 * "STANDARD_TWO_DIMENSIONAL" quantity (as opposed to a single "time"
 * value for TimeSeries data):
 */
struct StandardTwoDimensionalHdr_str
    {
    /* The number of rows, cols in the corresponding two-dim array: */
    unsigned short    nrows ;
    unsigned short    ncols ;

    /* The time (secs UT of the current day) at the start of the array: */
    double            time ;

    /* The time delta (secs UT of the current day) between consecutive
     * rows in the corresponding array:
     */
    double            delta_t ;

    /* Indexes to the first data element (element:  array[0]): */
    int               d_index ;

    /* Byte offsets, into the MDimArrDesc component of the corresponding
     * Data Quantity, of the "MDimArrDesc's" correponding to this
     * Multi-dimensional array.  If 0 or less, then there is NO such
     * corresponding "MDimArrDesc" (in which case the bin limits are
     * treated as just the array indices by SDT).
     *
     * NOTE:  When "arr_dsc0,1" > 0, it is ALWAYS assumed that the
     *        corresponding "MDimArrDesc"'s pointed to by "arr_dsc0,1"
     *        each has one associated MDimVal array as follows:
     *
     *           arr_dsc0 pts to an MDimArrDesc with "nrows" elts:
     *           arr_dsc1 pts to an MDimArrDesc with "ncols" elts:
     *
     *        It is up to the source sofware generating this data
     *        to insure this.  SDT will not attempt to protect against
     *        a mismatch which could cause crashes, etc.
     */
    AddrT            arr_dsc[2] ;
    } ;
typedef struct  StandardTwoDimensionalHdr_str  StandardTwoDimensionalHdr ;


/* Definition of a multi-dimension array header with dimension: 3 */
/* This is stored in the 0'th component of the DQI for a
 * "STANDARD_THREE_DIMENSIONAL" quantity (as opposed to a single "time"
 * value for TimeSeries data):
 */
struct StandardThreeDimensionalHdr_str
    {
    /* The number of rows, cols, echelons in the corresponding
     * three-dim array:
     */
    unsigned short    nrows ;
    unsigned short    ncols ;
    unsigned short    nechs ;

    /* The time (secs UT of the current day) at the start of the array: */
    double            time ;

    /* The time delta (secs UT of the current day) between consecutive
     * rows in the corresponding array:
     */
    double            delta_t ;

    /* Indexes to the first data element (element:  array[0]): */
    int               d_index ;

    /* Byte offset, into the MDimArrDesc component of the corresponding
     * Data Quantity, of the "MDimArrDesc" correponding to this
     * Multi-dimensional array.  If 0 or less, then there is NO such
     * corresponding "MDimArrDesc" (in which case the bin limits are
     * treated as just the array indices by SDT).
     *
     * NOTE:  When "arr_dsc0,1,2" > 0, it is ALWAYS assumed that the
     *        corresponding "MDimArrDesc"'s pointed to by "arr_dsc0,1,2"
     *        each has one associated MDimVal array as follows:
     *
     *           arr_dsc0 pts to an MDimArrDesc with "nrows" elts:
     *           arr_dsc1 pts to an MDimArrDesc with "ncols" elts:
     *           arr_dsc2 pts to an MDimArrDesc with "nechs" elts:
     *
     *        It is up to the source sofware generating this data
     *        to insure this.  SDT will not attempt to protect against
     *        a mismatch which could cause crashes, etc.
     */
    AddrT            arr_dsc[3] ;
    } ;
typedef struct StandardThreeDimensionalHdr_str StandardThreeDimensionalHdr ;


/* Note!  NEVER try to send a VoidList through an RPC call.  It just
 *        doesn't make sense.  The VoidList is for local process use only!
 */
struct VoidList_struct {
	int nqtys;
	struct {
		u_int qty_len;
		void **qty_val;
	} qty;
    } ;
typedef  struct  VoidList_struct  VoidList ;

struct IntList_struct
    {
	int nqtys;
	struct {
		u_int qty_len;
		int *qty_val;
	} qty;
    } ;
typedef  struct  IntList_struct  IntList ;

struct Int16List_struct
    {
	int nqtys;
	struct {
		u_int qty_len;
		int16 *qty_val;
	} qty;
    } ;
typedef  struct  Int16List_struct  Int16List ;

struct Int32List_struct
    {
	int nqtys;
	struct {
		u_int qty_len;
		int32 *qty_val;
	} qty;
    } ;
typedef  struct  Int32List_struct  Int32List ;

struct AddrTList_struct
    {
	int nqtys;
	struct {
		u_int qty_len;
		AddrT *qty_val;
	} qty;
    } ;
typedef  struct  AddrTList_struct  AddrTList ;

struct FloatList_struct
    {
	int nqtys;
	struct {
		u_int qty_len;
		float *qty_val;
	} qty;
    } ;
typedef  struct  FloatList_struct  FloatList ;

struct DoubleList_struct {
	int nqtys;
	struct {
		u_int qty_len;
		double *qty_val;
	} qty;
};
typedef struct DoubleList_struct  DoubleList ;

#ifdef SUNOS4

/* NOTE!  There is a problem compiling in SUNOS 4 when using the
 *    typedef struct String_struct String
 * line (which is the correct syntax).  Therefore we continue to use
 * the old, albeit, wrong typedef line for C++ on SUNOS 4.  Note that
 * this would never compile with ANSI-C:
 */

struct String_struct {
	char *str;
};
typedef String_struct String ;

#else

struct String_struct {
	char *str;
};
typedef struct String_struct String ;

#endif

struct StringList_struct {
	int nqtys;
	struct {
		u_int qty_len;
		String *qty_val;
	} qty;
};
typedef struct StringList_struct StringList ;

struct SpaceCraftDescription_struct
   {
   int16 Id ;                    /* Spacecraft Id */
   int16 SubId ;                 /* Spacecraft Sub-Id */
   } ;
typedef struct SpaceCraftDescription_struct SpaceCraftDescription ;

struct TimeSpanDescription_struct
   {
   int      Year ;               /* Year of start time. */
   int      DayOfYear ;          /* Day-of-year of start (Jan 1 is 1). */
   int      Month ;              /* Month of start time (Jan is 1). */
   int      DayOfMonth ;         /* Day in month of start time (base: 1). */
   long     StartJulianDay ;     /* Julian day of the start time. */
   double   StartTime ;          /* UT time of day, in seconds. */
   long     EndJulianDay ;       /* Julian day of the end time. */
   double   EndTime ;            /* UT time of day, in seconds. */
   } ;
typedef struct TimeSpanDescription_struct TimeSpanDescription ;

struct DataQuantityIdentifier_struct
    {
    char   *QuantityName ;  /* The name of the Quantity (DQD.Name): */
    int32  SourceId ;       /* Source the Quantity comes from: */
    } ;
typedef  struct  DataQuantityIdentifier_struct  DataQuantityIdentifier ;

struct DataQuantityIdentifierList_struct {
	int nqtys;
	struct {
		u_int qty_len;
		DataQuantityIdentifier *qty_val;
	} qty;
};
typedef struct DataQuantityIdentifierList_struct DataQuantityIdentifierList ;

struct ComponentSubDimension_struct
    {
    /* If the component is multi-dimensional, then the following
     * flag indicates the following:
     *
     *    0:   The corresponding dimension is "continuous" (e.g
     *         ranges are energies, etc.) and summing over a
     *         number of bins makes sense.
     *
     *    1:   The corresponding dimension is "discrete" (e.g
     *         electron and proton bins) and summing doesn't make sense.
     */
    int      DiscreteBinsOnly ;

    /* Both of these lists will have the same number of elements.  The
     * number of elements in this list will be one except in cases where
     * a multi-dimensional data quantity has a dimension that has bins
     * which have 2 or 3 dimension array descriptions - we only allow up
     * to 3.  An example would be the FAST Teams 3-D quantities where
     * the "angle" dimension consists of bins that are really solid
     * angles and has descriptions for both "phi" and "theta":
     */
    StringList  Labels ;
    StringList  Units ;
    } ;
typedef struct ComponentSubDimension_struct ComponentSubDimension ;

struct ComponentSubDimensionList_struct {
	int nqtys;
	struct {
		u_int qty_len;
		ComponentSubDimension *qty_val;
	} qty;
};
typedef struct ComponentSubDimensionList_struct ComponentSubDimensionList;

struct ComponentDescription_struct
   {
   int TypeOfStorage ;   /* DQ_STORE... type which indicates what type the 
			  * component is (int, float, etc.).
			  */
   int NRepeats ;        /* The number of elements of the type given in
			  * "TypeOfStorage" which make up a vector-unit of
			  * this component.  A value of ONE or less is
			  * equivalent to ONE.  Most of the time, this
			  * value is ONE.  Do NOT confuse this with the
			  * number of elements of this component inside
			  * of a DataQuantityInstance.  That is defined
			  * elsewhere.
			  */
   /* There should be one element in "SubDims" for each dimension in
    * the component:
    *   Time components and StdOneDimensional should have 1:
    *   StdTwoDimensional should have 2:
    *   StdThreeDimensional should have 3:
    */
   ComponentSubDimensionList  SubDims ;
   } ;
typedef struct ComponentDescription_struct ComponentDescription ;

struct ComponentDescriptionList_struct {
	int nqtys;
	struct {
		u_int qty_len;
		ComponentDescription *qty_val;
	} qty;
};
typedef struct ComponentDescriptionList_struct ComponentDescriptionList ;

struct DataQuantityDescription_struct
   {
   char *Name ;               /* The name of the quantity. */
   SpaceCraftDescription SpaceCraft ; /* The SpacecraftDescription */

   /* Added 2000/09/14 - indicates the source of this DQD (either
    * a decommutator or SCM.  Use same IDs as spacecraft IDs.
    * This will greatly aid SDT in list sorting in the UI.
    */
   int32 SourceId ;

   int QuantFmt ;             /* Data format indicator as follows:
			       *   if "quant_type" is TIME_BASED_DATA:
			       *      0 -> separate arrays for the
			       *           components (FORTRAN-like)
			       *      1 -> an array of C-like structures
			       *           where each structure element
			       *           consists of "components" floats.
			       *   if "quant_type" is RAW_BYTE_RECORD_DATA:
			       *      The number of bytes in each record.
			       */
   int QuantType ;            /* TIME_BASED_DATA ->
			       *    time,value type
	                       * FREQUENCY_BASED_DATA ->
			       *    frequency,value type
	                       * RAW_BYTE_RECORD_DATA ->
			       *    records consisting of a fixed number
			       *    of raw bytes:
	                       * X_VRS_Y_BASED_DATA ->
			       *    x vrs. y plot
	                       * BURST_HEADER_DATA
			       *    burst header
	                       * BIAS_SWEEP_HEADER_DATA
			       *    bias sweep header
	                       * STANDARD_ONE_DIMENSIONAL
			       *    one-dimensional array
	                       * STANDARD_TWO_DIMENSIONAL
			       *    two-dimensional array
	                       * STANDARD_THREE_DIMENSIONAL
			       *    three-dimensional array
			       */
   int Derivations ;          /* Flag indicating if there are derived
			       * DataQuantityDescriptions based on this
			       * one (along with the derived plots).
			       *    0 -> no
			       *    1 -> yes
			       *  When the answer is yes, the decommutator
			       *  or other data source for this quantity
			       *  must be requested for the names and
			       *  other attributes of these derived
			       *  quantities.  These exist mostly for
			       *  special, but rarely requested
			       *  quantities, like the individual
			       *  DSC byte quantities (128 of them) in
			       *  CRRES which, if automatically generated,
			       *  would use lots of unnecessary space.
			       */
   int16 NumberAlgorithms ;   /* Indicates how many different ways
			       * there are to compute this DQD.
			       * This was introduced in March 2007,
			       * in conjunction with the new ParameterReq
			       * type "DQ_STORE_ALGORITHM" to allow
			       * efficient use of the CLUSTER CAA FGM
			       * data in replacing the old usage
			       * of the raw FGM data.  It is expected
			       * that this feature will be very useful
			       * later as well.
			       *
			       * If this value is <= 0, then there
			       * is only one way of computing the DQD
			       */
   int16 CurrentAlgorithm ;   /* Relevant only if "NumberAlgorithms"
			       * is > 0.  It indicates which of the
			       * multiple ways of computing this DQD
			       * is to be used.
			       */
   ComponentDescriptionList CDList ;  /* The number of components in this
				       * quantity.  If this DQD is type:
				       *    TIME_BASED_DATA
				       * then the time-tag must be one of
				       * the components.
				       *
				       * This list must have at least one
				       * member in it.
				       *
				       * NOTE!  For QuantType's:
	                               *   STANDARD_ONE_DIMENSIONAL
	                               *   STANDARD_TWO_DIMENSIONAL
	                               *   STANDARD_THREE_DIMENSIONAL
				       * Only one member should be in
				       * the list.  All dimensions will
				       * then be of the type specified
				       * by this member.
			               */
   DataQuantityIdentifierList Ancestors ;  /* This list indicates the
					    * names and sources of
					    * DQD's required to build
					    * this DQD.  This is mainly
					    * for use by the SCM.
					    */
   } ;
typedef struct DataQuantityDescription_struct DataQuantityDescription ;

struct DataQuantityDescriptionList_strc {
	int nqtys;
	struct {
		u_int qty_len;
		DataQuantityDescription *qty_val;
	} qty;
};
typedef struct DataQuantityDescriptionList_strc DataQuantityDescriptionList ;

struct ComponentUse_struct
   {
   int Qty ;
   int Comp ;
   int IndexWithinComp ;      /* Used for multi-dimensional arrays. */
   int Associated ;         /* Used for Error Bars */
   } ;
typedef  struct ComponentUse_struct  ComponentUse ;

struct ComponentUseList_struct {
	int nqtys;
	struct {
		u_int qty_len;
		ComponentUse *qty_val;
	} qty;
};
typedef  struct ComponentUseList_struct  ComponentUseList ;










struct ComponentAccumulation_struct
   {
   int  ComponentUseIdx ;      /* Indicates which index in the
			        * ComponentUseList that this
			        * structure controls.
			        */
   int  Changeable ;           /* Flag indicating where SDT has
				* authority to change the Minimum
				* and Maximum accumulation limits.
				*/
   int  UseArrayDescription ;  /* Flag indicating whether or not
				* to use array descriptions (if
				* they exist).  If this flag is OFF
				* or if there are no array descriptions,
				* then accumulations will refer to the
				* range described by "BMinimum" and
				* "BMaximum".  Otherwise, "AMinimum"
				* and "AMaximum" are used.  Note that
				* the "BMinimum" and "BMaximum" refer
				* to absolute bin indices.
				*/
   int     NumArrDesc ;        /* This is the number of array descriptions
				* that the corresponding set of bins all
				* have.  This corresponds to what was
				* said regarding "Units" and "Labels"
				* in the "ComponentSubDimension"
				* definition section.  This number will
				* usually be 0 or 1.  An example of 2
				* would be for the solid angles in the
				* Fast Teams quantities which have both
				* a "phi" and "theta" description.
				*/
   int     AxisSubDim ;        /* This is used to help define spectrogram
				* plots which have dimensions > 1.  In
				* particular, for FAST teams, this helps
				* the spectrogram plotter create a PHI
				* plot as opposed to a THETA plot.
				*/
   double  AMinimum[MDIM_ADESC_MAX] ;  /* Minimum value of component to
				        * include in accumulation, in
				        * array description mode.
				        */
   double  AMaximum[MDIM_ADESC_MAX] ;  /* Maximum value of component to
				        * include in accumulation, in
				        * array description mode.
				        */
   double  BMinimum ;          /* Minimum value of component to
				* include in accumulation, in
				* bins-only mode.
				*/
   double  BMaximum ;          /* Maximum value of component to
				* include in accumulation, in
				* bins-only mode.
				*/
   } ;
typedef  struct ComponentAccumulation_struct  ComponentAccumulation ;

struct ComponentAccumulationList_struct {
	int nqtys;
	struct {
		u_int qty_len;
		ComponentAccumulation *qty_val;
	} qty;
};
typedef struct ComponentAccumulationList_struct ComponentAccumulationList;

struct DrawingUnit_struct
   {
   String  TraceLabel ;          /* Useful in the multi-line scenario:
				  *  The label (if any) to put on a trace
				  * in a multi-line timeseries plot panel.
				  */
   double  LinearOffset ;        /* value to offset the line with
				  * if this is a linear plot.  In this
				  * case the offset is additive.
				  */
   double  LogOffset ;           /* value to offset the line with
				  * if this is a log plot.  In this
				  * case the offset is an additive
				  * exponent.
				  */
   double BrkIntvl ;        /* Value indicating the distance between
			     * points for line-skippage:
			     */


   int Style ;              /* encapsulates both
                             * int MLineNSymbols ;
                             *    Used by multi-line plots for line
			     *    output control.  It may take on 2 values:
			     *       SDT_MLSTYLE_COLOR
			     *       SDT_MLSTYLE_SYMBOL
			     *
			     * and 
                             * int ContourStyle ;
                             *    Used by Contour plots as follows:
			     *       SDT_CONTOUR_STYLE_COLOR
			     *       SDT_CONTOUR_STYLE_LINE
			     */

   int PointsPlot ;         /* The Points Per plot for this panel */

   int MLineNSymbols ;      /* Used by multi-line plots for setting
			     * the number of symbols per plot on a page.
			     */

   int LineFont ;          /* Defined as follows:
			    *    0  ->  SOLID
			    *    1  ->  DASHED
			    *    2  ->  PHANTOM
			    *    3  ->  CENTERLINE
			    *
			    * Use the SDT_LFONT... definitions.
			    * If set to -1, the default of SOLID is used.
			    */

   int LineColor ;         /* Defined as follows:
			    *    0  ->  WHITE
			    *    1  ->  RED
			    *    2  ->  GREEN
			    *    3  ->  BLUE
			    *    4  ->  ORANGE
			    *    5  ->  PURPLE
			    *    6  ->  YELLOW
			    *
			    * Use the SDT_LCOLOR... definitions.
			    * If set to -1, the default of WHITE is used.
			    */

   int16   UseOffset ;           /* Line offset: 
				  * Note that, in MultiLine plots,
				  * each set of axes corresponds to
				  * one line.  Each line will be offset
				  * from its true value by "LinearOffset"
				  * or "LogOffset", depending on whether
				  * the plot axis is linear or log,
				  * it this flag is on:
				  */
   int16   ChangeOffset ;        /* 0 -> fixed
                                  * 1 -> user may adjust offset
                                  */
   int16 LinesPoints ;      /* Whether to plot this panel using lines or
			     * points.
			     */

   int16 BrkFlag ;          /* 0 -> Plot continuous line
			     *      between all points.
			     * 1 -> Skip line between consec. points
			     *      more that "BrkIntvl" apart.
			     */

   int16 ErrorBar ;         /* Used by multi-line plots for error
			     * bar control.  It may take on 3 values:
			     *    SDT_EBAR_NONE
			     *    SDT_EBAR_DATA_CENTERED
			     *    SDT_EBAR_BAR_CENTERED
			     */

   int16 SpecialCode ;     /* A flag, added 2002/11/09, which
			    * indicates that this plot has a special
			    * feature.  As of 2002/11/09, the only
			    * such special feature is for allowing
			    * the computation of EFLux on some
			    * SPECTRAL plots.  The code for this is:
			    *
			    *   SDT_SPECTRA_STYLE_EFLUX
			    *
			    * In conjunction with "SpecialCode",
			    * there is another flag:
			    *
			    *   SpecialArg
			    *
			    * which gives the software an argument
			    * with which to make distinctions on
			    * on to handle the SpecialCode differently
			    * for various plots. 
			    *
			    * Note that the default value is "0" -
			    * indicating that there are no special
			    * modes of plotting.
			    */

   int16 SpecialArg;       /* A flag, added 2002/11/09, which
			    * works with "SpecialCode".  For:
			    *
			    *   SDT_SPECTRA_STYLE_EFLUX
			    *
			    * this field will * indicate a "GF"
			    * coefficient as follows:
			    *
			    *    0 ->  GF = 1.0
			    *    1 ->  GF = 4.20 x 10exp(-3)
			    *    2 ->  GF = 8.70 x 10exp(-3)
			    *    3 ->  GF = 9.40 x 10exp(-3)
			    *    4 ->  GF = 5.64 x 10exp(-2)
			    *    5 ->  GF = 5.64 x 10exp(-2)
			    *    6 ->  GF = 9.00 x 10exp(-3)
			    */

   int16 SpecialFlag ;     /* A flag, added 2002/11/09, which
			    * indicates whether or not the
			    * feature in "SpecialCode" is turned
			    * ON (1) or OFF (0).
			    */

   ComponentUseList CUList ;     /* Which of the data quantities and
			          * which component of the data quantity
			          * is the abcsissa and ordinate for the
			          * DrawingUnit.  Note that the "Qty"
				  * index for each element in this
				  * array refers to "DQDList" in the
				  * PlotQuantityDescription.
			          */
   ComponentAccumulationList AList ;   /* List indicating accumulation
					* limits for multi-dimensional
					* arrays.
					*/
   } ;
typedef  struct DrawingUnit_struct  DrawingUnit ;

struct DrawingUnitList_struct {
	int nqtys;
	struct {
		u_int qty_len;
		DrawingUnit *qty_val;
	} qty;
};
typedef struct DrawingUnitList_struct DrawingUnitList ;

struct AxisUnit_struct
   {
   int     ScaleType ;           /* Axis scaling:
				  *    0 -> linear
                                  *    1 -> log
                                  *    2 -> hex
				  */

   int     Changeable ;          /* 0 -> fixed,  1 -> user may set */

   int     AutoScale ;           /* 0 -> none,  1 -> autoscale, if
				  *                  meaningful
				  */

   int     UseBins ;             /* 0 -> use MinLim, MaxLim
				  * 1 -> use MinBin, MaxBin
				  */

   double  MinLim ;              /* Current minimum limit */
   double  MaxLim ;              /* Current maximum limit */

   double  MinBin ;              /* Current bin minimum limit */
   double  MaxBin ;              /* Current bin maximum limit */

   String  Label ;               /* Axis label */
   String  Units ;               /* Axis units */
   } ;
typedef  struct AxisUnit_struct  AxisUnit ;

struct AxisUnitList_struct {
	int nqtys;
	struct {
		u_int qty_len;
		AxisUnit *qty_val;
	} qty;
};
typedef struct AxisUnitList_struct AxisUnitList ;

struct ColorLine_struct
   {
   int Use ;                /* 0 -> no line value used
			     * 1 -> line values are used
			     */
   double  High ;
   double  Low ;
   } ;
typedef  struct ColorLine_struct ColorLine ;

struct PlotQuantityDescription_struct
   {
   char *TheTitle ;           /* The default name of the plot quantity. */
   String Title ;             /* The current name of the plot quantity. */

   int PlotType ;             /* Plot Type */
   int PlotHeightUnits ;      /* Plot Height Ratio */

   /* Added 2000/09/14, to handle restricting Cluster FGM plots to
    * SSL.  The possible values are:
    *
    *    0 -> no restrictions (default)
    *    1 -> SSL (Berkeley) only
    */
   int Restrictions ;

   /* The list of DQD's from which are built the DrawingUnits
    * for this PQD:
    */
   DataQuantityDescriptionList DQDList ;

   ColorLine YellowLine ;
   ColorLine RedLine ;

   /* List of axis definitions: */
   AxisUnitList    Axes ;

   /* The list of "lines" that belong to this plot: */
   DrawingUnitList DrawList ;
   } ;
typedef  struct PlotQuantityDescription_struct  PlotQuantityDescription ;

struct PlotQuantityDescriptionList_struct {
	int nqtys;
	struct {
		u_int qty_len;
		PlotQuantityDescription *qty_val;
	} qty;
};
typedef struct PlotQuantityDescriptionList_struct
    PlotQuantityDescriptionList ;

struct DataPlotQuantityDescription_struct {
        DataQuantityDescriptionList DQDList ;
        PlotQuantityDescriptionList PQDList ;
} ;
typedef struct DataPlotQuantityDescription_struct DataPlotQuantityDescription ;


struct PlotPanelAttributes_struct
   {
   int X ;                   /* The relative position in the plot window for
			      * this.
			      */
   int Y ;		     /* plot panel. */
   int Width ;               /* The size in pixels of this plot panel. */
   int Height ;
   } ;
typedef  struct PlotPanelAttributes_struct  PlotPanelAttributes ;

/* ----------------------------------------------------------------------- */
/* Extra structures for parsing DQD and PQD files: */

/* This is returned in an argument by "EndDQDParsing": */
struct ParseDQDExtraInfo_struct
   {
   int        ECode ;          /* Error code (unused as of 93/10/28): */
   IntList    *CircularSize ;  /* List of number of elements per circular
				* buffer for each DQD in the returned
				* DQDList:
				*/
   IntList    *ArrDescSize ;   /* List of array descriptions for multi-
				* dimensional DQDs.  Since most DQD's
				* are not multi-dimensional, there is
				* a lot of wasted words here.
				*/
   /* Algorithms: */
   IntList    *Alg0 ;
   IntList    *Alg1 ;
   IntList    *Alg2 ;
   IntList    *Alg3 ;
   } ;
typedef  struct ParseDQDExtraInfo_struct  ParseDQDExtraInfo ;

/* This is returned in an argument by "EndPQDParsing": */
struct ParsePQDExtraInfo_struct
   {
   int        ECode ;          /* Error code (unused as of 93/10/28): */
   } ;
typedef  struct ParsePQDExtraInfo_struct  ParsePQDExtraInfo ;

/* ----------------------------------------------------------------------- */
/* Function Declarations: */

extern long  SDTConvertCalendarToJulianDay (int year, int month,
    int day) ;

extern int   SDTConvertJulianDayToCalendar (long julian_day, int *year,
    int *month, int *day, int *day_of_year) ;

extern int  SDTConvertDayOfYearToMonthDay (int year, int day_of_year,
    int *month, int *day_of_month) ;

extern int  SDTConvertMonthDayToDayOfYear(int year, int month, int day,
    int *day_of_year) ;

extern int  SDTConvertSecondsToClockTime (double in_time, int *ohour,
    int *ominute, int *osecond, int *omillisec) ;

extern int  SDTNormalizeJulianDayTime (long int *julian_day,
    double *time) ;

extern int  SDTSizeType (int in_type) ;

extern char *SDTParseEnvironmentVariablesInString (char *istr,
    int *err_code) ;

extern int  SDTGetDomainName (char *DomainName) ;

extern int  SDTCheckDomainNameForOverride (char *OverrideFile,
    int *oflags, int *ecode) ;

extern StringList *SDTExtractFileNamesFromString (char *in_string) ;

extern StringList *SDTRegroupDataSetNamesFromString (char *in_str) ;

extern TimeSpanDescription  *SDTConstructTimeSpanDescription (
    long start_julian_day, long end_julian_day, double start_time,
    double end_time) ;

extern TimeSpanDescription  *SDTCopyConstructTimeSpanDescription (
    TimeSpanDescription *in_tspan) ;

extern int  SDTFillTimeSpanDescription (TimeSpanDescription *tspan,
    long start_julian_day, long end_julian_day,
    double start_time, double end_time) ;

extern int  SDTCopyTimeSpanDescription (TimeSpanDescription *in_tspan,
        TimeSpanDescription *out_tspan) ;

extern int  SDTDestructTimeSpanDescription (TimeSpanDescription *tspan) ;

extern int  SDTClearTimeSpanDescription (TimeSpanDescription *tspan) ;

extern int  SDTPrintTimeSpanDescription (TimeSpanDescription *tspan,
    FILE *ofp) ;

extern int SDTCompareTimeSpanDescription( TimeSpanDescription *in_tspan, TimeSpanDescription *out_tspan) ;

extern SpaceCraftDescription  *SDTConstructSpaceCraftDescription (
                                   int16 id, int16 sub_id) ;

extern SpaceCraftDescription  *SDTCopyConstructSpaceCraftDescription (
    SpaceCraftDescription *in_spcraft) ;

extern int  SDTFillSpaceCraftDescription (SpaceCraftDescription *spcraft,
                int16 id, int16 sub_id) ;

extern int  SDTCopySpaceCraftDescription (SpaceCraftDescription *in_scraft,
    SpaceCraftDescription *out_scraft) ;

extern int  SDTDestructSpaceCraftDescription (SpaceCraftDescription *spcraft) ;

extern int  SDTClearSpaceCraftDescription (SpaceCraftDescription *scraft) ;

extern int  SDTPrintSpaceCraftDescription (SpaceCraftDescription *scraft,
    FILE *ofp) ;

extern int  SDTCompareSpaceCraftDescription (SpaceCraftDescription *sp1,
    SpaceCraftDescription *sp2) ;

extern int  SDTCompareSpaceCraftId (SpaceCraftDescription *sp1,
    SpaceCraftDescription *sp2) ;

extern DataQuantityIdentifier *SDTConstructDataQuantityIdentifier (
    char *qty_id, int32 source_id) ;

extern DataQuantityIdentifier  *SDTCopyConstructDataQuantityIdentifier (
    DataQuantityIdentifier *in_dqid) ;

extern int  SDTFillDataQuantityIdentifier (DataQuantityIdentifier *dqid,
    char *qty_id, int32 source_id) ;

extern int  SDTCopyDataQuantityIdentifier (DataQuantityIdentifier *in_dqid,
    DataQuantityIdentifier *out_dqid) ;

extern int  SDTDestructDataQuantityIdentifier (
    DataQuantityIdentifier *dqid) ;

extern int  SDTClearDataQuantityIdentifier (DataQuantityIdentifier *dqid) ;

extern int  SDTPrintDataQuantityIdentifier (DataQuantityIdentifier *dqid,
    FILE *ofp) ;

extern DataQuantityIdentifierList  *SDTConstructDataQuantityIdentifierList (
    int ndqids, int single_source, char **qty_id, int32 *source_id) ;

extern
   DataQuantityIdentifierList  *SDTCopyConstructDataQuantityIdentifierList (
    DataQuantityIdentifierList *in_dqdl) ;

extern int  SDTFillDataQuantityIdentifierList (
    DataQuantityIdentifierList *dqdl,
    int ndqids, int single_source, char **qty_id, int32 *source_id) ;

extern int  SDTCopyDataQuantityIdentifierList (
    DataQuantityIdentifierList *in_dqdl,
    DataQuantityIdentifierList *out_dqdl) ;

extern int  SDTDestructDataQuantityIdentifierList (
    DataQuantityIdentifierList *dqdl) ;

extern int  SDTClearDataQuantityIdentifierList (
    DataQuantityIdentifierList *dqdl) ;

extern int  SDTArrayFillDataQuantityIdentifierList (int ndqid,
    DataQuantityIdentifier *in_dqid,
    DataQuantityIdentifierList *out_dqidl) ;

extern int  SDTPtrArrayFillDataQuantityIdentifierList (int ndqids,
    DataQuantityIdentifier **in_dqid,
    DataQuantityIdentifierList *out_dqidl) ;

extern int  SDTPrintDataQuantityIdentifierList (
    DataQuantityIdentifierList *dqdl, FILE *ofp) ;

extern char *SDTReturnTrueMultiAlgorithmLabel (char *iname,
    int16 *ridx) ;

extern ComponentSubDimension  *SDTConstructComponentSubDimension (
    int discrete, int nstrings, char **labelp, char **unitp) ;

extern ComponentSubDimension  *SDTCopyConstructComponentSubDimension (
    ComponentSubDimension *in_subdim) ;

extern int SDTFillComponentSubDimension (ComponentSubDimension *dim_ptr,
    int discrete, int nstrings, char **labelp, char **unitp) ;

extern int  SDTCopyComponentSubDimension (
    ComponentSubDimension *in_subdim,
    ComponentSubDimension *out_subdim) ;

extern int  SDTDestructComponentSubDimension (
    ComponentSubDimension *dim_ptr) ;

extern int  SDTClearComponentSubDimension (
    ComponentSubDimension *dim_ptr) ;

extern int  SDTPrintComponentSubDimension (
    ComponentSubDimension *dim_ptr, FILE *ofp) ;

extern ComponentSubDimensionList 
    *SDTConstructComponentSubDimensionList (int ndims,
	int *dscr_flg, StringList *ilabels, StringList *iunits) ;

extern ComponentSubDimensionList 
    *SDTCopyConstructComponentSubDimensionList (
	ComponentSubDimensionList *in_clist) ;

extern int  SDTFillComponentSubDimensionList (
    ComponentSubDimensionList *clist, int ndims,
        int *dscr_flg, StringList *ilabels, StringList *iunits) ;

extern int  SDTCopyComponentSubDimensionList (
    ComponentSubDimensionList *in_clist,
	ComponentSubDimensionList *out_clist) ;

extern int  SDTDestructComponentSubDimensionList (
    ComponentSubDimensionList *clist) ;

extern int  SDTClearComponentSubDimensionList (
    ComponentSubDimensionList *clist) ;

extern int  SDTArrayFillComponentSubDimensionList (int ncd,
    ComponentSubDimension *in_cd, ComponentSubDimensionList *out_cdl) ;

extern int  SDTPtrArrayFillComponentSubDimensionList (int ncd,
    ComponentSubDimension **in_cd, ComponentSubDimensionList *out_cdl) ;

extern int  SDTPrintComponentSubDimensionList (
    ComponentSubDimensionList *clist, FILE *ofp) ;


extern ComponentDescription  *SDTConstructComponentDescription (
    int type_id, int nrepeats, ComponentSubDimensionList *subdiml) ;

extern ComponentDescription  *SDTCopyConstructComponentDescription (
    ComponentDescription *in_component) ;

extern int  SDTFillComponentDescription (ComponentDescription *component,
    int type_id, int nrepeats, ComponentSubDimensionList *subdiml) ;

extern int  SDTCopyComponentDescription (
    ComponentDescription *in_component,
	ComponentDescription *out_component) ;

extern int  SDTDestructComponentDescription (
    ComponentDescription *component) ;

extern int  SDTClearComponentDescription (ComponentDescription *comp) ;

extern int  SDTPrintComponentDescription (ComponentDescription *component,
    FILE *ofp) ;

extern ComponentDescriptionList  *SDTConstructComponentDescriptionList (
    int ncomps, int *type_id, int *nrepeats,
    ComponentSubDimensionList *subdiml) ;

extern ComponentDescriptionList 
    *SDTCopyConstructComponentDescriptionList (
        ComponentDescriptionList *in_clist) ;

extern int  SDTFillComponentDescriptionList (
    ComponentDescriptionList *clist, int ncomps, int *type_id,
    int *nrepeats, ComponentSubDimensionList *subdiml) ;

extern int  SDTCopyComponentDescriptionList (
    ComponentDescriptionList *in_clist,
    ComponentDescriptionList *out_clist) ;

extern int  SDTDestructComponentDescriptionList (
    ComponentDescriptionList *clist) ;

extern int  SDTClearComponentDescriptionList (
    ComponentDescriptionList *clist) ;

extern int  SDTArrayFillComponentDescriptionList (int ncd,
    ComponentDescription *in_cd, ComponentDescriptionList *out_cdl) ;

extern int  SDTPtrArrayFillComponentDescriptionList (int ncd,
    ComponentDescription **in_cd, ComponentDescriptionList *out_cdl) ;

extern int  SDTPrintComponentDescriptionList (
    ComponentDescriptionList *clist, FILE *ofp) ;

extern DataQuantityDescription *SDTConstructDataQuantityDescription (
    char *in_name, SpaceCraftDescription *in_scraft, int32 in_sourceid,
    int in_quantfmt, int in_quanttype, int in_deriv, int16 nalgs,
    int16 calg, ComponentDescriptionList *in_clist,
    DataQuantityIdentifierList *in_dqids) ;

extern DataQuantityDescription  *SDTCopyConstructDataQuantityDescription (
    DataQuantityDescription *in_dqd) ;

extern int  SDTFillDataQuantityDescription (DataQuantityDescription *in_dqd,
    char *in_name, SpaceCraftDescription *in_scraft, int32 in_sourceid,
    int in_quantfmt, int in_quanttype, int in_deriv, int16 nalgs,
    int16 calg, ComponentDescriptionList *in_clist,
    DataQuantityIdentifierList *in_dqids) ;

extern int  SDTCopyDataQuantityDescription (DataQuantityDescription *in_dqd,
    DataQuantityDescription *out_dqd) ;

extern int  SDTDestructDataQuantityDescription (
    DataQuantityDescription *in_dqd) ;

extern int  SDTClearDataQuantityDescription (
    DataQuantityDescription *in_dqd) ;

extern int  SDTPrintDataQuantityDescription (DataQuantityDescription *dqd,
    FILE *ofp) ;

extern int  SDTCompareDataQuantityDescription (DataQuantityDescription *dq1,
    DataQuantityDescription *dq2) ;

extern DataQuantityDescriptionList
  *SDTConstructDataQuantityDescriptionList (int nqtys,
    char **in_name, int nspcraft,
    SpaceCraftDescription *in_scraft, int32 *in_sourceid,
    int *in_quantfmt, int *in_quanttype, int *in_deriv,
    int16 *in_nalgs, int16 *in_calg,
    ComponentDescriptionList **in_clist,
    DataQuantityIdentifierList **in_dqids) ;

extern DataQuantityDescriptionList
    *SDTCopyConstructDataQuantityDescriptionList (
    DataQuantityDescriptionList *in_dqdl) ;

extern int  SDTFillDataQuantityDescriptionList (
    DataQuantityDescriptionList *dqdl,
    int nqtys, char **in_name, int nspcraft,
    SpaceCraftDescription *in_scraft, int32 *in_sourceid,
    int *in_quantfmt, int *in_quanttype, int *in_deriv,
    int16 *in_nalgs, int16 *in_calg,
    ComponentDescriptionList **in_clist,
    DataQuantityIdentifierList **in_dqids) ;

extern int  SDTCopyDataQuantityDescriptionList (
    DataQuantityDescriptionList *in_dqdl,
    DataQuantityDescriptionList *out_dqdl) ;

extern int  SDTDestructDataQuantityDescriptionList (
    DataQuantityDescriptionList *dqdl) ;

extern int  SDTClearDataQuantityDescriptionList (
    DataQuantityDescriptionList *dqdl) ;

extern int  SDTArrayFillDataQuantityDescriptionList (int ndqd,
    DataQuantityDescription *in_dqd, DataQuantityDescriptionList *out_dqdl) ;

extern int  SDTPtrArrayFillDataQuantityDescriptionList (int ndqd,
    DataQuantityDescription **in_dqd,
    DataQuantityDescriptionList *out_dqdl) ;

extern int  SDTPrintDataQuantityDescriptionList (
   DataQuantityDescriptionList *dqdl, FILE *ofp) ;

extern ComponentUse  *SDTConstructComponentUse (int qty_idx, int comp_idx,
    int idx_inside_comp, int assoc_idx) ;

extern ComponentUse  *SDTCopyConstructComponentUse (
   ComponentUse *in_component) ;

extern int  SDTFillComponentUse (ComponentUse *component, int qty_idx,
    int comp_idx, int idx_inside_comp, int assoc_idx) ;

extern int  SDTCopyComponentUse (ComponentUse *in_component,
    ComponentUse *out_component) ;

extern int  SDTDestructComponentUse (ComponentUse *component) ;

extern int  SDTClearComponentUse (ComponentUse *comp) ;

extern int  SDTPrintComponentUse (ComponentUse *component, FILE *ofp) ;

extern ComponentUseList  *SDTConstructComponentUseList (int nuses,
    int *qty_idx, int *comp_idx, int *within_idx, int *assoc_idx) ;

extern ComponentUseList  *SDTCopyConstructComponentUseList (
    ComponentUseList *in_clist) ;

extern int  SDTFillComponentUseList (ComponentUseList *clist, int nuses,
    int *qty_idx, int *comp_idx, int *within_idx, int *assoc_idx) ;

extern int  SDTCopyComponentUseList (ComponentUseList *in_clist,
    ComponentUseList *out_clist) ;

extern int  SDTDestructComponentUseList (ComponentUseList *clist) ;

extern int  SDTClearComponentUseList (ComponentUseList *clist) ;

extern int  SDTArrayFillComponentUseList (int ncu, ComponentUse *in_cu,
    ComponentUseList *out_cul) ;

extern int  SDTPtrArrayFillComponentUseList (int ncu, ComponentUse **in_cu,
    ComponentUseList *out_cul) ;

extern int  SDTPrintComponentUseList (ComponentUseList *clist, FILE *ofp) ;

extern String  *SDTConstructString (char *cptr) ;

extern String  *SDTCopyConstructString (String *in_sptr) ;

extern int  SDTFillString (String *sptr, char *cptr) ;

extern int  SDTCopyString (String *in_str, String *out_str) ;

extern int  SDTDestructString (String *sptr) ;

extern int  SDTClearString (String *sptr) ;

extern int  SDTPrintString (String *in_str, FILE *ofp) ;

extern StringList  *SDTConstructStringList (int nstrings, char **str_ptr) ;

extern StringList  *SDTCopyConstructStringList (StringList *in_slist) ;

extern int  SDTFillStringList (StringList *slist, int nstrings,
    char **str_ptr) ;

extern int  SDTCopyStringList (StringList *in_slist, StringList *out_slist) ;

extern int  SDTDestructStringList (StringList *slist) ;

extern int  SDTClearStringList (StringList *slist) ;

extern int  SDTArrayFillStringList (int nstr, String *in_str,
    StringList *out_strl) ;

extern int  SDTPtrArrayFillStringList (int nstr, String **in_str,
    StringList *out_strl) ;

extern int  SDTPrintStringList (StringList *slist, FILE *ofp) ;

extern VoidList  *SDTConstructVoidList (int nvoids, void **in_void) ;

extern VoidList  *SDTCopyConstructVoidList (VoidList *in_vptr) ;

extern int  SDTFillVoidList (VoidList *vptr, int nvoids, void **in_void) ;

extern int  SDTCopyVoidList (VoidList *in_void, VoidList *out_void) ;

extern int  SDTDestructVoidList (VoidList *iptr) ;

extern int  SDTClearVoidList (VoidList *iptr) ;

extern int  SDTArrayFillVoidList (int nvoid, void **in_void,
    VoidList *out_voidl) ;

extern int  SDTPrintVoidList (VoidList *in_vptr, FILE *ofp) ;

extern IntList  *SDTConstructIntList (int nints, int *in_int) ;

extern IntList  *SDTCopyConstructIntList (IntList *in_iptr) ;

extern int  SDTFillIntList (IntList *iptr, int nints, int *in_int) ;

extern int  SDTCopyIntList (IntList *in_int, IntList *out_int) ;

extern int  SDTDestructIntList (IntList *iptr) ;

extern int  SDTClearIntList (IntList *iptr) ;

extern int  SDTArrayFillIntList (int nint, int *in_int,
    IntList *out_intl) ;

extern int  SDTPrintIntList (IntList *in_iptr, FILE *ofp) ;

extern Int16List  *SDTConstructInt16List (int nint16s, int16 *in_int16) ;

extern Int16List  *SDTCopyConstructInt16List (Int16List *in_iptr) ;

extern int  SDTFillInt16List (Int16List *iptr, int nint16s, int16 *in_int16) ;

extern int  SDTCopyInt16List (Int16List *in_int16, Int16List *out_int16) ;

extern int  SDTDestructInt16List (Int16List *iptr) ;

extern int  SDTClearInt16List (Int16List *iptr) ;

extern int  SDTArrayFillInt16List (int nint16, int16 *in_int16,
    Int16List *out_int16l) ;

extern int  SDTPrintInt16List (Int16List *in_iptr, FILE *ofp) ;

extern Int32List  *SDTConstructInt32List (int nint32s, int32 *in_int32) ;

extern Int32List  *SDTCopyConstructInt32List (Int32List *in_iptr) ;

extern int  SDTFillInt32List (Int32List *iptr, int nint32s, int32 *in_int32) ;

extern int  SDTCopyInt32List (Int32List *in_int32, Int32List *out_int32) ;

extern int  SDTDestructInt32List (Int32List *iptr) ;

extern int  SDTClearInt32List (Int32List *iptr) ;

extern int  SDTArrayFillInt32List (int nint32, int32 *in_int32,
    Int32List *out_int32l) ;

extern int  SDTPrintInt32List (Int32List *in_iptr, FILE *ofp) ;

extern AddrTList *SDTConstructAddrTList (int naddr, AddrT *in_addr) ;

extern AddrTList  *SDTCopyConstructAddrTList (AddrTList *in_iptr) ;

extern int  SDTFillAddrTList (AddrTList *iptr, int naddr,
    AddrT *in_addr) ;

extern int  SDTCopyAddrTList (AddrTList *in_addr, AddrTList *out_addr) ;

extern int  SDTDestructAddrTList (AddrTList *iptr) ;

extern int  SDTClearAddrTList (AddrTList *iptr) ;

extern int  SDTArrayFillAddrTList (int naddr, AddrT *in_addrl,
    AddrTList *out_addrl) ;

extern int  SDTPrintAddrTList (AddrTList *in_iptr, FILE *ofp) ;

extern FloatList  *SDTConstructFloatList (int nflts, float *in_flt) ;

extern FloatList  *SDTCopyConstructFloatList (FloatList *in_iptr) ;

extern int  SDTFillFloatList (FloatList *iptr, int nflts, float *in_flt) ;

extern int  SDTCopyFloatList (FloatList *in_flt, FloatList *out_flt) ;

extern int  SDTDestructFloatList (FloatList *iptr) ;

extern int  SDTClearFloatList (FloatList *iptr) ;

extern int  SDTArrayFillFloatList (int nflt, float *in_flt,
    FloatList *out_fltl) ;

extern int  SDTPrintFloatList (FloatList *in_iptr, FILE *ofp) ;

extern DoubleList  *SDTConstructDoubleList (int ndbls, double *in_dbl) ;

extern DoubleList  *SDTCopyConstructDoubleList (DoubleList *in_dptr) ;

extern int  SDTFillDoubleList (DoubleList *dptr, int ndbls, double *in_dbl) ;

extern int  SDTCopyDoubleList (DoubleList *in_dbl, DoubleList *out_dbl) ;

extern int  SDTDestructDoubleList (DoubleList *dptr) ;

extern int  SDTClearDoubleList (DoubleList *dptr) ;

extern int  SDTArrayFillDoubleList(int ndbl, double *in_dbl,
    DoubleList *out_dbll) ;

extern int  SDTPrintDoubleList (DoubleList *in_dptr, FILE *ofp) ;

extern ComponentAccumulation *SDTConstructComponentAccumulation (
    int cuse_idx, int chg_flg, int use_arr_desc, int na_desc, int iasd,
    double *in_mina, double *in_maxa, double in_minb, double in_maxb) ;

extern ComponentAccumulation  *SDTCopyConstructComponentAccumulation (
    ComponentAccumulation *in_caccum) ;

extern int SDTFillComponentAccumulation (ComponentAccumulation *caccum,
    int cuse_idx, int chg_flg, int use_arr_desc, int na_desc, int iasd,
    double *in_mina, double *in_maxa, double in_minb, double in_maxb) ;

extern int SDTCopyComponentAccumulation (ComponentAccumulation *in_caccum,
    ComponentAccumulation *out_caccum) ;

extern int  SDTDestructComponentAccumulation (
    ComponentAccumulation *in_caccum) ;

extern int  SDTClearComponentAccumulation (
    ComponentAccumulation *in_caccum) ;

extern int  SDTPrintComponentAccumulation (
    ComponentAccumulation *in_caccum, FILE *ofp) ;

extern ComponentAccumulationList  *SDTConstructComponentAccumulationList (
    int  naccum, int *cuse_idx, int *chg_flg, int *use_arr_desc,
    int *na_desc, int *iasd, double **in_mina, double **in_maxa,
    double *in_minb, double *in_maxb) ;

extern ComponentAccumulationList
    *SDTCopyConstructComponentAccumulationList (
      ComponentAccumulationList *in_cal) ;

extern int SDTFillComponentAccumulationList (
    ComponentAccumulationList *cal,
    int  naccum, int *cuse_idx, int *chg_flg, int *use_arr_desc,
    int *na_desc, int *iasd, double **in_mina, double **in_maxa,
    double *in_minb, double *in_maxb) ;

extern int  SDTCopyComponentAccumulationList (
    ComponentAccumulationList *in_cal, ComponentAccumulationList *out_cal) ;

extern int  SDTDestructComponentAccumulationList (
    ComponentAccumulationList *cal) ;

extern int  SDTClearComponentAccumulationList (
    ComponentAccumulationList *cal) ;

extern int  SDTArrayFillComponentAccumulationList (int ncaccum,
    ComponentAccumulation *in_caccum,
    ComponentAccumulationList *out_cal) ;

extern int  SDTPtrArrayFillComponentAccumulationList (int ncaccum,
    ComponentAccumulation **in_caccum,
    ComponentAccumulationList *out_cal) ;

extern int  SDTPrintComponentAccumulationList (
    ComponentAccumulationList *cal, FILE *ofp) ;


extern DrawingUnit *SDTConstructDrawingUnit (char *tr_lab,
    int16 use_offset, double in_lin_off, double in_log_off,
    int16 change_offset, int in_style, int16 lines_pts,
    int points_plot, int16 brk_flg, double brk_intvl,
    int16 error_bar, int mlinesym, int in_lfont, int in_lcolor,
    int16 spec_cd, int16 sp_arg, int16 sp_flg, int ncu,
    ComponentUse *in_culist, int nca, ComponentAccumulation *in_calist) ;

extern DrawingUnit  *SDTCopyConstructDrawingUnit (DrawingUnit *in_drwun) ;

extern int SDTFillDrawingUnit (DrawingUnit *drwun, char *tr_lab,
    int16 use_offset, double in_lin_off, double in_log_off,
    int16 change_offset, int in_style, int16 lines_pts,
    int points_plot, int16 brk_flg, double brk_intvl,
    int16 error_bar, int mlinesym, int in_lfont, int in_lcolor,
    int16 spec_cd, int16 sp_arg, int16 sp_flg, int ncu,
    ComponentUse *in_culist, int nca, ComponentAccumulation *in_calist) ;

extern int  SDTCopyDrawingUnit (DrawingUnit *in_drwun,
    DrawingUnit *out_drwun) ;

extern int  SDTDestructDrawingUnit (DrawingUnit *in_drwun) ;

extern int  SDTClearDrawingUnit (DrawingUnit *in_drwun) ;

extern int  SDTPrintDrawingUnit (DrawingUnit *in_drwun, FILE *ofp) ;

extern DrawingUnitList *SDTConstructDrawingUnitList (int ndrwun,
    char **tr_lab, int16 *use_offset, double *in_lin_off,
    double *in_log_off, int16 *change_offset, int *in_style,
    int16 *lines_pts, int *points_plot, int16 *brk_flg,
    double *brk_intvl, int16 *error_bar, int *mlinesym,
    int *in_lfont, int *in_lcolor, int16 *spec_cd, int16 *sp_arg,
    int16 *sp_flg, ComponentUseList **in_culist,
    ComponentAccumulationList **in_calist) ;

extern DrawingUnitList  *SDTCopyConstructDrawingUnitList (
    DrawingUnitList *in_drwunl) ;

extern int  SDTFillDrawingUnitList (DrawingUnitList *drwunl,
    int ndrwun, char **tr_lab, int16 *use_offset, double *in_lin_off,
    double *in_log_off, int16 *change_offset, int *in_style,
    int16 *lines_pts, int *points_plot, int16 *brk_flg,
    double *brk_intvl, int16 *error_bar, int *mlinesym,
    int *in_lfont, int *in_lcolor, int16 *spec_cd, int16 *sp_arg,
    int16 *sp_flg, ComponentUseList **in_culist,
    ComponentAccumulationList **in_calist) ;

extern int  SDTCopyDrawingUnitList (DrawingUnitList *in_drwunl,
    DrawingUnitList *out_drwunl) ;

extern int  SDTDestructDrawingUnitList (DrawingUnitList *drwunl) ;

extern int  SDTClearDrawingUnitList (DrawingUnitList *drwunl) ;

extern int  SDTArrayFillDrawingUnitList (int ndrwun,
    DrawingUnit *in_drwun, DrawingUnitList *out_drwunl) ;

extern int SDTPtrArrayFillDrawingUnitList (int ndrwun,
    DrawingUnit **in_drwun, DrawingUnitList *out_drwunl) ;

extern int  SDTPrintDrawingUnitList (DrawingUnitList *drwunl, FILE *ofp) ;


extern AxisUnit *SDTConstructAxisUnit (int scale_type, int changeable,
    int autoscale, double in_minlim, double in_maxlim, char *in_label,
    char *in_units) ;

extern AxisUnit  *SDTCopyConstructAxisUnit (AxisUnit *in_axisun) ;

extern int SDTFillAxisUnit (AxisUnit *axisun, int scale_type,
    int changeable, int autoscale, double in_minlim, double in_maxlim,
    char *in_label, char *in_units) ;

extern int  SDTCopyAxisUnit (AxisUnit *in_axisun, AxisUnit *out_axisun) ;

extern int  SDTDestructAxisUnit (AxisUnit *in_axisun) ;

extern int  SDTClearAxisUnit (AxisUnit *in_axisun) ;

extern int  SDTPrintAxisUnit (AxisUnit *in_axisun, FILE *ofp) ;


extern AxisUnitList *SDTConstructAxisUnitList (int naxisun,
    int *scale_type, int *changeable, int *autoscale,
    double *in_minlim, double *in_maxlim,
    char **in_label, char **in_units) ;

extern AxisUnitList  *SDTCopyConstructAxisUnitList (
    AxisUnitList *in_axisunl) ;

extern int SDTFillAxisUnitList (AxisUnitList *axisunl, int naxisun,
    int *scale_type, int *changeable, int *autoscale,
    double *in_minlim, double *in_maxlim,
    char **in_label, char **in_units) ;

extern int  SDTCopyAxisUnitList (AxisUnitList *in_axisunl,
    AxisUnitList *out_axisunl) ;

extern int  SDTDestructAxisUnitList (AxisUnitList *axisunl) ; 

extern int  SDTClearAxisUnitList (AxisUnitList *axisunl) ;

extern int  SDTArrayFillAxisUnitList (int naxisun, AxisUnit *in_axisun,
    AxisUnitList *out_axisunl) ;

extern int SDTPtrArrayFillAxisUnitList (int naxisun, AxisUnit **in_axisun,
    AxisUnitList *out_axisunl) ;

extern int  SDTPrintAxisUnitList (AxisUnitList *axisunl, FILE *ofp) ;


extern ColorLine *SDTConstructColorLine (int use_flg, double in_low,
    double in_high) ;

extern ColorLine  *SDTCopyConstructColorLine (ColorLine *in_cline) ;

extern int SDTFillColorLine (ColorLine *cline, int use_flg,
    double in_low, double in_high) ;

extern int  SDTCopyColorLine (ColorLine *in_cline, ColorLine *out_cline) ;

extern int  SDTDestructColorLine (ColorLine *in_cline) ;

extern int  SDTClearColorLine (ColorLine *in_cline) ;

extern int  SDTPrintColorLine (ColorLine *in_cline, FILE *ofp) ;


extern PlotQuantityDescription *SDTConstructPlotQuantityDescription (
    char *in_title, int in_ptype, int in_phu, int in_rstrct,
    DataQuantityDescriptionList *in_dqdl,
    AxisUnitList *in_axes, DrawingUnitList *in_drawl,
    ColorLine *in_yline, ColorLine *in_rline) ;

extern PlotQuantityDescription  *SDTCopyConstructPlotQuantityDescription (
    PlotQuantityDescription *in_pqd) ;

extern int  SDTFillPlotQuantityDescription (
    PlotQuantityDescription *in_pqd,
    char *in_title, int in_ptype, int in_phu, int in_rstrct,
    DataQuantityDescriptionList *in_dqdl,
    AxisUnitList *in_axes, DrawingUnitList *in_drawl,
    ColorLine *in_yline, ColorLine *in_rline) ;

extern int  SDTCopyPlotQuantityDescription (
    PlotQuantityDescription *in_pqd, PlotQuantityDescription *out_pqd) ;

extern int  SDTDestructPlotQuantityDescription (
    PlotQuantityDescription *in_pqd) ;

extern int  SDTClearPlotQuantityDescription (
    PlotQuantityDescription *in_pqd) ;

extern int  SDTPrintPlotQuantityDescription (
    PlotQuantityDescription *in_pqd, FILE *ofp) ;


extern PlotQuantityDescriptionList
    *SDTConstructPlotQuantityDescriptionList (
    int nplots, char **in_tlist, int *in_ptype, int *in_phu,
    int *in_rstrct, DataQuantityDescriptionList *in_dqdl,
    AxisUnitList *in_axes, DrawingUnitList *in_drawl,
    ColorLine *in_yline, ColorLine *in_rline) ;

extern PlotQuantityDescriptionList 
    *SDTCopyConstructPlotQuantityDescriptionList (
    PlotQuantityDescriptionList *in_pqdl) ;

extern int  SDTFillPlotQuantityDescriptionList (
    PlotQuantityDescriptionList *pqdl,
    int nplots, char **in_tlist, int *in_ptype, int *in_phu,
    int *in_rstrct, DataQuantityDescriptionList *in_dqdl,
    AxisUnitList *in_axes, DrawingUnitList *in_drawl,
    ColorLine *in_yline, ColorLine *in_rline) ;

extern int  SDTCopyPlotQuantityDescriptionList (
    PlotQuantityDescriptionList *in_pqdl,
    PlotQuantityDescriptionList *out_pqdl) ;

extern int  SDTDestructPlotQuantityDescriptionList (
    PlotQuantityDescriptionList *pqdl) ;

extern int  SDTClearPlotQuantityDescriptionList (
    PlotQuantityDescriptionList *pqdl) ;

extern int  SDTArrayFillPlotQuantityDescriptionList (int npqd,
    PlotQuantityDescription *in_pqd, PlotQuantityDescriptionList *out_pqdl) ;

extern int  SDTPtrArrayFillPlotQuantityDescriptionList (int npqd,
    PlotQuantityDescription **in_pqd,
    PlotQuantityDescriptionList *out_pqdl) ;

extern int  SDTPrintPlotQuantityDescriptionList (
    PlotQuantityDescriptionList *pqdl, FILE *ofp) ;

extern DataPlotQuantityDescription
    *SDTConstructDataPlotQuantityDescription (
       DataQuantityDescriptionList *in_dqdl,
       PlotQuantityDescriptionList *in_pqdl) ;
extern int  SDTDestructDataPlotQuantityDescription (
    DataPlotQuantityDescription *in_dpqd) ;

/* For the Yacc/Lex DQD and PQD parsing routines: */
extern int  dqdparse (void) ;
extern int  pqdparse (void) ;

extern FILE  *init_dqd_session (char *fname) ;
extern FILE  *init_pqd_session (char *fname) ;
extern DataQuantityDescriptionList  *EndDQDParsing (
    ParseDQDExtraInfo *deptr) ;
extern PlotQuantityDescriptionList  *EndPQDParsing (
    ParsePQDExtraInfo *peptr) ;
extern int  SetDQDGenerationList (DataQuantityDescriptionList *dqdl) ;

extern int  SDTParseQuantities (char *dqd_fname, char *pqd_fname,
    DataQuantityDescriptionList **dqdl, ParseDQDExtraInfo *deptr,
    PlotQuantityDescriptionList **pqdl, ParsePQDExtraInfo *peptr) ;

extern int SDTParseDQDFile (char *dqd_fname,
    DataQuantityDescriptionList **dqdl, ParseDQDExtraInfo *deptr) ;

extern int SDTParsePQDFile (char *pqd_fname,
    DataQuantityDescriptionList *dqdl,
    PlotQuantityDescriptionList **pqdl, ParsePQDExtraInfo *peptr) ;

extern char *SDTEncodeDQDSubComString (ComponentUseList *culist,
    ComponentAccumulationList *calist, int *ecode) ;

extern int SDTParseDQDSubComString (char *in_str,
    ComponentUseList *culist, ComponentAccumulationList *calist) ;

extern int SDTCreateParameterNamePreamble (
    DataQuantityDescription *dqdp, char *SpaceCraftIdBuf) ;

extern int SDTAddProcessToClearScript (int ipid, int irpc,
    int irver, char *fname, char *idir) ;
extern int SDTRemoveProcessFromClearScript (int ipid, int irpc,
    int irver, char *fname, char *idir) ;
extern int SDTPrintClearScript (char *fname, char *idir, FILE *ofp) ;

extern int SDTQueryCurrentSDTJobs (int JobTypeToGet, int ThisUserOnly,
    int *NJobs, int **SdtIdx, char **SdtType, char ***SdtTime,
    int **SdtPid, char ***SdtUser, char ***SdtDir) ;

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* SDTDESCRIPTION_H */
