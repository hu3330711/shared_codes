/* ------------------------------------------------------------- */
/*
 * ThemisL1Data.h
 *
 * These are the declarations required for handling THEMIS L1 data.
 *
 */

#ifndef THEMISL1DATA_H
#define THEMISL1DATA_H

/* Here is the SCCS ID Keyword string 
 * (see page 99 of "Programming Utilities and Libraries").
 */
#define SccsId_ThemisL1Data_h "@(#)ThemisL1Data.h	1.14, 01/20/08"

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <errno.h>
#include <dirent.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <cdf.h>

#ifdef UTILIZE_SUN_THREADS
#include <thread.h>
#else
#include <pthread.h>
#endif

/* ------------------------------------------------------------- */
/* Constants. */

/* The Julian Day of Jan 1 1950: */
#define   JAN_1_1950_JULIAN_DAY            2433283

/* The Julian Day of Jan 1 1958: */
#define   JAN_1_1958_JULIAN_DAY            2436205

/* The Julian Day of Jan 1 1970: */
#define   JAN_1_1970_JULIAN_DAY            2440588

/* The Julian Day of Jan 1 2000: */
#define   JAN_1_2000_JULIAN_DAY            2451545

/* The Julian Day of Jan 1 2001: */
#define   JAN_1_2001_JULIAN_DAY            2451911

/* The Julian Day of Jan 1 2100: */
#define   JAN_1_2100_JULIAN_DAY            2488070

/* The Modified Julian Day (JD2000) time of Jan 1 1950 (0 GMT) : */
#define   JAN_1_1950_JD2000_TIME           -18262.0

/* The Modified Julian Day (JD2000) time of Jan 1 1958 (0 GMT) : */
#define   JAN_1_1958_JD2000_TIME           -15340.0

/* The Modified Julian Day (JD2000) time of Jan 1 1970 (0 GMT) : */
#define   JAN_1_1970_JD2000_TIME           -10957.0

/* The Modified Julian Day (JD2000) time of Jan 1 2100 (0 GMT) : */
#define   JAN_1_2100_JD2000_TIME           36525.0

/* The number of seconds between 1970/01/01 and 2000/01/01: */
#define   SECS_1970_2000                   946684800

/* Radians per Degree: */
#define   RAD_PER_DEG                      0.017453292

/* Degrees per Radian: */
#define   DEG_PER_RAD                      57.29578122

/* Earth's equatorial radius (KM): */
#define    EARTH_EQUATORIAL_RADIUS         6378.135

/* Earth's mean radius (KM): */
#define    EARTH_MEAN_RADIUS               6371.2

/* ------------------------------------------------------------- */
/* Themis L1 spacecraft ids: */

#define    THMA_SC_ID                        0

#define    THMB_SC_ID                        1

#define    THMC_SC_ID                        2

#define    THMD_SC_ID                        3

#define    THME_SC_ID                        4

#define    THMG_SC_ID                        5

/* ------------------------------------------------------------ */
/* DFB QTY LVL1 indices: */

extern const int THMS_V1_LVL1_QTY_IDX ;
extern const int THMS_V2_LVL1_QTY_IDX ;
extern const int THMS_V3_LVL1_QTY_IDX ;
extern const int THMS_V4_LVL1_QTY_IDX ;
extern const int THMS_V5_LVL1_QTY_IDX ;
extern const int THMS_V6_LVL1_QTY_IDX ;

extern const int THMS_E12_LVL1_QTY_IDX ;
extern const int THMS_E34_LVL1_QTY_IDX ;
extern const int THMS_E56_LVL1_QTY_IDX ;

extern const int THMS_SCM1_LVL1_QTY_IDX ;
extern const int THMS_SCM2_LVL1_QTY_IDX ;
extern const int THMS_SCM3_LVL1_QTY_IDX ;

/* ------------------------------------------------------------ */
/* DFB QTY LVL2 indices: */

extern const int THMS_V123456_A_FS_IDX ;
extern const int THMS_V123456_B_FS_IDX ;
extern const int THMS_V123456_A_PB_IDX ;
extern const int THMS_V123456_B_PB_IDX ;
extern const int THMS_V123456_A_WB_IDX ;
extern const int THMS_V123456_B_WB_IDX ;

extern const int THMS_EAC_FS_IDX ;
extern const int THMS_EAC_PB_IDX ;
extern const int THMS_EAC_WB_IDX ;
extern const int THMS_EDC_FS_IDX ;
extern const int THMS_EDC_PB_IDX ;
extern const int THMS_EDC_WB_IDX ;
extern const int THMS_EACDotB_IDX ;
extern const int THMS_EACxB_IDX ;
extern const int THMS_EDCDotB_IDX ;
extern const int THMS_EDCxB_IDX ;

extern const int THMS_SCM_FS_IDX ;
extern const int THMS_SCM_PB_IDX ;
extern const int THMS_SCM_WB_IDX ;
extern const int THMS_SCMDotB_IDX ;
extern const int THMS_SCMxB_IDX ;

/* ------------------------------------------------------------- */
/* Themis L1 GroundStation IDs: */

#define    THMG_NMB_STA                     31

#define    THMG_BMLS_ID                      0

#define    THMG_CCNS_ID                      1

#define    THMG_CHBG_ID                      2

#define    THMG_DRBY_ID                      3

#define    THMG_EKAT_ID                      4

#define    THMG_FSIM_ID                      5

#define    THMG_FSMI_ID                      6

#define    THMG_FYKN_ID                      7

#define    THMG_FYTS_ID                      8

#define    THMG_GAKO_ID                      9

#define    THMG_GARAGE_ID                   10

#define    THMG_GBAY_ID                     11

#define    THMG_GILL_ID                     12

#define    THMG_HOTS_ID                     13

#define    THMG_INUV_ID                     14

#define    THMG_KAPU_ID                     15

#define    THMG_KIAN_ID                     16

#define    THMG_LOYS_ID                     17

#define    THMG_MCGR_ID                     18

#define    THMG_PGEO_ID                     19

#define    THMG_PINA_ID                     20

#define    THMG_PINE_ID                     21

#define    THMG_PTRS_ID                     22

#define    THMG_RANK_ID                     23

#define    THMG_RMUS_ID                     24

#define    THMG_SWNO_ID                     25

#define    THMG_TAHO_ID                     26

#define    THMG_TBD1_ID                     27

#define    THMG_TPAS_ID                     28

#define    THMG_UKIA_ID                     29

#define    THMG_WHIT_ID                     30

/* ------------------------------------------------------------- */
/* Themis L1 Data File types: */

#define    THM_STATE_L1_DATA_TYPE            0
#define    THM_FGM_L1_DATA_TYPE              1
#define    THM_EVCMB_L1_DATA_TYPE            2
#define    THM_EFF_L1_DATA_TYPE              3
#define    THM_EFP_L1_DATA_TYPE              4
#define    THM_EFW_L1_DATA_TYPE              5
#define    THM_VAF_L1_DATA_TYPE              6
#define    THM_VBF_L1_DATA_TYPE              7
#define    THM_VAP_L1_DATA_TYPE              8
#define    THM_VBP_L1_DATA_TYPE              9
#define    THM_VAW_L1_DATA_TYPE             10
#define    THM_VBW_L1_DATA_TYPE             11
#define    THM_GND_B_L1_DATA_TYPE           13
#define    THM_EFI_HK1_L1_DATA_TYPE         14
#define    THM_EFI_HK2_L1_DATA_TYPE         15
#define    THM_FITS_L1_DATA_TYPE            16
#define    THM_MOM_L1_DATA_TYPE             17
#define    THM_FGM_L2_DATA_TYPE             19

/* ------------------------------------------------------------- */
/* Themis L1 EFF, EFP, EFW Data types: */

#define    THM_L1_EFLD_DATA                  0

/* Themis L1 EFF, EFP, EFW Data sub-types: */

/* This is the raw, straight-out-of-the APID data (V): */
#define    THM_L1_EFLD_RAW_DTYPE             0

/* This is the computed, but not despun, efield, requiring
 * knowledge of the boon lengths (mV/m):
 */
#define    THM_L1_EFLD_EFIELD_DTYPE          1

/* ------------------------------------------------------------- */
/* Themis L1 FGM SubData types: */
/* Note that FGC, if implemented, will be a combined quantity
 * always transmitting the highest available rate.
 */

#define    THM_L1_FGE_DATA_TYPE              0
#define    THM_L1_FGL_DATA_TYPE              1
#define    THM_L1_FGH_DATA_TYPE              2
#define    THM_L1_FGC_DATA_TYPE              3

/* ------------------------------------------------------------- */
/* Themis L2 FGM SubData types: */
/* Note that FGC, if implemented, will be a combined quantity
 * always transmitting the highest available rate - but, will NOT
 * EVER include the "S" data.
 */

#define    THM_L2_FGE_DATA_TYPE              0
#define    THM_L2_FGL_DATA_TYPE              1
#define    THM_L2_FGH_DATA_TYPE              2
#define    THM_L2_FGC_DATA_TYPE              3
#define    THM_L2_FGS_DATA_TYPE              4

/* Themis L2 FGM Available Coordinate types: */
#define    THM_L2_FGM_SSL_COORD_TYPE         0
#define    THM_L2_FGM_DSL_COORD_TYPE         1
#define    THM_L2_FGM_GSE_COORD_TYPE         2
#define    THM_L2_FGM_GSM_COORD_TYPE         3

/* The number of coordinate types available in FGM L2 files: */
#define    THM_L2_FGM_NMB_COORDS             4

/* ------------------------------------------------------------- */
/* Themis L1 State SubData types: */

#define    THM_STATE_L1_POSITION_TYPE        0
#define    THM_STATE_L1_VELOCITY_TYPE        1
#define    THM_STATE_L1_SPIN_RASC_TYPE       2
#define    THM_STATE_L1_SPIN_DEC_TYPE        3
#define    THM_STATE_L1_SPIN_PER_TYPE        4
#define    THM_STATE_L1_SPIN_PHASE_TYPE      5

/* ------------------------------------------------------------- */
/* Themis L1 Fits SubData types: */
#define    THM_FITS_L1_EFLD_TYPE             0
#define    THM_FITS_L1_BFLD_TYPE             1

/* ------------------------------------------------------------- */
/* Structures: */

/* For FGM data; */
struct L1IndexRangeInFiles_struct 
    {
    int     nfiles ;
    int     scid ;
    int     subtype ;
    int     dtype ;
    int     dtype2 ;
    double  stime_unix ;
    double  etime_unix ;
    char    **fnames ;
    long    *sidx ;
    long    *eidx ;
    long    *npts ;
    } ;
typedef  struct L1IndexRangeInFiles_struct  L1IndexRangeInFiles ;

/* This returns GEI position, Velocity, SpinAxis data from
 * the "State" CDF file.   Note that SunPulse data is returned,
 * from the "State" file, but in the "ThemisL1SunPulseData"
 * structure.
 */
struct ThemisL1StateData_struct
    {
    long      npts ;
    double    *ttags ;
    float     *px ;
    float     *py ;
    float     *pz ;
    float     *vx ;
    float     *vy ;
    float     *vz ;
    float     *ax ;
    float     *ay ;
    float     *az ;
    float     *spin_ra ;
    float     *spin_dec ;
    float     *spin_per ;
    float     *spin_phase ;
    } ;
typedef  struct ThemisL1StateData_struct  ThemisL1StateData ;

struct ThemisL1SunPulseData_struct
    {
    long      npts ;
    double    *ttags ;
    float     *spin_per ;
    } ;
typedef  struct ThemisL1SunPulseData_struct  ThemisL1SunPulseData ;

/* FGM sampling-rate data; */
struct FGMHdrCalibrationData_struct
    {
    int            nrecs ;
    long           *sidx ;
    long           *eidx ;
    int            *samp_rate ;
    unsigned char  *samp_rate_bits ;
    int            *filter_mode ;
    double         *stime ;
    double         *etime ;
    } ;
typedef  struct FGMHdrCalibrationData_struct  FGMHdrCalibrationData ;

/* For FGM data; */
struct ThemisL1FGMData_struct
    {
    long    nfpts ;
    int     dtype2 ;  /* 0 -> FGE, 1 -> FGL, 2 -> FGH, 3 ->FGC */
    double  TOffset ;
    double  *ft ;
    int     *fx ;
    int     *fy ;
    int     *fz ;
    FGMHdrCalibrationData *sr ;
    } ;
typedef  struct ThemisL1FGMData_struct  ThemisL1FGMData ;

/* For FGM L2 data; */
struct ThemisL2FGMData_struct
    {
    long    nfpts ;
    int     dtype2 ;  /* 0 -> FGE, 1 -> FGL, 2 -> FGH, 3 ->FGC,
                       * 4 -> FGS (spin-fit)
		       */
    int     ctype ;   /* 0 -> SSL, 1 -> DSL, 2 -> GSE, 3 ->GSM */
    double  TOffset ;
    double  *ft ;
    float    *fx ;
    float    *fy ;
    float    *fz ;
    } ;
typedef  struct ThemisL2FGMData_struct  ThemisL2FGMData ;

/* For EField data; */

/* For extracting EFI "state" information from packet hdrs: */
struct EFIHdrStateData_struct
    {
    int           dtype ;
    int           dtype2 ;
    int           nrecs ;
    double        TOffset ;
    double        *sutime ;
    double        *eutime ;
    long          *sidx ;
    long          *eidx ;

    int           *samp_rate ;

    /* To be used in conjunction with a "L1IndexRangeInFiles". */
    int           *fidx ;

    /* For the single probe qtys, VA(FPW), VB(FPW), all six
     * flags are used (0 -> V1, ... 5 -> V6).
     * For the double probe qtys, EF(FPW), only the first three
     * flags are used (0 -> E12,  1 -> E34,  2 -> E56)
     */
    int           *PFlg[6] ;
    } ;
typedef   struct EFIHdrStateData_struct   EFIHdrStateData ;

/* Used for Efi single and double probe data qtys to speed
 * up data extraction.
 */
struct EFIDRanges_struct
    {
    int   nrecs ;

    int   *lidx ;    /* which LInfo. */
    int   *fidx ;    /* which file within Linfo "lidx". */
    int   *ridx ;    /* which record in the "EFIHdrStateData" */
    long  *sidx ;    /* the start index. */
    long  *eidx ;    /* the endi index. */
    } ;
typedef   struct EFIDRanges_struct   EFIDRanges ;

/* -----------------------------------------------------------*/
/* For the combined single or double probe Efld data: */

/* Conjoins a List of files, for a particular APID (eff,
 * efp, efw, vaf, vbf, vap, vbp, vaw, vbw) and the
 * corresponding sampling history for that list.
 */
struct EFICmbNavigator_struct
    {
    struct EFICmbNavigator_struct   *prev ;
    struct EFICmbNavigator_struct   *next ;
    int        scid ;
    int        dtype ;
    int        dtype2 ;
    int        subtype ;
    int        dstyp ;
    int        chidx ;
    int        cid_open ;
    CDFid      cid ;
    double     TOffset ;
    double     ctime ;

    L1IndexRangeInFiles  *LInfo ;
    EFIHdrStateData      *Hist ;
    } ;
typedef   struct EFICmbNavigator_struct   EFICmbNavigator ;

/* A list of "EFICmbNavigator"'s.  Is used to find the
 * correct sequence of data types and the corrsponding
 * indices for the highest available data rate.
 */
struct EFICmbNavigatorList_struct
    {
    int              nelts ;
    EFICmbNavigator  *first ;
    EFICmbNavigator  *last ;
    int        scid ;
    int        dtype ;
    int        dtype2 ;
    int        subtype ;
    int        dstyp ;
    } ;
typedef   struct EFICmbNavigatorList_struct   EFICmbNavigatorList ;

struct EFICmbFileToDataOrder_struct
    {
    int       nrecs ;
    long      *sidx ;
    long      *eidx ;
    double    *stime ;
    double    *etime ;
    int       *samp_rate ;
    int       *fidx ;

    int       nfiles ;
    char      **fnames ;
    int       *OpenFlag ;
    int       *LastDRec ;
    int       *dtype ;
    CDFid     *cid ;
    } ;
typedef   struct EFICmbFileToDataOrder_struct   EFICmbFileToDataOrder ;

struct ThemisL1EFieldData_struct
    {
    long    npts ;
    int     dtype ;
    int     ncomponents ;   /* Does NOT include the time comp. */
    double  TOffset ;
    double  *ft ;
    short   *d[6] ;
    } ;
typedef  struct ThemisL1EFieldData_struct  ThemisL1EFieldData ;

/* For Ground mag data; */

struct ThemisL1GrndStationMag_struct
    {
    long      nfpts ;
    double    *ft ;
    float     *fx ;
    float     *fy ;
    float     *fz ;
    } ;
typedef  struct ThemisL1GrndStationMag_struct  ThemisL1GrndStationMag ;

/* ------------------------------------------------------------- */
/* Calibration structures: */

struct ThemisMatchIndices_struct
    {
    int  nranges ;
    long   *sidx ;
    long   *eidx ;
    } ;
typedef  struct ThemisMatchIndices_struct  ThemisMatchIndices ;

/* FGM: */
struct ThemisFGMClb_struct
    {
    char *fname ;         /* Full pathname of the cal file used. */
    int  scid ;           /* 0 -> "tha", ..., 4 -> "the" */
    int  nrecs ;          /* Number of records. */
    double  TOffset ;     /* Number of seconds to offset "sutime",
                           * "eutime", from 1970 time.
			   */
    double  *sutime ;     /* Start time of record, in 1970 secs -
			   * - TOffset.
			   */
    double  *eutime ;     /* End time of record, in 1970 secs -
			   * - TOffset.
			   */
    double  *offs ;       /* X,Y,Z offsets */
    double  *cmat ;       /* orthog matrices */
    double  *spinper ;    /* Spin-period (secs) */
    } ;
typedef  struct ThemisFGMClb_struct  ThemisFGMClb ;

/* EFields: */
struct ThemisEFieldClb_struct
    {
    /* The value of "ncomps" will be:
     *
     *    6:  for single probe V1 - V6 apids (441, 442, 445, 446,
     *        449, 44A), and all of the "gain", "offset" elements
     *        will be used.
     *   
     *    3:  for double probe Efield apids (443, 447, 448),
     *        and the values are in entries 0-2 of "gain" and
     *        "offset".
     */
    int       ncomps ;
    double    gain[6] ;
    double    offset[6] ;
    } ;
typedef  struct ThemisEFieldClb_struct  ThemisEFieldClb ;

/* ------------------------------------------------------------- */
/* The CDF variable names that we need to know.  As of 2007/04/07,
 * it is assumed that the actual CDF variable names are prepended by:
 *
 *  "tha_", "thb_", ..., "tbe_"
 *
 */

extern const char *Thm_Scid_Lcase[6] ;
extern const char *Thm_Scid_Ucase[6] ;
extern const char *Thmg_Stations_Lcase[31] ;
extern const char *Thmg_Stations_Ucase[31] ;

extern const char *Thm_DType_Lcase[] ;
extern const char *Thm_DType_Ucase[] ;

extern const char *Thm_FGM_ClbName ;

/* The FGM raw data and timing: */
extern const char *fge_name ;
extern const char *fge_time_name ;

/* The FGM header data and header timing: */
extern const char *fge_hed_name ;
extern const char *fge_hed_time_name ;

extern const int  Hk2_Qty_NComps[7] ;
extern const char *Thm_Hk2_Qty_Lcase[7] ;
extern const char *Thm_Fits_Qty_Lcase[3] ;

/* ------------------------------------------------------------- */
/* Function declarations: */

extern int ThemisGetL1DataFileNames (int ndirs, char **dlist,
    int ndates, char **idates, int scid, int subtype, int dtype,
    int *nfiles, char ***onames) ;

extern char *ThemisFindFileInDirectory (char *dname, char *spcrft,
    char *lvl, char *instr, char *gstation, char *date,
    int *iversion, int *ecode) ; 

extern ThemisL1StateData *GetThemisL1StateData (
    L1IndexRangeInFiles *rngs, double TOffset, int *ecode) ;
extern int DestructThemisL1StateData (ThemisL1StateData *data) ;
extern int ClearThemisL1StateData (ThemisL1StateData *data) ;
extern int PrintThemisL1StateData (ThemisL1StateData *data,
    FILE *ofp) ;

extern ThemisL1SunPulseData *GetThemisL1SunPulseAlg1 (
    ThemisL1StateData *sdata, double TOffset, int *ecode) ;
extern ThemisL1SunPulseData *GetThemisL1SunPulseAlg2 (
    char **spmod_files, int  nsps, double TOffset,
    long expected_npts, int *ecode) ;
extern int DestructThemisL1SunPulseData (
    ThemisL1SunPulseData *sdata) ;
extern int ClearThemisL1SunPulseData (
    ThemisL1SunPulseData *sdata) ;
extern int PrintThemisL1SunPulseData (
    ThemisL1SunPulseData *sdata, FILE *ofp) ;

extern ThemisL1FGMData *GetThemisL1FGMData (
    L1IndexRangeInFiles *rngs, double TOffset, int *ecode) ;

extern ThemisL2FGMData *GetThemisL2FGMData (
    L1IndexRangeInFiles *rngs, int coords, double TOffset,
    int *ecode) ;

extern L1IndexRangeInFiles *ThemisGetFileIndicesByTime (
    int nfiles, char **flist, double STime, double ETime, int scid,
    int subtype, int dtype, int dtype2, int *ecode) ;
extern L1IndexRangeInFiles *CopyConstructL1IndexRangeInFiles (
    L1IndexRangeInFiles *idata) ;
extern int CopyL1IndexRangeInFiles (L1IndexRangeInFiles *idata,
    L1IndexRangeInFiles *odata) ;
extern int DestructL1IndexRangeInFiles (L1IndexRangeInFiles *data) ;
extern int ClearL1IndexRangeInFiles (L1IndexRangeInFiles *data) ;
extern int PrintL1IndexRangeInFiles (L1IndexRangeInFiles *data,
    FILE *ofp) ;
extern L1IndexRangeInFiles *ThemisGetFileIndicesInDataDirectories (
    int ndirs, char **dlist, double STime, double ETime, int scid,
    int gsid, int dtype, int dtype2, int *ecode) ;
extern long CountL1IndexRangeInFiles (L1IndexRangeInFiles *idata) ;

extern int SimpleThemisGetTimeIndex (long tvarn, double itime,
    long sidx, long NPts, long *r_idx, double *r_time,
    int *r_code) ;
extern int GetClosestTimeIndices (long tvarn, double itime,
    long sidx, long NPts, long *r_sidx, long *r_eidx,
    double *r_stime, double *r_etime) ;

extern FGMHdrCalibrationData *ConstructFGMHdrCalibrationData (
    int nrecs, long *sidx, long *eidx, int *srates,
    unsigned char *sr_idx, int *fmodes,
    double *stimes, double *etimes) ;
extern FGMHdrCalibrationData *CopyConstructFGMHdrCalibrationData (
    FGMHdrCalibrationData *idata) ;
extern int CopyFGMHdrCalibrationData (FGMHdrCalibrationData *idata,
    FGMHdrCalibrationData *odata) ;
extern int DestructFGMHdrCalibrationData (
    FGMHdrCalibrationData *data) ;
extern int ClearFGMHdrCalibrationData (FGMHdrCalibrationData *data) ;
extern int PrintFGMHdrCalibrationData (FGMHdrCalibrationData *data,
    FILE *ofp) ;

extern ThemisL1FGMData *CopyConstructThemisL1FGMData (
    ThemisL1FGMData *idata) ;
extern int CopyThemisL1FGMData (ThemisL1FGMData *idata,
    ThemisL1FGMData *odata) ;
extern int DestructThemisL1FGMData (ThemisL1FGMData *data) ;
extern int ClearThemisL1FGMData (ThemisL1FGMData *data) ;
extern int PrintThemisL1FGMData (ThemisL1FGMData *data, FILE *ofp) ;

extern ThemisL2FGMData *CopyConstructThemisL2FGMData (
    ThemisL2FGMData *idata) ;
extern int CopyThemisL2FGMData (ThemisL2FGMData *idata,
    ThemisL2FGMData *odata) ;
extern int DestructThemisL2FGMData (ThemisL2FGMData *data) ;
extern int ClearThemisL2FGMData (ThemisL2FGMData *data) ;
extern int PrintThemisL2FGMData (ThemisL2FGMData *data, FILE *ofp) ;

extern ThemisL1FGMData *GetThemisFgcData (ThemisL1FGMData *dfge,
    ThemisL1FGMData *dfgl, ThemisL1FGMData *dfgh,
    L1IndexRangeInFiles *rfge, L1IndexRangeInFiles *rfgl,
    L1IndexRangeInFiles *rfgh) ;

extern ThemisL2FGMData *GetThemisL2FgcData (ThemisL2FGMData *dfge,
    ThemisL2FGMData *dfgl, ThemisL2FGMData *dfgh) ;

extern ThemisL1EFieldData *GetThemisL1EFieldData (
    L1IndexRangeInFiles *rngs, double TOffset, int *ecode) ;
extern ThemisL1EFieldData *CopyConstructThemisL1EFieldData (
    ThemisL1EFieldData *idata) ;
extern int CopyThemisL1EFieldData (ThemisL1EFieldData *idata,
    ThemisL1EFieldData *odata) ;
extern int DestructThemisL1EFieldData (ThemisL1EFieldData *data) ;
extern int ClearThemisL1EFieldData (ThemisL1EFieldData *data) ;
extern int PrintThemisL1EFieldData (ThemisL1EFieldData *data,
    FILE *ofp) ;

extern ThemisL1GrndStationMag *GetThemisL1GrndStationMag (
    L1IndexRangeInFiles *rngs, double TOffset, int *ecode) ;
extern int DestructThemisL1GrndStationMag (
    ThemisL1GrndStationMag *data) ;
extern int ClearThemisL1GrndStationMag (
    ThemisL1GrndStationMag *data) ;
extern int PrintThemisL1GrndStationMag (
    ThemisL1GrndStationMag *data, FILE *ofp) ;

extern int ConvertJulianDayToCalendar (long julian_day,
    int *year, int *month, int *day, int *day_of_year) ;

extern long ConvertCalendarToJulianDay (int year, int month,
    int day) ;

extern int ConvertUnixTimeToJulianDateTime (double isecs, long *jday,
    double *rsecs) ;

extern int ConvertJulianDateTimeToUnixTime (long jday, double dsecs,
    double *rsecs) ;

extern int CheckCDFforGoAhead (CDFstatus rstat) ;

extern void EPOCH_breakdown (double epoch, long *year, long *month,
    long *day, long *hour, long *minute, long *second, long *msec) ;

/* ------------------------------------------------------------- */
/* Calibration routines: */

extern ThemisMatchIndices *GetFgmCalibrationRanges (long nttags,
    double *itimes, ThemisFGMClb *ifc, int *ecode) ;

extern ThemisFGMClb *GetThemisFGMCalibrations (int ndirs,
    char **dlist, double STime, double ETime, int scid,
    double TOffs, int *ecode) ;

extern ThemisFGMClb *ContructThemisFGMClb () ;

extern int DestructThemisFGMClb (ThemisFGMClb *CInfo) ;

extern int ClearThemisFGMClb (ThemisFGMClb *CInfo) ;

extern int PrintThemisFGMClb (ThemisFGMClb *CInfo, FILE *ofp) ;

extern ThemisEFieldClb *GetThemisEFieldCalibrationStruct (
    int dtype) ;

extern int DestructThemisEFieldClb (ThemisEFieldClb *CInfo) ;

extern int ClearThemisEFieldClb (ThemisEFieldClb *CInfo) ;

extern int PrintThemisEFieldClb (ThemisEFieldClb *CInfo,
    FILE *ofp) ;



extern ThemisMatchIndices *GetGeneralCalibrationRanges (
    long nttags, double *itimes, int nrngs, double *stimes,
    double *etimes, int *ecode) ;

extern ThemisMatchIndices *ContructThemisMatchIndices () ;

extern int DestructThemisMatchIndices (ThemisMatchIndices *MInfo) ;

extern int ClearThemisMatchIndices (ThemisMatchIndices *MInfo) ;

extern int PrintThemisMatchIndices (ThemisMatchIndices *MInfo,
    FILE *ofp) ;



extern int ExtractEFIData (L1IndexRangeInFiles **rngs, int nrngs,
    EFIDRanges *esd, int dstyp, double TOffset, long maxv,
    double *tv, short *sv, long *nval) ;
extern EFIHdrStateData *ThemisGetEFIHdrState (
    L1IndexRangeInFiles *rngs, double TOffset, int *ecode) ;
extern EFIHdrStateData *CopyConstructEFIHdrStateData (
    EFIHdrStateData *idata) ;
extern int CopyEFIHdrStateData (EFIHdrStateData *idata,
    EFIHdrStateData *odata) ;
extern int DestructEFIHdrStateData (EFIHdrStateData *data) ;
extern int ClearEFIHdrStateData (EFIHdrStateData *data) ;
extern int PruneEFIHdrStateData (EFIHdrStateData *data,
    int dstyp) ;
extern int PrintEFIHdrStateData (EFIHdrStateData *data,
    FILE *ofp) ;

extern EFIDRanges *ComputeEFIReqIndices (
    L1IndexRangeInFiles *rngs, EFIHdrStateData *esd, int dstyp,
    double STime, double ETime) ;
extern EFIDRanges *CopyConstructEFIDRanges (EFIDRanges *idata) ;
extern int CopyEFIDRanges (EFIDRanges *idata, EFIDRanges *odata) ;
extern int DestructEFIDRanges (EFIDRanges *data) ;
extern int ClearEFIDRanges (EFIDRanges *data) ;
extern int PrintEFIDRanges (EFIDRanges *data, FILE *ofp) ;

extern EFICmbNavigator *ConstructEFICmbNavigator (
    L1IndexRangeInFiles *i_LInfo, EFIHdrStateData *i_Hist,
    double  TOffset, int i_dstyp) ;
extern EFICmbNavigator *CopyConstructEFICmbNavigator (
    EFICmbNavigator *idata) ;
extern int CopyEFICmbNavigator (EFICmbNavigator *idata,
    EFICmbNavigator *odata) ;
extern int DestructEFICmbNavigator (EFICmbNavigator *data) ;
extern int ClearEFICmbNavigator (EFICmbNavigator *data) ;
extern int PrintEFICmbNavigator (EFICmbNavigator *data,
    FILE *ofp) ;

extern EFICmbNavigatorList *ConstructEFICmbNavigatorList (
    int in_scid, int in_dtype, int in_dtype2, int in_subtype,
    int in_dstype) ;
extern EFICmbNavigatorList *CopyConstructEFICmbNavigatorList (
    EFICmbNavigatorList *idata) ;
extern int CopyEFICmbNavigatorList (EFICmbNavigatorList *idata,
    EFICmbNavigatorList *odata) ;
extern int DestructEFICmbNavigatorList (
    EFICmbNavigatorList *data) ;
extern int ClearEFICmbNavigatorList (
    EFICmbNavigatorList *data) ;
extern int PrintEFICmbNavigatorList (EFICmbNavigatorList *data,
    FILE *ofp) ;
extern int AddToEFICmbNavigatorList (EFICmbNavigatorList *list,
    EFICmbNavigator *nnl) ;
extern int DeleteFromEFICmbNavigatorList (
    EFICmbNavigatorList *list, EFICmbNavigator *nnl) ;
extern int ReInsertEFICmbNavigatorList (
    EFICmbNavigatorList *list, EFICmbNavigator *nnl) ;
extern EFICmbFileToDataOrder *ProcessEFICmbNavigatorList (
    EFICmbNavigatorList *list, double STime, double ETime,
    int *ecode) ;

extern int AddDataToEFICmbFileToDataOrder (
    EFICmbFileToDataOrder *data, long sidx, long eidx,
    double stime, double etime, int srate, int fidx,
    int dtype, char *fname) ;

extern
    EFICmbFileToDataOrder *CopyConstructEFICmbFileToDataOrder (
    EFICmbFileToDataOrder *idata) ;
extern int CopyEFICmbFileToDataOrder (
    EFICmbFileToDataOrder *idata, 
    EFICmbFileToDataOrder *odata) ;
extern int DestructEFICmbFileToDataOrder (
    EFICmbFileToDataOrder *data) ;
extern int ClearEFICmbFileToDataOrder (
    EFICmbFileToDataOrder *data) ;
extern int PrintEFICmbFileToDataOrder (
    EFICmbFileToDataOrder *data, FILE *ofp) ;

extern int ThemisExtractHK2Data (L1IndexRangeInFiles *rngs,
    long MaxPts, double TOffset, double *tdata, float **fvals,
    int Nfv, long *NPts) ;

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif  /* THEMISL1DATA_H */

/* ------------------------------------------------------------- */
/*  end:  ThemisL1Data.h  */
