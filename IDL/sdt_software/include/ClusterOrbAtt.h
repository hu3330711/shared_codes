/* ClusterOrbAtt.h */

/* Definitions required by the "ClusterOrbAtt" module - which is
 * used to read the Cluster project orbit and attitude data files.
 */
#ifndef CLUSTERORBATT_H
#define CLUSTERORBATT_H

/* Here is the SCCS ID Keyword string 
 * (see page 99 of "Programming Utilities and Libraries").
 */
#define SccsId_ClusterOrbAtt_h "@(#)ClusterOrbAtt.h	1.18, 12/03/06"

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <memory.h>
#include <errno.h>
#include <time.h>
#include <math.h>
#include <sys/types.h>
#include <sys/time.h>

/* --------------------------------------------------------------- */
/* TypeDefs: */

#ifndef int32
typedef int     int32 ;
#endif

#ifndef uint32
typedef unsigned int     uint32 ;
#endif

/* --------------------------------------------------------------- */
/* Definitions: */

#define   CLUSTER_ORB_MAX_BLOCK_COEFFS   10

/* The number of days from Jan 1 1958 to Jan 1 2000: */
#define   CLUSTER_DAYS_FROM_1958_TO_2000   15340

/* The Julian Day of Jan 1 1950: */
#define   JAN_1_1950_JULIAN_DAY            2433283

/* The Julian Day of Jan 1 1958: */
#define   JAN_1_1958_JULIAN_DAY            2436205

/* The Julian Day of Jan 1 1970: */
#define   JAN_1_1970_JULIAN_DAY            2440588

/* The Julian Day of Jan 1 2000: */
#define   JAN_1_2000_JULIAN_DAY            2451545

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

/* The number of lines per record of covariant matrix data in the
 * files:
 */
#define   COVMATRIX_LINES   6

/* The maximum number of polynomial coefficient lines in an STOF
 * orbit record.
 */
#define   MAX_STOF_POLYCOEFFS   10

/* The output size (number of chars) of each line to the covariant
 * matrix database files:
 */
#define   OREC_SIZE         75

/* Length (including new-line) of all lines in the orbit dbase
 * spacecraft files:
 */
#define   CLS_ORB_DBASE_LINE_LENGTH   100

/* The name of the covariant matrix index file for the database: */
extern  const char *cov_mat_index_file ;

/* The names of the spacecraft covariant matrix database files
 * (one per spacecraft):
 */
extern  const char *cov_mat_names[4] ;

/* The name of the orbit history index file for the database: */
extern  const char *cls_orb_index_fil ;

/* The names of the spacecraft orbit history database files
 * (one per spacecraft):
 */
extern  const char *cls_orb_names[4] ;

/* --------------------------------------------------------------- */
/* Typedefs: */

/* Structure returning Orbit data: */
struct ClusterOrbBlock_struct
     {
     int   nrec1 ;

     /* 1, 2, 3, or 4 */
     int   spacecraft ;

     /* 0 -> predicted, 1 -> reconstructed (i.e. definitive) */
     int   reconstructed ;

     /* CCSDS time of generation: */
     char    GenTime[24] ;

     /* CCSDS start time of validity: */
     char    StartTime[24] ;

     /* CCSDS end time of validity: */
     char    EndTime[24] ;

     int  nrec2 ;

     /* JD2000 time of start time */
     double  DayBeg ;

     /* JD2000 time of end time */
     double  DayEnd ;

     /* JD2000 time of epoch for the Kepler reference orbit  */
     double  Epoch ;

     /* Revolution number of this orbit: */
     double  Orbin ;

     /* Semi-major axis, in KM, of the reference Kepler orbit. */
     double  SMaxis ;

     /* Inverse mean motion of the reference Kepler orbit. */
     double  Omotin ;

     int  nrec3 ;

     /* XYZ components of the pos. vec. (KM) of the ref K orbit. */
     double  xyzpos[3] ;

     /* XYZ components of the vel. vec. (KM/sec) of the ref K orbit. */
     double  xyzvel[3] ;

     /* Absolute "r" (KM) of the pos. vec. of the ref. K orbit. */
     double  RDist ;

     /* The number of coefficient blocks in this block: */
     /* Should be:   0  <=  ncoeffs  <=  CLUSTER_ORB_MAX_BLOCK_COEFFS */
     int  ncoeffs ;

     int     recid[CLUSTER_ORB_MAX_BLOCK_COEFFS] ;
     double  polpos[CLUSTER_ORB_MAX_BLOCK_COEFFS][3] ;
     double  polvec[CLUSTER_ORB_MAX_BLOCK_COEFFS][3] ;
     } ;
typedef   struct ClusterOrbBlock_struct   ClusterOrbBlock ;

/* Structure returning Orbit data: */
struct ClusterOrbitData_struct
     {
     /*  1, 2, 3, or 4: */
     int   Spacecraft ;

     /* 1 -> orbit propagation produced valid data
      * 0 -> orbit propagation failed, for the given time.
      *      In this case, "RevNum", "GeiPos", "GeiVel",
      *      are filled with -1.0, and "Reconstructed" is
      *      filled with 0.
      */
     int   Valid ;

     /*  1 -> reconstructed (i.e. definitive)
      *  0 -> predicted
      */
     int   Reconstructed ;

     /* Time-tag of the data in JulianDay 2000 time. */
     double  JD2000Time ;

     double  RevNum ;      /* Revolution (orbit?) number */
     double  GeiPos[3] ;   /* Spacecraft position in GEI (Km) */
     double  GeiVel[3] ;   /* Spacecraft velocity in GEI (Km/sec) */
     } ;
typedef   struct ClusterOrbitData_struct   ClusterOrbitData ;

/* Structure returning Orbit data: */
struct ClusterAttRec_struct
     {
     /* 1, 2, 3, or 4 */
     int   spacecraft ;

     /* 0 -> predicted, 1 -> reconstructed (i.e. definitive) */
     int   reconstructed ;

     /* CCSDS time of generation: */
     char    GenTime[24] ;

     /* CCSDS start time of validity: */
     char    StartTime[24] ;

     /* CCSDS end time of validity: */
     char    EndTime[24] ;

     /* JD2000 time of start time */
     double  DayBeg ;

     /* JD2000 time of end time */
     double  DayEnd ;

     /* Right ascension in GEI: */
     double  sp_rasc ;

     /* Declination in GEI: */
     double  sp_decl ;

     /* Spacecraft axis (normalized) in Cartesian GEI: */
     double  gei_cartesian[3] ;

     /* spin rate (RPM): */
     double  spin_rate ;

     /* Spacecraft phase (deg) at Sun Reference Pulse: */
     double  sp_phas ;

     /* Center of Mass position, in Cluster body build frame (mm) */
     double  com_shf[3] ;

     /* First Euler angle (deg): */
     double  tpsi_2 ;

     /* Second Euler angle (deg): */
     double  tpsi_1 ;
     } ;
typedef   struct ClusterAttRec_struct   ClusterAttRec ;

/* Structure returning Attitude data: */
struct ClusterAttitudeData_struct
     {
     /*  1, 2, 3, or 4: */
     int   Spacecraft ;

     /* 1 -> record contains valid data.
      * 0 -> record does NOT contain valid data.
      *      In this case, "RevNum", "GeiPos", "GeiVel",
      *      are filled with -1.0, and "Reconstructed" is
      *      filled with 0.
      */
     int   Valid ;

     /*  1 -> reconstructed (i.e. definitive)
      *  0 -> predicted
      */
     int   Reconstructed ;

     /* Time-tag of the data in JulianDay 2000 time. */
     double  JD2000Time ;

     /* Right ascension in GEI: */
     double  sp_rasc ;

     /* Declination in GEI: */
     double  sp_decl ;

     /* Spacecraft axis (normalized) in Cartesian GEI: */
     double  gei_cartesian[3] ;

     /* spin rate (RPM): */
     double  spin_rate ;

     /* Spacecraft phase (deg) at Sun Reference Pulse: */
     double  sp_phas ;

     /* Center of Mass position, in Cluster body build frame (mm) */
     double  com_shf[3] ;

     /* First Euler angle (deg): */
     double  tpsi_2 ;

     /* Second Euler angle (deg): */
     double  tpsi_1 ;
     } ;
typedef   struct ClusterAttitudeData_struct   ClusterAttitudeData ;

/* Structure returning Orbit data: */
struct ClusterSCETHdr_struct
     {
     /* Day, millisec, microsec, since Jan 1, 1958: */
     short   scet_day ;
     int32   scet_millis ;
     short   scet_micros ;

     /* Data source/type ID: */
     char    data_source ;

     /* Packet length: */
     int32   packet_length ;

     /* Spacecraft ID: */
     char    spacecraft_id ;

     /* Ground station ID: */
     char    ground_station_id ;

     /* Data stream: */
     char    data_stream ;

     /* Time Quality: */
     char    time_quality ;

     /* Telemetry Acquisition Sequence ID: */
     char    telemetry_acq_seq_id ;
     } ;
typedef   struct ClusterSCETHdr_struct   ClusterSCETHdr ;

/* This is the structure containing data from a project covariant
 * matrix record, after parsing:
 */
struct  ClusterCovariantCoeffs_struct
    {
    /* Spacecraft (1,2,3,4) */
    int      sc ;

    /* Flag indicating if the record is predicted (0) or
     * reconstructed (1):
     */
    int      reconstructed ;

    /* Flag used when accessing the database - it returns whether or
     * not the requested time was covered by the database ("gap" == 0),
     * in which case the returned data is from the coverning record,
     * or was in a gap ("gap" == 1).  In the "gap" case, the nearest
     * record is returned.
     */
    int      gap ;

    /* These three timestamps are in JD2000 time (number of days
     * from 2002/1/1).  Note that 00:00 GMT on 1/1/2000 is 0.0,
     * 00:00 GMT on 1/2/2000 is 1.0, and so on.
     */
    double   gtime ;
    double   stime ;
    double   etime ;

    /* Reference time state (JD2000): */
    double   t1 ;

    /* SC state at "t1": */
    double   x[6] ;

    /* These are the coefficient values: */
    double   coeffs[6][6] ;
    } ;
typedef  struct  ClusterCovariantCoeffs_struct  ClusterCovariantCoeffs ;

/* --------------------------------------------------------------- */
/* For MagPosition: */
#ifdef AT_SSL

int number_mpos_directories = 1 ;

char *mposnames[1] = {
    "/disks/polar2/disk2/ftp/pub/cluster/sc_position"
    } ;

#endif  /* AT_SSL */

struct  BMagPosData_struct
    {
    int      jday ;   /* Julian Day of the data. */
    int      yr ;
    int      mn ;
    int      dy ;
    int      hr ;
    int      mi ;
    int      se ;
    int      msec ;
    double   ttag ;   /* Secs after midnight of "jday". */
    float    mlt ;
    float    ilat ;
    float    lshell ;
    float    pred_bmag ;
    } ;
typedef   struct  BMagPosData_struct   BMagPosData ;

/* --------------------------------------------------------------- */
/* Exported functions: */

extern int GetClusterOrbitData (char *fname, double StartTime,
    double IntervalSeconds, double DeltaSeconds,
    int *npts, ClusterOrbitData **odata) ;

extern int GetClusterAttitudeData (char *fname, double StartTime,
    double IntervalSeconds, double DeltaSeconds,
    int *npts, ClusterAttitudeData **adata) ;

extern int UnpackClusterOrbFile (char *fname,
    ClusterOrbBlock **oblocks, int *nblocks) ;

extern int UnpackClusterOrbFromFP (FILE *ifp, ClusterOrbBlock **oblocks,
    int *nblocks) ;

extern int ComputeClusterOrbDataFromBlock (double jdt,
    ClusterOrbBlock *blk, double *pos, double *vel, double *revnum) ;

extern int ConvertCCSDSAtoJD2000 (char *istr, double *otime) ;

extern long ClusterConvertCalendarToJulianDay (int year,
    int month, int day) ;

extern int ClusterConvertJulianDayToCalendar (long julian_day,
    int *year, int *month, int *day, int *day_of_year) ;

extern int ConvertJD2000ToDate (double tjd2000,
    int *year, int *month, int *mday, double *secs) ;

extern int ConvertJD2000ToJulianDay (double tjd2000,
    long *jday, double *secs) ;

extern int ConvertDateToJD2000 (int year, int month, int mday,
    double secs, double *tjd2000) ;

extern int ConvertJulianDayToJD2000 (long jday, double secs,
    double *tjd2000) ;

extern int UnpackClusterAttFile (char *fname, ClusterAttRec **orecs,
    int *nrecs) ;

extern int UnpackClusterAttFromFP (FILE *ifp, ClusterAttRec **orecs,
    int *nrecs) ;

extern int GetClusterSunPulseData (char *fname, double StartTime,
    double EndTime, int *npts, double **ottags, float **osprds) ;

extern int DecodeSCETHeader (unsigned char *bptr,
    ClusterSCETHdr *pscet) ;

extern int InitClusterOrbBlock (ClusterOrbBlock *orbb) ;

extern int CopyClusterOrbBlock (ClusterOrbBlock *src,
     ClusterOrbBlock *dst) ;

extern int ClearClusterOrbBlock (ClusterOrbBlock *orbb) ;

extern int PrintClusterOrbBlock (ClusterOrbBlock *orbb,
     FILE *ofp) ;

extern int InitClusterCovariantCoeffs (ClusterCovariantCoeffs *coeff) ;

extern int CopyClusterCovariantCoeffs (ClusterCovariantCoeffs *src,
     ClusterCovariantCoeffs *dst) ;

extern int ClearClusterCovariantCoeffs (ClusterCovariantCoeffs *coeff) ;

extern int PrintClusterCovariantCoeffs (ClusterCovariantCoeffs *coeff,
     FILE *ofp) ;

extern char *ClusterParseEnvironmentVariablesInString (char *istr,
    int *err_code) ;

extern int GetUTClusterDBCovarientCoeffs (double *itime, int ntimes,
    char *dbdir,
    ClusterCovariantCoeffs *coeff1, ClusterCovariantCoeffs *coeff2,
    ClusterCovariantCoeffs *coeff3, ClusterCovariantCoeffs *coeff4) ;

extern int GetClusterDBCovarientCoeffs (double *itime, int ntimes,
    char *dbdir,
    ClusterCovariantCoeffs *coeff1, ClusterCovariantCoeffs *coeff2,
    ClusterCovariantCoeffs *coeff3, ClusterCovariantCoeffs *coeff4) ;

extern int ParseCMatFileTime (char *itime, int *day, int *secs) ;

extern int GetUTClusterDBOrbitData (double StartTime,
    double IntervalSeconds, double DeltaSeconds, char *dbdir,
    int *npts, ClusterOrbitData **odata1, ClusterOrbitData **odata2,
    ClusterOrbitData **odata3, ClusterOrbitData **odata4) ;

extern int GetClusterDBOrbitData (double StartTime,
    double IntervalSeconds, double DeltaSeconds, char *dbdir,
    int *npts, ClusterOrbitData **odata1, ClusterOrbitData **odata2,
    ClusterOrbitData **odata3, ClusterOrbitData **odata4) ;

extern int UnpackClusterOrbFromDB (FILE *sfp, int sc, int srec,
    int lrec, ClusterOrbBlock **oblocks, int *nblocks) ;

extern int CL_FillMagPostionFiles (int syear, int smonth, int sday,
    int shour, int sminute, int ssecond, int npoints, int sintvls,
    int number_mpos_directories, char **mposnames, int *i_ndsets,
    char ***MagPositionName) ;

extern int CL_GetMagPositionData (int syear, int smonth, int sday,
    int shour, int sminute, int ssecond, int npoints, int sintvls,
    int number_mpos_directories, char **mposnames, int *sc_flags,
    int *out_cnts, BMagPosData **bp_data) ;

extern int CL_FindMagPositionNameFiles (int ndirs, char **idir,
    int ndays, long *jday, char ***ofiles) ;

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* CLUSTERORBATT_H */
