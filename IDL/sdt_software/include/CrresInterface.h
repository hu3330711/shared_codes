/* ------------------------------------------------------------- */
/*
 * CrresInterface.h
 *
 * Declarations for the Crres User's Library:
 *
 */
#define SccsId_CrresInterface_h "@(#)CrresInterface.h	1.7, 11/27/98" ;

#include <SDTDescription.h>
#include <SDTInstance.h>
#include <SDTSpacecraft.h>
#include <DQHInterface.h>

/* ------------------------------------------------------------- */
/* Constants. */

/* These are the Crres Spacecraft instrument ID's known to the
 * "agmod" software.  These need to be fed into the "CrresGetAttitude"
 * routine in the "instrument" argument.
 */
#ifdef ALREADY_IN_SDTSpacecraft_h
const int CRR_SPHERE_ID_1 =        1 ;
const int CRR_SPHERE_ID_2 =        2 ;
const int CRR_CYLINDER_ID_3 =      3 ;
const int CRR_CYLINDER_ID_4 =      4 ;
const int CRR_POSITIVE_SPIN_AXIS = 5 ;
const int CRR_SPACECRAFT_X =       10 ;
const int CRR_SPACECRAFT_Y =       11 ;
const int CRR_SPACECRAFT_Z =       12 ;
#endif

/* ------------------------------------------------------------- */
/* Typedefs. */

/* The structure for an Attitude sub-record (one per orbit) in
 * shared-memory.
 *
 *  The sub-fields for the SubRecords are defined as follows:
 *
 *    start:     The starting time (in telemetry time) of the timespan
 *               covered by the corresponding Attitude file for this
 *               orbit.
 *    end:       The ending time (in telemetry time) of the timespan
 *               covered by the corresponding Attitude file for this
 *               orbit.
 *    t_offset:  The time offset, in seconds, to subtract from telemetry
 *               time, to get to a time system that the corresponding
 *               Attitude file can understand.
 *    b_offset:  The number of bytes, from the start of the DQI for
 *               Attitude (the data section, not including the DQI
 *               header) to the start of the corresponding Attitude file.
 *    orbit:     The CRRES orbit which is covered by the corresponding
 *               Attitude file.
 */
struct  CrresAttitudeSubRecord_struct
    {
    double    start ;
    double    end ;
    double    t_offset ;
    int       b_offset ;
    int       orbit ;
    } ;
typedef   struct  CrresAttitudeSubRecord_struct  CrresAttitudeSubRecord ;

/* The structure for the header section of the Attitude information
 * in the Crres Attitude DQI (DQD name is:  "CRRES_attitude_data").
 * This header exists to allow for handling multiple-orbit time
 * spans.  For each Crres orbit in the timespan, there is one copy
 * of its corresponding Attitude file in the DQI.  Access into the
 * correct file is handled by the Crres Interface library using the
 * header information.  Note that the CRRES Attitude DQI is organized
 * as follows:
 *
 *  int:         Number of Attitude files in the DQI (there should be
 *               one per orbit.  Referred to as "NumFiles".
 *  int:         Byte offset from the start of the data section of the
 *               DQI to the byte following the end of the last Attitude
 *               file currently in the DQI.  Referred to as "NextByte".
 *  int[6]:      Unused filler.
 *  SubRec[5]:   Five SubRecord structures (32 bytes each).  See
 *               the definition of "CrresAttitudeSubRecord" above.
 *
 *  AttFile[0]:  The first orbit Attitude file.
 *     .
 *     .
 *     .
 *  AttFile[NumFiles - 1]:   The last orbit Attitude file (max is 5).
 *  
 */
struct  CrresAttitudeHeader_struct
    {
    int       NumFiles ;
    int       NextByte ;
    int       Filler1[6] ;
    CrresAttitudeSubRecord  SubRec[5] ;
    } ;
typedef   struct  CrresAttitudeHeader_struct  CrresAttitudeHeader ;

/* The structure of an ephemeris record. */
struct  EphemerisDataRecord_struct
    {
    double    ut ;                 /* Seconds (UT) */
    float     posx ;               /* Spacecraft pos. (km) ECI-x coord. */
    float     posy ;               /* Spacecraft pos. (km) ECI-y coord. */
    float     posz ;               /* Spacecraft pos. (km) ECI-z coord. */
    float     vx ;                 /* Satellite velocity (x) */
    float     vy ;                 /* Satellite velocity (y) */
    float     vz ;                 /* Satellite velocity (z) */
    float     radius ;             /* Radius from earth center (km) */
    float     Altitude ;           /* km */
    float     Latitude ;           /* deg */
    float     Longitude ;          /* deg */
    float     Velocity ;           /* km/sec */
    float     LocalTime ;          /* HR */
    float     mradius ;            /* Mag. radius (emr) */
    float     mlat ;               /* Mag. latitude (deg) */
    float     mlong ;              /* Mag. longitude (deg) */
    float     Radius_SM ;          /* EMR */
    float     Latitude_SM ;        /* Deg */
    float     LocalTime_SM ;       /* HR */
    float     Radius_GSM ;         /* EMR */
    float     Latitude_GSM ;       /* Deg */
    float     LocalTime_GSM ;      /* HR */
    float     ModelB;              /* Model B field, magnitude */
    float     ModelBx;             /* Model B field, x-component */
    float     ModelBy;             /* Model B field, y-component */
    float     ModelBz;             /* Model B field, z-component */
    float     mlt ;                /* Mag. local time (hr) */
    float     SolarZenithAngle ;   /* Deg */
    float     InvariantLatitude ;  /* Deg */
    float     B100NLatitude ;      /* Deg */
    float     B100NLongitude ;     /* Deg */
    float     B100SLatitude ;      /* Deg */
    float     B100SLongitude ;     /* Deg */
    float     lshell;              /* L-shell (EMR) */
    float     BMin ;               /* nT */
    float     BMinLatitude ;       /* deg */
    float     BMinLongitude ;      /* deg */
    float     BMinAltitude ;       /* km */
    float     BConjLatitude ;      /* deg */
    float     BConjLongitude ;     /* deg */
    float     BConjAltitude ;      /* km */
    float     xsun ;               /* Sun pos. (km) ECI-x coord. */
    float     ysun ;               /* Sun pos. (km) ECI-y coord. */
    float     zsun ;               /* Sun pos. (km) ECI-z coord. */
    float     xmoon ;              /* km */
    float     ymoon ;              /* km */
    float     zmoon ;              /* km */
    float     RightAscension ;     /* hr */
    float     B100NMagField ;      /* nT */
    float     B100SMagField ;      /* nT */
    float     MxDMoment_ECI ;      /* nT */
    float     MyDMoment_ECI ;      /* nT */
    float     MzDMoment_ECI ;      /* nT */
    float     DxDOffset_ECI ;      /* km */
    float     DyDOffset_ECI ;      /* km */
    float     DzDOffset_ECI ;      /* km */
    } ;

typedef struct  EphemerisDataRecord_struct  EphemerisDataRecord ;

/* -------------------------------------------------------------------- */
/* Variables and arrays: */

/* -------------------------------------------------------------------- */
/* Function Declarations: */

int CrresOpenAttitudeEphemeris (TimeSpanDescription *tsp) ;

int CrresCloseAttitudeEphemeris (int TSpanIdx) ;

int CrresGetAttitude (int TSpanIdx, int instrument, double itime,
    float *ra_ds, float *los) ;

int CrresGetEphemeris (int TSpanIdx, double itime,
    EphemerisDataRecord *eptr) ;

int ECItoSCxfm (int TSpanIdx, double timein, double eciTsc[3][3]) ;

int ECItoSC_Double (int TSpanIdx, double timein,
    double *eci, double *sc) ;

int ECItoSC_Float (int TSpanIdx, double timein,
    float *eci, float *sc) ;

int SCtoECIxfm (int TSpanIdx, double timein, double scTei[3][3]) ;

int SCtoECI_Double (int TSpanIdx, double timein,
    double *sc, double *eci) ;

int SCtoECI_Float (int TSpanIdx, double timein,
    float *sc, float *eci) ;

int ECItoGSExfm (int TSpanIdx, double timein, double eciTgse[3][3]) ;

int ECItoGSE_Double (int TSpanIdx, double timein, double *eci,
    double *gse) ;

int ECItoGSE_Float (int TSpanIdx, double timein, float *eci,
    float *gse) ;

int GSEtoECIxfm (int TSpanIdx, double timein, double gseTeci[3][3]) ;

int ECItoMGSExfm (int TSpanIdx, double timein, double eciTmgse[3][3]) ;

int ECItoMGSE_Double (int TSpanIdx, double timein, double *eci,
    double *mgse) ;

int ECItoMGSE_Float (int TSpanIdx, double timein, float *eci,
    float *mgse) ;

void   CrresCrossProductDouble (double *v1, double *v2, double *v3) ;

double CrresMagnitudeDouble (double *v) ;

void   CrresScaleVectorDouble (double *v, double scale) ;

/* -------------------------------------------------------------------- */
/* End:  CrresInterface.h */
