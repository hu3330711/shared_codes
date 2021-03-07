/* PolarToolsLib.h */

#ifndef POLARTOOLSLIB_H
#define POLARTOOLSLIB_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include <PolarDecomConfig.h>
#include <cdf.h>

/* For SCCS: */
#define SccsId_PolarToolsLib_h  "@(#)PolarToolsLib.h	1.3, 11/03/01"

/* --------------------------------------------------------------- */
/* Exported Structures: */

/* Fortran-like ephemeris data storage: */
struct PolarEphemerisData_struct
   {
   int       number_recs ;
   double    *ttag ;

   float     *gci_pos_x ;
   float     *gci_pos_y ;
   float     *gci_pos_z ;
   float     *gci_vel_x ;
   float     *gci_vel_y ;
   float     *gci_vel_z ;
   float     *gse_pos_x ;
   float     *gse_pos_y ;
   float     *gse_pos_z ;
   float     *gse_vel_x ;
   float     *gse_vel_y ;
   float     *gse_vel_z ;
   float     *gsm_pos_x ;
   float     *gsm_pos_y ;
   float     *gsm_pos_z ;
   float     *gsm_vel_x ;
   float     *gsm_vel_y ;
   float     *gsm_vel_z ;
   float     *sun_vec_x ;
   float     *sun_vec_y ;
   float     *sun_vec_z ;
   float     *distance_er ;
   float     *mlt ;
   float     *maglat ;
   float     *lshell ;

   /* This is the measurement of the angle
    * of Greenwich to the vernal equinox as measured westward
    * from Greenwich.
    */
   float           *gha ;
   } ;
typedef  struct PolarEphemerisData_struct PolarEphemerisData ;

/* --------------------------------------------------------------- */
/* Alternate C-like ephemeris data storage: */

struct PolarEphemerisDataVec_struct
   {
   double    ttag ;

   float     gci_pos_x ;
   float     gci_pos_y ;
   float     gci_pos_z ;
   float     gci_vel_x ;
   float     gci_vel_y ;
   float     gci_vel_z ;
   float     gse_pos_x ;
   float     gse_pos_y ;
   float     gse_pos_z ;
   float     gse_vel_x ;
   float     gse_vel_y ;
   float     gse_vel_z ;
   float     gsm_pos_x ;
   float     gsm_pos_y ;
   float     gsm_pos_z ;
   float     gsm_vel_x ;
   float     gsm_vel_y ;
   float     gsm_vel_z ;
   float     sun_vec_x ;
   float     sun_vec_y ;
   float     sun_vec_z ;
   float     distance_er ;
   float     mlt ;
   float     maglat ;
   float     lshell ;

   /* This is the measurement of the angle
    * of Greenwich to the vernal equinox as measured westward
    * from Greenwich.
    */
   float      gha ;
   } ;
typedef  struct PolarEphemerisDataVec_struct PolarEphemerisDataVec ;

struct PolarEphemerisDataVecArr_struct
   {
   int       number_recs ;
   PolarEphemerisDataVec  *vec ;
   } ;
typedef  struct PolarEphemerisDataVecArr_struct PolarEphemerisDataVecArr ;

/* --------------------------------------------------------------- */
/* Exported Functions: */

extern char *PolarGetOrbitFileNameForDate (int iyear, int imonth,
     int imday) ;

extern char *PolarGetOrbitFileNameForJulianDay (int jday) ;

extern char **SSLPolarGetOrbitFileNamesForDateRange (int syear,
    int smonth, int smday, int eyear, int emonth, int emday,
    int *nfiles) ;

extern long  PolarConvertCalendarToJulianDay (int year, int month,
    int day) ;

extern int PolarConvertJulianDayToCalendar (long julian_day,
    int *year, int *month, int *day, int *day_of_year) ;


extern PolarEphemerisData *ConstructPolarEphemerisData (int irecs) ;

extern int FillPolarEphemerisData (PolarEphemerisData *ephd,
    int irecs) ;

extern int DestructPolarEphemerisData (PolarEphemerisData *ephd) ;

extern int ClearPolarEphemerisData (PolarEphemerisData *ephd) ;

extern int PrintPolarEphemerisData (PolarEphemerisData *ephd,
    FILE *ofp) ;

extern PolarEphemerisDataVecArr *ConstructPolarEphemerisDataVecArr (
    int irecs) ;

extern int FillPolarEphemerisDataVecArr (PolarEphemerisDataVecArr *ephd,
    int irecs) ;

extern int DestructPolarEphemerisDataVecArr (
    PolarEphemerisDataVecArr *ephd) ;

extern int ClearPolarEphemerisDataVecArr (
    PolarEphemerisDataVecArr *ephd) ;

extern int PrintPolarEphemerisDataVecArr (PolarEphemerisDataVecArr *ephd,
    FILE *ofp) ;

extern PolarEphemerisData *SSLPolarGetOrbitDataForDateRange (
    int syear, int smonth, int smday, double ssec,
    int eyear, int emonth, int emday, double esec) ;

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* POLARTOOLSLIB_H */
