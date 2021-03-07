
/* ComputeOrbLib.h */

/* Definitions required by the "ComputeOrbLib" module - which is
 * used to compute Model Magnetic field data.
 */
#ifndef COMPUTEORBLIB_H
#define COMPUTEORBLIB_H

/* Here is the SCCS ID Keyword string 
 * (see page 99 of "Programming Utilities and Libraries").
 */
#define SccsId_ComputeOrbLib_h "@(#)ComputeOrbLib.h	1.4, 04/06/03"

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
#include <sys/times.h>

#include <orbitprop.h>

/* --------------------------------------------------------------- */
/* Definitions: */

#define   FAO_MU                398600.64
#define   FAO_EQ_RAD            6378.14
#define   FAO_DY_OFS1           47892
#define   FAO_JDAY_1990         2447893
#define   FAO_DEF_EXOTEMP       1000.0
#define   FAO_DEF_GEOKP         1.0
#define   FAO_DEF_SC_MASS       168.0
#define   FAO_DEF_SC_AREA       1.67e-06

/* step parameter for mag field tracing */
#define    FAO_DEL_1            0.1

#define   FAO_EPOCH               1995   /* Epoch for celestial coords */
#define   FAO_EPOCH_JAN_1_JULIAN  49718  /* Julian for EPOCH Jan 1 */

#define   FAO_MEAN_RAD            6371.2 /* Earth's mean radius (km) */

/* --------------------------------------------------------------- */
/* Macros: */

#define deg_rad(deg)        ((deg) * M_PI / 180)
#define hr_rad(hr)  (deg_rad((hr * 15.0)))

/* --------------------------------------------------------------- */
/* Typedefs: */

struct magsat_data_struct
    {
    double   epmodl;
    double   epocsv;
    double   g[784]  /* was [14][14][4] */  ;
    double   sch[196]  /* was [14][14] */  ;
    double   lgn_cons[196] ; /* for legendre computation: */
    double   ga[196] ; /* was [14][14] */
    double   p[196] ;
    double   dp[196] ;
    int      nt[4] ;
    int      nmax ;
    int      lgn_init ;   /* 0 -> Legendre constants need initing */
    } ;
typedef   struct magsat_data_struct   magsat_data ;

struct magft_data_struct
    {
    int    n ;
    double bvec[1350]    /* was [450][3] */ ;
    double rvec[3] ;
    double ps[1350]      /* was [450][3] */ ;
    double xx[3] ;
    } ;
typedef   struct magft_data_struct  magft_data ;

struct const_1_struct
    {
    double day90, mu, req, exotemp, geokp, mass, area ;
    } ;
typedef  struct const_1_struct  const_1 ;

typedef void (*S_fp) (double *, double *, int *, double *, const_1 *) ;

/* --------------------------------------------------------------- */
/* Exported functions: */

int fao_integrate (int *ndim, double *t, double *dt, double *y,
     const_1 *cns, S_fp fct) ;

void fao_fct (double *t, double *y, int *ndim, double *dery,
    const_1 *cns) ;

void fao_fctquick (double *t, double *y, int *ndim, double *dery,
    const_1 *cns) ;

int fao_jrdens(double *time, double *usun, double *r,
	double *temp, double *geokp, double *rho, int *ierr) ;

int fao_solar(double *day90, double *s) ;

int fao_jrcomp(double *del0, double *height, double *phj,
    double *tinf, double *tx, double *zj0, double *rho) ;

void fao_setc (const_1 *cns, double mjd, double exotemp, double geokp,
    double mass, double area) ;

void FaoDragProp (orbVector *currentp, orbVector *nextp, double dt,
    const_1 *cns) ;

void FaoNoDragProp(orbVector *currentp, orbVector *nextp, double dt,
    const_1 *cns) ;

#ifdef MAYBE_LATER
void FaoQuickProp(orbVector *currentp, orbVector *nextp, double dt,
    const_1 *cns) ;
#endif

void FaoFillVec (double baseMjd, orbVector *vecp, orbLabel label,
    int *mst_flag, magsat_data *mst_data, magft_data *mft_data) ;

int fao_magsat  (double *rvec, double *epoch, int *ntop, double *bvec,
    int *linit, magsat_data *mdata) ;

int fao_legndr (double *sth, double *cth, int *ntop, 
    double *p, double *dp, double *cons, int *linit) ;

double fao_rtrace (double *x) ;

int fao_lines (double *xx, double *del, double *stpmx,
	double *bvec, double *pos, int *n, double *rtop,
	double *epoch, double *rvec, int *mlinit, magsat_data *mdata) ;

int fao_magft(double *pos, double *allat1, double *allong1, 
	double *del, double *rvecr, double *epoch,
	int *mdata_init, magsat_data *mdata, magft_data *mfptr) ;

int fao_geo2 (double *x, double *latitude, double *longitude) ;

double d_sign (double a, double b) ;

double d_lg10 (double x) ;

double fao_gei_alt(double r[3], double mjd) ;

void fao_gei_geo(double mjd, double gei[3], double geo[3]) ;

void fao_geo_gei(double mjd, double geo[3], double gei[3]) ;

void fao_geo_mag_rot(double geo[3], double mag[3], double year) ;

void fao_geo_mag_trans(double geo[3], double mag[3], double year) ;

void fao_geo_mag(double lng, double lat, double *dlng,
    double *dlat, double year) ;

double fao_gmst(double mjd) ;

void fao_sun_position(double mjd, double sun_pos[3]) ;

double fao_r3mag (double v[3]) ;

double fao_cart_lng(double r[3]) ;

double fao_cart_lat(double r[3]) ;

void fao_cart_sph(double r[3], double *lng, double *lat) ;

void fao_sph_cart(double lng, double lat, double r[3]) ;

int ComputeT96FromOrbVectors (double SolarWindPressure,
    double Dst, double ByImf, double BzImf, double GeoTilt,
    int NumberHarmonics, double baseMjd, int ComputeMagFootprint,
    int NPts, orbVector *vecp) ;

int ReadClusterSCPositionFileList (int nfiles, char **fnames,
    int sc, int StartJDay, int EndJDay, orbVector **vecp,
    int *num_vecs, int *arr_size) ;

int ComputeUTClusterDBT96 (double SolarWindPressure, double Dst,
    double ByImf, double BzImf, double GeoTilt, int NumberHarmonics,
    int ComputeMagFootprint, double StartTime,
    double IntervalSeconds, double DeltaSeconds,
    int sc, char *dbdir, int *NPts, orbVector **vecp) ;

int ComputeClusterDBT96 (double SolarWindPressure, double Dst,
    double ByImf, double BzImf, double GeoTilt, int NumberHarmonics,
    int ComputeMagFootprint, double StartTime,
    double IntervalSeconds, double DeltaSeconds,
    int sc, char *dbdir, int *NPts, orbVector **vecp) ;

/* -------------------------------------------------------------- */

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* COMPUTEORBLIB_H */

