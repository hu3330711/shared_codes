
/* idl_get_tsyg96.h */

/* Definitions required by the "idl_get_tsyg96" module - which is
 * used to compute Model Magnetic field data.
 */
#ifndef IDL_GET_TSYG96_H
#define IDL_GET_TSYG96_H

/* Here is the SCCS ID Keyword string 
 * (see page 99 of "Programming Utilities and Libraries").
 */
#define SccsId_idl_get_tsyg96_h "@(#)idl_get_tsyg96.h	1.2, 06/21/04"

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

#include <orbitprop.h>

/* --------------------------------------------------------------- */
/* Definitions: */

#define   GEI_COORDINATES   0
#define   GSE_COORDINATES   1
#define   GSM_COORDINATES   2
#define   GEO_COORDINATES   3

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
/* Exported constants: */

/* Constant for converting UCLA "Cline" time to Unix time.
 * Note that "Cline" time starts 1966/01/01 00:00 GMT and
 * Unix time starts 1970/01/01 00:00 GMT.
 */
extern const int UCLAClineToUnix ;

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

/* Common Block Declarations */

union geopack_data_union
    {
    struct {
	double st0, ct0, sl0, cl0, ctcl, stcl, ctsl, stsl, sfi, cfi,
	sps, cps, shi, chi, hi, psi, xmut, a11, a21, a31, a12, a22,
	a32, a13, a23, a33, ds3;
	int k, iy;
	double cgst, sgst, ba[6];
    } _1;
    struct {
	double st0, ct0, sl0, cl0, ctcl, stcl, ctsl, stsl, ab[19];
	int k, iy;
	double bb[8];
    } _2;
    struct {
	double a[27];
	int kkkk[2];
	double cgst, sgst, b[6];
    } _3;
    struct {
	double a[8], sfi, cfi, b[7], ab[10];
	int k, iy;
	double ba[8];
    } _4;
    struct {
	double a[12], shi, chi, ab[13];
	int k, iy;
	double ba[8];
    } _5;
    struct {
	double a[10], sps, cps, b[15];
	int k, iy;
	double ab[8];
    } _6;
    struct {
	double aa[17], a11, a21, a31, a12, a22, a32, a13, a23, a33, d;
	int k, iy;
	double b[8];
    } _7;
    struct {
	double a[15], psi, aa[10], ds3;
	int k, iy;
	double bb[8];
    } _8;
    struct {
	double a[26], ds3;
	int k, iy;
	double b[8];
    } _9;
    struct {
	double aa[26], dd;
	int k1, k2;
	double bb[8];
    } _10;
} ;

typedef union geopack_data_union geopack_data ;

typedef /* Subroutine */ int (*T_fp)(int nargs, ...);

/* ----------------------------------------------------------------- */
struct NasaGeoPack_struct
    {
    geopack_data   XfmState ;
    } ;

typedef  struct NasaGeoPack_struct  NasaGeoPack ;

/* ----------------------------------------------------------------- */
union warp_data_union
    {
    struct {
	double cpss, spss, dpsrr, rps, warp, d, xs, zs, dxsx,
	    dxsy, dxsz, dzsx, dzsy, dzsz, dzetas, ddzetadx,
	    ddzetady, ddzetadz, zsww;
    } _1;
    struct {
	double cpss, spss, dpsrr, xnext[3], xs, zswarped, dxsx,
	    dxsy, dxsz, dzsx, dzsywarped, dzsz, other[4], zs;
    } _2;
    struct {
	double cpss, spss, dpsrr, xnext[3], xs, zs, dxsx, dxsy,
	    dxsz, other[3], dzetas, ddzetadx, ddzetady,
	    ddzetadz, zsww;
    } _3;
    struct {
	double first[3], rps, warp, d, other[13];
    } _4;
}  ;

typedef union warp_data_union warp_data ;

struct T96Mdl_struct
    {
    warp_data     Wpd ;
    geopack_data  XfmState ;
    } ;
typedef  struct T96Mdl_struct  T96Mdl ;

/* --------------------------------------------------------------- */
/* Exported functions: */

int idl_get_tsyganenko_96_vectors (int argc, void *argv[]) ;

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
    int NumberHarmonics, double baseMjd, int NPts, orbVector *vecp) ;

int ComputeT96FromSCPositionAndUnixTime (double SolarWindPressure,
    double Dst, double ByImf, double BzImf, double GeoTilt,
    int NumberHarmonics, int InputCoordinates, int InputKm,
    int OutputCoordinates, int OutputKm, int NPts,
    double *Ttags, float *Xpos, float *Ypos, float *Zpos,
    float *Bxout, float *Byout, float *Bzout) ;

T96Mdl *ConstructT96Mdl (void) ;

int FillT96Mdl (T96Mdl *tmdl) ;

int DestructT96Mdl (T96Mdl *tmdl) ;

int ClearT96Mdl (T96Mdl *tmdl) ;

int T96Mdl_t96_01(T96Mdl *tmdl, int *iopt, double *parmod,
    double *ps, double *x, double *y, double *z,
    double *bx, double *by, double *bz) ;

int T96Mdl_dipshld(double *ps, double *x, double *y, 
    double *z, double *bx, double *by, double *bz) ;

int T96Mdl_cylharm(double *a, double *x, double *y, 
    double *z, double *bx, double *by, double *bz) ;

int T96Mdl_cylhar1(double *a, double *x, double *y, 
    double *z, double *bx, double *by, double *bz) ;

double T96Mdl_bes(double *x, int *k) ;

double T96Mdl_bes0(double *x) ;

double T96Mdl_bes1(double *x) ;

int T96Mdl_intercon(double *x, double *y, double *z, 
    double *bx, double *by, double *bz) ;

int T96Mdl_tailrc96(T96Mdl *tmdl, double *sps, double *x,
    double *y, double *z, double *bxrc, double *byrc,
    double *bzrc, double *bxt2, double *byt2, double *bzt2,
    double *bxt3, double *byt3, double *bzt3) ;

int T96Mdl_ringcurr96(T96Mdl *tmdl, double *x, double *y,
    double *z, double *bx, double *by, double *bz) ;

int T96Mdl_taildisk(T96Mdl *tmdl, double *x, double *y,
    double *z, double *bx, double *by, double *bz) ;

int T96Mdl_tail87(T96Mdl *tmdl, double *x, double *z,
    double *bx, double *bz) ;

int T96Mdl_shlcar3x3(double *a, double *x, double *y, 
    double *z, double *sps, double *hx, double *hy, 
    double *hz) ;

int T96Mdl_birk1tot_02(double *ps, double *x, double *y,
    double *z, double *bx, double *by, double *bz) ;

int T96Mdl_diploop1(double *xi, double *d) ;

int T96Mdl_circle(double *x, double *y, double *z, 
    double *rl, double *bx, double *by, double *bz) ;

int T96Mdl_crosslp(double *x, double *y, double *z, 
    double *bx, double *by, double *bz, double *xc, 
    double *rl, double *al) ;

int T96Mdl_dipxyz(double *x, double *y, double *z, 
    double *bxx, double *byx, double *bzx, double *bxy, 
    double *byy, double *bzy, double *bxz, double *byz, 
    double *bzz) ;

int T96Mdl_condip1(double *xi, double *d) ;

int T96Mdl_birk1shld(double *ps, double *x, double *y, 
    double *z, double *bx, double *by, double *bz) ;

int T96Mdl_birk2tot_02(double *ps, double *x, double *y,
    double *z, double *bx, double *by, double *bz) ;

int T96Mdl_birk2shl(double *x, double *y, double *z, 
    double *ps, double *hx, double *hy, double *hz) ;

int T96Mdl_r2_birk(double *x, double *y, double *z, 
    double *ps, double *bx, double *by, double *bz) ;

int T96Mdl_r2inner(double *x, double *y, double *z, 
    double *bx, double *by, double *bz) ;

int T96Mdl_bconic(double *x, double *y, double *z, 
    double *cbx, double *cby, double *cbz, int *nmax) ;

int T96Mdl_dipdistr(double *x, double *y, double *z, 
    double *bx, double *by, double *bz, int *mode) ;

int T96Mdl_r2outer(double *x, double *y, double *z, 
    double *bx, double *by, double *bz) ;

int T96Mdl_loops4(double *x, double *y, double *z, 
    double *bx, double *by, double *bz, double *xc, 
    double *yc, double *zc, double *r, double *theta, 
    double *phi) ;

int T96Mdl_r2sheet(double *x, double *y, double *z, 
    double *bx, double *by, double *bz) ;

double T96Mdl_xksi1(double *x, double *y, double *z) ;

double T96Mdl_fexp(double *s, double *a) ;

double T96Mdl_fexp1(double *s, double *a) ;

double T96Mdl_tksi(double *xksi, double *xks0, double *dxksi) ;

int T96Mdl_dipole(double *ps, double *x, double *y, double *z,
     double *bx, double *by, double *bz) ;

int T96Mdl_cttodate(double *ct, int *year, char *month,
     int *day, int *hour, int *min_, double *sec,
     int *itoggle) ;

int T96Mdl_dayofyr(int *dayoy, int *year, char *month,
     int *day, int *itoggle) ;

double T96Mdl_d_sign(double *a, double *b) ;

/* Based on NASA "geopack" routines: */

int NGP_igrf(int *iy, int *nm, double *r, double *t,
           double *f, double *br, double *bt, double *bf) ;

int NGP_dip(double *ps, double *x, double *y, double *z,
           double *bx, double *by, double *bz) ;

int NGP_sunl(int *iyr, int *iday, int *ihour, 
           int *min_, int *isec, double *gst,
	   double *slong, double *srasn, double *sdec) ;

int NGP_sphcar(double *r, double *teta, double *phi, double *x,
           double *y, double *z, int *j) ;

int NGP_bspcar(double *teta, double *phi, double *br, double *btet, 
           double *bphi, double *bx, double *by, double *bz) ;

int NGP_recalc(geopack_data *xfm, int *iyr, int *iday, int *ihour, 
           int *min_, int *isec) ;

int NGP_geomag(geopack_data *xfm, double *xgeo, double *ygeo,
	   double *zgeo, double *xmag, double *ymag, double *zmag,
	   int *j, int *iyr) ;

int NGP_geigeo(geopack_data *xfm, double *xgei, double *ygei,
	   double *zgei, double *xgeo, double *ygeo, double *zgeo,
	   int *j) ;

int NGP_magsm(geopack_data *xfm, double *xmag, double *ymag,
	   double *zmag, double *xsm, double *ysm, double *zsm,
	   int *j) ;

int NGP_gsmgse(geopack_data *xfm, double *xgsm, double *ygsm,
	   double *zgsm, double *xgse, double *ygse, double *zgse,
	   int *j) ;

int NGP_smgsm(geopack_data *xfm, double *xsm, double *ysm, double *zsm,
	   double *xgsm, double *ygsm, double *zgsm, int *j) ;

int NGP_geogsm(geopack_data *xfm, double *xgeo, double *ygeo,
	   double *zgeo, double *xgsm, double *ygsm, double *zgsm,
	   int *j) ;

int NGP_rhand(geopack_data *xfm, double *x, double *y, double *z,
	   double *r1, double *r2, double *r3, int *iopt, double *parmod,
	   T_fp exname) ;

int NGP_step(geopack_data *xfm, int *n, double *x, double *y, double *z,
	   double *ds, double *errin, int *iopt, double *parmod,
	   T_fp exname) ;

int NGP_trace(geopack_data *xfm, double *xi, double *yi, double *zi,
	   double *dir, double *rlim, double *r0, int *iharm, int *np,
	   int *iopt, double *parmod, T_fp exname, double *xf,
	   double *yf, double *zf, double *xx, double *yy,
	   double *zz, int *l) ;

double NGP_d_mod(double *x, double *y) ;

double NGP_r_sign(double *a, double *b) ;
/* -------------------------------------------------------------- */

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* IDL_GET_TSYG96_H */
