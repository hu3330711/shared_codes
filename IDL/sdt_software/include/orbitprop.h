/* @(#)orbitprop.h	1.10 09/16/96	UCB SSL */

#ifndef ORBITPROP_H
#define ORBITPROP_H

#include <orbitlib.h>

#ifdef	__cplusplus
extern "C" {
#endif

/* OrbGetVectors returns an extended vector structure containing
 * an orbVector along with an orbit number and absolute time
 */
typedef struct {
        orbVector  vector;
        long       orbit;
        orbTime    absTime;
} orbExVector;
 
/* Other orbit quantities */

/* From orb_quant.c */
extern void geo_gei(double mjd, double geo[3], double gei[3]);
extern double gei_alt(double r[3], double mjd);
extern void gei_geo(double mjd, double gei[3], double geo[3]);
extern void geo_mag_rot(double geo[3], double mag[3], double year);
extern void geo_mag_trans(double geo[3], double mag[3], double year);
extern void geo_mag(double lng, double lat, double *mlng, double *mlat, double year);
extern double gmst(double mjd);
extern void sun_position(double mjd, double s[3]);

/* From elem_vec.c */
extern void elem_vec(orbHeader *elemp, double r[3], double v[3]);
extern void vec_elem(double r[3], double v[3], double dt, orbHeader *elemp);

/* From orb_vec.c */
extern void DragProp(orbVector *currentp, orbVector *nextp, double dt);
extern void QuickProp(orbVector *currentp, orbVector *nextp, double dt);
extern void NoDragProp(orbVector *currentp, orbVector *nextp, double dt);
extern void FillVec(double baseMjd, orbVector *vecp, orbLabel label);

/* From orbit_props.c */
extern void orbit_no_drag(double x0[], double v0[], double x1[], 
	double v1[], double dt);

/* from gravity.c */
extern void gravity(double x[], double a[]);

/* from setc.c */
extern void setc(double mjd, double exotemp, double geokp, 
	double mass, double area);

/* Vector tools from vectors.c */
extern void r3cross(double v1[3], double v2[3], double prod[3]);
extern double r3mag(double v[3]);
extern void r3sxv(double s, double v[3]);
extern double r3dot(double v1[3], double v2[3]);
extern double gcang(double v1[3], double v2[3]);
extern double cart_lng(double r[3]);
extern double cart_lat(double r[3]);
extern void cart_sph(double r[3], double *lng, double *lat);
extern void sph_cart(double lng, double lat, double r[3]);
#define day_hr(dy)	((dy) * 24)
#define hr_day(hr)	((hr) / 24.0)
#define rad_deg(rad)	((rad) * 180 / M_PI)
#define deg_rad(deg)	((deg) * M_PI / 180)
#define rad_hr(rad)	(rad_deg(rad) / 15.0)
#define hr_rad(hr)	(deg_rad((hr * 15.0)))

/* from OrbPropTools.c */
extern void vec_gei_cpy(orbVector *dst, orbVector *src);
extern double OrbTimeDiff(orbTime *t1, orbTime *t2);

/* from OrbPropVector.c */
extern int OrbPropVector(orbExVector *previous, orbExVector *target, 
	orbLabel label, double delta, double intvl, 
	void (*propagator)(orbVector *currentp, orbVector *nextp,double dt));

/* from OrbGetVectors.c */
extern int OrbGetVectors(orbHandle *orp, orbTime *times, orbHeader
	*elem, orbExVector *vecs, orbLabel label, int n);
extern int OrbQuickVectors(orbHandle *orp, orbTime *times, orbHeader
	*elem, orbExVector *vecs, orbLabel label, int n);
extern int OrbComputeVectors(orbHandle *orp, orbTime *times, orbHeader
	*elem, orbExVector *vecs, orbLabel label, int n,
	double comptintvl, void (*propagator)(orbVector *currentp,
	orbVector *nextp,double dt));


#ifdef	__cplusplus
}
#endif

#endif

