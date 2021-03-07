/* @(#)orbconsts.h	1.7	08/27/96 */

#ifndef ORBCONSTS_H
#define ORBCONSTS_H

#define MU 398600.64		/* Earth's grav. const (km^3/sec^2) */
#define EQ_RAD 6378.14		/* Earth's equatorial radius (km) */
#define MEAN_RAD 6371.2		/* Earth's mean radius (km) */
                		/* Change by UCLA 9/01/94 */

#define GRAV_CONST -1.5362280e-6	/* Earth's grav. constant, Re/s^2 */

/*
 * WARNING !!!
 *
 * EPOCH and EPOCH_JAN_1_JULIAN must be changed together so that they
 * are consistent.
 */

#define EPOCH  1995		/* Epoch for celestial coords */
                		/* Change by UCLA 9/01/94 */
                		/* Change by M. Temerin 8/23/96 */
                		/* Change by M. Temerin 8/27/96 */

#define EPOCH_JAN_1_JULIAN  49718	/* Julian for EPOCH Jan 1 */

#define RAD2DEG 57.29578	/* Radian to degree conversion */
#define DEG2RAD 0.0174533	/* Degree to radian conversion */

#define ONE_MINUTE 60
#define ONE_HOUR 3600		/* Times in seconds */
#define ONE_DAY 86400

/* ---------------------------------------------- */
/*
 * Offset dipole coordinates are based on calculations using equations
 * from
 * "Coordinate systems for space and geophysical appications"
 * by K. H. Bhavnani and R. P. Vanccour, Philips Laboratory publication
 * PL_TR-91-2296, Philips Laboratory, Air force systems command,
 * Hanscom Air Force Base, Masschusetts 01731-5000
 * with typoes corrected by M. Temerin, 8/21/96
 * using IGRF 1995 with secular variation to 1/1/97.
 * 1995 IGRF coef  are from EOS, page 153, April 16, 1996 )
 */

/* Dipole position, Km, GEO coordinates */
#define DIPOLE_X	-402.199
#define DIPOLE_Y	287.504
#define DIPOLE_Z	195.908

/* Dipole Z-axis; GEO Latitude, longitude; degrees */
#define DIPOLE_LAT	79.3637
#define DIPOLE_LNG	288.454

#endif  /* ORBCONSTS_H */

