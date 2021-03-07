/* ************************************************************ */
/*	orbitlib.h
 *
 *	Author: George Kaplan
 *	@(#)orbitlib.h	1.14 02/27/97	UCB SSL
 */

/* ************************************************************ */
#ifndef ORBITLIB_H
#define ORBITLIB_H

#include <stdio.h>
#include <time.h>

#ifdef	__cplusplus
extern "C" {
#endif

/* ============================================================ */
/*	Identifiers and constants				 */

/* Header info */
#define  ORB_VERSION	"1.2"	/* Orbit file format version */
#define  ORB_VERLEN	5	/* Max length of version string */
#define  ORB_MAXNAME	40	/* Max satellite name length */
#define  ORB_MAXVEC	18	/* Max fields per orbit vector */

/* File I/O modes  */
#define  ORB_FILE	0	/* File type:  orbit file */
#define  ORB_RDONLY	1	/* Open for reading */
#define  ORB_APPEND	2	/* Create or open for append */
#define  ORB_WRITE	3	/* Create or open for write at beginning */

/* Return codes */
#define  ORB_OK		0	/* No errors */
#define  ORB_EOF	1	/* End of file */
#define  ORB_ERROR	2	/* I/O or other error */
#define  ORB_HEADER	3	/* Found an orbit header */
#define  ORB_XHEADER	4	/* Found an extended header (with optional */
				/* fields */
#define  ORB_LABEL	5	/* Found a vector set label */
#define  ORB_VECTOR	6	/* Found a vector record */
#define  ORB_TIME_ERR	7	/* err return from OrbInterpTime () value*/
#define  ORB_CRD_ORB_ERR 8	/* bad orbit spec in call OrbInterpCoords () */
#define  ORB_CRD_TIME_ERR 9	/* time not found in call OrbInterpCoords () */
#define  ORB_TM_NO_CRD_ERR 10   /* coord not in file in call OrbInterpTime ()*/
#define  ORB_NO_ORB_ERR 11      /* bad orb in OrbInterpTime() & OrbSeek() */
#define  ORB_TM_LAB_ERR 12      /* bad label spec in call OrbInterpTime () */
#define  ORB_TM_NFND_ERR 13     /* time not found in call OrbInterpTime () */
#define  ORB_HNDL_FREE_ERR 14   /* bad orbHandle in call OrbFreeHandle () */
#define  ORB_SEEK_ERR   15      /* no indexing on orbHandle in call OrbSeek()*/
    
/* Orbit vector field selection flags */
#define  ORBL_TIME	0x00000001
#define  ORBL_X		0x00000002
#define  ORBL_Y		0x00000004
#define  ORBL_Z		0x00000008
#define  ORBL_VX	0x00000010
#define  ORBL_VY	0x00000020
#define  ORBL_VZ	0x00000040
#define  ORBL_LAT	0x00000080
#define  ORBL_LNG	0x00000100
#define  ORBL_ALT	0x00000200
#define  ORBL_FLAT	0x00000400
#define  ORBL_FLNG	0x00000800
#define  ORBL_MLT	0x00001000
#define  ORBL_ILAT	0x00002000
#define  ORBL_ILNG	0x00004000
#define  ORBL_BX	0x00008000
#define  ORBL_BY	0x00010000
#define  ORBL_BZ	0x00020000

#define  ORBL_POS	(ORBL_X | ORBL_Y | ORBL_Z)
#define  ORBL_VEL	(ORBL_VX | ORBL_VY | ORBL_VZ)
#define  ORBL_GEI_STATE	(ORBL_TIME | ORBL_POS | ORBL_VEL)
#define  ORBL_ALL	0x0003ffff

/* Other limits */
#define  ORB_MAXLINE	1024	/* Maximum file line length */
#define  ORB_MAX_INTRP_SLN 4    /* Maximum number of coordinate crossings
                                 * solutions for a given time */

/* Other Constants */
#define  ORB_IGNORE     -1      /* ignore orbit number */

/* ============================================================ */
/*	Data types						*/

/*  Orbit time and date information */
/*    Ranges correspond to those in 'struct tm' except for year */
typedef struct {
	int  year;		/* 1600 or later */
	int  DOY;		/* Day of year, [0, 365] */
	int  month;		/* months since Jan - [0, 11] */
	int  mday;		/* day of the month  [1, 31] */
	int  wday;		/* day of the week  [0, 6] */
	int  hour;		/* hours UTC [0, 23] */
	int  minute;		/* minutes after hour [0, 59] */
	double second;		/* seconds after minute [0, 61] */
	double MJD;		/* Modified Julian Date */
} orbTime;
	

/*  Orbit header data */
typedef struct {
	char	version[ORB_VERLEN];	/* orbitlib version #, e.g. "1.00" */
	char	satellite[ORB_MAXNAME];	/* Satellite identifier */
	long	orbit;		/* orbit number */

	/* Keplerian elements */
	
	orbTime  epoch;		/* Epoch date & time */
	double	axis;		/* Semimajor axis, Km */
	double	ecc;		/* Eccentricity */
	double	inc;		/* Inclination to equator, deg. */
	double	node;		/* RA of ascending node, deg. */
	double	aperigee;	/* Argument of perigee, deg. */
	double	manomaly;	/* Mean anomaly, deg */
} orbHeader;

/* Optional header fields in extended header */
typedef struct {
	double	apogee;		/* Apogee altitude */
	double	apmlt;		/* Apogee Mean Local Time, hours */
	double	apilat;		/* Apogee invariant altitude */
} orbExHeader;

/*  Orbit Vectors */
typedef unsigned long orbLabel;	/* Vector label bitmask */

typedef struct {
	double	time;		/* Time since epoch in header, sec */
	double	r[3];		/* Position, GEI coords, Km */
	double	v[3];		/* Velocity, Km/sec */
	double	lat, lng;	/* Geographic latitud, longitude, deg */
	double	alt;		/* Altitude, Km */
	double	flat, flng;	/* Geographic/geodetic latitude, longitude of magnetic */
				/* footprint, deg */
	double	mlt;		/* Magnetic local time, hours */
	double	ilat, ilng;	/* Invariant latitude, longitude, deg */
	double	b[3];		/* Magnetic field, nTesla */
} orbVector;

typedef union {
	orbHeader  header;
	orbExHeader  xheader;
	orbLabel   label;
	double     rawVector[ORB_MAXVEC];
} orbRecord;

/* An orbIndex is one file location that points to the start of an orbit in the
 * orbit file.  It contains the an orbHeader, and the offset from the
 * begining of the file.  It is possible to write a function to match any
 * quantity in this header for seeking to a given orbit */
typedef struct {
	orbHeader	header ;
	long		offset ;
} orbIndex ;

/*  orbHandle currently holds a FILE * plus a pointer to an orbIndex struct. */
typedef struct {
	FILE * 		orbFile ;
	orbIndex *	index ;
	int		nIndices ;
} orbHandle ;

/* ============================================================ */
/*	Function prototypes					*/

extern orbHandle *OrbOpen(char *name, int ftype, int mode);

extern orbHandle *OrbOpenStream(FILE * stream);

extern int OrbFreeHandle(orbHandle * orp);

extern int OrbClose(orbHandle *orp);

extern int OrbRewind(orbHandle *orp);

extern int OrbRead(orbHandle *orp, orbRecord *rec);

extern int OrbExpandVector(double raw[], orbVector *vec, 
		orbLabel flabel, orbLabel dlabel);

extern int OrbWriteHeader(orbHandle *orp, orbHeader *headerp);

extern int OrbWriteExHeader(orbHandle *orp, orbExHeader *headerp);

extern int OrbWriteLabel(orbHandle *orp, orbLabel label);

extern int OrbWriteVector(orbHandle *orp, orbVector *vector, orbLabel label);

extern void OrbNormalizeMJD(orbTime *oTime);

extern void OrbNormalizeDOY(orbTime *oTime);

extern void OrbNormalizeDate(orbTime *oTime);

extern int OrbInterpTime (orbHandle *orp, orbTime * times, int * nFound,
			  long orbit, double coordinate, orbLabel lab ) ;

extern int OrbInterpCoords (orbHandle *orp, orbTime time, orbVector *v,
			    long *orbit, orbLabel *lab) ;

extern int OrbSeek (orbHandle *orp, long orbit) ;

extern int OrbIndexCount(orbHandle *orp);

extern orbHeader *OrbIndexHeader(orbHandle *orp, int n);

#ifdef	__cplusplus
}
#endif

#endif /* ORBITLIB_H */
