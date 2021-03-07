/* -------------------------------------------------------------------------*
 *									    *
 * attlib.h - Contains Attquery declarations.                               *
 *									    *
 * Copyright (c) 1993-1996 Regents of the University of California.         *
 * All Rights Reserved.                                                     *
 *                                                                          *
 * Redistribution and use in source and binary forms are permitted          *
 * provided that the above copyright notice and this paragraph are          *
 * duplicated in all such forms and that any documentation, advertising     *
 * materials, and other materials related to such distribution and use      *
 * acknowledge that the software was developed by the University of         *
 * California, Los Angeles.  The name of the University may not be used     *
 * to endorse or promote products derived from this software without        *
 * specific prior written permission.  THIS SOFTWARE IS PROVIDED "AS IS"    *
 * AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, WITHOUT        *
 * LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS        *
 * FOR A PARTICULAR PURPOSE.                                                *
 *                                                                          *
 * Written by Bryan Littlefield (bryan@igpp.ucla.edu)                       *
 *									    *
 * -------------------------------------------------------------------------*/

#ifndef LINT
	static char sccsid_attlib_h[] = "@(#)attlib.h	1.12 11/25/96 UCLA";
#endif

/*****************************************************************************/

#ifndef ATTLIB_H
#define ATTLIB_H
 
#ifdef  __cplusplus
extern "C" {
#endif

/*****************************************************************************/
/************************ ATTQUERY DEFINES ***********************************/
/*****************************************************************************/
 
#define ATTQ_VERSION    1.1
#define ATTQ_VERLEN     5

#define ATTQ_MAXNAME    40
#define ATTQ_MAXBUF     250
 
#define DIM		3
#define BAD_ATT         -1
#define BEST_ATT        32767
#define SSC_OFFSET      98.4
 
/*****************************************************************************/
/************************** FILE DEFINES *************************************/
/*****************************************************************************/
 
#define FLATFILE        1
#define ASCIIFILE       2
#define OTHERSRC        3
#define DIM		3

/*****************************************************************************/
/********************* DEFINES TRANSFORMATION TYPES **************************/
/*****************************************************************************/
 
#define FASTSPIN	0
#define DESPUN		1
#define FASTMAG		2
#define GEI		3
#define GEO		4
#define GSE		5
#define GSM		6
#define MAG		7
#define GECI		8
#define GSEQ		9
#define SM		10

#define MAXCOORD	10

/***************************************************************************/
/******************** DEFINE ATTQUERY DATA STRUCTURES **********************/
/***************************************************************************/

typedef struct {
        int     year;     /* [1950 - 2049] */
        int     doy;      /* [1 - 365] (Jan 1 == 1) */
        int     hour;     /* [0 - 23] */
        int     minute;   /* [0 - 59] */
        double  seconds;  /* [0.0 - 59.999999] micro-sec resolution */
}
attEpoch;

typedef struct {
        double  spin_ra;        /* Spin right ascension (deg) - GEI coords */
        double  spin_dec;       /* Spin declination (deg) - GEI coords */
        double  spin_phase;     /* Phase about spin axis(deg) - zero phase when
                                   spacecraft x-axis is in the sun */
        double  spin_freq;      /* Spin frequency (deg/sec) */
}
attAngles;

/***************************************************************************/
/********************* DEFINE FUNCTION PROTOTYPES **************************/
/***************************************************************************/
 
#if defined(__STDC__) || defined(__cplusplus)
#define P_(s) s
#else
#define P_(s) ()
#endif
 
extern int  AttQuery  P_((attEpoch *att_time, int tocoord, int *attlevel, double matrix[DIM][DIM], attAngles *angles));

extern void AttDebug  P_((int));
extern void AttInputSet  P_((int));

#undef P_

/**************************************************************************/

#ifdef  __cplusplus
}
#endif
 
#endif /* ATTLIB_H */
