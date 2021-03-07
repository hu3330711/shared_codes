#ifndef FASTTIMEUTILITIES_H
#define FASTTIMEUTILITIES_H

#define SccsId_DecomUtilities_h "@(#)FastTimeUtilities.h	1.6, 12/03/06"

#include <FastTelemDefs.h>

/* --------------------------------------------------------------------- */
/* Exports: */

extern const long MissionEpochStartJulianDay ;
extern const long UnixStartJulianDay ;
extern const int SecondsOffsetFromMissionEpochToUnix ;

/* ------------------------------------------------------------------ */
/* Function declarations: */

double decom_TIME42(unsigned char *tmptr) ;
double decom_TIME12(unsigned char *tmptr) ;
double decom_TIME22L(unsigned char *tmptr) ;
unsigned short decom_TIME20L(unsigned char *tmptr) ;
uint32 decom_TIME40L(unsigned char *tmptr) ;
float decom_TIME02L(unsigned char *tmptr) ;
float decom_TIMEweirdL(unsigned char *tmptr) ;
double decom_TIME12_rel_to_TIME42(unsigned char *t12, unsigned char *t42) ;
short decom_SI085(unsigned char *tmptr) ;

int  ConvertSecsToTimeExtended (double insec, int *shour, int *sminute,
    int *ssecond, int *msecs) ;

#endif /* FASTTIMEUTILITIES_H */
