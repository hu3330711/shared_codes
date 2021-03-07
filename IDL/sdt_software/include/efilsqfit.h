// -------------------------------------------------------------------
#ifndef efilsqfit_h
#define efilsqfit_h

// SCCS ID string:
#define SccsId_efilsqfit_h "@(#)efilsqfit.h	1.7, 06/12/07"

#include <stdio.h>
#include <stdlib.h>
#include <SMISL.Depth.h>
#include <math.h>

// *********************************************************************

struct LSQFIT_struct {
   double Time ;
   float ParamA ;
   float ParamB ;
   float ParamC ;
   float ParamSigma ;
   int ParamN ;
   } ;
typedef struct LSQFIT_struct LSQFIT ;

// *********************************************************************

const int XDIM = 3 ;
const int YDIM = 4 ;
const int MAX_POINTS_PER_SPIN = 200000 ;

// *********************************************************************

void GetMatrixBasic( double Matrix[ XDIM][ YDIM], int *Indicies,
    int *orig_pts, double spin_period, double spulse_time,
    double *tarr, double *varr, int StartIdx, int NDataPts,
    int DiagnosticsOn) ;

LSQFIT *LstSquaresFitBasic( double Matrix[ XDIM][ YDIM], int *Indicies,
    int orig_pts, double spin_period, double spulse_time,
    double *tarr, double *varr, int DiagnosticsOn) ;

int EjectPointsBasic (double Matrix[ XDIM][ YDIM], int *Indicies,
    int orig_pts, LSQFIT *lsqf, double Alpha, double Beta,
    double spin_period, double spulse_time,
    double *tarr, double *varr, int DiagnosticsOn) ;

void GetMatrix (double Matrix[ XDIM][ YDIM], int *Indecies,
    int *orig_pts, DepthStruct *Data, int DiagnosticsOn) ;

double EfiDeterminant (double matrix[ XDIM][ XDIM], int size) ;

LSQFIT *LstSquaresFit (double Matrix[ XDIM][ YDIM], int *Indecies,
    int orig_pts, DepthStruct *Data, int DiagnosticsOn) ;

BOOLEAN EjectPoints( double Matrix[ XDIM][ YDIM], int *Indecies,
                         int orig_pts, DepthStruct *Data,
                         LSQFIT *lsqf, double Alpha, double Beta,
                         int DiagnosticsOn) ;

// *********************************************************************

#endif // efilsqfit_h
