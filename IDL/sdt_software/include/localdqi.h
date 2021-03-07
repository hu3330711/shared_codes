/*  ------------------------------------------------------------ */
#ifndef localdqi_h
#define localdqi_h

/* SCCS ID string: */
#define SccsId_localdqi_h "@(#)localdqi.h	1.3, 05/30/04"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <memory.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/times.h>

#include <SDTDescription.h>
#include <SDTInstance.h>
#include <SDTDataUtilities.h>

/* -------------------------------------------------------------- */

/* -------------------------------------------------------------- */

DataQuantityInstance *CreateLocalDataQuantityInstance (
    DataQuantityInstance *in_dqi, int PopulateData) ;

int FillLocalDataQuantityInstance (DataQuantityInstance *in_dqi,
     DataQuantityInstance *new_dqi, int PopulateData) ;

int DestructLocalDataQuantityInstance (DataQuantityInstance *local_dqi) ;

int ClearLocalDataQuantityInstance (DataQuantityInstance *local_dqi) ;

#endif // localdqi_h
