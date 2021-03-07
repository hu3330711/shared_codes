
/* ------------------------------------------------------------- */
/*
 * efw_interface.h
 *
 * These are the declarations required for decommutating the EFW
 * instrument data from the Cluster spacecraft.
 *
 */

#ifndef EFW_INTERFACE_H
#define EFW_INTERFACE_H

/* Here is the SCCS ID Keyword string 
 * (see page 99 of "Programming Utilities and Libraries").
 */
#define SccsId_efw_interface_h "@(#)efw_interface.h	1.3, 06/07/04"

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <errno.h>
#include <dirent.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/times.h>

/* "ted" includes */
#include <ted/ted.h>
#include <ted/version.h>
#include <ted/read.h>
#include <ted/unpack.h>
#include <ted/dsd.h>
#include <ted/toolbox.h>

/* "efw_dcm: includes */
#include <efw_dcom.h>

/* ------------------------------------------------------------- */
/* Constants. */

/* ------------------------------------------------------------- */
/* Typedefs. */

/* ------------------------------------------------------------- */
/* Variables and arrays. */

/* ------------------------------------------------------------- */
/* Function declations. */

extern int  StartEFWDataSession (FILE *nsdf, FILE *bsdf, FILE *hkdf,
    double STime, int imode, int SpCraft, int Cluster1Flag,
    int EfwFormat, efw_decom_contents *EfwState) ;

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif  /* EFW_INTERFACE_H */

/* ------------------------------------------------------------- */
/*  end:  efw_interface.h  */
