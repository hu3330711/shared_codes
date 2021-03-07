/* ------------------------------------------------------------------- */
/*
 * cluster_const.h
 *
 * These are the declarations that should be relevant to all
 * Cluster data as defined by ESA:
 *
 */

#ifndef CLUSTER_CONST_H
#define CLUSTER_CONST_H

/* For SCCS */
#define SccsId_cluster_const_h "@(#)cluster_const.h	1.4, 05/21/04"

#include <stdio.h>
#include <stdlib.h>

#ifdef OLD_STYLE_STR_HDRS
#include <iostream.h>
#include <fstream.h>
#else
#include <iostream>
#include <fstream>
using std::cin ;
using std::cout ;
using std::cerr ;
using std::flush ;
using std::endl ;
#endif

#include <ctype.h>

/* ------------------------------------------------------------------- */
/* Typedefs: */

/* ------------------------------------------------------------------- */
/* Constants. */

#define  MAX_CLS_CD_CATALOGUE_ENTRIES   256

#endif /* CLUSTER_CONST_H */
