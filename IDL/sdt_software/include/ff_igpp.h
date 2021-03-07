/*
 +--------------------------------------------------------------------+ 
 | Copyright(c) 1991 Regents of the University of California          |
 | All rights reserved.                                               |
 |                                                                    |
 | Redistribution and use in source and binary forms are permitted    |
 | provided that the above copyright notice and this paragraph are    |
 | duplicated in all such forms and that any documentation,           |
 | advertising materials, and other materials related to such         |
 | distribution and use acknowledge that the software was developed   |
 | by the University of California, Los Angeles.  The name of the     |
 | University may not be used to endorse or promote products derived  |
 | from this software without specific prior written permission.      |
 | THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR     |
 | IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED     |
 | WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.|
 +--------------------------------------------------------------------+ 

*/

/*
@(#)ff_igpp.h	1.3, 01/04/07
*/

#ifndef FF_IGPP

#ifdef vax
#include stdio.h
#include errno.h
#else
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#ifdef MACOSX_A
#include <sys/wait.h>
#else
#include <wait.h>
#endif
#endif

#include <time_igpp.h>

#define FF_LOCK		020
#define FF_EXIST	010
#define FF_CREATE	004
#define FF_READ		002
#define FF_WRITE	001

#define FF_MAINID	000
#define FF_HEADER	001
#define FF_DATA		002

#define TIME66		000
#define DOUBLE		001
#define FLOAT		002
#define LONG		003
#define ULONG		004
#define SHORT		005
#define USHORT		006
#define BYTE		007
#define CHAR		010

#ifndef LINUX_A
extern int errno;
#endif

#ifdef FF_LIB

#ifdef vax
FILE *ff_stdout=NULL;	/* file pointer for "opened" messages and ff_hlist */
FILE *ff_stderr=NULL;	/* file pointer for errors */
#else
#ifdef LINUX_A
FILE *ff_stdout=NULL;	/* file pointer for "opened" messages and ff_hlist */
FILE *ff_stderr=NULL;	/* file pointer for errors */
#else
FILE *ff_stdout=stdout;	/* file pointer for "opened" messages and ff_hlist */
FILE *ff_stderr=stderr;	/* file pointer for errors */
#endif
#endif

#else
extern FILE *ff_stdout;
extern FILE *ff_stderr;
#endif

/*
  The first four elements of ff_id, ff_header and ff_data should be
  indentical so that ff_reporterror can be called with a pointer
  to any of these structures.
*/
struct ff_id {
  int type;
  struct ff_header *ffh;
  struct ff_data *ffd;
  struct ff_id *id;
  int status,opsys;
} ;

/*
  The first 7 elements of ff_header and ff_data should be
  indentical so that ff_reporterror can be called with a pointer
  to either of these structures.
*/
struct ff_header {
  int type;
  struct ff_header *ffh;
  struct ff_data *ffd;
  struct ff_id *id;
  FILE *fp;
  char *name;
  int error;
  int loc;
};
struct ff_data {
  int type;
  struct ff_header *ffh;
  struct ff_data *ffd;
  struct ff_id *id;
  FILE *fp;
  char *name;
  int error;
  struct ff_datafields *df;
  long nrows,size;
  int recl,ncols;
};

struct ff_param {
  int status,recl,ncols,nbufs;
  unsigned long nrows;
};


struct ff_h_info {
  double first_time,last_time,resolution,flag;
  char owner[L_cuserid];
  int first_orbit,last_orbit;
};

#define FF_COL_NAMELEN 20
#define FF_COL_UNITSLEN 20
#define FF_COL_SOURCELEN 30
#define FF_COL_TYPELEN 8
struct ff_col_desc {
  int ncol;
  char name[FF_COL_NAMELEN];
  char units[FF_COL_UNITSLEN];
  char source[FF_COL_SOURCELEN];
  char type[FF_COL_TYPELEN];
  int loc;
};

struct ff_datafields { int type,loc,len; };

typedef struct ff_id FF_ID ;
typedef struct ff_header FF_HID ;
typedef struct ff_data FF_DID ;
typedef struct ff_col_desc FF_COL_DESC ;
typedef struct ff_h_info FF_H_INFO ;
typedef struct ff_param FF_O_PARAM ;

#ifdef ANSI_STD_C

FF_ID *ff_open( char *fname, FF_O_PARAM *o_parms) ;
int ff_close( FF_ID *id) ;
int ff_put( FF_DID *id, char *record, int len) ;
int ff_get( FF_DID *id, char *record, int len) ;
int ff_cget( FF_ID *id, void *record) ;
int ff_cput( FF_HID *id, char *record) ;
int ff_hput( FF_HID *id, FF_COL_DESC *hr) ;
int ff_hget( FF_HID *id, FF_COL_DESC *hr) ;
int ff_hputinfo( FF_HID *id, FF_H_INFO *info) ;
int ff_hgetinfo( FF_HID *id, FF_H_INFO *info) ;
long ff_setrow( FF_DID *id, long row) ;
long ff_getrow( FF_DID *id) ;
long ff_bsearch( FF_ID *id, double time, long row1, long row2) ;
void ff_reporterror( FF_HID *id) ;
int ff_buildname( char *inname, char *outname, int type, int path) ;
void ff_routineerror( char *s) ;
char *strsrch( char *string1, char *string2) ;
int ff_hupdinfo( FF_HID *id, FF_H_INFO *info) ;

#else /* ANSI_STD_C */

FF_ID *ff_open();
int ff_put(),ff_get(),ff_cput(),
	ff_cget(),ff_hput(),ff_hget(),ff_hputinfo(),ff_hgetinfo(),
	ff_close();
long ff_setrow(),ff_getrow(),ff_bsearch();
void ff_reporterror();

#endif /* ANSI_STD_C */

#define FF_IGPP
#endif

