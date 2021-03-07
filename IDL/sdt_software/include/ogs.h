/*
 ***********************************************************************
 *
 *     ogs.h
 *
 *     Author: Winston Teitler.
 *
 ***********************************************************************
 *
 *     Header for the ogs programs.
 *
 ***********************************************************************
 *
 *     @(#)ogs.h	1.6    03/25/03    UCB SSL
 *
 ***********************************************************************
 */





/*
 *======================================================================
 *======================================================================
 */





#ifndef  OGS_H
#define  OGS_H





/*
 *======================================================================
 *======================================================================
 */





#ifdef  __cplusplus
   extern  "C"
   {
#endif





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Constants.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     Dates and times.
 *
 *----------------------------------------------------------------------
 */



#define  WT_FAST_MINCENT       (1900)  /* minimum century */

#define  WT_FAST_MINYEAR       (1994)  /* minimum year */



/*
 *----------------------------------------------------------------------
 *
 *     Goddard file name format.
 *
 *----------------------------------------------------------------------
 */



#define  WT_FAST_MAXNLEN       (40)    /* max name length */

/* Array size for strings */
#define  WT_FAST_NBUFF         (WT_FAST_MAXNLEN + 1)

#define  WT_FAST_GDEFBVER      "-1"    /* def Berk dupl ver no */

#define  WT_FAST_GN1DIGYY      (2)     /* 1 no digits year */

#define  WT_FAST_GN1DIGDDD     (3)     /* 1 no digits day */

#define  WT_FAST_GN1DIGNN      (2)     /* 1 no digits Godd ver no */





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Type definitions.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     Parsed Goddard-type file name.
 *
 *----------------------------------------------------------------------
 */



/*
 *
 *     File name type 1.
 *
 */

   struct ogs_file_name_1_struct
     {
       char filnam[WT_FAST_NBUFF] ;  /* file name */
       char prefix[WT_FAST_NBUFF] ;  /* file name prefix */
       char yy[WT_FAST_GN1DIGYY + 1] ;  /* year */
       char ddd[WT_FAST_GN1DIGDDD + 1] ;  /* day of year */
       char code[WT_FAST_NBUFF] ;  /* file type code */
       char nn[WT_FAST_GN1DIGNN + 1] ;  /* Godd ver no */
       char m[WT_FAST_NBUFF] ; /* Berk dupl ver no */
       int year ;              /* year */
       int yearday ;           /* day of year */
       int gver ;              /* Godd ver no */
       int bver ;              /* Berk dupl ver no */
       long int jdn ;          /* Jul Day Number */
     } ;

   typedef
       struct ogs_file_name_1_struct
       ogsFileName1 ;



/*
 *
 *     File name type 2.
 *
 */

   struct ogs_file_name_2_struct
     {
       char filnam[WT_FAST_NBUFF] ;  /* file name */
       char code[WT_FAST_NBUFF] ;  /* file type code */
       char suffix[WT_FAST_NBUFF] ;  /* file name suffix */
       char w[WT_FAST_NBUFF] ; /* week of year */
       char v[WT_FAST_NBUFF] ; /* Godd ver no */
       char m[WT_FAST_NBUFF] ; /* Berk dupl ver no */
       int week ;              /* week of year */
       int gver ;              /* Godd ver no */
       int bver ;              /* Berk dupl ver no */
     } ;

   typedef
       struct ogs_file_name_2_struct
       ogsFileName2 ;





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Function prototypes.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     File names.
 *
 *----------------------------------------------------------------------
 */



   int
       OgsCompFileNames1(const ogsFileName1 *a_ptr,
           const ogsFileName1 *b_ptr,
           int *sameDate_ptr) ;


   int
       OgsCompFileNames2(const ogsFileName2 *a_ptr,
           const ogsFileName2 *b_ptr,
           int *sameDate_ptr) ;


   int
       OgsOrbFileEndTime(FILE *fp,
           int *year_ptr, int *month_ptr, int *day_ptr,
           int *yearday_ptr, int *hr_ptr, int *min_ptr, double *sec_ptr,
           long int *jdn_ptr, double *time_ptr) ;


   int
       OgsOrbStartTime(FILE *fp,
           long int *norbit_ptr, int *year_ptr, int *month_ptr,
           int *day_ptr, int *yearday_ptr, int *hr_ptr, int *min_ptr,
           double *sec_ptr, long int *jdn_ptr, double *time_ptr,
           int *eof_ptr) ;


   int
       OgsParseFileName1(const char *filnam,
           ogsFileName1 *name_ptr) ;


   int
       OgsParseFileName2(const char *filnam,
           ogsFileName2 *name_ptr) ;





/*
 *======================================================================
 *======================================================================
 */





#ifdef  __cplusplus
   }
#endif





/*
 *======================================================================
 *======================================================================
 */





#endif   /* OGS_H */
