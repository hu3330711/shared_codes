/*
 ***********************************************************************
 *
 *     General_sdt_ascii_dump_files.h
 *
 *     Author: Winston Teitler.
 *
 ***********************************************************************
 *
 *     C header for the SDT ASCII dump files.
 *
 ***********************************************************************
 *
 *     @(#)General_sdt_ascii_dump_files.h	1.1    09/07/07    UCB SSL
 *
 ***********************************************************************
 */



/*
 ***********************************************************************
 *
 *     We assume the simplest case: SDT ASCII dump, separate files, line
 *     plots of time series, with no merged plots.
 *
 *     Therefore, each file corresponds to 1 single line plot with 1
 *     single trace.
 *
 *     There should be 2 components (time, and data value), each of
 *     depth equal to 1.
 *
 ***********************************************************************
 */





/*
 *======================================================================
 *======================================================================
 */





#ifndef  GENERAL_SDT_ASCII_DUMP_FILES_H
#define  GENERAL_SDT_ASCII_DUMP_FILES_H





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



/* Max. length of input line */
#define  SDT_FILE_MAX_LEN_I    (10000)

/* Max. space needed for input line */
#define  SDT_FILE_MAX_SPACE_I  (SDT_FILE_MAX_LEN_I + 3)

/* Max. space length of a part of an input line */
#define  SDT_FILE_MAX_SUB_LEN_I    (SDT_FILE_MAX_LEN_I - 100)



/* Comment characters for an SDT file */
#define  SDT_FILE_COMMENT_CHARS    "%#"



/* Format to write a time tag */
#define  SDT_FILE_FMT_W_TIME_TAG   "%12.6f"





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
 *     Values for an SDT file header.
 */


   struct WT_T_SDT_File_Header_struct


     {


       char *name ;            /* plot name */


       char *label ;           /* trace label */


       long int beg_jdn ;      /* begin JDN */

       double beg_time ;       /* begin time (ref. to beg_jdn) */


       long int end_jdn ;      /* end JDN */

       double end_time ;       /* end time (ref. to end_jdn) */


       int n_pts ;             /* no. of data points */


       int n_comps ;           /* no. of components */


       int *comp_depth ;       /* array of n_comps comp depths */


     } ;


   typedef
       struct WT_T_SDT_File_Header_struct
           WT_T_SDT_File_Header ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Values for an SDT file data line.
 */


   struct WT_T_SDT_File_Data_Line_struct


     {


       int index ;             /* data point index */


       double time_tag ;       /* data point time tag */


       double value ;          /* data point value */


     } ;


   typedef
       struct WT_T_SDT_File_Data_Line_struct
           WT_T_SDT_File_Data_Line ;





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



   int
       Print_WT_T_SDT_File_Header(const WT_T_SDT_File_Header *p,
           FILE *o_fp) ;


   int
       Check_WT_T_SDT_File_Header(
           const WT_T_SDT_File_Header *p) ;


   int
       Copy_WT_T_SDT_File_Header(const WT_T_SDT_File_Header *p,
           WT_T_SDT_File_Header *q) ;


   int
       Read_WT_T_SDT_File_Header(WT_T_File_Info *file_info_ptr,
           WT_T_SDT_File_Header *p) ;


   int
       Write_WT_T_SDT_File_Header(const WT_T_SDT_File_Header *p,
           WT_T_File_Info *file_info_ptr) ;


   int
       Print_WT_T_SDT_File_Data_Line(
           const WT_T_SDT_File_Data_Line *p,
           FILE *o_fp) ;


   int
       Read_WT_T_SDT_File_Data_Line(
           WT_T_File_Info *file_info_ptr,
           WT_T_SDT_File_Data_Line *p, int *end_flag_ptr) ;


   int
       Write_WT_T_SDT_File_Data_Line(
           const WT_T_SDT_File_Data_Line *p,
           WT_T_File_Info *file_info_ptr) ;


   int
       Write_SDT_File_Trailer(const WT_T_SDT_File_Header *p,
           WT_T_File_Info *file_info_ptr) ;





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





#endif   /* GENERAL_SDT_ASCII_DUMP_FILES_H */
