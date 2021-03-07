/*
 ***********************************************************************
 *
 *     wt_POLAR_001.h
 *
 *     Author: Winston Teitler.
 *
 ***********************************************************************
 *
 *     C header for the POLAR library.
 *
 ***********************************************************************
 *
 *     @(#)wt_POLAR_001.h	1.9    05/08/02    UCB SSL
 *
 ***********************************************************************
 */





/*
 *======================================================================
 *======================================================================
 */





#ifndef  WT_POLAR_001_H
#define  WT_POLAR_001_H





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Headers.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     General purpose C header.
 *
 *----------------------------------------------------------------------
 */



#include <wtgen001.h>





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
 *     POLAR "Computed" files.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     General information.
 *
 *----------------------------------------------------------------------
 */



/*
 *     These files are created typically by running sdt in batch mode.
 *
 *
 *     Each file contains data for one single day.
 */



/*
 *----------------------------------------------------------------------
 *
 *     File types.
 *
 *----------------------------------------------------------------------
 */



/*
 *     File types are integer numbers.
 *
 *     There is a number of different possible file types.
 *
 *     The symbolic constant defined below contains the number of
 *     different file types.
 *
 *     File types go from 0 to (number of file types - 1).
 */


/* No. of file types */
#define  WT_POL_COMP_N_FILE_TYPES  (18)



/*
 *----------------------------------------------------------------------
 *
 *     File names.
 *
 *----------------------------------------------------------------------
 */



/*
 *     The following refers to the name of a file; this is just the file
 *     name, without any path or directory.
 *
 *
 *
 *     The name of a file consists of 3 parts, separated from each other
 *     by a single underscore.
 *
 *
 *
 *     1) Part 1 is a string that is determined by the file type.
 *
 *        There is a function to provide this string; the name of the
 *        function is
 *
 *            POLAR_GetCompFileName1
 *
 *
 *
 *     2) Part 2 is a string that is determined by the date of the data.
 *
 *        Format is YYYYMMDD (for example, 19960704).
 *
 *
 *
 *     3) Part 3 is a string that is determined by the version number of
 *        the file.
 *
 *        The version number is used with a length of 2 digits (leading
 *        0 inserted if necessary).
 *
 *        Format is vNN (for example, v01).
 */



/*
 *----------------------------------------------------------------------
 *
 *     File header.
 *
 *----------------------------------------------------------------------
 */



/*
 *     The file header consists of an ASCII part and a binary part.
 *
 *
 *     The ASCII part comes first; it consists of character data, and it
 *     is intended for visual inspection only.
 *
 *     This part consists of an array of char.
 *
 *
 *     The binary part comes next; it consists of integer data, and it
 *     is intended for use by programs.
 *
 *     This part consists of an array of int.
 */


/* No. of bytes for the ASCII part of the file header */
#define  WT_POL_COMP_HDR_ASCII_NBYTES  (960)

/* No. of bytes for the binary part of the file header */
#define  WT_POL_COMP_HDR_BINARY_NBYTES  (64)


/* No. of integers in the binary part of the file header */
#define  WT_POL_COMP_HDR_BINARY_NINTS  \
    (WT_POL_COMP_HDR_BINARY_NBYTES / WT_NBYTES_LONG)




   struct wt_POL_comp_hdr_struct


     {


/*
 *     ASCII part of the header.
 */

       char ASCII_data[WT_POL_COMP_HDR_ASCII_NBYTES] ;


/*
 *     Binary part of the header.
 */

       int binary_data[WT_POL_COMP_HDR_BINARY_NINTS] ;


     } ;


   typedef
       struct wt_POL_comp_hdr_struct
       WT_T_POL_Comp_Hdr ;




/* Size of the file header */
#define  WT_POL_COMP_HDR_SIZE               \
           (WT_POL_COMP_HDR_ASCII_NBYTES +  \
            WT_POL_COMP_HDR_BINARY_NBYTES)



/*
 *     Indices of the binary integers.
 */


/* Index of the file type */
#define  WT_POL_COMP_HDR_FILE_TYPE_INDEX  (0)

/* Index of the version number */
#define  WT_POL_COMP_HDR_VERSION_NUMBER_INDEX  (1)

/* Index of the record count */
#define  WT_POL_COMP_HDR_RECORD_COUNT_INDEX  (2)

/* Index of the date of the data */
#define  WT_POL_COMP_HDR_DATE_OF_DATA_INDEX  (3)

/* Index of the start date */
#define  WT_POL_COMP_HDR_START_DATE_INDEX  (4)

/* Index of the number of double */
#define  WT_POL_COMP_HDR_N_DOUBLE_INDEX  (5)

/* Index of the number of float */
#define  WT_POL_COMP_HDR_N_FLOAT_INDEX  (6)





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
 *     POLAR "Computed" files.
 *
 *----------------------------------------------------------------------
 */



   int
       POLAR_GetCompFileName1(int type,
           char **name1_ptr, int *n_double_ptr, int *n_float_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Plasma density.
 *
 *----------------------------------------------------------------------
 */



   int
       POLAR_Plasma_Density_001(double pot,
           double *n_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Spacecraft rotation.
 *
 *----------------------------------------------------------------------
 */



   int
       POLAR_Spin_Pulse_Parm(long int jdn, double time,
           WT_T_Spin_Pulse_Parm **parm_ptr_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Transformations of coordinates.
 *
 *----------------------------------------------------------------------
 */



   void
       POLAR_RotMat_DSC_SPC(const double *att_GSE_ptr,
           double a[3][3]) ;





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





#endif   /* WT_POLAR_001_H */
