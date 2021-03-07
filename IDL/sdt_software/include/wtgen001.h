/*
 ***********************************************************************
 *
 *     wtgen001.h
 *
 *     Author: Winston Teitler.
 *
 ***********************************************************************
 *
 *     General purpose C header.
 *
 ***********************************************************************
 *
 *     @(#)wtgen001.h	1.71    06/22/07    UCB SSL
 *
 ***********************************************************************
 */





/*
 *======================================================================
 *======================================================================
 */





#ifndef  WTGEN001_H
#define  WTGEN001_H





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
 *     Standard libraries.
 *
 *----------------------------------------------------------------------
 */



#include <assert.h>

#include <ctype.h>

#include <errno.h>

#include <float.h>

#include <limits.h>

#include <locale.h>

#include <math.h>

#include <setjmp.h>

#include <signal.h>

#include <stdarg.h>

#include <stddef.h>

#include <stdio.h>

#include <stdlib.h>

#include <string.h>

#include <time.h>





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
 *     General.
 *
 *----------------------------------------------------------------------
 */



#define  WT_TRUE               (1)     /* logical TRUE value */

#define  WT_FALSE              (0)     /* logical FALSE value */



#define  WT_HUGE_VALUE         (1.0e+36)  /* a very large value */

#define  WT_TINY_VALUE         (1.0e-36)  /* a very small value */



#define  WT_TOL                (1.0e-3)  /* default numeric tolerance */

#define  WT_NEAR_1             (1.0 - WT_TOL)  /* default "near 1" */



/*
 *     Default large limit.
 */

#define  WT_HUGE_LIM           (WT_NEAR_1 * WT_HUGE_VALUE)



/*
 *----------------------------------------------------------------------
 *
 *     General - Data types.
 *
 *----------------------------------------------------------------------
 */



#define  WT_TYPE_CHAR          (11)    /* "plain" char */

#define  WT_TYPE_S_CHAR        (12)    /* signed char */

#define  WT_TYPE_U_CHAR        (13)    /* unsigned char */



#define  WT_TYPE_SHORT_INT     (31)    /* short int */

#define  WT_TYPE_INT           (32)    /* int */

#define  WT_TYPE_LONG_INT      (33)    /* long int */

#define  WT_TYPE_U_SHORT_INT   (34)    /* unsigned short int */

#define  WT_TYPE_U_INT         (35)    /* unsigned int */

#define  WT_TYPE_U_LONG_INT    (36)    /* unsigned long int */



#define  WT_TYPE_FLOAT         (51)    /* float */



#define  WT_TYPE_DOUBLE        (71)    /* double */

#define  WT_TYPE_LONG_DOUBLE   (72)    /* long double */



/*
 *----------------------------------------------------------------------
 *
 *     General - General-purpose functions.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Function code for the general-purpose function Gen_F_1_Var_001.
 */

#define  WT_GEN_F_1_TEST_CODE                  (0)

#define  WT_GEN_F_1_IDENTITY                   (1)

#define  WT_GEN_F_1_ABS_VAL                    (2)

#define  WT_GEN_F_1_NEGATIVE                   (3)

#define  WT_GEN_F_1_RECIPROCAL                 (4)

#define  WT_GEN_F_1_SQUARE                     (5)

#define  WT_GEN_F_1_SQ_ROOT                    (6)



/*
 *     Function code for the general-purpose function Gen_F_2_Var_001.
 */

#define  WT_GEN_F_2_TEST_CODE                  (0)

#define  WT_GEN_F_2_SUM                        (1)

#define  WT_GEN_F_2_DIFF                       (2)

#define  WT_GEN_F_2_PRODUCT                    (3)

#define  WT_GEN_F_2_QUOTIENT                   (4)

#define  WT_GEN_F_2_SUM_SQ                     (5)

#define  WT_GEN_F_2_SQ_ROOT_SUM_SQ             (6)

#define  WT_GEN_F_2_DIFF_OVER_AVG              (7)



/*
 *----------------------------------------------------------------------
 *
 *     Angles.
 *
 *----------------------------------------------------------------------
 */



#define  WT_PI                 (3.1415926535897932)  /* pi */



#define  WT_TWOPI              (2.0 * WT_PI)  /* 2 * pi */

#define  WT_HPI                (0.5 * WT_PI)  /* pi / 2 */



#define  WT_CIRRAD             (WT_TWOPI)  /* circle (radians) */

#define  WT_HCIRRAD            (WT_PI) /* half circle (radians) */

#define  WT_QCIRRAD            (WT_HPI)  /* quarter circle (radians) */



#define  WT_CIRDEG             (360.0) /* circle (degrees) */

#define  WT_HCIRDEG            (180.0) /* half circle (degrees) */

#define  WT_QCIRDEG            (90.0)  /* quarter circle (degrees) */



#define  WT_CIRHR              (24.0)  /* circle (hours) */

#define  WT_HCIRHR             (12.0)  /* half circle (hours) */

#define  WT_QCIRHR             (6.0)   /* quarter circle (hours) */



/*
 *----------------------------------------------------------------------
 *
 *     Angles - Multiplicative factors for conversion of units.
 *
 *----------------------------------------------------------------------
 */



#define  WT_RAD_TO_DEG         (WT_HCIRDEG / WT_PI)  /* rad -> deg */

#define  WT_DEG_TO_RAD         (WT_PI / WT_HCIRDEG)  /* deg -> rad */



#define  WT_RAD_TO_HR          (WT_HCIRHR / WT_PI)  /* rad -> hr */

#define  WT_HR_TO_RAD          (WT_PI / WT_HCIRHR)  /* hr -> rad */



#define  WT_DEG_TO_HR          (WT_HCIRHR / WT_HCIRDEG)  /* deg -> hr */

#define  WT_HR_TO_DEG          (WT_HCIRDEG / WT_HCIRHR)  /* hr -> deg */



/*
 *----------------------------------------------------------------------
 *
 *     Astronomy.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Earth equatorial radius (Km).
 */

#define  WT_EARTH_EQUAT_RADIUS (6378.140)



/*
 *     Earth polar radius (Km).
 */

#define  WT_EARTH_POLAR_RADIUS (6356.755)



/*
 *     Earth mean radius (Km).
 */

#define  WT_EARTH_MEAN_RADIUS  (6371.2)



/*
 *     Earth radius Re (Km).
 */

#define  WT_RE                 (WT_EARTH_MEAN_RADIUS)



/*
 *----------------------------------------------------------------------
 *
 *     Astronomy - Standard epochs.
 *
 *----------------------------------------------------------------------
 */



#define  WT_EPOCH_1900JAN05_YEAR       (1900)  /* 1900 Jan 0.5 */

#define  WT_EPOCH_1900JAN05_YEARDAY    (0)

#define  WT_EPOCH_1900JAN05_HR         (12)

#define  WT_EPOCH_1900JAN05_MIN        (0)

#define  WT_EPOCH_1900JAN05_SEC        (0.0)



#define  WT_EPOCH_B19500_YEAR          (1950)  /* B1950.0 */

#define  WT_EPOCH_B19500_YEARDAY       (0)

#define  WT_EPOCH_B19500_HR            (22)

#define  WT_EPOCH_B19500_MIN           (9)

#define  WT_EPOCH_B19500_SEC           (7.2)



#define  WT_EPOCH_J20000_YEAR          (2000)  /* J2000.0 */

#define  WT_EPOCH_J20000_YEARDAY       (1)

#define  WT_EPOCH_J20000_HR            (12)

#define  WT_EPOCH_J20000_MIN           (0)

#define  WT_EPOCH_J20000_SEC           (0.0)



/*
 *----------------------------------------------------------------------
 *
 *     Bits and bytes.
 *
 *----------------------------------------------------------------------
 */



#define  WT_NBITS_BYTE         ((int) (CHAR_BIT))  /* bits per byte */

#define  WT_NBYTES_LONG        (4)     /* bytes per long int */



/*
 *     Bits per long int.
 */

#define  WT_NBITS_LONG         (WT_NBYTES_LONG * WT_NBITS_BYTE)



/*
 *----------------------------------------------------------------------
 *
 *     Characters.
 *
 *----------------------------------------------------------------------
 */



#define  WT_NULL_CHAR          ('\0')  /* null character */



#define  WT_NEWLINE            ('\n')  /* newline */



#define  WT_SPACE              (' ')   /* space */

#define  WT_HORIZONTAL_TAB     ('\t')  /* horizontal tab */



#define  WT_SINGLE_QUOTE       ('\'')  /* single quote */

#define  WT_DOUBLE_QUOTE       ('\"')  /* double quote */



#define  WT_ALERT              ('\a')  /* alert (bell) */

#define  WT_BACKSPACE          ('\b')  /* backspace */

#define  WT_FORM_FEED          ('\f')  /* form feed */

#define  WT_CARRIAGE_RETURN    ('\r')  /* carriage return */

#define  WT_VERTICAL_TAB       ('\v')  /* vertical tab */

#define  WT_BACKSLASH          ('\\')  /* backslash */

#define  WT_QUESTION_MARK      ('\?')  /* question mark */



/*
 *----------------------------------------------------------------------
 *
 *     Character strings.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Empty string.
 */

#define  WT_EMPTY_STRING       ""



/*
 *     White space characters inside a line (restricted definition).
 */

#define  WT_WHITE_SPACE        " \t"



/*
 *     White space characters to be trimmed from a string (restricted
 *     definition).
 */

#define  WT_WHITE_SPACE_TRIM   "\n\r \t"



/*
 *----------------------------------------------------------------------
 *
 *     Codes.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Distance.
 */

#define  WT_UNITS_KM           (1)     /* units are Km */

#define  WT_UNITS_RE           (2)     /* units are Re */



/*
 *----------------------------------------------------------------------
 *
 *     Comparison.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Basic comparison return values.
 */

#define  WT_CMP_LESS           (-1)    /* return value for < */

#define  WT_CMP_EQUAL          (0)     /* return value for == */

#define  WT_CMP_GREATER        (1)     /* return value for > */



#define  WT_CMP_ALG            (1)     /* algebraic value */



/*
 *     Comparison to interval.
 */

#define  WT_CMP_I_LT_A         (1)     /* code for: x < a */

#define  WT_CMP_I_LE_A         (2)     /* code for: x <= a */

#define  WT_CMP_I_EQ_A         (3)     /* code for: x == a */

#define  WT_CMP_I_NE_A         (4)     /* code for: x != a */

#define  WT_CMP_I_GE_A         (5)     /* code for: x >= a */

#define  WT_CMP_I_GT_A         (6)     /* code for: x > a */

#define  WT_CMP_I_GE_A_LT_B    (7)     /* code for: a <= x < b */

#define  WT_CMP_I_GE_A_LE_B    (8)     /* code for: a <= x <= b */

#define  WT_CMP_I_GT_A_LT_B    (9)     /* code for: a < x < b */

#define  WT_CMP_I_GT_A_LE_B   (10)     /* code for: a < x <= b */

#define  WT_CMP_I_N_CODES     (10)     /* no. of codes for comp. int. */



/*
 *     Comparison condition for sequence of data values.
 *
 *     The symbols refer to the comparison between an earlier data value
 *     on the left side, and a later data value on the right side.
 */

#define  WT_SEQ_NONE           (1)     /* code for: no condition */

#define  WT_SEQ_INC            (2)     /* code for: inc <= */

#define  WT_SEQ_STR_INC        (3)     /* code for: strictly inc < */

#define  WT_SEQ_DEC            (4)     /* code for: dec >= */

#define  WT_SEQ_STR_DEC        (5)     /* code for: strictly dec > */

#define  WT_SEQ_N_CODES        (5)     /* no. of codes for comp. seq. */




/*
 *----------------------------------------------------------------------
 *
 *     Date and time.
 *
 *----------------------------------------------------------------------
 */



#define  WT_YEAR0              (1600)  /* base year for JDN conv  */

#define  WT_JDN00              (2305447L)  /* base year "January 0" */



#define  WT_MINCENT            (1900)  /* minimum century for full yr */

#define  WT_MINYEAR            (1994)  /* minimum year for full year */



#define  WT_MONTHS_IN_YEAR       (12)  /* no. of months in one year */

#define  WT_DAYS_IN_WEEK          (7)  /* no. of days in one week */



/*
 *----------------------------------------------------------------------
 *
 *     Date and time - Epochs.
 *
 *----------------------------------------------------------------------
 */



/*
 *     "Unix time".
 */

#define  WT_EPOCH_UNIX_TIME_YEAR       (1970)

#define  WT_EPOCH_UNIX_TIME_D_O_Y      (1)

#define  WT_EPOCH_UNIX_TIME_HR         (0)

#define  WT_EPOCH_UNIX_TIME_MIN        (0)

#define  WT_EPOCH_UNIX_TIME_SEC        (0.0)



/*
 *     "1900 time".
 */

#define  WT_EPOCH_1900_TIME_YEAR       (1900)

#define  WT_EPOCH_1900_TIME_D_O_Y      (1)

#define  WT_EPOCH_1900_TIME_HR         (0)

#define  WT_EPOCH_1900_TIME_MIN        (0)

#define  WT_EPOCH_1900_TIME_SEC        (0.0)



/*
 *----------------------------------------------------------------------
 *
 *     Date and time - Units for time.
 *
 *----------------------------------------------------------------------
 */



#define  WT_TIM_UNITS_SEC      (1)     /* units are seconds */

#define  WT_TIM_UNITS_MIN      (2)     /* units are minutes */

#define  WT_TIM_UNITS_HR       (3)     /* units are hours */

#define  WT_TIM_UNITS_DAY      (4)     /* units are days */



#define  WT_TIM_DAY_HR         (24.0)  /* day (hours) */

#define  WT_TIM_HR_MIN         (60.0)  /* hour (minutes) */

#define  WT_TIM_MIN_SEC        (60.0)  /* minute (seconds) */



/*
 *----------------------------------------------------------------------
 *
 *     Date and time - Multiplicative factors for conversion of units
 *     for time.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Factors for conversion to seconds.
 */


/*
 *     Min -> sec.
 */

#define  WT_TIM_MIN_TO_SEC     (WT_TIM_MIN_SEC)


/*
 *     Hr -> sec.
 */

#define  WT_TIM_HR_TO_SEC      (WT_TIM_HR_MIN * WT_TIM_MIN_TO_SEC)



/*
 *     Day -> sec.
 */

#define  WT_TIM_DAY_TO_SEC     (WT_TIM_DAY_HR * WT_TIM_HR_TO_SEC)



/*
 *     Factors for conversion from seconds.
 */


/*
 *     Sec -> min.
 */

#define  WT_TIM_SEC_TO_MIN     (1.0 / WT_TIM_MIN_TO_SEC)


/*
 *     Sec -> hr.
 */

#define  WT_TIM_SEC_TO_HR      (1.0 / WT_TIM_HR_TO_SEC)


/*
 *     Sec -> day.
 */

#define  WT_TIM_SEC_TO_DAY     (1.0 / WT_TIM_DAY_TO_SEC)



/*
 *----------------------------------------------------------------------
 *
 *     Error messages.
 *
 *----------------------------------------------------------------------
 */



#define  WT_ERR_MSG_SPACE      (4000)  /* no. of chars for err. msg. */



/*
 *----------------------------------------------------------------------
 *
 *     File codes.
 *
 *----------------------------------------------------------------------
 */



#define  WT_IO_STDIN           (0)     /* standard input */

#define  WT_IO_STDOUT          (1)     /* standard output */

#define  WT_IO_STDERR          (2)     /* standard error */

#define  WT_IO_FILE            (-1)    /* ordinary file */



/*
 *----------------------------------------------------------------------
 *
 *     Files - Mode for opening a file.
 *
 *----------------------------------------------------------------------
 */



/*
 *     The modes are described in "The C Programming Language", Brian W.
 *     Kernighan and Dennis M Ritchie, 2nd. Edition; page 242, function
 *     "fopen".
 */



#define  WT_FILE_OPEN_R        "r"     /* text file, reading */

#define  WT_FILE_OPEN_W        "w"     /* text file, writing */

#define  WT_FILE_OPEN_A        "a"     /* text file, append */

#define  WT_FILE_OPEN_R_PL     "r+"    /* text file, update r/w */

#define  WT_FILE_OPEN_W_PL     "w+"    /* text file, update create */

#define  WT_FILE_OPEN_A_PL     "a+"    /* text file, update append */

#define  WT_FILE_OPEN_R_B      "rb"    /* binary file, reading */

#define  WT_FILE_OPEN_W_B      "wb"    /* binary file, writing */

#define  WT_FILE_OPEN_A_B      "ab"    /* binary file, append */

#define  WT_FILE_OPEN_R_PL_B   "r+b"   /* binary file, update r/w */

#define  WT_FILE_OPEN_W_PL_B   "w+b"   /* binary file, update create */

#define  WT_FILE_OPEN_A_PL_B   "a+b"   /* binary file, update append */



/*
 *----------------------------------------------------------------------
 *
 *     Function return codes.
 *
 *----------------------------------------------------------------------
 */



#define  WT_OK                 (0)     /* indicates success */

#define  WT_BAD_POS            (1)     /* indicates error, return > 0 */

#define  WT_BAD_NEG            (-1)    /* indicates error, return < 0 */

#define  WT_BAD                (WT_BAD_POS)  /* indicates error */



/*
 *----------------------------------------------------------------------
 *
 *     Geophysics - IGRF.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Highest maximum spherical harmonic degree of the IGRF expansion.
 *
 *
 *
 *     Note that the models for some years may have a different maximum
 *     degree than the models for some other years.
 *
 *     "WT_IGRF_N" is the highest maximum degree for all the models, for
 *     all the years available; this number is used to determine array
 *     sizes.
 */

#define  WT_IGRF_N             (13)



/*
 *     Number of terms of the IGRF expansion (pairs n, m).
 */

#define  WT_IGRF_NTERMS        ((WT_IGRF_N * (WT_IGRF_N + 3)) / 2)



/*
 *     Maximum spherical harmonic degree of the IGRF expansion, for
 *     which secular variation terms are available.
 *
 *
 *
 *     The secular variation terms are for the model for the last year
 *     available.
 */

#define  WT_IGRF_N_SV          (8)



/*
 *----------------------------------------------------------------------
 *
 *     I/O streams.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Input stream.
 */

#define  WT_I                  (stdin)



/*
 *     Output streams.
 */

#define  WT_O_1                (stdout)

#define  WT_O_2                (stderr)



/*
 *     Output stream for messages.
 */

#define  WT_O_MSSG             (WT_O_2)



/*
 *     Output stream for dump of output values.
 */

#define  WT_O_DUMP             (WT_O_1)



/*
 *     Output stream for debugging messages.
 */

#define  WT_O_DBG              (WT_O_MSSG)



/*
 *----------------------------------------------------------------------
 *
 *     Physics.
 *
 *----------------------------------------------------------------------
 */



#define  WT_EV_TO_ERG          (1.60218e-12)  /* mult. factor eV->erg */



#define  WT_MASS_ELECTRON      (9.10939e-28)  /* mass - electron (gm) */

#define  WT_MASS_PROTON        (1.67262e-24)  /* mass of proton (gm) */

#define  WT_MASS_NEUTRON       (1.67493e-24)  /* mass of neutron (gm) */



/*
 *----------------------------------------------------------------------
 *
 *     Sub-intervals.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Codes for sub-interval generation.
 */

#define  WT_SUB_INT_PROPER     (1)     /* proper sub-interval */

#define  WT_SUB_INT_1_PT       (2)     /* sub-interval has only 1 pt. */

#define  WT_SUB_INT_EMPTY      (3)     /* sub-interval is empty */



/*
 *----------------------------------------------------------------------
 *
 *     Tables.
 *
 *----------------------------------------------------------------------
 */



#define  WT_TS_EMPTY           (-1L)   /* empty table */

#define  WT_TS_BEFORE          (-2L)   /* value before table */

#define  WT_TS_AFTER           (-3L)   /* value after table */



/*
 *----------------------------------------------------------------------
 *
 *     Time series - Fourier transform, convolution, etc.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Code for the number of convolution coefficients.
 *
 *     For even number of convolution coefficients, the time tag of the
 *     output data point can be the time tag of the 1st. central point,
 *     or the time tag of the 2nd. central point.
 */

#define  WT_CNV_N_ODD          (1)     /* odd */

#define  WT_CNV_N_EVEN_1       (2)     /* even, time 1st. central pt. */

#define  WT_CNV_N_EVEN_2       (3)     /* even, time 2nd. central pt. */



/*
 *     Code for the divisor for the convolution coefficients.
 */

#define  WT_CNV_DIV_USER       (1)     /* divisor set by user */

#define  WT_CNV_DIV_SUM_1      (2)     /* divisor is sum of coeffs. */

#define  WT_CNV_DIV_SUM_SQ_1   (3)     /* divisor sqrt sum coeff. sq. */



/*
 *     Code for the computation method for a convolution.
 *
 *     The methods are described in the source code file for the
 *     function
 *
 *         Convolution_002
 */

/*
 *     Code for the computation method:
 *
 *         "Multiplications and additions"
 */

#define  WT_CNV_CMP_MULT_ADD   (1)

/*
 *     Code for the computation method:
 *
 *         "FFT with intermediate time domain"
 */

#define  WT_CNV_CMP_FFT_TIME   (2)

/*
 *     Code for the computation method:
 *
 *         "FFT with intermediate frequency domain"
 */

#define  WT_CNV_CMP_FFT_FREQ   (3)



/*
 *----------------------------------------------------------------------
 *
 *     Vectors and matrices.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Number of dimensions of vectors and matrices in 2D.
 */

#define  DIM_2                 (2)

/*
 *     Number of matrix elements in 2D.
 */

#define  N_MAT_ELEM_2          (DIM_2 * DIM_2)



/*
 *     Number of dimensions of vectors and matrices in 3D.
 */

#define  DIM_3                 (3)

/*
 *     Number of matrix elements in 3D.
 */

#define  N_MAT_ELEM_3          (DIM_3 * DIM_3)



/*
 *     Index for the primary matrix of a time tagged matrix structure.
 */

#define  WT_MAT_IDX_PRIM       (0)



/*
 *     Index for the secondary matrix of a time tagged matrix structure.
 */

#define  WT_MAT_IDX_SEC        (1)





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
 *     General.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Interval.
 */


   struct WT_T_Interval_struct


     {


       double lower ;          /* lower limit */

       double upper ;          /* upper limit */


     } ;


   typedef
       struct WT_T_Interval_struct
           WT_T_Interval ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Structure of 2 double.
 *
 *     Key for comparison, and 1 value.
 */


   struct WT_T_Double_Key_Value_struct


     {


       double key ;            /* key */

       double v ;              /* value */


     } ;


   typedef
       struct WT_T_Double_Key_Value_struct
           WT_T_Double_Key_Value ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Time tagged int.
 */


   struct WT_T_Time_Tagged_Int_struct


     {


       double time ;           /* time tag */

       int v ;                 /* int value */


     } ;


   typedef
       struct WT_T_Time_Tagged_Int_struct
           WT_T_Time_Tagged_Int ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Time tagged float.
 */


   struct WT_T_Time_Tagged_Float_struct


     {


       double time ;           /* time tag */

       float v ;               /* float value */


     } ;


   typedef
       struct WT_T_Time_Tagged_Float_struct
           WT_T_Time_Tagged_Float ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Time tagged double.
 */


   struct WT_T_Time_Tagged_Double_struct


     {


       double time ;           /* time tag */

       double v ;              /* double value */


     } ;


   typedef
       struct WT_T_Time_Tagged_Double_struct
           WT_T_Time_Tagged_Double ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     General array.
 */


   struct WT_T_Gen_Array_struct


     {


/*
 *     Code for the data type.
 */

       int type ;


/*
 *     Array of values.
 */

       void *value ;


     } ;


   typedef
       struct WT_T_Gen_Array_struct
           WT_T_Gen_Array ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     General const array.
 */


   struct WT_T_Gen_Const_Array_struct


     {


/*
 *     Code for the data type.
 */

       int type ;


/*
 *     Array of values.
 */

       const void *value ;


     } ;


   typedef
       struct WT_T_Gen_Const_Array_struct
           WT_T_Gen_Const_Array ;



/*
 *----------------------------------------------------------------------
 *
 *     General - General-purpose functions.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Number of auxiliary parameters needed.
 */


   struct WT_T_Gen_F_N_Aux_Param_struct


     {


       int n_I ;               /* no. aux. param. in array I */

       int n_D ;               /* no. aux. param. in array D */


     } ;


   typedef
       struct WT_T_Gen_F_N_Aux_Param_struct
           WT_T_Gen_F_N_Aux_Param ;



/*
 *----------------------------------------------------------------------
 *
 *     Date and time.
 *
 *----------------------------------------------------------------------
 */



/*
 *     JDN/Time.
 *
 *     The time is in seconds.
 *     The time is referred to 0 hours of the date.
 */


   struct WT_T_JDN_Time_struct


     {


       long int jdn ;          /* JDN for the date */

       double time ;           /* time */


     } ;


   typedef
       struct WT_T_JDN_Time_struct
           WT_T_JDN_Time ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Simple time interval.
 *
 *     Has no dates.
 *     Duration and all times are in seconds.
 *     Normally, all times are referred to the same instant (usually, 0
 *     hours of some reference date).
 */


   struct WT_T_Simple_Time_Interval_struct


     {


       double beg_time ;       /* begin time */
       double end_time ;       /* end time */

       double mid_point ;      /* mid-point time */

       double duration ;       /* length of the interval */


     } ;


   typedef
       struct WT_T_Simple_Time_Interval_struct
           WT_T_Simple_Time_Interval ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Time span.
 *
 *     Has a date.
 *     Duration and all times are in seconds.
 *     All times are referred to the same instant (0 hours of the
 *     reference date).
 */


   struct WT_T_Time_Span_struct


     {


       long int jdn ;          /* JDN for the reference date */

       double beg_time ;       /* begin time */

       double end_time ;       /* end time */

       double mid_point ;      /* mid-point time */

       double duration ;       /* duration of time span */


     } ;


   typedef
       struct WT_T_Time_Span_struct
           WT_T_Time_Span ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Time of day.
 *
 *     Hours, minutes, and seconds, without a date.
 *     Comes from time in seconds, which can be negative.
 *     Sign is +1, 0, or -1; hours, minutes, seconds are computed from
 *     the absolute value of the time in seconds.
 */


   struct WT_T_Time_Of_Day_struct


     {


       int sign ;              /* sign for the original no. of secs. */

       int hr ;                /* value is >= 0 ; could be >= 24 */

       int min ;               /* value is >= 0 and <= 59 */

       double sec ;            /* value is >= 0 and < 60 */


     } ;


   typedef
       struct WT_T_Time_Of_Day_struct
           WT_T_Time_Of_Day ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Full date.
 */


   struct WT_T_Full_Date_struct


     {


       int year ;              /* year (full year, e.g., 2006) */

       int month ;             /* month (1-12) */

       int d_o_m ;             /* day of month (1-31) */

       int d_o_y ;             /* day of year (1-366) */

       int d_o_w ;             /* day of week (1-7, 1 = Sunday) */

       long int jdn ;          /* JDN */

       long int mjn ;          /* MJN */


     } ;


   typedef
       struct WT_T_Full_Date_struct
           WT_T_Full_Date ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Full time.
 *
 *     Time of day comes from combining the hours, minutes, and seconds;
 *     it represents seconds since 00:00:00.
 */


   struct WT_T_Full_Time_struct


     {


       int hour ;              /* hours */

       int min ;               /* minutes */

       double sec ;            /* seconds */

       double t_o_d ;          /* time of day */


     } ;


   typedef
       struct WT_T_Full_Time_struct
           WT_T_Full_Time ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Full date and time.
 */


   struct WT_T_Full_Date_Time_struct


     {


       WT_T_Full_Date full_date ;  /* date */

       WT_T_Full_Time full_time ;  /* time */

       WT_T_JDN_Time jdn_time ;    /* JDN/time */


     } ;


   typedef
       struct WT_T_Full_Date_Time_struct
           WT_T_Full_Date_Time ;



/*
 *----------------------------------------------------------------------
 *
 *     Files.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Information for a file.
 *
 *     The file name must contain all directory path information needed.
 *
 *     The file mode is the mode code used to open the file.
 *
 *     The count of lines is intended for text files.
 *     The count of bytes is intended for binary files.
 *     The other counts can be used as desired.
 *
 */


   struct WT_T_File_Info_struct


     {


       char f_name[FILENAME_MAX + 1] ;  /* file name */


       char f_mode[20] ;       /* file mode */


       FILE *f_ptr ;           /* file pointer */


       int n_lines ;           /* no. of lines read or written */

       int n_bytes ;           /* no. of bytes read or written */

       int n_01 ;              /* counts for convenience */
       int n_02 ;
       int n_03 ;


     } ;


   typedef
       struct WT_T_File_Info_struct
           WT_T_File_Info ;



/*
 *----------------------------------------------------------------------
 *
 *     Geophysics - IGRF.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Term of the IGRF expansion.
 */


   struct WT_T_IGRF_Term_struct


     {


       int n ;                 /* degree */

       int m ;                 /* order */

       double g ;              /* g coefficient */

       double h ;              /* h coefficient */


     } ;


   typedef
       struct WT_T_IGRF_Term_struct
           WT_T_IGRF_Term ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     IGRF model.
 */


   struct WT_T_IGRF_Model_struct


     {


/*
 *     Maximum spherical harmonic degree of the IGRF expansion for this
 *     model.
 *
 *
 *
 *     Note that the model has space allocated for the highest maximum
 *     degree for all the models, for all the years available.
 *
 *     However, if that highest maximum is larger than "max_n", then
 *     this model will have values that are zero filler for degrees
 *     higher than "max_n".
 */

       int max_n ;


       WT_T_IGRF_Term term[WT_IGRF_NTERMS] ;  /* IGRF expansion terms */


     } ;


   typedef
       struct WT_T_IGRF_Model_struct
           WT_T_IGRF_Model ;



/*
 *----------------------------------------------------------------------
 *
 *     Spacecraft rotation.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Parameters for the spin pulse.
 *
 *     The date and time of each entry indicate the epoch when that set
 *     of parameters becomes effective.
 */


   struct WT_T_Spin_Pulse_Parm_struct


     {


       int year ;              /* year */

       int month ;             /* month */

       int day ;               /* day of month */

       int hr ;                /* time of day, hours */

       int min ;               /* time of day, minutes */

       double sec ;            /* time of day, seconds */

       long int jdn ;          /* date JDN */

       double time ;           /* time, seconds */

       double time_diff_0 ;    /* time difference from first epoch */

       double min_period ;     /* min. duration of spin period */

       double max_period ;     /* max. duration of spin period */


     } ;


   typedef
       struct WT_T_Spin_Pulse_Parm_struct
           WT_T_Spin_Pulse_Parm ;



/*
 *----------------------------------------------------------------------
 *
 *     Time series.
 *
 *----------------------------------------------------------------------
 */



/*
 *     A time series is a collection of data points, with each data
 *     point having an associated time tag.
 *
 *
 *
 *     It is assumed that there is an array that gives the time tags of
 *     the data points.
 *
 *     The same array, or one or more other arrays, give the data values
 *     for the data points, and any other pertinent information for each
 *     data point.
 *
 *     If there is more than one array, all the arrays (giving the time
 *     tags, the data values, and any other information for each data
 *     point) have the same number of elements in use (i.e., the actual
 *     number of data points); and all the arrays correspond exactly
 *     (one to one) with one another.
 *
 *     All these arrays that refer to the data points are designated as
 *     "data arrays".
 *
 *
 *
 *     All data point indices in the type definitions that follow, refer
 *     to the data array or arrays for the time series data.
 *
 *     Data array indices start at 0, and go up to (N - 1) (where N is
 *     the actual number of data points).
 *
 *
 *
 *     All the time tags must refer to the same epoch (normally, time
 *     tags are in units of seconds, and represent seconds after 0 hours
 *     of a reference date).
 *
 *
 *
 *     A data point has a "retrograde" or "backwards" time tag if:
 *
 *
 *      1) The data point is not the first data point in the data array
 *         (i.e., its index is >= 1); and
 *
 *
 *      2) The time tag of the data point is less than the time tag of
 *         some data point with a smaller index.
 *
 *
 *
 *     A data point has a "duplicate" time tag if:
 *
 *
 *      1) The data point is not the first data point in the data array
 *         (i.e., its index is >= 1); and
 *
 *
 *      2) The time tag of the data point is not retrograde; and
 *
 *
 *      3) The time tag of the data point is equal to the time tag of
 *         some data point with a smaller index.
 *
 *
 *
 *     From the above, it follows that if a data point does not have a
 *     retrograde or a duplicate time tag, then it has a "legitimate"
 *     time tag:
 *
 *
 *      1) The data point is the first data point in the data array
 *         (i.e., its index is == 0); or
 *
 *
 *      2) a) The data point is not the first data point in the data
 *            array (i.e., its index is >= 1); and
 *
 *         b) The time tag of the data point is greater than the time
 *            tag of every data data point with a smaller index.
 */



/*
 *     Sampling rate information.
 *
 *
 *
 *     The data points of a time series are considered to be divided
 *     into separate sampling rate "regimes".
 *
 *
 *
 *     Each regime consists of a sequence of data points with
 *     consecutive data array indices, that correspond to a constant,
 *     positive "time-delta" (time separation between consecutive
 *     points); or, actually, nearly constant time-delta, as some
 *     tolerance for variation is allowed.
 *
 *
 *
 *     Note that there can be data gaps within a regime; there is a
 *     specified maximum for the size of a data gap allowed within a
 *     regime.
 *
 *     Therefore, two data points in the same regime, with consecutive
 *     data array indices, may be separated by a time interval of twice
 *     the time-delta, if there is a 1-point data gap; or 3 times the
 *     time-delta, if there is a 2-point data gap; etc.
 *
 *
 *
 *     There is also a special type of regime, which consists of data
 *     points with consecutive data array indices, for which the
 *     time-delta varies in an inconsistent fashion.
 *
 *
 *
 *     The notion of data gaps within a regime with inconsistent
 *     time-delta is meaningless.
 *
 *
 *
 *     Only data points with legitimate time tags are included in the
 *     regimes; data points with retrograde or duplicate time tags are
 *     not included in any regime.
 *
 *
 *
 *     A regime comes to an end when any of the following occurs:
 *
 *
 *      1) There is a data gap or data dropout, of a size larger than
 *         the specified maximum data gap allowed within a regime.
 *
 *         This starts a new regime, for the data points that come after
 *         the data gap.
 *
 *
 *      2) The regime corresponds to some time-delta, and the data
 *         points that follow have a different time-delta.
 *
 *         This starts a new regime, for the new time-delta.
 *
 *
 *      3) The regime corresponds to some time-delta, and the data
 *         points that follow have inconsistent values of time-delta.
 *
 *         This starts a new regime, for inconsistent time-delta.
 *
 *
 *      4) The regime corresponds to inconsistent time-delta, and the
 *         data points that follow have consistent values of time-delta.
 *
 *         This starts a new regime, for the new time-delta.
 *
 *
 *      5) There are no more data points.
 *
 *
 *      6) A data point with a retrograde or duplicate time tag is
 *         encountered.
 *
 *         Because data points with retrograde or duplicate time tags
 *         are not included in any regime; a new regime is started only
 *         after these points that have been encountered have been
 *         passed over.
 *
 *         A data point might be excluded from all the regimes for some
 *         other reason, other than a retrogade or duplicate time tag.
 *         This, too, would cause a regime to end.
 *
 *         This starts a new regime, for the data points following the
 *         group of one or more data points with consecutive data array
 *         indices, that were encountered; that are excludable, because
 *         they have retrograde or duplicate time tags; or that are
 *         excludable for any other reason.
 *
 *
 *
 *     Because only data points with legitimate time tags are included
 *     in the regimes; and because data points might be excluded from
 *     the regimes for other reasons as well, other than retrograde or
 *     duplicate time tags; it follows that the index of the first data
 *     point of a regime is not necessarily equal to the integer
 *     following the index of the last data point of the previous
 *     regime; there may be a gap between the two indices, corresponding
 *     to data points excluded from the regimes, due to retrograde or
 *     duplicate time tags, or due to other reasons.
 *
 *
 *
 *     Since there can be data gaps within a regime that has a
 *     consistent time-delta, the gaps divide the regime into separate
 *     "skeins".
 *
 *     A skein, then, is a sequence of data points with consecutive data
 *     array indices, all belonging to the same regime, and with no data
 *     gaps within the sequence.
 *
 *     The time separation between any two data points within the
 *     skein, that have consecutive data array indices, will be equal to
 *     the time-delta for the regime (allowing for the tolerance for the
 *     time-delta).
 *
 *
 *
 *     For a regime with a consistent time-delta, the index of the first
 *     data point of a skein will always be equal to the integer
 *     following the index of the last data point of the previous skein
 *     of the same regime.
 *
 *     There will be a gap between the time tags of these two data
 *     points; but there is no gap between their array indices.
 *
 *
 *
 *     If the regime has inconsistent time-delta, then the concept of
 *     skein has no meaning.
 *
 *
 *
 *     The sampling rate information for a time series consists of
 *     regimes; the regimes with consistent time-delta consist of
 *     skeins.
 *
 *
 *
 *     The data points within a skein are considered in the normal data
 *     array order, which means that they are in chronological sequence.
 *
 *     For a regime with inconsistent time-delta, where there are no
 *     skeins, the data points also are considered in the normal data
 *     array order, which means that they are in chronological sequence.
 *
 *     The skeins within a regime, and the regimes within the sampling
 *     rate information for a time series; all follow the normal data
 *     array order, which means that they are in chronological sequence.
 */



/*
 *----------------------------------------------------------------------
 */



/*
 *     Skein in a regime for a time series.
 */


   struct WT_T_Time_Series_Skein_struct


     {


/*
 *     Number of data points in the skein.
 */

       int n_pts ;


/*
 *     Data array indices of the first and last data points in the
 *     skein.
 *
 *
 *     The number of data points must have the proper relation to the
 *     first and last data array indices; i.e.,
 *
 *         n_pts == last_data_idx - first_data_idx + 1
 */

       int first_data_idx ;

       int last_data_idx ;


/*
 *     Number of data points for the gap that follows the skein.
 *
 *
 *     If this is not the last skein for this regime, then this item
 *     is the number of data points that could be inserted between this
 *     skein and the next skein, such that:
 *
 *      1) The last data point of this skein, the inserted points, and
 *         the first data point of the next skein; would form a set of
 *         points with the same time interval between any two
 *         consecutive points.
 *
 *      2) The time interval in 1) above is equal to the time-delta for
 *         the regime (allowing for the tolerance for the time-delta).
 *
 *
 *     The last data point of this skein, and the first data point of
 *     the next skein, are not included in the count of the number of
 *     data points for the gap.
 *
 *
 *     If this is the last skein for this regime, then this item is
 *     meaningless.
 */

       int n_pts_gap ;


     } ;


   typedef
       struct WT_T_Time_Series_Skein_struct
           WT_T_Time_Series_Skein ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Regime for a time series.
 */


   struct WT_T_Time_Series_Regime_struct


     {


/*
 *     Number of data points in the regime.
 */

       int n_pts ;


/*
 *     Data array indices of the first and last data points in the
 *     regime.
 *
 *
 *     The number of data points must have the proper relation to the
 *     first and last data array indices; i.e.,
 *
 *         n_pts == last_data_idx - first_data_idx + 1
 */

       int first_data_idx ;

       int last_data_idx ;


/*
 *     Flag for consistent sampling rate for the regime.
 *
 *
 *     The value of this item should be as follows:
 *
 *         WT_TRUE,   if there is a consistent sampling rate (i.e., a
 *                    consistent time-delta).
 *
 *         WT_FALSE,  if there is no consistent sampling rate (i.e., an
 *                    inconsistent time-delta).
 *
 *
 *     If the sampling rate for the regime is not consistent, then the
 *     items following this one are meaningless.
 */

       int consistent_sampling_rate ;


/*
 *     Ideal time-delta for the regime.
 *
 *
 *     This is the "best" value of the time-delta, that can be
 *     determined for the regime.
 *
 *
 *     Units are seconds.
 *
 *
 *     For example, for 100 Hz frequency data, this item would have the
 *     value 0.01 (corresponding to a time-delta of 0.01 seconds).
 *
 *
 *     The ideal sampling frequency for this regime, in Hz, is:
 *
 *         1.0 / t_delta
 */

       double t_delta ;


/*
 *     Minimum and maximum time-delta for the regime.
 *
 *
 *     These are the limits for the time-delta, as allowed by the
 *     tolerance for variation.
 *
 *
 *     Units are seconds.
 *
 *
 *     The minimum and the maximum time-delta must be positive numbers,
 *     and the interval defined by them must contain the ideal
 *     time-delta; i.e.,
 *
 *         0.0 < min_t_delta <= t_delta <= max_t_delta
 *
 *
 *     The minimum and maximum sampling frequencies for this regime, in
 *     Hz, as allowed by the tolerance for variation, are respectively:
 *
 *         1.0 / max_t_delta
 *
 *         1.0 / min_t_delta
 */

       double min_t_delta ;

       double max_t_delta ;


/*
 *     Ideal frequency for the regime.
 *
 *
 *     This is the "best" value of the frequency, that can be
 *     determined for the regime.
 *
 *
 *     Units are Hz.
 */

       double freq ;


/*
 *     Number of skeins for the regime.
 */

       int n_skeins ;


/*
 *     Array with the skeins for this regime.
 *
 *     There will be "n_skeins" of these.
 */

       WT_T_Time_Series_Skein *skein ;


/*
 *     Number of data points that would be in the regime, if there had
 *     been no gaps within the regime.
 */

       int n_ideal_pts ;


/*
 *     Number of data points skippped within the regime.
 *
 *     This is the total number of points corresponding to the gaps
 *     within the regime.
 *
 *
 *     The numbers of data points must have the following relation:
 *
 *         n_ideal_pts == n_pts + n_skipped_pts
 */

       int n_skipped_pts ;


/*
 *     Index of the next regime with the same time-delta.
 *
 *
 *     If the time series sampling information contains "n" regimes,
 *     then the regime indices go from 0 up to (n - 1).
 *
 *
 *     The value of this item should be as follows:
 *
 *         m,   if the next regime with the same time-delta has a regime
 *              index equal to m.
 *
 *         -1,  if there is no later regime with the same time-delta;
 *              this includes the case when this regime is the last
 *              one, i.e., this regime has index equal to (n - 1).
 */

       int next_regime_idx ;


     } ;


   typedef
       struct WT_T_Time_Series_Regime_struct
           WT_T_Time_Series_Regime ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Sampling rate information for a time series.
 */


   struct WT_T_Time_Series_Sampling_Rate_Info_struct


     {


/*
 *     Number of regimes.
 */

       int n_regimes ;


/*
 *     Array with the regimes for this time series.
 *
 *     There will be "n_regimes" of these.
 */

       WT_T_Time_Series_Regime *regime ;


     } ;


   typedef
       struct WT_T_Time_Series_Sampling_Rate_Info_struct
           WT_T_Time_Series_Sampling_Rate_Info ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Legitimate data points for a time series.
 */


   struct WT_T_Time_Series_Legit_Data_Points_struct


     {


/*
 *     Number of data points.
 */

       int n_pts ;


/*
 *     Array with the time tags for this time series.
 *
 *     There will be "n_pts" of these.
 *
 *     The "value" for each time tag is the index to the data arrays.
 */

       WT_T_Time_Tagged_Int *legit_time_tag ;


     } ;


   typedef
       struct WT_T_Time_Series_Legit_Data_Points_struct
           WT_T_Time_Series_Legit_Data_Points ;



/*
 *----------------------------------------------------------------------
 *
 *     Time series - Fourier transform, convolution, etc.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Control info. for a table of convolution coefficients.
 */


   struct WT_T_Conv_Control_Coeffs_Table_struct


     {


/*
 *     The concept of time-delta, and other concepts related to data
 *     sampling rate; are defined in the type definitions for time
 *     series data.
 *
 *
 *     Ideal time-delta for the table.
 *
 *
 *     This is the "best" value of the time-delta, that corresponds to
 *     this table of coefficients.
 *
 *
 *     Units are seconds.
 *
 *
 *     For example, for 100 Hz frequency data, this item would have the
 *     value 0.01 (corresponding to a time-delta of 0.01 seconds).
 *
 *
 *     The value of this item should be  >  0.0.
 *
 *
 *     The ideal frequency for this table, in Hz, is:
 *
 *         1.0 / t_delta
 */

       double t_delta ;


/*
 *     Ideal frequency for the table.
 *
 *
 *     This is the "best" value of the frequency, that corresponds to
 *     this table of coefficients.
 *
 *
 *     Units are Hz.
 */

       double freq ;


/*
 *     The file that contains the table of convolution coefficients is
 *     an ASCII file, consisting of lines of text.
 *
 *     Each line must be terminated by a "newline" character.
 *
 *     Optionally, there can be a terminating "carriage return"
 *     character immediately before the "newline" character.
 *
 *     "White space" consists of one or more blanks (spaces); or one or
 *     more "tabs" (horizontal tabs); or any mixture of the two.
 *
 *     Other than the "newline" character that terminates each line, the
 *     optional terminating "carriage return" character, and the "white
 *     space" characters; there must not be any other non-printing
 *     characters in the file.
 *
 *
 *     The user can designate one character as a "comment character".
 *
 *
 *     The following lines are considered to be comment lines (and
 *     skipped):
 *
 *      1) Empty lines (no characters before the optional terminating
 *         "carriage return" and the terminating "newline").
 *
 *      2) Lines that contain only white space.
 *
 *      3) Lines whose first non-white-space character is the character
 *         designated by the user as the "comment character".
 *
 *
 *     The user can avoid having a special "comment character", by
 *     setting the "comment character" to any one of the following: the
 *     null character, or the "newline" character, or a white space
 *     character.
 *
 *
 *     A line that is not a comment line is a data line.
 *
 *     A data line must contain one or more data fields, which are
 *     separated from one another by white space.
 *
 *     There can be optional white space before the first (or only) data
 *     field of a line; and after the last (or only) data field of a
 *     line.
 *
 *
 *     Each data field consists of a real number, which is a convolution
 *     coefficient.
 *
 *
 *     A table must contain at least 1 convolution coefficient.
 */


/*
 *     Pathname to the file that contains the table of coefficients.
 */

       char *conv_table_coeffs_path ;


/*
 *     Upper limit for the number of characters that can be in a line
 *     (not counting the optional terminating "carriage return"
 *     character and the terminating "newline" character); in the file
 *     that contains the table of convolution coefficients.
 */

       int max_chars_per_line ;


/*
 *     Special "comment character" for the input lines; in the file that
 *     contains the table of convolution coefficients.
 */

       char comment_character ;


/*
 *     Flag for the order of the convolution coefficients being reversed
 *     in the file that contains the table of convolution coefficients.
 *
 *     The definition of convolution is a sum of products of
 *     coefficients and data values; with the array index for the
 *     coefficients and the array index for the data values progressing
 *     in opposite directions.
 *
 *     The normal order of the coefficients in the file is in increasing
 *     value of their own array index.
 *
 *
 *     If the coefficients in the file are in wrapped order, this item
 *     refers to the unwrapped set.
 *
 *     In other words, the procedure for wrapping a set of numbers is
 *     the same, whether these numbers are the set of coefficients in
 *     normal order, or whether these numbers are the set of
 *     coefficients in reversed order.
 *
 *
 *     If the number of coefficients is M, and the coefficients are
 *     designated as r{i}, where the {} denote subscript, and where
 *     0 <= i <= (M - 1); and r{i} are the coefficients in normal order;
 *
 *     we define a new set of numbers c{i}, with i in the same range;
 *
 *     if the coefficients in the file are in normal order, then
 *
 *         c{i} = r{i} for all values of i
 *
 *     if the coefficients in the file are in reversed order, then
 *
 *         c{i} = r{M - 1 - i} for all values of i
 *
 *     It is this set of numbers c{i} that are placed in the file,
 *     either wrapped or unwrapped; if wrapped, then the wrapping
 *     procedure starts with these numbers c{i}.
 *
 *
 *
 *     The value of this item should be as follows:
 *
 *         WT_TRUE,   if the coefficients are in reversed order in the
 *                    file.
 *
 *         WT_FALSE,  if the coefficients are in normal order in the
 *                    file.
 */

       int coeffs_reversed ;


/*
 *     Code for the number of convolution coefficients.
 *
 *     The number of convolution coefficients refers to the "original",
 *     unwrapped table (wrapping can alter the number of coefficients).
 *
 *
 *     The value of this item should be as follows:
 *
 *         WT_CNV_N_ODD     =  The number of coefficients is odd.
 *
 *         WT_CNV_N_EVEN_1  =  The number of coefficients is even.
 *                             The time tag of the output point will
 *                             be the same as that of the 1st.
 *                             central point.
 *
 *         WT_CNV_N_EVEN_2  =  The number of coefficients is even.
 *                             The time tag of the output point will
 *                             be the same as that of the 2nd.
 *                             central point.
 */

       int n_coeffs_code ;


/*
 *     Flag for the wrapping of the convolution coefficients in the file
 *     that contains the table of convolution coefficients.
 *
 *
 *
 *     The ideas for wrapping arrays are adapted from
 *
 *         "Numerical Recipes in C"
 *
 *         William H. Press
 *         Saul A. Teukolsky
 *         William T. Vetterling
 *         Brian P. Flannery
 *
 *         Cambridge University Press
 *
 *         Second Edition, 1992 (Reprinted 1996)
 *
 *         Chapter 13
 *
 *
 *
 *     If we start with an odd number M of unwrapped coefficients, these
 *     can be "wrapped" into a set of N coefficients (N >= M) as
 *     follows:
 *
 *      1) Let M = 2 * K + 1.
 *
 *      2) The original (unwrapped, possibly reversed) coefficients are:
 *
 *             c[0], c[1], ..., c[M - 1]  or equivalently
 *
 *             c[0], c[1], ..., c[2 * K]
 *
 *      3) We define the "shifted" coefficients by subtracting K from
 *         each index:
 *
 *             d[-K]       = c[0]
 *
 *             d[-(K - 1)] = c[1]
 *
 *             d[-(K - 2)] = c[2]
 *
 *             ...
 *
 *             d[-1]       = c[K - 1]
 *
 *             d[0]        = c[K]
 *
 *             d[1]        = c[K + 1]
 *
 *             ...
 *
 *             d[K - 2]    = c[2 * K - 2]
 *
 *             d[K - 1]    = c[2 * K - 1]
 *
 *             d[K]        = c[2 * K]
 *
 *      4) The "shifted" coefficients are placed in the final (wrapped)
 *         array by having the coefficients with non-negative indices at
 *         the beginning of the array, and the coefficients with
 *         negative indices at the end of the array.
 *
 *             w[0]           = d[0]
 *
 *             w[1]           = d[1]
 *
 *             w[2]           = d[2]
 *
 *             ...
 *
 *             w[K - 2]       = d[K - 2]
 *
 *             w[K - 1]       = d[K - 1]
 *
 *             w[K]           = d[K]
 *
 *             ...
 *             ...
 *
 *             w[N - K]       = d[-K]
 *
 *             w[N - (K - 1)] = d[-(K - 1)]
 *
 *             w[N - (K - 2)] = d[-(K - 2)]
 *
 *             ...
 *
 *             w[N - 3]       = d[-3]
 *
 *             w[N - 2]       = d[-2]
 *
 *             w[N - 1]       = d[-1]
 *
 *      5) If N == M, then the "wrapped" coefficients w[i] shown above
 *         fill exactly the final array, and there is no gap where the 2
 *         consecutive lines of "..." are shown.
 *
 *         If N > M, then the "wrapped" coefficients w[i] shown above
 *         will leave a gap where the 2 consecutive lines of "..." are
 *         shown; this gap must be filled with zeros.
 *
 *             w[i] = 0.0    for    (K + 1) <= i <= (N - K - 1)
 *
 *      6) In the minimum case,
 *
 *             K == 0
 *
 *         i.e.
 *
 *             N >= M == 1
 *
 *         the part of the array w{i} with negative indices is empty.
 *
 *
 *
 *     There are 2 ways to wrap an even number of coefficients.
 *
 *
 *
 *     Method 1: To be used when
 *
 *         even_time_tag_code == WT_CNV_TIME_EVEN_1
 *
 *     If we start with an even number M of unwrapped coefficients,
 *     these can be "wrapped" into a set of N coefficients (N >= M) as
 *     follows:
 *
 *      1) Let M = 2 * K.
 *
 *      2) The original (unwrapped, possibly reversed) coefficients are:
 *
 *             c[0], c[1], ..., c[M - 1]  or equivalently
 *
 *             c[0], c[1], ..., c[2 * K - 1]
 *
 *      3) We define the "shifted" coefficients by subtracting K from
 *         each index:
 *
 *             d[-K]       = c[0]
 *
 *             d[-(K - 1)] = c[1]
 *
 *             d[-(K - 2)] = c[2]
 *
 *             ...
 *
 *             d[-1]       = c[K - 1]
 *
 *             d[0]        = c[K]
 *
 *             d[1]        = c[K + 1]
 *
 *             ...
 *
 *             d[K - 3]    = c[2 * K - 3]
 *
 *             d[K - 2]    = c[2 * K - 2]
 *
 *             d[K - 1]    = c[2 * K - 1]
 *
 *      4) The "shifted" coefficients are placed in the final (wrapped)
 *         array by having the coefficients with non-negative indices at
 *         the beginning of the array, and the coefficients with
 *         negative indices at the end of the array.
 *
 *             w[0]           = d[0]
 *
 *             w[1]           = d[1]
 *
 *             w[2]           = d[2]
 *
 *             ...
 *
 *             w[K - 3]       = d[K - 3]
 *
 *             w[K - 2]       = d[K - 2]
 *
 *             w[K - 1]       = d[K - 1]
 *
 *             ...
 *             ...
 *
 *             w[N - K]       = d[-K]
 *
 *             w[N - (K - 1)] = d[-(K - 1)]
 *
 *             w[N - (K - 2)] = d[-(K - 2)]
 *
 *             ...
 *
 *             w[N - 3]       = d[-3]
 *
 *             w[N - 2]       = d[-2]
 *
 *             w[N - 1]       = d[-1]
 *
 *      5) If N == M, then the "wrapped" coefficients w[i] shown above
 *         fill exactly the final array, and there is no gap where the 2
 *         consecutive lines of "..." are shown.
 *
 *         If N > M, then the "wrapped" coefficients w[i] shown above
 *         will leave a gap where the 2 consecutive lines of "..." are
 *         shown; this gap must be filled with zeros.
 *
 *             w[i] = 0.0    for    K <= i <= (N - K - 1)
 *
 *      6) In the minimum case,
 *
 *             K == 1
 *
 *         i.e.
 *
 *             N >= M == 2
 *
 *
 *
 *     Method 2: To be used when
 *
 *         even_time_tag_code == WT_CNV_TIME_EVEN_2
 *
 *     If we start with an even number M of unwrapped coefficients,
 *     these can be "wrapped" into a set of N coefficients (N >= M) as
 *     follows:
 *
 *      1) Let M = 2 * K.
 *
 *      2) The original (unwrapped, possibly reversed) coefficients are:
 *
 *             c[0], c[1], ..., c[M - 1]  or equivalently
 *
 *             c[0], c[1], ..., c[2 * K - 1]
 *
 *      3) We define the "shifted" coefficients by subtracting (K - 1)
 *         from each index:
 *
 *             d[-(K - 1)] = c[0]
 *
 *             d[-(K - 2)] = c[1]
 *
 *             d[-(K - 3)] = c[2]
 *
 *             ...
 *
 *             d[-1]       = c[K - 2]
 *
 *             d[0]        = c[K - 1]
 *
 *             d[1]        = c[K]
 *
 *             d[2]        = c[K + 1]
 *
 *             ...
 *
 *             d[K - 2]    = c[2 * K - 3]
 *
 *             d[K - 1]    = c[2 * K - 2]
 *
 *             d[K]        = c[2 * K - 1]
 *
 *      4) The "shifted" coefficients are placed in the final (wrapped)
 *         array by having the coefficients with non-negative indices at
 *         the beginning of the array, and the coefficients with
 *         negative indices at the end of the array.
 *
 *             w[0]           = d[0]
 *
 *             w[1]           = d[1]
 *
 *             w[2]           = d[2]
 *
 *             ...
 *
 *             w[K - 2]       = d[K - 2]
 *
 *             w[K - 1]       = d[K - 1]
 *
 *             w[K]           = d[K]
 *
 *             ...
 *             ...
 *
 *             w[N - (K - 1)] = d[-(K - 1)]
 *
 *             w[N - (K - 2)] = d[-(K - 2)]
 *
 *             w[N - (K - 3)] = d[-(K - 3)]
 *
 *             ...
 *
 *             w[N - 3]       = d[-3]
 *
 *             w[N - 2]       = d[-2]
 *
 *             w[N - 1]       = d[-1]
 *
 *      5) If N == M, then the "wrapped" coefficients w[i] shown above
 *         fill exactly the final array, and there is no gap where the 2
 *         consecutive lines of "..." are shown.
 *
 *         If N > M, then the "wrapped" coefficients w[i] shown above
 *         will leave a gap where the 2 consecutive lines of "..." are
 *         shown; this gap must be filled with zeros.
 *
 *             w[i] = 0.0    for    (K + 1) <= i <= (N - K)
 *
 *      6) In the minimum case,
 *
 *             K == 1
 *
 *         i.e.
 *
 *             N >= M == 2
 *
 *         the part of the array w{i} with negative indices is empty.
 *
 *
 *
 *     Note that if N > M, it is not required that N have the same
 *     parity as M.
 *
 *     Therefore, the number of zeros filled into the "middle" of the
 *     final wrapped array, (N - M), could be even or could be odd.
 *
 *     Starting from an odd number of unwrapped coefficients, it is
 *     possible to end up with either and even or an odd number of final
 *     wrapped coefficients.
 *
 *     Starting from an even number of unwrapped coefficients, it is
 *     possible to end up with either and even or an odd number of final
 *     wrapped coefficients.
 *
 *
 *
 *     The value of this item should be as follows:
 *
 *         WT_TRUE   =  The coefficients are wrapped.
 *
 *         WT_FALSE  =  The coefficients are unwrapped.
 */

       int wrap_flag ;


/*
 *     Code for the divisor for the convolution coefficients.
 *
 *
 *     All convolution coefficients for this table will be divided by
 *     the same divisor, which must be not equal to zero.
 *
 *
 *     The value of this item should be as follows:
 *
 *         WT_CNV_DIV_USER      =  The divisor is set by the user.
 *
 *         WT_CNV_DIV_SUM_1     =  The divisor should be the sum of the
 *                                 coefficients (so that the sum of the
 *                                 effective coefficients is equal to
 *                                 1).
 *
 *         WT_CNV_DIV_SUM_SQ_1  =  The divisor should be the square root
 *                                 of the sum of the squares of the
 *                                 coefficients (so that the sum of the
 *                                 squares of the effective coefficients
 *                                 is equal to 1).
 */

       int divisor_code ;


/*
 *     User divisor for the convolution coefficients.
 *
 *
 *     If the divisor is set by the user, then the value of this item
 *     should be  !=  0.0.
 *
 *
 *     If the divisor is not set by the user, then this item is
 *     meaningless.
 */

       double user_divisor ;


/*
 *     Flag for the maximum number of missing data points in a gap
 *     within a data rate sampling regime; over which to do
 *     interpolation.
 *
 *     This refers to the gap between two consecutive skeins in the same
 *     regime.
 *
 *
 *     The value of this item should be as follows:
 *
 *         WT_TRUE,   if there is a maximum number of missing data
 *                    points for interpolation.
 *
 *         WT_FALSE,  if there is no maximum number of missing data
 *                    points for interpolation.
 */

       int max_pts_interp_within_regime_flag ;


/*
 *     Maximum number of missing data points in a gap within a data rate
 *     sampling regime; over which to do interpolation.
 *
 *     This refers to the gap between two consecutive skeins in the same
 *     regime.
 *
 *
 *     If the flag for this item is true, then the value of this item
 *     should be  >=  0.
 *
 *     If the number of missing data points exceeds this limit, no
 *     interpolation is performed, and the missing points are filled
 *     with zeros.
 *
 *
 *     If the flag for this item is false, then this item is
 *     meaningless.
 */

       int max_pts_interp_within_regime ;


/*
 *     Flag for the largest duration of a gap within a data rate
 *     sampling regime; over which to do interpolation.
 *
 *     This refers to the gap between two consecutive skeins in the same
 *     regime.
 *
 *
 *     The value of this item should be as follows:
 *
 *         WT_TRUE,   if there is a maximum duration of the gap for
 *                    interpolation.
 *
 *         WT_FALSE,  if there is no maximum duration of the gap for
 *                    interpolation.
 */

       int max_time_interp_within_regime_flag ;


/*
 *     Largest duration of a gap within a data rate sampling regime;
 *     over which to do interpolation.
 *
 *     This refers to the gap between two consecutive skeins in the same
 *     regime.
 *
 *
 *     The duration of a gap is the time between the last point before
 *     the gap and the first point after the gap.
 *
 *
 *     Units are seconds.
 *
 *
 *     If the flag for this item is true, then the value of this item
 *     should be  >=  0.0.
 *
 *     If the duration of the gap exceeds this limit, no interpolation
 *     is performed, and the missing points are filled with zeros.
 *
 *
 *     If the flag for this item is false, then this item is
 *     meaningless.
 */

       double max_time_interp_within_regime ;


/*
 *     Flag for the maximum number of missing data points in a gap
 *     outside a data rate sampling regime; over which to do
 *     interpolation.
 *
 *     This refers to the gap between a regime and the next regime with
 *     the same time-delta.
 *
 *
 *     The value of this item should be as follows:
 *
 *         WT_TRUE,   if there is a maximum number of missing data
 *                    points for interpolation.
 *
 *         WT_FALSE,  if there is no maximum number of missing data
 *                    points for interpolation.
 */

       int max_pts_interp_outside_regime_flag ;


/*
 *     Maximum number of missing data points in a gap outside a data
 *     rate sampling regime; over which to do interpolation.
 *
 *     This refers to the gap between a regime and the next regime with
 *     the same time-delta.
 *
 *
 *     If the flag for this item is true, then the value of this item
 *     should be  >=  0.
 *
 *     If the number of missing data points exceeds this limit, no
 *     interpolation is performed, and the missing points are filled
 *     with zeros.
 *
 *
 *     If the flag for this item is false, then this item is
 *     meaningless.
 */

       int max_pts_interp_outside_regime ;


/*
 *     Flag for the largest duration of a gap outside a data rate
 *     sampling regime; over which to do interpolation.
 *
 *     This refers to the gap between a regime and the next regime with
 *     the same time-delta.
 *
 *
 *     The value of this item should be as follows:
 *
 *         WT_TRUE,   if there is a maximum duration of the gap for
 *                    interpolation.
 *
 *         WT_FALSE,  if there is no maximum duration of the gap for
 *                    interpolation.
 */

       int max_time_interp_outside_regime_flag ;


/*
 *     Largest duration of a gap outside a data rate sampling regime;
 *     over which to do interpolation.
 *
 *     This refers to the gap between a regime and the next regime with
 *     the same time-delta.
 *
 *
 *     The duration of a gap is the time between the last point before
 *     the gap and the first point after the gap.
 *
 *
 *     Units are seconds.
 *
 *
 *     If the flag for this item is true, then the value of this item
 *     should be  >=  0.0.
 *
 *     If the duration of the gap exceeds this limit, no interpolation
 *     is performed, and the missing points are filled with zeros.
 *
 *
 *     If the flag for this item is false, then this item is
 *     meaningless.
 */

       double max_time_interp_outside_regime ;


     } ;


   typedef
       struct WT_T_Conv_Control_Coeffs_Table_struct
           WT_T_Conv_Control_Coeffs_Table ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Control info. for a set of tables of convolution coefficients.
 */


   struct WT_T_Conv_Control_Coeffs_Table_Set_struct


     {


/*
 *     Number of tables for the set.
 */

       int n_tables ;


/*
 *     Array with the tables for this set.
 *
 *     There will be "n_tables" of these.
 */

       WT_T_Conv_Control_Coeffs_Table *table ;


/*
 *     Flag for writing out unfiltered data points for a regime with an
 *     inconsistent time-delta.
 *
 *
 *     The value of this item should be as follows:
 *
 *         WT_TRUE,   if such points are to be written.
 *
 *         WT_FALSE,  if such points are not to be written.
 */

       int write_inconsistent ;


/*
 *     Flag for writing out unfiltered data points for a regime with a
 *     consistent time-delta, for which there is no table in the set.
 *
 *
 *     The value of this item should be as follows:
 *
 *         WT_TRUE,   if such points are to be written.
 *
 *         WT_FALSE,  if such points are not to be written.
 */

       int write_if_no_table ;


     } ;


   typedef
       struct WT_T_Conv_Control_Coeffs_Table_Set_struct
           WT_T_Conv_Control_Coeffs_Table_Set ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Control info. for a convolution.
 */



/*
 *     The "convolution" is considered as consisting of multiple
 *     "convolution operations".
 *
 *
 *
 *     Each "convolution operation" uses one set of tables.
 *
 *
 *
 *     The sets of tables are used in succession (in the order of the
 *     sets).
 *
 *
 *
 *     The original input data is the input for the first convolution
 *     operation.
 *
 *     After that, the result of each convolution operation is the input
 *     for the next one.
 */



   struct WT_T_Conv_Control_Info_struct


     {


/*
 *     Number of sets of tables.
 */

       int n_table_sets ;


/*
 *     Array with the sets of tables for this convolution.
 *
 *     There will be "n_table_sets" of these.
 */

       WT_T_Conv_Control_Coeffs_Table_Set *table_set ;


     } ;


   typedef
       struct WT_T_Conv_Control_Info_struct
           WT_T_Conv_Control_Info ;



/*
 *----------------------------------------------------------------------
 *
 *     Vectors and matrices.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Time tagged vector.
 */


   struct WT_T_Time_Tagged_Vector_struct


     {


       double time ;           /* time tag */

       double v[DIM_3] ;       /* vector components */


     } ;


   typedef
       struct WT_T_Time_Tagged_Vector_struct
           WT_T_Time_Tagged_Vector ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Time tagged matrix.
 *
 *     The structure has space for 2 matrices, each of dimension 3; but
 *     different applications may use either one, or both.
 *
 *     In many cases in which both matrices are used, each matrix may be
 *     the transpose of the other one.
 */


   struct WT_T_Time_Tagged_Matrix_struct


     {


       double time ;           /* time tag */

       double a[2][DIM_3][DIM_3] ;  /* elements of the 2 matrices */


     } ;


   typedef
       struct WT_T_Time_Tagged_Matrix_struct
           WT_T_Time_Tagged_Matrix ;





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Arrays.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     General - General-purpose functions.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Number of auxiliary parameters for the general-purpose function
 *     Gen_F_1_Var_001.
 *
 *     The array is indexed by the function code.
 */


   static WT_T_Gen_F_N_Aux_Param WT_A_gen_f_1_001_n_aux_param[] =


     {


       { 0, 0 },

       { 0, 4 },

       { 0, 4 },

       { 0, 4 },

       { 1, 5 },

       { 0, 4 },

       { 0, 4 },


     } ;



/*
 *     Number of auxiliary parameters for the general-purpose function
 *     Gen_F_2_Var_001.
 *
 *     The array is indexed by the function code.
 */


   static WT_T_Gen_F_N_Aux_Param WT_A_gen_f_2_001_n_aux_param[] =


     {


       { 0, 0 },

       { 0, 6 },

       { 0, 6 },

       { 0, 6 },

       { 1, 7 },

       { 0, 6 },

       { 0, 6 },

       { 1, 7 },


     } ;





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Macros.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     General.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Maximum.
 */


#define                                                                \
   WT_M_Max(a, b)                                                      \
       (((a) >= (b))  ?  (a)  :  (b))



/*
 *----------------------------------------------------------------------
 */



/*
 *     Minimum.
 */


#define                                                                \
   WT_M_Min(a, b)                                                      \
       (((a) <= (b))  ?  (a)  :  (b))



/*
 *----------------------------------------------------------------------
 */



/*
 *     Square.
 */


#define                                                                \
   WT_M_Power_2(x)                                                     \
       ((x) * (x))



/*
 *----------------------------------------------------------------------
 */



/*
 *     Cube.
 */


#define                                                                \
   WT_M_Power_3(x)                                                     \
       ((WT_M_Power_2(x)) * (x))



/*
 *----------------------------------------------------------------------
 */



/*
 *     4th. power.
 */


#define                                                                \
   WT_M_Power_4(x)                                                     \
       ((WT_M_Power_3(x)) * (x))



/*
 *----------------------------------------------------------------------
 *
 *     Debugging messages.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Debugging message, any stream.
 *
 *     Writes the source file name and line number.
 *
 *     "stream" must be a stream for the output.
 */


#define                                                                \
                                                                       \
   WT_M_Write_Dbg_Msg_0(stream)                                        \
                                                                       \
       do                                                              \
                                                                       \
         {                                                             \
                                                                       \
           (void) fprintf((stream),                                    \
                      "\n"                                             \
                      "\n"                                             \
                      "    File: <%s>\n"                               \
                      "    Line: %d\n",                                \
                      __FILE__,                                        \
                      __LINE__) ;                                      \
                                                                       \
           (void) fflush(stream) ;                                     \
                                                                       \
         }   while  (WT_FALSE)



/*
 *----------------------------------------------------------------------
 *
 *     Error messages.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Error message without a specific message, any stream.
 *
 *     Writes the source file name and line number.
 *
 *     "stream" must be a stream for the output.
 */


#define                                                                \
                                                                       \
   WT_M_WriteErrMsg0(stream)                                           \
                                                                       \
       do                                                              \
                                                                       \
         {                                                             \
                                                                       \
           (void) fprintf((stream),                                    \
                      "\n"                                             \
                      "\n"                                             \
                      "ERROR\n"                                        \
                      "    File: <%s>\n"                               \
                      "    Line: %d\n",                                \
                      __FILE__,                                        \
                      __LINE__) ;                                      \
                                                                       \
           (void) fflush(stream) ;                                     \
                                                                       \
         }   while  (WT_FALSE)



/*
 *----------------------------------------------------------------------
 */



/*
 *     Error message with a specific message, any stream.
 *
 *     Writes the source file name and line number, and the specific
 *     message.
 *
 *     "stream" must be a stream for the output.
 *
 *     "message" must be a character pointer.
 */


#define                                                                \
                                                                       \
   WT_M_WriteErrMsg(stream, message)                                   \
                                                                       \
       do                                                              \
                                                                       \
         {                                                             \
                                                                       \
           (void) fprintf((stream),                                    \
                      "\n"                                             \
                      "\n"                                             \
                      "ERROR\n"                                        \
                      "    File: <%s>\n"                               \
                      "    Line: %d\n"                                 \
                      "%s\n",                                          \
                      __FILE__,                                        \
                      __LINE__,                                        \
                      (message)) ;                                     \
                                                                       \
           (void) fflush(stream) ;                                     \
                                                                       \
         }   while  (WT_FALSE)



/*
 *----------------------------------------------------------------------
 */



/*
 *     Error message without a specific message, set stream.
 *
 *     Functions related to error messages control whether any error
 *     messages are written at all or not; and if they are written, to
 *     which stream.
 */


#define                                                                \
                                                                       \
   WT_M_PutErrMsg0                                                     \
                                                                       \
       do                                                              \
                                                                       \
         {                                                             \
                                                                       \
           FILE *wt_m_stream ;                                         \
           int wt_m_writeSwitch ;                                      \
                                                                       \
           GetErrMsgIO(&wt_m_stream, &wt_m_writeSwitch) ;              \
                                                                       \
           if  (wt_m_writeSwitch)                                      \
             {                                                         \
               WT_M_WriteErrMsg0(wt_m_stream) ;                        \
             }                                                         \
                                                                       \
         }   while  (WT_FALSE)



/*
 *----------------------------------------------------------------------
 */



/*
 *     Error message with a specific message, set stream.
 *
 *     "message" must be a character pointer.
 *
 *     Functions related to error messages control whether any error
 *     messages are written at all or not; and if they are written, to
 *     which stream.
 */


#define                                                                \
                                                                       \
   WT_M_PutErrMsg(message)                                             \
                                                                       \
       do                                                              \
                                                                       \
         {                                                             \
                                                                       \
           FILE *wt_m_stream ;                                         \
           int wt_m_writeSwitch ;                                      \
                                                                       \
           GetErrMsgIO(&wt_m_stream, &wt_m_writeSwitch) ;              \
                                                                       \
           if  (wt_m_writeSwitch)                                      \
             {                                                         \
               WT_M_WriteErrMsg(wt_m_stream, (message)) ;              \
             }                                                         \
                                                                       \
         }   while  (WT_FALSE)



/*
 *----------------------------------------------------------------------
 *
 *     Exchange.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Exchange.
 *
 *     Exchanges the values of "a" and "b".
 *
 *     "a", "b", and "temp" must all be l-values of the same type; and
 *     this must be a type for which assignment is defined.
 *
 *     The initial value of "temp" will be lost.
 *
 *     It should be noted with special care that this macro changes the
 *     values of its arguments, even though no pointers or
 *     de-referencing are involved.
 */


#define                                                                \
                                                                       \
   WT_M_Swap(a, b, temp)                                               \
                                                                       \
       do                                                              \
                                                                       \
         {                                                             \
                                                                       \
           temp = a ;                                                  \
           a = b ;                                                     \
           b = temp ;                                                  \
                                                                       \
         }   while  (WT_FALSE)





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
 *     General.
 *
 *----------------------------------------------------------------------
 */



   long int
       AbsLongInt(long int n,
           int *sign_ptr) ;


   int
       Check_Sequence_001(const void *a_ptr, const void *b_ptr,
           int code,
           int (*Comp)(const void *v_1_ptr,
                    const void *v_2_ptr),
           int *cond_result_ptr) ;


   int
       Compare_To_Interval_001(const void *x_ptr, int code,
           int reverse, const void *a_ptr, const void *b_ptr,
           int (*Comp)(const void *v_1_ptr,
                    const void *v_2_ptr),
           int *cond_result_ptr) ;


   long int
       DoubleToScaledInteger(double x, int n) ;


   int
       GCFLongInt(long int a, long int b,
           long int *gcf_ptr) ;


   int
       Get_Data_Item_001(const void *item_ptr, int type,
           double *value_ptr) ;


   int
       IntervalIntersection(double a, double b, double c, double d,
           int *inter_ptr, double *lower_ptr, double *upper_ptr) ;


   double
       MaxDouble(double a, double b) ;


   long int
       MaxLongInt(long int a, long int b) ;


   double
       MinDouble(double a, double b) ;


   long int
       MinLongInt(long int a, long int b) ;


   int
       PointWithinInterval(double a, double b, double x) ;


   int
       Polyn1Var(long int n, const double *a, double x,
           double *val_ptr) ;


   int
       Quotient_001(int flag, double lim, double x, double y,
           int *o_flag_ptr, double *f_ptr) ;


   double
       RoundDouble(double x, int n) ;


   int
       SignDouble(double x) ;


   int
       SignLongInt(long int x) ;


   int
       Sub_Interval_001(double x_1, double y_1, double x_2,
           double y_2, int code, double a, double b,
           int *sub_int_code_ptr, double *sub_x_1_ptr,
           double *sub_x_2_ptr) ;


   int
       Sub_Interval_002(double x_1, double y_1, double x_2,
           double y_2, double a,
           int *sub_int_code_ptr, double *sub_x_1_ptr,
           double *sub_x_2_ptr) ;


   int
       Sub_Interval_003(double x_1, const double *y_1,
           double x_2, const double *y_2, int code, double a,
           double b,
           int *sub_int_code_ptr, double *sub_x_1_ptr,
           double *sub_x_2_ptr) ;


   void
       SwapInt(int *a_ptr, int *b_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     General - General-purpose functions.
 *
 *----------------------------------------------------------------------
 */



   int
       Gen_F_1_Var_001(int code, int test_code, const int *I,
           const double *D, double x_1,
           int *o_flag_ptr, double *f_ptr) ;


   int
       Gen_F_1_Var_002(int code, int test_code, const int *I,
           const double *D, double x_1, double e_1,
           int *o_flag_ptr, double *f_ptr, double *e_f_ptr) ;


   int
       Gen_F_2_Var_001(int code, int test_code, const int *I,
           const double *D, double x_1, double x_2,
           int *o_flag_ptr, double *f_ptr) ;


   int
       Gen_F_2_Var_002(int code, int test_code, const int *I,
           const double *D, double x_1, double e_1,
           double x_2, double e_2,
           int *o_flag_ptr, double *f_ptr, double *e_f_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Angles.
 *
 *----------------------------------------------------------------------
 */



   int
       AngleIntervalIntersectionDeg(double a, double b, double c,
           double d,
           int *n_ptr, WT_T_Interval *intervs) ;


   double
       DegIntoCircle(double x) ;


   double
       DegIntoMinAbs(double x) ;


   double
       DegToHr(double x) ;


   double
       DegToRad(double x) ;


   double
       HrToDeg(double x) ;


   double
       HrToRad(double x) ;


   int
       NormalizeAngleIntervalDeg(double a, double b,
           double *lower_ptr, double *upper_ptr) ;


   double
       RadToDeg(double x) ;


   double
       RadToHr(double x) ;



/*
 *----------------------------------------------------------------------
 *
 *     Arrays.
 *
 *----------------------------------------------------------------------
 */



   ptrdiff_t
       ArrayElemIndex(int kr, int kc, int ke, int nc, int ne) ;


   int
       Get_Array_Elem_001(const void *array, int type, int n,
           double *value_ptr) ;


   int
       RotateLongIntArray(long int m, long int n,
           long int *array) ;



/*
 *----------------------------------------------------------------------
 *
 *     Astronomy.
 *
 *----------------------------------------------------------------------
 */



   int
       EquatCoordTrueToMeanEquinox(long int jdn, double time,
           const double *v1,
           double *v2) ;


   int
       GreenwichMeanSiderealTime(long int jdn, double time,
           double *GMST_ptr, double *cosGMST_ptr, double *sinGMST_ptr) ;


   int
       NutationMatrix(long int jdn, double time,
           double nut[DIM_3][DIM_3]) ;


   int
       NutationMatrixForJ2000(double nut[DIM_3][DIM_3]) ;


   int
       PrecessEquatCoord(long int jdn1, double time1, long int jdn2,
           double time2, const double *v1,
           double *v2) ;


   int
       PrecessEquatCoordToJ2000(long int jdn, double time,
           const double *v1,
           double *v2) ;


   int
       PrecessionMatrix(long int jdn1, double time1, long int jdn2,
           double time2,
           double prec[DIM_3][DIM_3]) ;


   int
       PrecessionMatrixToJ2000(long int jdn, double time,
           double prec[DIM_3][DIM_3]) ;


   int
       Sun_001(long int jdn, double time,
           double *slong_ptr, double *obliq_ptr, double *cosObliq_ptr,
           double *sinObliq_ptr, double *rasn_ptr, double *dec_ptr,
           double *r_ptr, double *sunr) ;


   int
       Sun_002(long int jdn, double time,
           double *sunr) ;


   int
       Sun_003(long int jdn, double time,
           double *sunr) ;


   int
       Sun_004(long int jdn, double time, double nut[DIM_3][DIM_3],
           double *sunr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Bits and bytes.
 *
 *----------------------------------------------------------------------
 */



   int
       Get_Bits_001_Char(unsigned char x, int p, int n,
           unsigned char *y_ptr) ;


   int
       Set_Bits_001_Char(unsigned char *x_ptr,
           int p, int n, int bit) ;


   int
       Set_Bits_002_Char(unsigned char *x_ptr,
           int p, int n, unsigned char y) ;



/*
 *----------------------------------------------------------------------
 *
 *     Character strings.
 *
 *----------------------------------------------------------------------
 */



   int
       Copy_Construct_Char_String_001(const char *i_s,
           char **o_s_ptr) ;


   int
       ExtractField(const char *s, int pos, int n,
           char *field, int *posNext_ptr) ;


   int
       Extract_Field_002(const char *s, int k,
           char *field, char **next_ptr) ;


   int
       StringToLogical(const char *s, int skipW, const char *def,
           int *log_ptr) ;


   void
       Trim_String_001(char *s) ;


   char *
       Trim_String_002(const char *s) ;


   void
       Trim_String_003(char *s) ;


   char *
       Trim_String_004(char *s) ;



/*
 *----------------------------------------------------------------------
 *
 *     Comparison.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Type is: Double.
 *
 *     The comparison is on algebraic value.
 */

   int
       Comp_001(const void *v_1_ptr, const void *v_2_ptr) ;


/*
 *     Type is: Structure of 2 double.
 *
 *     Key for comparison, and 1 value.
 *
 *     The comparison is on algebraic value of the key.
 */

   int
       Comp_002(const void *v_1_ptr, const void *v_2_ptr) ;


/*
 *     Type is: Time tagged int.
 *
 *     The comparison is on algebraic value of the time tag.
 */

   int
       Comp_003(const void *v_1_ptr, const void *v_2_ptr) ;


/*
 *     Type is: Char *.
 *
 *     The comparison is the normal character string comparison.
 */

   int
       Comp_004(const void *v_1_ptr, const void *v_2_ptr) ;


/*
 *     Type is: WT_T_JDN_Time.
 */

   int
       Comp_005(const void *v_1_ptr, const void *v_2_ptr) ;


   int
       CompareDouble_001(double *a_ptr, double *b_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Date and time.
 *
 *----------------------------------------------------------------------
 */



   int
       Curr_Date_Time_Local_001(
           WT_T_Full_Date_Time *o_date_time) ;


   int
       Curr_Date_Time_UTC_001(
           WT_T_Full_Date_Time *o_date_time) ;


   int
       CurrentDateTimeLocal(struct tm *time_ptr) ;


   int
       CurrentDateTimeUTC(struct tm *time_ptr) ;


   int
       DateFromJDN(long int jdn,
           int *year_ptr, int *month_ptr, int *day_ptr,
           int *yearday_ptr, long int *mjn_ptr, int *weekday_ptr) ;


   int
       DateFromMJN(long int mjn,
           int *year_ptr, int *month_ptr, int *day_ptr,
           int *yearday_ptr, long int *jdn_ptr, int *weekday_ptr) ;


   int
       DateFromYearDay(int year, int yearday,
           int *month_ptr, int *day_ptr, long int *jdn_ptr,
           long int *mjn_ptr, int *weekday_ptr) ;


   int
       DateFromYearMonthDay(int year, int month, int day,
           int *yearday_ptr, long int *jdn_ptr, long int *mjn_ptr,
           int *weekday_ptr) ;


   int
       DateTimeFromJDNTime(long int jdn, double time,
           int *year_ptr, int *month_ptr, int *day_ptr,
           int *yearday_ptr, long int *mjn_ptr, int *weekday_ptr,
           int *hr_ptr, int *min_ptr, double *sec_ptr) ;


   int
       DateTimeFromMJNTime(long int mjn, double time,
           int *year_ptr, int *month_ptr, int *day_ptr,
           int *yearday_ptr, long int *jdn_ptr, int *weekday_ptr,
           int *hr_ptr, int *min_ptr, double *sec_ptr) ;


   int
       DateTimeFromYDTime(int year, int yearday, int hr, int min,
           double sec,
           int *month_ptr, int *day_ptr, long int *jdn_ptr,
           long int *mjn_ptr, int *weekday_ptr, double *time_ptr) ;


   int
       DateTimeFromYMDTime(int year, int month, int day, int hr,
           int min, double sec,
           int *yearday_ptr, long int *jdn_ptr, long int *mjn_ptr,
           int *weekday_ptr, double *time_ptr) ;


   int
       Date_Time_From_Time_Struct_001(
           const struct tm *i_date_time,
           WT_T_Full_Date_Time *o_date_time) ;


   int
       DayOfYearToMonthDay(int year, int yearday,
           int *month_ptr, int *day_ptr) ;


   int
       DaysInYear(int leap) ;


   int
       Days_In_Month_001(int leap, int month,
           int *n_days_ptr) ;


   int
       Days_In_Month_002(int year, int month,
           int *n_days_ptr) ;


   int
       Days_In_Year_002(int year) ;


   void
       FormSysDateTime(char *s) ;


   int
       FormatDate(int code, int nsp, int year, int month, int day,
           int yearday,
           char *s, int *length_ptr) ;


   int
       FormatDateTime(int code, int nsp1, int nsp2, int ndec, int year,
           int month, int day, int yearday, int hr, int min, double sec,
           char *s, int *length_ptr) ;


   int
       FormatJDNTime(int code, int nsp1, int nsp2, int ndec,
           long int jdn1, double time1,
           long int *jdn2_ptr, double *time2_ptr, char *s,
           int *length_ptr) ;


   int
       FormatTime(int ndec, int hr, int min, double sec,
           char *s, int *length_ptr) ;


   int
       FormatTimeSec(int ndec, double timsec,
           int *hr_ptr, int *min_ptr, double *sec_ptr, char *s,
           int *length_ptr) ;


   int
       FullYear(int n, int mincent, int minyear,
           int *year_ptr) ;


   double
       HrMinSecToSec(int hr, int min, double sec) ;


   double
       JDNTimesDifference(long int jdn1, double time1, long int jdn2,
           double time2) ;


   long int
       JDNToMJN(long int jdn) ;


   int
       JDNToYearDay(long int jdn,
           int *year_ptr, int *yearday_ptr, int *weekday_ptr) ;


   int
       LeapYear(int year) ;


   long int
       MJNToJDN(long int mjn) ;


   int
       MonthDayToDayOfYear(int year, int month, int day,
           int *yearday_ptr) ;


   int
       NormalizeJDNTime(long int jdn1, double time1,
           long int *jdn2_ptr, double *time2_ptr) ;


   int
       Parse_Date_001(const char *s, char sep, int full_year,
           int min_cent, int min_year,
           int *year_ptr, int *month_ptr, int *d_o_m_ptr,
           int *d_o_y_ptr, long int *jdn_ptr,
           long int *mjn_ptr, int *d_o_w_ptr, char **next_ptr) ;


   int
       Parse_Time_001(const char *s, char sep, int fixed,
           int n_dec,
           int *hour_ptr, int *min_ptr, double *sec_ptr,
           double *time_ptr, char **next_ptr) ;


   void
       SecToHrMinSec(double timsec,
           int *hr_ptr, int *min_ptr, double *sec_ptr) ;


   void
       Sec_To_TimeOfDay(double timsec,
           WT_T_Time_Of_Day *tod_ptr) ;


   double
       Tim_Day_To_Sec(double x) ;


   int
       Tim_From_Sec(int units, double x,
           double *y_ptr) ;


   double
       Tim_Hr_To_Sec(double x) ;


   double
       Tim_Min_To_Sec(double x) ;


   double
       Tim_Sec_To_Day(double x) ;


   double
       Tim_Sec_To_Hr(double x) ;


   double
       Tim_Sec_To_Min(double x) ;


   int
       Tim_To_Sec(int units, double x,
           double *y_ptr) ;


   double
       TimeOfDay_To_Sec(const WT_T_Time_Of_Day *tod_ptr) ;


   double
       TimeRefToChosenJDN(long int jdn1, double time1, long int jdn2) ;


   int
       YearDayToJDN(int year, int yearday,
           long int *jdn_ptr, int *weekday_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Error messages.
 *
 *----------------------------------------------------------------------
 */



   void
       GetErrMsgIO(FILE **stream_ptr, int *writeSwitch_ptr) ;


   void
       PutErrMsg(const char *s1, const char *s2, int errCode) ;


   void
       SetErrMsgIO(FILE *stream, int writeSwitch) ;


   void
       WriteErrMsg(FILE *stream, const char *s1, const char *s2,
           int errCode) ;



/*
 *----------------------------------------------------------------------
 *
 *     Files.
 *
 *----------------------------------------------------------------------
 */



   int
       Close_File_001(WT_T_File_Info *file_info_ptr) ;


   int
       Open_File_001(const char *file_name, const char *mode,
           WT_T_File_Info *file_info_ptr) ;


   int
       Rewind_File_001(int clear_flag,
           WT_T_File_Info *file_info_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Geophysics - IGRF.
 *
 *----------------------------------------------------------------------
 */



   int
       EccentricDipole(long int jdn, double time,
           double *eccDipCenter, double *eccDipAxis) ;


   int
       IGRFIndex(int n, int m,
           int *index_ptr) ;


   int
       IGRFModel(long int jdn, double time, int n_max,
           WT_T_IGRF_Model *model_ptr) ;


   int
       IGRF_Max_Degree_001(int year,
           int *max_degree_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Input/Output.
 *
 *----------------------------------------------------------------------
 */



   int
       File_Size_001(FILE *fp,
           int *f_size_ptr, int *err_ptr) ;


   int
       Fold_String_001(const char *s, int n_1, int n_2, int n_3,
           int n_4, int n_5, int align_w_s, int align_word,
           int nl_space, int single_w_s, int trim,
           int write_empty, int flush,
           FILE *fp, char *o_line, int *n_6_ptr,
           int *word_char_present_ptr,
           int *n_7_ptr, int *err_ptr) ;


   int
       ReadLine_001(int lim, int skipFlag, char c,
           FILE *fp, int *line_no_ptr,
           char *line, int *len_ptr, int *endFlag_ptr, int *err_ptr) ;


   int
       Read_Bin_001(size_t size, size_t n,
           WT_T_File_Info *file_info_ptr,
           void *buff, size_t *n_actual_ptr, int *end_flag_ptr,
           int *err_ptr) ;


   int
       Read_Line_002(int lim, int skip_flag,
           const char *comment_chars,
           FILE *fp, int *line_no_ptr,
           char *line, int *len_ptr, int *end_flag_ptr,
           int *err_ptr) ;


   int
       Read_Line_003(int lim, int skip_flag,
           const char *comment_chars,
           WT_T_File_Info *file_info_ptr,
           char *line, int *len_ptr, int *end_flag_ptr,
           int *err_ptr) ;


   int
       Write_Bin_001(const void *buff, size_t size, size_t n,
           WT_T_File_Info *file_info_ptr,
           int *err_ptr) ;


   int
       Write_Blank_Lines_001(int n,
           WT_T_File_Info *file_info_ptr) ;


   int
       Write_Line(char *s,
           FILE *fp,
           int *err_ptr) ;


   int
       Write_Line_002(const char *line,
           WT_T_File_Info *file_info_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Interpolation.
 *
 *----------------------------------------------------------------------
 */



   int
       LinearInterp(double a, double b, double fa, double fb, double x,
           double *fx_ptr) ;


   int
       LinearInterpDeg(double a, double b, double fa, double fb,
           double x,
           double *fx_ptr) ;


   int
       LinearInterpVector(double t1, double t2, const double *v1,
           const double *v2, double t,
           double *v) ;


   int
       Linear_Interp_Matrix_2D(double t1, double t2,
           double m1[DIM_2][DIM_2], double m2[DIM_2][DIM_2], double t,
           double m[DIM_2][DIM_2]) ;


   int
       Linear_Interp_Matrix_3D(double t1, double t2,
           double m1[DIM_3][DIM_3], double m2[DIM_3][DIM_3], double t,
           double m[DIM_3][DIM_3]) ;



/*
 *----------------------------------------------------------------------
 *
 *     Spacecraft orbit.
 *
 *----------------------------------------------------------------------
 */



   int
       PosGEIWrtDipCtr(const double *r1, int units_code, long int jdn,
           double time,
           double *r2) ;


   int
       Shadow_001(const double *scv, const double *sunv, double effRe,
           double *shadow_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Statistics.
 *
 *----------------------------------------------------------------------
 */



   int
       MeanFormula(long int nx, double sumx,
           double *meanx_ptr) ;


   int
       StandardDevFormula(long int nx, double sumx, double sumxsq,
           double *meanx_ptr, double *stdevx_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Tables.
 *
 *----------------------------------------------------------------------
 */



   int
       BinSearchDouble(double *x_ptr, double *v, long int n, int comp,
           int ascFlag,
           long int *l1_ptr, long int *l2_ptr) ;


   int
       Bin_Search_001(const void *x_ptr, const void *v,
           long int n, size_t size,
           int (*Comp)(const void *v_1_ptr,
                    const void *v_2_ptr),
           long int *l_1_ptr, long int *l_2_ptr) ;


   int
       Load_Table_001(const char *file_path,
           int max_chars_per_line, char comment_character,
           int sort,
           int (*Comp)(const void *v_1_ptr,
                   const void *v_2_ptr),
           int dupl_allowed,
           long int *n_ptr, void **p, int *has_dupl_ptr) ;


   int
       SearchInterpSubs(long int n, long int l1, long int l2,
           long int *n1_ptr, long int *n2_ptr, int *range_ptr) ;


   int
       Table_Lookup_001(const void *x_ptr, const void *v,
           long int n,
           int (*Comp)(const void *v_1_ptr,
                   const void *v_2_ptr),
           int empty_user_value_flag, double empty_user_value,
           int bef_extrap_flag, int bef_repl_flag,
           int bef_user_value_flag, double bef_user_value,
           int aft_extrap_flag, int aft_repl_flag,
           int aft_user_value_flag, double aft_user_value,
           int min_value_flag, double min_value,
           int small_user_value_flag, double small_user_value,
           int max_value_flag, double max_value,
           int large_user_value_flag, double large_user_value,
           int *value_def_ptr, double *value_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Time series.
 *
 *----------------------------------------------------------------------
 */



   int
       Check_Sampling_Rate_Info_001(int i_n_pts,
           const WT_T_Time_Series_Sampling_Rate_Info
               *rate_ptr) ;


   int
       Check_Sampling_Rate_Info_002(int i_n_pts,
           const WT_T_Time_Series_Sampling_Rate_Info *rate_ptr,
           const double *i_time_ptr) ;


   int
       Find_Time_Series_Starting_Index_001(
           const WT_T_Time_Series_Legit_Data_Points *pts_ptr,
           double t,
           int *m_ptr, int *found_ptr) ;


   int
       Set_Time_Series_Legit_Points(int n_pts,
           const double *time_ptr,
           WT_T_Time_Series_Legit_Data_Points *p) ;


   int
       Time_Span_001(int n_pts, const void *t_ptr, int t_type,
           const void *v_ptr, int v_type, int start_idx,
           double max_gap, int cond_code, int reverse, double a,
           double b, int seq_code,
           double *beg_ptr, double *end_ptr,
           int *span_found_flag_ptr, int *end_flag_ptr,
           int *next_idx_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Time series - Fourier transform, convolution, etc.
 *
 *----------------------------------------------------------------------
 */



   int
       Check_Conv_Control_Info_001(
           const WT_T_Conv_Control_Info *control_ptr) ;


   int
       Convolution_001(int i_n_pts, const double *i_time_ptr,
           const void *i_value_ptr, int i_type, int o_type,
           const WT_T_Time_Series_Sampling_Rate_Info *rate_ptr,
           const WT_T_Conv_Control_Info *control_ptr,
           int *o_n_pts_ptr, double *o_time_ptr,
           void *o_value_ptr) ;


   int
       Convolution_002(int method, int clip, int i_n_pts,
           const double *i_time_ptr, const void *i_value_ptr,
           int i_type, int o_type,
           const WT_T_Time_Series_Sampling_Rate_Info *rate_ptr,
           const WT_T_Conv_Control_Info *control_ptr,
           int *o_n_pts_ptr, double *o_time_ptr,
           void *o_value_ptr) ;


   int
       Fourier_Transform_001(int direct_flag,
           unsigned long int nn,
           double *data) ;


   int
       Fourier_Transform_002(int direct_flag,
           unsigned long int n,
           double *data) ;



/*
 *----------------------------------------------------------------------
 *
 *     Time series - maintenance functions for sampling rate.
 *
 *----------------------------------------------------------------------
 */



/*
 *     WT_T_Time_Series_Skein.
 */


   int
       Clear_WT_T_Time_Series_Skein(
           WT_T_Time_Series_Skein *p) ;


   WT_T_Time_Series_Skein *
       Construct_WT_T_Time_Series_Skein(void) ;


   WT_T_Time_Series_Skein *
       Copy_Construct_WT_T_Time_Series_Skein(
           WT_T_Time_Series_Skein *p) ;


   int
       Copy_WT_T_Time_Series_Skein(
           WT_T_Time_Series_Skein *p,
           WT_T_Time_Series_Skein *q) ;


   int
       Destruct_WT_T_Time_Series_Skein(
           WT_T_Time_Series_Skein *p) ;


   int
       Fill_WT_T_Time_Series_Skein(
           WT_T_Time_Series_Skein *p) ;


   int
       Print_WT_T_Time_Series_Skein(
           WT_T_Time_Series_Skein *p,
           FILE *o_fp) ;



/*
 *     WT_T_Time_Series_Regime.
 */


   int
       Clear_WT_T_Time_Series_Regime(
           WT_T_Time_Series_Regime *p) ;


   WT_T_Time_Series_Regime *
       Construct_WT_T_Time_Series_Regime(void) ;


   WT_T_Time_Series_Regime *
       Copy_Construct_WT_T_Time_Series_Regime(
           WT_T_Time_Series_Regime *p) ;


   int
       Copy_WT_T_Time_Series_Regime(
           WT_T_Time_Series_Regime *p,
           WT_T_Time_Series_Regime *q) ;


   int
       Destruct_WT_T_Time_Series_Regime(
           WT_T_Time_Series_Regime *p) ;


   int
       Fill_WT_T_Time_Series_Regime(
           WT_T_Time_Series_Regime *p) ;


   int
       Print_WT_T_Time_Series_Regime(
           WT_T_Time_Series_Regime *p,
           FILE *o_fp) ;



/*
 *     WT_T_Time_Series_Sampling_Rate_Info.
 */


   int
       Clear_WT_T_Time_Series_Sampling_Rate_Info(
           WT_T_Time_Series_Sampling_Rate_Info *p) ;


   WT_T_Time_Series_Sampling_Rate_Info *
       Construct_WT_T_Time_Series_Sampling_Rate_Info(void) ;


   WT_T_Time_Series_Sampling_Rate_Info *
       Copy_Construct_WT_T_Time_Series_Sampling_Rate_Info(
           WT_T_Time_Series_Sampling_Rate_Info *p) ;


   int
       Copy_WT_T_Time_Series_Sampling_Rate_Info(
           WT_T_Time_Series_Sampling_Rate_Info *p,
           WT_T_Time_Series_Sampling_Rate_Info *q) ;


   int
       Destruct_WT_T_Time_Series_Sampling_Rate_Info(
           WT_T_Time_Series_Sampling_Rate_Info *p) ;


   int
       Fill_WT_T_Time_Series_Sampling_Rate_Info(
           WT_T_Time_Series_Sampling_Rate_Info *p) ;


   int
       Print_WT_T_Time_Series_Sampling_Rate_Info(
           WT_T_Time_Series_Sampling_Rate_Info *p,
           FILE *o_fp) ;



/*
 *----------------------------------------------------------------------
 *
 *     Time series - maintenance functions for convolution.
 *
 *----------------------------------------------------------------------
 */



/*
 *     WT_T_Conv_Control_Coeffs_Table.
 */


   int
       Clear_WT_T_Conv_Control_Coeffs_Table(
           WT_T_Conv_Control_Coeffs_Table *p) ;


   WT_T_Conv_Control_Coeffs_Table *
       Construct_WT_T_Conv_Control_Coeffs_Table(void) ;


   WT_T_Conv_Control_Coeffs_Table *
       Copy_Construct_WT_T_Conv_Control_Coeffs_Table(
           WT_T_Conv_Control_Coeffs_Table *p) ;


   int
       Copy_WT_T_Conv_Control_Coeffs_Table(
           WT_T_Conv_Control_Coeffs_Table *p,
           WT_T_Conv_Control_Coeffs_Table *q) ;


   int
       Destruct_WT_T_Conv_Control_Coeffs_Table(
           WT_T_Conv_Control_Coeffs_Table *p) ;


   int
       Fill_WT_T_Conv_Control_Coeffs_Table(
           WT_T_Conv_Control_Coeffs_Table *p) ;


   int
       Print_WT_T_Conv_Control_Coeffs_Table(
           WT_T_Conv_Control_Coeffs_Table *p,
           FILE *o_fp) ;



/*
 *     WT_T_Conv_Control_Coeffs_Table_Set.
 */


   int
       Clear_WT_T_Conv_Control_Coeffs_Table_Set(
           WT_T_Conv_Control_Coeffs_Table_Set *p) ;


   WT_T_Conv_Control_Coeffs_Table_Set *
       Construct_WT_T_Conv_Control_Coeffs_Table_Set(void) ;


   WT_T_Conv_Control_Coeffs_Table_Set *
       Copy_Construct_WT_T_Conv_Control_Coeffs_Table_Set(
           WT_T_Conv_Control_Coeffs_Table_Set *p) ;


   int
       Copy_WT_T_Conv_Control_Coeffs_Table_Set(
           WT_T_Conv_Control_Coeffs_Table_Set *p,
           WT_T_Conv_Control_Coeffs_Table_Set *q) ;


   int
       Destruct_WT_T_Conv_Control_Coeffs_Table_Set(
           WT_T_Conv_Control_Coeffs_Table_Set *p) ;


   int
       Fill_WT_T_Conv_Control_Coeffs_Table_Set(
           WT_T_Conv_Control_Coeffs_Table_Set *p) ;


   int
       Print_WT_T_Conv_Control_Coeffs_Table_Set(
           WT_T_Conv_Control_Coeffs_Table_Set *p,
           FILE *o_fp) ;



/*
 *     WT_T_Conv_Control_Info.
 */


   int
       Clear_WT_T_Conv_Control_Info(
           WT_T_Conv_Control_Info *p) ;


   WT_T_Conv_Control_Info *
       Construct_WT_T_Conv_Control_Info(void) ;


   WT_T_Conv_Control_Info *
       Copy_Construct_WT_T_Conv_Control_Info(
           WT_T_Conv_Control_Info *p) ;


   int
       Copy_WT_T_Conv_Control_Info(
           WT_T_Conv_Control_Info *p,
           WT_T_Conv_Control_Info *q) ;


   int
       Destruct_WT_T_Conv_Control_Info(
           WT_T_Conv_Control_Info *p) ;


   int
       Fill_WT_T_Conv_Control_Info(
           WT_T_Conv_Control_Info *p) ;


   int
       Print_WT_T_Conv_Control_Info(
           WT_T_Conv_Control_Info *p,
           FILE *o_fp) ;


   int
       Read_Conv_Control_File(const char *file_name,
           const char *sub_dir,
           WT_T_Conv_Control_Info *p) ;



/*
 *----------------------------------------------------------------------
 *
 *     Transformation of coordinates.
 *
 *----------------------------------------------------------------------
 */



   int
       DSC_And_GSE(int transf, const double *s, const double *v1,
           double *v2) ;


   int
       GEI_And_GEO(int GEIFlag, long int jdn, double time,
           const double *v1,
           double *v2) ;


   int
       GEI_And_GSE(int GEIFlag, long int jdn, double time,
           const double *v1,
           double *v2) ;


   int
       GEI_And_GSM(int GEIFlag, long int jdn, double time,
           const double *v1,
           double *v2) ;


   int
       GSE_And_GSM(int GSEFlag, long int jdn, double time,
           const double *v1,
           double *v2) ;


   void
       RectCoordToSpher(const double *v,
           double *r_ptr, double *theta_ptr, double *phi_ptr) ;


   void
       Rot2DFromElem(double xR, double yR, double cosPhi, double sinPhi,
           double *xF_ptr, double *yF_ptr) ;


   int
       RotElem_SCC_And_DSC(double t, double t0, double duration,
           double *phi_ptr, double *cosPhi_ptr, double *sinPhi_ptr) ;


   int
       RotMat_DSC_GSE(int DSCFlag, const double *s,
           double a[DIM_3][DIM_3]) ;


   int
       RotMat_FAC_001(int ToFACFlag, const double *b, const double *r,
           double a[DIM_3][DIM_3]) ;


   int
       RotMat_GEI_GEO(int GEIFlag, long int jdn, double time,
           double a[DIM_3][DIM_3]) ;


   int
       RotMat_GEI_GSE(int GEIFlag, long int jdn, double time,
           double a[DIM_3][DIM_3]) ;


   int
       RotMat_GEI_GSM(int GEIFlag, long int jdn, double time,
           double a[DIM_3][DIM_3]) ;


   int
       RotMat_GSE_GSM(int GSEFlag, long int jdn, double time,
           double a[DIM_3][DIM_3]) ;


   void
       Rot_Mat_2D(double alpha,
           double a[DIM_2][DIM_2]) ;


   int
       Rot_Mat_SCC_DSC_2D(int rot_flag, double t, double t0,
           double duration,
           double a[DIM_2][DIM_2]) ;


   int
       Rot_Mat_Vector_Proj_2D(int rot_dir_flag, const double *v,
           double a[DIM_2][DIM_2]) ;


   int
       RotateCoordAroundAxis(int axis, double alpha,
           double a[DIM_3][DIM_3]) ;


   int
       SCC_And_DSC(int SCCFlag, double t, const double *v1, double t0,
           double duration,
           double *v2) ;


   int
       SpherCoordToRect(double r, double theta, double phi,
           double *v) ;



/*
 *----------------------------------------------------------------------
 *
 *     Vectors and matrices.
 *
 *----------------------------------------------------------------------
 */



   int
       AllocateMatrix(long int m, long int n,
           double ***a_ptr) ;


   void
       CopyMatrix_2D(double a[DIM_2][DIM_2],
           double b[DIM_2][DIM_2]) ;


   void
       CopyMatrix_3D(double a[DIM_3][DIM_3],
           double b[DIM_3][DIM_3]) ;


   void
       CopyVector(const double *v1,
           double *v2) ;


   int
       FreeMatrix(long int m, long int n,
           double **a) ;


   int
       MatrixMultiplication(long int m, long int l, long int n,
           double **a, double **b,
           double **c) ;


   void
       MatrixMultiplication_2D(double a[DIM_2][DIM_2],
           double b[DIM_2][DIM_2],
           double c[DIM_2][DIM_2]) ;


   void
       MatrixMultiplication_3D(double a[DIM_3][DIM_3],
           double b[DIM_3][DIM_3],
           double c[DIM_3][DIM_3]) ;


   int
       MatrixTimesVector(long int m, long int n, double **a,
           const double *v1,
           double *v2) ;


   void
       MatrixTimesVector_2D(double a[DIM_2][DIM_2], const double *v1,
           double *v2) ;


   void
       MatrixTimesVector_3D(double a[DIM_3][DIM_3], const double *v1,
           double *v2) ;


   int
       Matrix_Elem_Code_To_Index_2D(int i, int j,
           int *comp_idx_ptr) ;


   int
       Matrix_Elem_Code_To_Index_3D(int i, int j,
           int *comp_idx_ptr) ;


   int
       Matrix_For_Vector_Orthog_Comp_2D(double alpha_1,
           double alpha_2, double beta, int orient,
           double min_denom,
           double a[2][2]) ;


   int
       NormalizeVector(const double *v1,
           double *mag_ptr, double *v2) ;


   int
       PlaneBaseFromNormalVector(const double *nv,
           double *p1, double *p2) ;


   void
       ScalarTimesVector(double a, const double *v1,
           double *v2) ;


   void
       ScalarTimesVectorInPlace(double a,
           double *v) ;


   int
       TransposeMatrix(long int m, long int n, double **a,
           double **b) ;


   void
       TransposeMatrixInPlace_2D(double a[DIM_2][DIM_2]) ;


   void
       TransposeMatrixInPlace_3D(double a[DIM_3][DIM_3]) ;


   void
       TransposeMatrix_2D(double a[DIM_2][DIM_2],
           double b[DIM_2][DIM_2]) ;


   void
       TransposeMatrix_3D(double a[DIM_3][DIM_3],
           double b[DIM_3][DIM_3]) ;


   int
       VectorAzimuthDifference(const double *v1, const double *v2,
           double *diff_ptr) ;


   void
       VectorCrossProduct(const double *v1, const double *v2,
           double *v3) ;


   void
       VectorDifference(const double *v1, const double *v2,
           double *v3) ;


   void
       VectorDifferenceInPlace(double *v1,
           const double *v2) ;


   double
       VectorDotProduct(const double *v1, const double *v2) ;


   double
       VectorMagnitude(const double *v) ;


   int
       VectorProjectionAzimuthDifference(const double *v1,
           const double *v2, const double *p1, const double *p2,
           double *diff_ptr) ;


   void
       VectorSum(const double *v1, const double *v2,
           double *v3) ;


   void
       VectorSumInPlace(double *v1,
           const double *v2) ;


   int
       Vector_Comp_Code_To_Index_2D(int i,
           int *comp_idx_ptr) ;


   int
       Vector_Comp_Code_To_Index_3D(int i,
           int *comp_idx_ptr) ;


   int
       Vector_From_Matrix_2D_001(double a[DIM_2][DIM_2], int code,
           int n,
           double *v) ;


   int
       Vector_From_Matrix_3D_001(double a[DIM_3][DIM_3], int code,
           int n,
           double *v) ;


   double
       Vector_Magnitude_Squared(const double *v) ;


   void
       Vector_Orientation_001(const double *v,
           double *dir_cos, double *dir_ang, double *mag_ptr,
           double *lat_ptr, double *lon_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Workspaces.
 *
 *----------------------------------------------------------------------
 */



   int
       Find_Config_File_001(const char *file_name,
           const char *sub_dir,
           int *found_ptr, char *file_path) ;





/*
 *======================================================================
 *======================================================================
 */





/*
 ***********************************************************************
 *
 *     Function prototypes for C++.
 *
 *     Function pointer type definitions, used to avoid C++ "name
 *     mangling" problems.
 *
 ***********************************************************************
 */



/*
 *----------------------------------------------------------------------
 *
 *     Transformation of coordinates.
 *
 *----------------------------------------------------------------------
 */



   typedef
       int
           (*WT_T_FP_Rot_Mat) (int rot_flag, long int jdn, double time,
               double a[DIM_3][DIM_3]) ;





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





#endif   /* WTGEN001_H */
