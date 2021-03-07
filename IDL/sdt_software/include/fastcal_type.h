/************************************************************************/
/* fastcal_type.h : Struc types for all functions in fastcal.c and 	*/
/* cal_init.c.								*/
/*									*/ 
/* Contains:	fastcal_type_1	Linear calibration structure.		*/
/*		fastcal_type_2  Density probe exponetial fit.		*/
/*		fastcal_type_3  BBF linear calibration (exploded).	*/
/*		fastcal_type_4  SFA interpolated frequency fits.	*/
/*									*/
/************************************************************************/
/* 			REVISION HISTORY				*/
/* DATE		NAME	COMMENT						*/
/*									*/
/* 06/20/95	REE	Initial version. Fields calibration types.	*/
/* 08/10/96	REE	Modified DSP - added bandwidth.			*/
/* 08/20/96	REE	Added type_12 + several modifications.		*/
/* 10/31/96	REE	Added Freq-Phase-Amp to DSP structure.		*/
/* 11/06/96	REE	Change SFA to conform to IDL.			*/
/* 11/11/96	REE	Change of fastcal_5() MagAC.			*/
/* 11/12/96	REE	Change of fastcal_12() MagDC.			*/
/* 11/27/96	REE	Added min/max to DSP(11) and SFA(4).		*/
/* 12/03/96	REE	Fixed LFF roll below zero get_fastcal_2(). 	*/
/************************************************************************/

#include "fastcal_defines.h"

#ifndef fastcal_type
#define fastcal_type

/* For use by SCCS: */
#define SccsId_fastcal_type_h  "@(#)fastcal_type.h	1.14, 02/19/04"

/* Used by IDL calling routines call_fastcal(). */
typedef int IDL_long;

/* Used by fa_fields_lib.c. */
typedef struct two_pole_filt_struc_ {
    double freq;
    double q;     /* Actually 1/Q. */
    double last_x1;
    double last_x2;
    double last_out1;
    double last_out2;
    double last_t;
    double last_dt;
    double c0;
    double c1;
    double c2;
    double d1;
    double d2;
    int    valid;
    } two_pole_filt_struc;

/* Header structure.		*/
typedef struct fastcal_header_ {

    char   data_name[max_cal_char_length];
    int    cal_type;
    char   x_units[max_cal_char_length];
    char   y_units[max_cal_char_length];
    char   z_units[max_cal_char_length];
    int    subtype_1;
    int    subtype_2;
    int    history;

    } fastcal_header_type;

/* Type 1 structure. (Linear)		*/

typedef struct fastcal_struc_type_1_ {
    double unix_time;
    double offset;
    double gain;
    unsigned char highGainMode[256];
    } fastcal_struc_type_1;

/* Type 2 structure. (Ne, LFF - Log)	*/

typedef struct fastcal_struc_type_2_ {
    double unix_time;
    double offset;
    double gain;
    double zero_level;
    double last_value;
    unsigned char highGainMode[256];

    } fastcal_struc_type_2;

/* Type 3 structure. (BBF)		*/

typedef struct fastcal_struc_type_3_ {
    double unix_time;
    double offset;
    double gain;

    } fastcal_struc_type_3;

/* Type 4 structure. (SFA)		*/

typedef struct fastcal_struc_type_4_ {
    double unix_time;
    double Vzero;
    double gain;
    double order_2;
    double bandwidth;
    double min;			/* Min and max values. Values above/	*/
    double max;			/* below will be set. Cleans up plots.  */
    int	   word_index;		/* Set to 4 if channel 4!		*/
    int    cof_type;		/* 0=poly, 1=Hgh P, 2-Low P., 3-RC/CC	*/
    int    num_cof;		/* Max 10.				*/
    double amp_cof[10];		
    double freq_zero[8];	/* Index is (BBFOFF, STP, INC) = (0:7).	*/
    double freq_delta[8];

    double amp_zero;		/* The value of amp_vs_freq at f=0.     */
    int    arrays_valid;	/* NOT PART OF CAL FILE! Data storage.  */
    float  amp_vs_freq[256];	/* NOT PART OF CAL FILE! Data storage.  */
    float  freq_min[256];	/* NOT PART OF CAL FILE! Data storage.  */
    float  freq_max[256];	/* NOT PART OF CAL FILE! Data storage.  */
    } fastcal_struc_type_4;

/* Type 5 structure: Mag AC.	*/

typedef struct fastcal_struc_type_5_ {
    double unix_time;
    double gain;
    double offset;
    double base_freq;		/* Frequency in kHz of one-pole filter.  */
    double pole_freq;		/* Frequency in kHz of one-pole filter.  */

    double last_time;		/* NOT PART OF CAL FILE! Data storage.   */
    double last_value1;		/* NOT PART OF CAL FILE! Data storage.   */
    double last_value2;		/* NOT PART OF CAL FILE! Data storage.   */
    double last_value3;		/* NOT PART OF CAL FILE! Data storage.   */
    double filt_b;		/* NOT PART OF CAL FILE! Data storage.   */
    double filt_p;		/* NOT PART OF CAL FILE! Data storage.   */
    int    sample_rate; 	/* NOT PART OF CAL FILE! Data storage.   */
    int    data_struc_type;	/* Give 7 for MagAC. Otherwise FLT.	 */
    } fastcal_struc_type_5;

/* Type 6 structure.		*/

typedef struct fastcal_struc_type_6_ {
    double unix_time;
    double a0; 	/* Offset	*/
    double a1; 	/* Linear	*/
    double a2; 	/* 2nd Order	*/
    double a3; 	/* 3rd Order	*/
    double a4; 	/* 4th order	*/
    double a5; 	/* 5th order	*/

    } fastcal_struc_type_6;

/* Type 7 structure.		*/

typedef struct fastcal_struc_type_7_ {
    double unix_time;
    double gain;
    double offset;
    double linear;
    double quadratic;
    double cubic;

    } fastcal_struc_type_7;

/* Type 8 structure. (HSBM)	*/

typedef struct fastcal_struc_type_8_ {
    double unix_time;
    double offset;
    double gain;
    double base_freq1;		/* Frequency in Hz of one-pole filter. */
    double pole_freq1;		/* Frequency in Hz undo low-pass.  	*/
    double pole_freq2;		/* Frequency in Hz undo low-pass.  	*/
    double pole_freq3;		/* Frequency in Hz undo low-pass.  	*/

    double last_b1;		/* Data storage.   */
    double last_p1;		/* Data storage.   */
    double last_p2;		/* Data storage.   */
    double last_p3;		/* Data storage.   */
    double filt_b1;		/* Data storage.   */
    double filt_p1;		/* Data storage.   */
    double filt_p2;		/* Data storage.   */
    double filt_p3;		/* Data storage.   */
    int    sample_rate; 	/* Data storage.   */

    } fastcal_struc_type_8;

/* Type 9 structure. (WPC)	*/

typedef struct fastcal_struc_type_9_ {
    double unix_time;
    int esa ;			/* 1-4, based on data quantity name */
    double tau_r;		/* registration time for dead time correction. */
    double lf_freq[2];
    double lf_phase[2];
    } fastcal_struc_type_9;


/* Type 11 structure. (DSP)	*/

typedef struct fastcal_struc_type_11_ {
    double unix_time;
    double gain;
    double bandwidth;
    double min;			/* Min and max values. Values above/	*/
    double max;			/* below will be set. Cleans up plots.  */
    int    cof_type;		/* 0=poly, 1=Hgh P, 2-Low P., 3-RC/CC	*/
    int    num_cof;		/* Max 10.				*/
    double amp_cof[10];		
    double phi_cof[10];		

    int    arrays_valid;	/* NOT PART OF CAL FILE! Data storage.  */
    float  amp_vs_freq[512];	/* NOT PART OF CAL FILE! Data storage.  */
    float  phi_vs_freq[512]; 	/* NOT PART OF CAL FILE! Data storage.  */
    float  freq[512];		/* NOT PART OF CAL FILE! Data storage.  */
    unsigned char highGainMode[256];
    } fastcal_struc_type_11;

/* Type 12 structure. (MagDC)	*/

#define MAX_SM_COF 128
typedef struct fastcal_struc_convol_smooth_ {
    int num_cof[8];             /* num coefficients at each survey speed*/
    double cof[8][MAX_SM_COF];	/* coefficients for each survey speed	*/
    float stored_x[MAX_SM_COF];/* NOT PART OF CAL FILE! Data storage.  */
    float stored_y[MAX_SM_COF];/* NOT PART OF CAL FILE! Data storage.  */
    float stored_z[MAX_SM_COF];/* NOT PART OF CAL FILE! Data storage.  */
    double stored_t[MAX_SM_COF];/* NOT PART OF CAL FILE! Data storage.  */
    int stored_N;		/* NOT PART OF CAL FILE! Data storage.  */
} fastcal_struc_convol_smooth;

#define MAX_SPIN_POINTS 6000
typedef struct fastcal_struc_despin_ {
    double tweak_filter;	/* filter cof for spin tone removal.	*/
    int fit;			/* use least squares fit if true */
    double min_chisqr;		/* minimum acceptable chisqr for fit */
    float stored_x[MAX_SPIN_POINTS];
    float stored_y[MAX_SPIN_POINTS];
    float stored_z[MAX_SPIN_POINTS];
    int num_pts;
    double tweak_x;
    double tweak_y;
    int have_tweak_x;
    int have_tweak_y;
} fastcal_struc_despin;

typedef struct fastcal_struc_type_12_ {
    double unix_time;
    double gain[3][3];		/* Gain / orth matrix.			*/
    double offset[3];		/* Mag1dc = (raw-offset)*linear +	*/
    double time_cof;		/* Re-adjust time - stored_t[time_cof].	*/
    int    num_cof;		/* Max 10.				*/
    double cof_x[10];		/* Cubic / spline coefficients.		*/
    double cof_y[10];		/* Cubic / spline coefficients.		*/
    double cof_z[10];		/* Cubic / spline coefficients.		*/
    fastcal_struc_convol_smooth *pre_smooth;  /* coefficients and data.	*/
    fastcal_struc_convol_smooth *post_smooth; /* coefficients and data.	*/

    fastcal_struc_despin *despin_z;	/* NOT PART OF CAL FILE! Data.  */

    float stored_x[10];		/* NOT PART OF CAL FILE! Data storage.  */
    float stored_y[10];		/* NOT PART OF CAL FILE! Data storage.  */
    float stored_z[10];		/* NOT PART OF CAL FILE! Data storage.  */
    double stored_t[10];	/* NOT PART OF CAL FILE! Data storage.  */

    } fastcal_struc_type_12;

typedef union fastcal_union_ {
    fastcal_struc_type_1	type_1;
    fastcal_struc_type_2	type_2;
    fastcal_struc_type_3	type_3;
    fastcal_struc_type_4	type_4;
    fastcal_struc_type_5	type_5;
    fastcal_struc_type_6	type_6;
    fastcal_struc_type_7	type_7;
    fastcal_struc_type_8	type_8;
    fastcal_struc_type_9	type_9;
    fastcal_struc_type_11	type_11;
    fastcal_struc_type_12	type_12;

    } fastcal_union;

/* Fast calibration structure.	*/

typedef struct fastcal_struc_type_ {

    fastcal_header_type 	header;
    fastcal_union		cal_union;
    double			boomlength;
    int				init_error;
    int				cal_error;
    int				cal_error_count;

    } fastcal_struc_type;

#endif
