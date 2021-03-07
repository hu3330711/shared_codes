/************************************************************************/
/* fastcal_defines.h : #defines for fastcal.c and fastcal_init.c.	*/
/* 									*/ 
/************************************************************************/
/* 			REVISION HISTORY				*/
/* DATE		NAME	COMMENT						*/
/*									*/
/* 6/20/95	REE	Initial version. 				*/
/* 8/20/96	REE	Changed max_cal_error_count to 100.		*/
/************************************************************************/

#ifndef fastcal_defines
#define fastcal_defines

/* For use by SCCS: */
#define SccsId_fastcal_defines_h  "@(#)fastcal_defines.h	1.5, 02/19/04"

/* Array and char sizes.	*/

#define max_cal_linelength	256	/* Max linelength in cal file.	*/ 
#define max_cal_char_length	256	/* Maximum length of char str.	*/
#define max_cal_sfa_entries	10	/* SFA entries. Struc type 4.	*/
#define max_num_of_cal_strucs	1000	/* Max # of fastcal structures.	*/
#define max_cal_num_boom_pairs	50	/* Maximum number of boom pairs.*/
#define max_cal_array_size	1024	/* Maximum cal array size.	*/
#define max_cal_HSBM_entries	8	/* HSBM entries. Struc type 8.	*/
#define max_cal_HSBM_rates	3	/* HSBM entries. Struc type 8.	*/
#define max_cal_error_count	100	/* Max # of cal error messages.	*/

/* Calibration file path.	*/

#define DEFAULT_CAL_PATH 	"$(FASTLIB)/fast_fields_cals"
#define FAST_CALIBRATION_SUFFIX ".cal"

/* Bit masks.			*/

#define bitmask0		1	/* Mask to extract bit 0.	*/
#define bitmask1		2	/* Mask to extract bit 1.	*/
#define bitmask2		4	/* Mask to extract bit 2.	*/
#define bitmask3		8	/* Mask to extract bit 3.	*/
#define bitmask4		16	/* Mask to extract bit 4.	*/
#define bitmask5		32	/* Mask to extract bit 5.	*/
#define bitmask6		64	/* Mask to extract bit 6.	*/
#define bitmask7		128	/* Mask to extract bit 7.	*/
#define bitmask01		3	/* Mask to extract bits 0 & 1.	*/
#define bitmask012		7	/* Mask to extract bits 0,1,&2.	*/


/* Science quantities.		*/

#define pi 3.1415926536
#define e 4.8e-10                /* Electron charge  (esu)              */
#define erg_eV 1.6e-12		 /* Erg per eV conversion		*/
#define MO 2.67e-23		 /* O+ mass (gm)	              	*/
#define MH 1.67e-24		 /* H+ mass (gm)	              	*/
#define me 0.91e-27		 /* e- mass (gm)	              	*/

#endif
