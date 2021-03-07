/*
 * Definitions for Fast Calibration access in FastDecom:
 */
#ifndef FASTCALDATA_STRUC_H
#define FASTCALDATA_STRUC_H

/* For use by SCCS: */
#define SccsId_FastCalData_struc_h  "@(#)FastCalData_struc.h	1.5, 02/19/04"

/* ---------------------------------------------------------------- */
/* enumeration for checking of data types */

enum fastcal_data_type { FLT, SFA, DSP, HFQ, MAG, WPC, FPA, MAC} ;

/* ---------------------------------------------------------------- */
/* SFA (Apids 1036, 1057): */

struct SfaCalibrateBin_struct
    {
    /* Takes on a value of 1, 2, 3, or 4 - it indicates which of the
     * four arrays for the packet we are dealing with here.  This may
     * be required to fully utilize the data header information.
     */
    int    WordIdx ;

    /* Pointer to the Data Array (256 bins): */
    float  *Data ;

    /* Points to, respectively, the arrays of values defining the
     * minimums and maximums of the particular 256 bin array in the SFA
     * packet.  The storage is defined as follows:
     *
     *  Range of Bin[0]:     MinVal[0]  to  MaxVal[0]
     *  Range of Bin[1]:     MinVal[1]  to  MaxVal[1]
     *        .                         .
     *        .                         .
     *        .                         .
     *  Range of Bin[255]:   MinVal[255]  to  MaxVal[255]
     *
     * Note that the "MinVal"'s and "MaxVal"'s actually point into
     * shared-memory although that need not concern the user of this
     * structure.
     */
    double  *MinVal ;
    double  *MaxVal ;
    } ;
typedef  struct  SfaCalibrateBin_struct  SfaCalibrateBin ;

/* ---------------------------------------------------------------- */
/* DSP  */

typedef struct DSP_calibrate_struct_
    {
    /* Takes on a value of 1-16 - it indicates which of the
     * DSP channel we are dealing with here.  This may
     * be required to fully utilize the data header information.
     */
    int    WordIdx ;

    /* Pointer to the Data Array (512 bins): */
    float  *Data ;

    /* Points to, respectively, the arrays of values defining the
     * minimums and maximums of the particular 512 bin array in the DSP
     * packet.  The storage is defined as follows:
     *
     *  Range of Bin[0]:     MinVal[0]  to  MaxVal[0]
     *  Range of Bin[1]:     MinVal[1]  to  MaxVal[1]
     *        .                         .
     *        .                         .
     *        .                         .
     *  Range of Bin[511]:   MinVal[511]  to  MaxVal[511]
     *
     * Note that the "MinVal"'s and "MaxVal"'s actually point into
     * shared-memory although that need not concern the user of this
     * structure.
     */
    double  *MinVal ;
    double  *MaxVal ;
    } DSP_calibrate_struct;

/* Structure for HFQ Phase Difference - type 7 calibration. */

typedef struct HFQ_PD_calibrate_struct_ {
    float  *data;		/* Pointer to array of raw data values.  */
    float  *frequency;		/* Frequency as specified in HFQ packet. */
    } HFQ_PD_calibrate_struct;

typedef struct WPC_calibrate_struct_ {
    double                *time ;
    float                 *wfrq ;
    float		  *wamp ;
    float		  *tot0 ;
    float                 *res0 ;
    float                 *rac0 ;
    float		  *tot1 ;
    float                 *res1 ;
    float                 *rac1 ;
    float		  *tot2 ;
    float                 *res2 ;
    float                 *rac2 ;
    float                 *dres ;
    float                 *drac ;
    float	          *dtot ;
    float		  *p3sig0 ;
    float		  *n3sig0 ;
    float		  *p3sig1 ;
    float		  *n3sig1 ;
    float		  *p3sig2 ;
    float		  *n3sig2 ;
    char		  *bres0 ;
    char		  *brac0 ;
    char		  *bres1 ;
    char		  *brac1 ;
    char		  *bres2 ;
    char		  *brac2 ;
    double		  *Bphase ;
    } WPC_calibrate_struct;

/* ---------------------------------------------------------------- */
/* MAG dc  */
/* Modified by REE 11/11/96, */
typedef struct MagDC_calibrate_struct_ {
    
    float *MagX;
    float *MagY;
    float *MagZ;
    double *time;
    
    } MagDC_calibrate_struct;

/* ---------------------------------------------------------------- */
/* MAG ac  */
/* Added by REE 11/11/96. */
typedef struct MagAC_calibrate_struct_ {
    
    double *data;
    double *time;
    
    } MagAC_calibrate_struct;

#endif  /* FASTCALDATA_STRUC_H */
