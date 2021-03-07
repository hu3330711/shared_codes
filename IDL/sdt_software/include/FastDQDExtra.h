/* FastDQDExtra.h */

#ifndef FASTDQDEXTRA_H
#define FASTDQDEXTRA_H

/* For SCCS: */
#define SccsId_FastDQDExtra_h  "@(#)FastDQDExtra.h	1.7, 12/04/96"

#include <SDTDescription.h>
#include <SDTLinkedList.h>

#define CALIBR_STANDARD_LINEAR    0
#define CALIBR_GENERAL_LINEAR     1
#define CALIBR_INTEGER_TABLE      2
#define CALIBR_FLOAT_TABLE        3
#define CALIBR_DOUBLE_TABLE       4

/* Scale, offset pair: */
struct DQDScaleOffset_strct
    {
    float   scale ;
    float   offset ;
    } ;
typedef struct  DQDScaleOffset_strct  DQDScaleOffset ;

/* Standard Linear calibration:  For time-series, "n"-components
 * (only "n -1" non-time components) and only one gain setting:
 */ 
struct DQDStdLinearCalibration_strct
    {
    DQDScaleOffset *so ;
    } ;
typedef struct  DQDStdLinearCalibration_strct  DQDStdLinearCalibration ;

/* General Linear calibration: */ 
struct DQDGenLinearCalibration_strct
    {
    /* The number of gain settings to worry about: */
    int     ngains ;

    /* This points to an array of pointers to DQDScaleOffset
     * arrays.  There is one array pointer per DQD component.
     * If a given component has no significant scale/offset
     * (i.e. scale = 1.0, offset = 0.0) then its array pointer
     * will be NULL.  Otherwise, the array for a component will
     * contain "ngains" entries of "DQDScaleOffset"'s:
     */
    DQDScaleOffset **so ;
    } ;
typedef struct  DQDGenLinearCalibration_strct  DQDGenLinearCalibration ;

/* Table calibration: */
/* This is UNDEFINED as of 93/11/28: */
struct DQDTableCalibration_strct
    {
    /* Indicates if the table is made of
     *    integers (CALIBR_INTEGER_TABLE)
     *    floats   (CALIBR_FLOAT_TABLE)
     *    doubles  (CALIBR_DOUBLE_TABLE)
     */
    int     type ;
    } ;
typedef struct  DQDTableCalibration_strct  DQDTableCalibration ;

union DQDCalibration_union
    {
    DQDStdLinearCalibration     stdlinear ;
    DQDGenLinearCalibration     genlinear ;
    DQDTableCalibration         table ;
    } ;
typedef union DQDCalibration_union  DQDCalibration ;

/* This structure contains the auxiliary FAST information which
 * accompanies each Fast DQD:
 */
struct DQDExtra_strct
    {
    /* Points to the SDT general DQD: */
    /* NOTE!  This points to a structure with a life independent
     * of this structure.  Therefore, "dqd" should NOT be deleted
     * by the destructor for "DQDExtra":
     */
    DataQuantityDescription  *dqd ;

    IntList  apid ;  /* The APID's for the DQD: */
    int      vcid ;  /* The Virtual Channel for the DQD: */

    /* Type of calibration:
     *    standard linear (CALIBR_STANDARD_LINEAR)
     *    general linear  (CALIBR_GENERAL_LINEAR)
     *    integer table   (CALIBR_INTEGER_TABLE)
     *    float table     (CALIBR_FLOAT_TABLE)
     *    double table    (CALIBR_DOUBLE_TABLE)
     */
    int    clbr_type ;

    /* The calibration (union based on "clbr_type"): */
    DQDCalibration  clbr ;
    } ;
typedef struct  DQDExtra_strct  DQDExtra ;

struct DQDExtraList_struct
    {
    int  nqtys ;
    struct {
	u_int      qty_len ;
	DQDExtra  *qty_val ;
	} qty ;
    } ;
typedef struct DQDExtraList_struct DQDExtraList ;


/* DQDExtra function declarations (all of these are defined in
 * FastDQDExtra.c or in the FastParseDQD, FastParsePQD files):
 */

extern int  CopyDQDCalibration (DQDCalibration *source,
    DQDCalibration *dest, int  type, DataQuantityDescription *dqd) ;

extern int  ClearDQDCalibration (DQDCalibration *source,
    int  type, DataQuantityDescription *dqd) ;

extern DQDExtra  *ConstructDQDExtra (int n_ap, int *i_ap, int i_vc,
    int cal_type, DQDCalibration *dc_ptr, DataQuantityDescription *dqd) ;

extern DQDExtra  *CopyConstructDQDExtra (DQDExtra *in_dqde) ;

extern int  FillDQDExtra (DQDExtra *dptr, int n_ap, int *i_ap, int i_vc,
    int cal_type, DQDCalibration *dc_ptr, DataQuantityDescription *dqd) ;

extern int  CopyDQDExtra (DQDExtra *in_dqde, DQDExtra *out_dqde) ;

extern int  DestructDQDExtra (DQDExtra *dptr) ;

extern int  ClearDQDExtra (DQDExtra *dptr) ;

extern int  PrintDQDExtra (DQDExtra *dptr, FILE *ofp) ;

extern DQDExtraList  *ConstructDQDExtraList (int nqtys, DQDExtra *dqdel) ;

extern DQDExtraList  *CopyConstructDQDExtraList (DQDExtraList *in_dqdl) ;

extern int  FillDQDExtraList (DQDExtraList *dqdl, int nqtys,
    DQDExtra *dqdel) ;

extern int  CopyDQDExtraList (DQDExtraList *in_dqdl,
    DQDExtraList *out_dqdl) ;

extern int  DestructDQDExtraList (DQDExtraList *dqdl) ;

extern int  DestructDQDExtraLinkedList (SDTListClass *dlist) ;

extern int  ClearDQDExtraList (DQDExtraList *dqdl) ;

extern int  ArrayFillDQDExtraList (int ndqd,
    DQDExtra *in_dqd, DQDExtraList *out_dqdl) ;

extern int  PtrArrayFillDQDExtraList (int ndqd,
    DQDExtra **in_dqd, DQDExtraList *out_dqdl) ;

extern int  PrintDQDExtraList (DQDExtraList *dqdl,
    FILE *ofp) ;

extern DataQuantityDescriptionList  *EndFastDQDParsing (
    ParseDQDExtraInfo *deptr, DQDExtraList **ret_extra) ;

extern PlotQuantityDescriptionList  *EndFastPQDParsing (
    ParsePQDExtraInfo *peptr) ;

extern int  SetDQD1GenerationList (DataQuantityDescriptionList *dqdl) ;

extern FILE  *init_dqde_session (char *fname) ;
extern int  init_pqde_session (char *fname) ;

extern int  dqd1parse (void) ;
extern int  pqd1parse (void) ;

int SDTFastParseQuantities (char *dqd_fname, char *pqd_fname,
    DataQuantityDescriptionList **dqdl, ParseDQDExtraInfo *deptr,
    DQDExtraList **dqdel,
    PlotQuantityDescriptionList **pqdl, ParsePQDExtraInfo *peptr) ;

#endif /* FASTDQDEXTRA_H */
