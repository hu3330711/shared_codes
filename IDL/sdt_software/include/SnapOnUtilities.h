/* ---------------------------------------------------------------------- */
/*
 * Declarations for the SnapOnUtilities library:
 */
#ifndef SNAPONUTILITIES_H
#define SNAPONUTILITIES_H

#include "SnapOn.ANSI-C.h"

/* For SCCS: */
#define SccsId_SnapOnUtilities_h "@(#)SnapOnUtilities.h	1.2, 12/03/06"

#define  EXTRAPOLATE_BACK_FROM_FIRST_ELEMENT  0
#define  EXTRAPOLATE_FORWARD_FROM_LAST_ELEMENT  1
#define  INTERPOLATE_BRACKETING_PAIR  2
#define  INTERPOLATE_EXACT_RESULT  3

extern double InterpolateData (DataQuantityInstance *dqi, int in_comp,
    int out_comp, double in_value, double close_value,
    int last_known_idx, int *new_last_idx, int *ret_flg) ;

extern double InterpolateFloatFloatArrayData (DataQuantityInstance *dqi,
    int in_comp, int out_comp, double in_value, double close_value,
    int last_known_idx, int *new_last_idx, int *ret_flg) ;

extern double InterpolateDoubleFloatArrayData (DataQuantityInstance *dqi,
    int in_comp, int out_comp, double in_value, double close_value,
    int last_known_idx, int *new_last_idx, int *ret_flg) ;

extern double InterpolateFloatDoubleArrayData (DataQuantityInstance *dqi,
    int in_comp, int out_comp, double in_value, double close_value,
    int last_known_idx, int *new_last_idx, int *ret_flg) ;

extern double InterpolateDoubleDoubleArrayData (DataQuantityInstance *dqi,
    int in_comp, int out_comp, double in_value, double close_value,
    int last_known_idx, int *new_last_idx, int *ret_flg) ;

extern double InterpolateFloatVectorData (DataQuantityInstance *dqi,
    int vsize, int in_comp, int out_comp, double in_value,
    double close_value,
    int last_known_idx, int *new_last_idx, int *ret_flg) ;

extern double InterpolateDoubleVectorData (DataQuantityInstance *dqi,
    int vsize, int in_comp, int out_comp, double in_value,
    double close_value,
    int last_known_idx, int *new_last_idx, int *ret_flg) ;

extern int FillXvsYData (char *XQtyName, char *YQtyName,
    int xcomparative_comp, int xdata_comp,
    int ycomparative_comp, int ydata_comp) ;

extern int FillXvsYDataExt (char *XQtyName, char *YQtyName,
    double xminlim, double xmaxlim,
    int xcomparative_comp, int xdata_comp,
    int ycomparative_comp, int ydata_comp) ;

extern DataQuantityInstance  *GenerateXvsYData (
    PlotQuantityDescription *pptr,
    DataQuantityInstanceList  *dqil, int mem_type,
    int xcomparative_comp, int xdata_comp,
    int ycomparative_comp, int ydata_comp) ;

extern int  GetDataVectorByIndex (DataQuantityInstance *dqi,
    int index, double *rvec) ;

extern int  Get_zoom_indices_vecn_float (float *in_ptr,
    int first_index, int last_index, int max_pts_to_report,
    double start_time, double end_time,
    int *avail_pts, int *next_index, int ncomp, int tcmp) ;

extern int  Get_zoom_indices_vecn_double (double *in_ptr,
    int first_index, int last_index, int max_pts_to_report,
    double start_time, double end_time,
    int *avail_pts, int *next_index, int ncomp, int tcmp) ;

extern int  Get_zoom_indices_array_time_float (float *in_ptr,
    int first_index, int last_index, int max_pts_to_report,
    double start_time, double end_time,
    int *avail_pts, int *next_index) ;

extern int  Get_zoom_indices_array_time_double (double *in_ptr,
    int first_index, int last_index, int max_pts_to_report,
    double start_time, double end_time,
    int *avail_pts, int *next_index) ;

#endif /* SNAPONUTILITIES_H */
