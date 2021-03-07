/* -----------------------------------------------------------------------
 * SDTDataUtilities.h
 */

#ifndef SDTDATAUTILITIES_H
#define SDTDATAUTILITIES_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* SCCS ID string: */
#define SccsId_SDTDataUtilities_h "@(#)SDTDataUtilities.h	1.15, 08/17/07"

#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <math.h>

#include <SDTDescription.h>
#include <SDTInstance.h>
#include <DQHInterface.h>

/* -------------------------------------------------------------------- */
/* Constants: */

/* The minimum time allowed for equal time-delta consecutive
 * data points:
 */
#define SDT_MIN_TIME_DELTA_FOR_INDICES   0.000000001

/* For vertical slicing: */
#define  VSLICE_TOTAL_IN_ROW   0
#define  VSLICE_AVG_IN_ROW     1

#define  SLICE_USE_BIN_MIN_VAL     0
#define  SLICE_USE_BIN_MID_VAL     1
#define  SLICE_USE_BIN_MAX_VAL     2

/* -------------------------------------------------------------------- */
/* Typedefs: */

/* This structure is used by routine:
 *
 *    SDTFindConstantTimeDeltaRanges
 *
 * to return index ranges of constant time delta for use in
 * FFT, etc. handling.
 *
 * Note that there are no access/construction support routines
 * for this structure as it is not used in many places.
 */
struct SDTTimeDeltaIndexRanges_struct
    {
    int      NumberRanges ;
    int      NPointsMinimum ;
    double   *TimeDelta ;
    double   *AvgTimeDelta ;
    double   *MinTimeDelta ;
    double   *MaxTimeDelta ;
    int      *StartIdx ;
    int      *EndIdx ;
    double   *StartTime ;
    double   *EndTime ;
    } ;
typedef  struct SDTTimeDeltaIndexRanges_struct  SDTTimeDeltaIndexRanges ;


/* -------------------------------------------------------------------
 * Structure containing information about vertical slicing of
 * 1, 2, 3 Dimensional DQD's.
 */
struct SDTVrtSliceSpectraInfo_struct
    {
    /* The data point of the input DQI to be vertically sliced: */
    int   didx ;

    /* Type of row-by-row accumulation that will defined the
     * slicing.  As of 2000/01/20, there are two types:
     *
     *    VSLICE_TOTAL_IN_ROW      sum all elts in each row
     *
     *    VSLICE_AVG_IN_ROW        avg up elts in each row
     */
    int   Type ;

    /* Flag indicating whether or not the X-axis will represent
     * bins (0) or physical units (1), from the associated array
     * description, if it exists.  Note that if this flag
     * is ON (physical units), then the data points generated
     * by the drawing routine will probably have to be sorted
     * as the order of the bins is not necessarily the in ascending
     * or descending physical units order.
     */
    int   XAxisUnits ;

    /* If "XAxisUnits" is ON, and if array descriptions for the
     * multi-dimensional qty exist, this flag indicates whether
     * or not we should skip bins marked OFF by the QFlag for the
     * bin.  If "0", we ignore the QFlag's and look at everything.
     * If "1", we skip any bins marked OFF by the QFlag.
     */
    int   SkipBadBins ;

    /* If "XAxisUnits" is ON, and if array descriptions for the
     * multi-dimensional qty exist, this flag indicates how
     * we are to set the X-value for each point in the plot.
     * The possible values are:
     *
     *    SLICE_USE_BIN_MIN_VAL
     *    SLICE_USE_BIN_MID_VAL
     *    SLICE_USE_BIN_MAX_VAL
     *
     * Of course, if the X-Axis is based on bins, there is no
     * need for this - the X-value of each point will be the
     * integral value of the bin number.
     */
    int   PhysicalBinRepFlag ;

    /* The subdimension of the multi-dimensional DQI that is
     * to be represented on the abcissa (X axis).  The number
     * has the following meaning:
     *
     *   1-Dimensional:
     *       Not relevent in this case as there is only one
     *       sub-dimension to choose from.
     *
     *   2-Dimensional:
     *       Val:  0  ->  axis is "innermost" subdim in storage
     *             1  ->  axis is "outermost" subdim in storage
     *
     *   3-Dimensional:
     *       Val:  0  ->  axis is "innermost" subdim in storage
     *             1  ->  axis is "in the middle" subdim in storage
     *             2  ->  axis is "outermost" subdim in storage
     */
    int   XAxis_SubDim ;

    /* These indicate which dimension of the spectra are
     * "vertical" (defining rows) and "horizontal" (defining
     * columns).   For 3-Dim case, "FixedSubDimIdx" is the
     * fixed-dimension.   The numbers have the following meaning:
     *
     *   1-Dimensional:
     *       Not relevent in this case as there is only one
     *       sub-dimension to choose from.
     *
     *   2-Dimensional:
     *       Val:  0  ->  axis is "innermost" subdim in storage
     *             1  ->  axis is "outermost" subdim in storage
     *
     *   3-Dimensional:
     *       Val:  0  ->  axis is "innermost" subdim in storage
     *             1  ->  axis is "in the middle" subdim in storage
     *             2  ->  axis is "outermost" subdim in storage
     */
    int   VrtSubDimIdx ;
    int   HrzSubDimIdx ;
    int   FixedSubDimIdx ;

    /* If "XAxisUnits" is ON (indicating physical units on the
     * X axis), then we have to know which of the array descriptions
     * corresponding to the bins in the X-axis sub-dimension these
     * physical units refer to.  For instance, some of the FAST
     * TEAMS quantities have the angle sub-dimension, which contains
     * 2 array descriptions - PHI and THETA.  Most mdim quantities
     * have only one corresponding array description - the maximum is:
     *
     *        MDIM_ADESC_MAX
     */
    int   XAxisArrayDescIdx ;

    /* The number of dimensions in the multi-dimensional DQD -
     * will be 1, 2, or 3:
     */
    int   NDims ;

    /* Size of (i.e. number of elements in) each dimension: */
    int   DimSize[3] ;

    /* For the 3-Dimensional case only - this integer indicates
     * the value of the fixed sub-dimension that is of interest.
     * This must be between 0 and N-1, where N is the number of
     * elements in the fixed sub-dimension.  In the case of
     * the "DataProbeBox", this corresponds, in the 3-D case,
     * to the selected index in the Fixed sub-dimension, as
     * selected by the "Component of Fixed Dimension" button.
     */
    int   FixedSubDimValue ;

    /* Flag indicating whether we want X to be in ascending or
     * descending order:
     *    0 -> Ascending order
     *    1 -> Descending order
     */
    int   XOrder ;
    } ;
typedef  struct SDTVrtSliceSpectraInfo_struct SDTVrtSliceSpectraInfo ;

/* -------------------------------------------------------------------
 * Structure containing input criteria to determine the sampling
 * rate history of a time-series quantity:
 */
struct SDTTSeriesSmpRateInput_struct
    {
    /* The allowable number of skips before we consider it the
     * start of a new regime.  As follows:
     *     0 -> any skip starts a new regime
     *     1 -> 1 skipped pt allowed.
     *     etc.
     */
    int                    nskips ;

    /* Flag indicating how we determine if consecutive data points
     * are consistent with the current sampling rate.  It is
     * defined as follows:
     *
     *    0  ->  the criteria is based on a percentage difference
     *           from the previous consistent points.  This value
     *           is stored in "srate_fuzz".
     *
     *    1  ->  the criteria is based on the absolute time difference
     *           (in seconds) between data points.  This value
     *           is stored in "max_t_diff".
     *
     */
    int                    consistency_check_type ;

    /* The percentage difference allowed from a time-delta, to the
     * previous time-delta, for them to be considered to be sampled
     * at the same rate.  A standard value here would be 1% (0.01)
     */
    double                 srate_fuzz ;

    /* If we wish to define time-delta "fuzz" by an actual time
     * value (in seconds), the allowable time diff will be stored
     * here:
     */
    double                 max_t_diff ;

    /* The minimum number of data points, including allowable skipped
     * points as defined by "nskips", that are required to define
     * a "SDTTSeriesSmpRateRegime".  All the points, including
     * interpolated missing points, should be consistent within
     * the definition of either "srate_fuzz" or "max_t_diff".
     *
     * This should probably always be a significant positive value
     * (> 100, for instance).
     */
    int                    min_consistent_pts ;

    /* Flag indicating inclusion of "Gap" (i.e. non-consistent) regimes.  
     *
     *     0 -> show all regimes, including inconsistent one.
     *     1 -> show only constistent regimes.
     *
     */
    int                    show_only_constistent_regimes ;

    /* Flag (not used as of 2002/01/11) indicating how the internal
     * algorithm should check for consistency.  As of 2002/01/11,
     * the algorithm makes an initial computation of the idealized
     * frequency of the data and consistency checks are based on
     * that.  It might later be decided to use a running average
     * or some other scheme.
     */    
    int                    alg_code ;
    } ;
typedef  struct SDTTSeriesSmpRateInput_struct SDTTSeriesSmpRateInput ;

/* -------------------------------------------------------------------
 * Structure containing information about a given sampling rate regime
 * for a time series DQI.
 */
struct SDTTSeriesSmpRateRegime_struct
    {
    /* The time span of the regime.  The Start date/time is the time
     * of the first data point in the regime, the End date/time is
     * the time of the last data point in the regime.
     */
    TimeSpanDescription  TSpan ;

    /* Total points actually found in the regime. */
    int                  TotalPts ;

    /* Within the DQI, the first and last indices of the points in
     * this regime.  So "TotalPts" == "LastDataIdx" - "FirstDataIdx" + 1.
     */
    int                  FirstDataIdx ;
    int                  LastDataIdx ;

    /* Flag indicating whether or not this regime has an apparent
     * consistant time sampling rate, in which case the value of the
     * flag is "1".  If not (value == 0), then the fields following
     * this one have meaningless values.
     */
    int                  ConsistentSamplingRate ;

    /* The ideal time delta of the points in the regime (i.e.
     * 0.01 secs == 100 Hz).
     */
    double               TDelta ;

    /* The ideal frequency of the points in the regime. */
    double               Freq ;

    /* The average time delta of the points in the regime - this
     * computation only includes points that are consecutive.
     */
    double               AvgTDelta ;

    /* The average frequency of the points in the regime. */
    double               AvgFreq ;

    /* The total number of points in the regime, if there had
     * been no skips:
     */
    int                  TotalIdealPts ;

    /* The total number of skipped points.  So "TotalIdealPts" ==
     * == "TotalPts" + "TotalSkippedPts".
     */
    int                  TotalSkippedPts ;

    /* Minimum and Maximum time deltas allowed from the ideal
     * frequency "Freq" for points to be considered consistent.
     */
    double               MinAllowableTimeDelta ;
    double               MaxAllowableTimeDelta ;

    /* The total number of data gaps in the regime. */
    int                  ngaps ;

    /* The number of gap-less sets of consecutive points. */
    int                  nskeins ;

    /* Array with the starting pt. index of each "skein".
     * There will be "nskeins" of these and the array is
     * allocated and filled in:
     *
     *    SDTQuerySamplingHistory
     *
     * It is up to the caller of this routine to later call:
     *
     *    SDTDestructSDTTSeriesSmpRateRegime
     *
     * to insure that this memory gets freed.
     */
    int                  *skein_start_idx ;

    /* Array with the number of pts in each "skein", corresponding
     * in a one-to-one correspondence with "skein_start_idx".
     * There will be "nskeins" of these and the array is
     * allocated and filled in:
     *
     *    SDTQuerySamplingHistory
     *
     * It is up to the caller of this routine to later call:
     *
     *    SDTDestructSDTTSeriesSmpRateRegime
     *
     * to insure that this memory gets freed.
     */
    int                  *skein_npts ;

    /* Array with the number of points in the data gap, before this
     * "skein".  Of course, the first "skein" will have a value of 0.
     * There will be "nskeins" of these and the array is
     * allocated and filled in:
     *
     *    SDTQuerySamplingHistory
     *
     * It is up to the caller of this routine to later call:
     *
     *    SDTDestructSDTTSeriesSmpRateRegime
     *
     * to insure that this memory gets freed.
     */
    int                  *npts_in_precediing_gap ;
    } ;
typedef  struct SDTTSeriesSmpRateRegime_struct SDTTSeriesSmpRateRegime ;

/* -------------------------------------------------------------------
 * Structure containing a list of "SDTTSeriesSmpRateRegime"'s */
struct SDTTSeriesSmpRateRegimeList_struct
    {
    /* The input criteria for generation of this list: */
    SDTTSeriesSmpRateInput   criteria ;

    /* The number of elements in array "list": */
    int                      nelts ;

    /* The array of "SDTTSeriesSmpRateRegime"'s: */
    SDTTSeriesSmpRateRegime  *list ;
    } ;
typedef  struct SDTTSeriesSmpRateRegimeList_struct SDTTSeriesSmpRateRegimeList ;

/* -------------------------------------------------------------------- */
/* Variables and Arrays: */

/* -------------------------------------------------------------------- */
/* Function declarations: */

#ifdef ANSI_STD_C

extern int SDTGetComponentByIndexBase (DataQuantityInstance *dqi,
    int index, int nidx, int cidx, int DorA, ComponentUseList *CUList,
    ComponentAccumulationList *AList, int return_dbl,
    double *dval, float *fval, int *nret) ;

extern int SDTGetDataComponentByIndex (DataQuantityInstance *dqi,
    int index, int nidx, int cidx, ComponentUseList *CUList,
    ComponentAccumulationList *AList, int return_dbl,
    double *dval, float *fval, int *nret) ;

extern int SDTGetAssociatedComponentByIndex (DataQuantityInstance *dqi,
    int index, int nidx, int cidx, ComponentUseList *CUList,
    ComponentAccumulationList *AList, int return_dbl,
    double *dval, float *fval, int *nret) ;

extern int SDTGetTimeComponentByIndex (DataQuantityInstance *dqi,
    int index, int nidx, double *tval, int *nret) ;

extern int SDTGetTimeDataPointByIndex (DataQuantityInstance *dqi,
    int index, int nidx, int cidx, ComponentUseList *CUList,
    ComponentAccumulationList *AList, int return_dbl, double *tval,
    double *dval, float *fval, int *nret) ;

extern int SDTGetDataPointMinMaxByIndex (DataQuantityInstance *dqi,
    int index, int nidx, int cidx, ComponentUseList *CUList,
    ComponentAccumulationList *AList, int AccumFlg,
    double *MinVal, double *MaxVal, double *MinPos, double *MeanVal) ;

extern int SDTGetStandardOneDimensionalData (
    DataQuantityInstance *datadqi, 
    ComponentUseList *CUList, ComponentAccumulationList *AList,
    int start_index, int npts, int SkipPoints,
    double *tval, double *dval) ;

extern int SDTGetStandardTwoDimensionalData (
    DataQuantityInstance *datadqi,
    ComponentUseList *CUList, ComponentAccumulationList *AList,
    int start_index, int npts, int SkipPoints,
    double *tval, double *dval) ;

extern int SDTGetStandardThreeDimensionalData (
    DataQuantityInstance *datadqi,
    ComponentUseList *CUList, ComponentAccumulationList *AList,
    int start_index, int npts, int SkipPoints,
    double *tval, double *dval) ;

extern int SDTGetStandardOneDimensionalBinRanges (
    DataQuantityInstance *datadqi, ComponentUseList *CUList,
    ComponentAccumulationList *AList, int didx, int MaxBinRanges,
    int UseADesc, int *ADescFlg, int rdim, int NASub, int AxIdx,
    double *lowererange, double *uppererange,
    int *Nrng0, int *istart0, int *iend0) ;

extern int SDTGetStandardTwoDimensionalBinRanges (
    DataQuantityInstance *datadqi, ComponentUseList *CUList,
    ComponentAccumulationList *AList, int didx, int MaxBinRanges,
    int UseADesc, int *ADescFlg0, int *ADescFlg1, int rdim, int cdim,
    int *Ndesc, int *AxIdx,
    double *lowererange, double *uppererange,
    double *lowerprange, double *upperprange,
    int *Nrng0, int *istart0, int *iend0,
    int *Nrng1, int *istart1, int *iend1) ;

extern int SDTGetStandardThreeDimensionalBinRanges (
    DataQuantityInstance *datadqi, ComponentUseList *CUList,
    ComponentAccumulationList *AList, int didx, int MaxBinRanges,
    int UseADesc, int *ADescFlg0, int *ADescFlg1, int *ADescFlg2,
    int rdim, int cdim, int edim, int *Ndesc, int *AxIdx,
    double *lowererange, double *uppererange,
    double *lowerprange, double *upperprange,
    double *lowerzrange, double *upperzrange,
    int *Nrng0, int *istart0, int *iend0,
    int *Nrng1, int *istart1, int *iend1,
    int *Nrng2, int *istart2, int *iend2) ;

extern int SDTGetBinRangesForMultiDimensionSubComponent (
    DataQuantityInstance *datadqi, int SubComp,
    ComponentUseList *CUList, ComponentAccumulationList *AList,
    int didx, int MaxBinRanges,
    int *Nrng0, int *istart0, int *iend0) ;

extern int SDTReturnMDimAccumIndex (ComponentUseList *CUList,
    ComponentAccumulationList *AList, int SubDim) ;

extern int SDTReturnMDimSubDimensionBounds (ComponentUseList *CUList,
    ComponentAccumulationList *AList, int SubDim,
    int UseADesc, double *lval, double *hval, int *ADescFlg,
    int *NumASubDim, int *AxisSubDim) ;

extern double SDTComputeOneDimensionalAccumulation (
    DataQuantityInstance *dqi,
    StandardOneDimensionalHdr *hptr, void *data_vptr, int data_type,
    int rdim, int UseADesc, double *lrng0, double *urng0, int IStart0,
    int IEnd0, int *ecode) ;

extern double SDTComputeTwoDimensionalAccumulation (
    DataQuantityInstance *dqi,
    StandardTwoDimensionalHdr *hptr, void *data_vptr, int data_type,
    int *rdim, int *UseADesc, double **lrng, double **urng,
    int *IStart, int *IEnd, int *ecode) ;

extern double SDTComputeThreeDimensionalAccumulation (
    DataQuantityInstance *dqi,
    StandardThreeDimensionalHdr *hptr, void *data_vptr, int data_type,
    int *rdim, int *UseADesc, double **lrng, double **urng,
    int *IStart, int *IEnd, int *ecode) ;

extern double SDTAccumulateOneDimensional (
    StandardOneDimensionalHdr *hptr,
    void *data_vptr, int data_type, MDimVal *rmdv, int nrng,
    int *istart, int *iend) ;

extern double SDTAccumulateTwoDimensional (
    StandardTwoDimensionalHdr *hptr,
    void *data_vptr, int data_type, MDimVal *rmdv, MDimVal *cmdv,
    int nrng_row, int *istrow, int *iendrow,
    int nrng_col, int *istcol, int *iendcol) ;

extern double SDTAccumulateThreeDimensional (
    StandardThreeDimensionalHdr *hptr,
    void *data_vptr, int data_type, MDimVal *rmdv, MDimVal *cmdv,
    MDimVal *emdv, int nrng_row, int *istrow, int *iendrow,
    int nrng_col, int *istcol, int *iendcol, int nrng_ech,
    int *istech, int *iendech) ;

extern int SDTComputeTimeAxisIndices (DataQuantityInstance *timedqi,
    int time_cmp, int first_index, int last_index,
    int first_query, double zoom_start, double zoom_end,
    int max_points, int idx_in_comp, int *array_size,
    int *new_index, int *done_flag, int *total_data_pts) ;

extern int SDTDeleteSDTTimeDeltaIndexRanges (
    SDTTimeDeltaIndexRanges *srng) ;

extern int SDTClearSDTTimeDeltaIndexRanges (
    SDTTimeDeltaIndexRanges *srng) ;

extern int SDTPrintSDTTimeDeltaIndexRanges (
    SDTTimeDeltaIndexRanges *srng, FILE *ofp) ;

extern SDTTimeDeltaIndexRanges
    *SDTFindConstantTimeDeltaRanges (
    DataQuantityInstance *dqi, double STime, double ETime,
    int NPts, double DeltaT, int DltFlg, int *IRes) ;

extern int SDTComputeVSliceValues (DataQuantityInstance *dqip,
    SDTVrtSliceSpectraInfo *vsl_info,
    ComponentUseList *ulist, ComponentAccumulationList *alist,
    int *NPts, double **xarr, double **yarr,
    int *x_is_phys, int *linlogflg) ;

extern int SDTBuildArrayDescriptionOrderedList (
    DataQuantityInstance *dqip, int SubDim, int ArrDscIdx,
    AddrT *offsets, int Order, int VType, int SkipBadBins,
    ComponentUseList *ulist, ComponentAccumulationList *alist,
    int *LCnt, double **out_dval, int **out_idx,
    int *LinLogFlg) ;

extern int SDTReturnOneDimensionalBinRanges (DataQuantityInstance *dqip,
    int didx, int UseArrDesc, int NumRanges,
    ComponentUseList *ulist, ComponentAccumulationList *alist,
    int *SubDimFlg, int *NRows, int *istart, int *iend) ;

extern int SDTReturnTwoDimensionalBinRanges (DataQuantityInstance *dqip,
    int didx, int UseArrDesc, int NumRanges,
    ComponentUseList *ulist, ComponentAccumulationList *alist,
    int *SubDimFlg, int *NRows, int *NCols, int *istart0, int *iend0,
    int *istart1, int *iend1) ;

extern int SDTReturnThreeDimensionalBinRanges (DataQuantityInstance *dqip,
    int didx, int UseArrDesc, int NumRanges,
    ComponentUseList *ulist, ComponentAccumulationList *alist,
    int *SubDimFlg, int *NRows, int *NCols, int *NEchs,
    int *istart0, int *iend0, int *istart1, int *iend1,
    int *istart2, int *iend2) ;

extern int SDTGetLabelUnitsStrings (DataQuantityDescription *dqdp,
    int comp_idx, int subdim, int adesc,
    char **l_ptr, char **u_ptr) ;

extern SDTTSeriesSmpRateInput *SDTConstructSDTTSeriesSmpRateInput (
    int i_nskips, int i_consistency_check_type, double i_srate_fuzz,
    double i_max_t_diff, int i_min_consistent_pts,
    int i_show_only_constistent_regimes, int i_alg_code) ;

extern SDTTSeriesSmpRateInput *SDTCopyConstructSDTTSeriesSmpRateInput (
    SDTTSeriesSmpRateInput *i_input) ;

extern int SDTFillSDTTSeriesSmpRateInput (
    SDTTSeriesSmpRateInput *input, int i_nskips,
    int i_consistency_check_type, double i_srate_fuzz,
    double i_max_t_diff, int i_min_consistent_pts,
    int i_show_only_constistent_regimes, int i_alg_code) ;

extern int SDTCopySDTTSeriesSmpRateInput (
    SDTTSeriesSmpRateInput *src, SDTTSeriesSmpRateInput *dst) ;

extern int SDTDestructSDTTSeriesSmpRateInput (
    SDTTSeriesSmpRateInput *input) ;

extern int SDTClearSDTTSeriesSmpRateInput (
    SDTTSeriesSmpRateInput *input) ;

extern int SDTPrintSDTTSeriesSmpRateInput (
    SDTTSeriesSmpRateInput *input, FILE *ofp) ;

extern SDTTSeriesSmpRateRegime
    *SDTConstructSDTTSeriesSmpRateRegime () ;

extern SDTTSeriesSmpRateRegime
    *SDTCopyConstructSDTTSeriesSmpRateRegime (
	SDTTSeriesSmpRateRegime *ismp) ;

extern int SDTFillSDTTSeriesSmpRateRegime (
    SDTTSeriesSmpRateRegime *ismp) ;

extern int SDTCopySDTTSeriesSmpRateRegime (
    SDTTSeriesSmpRateRegime *ismp,
    SDTTSeriesSmpRateRegime *osmp) ;

extern int SDTDestructSDTTSeriesSmpRateRegime (
    SDTTSeriesSmpRateRegime *ismp) ;

extern int SDTClearSDTTSeriesSmpRateRegime (
    SDTTSeriesSmpRateRegime *ismp) ;

extern int SDTPrintSDTTSeriesSmpRateRegime (
    SDTTSeriesSmpRateRegime *ismp, FILE *ofp) ;

extern SDTTSeriesSmpRateRegimeList
    *SDTConstructSDTTSeriesSmpRateRegimeList (
    SDTTSeriesSmpRateInput *input) ;

extern SDTTSeriesSmpRateRegimeList *
    SDTCopyConstructSDTTSeriesSmpRateRegimeList (
        SDTTSeriesSmpRateRegimeList *ismpl) ;

extern int SDTFillSDTTSeriesSmpRateRegimeList (
    SDTTSeriesSmpRateRegimeList *ismpl,
    SDTTSeriesSmpRateInput *input) ;

extern int SDTCopySDTTSeriesSmpRateRegimeList (
    SDTTSeriesSmpRateRegimeList *ismpl,
    SDTTSeriesSmpRateRegimeList *osmpl) ;

extern int SDTDestructSDTTSeriesSmpRateRegimeList (
    SDTTSeriesSmpRateRegimeList *ismpl) ;

extern int SDTClearSDTTSeriesSmpRateRegimeList (
    SDTTSeriesSmpRateRegimeList *ismpl) ;

extern int SDTPrintSDTTSeriesSmpRateRegimeList (
    SDTTSeriesSmpRateRegimeList *ismpl, FILE *ofp) ;

extern SDTTSeriesSmpRateRegimeList *SDTQuerySamplingHistory (
    DataQuantityInstance *dqip, SDTTSeriesSmpRateInput *input,
    int *ecode) ;

extern SDTTimeDeltaIndexRanges *SDTModify_1_TimeDeltaIndexRanges (
    SDTTimeDeltaIndexRanges *i_rng, DataQuantityInstance *dqip,
    int dcomp, int nmin, int exact_flg, double deld) ;

extern SDTTimeDeltaIndexRanges *SDTModify_2_TimeDeltaIndexRanges (
    SDTTimeDeltaIndexRanges *i_rng, DataQuantityInstance *dqip,
    int dcomp, int nmin, double vmax, double vmin) ;

extern int SDTModify_3_TimeDeltaIndexRanges (
    SDTTimeDeltaIndexRanges *o_rng,
    double *ttags, double *dvals, double i_tdelta,
    int start_idx, int end_idx, int idx_offset) ;


#else

extern int SDTGetDataComponentByIndex () ;
extern int SDTGetTimeComponentByIndex () ;
extern int SDTGetDataPointMinMaxByIndex () ;
extern int SDTGetStandardOneDimensionalData () ;
extern int SDTGetTimeDataPointByIndex () ;
extern int SDTGetStandardTwoDimensionalData () ;
extern int SDTGetStandardThreeDimensionalData () ;
extern int SDTGetStandardOneDimensionalBinRanges () ;
extern int SDTGetStandardTwoDimensionalBinRanges () ;
extern int SDTGetStandardThreeDimensionalBinRanges () ;
extern int SDTGetBinRangesForMultiDimensionSubComponent () ;
extern int SDTReturnMDimAccumIndex () ;
extern int SDTReturnMDimSubDimensionBounds () ;
extern double SDTComputeOneDimensionalAccumulation () ;
extern double SDTComputeTwoDimensionalAccumulation () ;
extern double SDTComputeThreeDimensionalAccumulation () ;
extern double SDTAccumulateOneDimensional () ;
extern double SDTAccumulateTwoDimensional () ;
extern double SDTAccumulateThreeDimensional () ;
extern int SDTComputeTimeAxisIndices () ;
extern int SDTDeleteSDTTimeDeltaIndexRanges () ;
extern int SDTClearSDTTimeDeltaIndexRanges () ;
extern int SDTPrintSDTTimeDeltaIndexRanges () ;
extern SDTTimeDeltaIndexRanges
    *SDTFindConstantTimeDeltaRanges () ;
extern int SDTComputeVSliceValues () ;
extern int SDTBuildArrayDescriptionOrderedList () ;
extern int SDTReturnOneDimensionalBinRanges () ;
extern int SDTReturnTwoDimensionalBinRanges () ;
extern int SDTReturnThreeDimensionalBinRanges () ;
extern int SDTGetLabelUnitsStrings () ;
extern SDTTSeriesSmpRateInput
    *SDTConstructSDTTSeriesSmpRateInput () ;
extern SDTTSeriesSmpRateInput
    *SDTCopyConstructSDTTSeriesSmpRateInput () ;
extern int SDTFillSDTTSeriesSmpRateInput () ;
extern int SDTCopySDTTSeriesSmpRateInput () ;
extern int SDTDestructSDTTSeriesSmpRateInput () ;
extern int SDTClearSDTTSeriesSmpRateInput () ;
extern int SDTPrintSDTTSeriesSmpRateInput () ;
extern SDTTSeriesSmpRateRegime
    *SDTConstructSDTTSeriesSmpRateRegime () ;
extern SDTTSeriesSmpRateRegime
    *SDTCopyConstructSDTTSeriesSmpRateRegime () ;
extern int SDTFillSDTTSeriesSmpRateRegime () ;
extern int SDTCopySDTTSeriesSmpRateRegime () ;
extern int SDTDestructSDTTSeriesSmpRateRegime () ;
extern int SDTClearSDTTSeriesSmpRateRegime () ;
extern int SDTPrintSDTTSeriesSmpRateRegime () ;
extern SDTTSeriesSmpRateRegimeList
    *SDTConstructSDTTSeriesSmpRateRegimeList () ;
extern SDTTSeriesSmpRateRegimeList
    *SDTCopyConstructSDTTSeriesSmpRateRegimeList () ;
extern int SDTFillSDTTSeriesSmpRateRegimeList () ;
extern int SDTCopySDTTSeriesSmpRateRegimeList () ;
extern int SDTDestructSDTTSeriesSmpRateRegimeList () ;
extern int SDTClearSDTTSeriesSmpRateRegimeList () ;
extern int SDTPrintSDTTSeriesSmpRateRegimeList () ;
extern SDTTSeriesSmpRateRegimeList *SDTQuerySamplingHistory () ;

extern SDTTimeDeltaIndexRanges *SDTModify_1_TimeDeltaIndexRanges () ;
extern SDTTimeDeltaIndexRanges *SDTModify_2_TimeDeltaIndexRanges () ;
extern int SDTModify_3_TimeDeltaIndexRanges () ;

#endif

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* SDTDATAUTILITIES_H */

