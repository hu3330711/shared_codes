/*  ------------------------------------------------------------ */
#ifndef convol1_h
#define convol1_h

/* SCCS ID string: */
#define SccsId_convol1_h "@(#)convol1.h	1.2, 05/30/04"

/* Library of "Convolution" routines of type "1".
 * #include <SMISL.h>
 */

#include <SnapOn.ANSI-C.h>
#include <stdio.h>
#include <math.h>
#include <memory.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/times.h>

#include <wtgen001.h>
#include <SDTDescription.h>
#include <SDTInstance.h>
#include <SDTDataUtilities.h>
#include <toolm/fft.h>

/* -------------------------------------------------------------- */
/* Structure to ease creation of different levels of convolutions: */
struct Convolution1Level_struct
    {
    int   n_names ;
    char  **names ;
    double *freq ;
    int    *even_odd ;
    int    *wrap_codes ;
    int    *reverse_order ;
    double *divisor ;
    } ;
typedef   struct Convolution1Level_struct  Convolution1Level ;

struct Convolution1TableList_struct
    {
    int   n_cascades ;
    Convolution1Level *cascades ;
    } ;
typedef   struct Convolution1TableList_struct  Convolution1TableList ;

/* -------------------------------------------------------------- */
/* Structure to read a convolution-definition input file. */
struct Convolution1Input_struct
    {
    int   n_levels ;
    StringList  *levels ;
    DoubleList  *freqs ;
    IntList  *even_odd ;
    IntList  *wrap_flg ;
    IntList  *reverse_order ;
    DoubleList  *divisors ;
    } ;
typedef   struct Convolution1Input_struct  Convolution1Input ;

/* -------------------------------------------------------------- */

/* -------------------------------------------------------------- */
int RunConvolution1 (char *ConvolutionTable, int method, int clip,
    DataQuantityInstance *idqi, DataQuantityInstance *odqi,
    int ncomps, int  *components,
    int t_nskips, int t_consist, double t_srate_fuzz,
    double t_max_t_diff, int t_min_pts, int t_show_only_consistent,
    int t_hist_alg_code, int *final_n_pts) ;

int Convert_SDT_to_WT_Sampling_History (
    SDTTSeriesSmpRateRegimeList *shst,
    WT_T_Time_Series_Sampling_Rate_Info *whst) ;

int Fill_WT_Convolution1_Controls (Convolution1Input *table,
    WT_T_Conv_Control_Info *wfo) ;

int Fill_WT_Convolution1_Table (Convolution1TableList *ilist,
    WT_T_Conv_Control_Info *wfo) ;

int Clear_WT_Convolution1_Table (WT_T_Conv_Control_Info *wfo) ;

int  ReadConvolution1Table (char *ConvolutionTable,
    char *SpaceCraftSubDir, Convolution1Input *cinput) ;

char *FindFIR (char *fname, char *SpaceCraftSubDir, char *tpath,
    int flag1) ;

int ConvoluteViaFFT (double *respns, int res_size, int res_type,
    double *data, int data_size, double *result, int *out_size) ;

#endif // convol1_h
