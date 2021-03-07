/*
 ***********************************************************************
 *
 *     General.UCB.wt.scimod.001.h
 *
 *     Author: Winston Teitler.
 *
 ***********************************************************************
 *
 *     General C header for SDT Science Modules.
 *
 ***********************************************************************
 *
 *     @(#)General.UCB.wt.scimod.001.h	1.27    07/19/07    UCB SSL
 *
 ***********************************************************************
 */





/*
 *======================================================================
 *======================================================================
 */





#ifndef  GENERAL_UCB_WT_SCIMOD_001_H
#define  GENERAL_UCB_WT_SCIMOD_001_H





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
 *----------------------------------------------------------------------
 *
 *     ALL SPACECRAFT library header.
 *
 *----------------------------------------------------------------------
 */



#include <wt_ALL_S_C_001.h>



/*
 *----------------------------------------------------------------------
 *
 *     SDT ANSI-C Science Module interface header.
 *
 *----------------------------------------------------------------------
 */



#include <SnapOn.ANSI-C.h>





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
 *     Averages.
 *
 *----------------------------------------------------------------------
 */



/*
 *     An SDT data quantity that is a "short format" average is a time
 *     series (Standard data type) and consists of the following
 *     components:
 *
 *     1) Time.
 *        Double.
 *
 *     2) Number of input data points.
 *        Float.
 *
 *     3) 1 or more average values, one for each input component.
 *        Float.
 *
 *
 *
 *     An SDT data quantity that is a "long format" average is a time
 *     series (Standard data type) and consists of the following
 *     components:
 *
 *     1) Time.
 *        Double.
 *
 *     2) Begin time of averaging interval
 *        Double.
 *
 *     3) End time of averaging interval
 *        Double.
 *
 *     4) Number of input data points.
 *        Double.
 *
 *     5) 1 or more average values, one for each input component.
 *        Float.
 */



#define  WT_SM_AVG_FMT_SHORT   (1)     /* code for short format avg. */

#define  WT_SM_AVG_FMT_LONG    (2)     /* code for long format avg. */



#define  WT_SM_SHORT_AVG_F_NPT (0)     /* idx. special flt. no. pts. */

#define  WT_SM_N_SHORT_AVG_F   (1)     /* no. of special floats */



#define  WT_SM_LONG_AVG_D_BEG  (0)     /* idx. special dbl. beg time */

#define  WT_SM_LONG_AVG_D_END  (1)     /* idx. special dbl. end time */

#define  WT_SM_LONG_AVG_D_NPT  (2)     /* idx. special dbl. no. pts. */

#define  WT_SM_N_LONG_AVG_D    (3)     /* no. of special doubles */



/*
 *----------------------------------------------------------------------
 *
 *     Basic values.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Output stream for messages.
 */

#define  WT_SM_MESSAGES        (WT_O_MSSG)



/*
 *     Output stream for dump of output data points.
 */

#define  WT_SM_DUMP            (WT_O_DUMP)



/*
 *     Minimum number of allocated output data points.
 */

#define  WT_SM_MIN_O_BUFF_SIZE (2)



/*
 *     Maximum number of inputs.
 */

#define  WT_SM_MAX_N_INPUTS    (20)



/*
 *     Number of basic sub-directories for special configuration files.
 */

#define  WT_SM_N_CONF_SUB_DIR      (3)


/*
 *     Basic sub-directories for special configuration files.
 */

#define  WT_SM_CONF_SUB_DIR_001    "sdt_cfg/scm/Special_Config"

#define  WT_SM_CONF_SUB_DIR_002    "sdt_cfg/scm"

#define  WT_SM_CONF_SUB_DIR_003    "sdt_cfg"



/*
 *----------------------------------------------------------------------
 *
 *     Codes to get versions of input matrices.
 *
 *----------------------------------------------------------------------
 */



#define  WT_SM_GET_MAT_PLAIN   (1)     /* get only "plain" matrix */

#define  WT_SM_GET_MAT_TRANSP  (2)     /* get only transposed matrix */

#define  WT_SM_GET_MAT_BOTH    (3)     /* get "plain" and transposed */



/*
 *----------------------------------------------------------------------
 *
 *     Codes to set parameters for data quantities.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Set the final value from the user value.
 */

#define  WT_SM_PARM_USR        (1)



/*
 *     Set the final value by copying the user value from another
 *     parameter.
 */

#define  WT_SM_PARM_COPY_USR   (2)



/*
 *     Set the final value by copying the final value from another
 *     parameter.
 */

#define  WT_SM_PARM_COPY_VAL   (3)



/*
 *----------------------------------------------------------------------
 *
 *     "Computed" files.
 *
 *----------------------------------------------------------------------
 */



#define  WT_COMP_I_H           (1)     /* write the initial header */

#define  WT_COMP_F_H           (2)     /* write the final header */



/*
 *----------------------------------------------------------------------
 *
 *     Data directories for SDT.
 *
 *----------------------------------------------------------------------
 */



/*
 *     POLAR.
 */

#define  WT_SDT_DATA_DIR_POL_001                                       \
             "/disks/gpc1/raid1/sdt/PolarData"



/*
 *     CLUSTER.
 */

#define  WT_SDT_DATA_DIR_CLU_001                                       \
             "/disks/gpc1/raid1/sdt/ClusterCommissioning"





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
 *     DataBaseResponse and Science Module time span information.
 *
 *----------------------------------------------------------------------
 */



/*
 *     DataBaseResponse and time span information.
 */


   struct wt_sci_mod_DBR_and_time_span_struct


     {


/*
 *     Pointer to the DataBaseResponse information.
 */

       DataBaseResponse *DBR_ptr ;


/*
 *     Time span, with the start date from SDT (DBR TimeSpan
 *     Year/DayOfYear).
 *
 *     The year, day of year, month, day in SDT are documented to have
 *     the proper values (e.g., January 1 is day 1).
 */

       WT_T_Time_Span time_span_sdt ;


/*
 *     Times of day, with the start date from SDT.
 */

       WT_T_Time_Of_Day time_of_day_beg_sdt ;

       WT_T_Time_Of_Day time_of_day_end_sdt ;


/*
 *     Time span, with the start date/time normalized.
 *
 *     This allows for the possibility that the start date/time in SDT
 *     may not be normalized.
 */

       WT_T_Time_Span time_span_norm ;


/*
 *     Times of day, with the start date/time normalized.
 */

       WT_T_Time_Of_Day time_of_day_beg_norm ;

       WT_T_Time_Of_Day time_of_day_end_norm ;


/*
 *     Offset to add to the JDN in time_span_sdt, to get the JDN for the
 *     start date as defined by SDT (DBR TimeSpan StartJulianDay).
 *
 *     This allows for the possibility of a different definition of JDN
 *     in SDT.
 */

       long int jdn_offset_sdt ;


/*
 *     Offset to add to the JDN in time_span_norm, to get the JDN in
 *     time_span_sdt.
 */

       long int jdn_offset_norm ;


/*
 *     Offset to add to a time in time_span_norm, to get the
 *     corresponding time in time_span_sdt.
 */

       double time_offset_norm ;


     } ;


   typedef
       struct wt_sci_mod_DBR_and_time_span_struct
       WT_T_Sci_Mod_DBR_And_Time_Span ;



/*
 *----------------------------------------------------------------------
 *
 *     Input data quantities.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Counts of input data points.
 */


   struct wt_sci_mod_input_point_counts_struct


     {


       int n_pts_used ;        /* no. of input data points used */

       int n_pts_avail ;       /* no. of input data points available */


     } ;


   typedef
       struct wt_sci_mod_input_point_counts_struct
       WT_T_Sci_Mod_Input_Point_Counts ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Parameters for an integer scalar input quantity.
 */


   struct wt_sci_mod_int_scalar_parm_struct


     {


       int input_needed ;      /* flag for input needed */

       int input_idx ;         /* input index */

       double dropout_time ;   /* dropout time */

       int limits_flag ;       /* flag for limits */

       int lower_limit ;       /* lower limit */

       int upper_limit ;       /* upper limit */


/*
 *     Flag for interpolation at intermediate points.
 *
 *     If true, the value at an intermediate point is to be determined
 *     by linear interpolation (followed by rounding).
 *
 *     If false, the value at an intermediate point is to be determined
 *     by replicating the value at the closest available data point.
 */

       int interp_flag ;


       int comp_idx ;          /* index for the scalar data component */


     } ;


   typedef
       struct wt_sci_mod_int_scalar_parm_struct
       WT_T_Sci_Mod_Int_Scalar_Parm ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Parameters for a real scalar input quantity.
 */


   struct wt_sci_mod_real_scalar_parm_struct


     {


       int input_needed ;      /* flag for input needed */

       int input_idx ;         /* input index */

       double dropout_time ;   /* dropout time */

       int limits_flag ;       /* flag for limits */

       double lower_limit ;    /* lower limit */

       double upper_limit ;    /* upper limit */


/*
 *     Flag for interpolation at intermediate points.
 *
 *     If true, the value at an intermediate point is to be determined
 *     by linear interpolation.
 *
 *     If false, the value at an intermediate point is to be determined
 *     by replicating the value at the closest available data point.
 */

       int interp_flag ;


       int comp_idx ;          /* index for the scalar data component */


     } ;


   typedef
       struct wt_sci_mod_real_scalar_parm_struct
       WT_T_Sci_Mod_Real_Scalar_Parm ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Parameters for a vector input quantity.
 */


   struct wt_sci_mod_vector_parm_struct


     {


       int input_needed ;      /* flag for input needed */

       int input_idx ;         /* input index */

       double dropout_time ;   /* dropout time */


/*
 *     Flag for interpolation at intermediate points.
 *
 *     If true, the value at an intermediate point is to be determined
 *     by linear interpolation.
 *
 *     If false, the value at an intermediate point is to be determined
 *     by replicating the value at the closest available data point.
 */

       int interp_flag ;


       int comp_idx[DIM_3] ;   /* indices for the vector components */


     } ;


   typedef
       struct wt_sci_mod_vector_parm_struct
       WT_T_Sci_Mod_Vector_Parm ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Parameters for a matrix input quantity.
 */


   struct wt_sci_mod_matrix_parm_struct


     {


       int input_needed ;      /* flag for input needed */

       int input_idx ;         /* input index */

       int transpose ;         /* transpose flag */

       int get_mat_code ;      /* code to get the matrix versions */

       double dropout_time ;   /* dropout time */


/*
 *     Flag for interpolation at intermediate points.
 *
 *     If true, the value at an intermediate point is to be determined
 *     by linear interpolation.
 *
 *     If false, the value at an intermediate point is to be determined
 *     by replicating the value at the closest available data point.
 */

       int interp_flag ;


/*
 *     Indices for the matrix data components.
 *
 *     Must correspond to the matrix elements in row-major order (column
 *     subscript varies fastest).
 */

       int comp_idx[N_MAT_ELEM_3] ;


     } ;


   typedef
       struct wt_sci_mod_matrix_parm_struct
       WT_T_Sci_Mod_Matrix_Parm ;



/*
 *----------------------------------------------------------------------
 *
 *     Parameters for data quantities.
 *
 *----------------------------------------------------------------------
 */



/*
 *     These type definitions are for situations in which some important
 *     parameter values may apply to more than 1 parameter, and we want
 *     to avoid having to specify the same parameter values multiple
 *     times.
 *
 *
 *
 *     The user still needs to specify all the parameter values, but
 *     some can be dummy values to be ignored.
 */



/*
 *     Integer parameter for a data quantity.
 */


   struct wt_sci_mod_int_parm_for_data_qty_struct


     {


       int usr ;               /* parameter value entered by the user */

       int val ;               /* final parameter value */


     } ;


   typedef
       struct wt_sci_mod_int_parm_for_data_qty_struct
       WT_T_Sci_Mod_Int_Parm_For_Data_Qty ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Double parameter for a data quantity.
 */


   struct wt_sci_mod_dbl_parm_for_data_qty_struct


     {


       double usr ;            /* parameter value entered by the user */

       double val ;            /* final parameter value */


     } ;


   typedef
       struct wt_sci_mod_dbl_parm_for_data_qty_struct
       WT_T_Sci_Mod_Dbl_Parm_For_Data_Qty ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Character parameter for a data quantity.
 */


   struct wt_sci_mod_char_parm_for_data_qty_struct


     {


       char *usr ;             /* parameter value entered by the user */

       char *val ;             /* final parameter value */


     } ;


   typedef
       struct wt_sci_mod_char_parm_for_data_qty_struct
       WT_T_Sci_Mod_Char_Parm_For_Data_Qty ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Vector parameter for a data quantity.
 *
 *     The vector is referred to as 1 parameter, even though it really
 *     contains several parameter values.
 */


   struct wt_sci_mod_vec_parm_for_data_qty_struct


     {


       double usr[DIM_3] ;     /* parameter value entered by the user */

       double val[DIM_3] ;     /* final parameter value */


     } ;


   typedef
       struct wt_sci_mod_vec_parm_for_data_qty_struct
       WT_T_Sci_Mod_Vec_Parm_For_Data_Qty ;



/*
 *----------------------------------------------------------------------
 */



/*
 *     Matrix parameter for a data quantity.
 *
 *     The matrix is referred to as 1 parameter, even though it really
 *     contains several parameter values.
 */


   struct wt_sci_mod_mat_parm_for_data_qty_struct


     {


/*
 *     Parameter value entered by the user.
 */

       double usr[DIM_3][DIM_3] ;


/*
 *     Final parameter value.
 */

       double val[DIM_3][DIM_3] ;


     } ;


   typedef
       struct wt_sci_mod_mat_parm_for_data_qty_struct
       WT_T_Sci_Mod_Mat_Parm_For_Data_Qty ;





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
 *     Data directories for SDT.
 *
 *----------------------------------------------------------------------
 */



/*
 *     Monthly directories.
 */

static char *WT_A_sdt_data_dir_month_001[] =


  {


    "01_January",

    "02_February",

    "03_March",

    "04_April",

    "05_May",

    "06_June",

    "07_July",

    "08_August",

    "09_September",

    "10_October",

    "11_November",

    "12_December",


  } ;





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
 *     DQIs for Science Modules.
 *
 *----------------------------------------------------------------------
 */



   int
       General_Get_Sci_Mod_Input_DQI_001(int idx,
           DataQuantityInstance **dqi_ptr_ptr) ;


   int
       General_Get_Sci_Mod_Output_DQI_001(
           DataQuantityInstance **dqi_ptr_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Data types.
 *
 *----------------------------------------------------------------------
 */



   int
       General_Convert_Data_Type_Code_001(int i_code,
           int *o_code_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     DataBaseResponse and Science Module time span information.
 *
 *----------------------------------------------------------------------
 */



   int
       General_GetDBRAndTimeSpan(WT_T_Sci_Mod_DBR_And_Time_Span *p) ;


   int
       General_GetDataDate(int *year_ptr, int *month_ptr, int *day_ptr,
           int *yearDay_ptr, long int *jdn_ptr, long int *mjn_ptr,
           long int *jdnStart_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Dump output.
 *
 *----------------------------------------------------------------------
 */



   int
       General_DumpFloat(FILE *stream,
           const WT_T_Sci_Mod_DBR_And_Time_Span *p, int n,
           const double *time_ptr, float *value_ptr) ;


   int
       General_DumpInt(FILE *stream,
           const WT_T_Sci_Mod_DBR_And_Time_Span *p, int n,
           const double *time_ptr, int *value_ptr) ;


   int
       General_DumpMatrix(FILE *stream,
           const WT_T_Sci_Mod_DBR_And_Time_Span *p, int n,
           const double *time_ptr, double **value_ptr_ptr) ;


   int
       General_DumpVector(FILE *stream,
           const WT_T_Sci_Mod_DBR_And_Time_Span *p, int n,
           const double *time_ptr, float **value_ptr_ptr) ;


   int
       General_Dump_Long_Avg_N_Float(FILE *stream,
           int n_data, const WT_T_Sci_Mod_DBR_And_Time_Span *p,
           int n, const double *time_ptr,
           double **long_avg_ptr_ptr, float **value_ptr_ptr) ;


   int
       General_Dump_ND_Double_NF_Float(FILE *stream,
           int n_d_data, int n_f_data,
           const WT_T_Sci_Mod_DBR_And_Time_Span *p, int n,
           const double *time_ptr, double **d_value_ptr_ptr,
           float **f_value_ptr_ptr) ;


   int
       General_Dump_N_Float(FILE *stream,
           int n_data, const WT_T_Sci_Mod_DBR_And_Time_Span *p,
           int n, const double *time_ptr,
           float **value_ptr_ptr) ;


   int
       General_Dump_Short_Avg_N_Float(FILE *stream,
           int n_data, const WT_T_Sci_Mod_DBR_And_Time_Span *p,
           int n, const double *time_ptr,
           float **short_avg_ptr_ptr, float **value_ptr_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Get input data.
 *
 *----------------------------------------------------------------------
 */



   int
       General_GetDoubleSetFromFloat(
           const WT_T_Sci_Mod_Real_Scalar_Parm *parm_ptr,
           DataQuantityInstance **i_dqi_ptr_ptr,
           WT_T_Time_Tagged_Double *i_point_ptr, int *point_index_ptr,
           WT_T_Sci_Mod_Input_Point_Counts *i_counts_ptr,
           int *end_flag_ptr) ;


   int
       General_GetFloatSet(
           const WT_T_Sci_Mod_Real_Scalar_Parm *parm_ptr,
           DataQuantityInstance **i_dqi_ptr_ptr,
           WT_T_Time_Tagged_Float *i_point_ptr, int *point_index_ptr,
           WT_T_Sci_Mod_Input_Point_Counts *i_counts_ptr,
           int *end_flag_ptr) ;


   int
       General_GetIntSet(const WT_T_Sci_Mod_Int_Scalar_Parm *parm_ptr,
           DataQuantityInstance **i_dqi_ptr_ptr,
           WT_T_Time_Tagged_Int *i_point_ptr, int *point_index_ptr,
           WT_T_Sci_Mod_Input_Point_Counts *i_counts_ptr,
           int *end_flag_ptr) ;


   int
       General_GetMatrixSet(const WT_T_Sci_Mod_Matrix_Parm *parm_ptr,
           DataQuantityInstance **i_dqi_ptr_ptr,
           WT_T_Time_Tagged_Matrix *i_point_ptr, int *point_index_ptr,
           WT_T_Sci_Mod_Input_Point_Counts *i_counts_ptr,
           int *end_flag_ptr) ;


   int
       General_GetVectorSet(const WT_T_Sci_Mod_Vector_Parm *parm_ptr,
           DataQuantityInstance **i_dqi_ptr_ptr,
           WT_T_Time_Tagged_Vector *i_point_ptr, int *point_index_ptr,
           WT_T_Sci_Mod_Input_Point_Counts *i_counts_ptr,
           int *end_flag_ptr) ;


   int
       General_Get_Float_001(
           const WT_T_Time_Series_Legit_Data_Points *pts_ptr,
           double t, double drop, const float *value_ptr,
           int interp,
           double *result_ptr, int *found_ptr) ;


   int
       General_Get_Vector_001(
           const WT_T_Time_Series_Legit_Data_Points *pts_ptr,
           double t, double drop, float **vec_ptr,
           int interp,
           double *result_ptr, int *found_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Parameters for data quantities.
 *
 *----------------------------------------------------------------------
 */



   int
       General_Get_Param_Alg_001(int n_parm,
           int *k_parm_ptr,
           int *usr_ptr) ;


   int
       General_Get_Param_Char_001(int n_parm,
           int *k_parm_ptr,
           char *usr_ptr) ;


   int
       General_Get_Param_Dbl_001(int n_parm,
           int *k_parm_ptr,
           double *usr_ptr) ;


   int
       General_Get_Param_Int_001(int n_parm,
           int *k_parm_ptr,
           int *usr_ptr) ;


   int
       General_Get_Param_Matrix_001(int n_parm,
           int *k_parm_ptr,
           double usr_ptr[DIM_3][DIM_3]) ;


   int
       General_Get_Param_Vector_001(int n_parm,
           int *k_parm_ptr,
           double *usr_ptr) ;


   int
       General_Set_Param_Char_001(int code,
           const WT_T_Sci_Mod_Char_Parm_For_Data_Qty
               *i_parm_ptr,
           WT_T_Sci_Mod_Char_Parm_For_Data_Qty *o_parm_ptr) ;


   int
       General_Set_Param_Dbl_001(int code,
           const WT_T_Sci_Mod_Dbl_Parm_For_Data_Qty
               *i_parm_ptr,
           WT_T_Sci_Mod_Dbl_Parm_For_Data_Qty *o_parm_ptr) ;


   int
       General_Set_Param_Int_001(int code,
           const WT_T_Sci_Mod_Int_Parm_For_Data_Qty
               *i_parm_ptr,
           WT_T_Sci_Mod_Int_Parm_For_Data_Qty *o_parm_ptr) ;


   int
       General_Set_Param_Matrix_001(int code,
           const WT_T_Sci_Mod_Mat_Parm_For_Data_Qty
               *i_parm_ptr,
           WT_T_Sci_Mod_Mat_Parm_For_Data_Qty *o_parm_ptr) ;


   int
       General_Set_Param_Vector_001(int code,
           const WT_T_Sci_Mod_Vec_Parm_For_Data_Qty
               *i_parm_ptr,
           WT_T_Sci_Mod_Vec_Parm_For_Data_Qty *o_parm_ptr) ;



/*
 *----------------------------------------------------------------------
 *
 *     Time series.
 *
 *----------------------------------------------------------------------
 */



   int
       General_Get_DQI_Sampling_Rate_Info_001(
           int t_nskips, int t_consist, double t_srate_fuzz,
           double t_max_t_diff, int t_min_pts,
           int t_show_only_consistent, int t_hist_alg_code,
           DataQuantityInstance *dqi_ptr,
           WT_T_Time_Series_Sampling_Rate_Info *p) ;



/*
 *----------------------------------------------------------------------
 *
 *     Workspaces.
 *
 *----------------------------------------------------------------------
 */



   int
       General_Find_Config_File_001(const char *file_name,
           const DataQuantityInstance *dqi_ptr,
           int *found_ptr, char *o_sub_dir, char *file_path) ;



/*
 *----------------------------------------------------------------------
 *
 *     Write basic values from Science Module.
 *
 *----------------------------------------------------------------------
 */



   int
       General_WriteDataPointCounts(FILE *stream,
           int n_inputs,
           const WT_T_Sci_Mod_Input_Point_Counts *i_counts_ptr,
           int n_o_curr) ;


   int
       General_WriteTimeSpan(FILE *stream,
           const WT_T_Sci_Mod_DBR_And_Time_Span *p) ;





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





#endif   /* GENERAL_UCB_WT_SCIMOD_001_H */
