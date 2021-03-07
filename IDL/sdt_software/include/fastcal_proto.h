/************************************************************************/
/* fastcal_proto.h : Prototypes for all functions in fastcal.c,  	*/
/* fastcal_init.c, and fopen_fast_cal.c.				*/
/*									*/
/************************************************************************/
/* 			REVISION HISTORY				*/
/* DATE		NAME	COMMENT						*/
/*									*/
/* 06/23/95	REE	Initial version. 				*/
/* 07/27/95	REE	Modified to have fastcal_init return a pointer.	*/
/* 08/10/96	REE	Modified fastcal() - removed type_check.	*/
/* 08/10/96	REE	Modified fastcal_init() - added data_type.	*/
/* 08/20/96	REE	Added fastcal_12(), get_fastcal_12(), mag_fit(),*/
/* 08/20/96	 "	build_A_4(), bulid_M_4(), poly_fit_4(),		*/
/* 08/20/96	 "	and invert_matrix_4(), and polyfit_evaluate().	*/
/* 11/05/96	REE	Added fpa_evaluate(), fpa_highpass(), 		*/
/*			fpa_lowpass(), and fpa_cap.			*/
/* 11/06/96	REE	Added sfa_freqs().		 		*/
/* 11/06/96	REE	Added set_sfa_freqs().REMOVED Get_sfa_scale().	*/
/* 11/1/96	REE	Added fastcal_5_IDL().REMOVED Get_sfa_scale().	*/
/* 11/11/96	REE	Major rewrite of fastcal_12() MagDC.		*/
/* 			Removed: mag_time_OK(), mag_fit(), mag_cal(), 	*/
/* 			and mag_matrix_mult().			 	*/
/* 			Removed: bulid_A_4(), bulid_M_4(),		*/
/* 			poly_fit_4(), and invert_matrix_4().	 	*/
/* 11/12/96	REE	Added get_hsbm_smpl_rate().			*/
/* 04/07/97	KRB	Added free_fastcal().				*/
/* 05/13/97	KRB	Added get_wpc_smpl_rate(),lin_interp(),rotate() */
/* 08/20/97	KRB	get_wpc_smpl_rate() is now get_wpc_freq_scale()	*/
/************************************************************************/

#ifndef fastcal_proto
#define fastcal_proto

/* For use by SCCS: */
#define SccsId_fastcal_proto_h  "@(#)fastcal_proto.h	1.13, 02/19/04"

#include "fastcal_type.h"
#include "FastCalData_struc.h"

/* Prototypes from fastcal_init.c. */

void* 	fastcal_init(char* data_name, double epoch_time,
        	enum fastcal_data_type datatype);
void 	free_fastcal(void *fastcal_ptr);
int 	fastcal_init_switch(FILE *fp, int cal_type, fastcal_union *cal_ptr, 
                double *cal_time);
int 	fastcal_type_check(enum fastcal_data_type datatype, int cal_type ); 
int 	get_cal_header(FILE *fp, fastcal_header_type *header);
void 	fast_boomlength_init(double *boomlength, double epoch_time);
double 	fast_boomlength( double *boomlength_arr, int index );
int 	get_fast_boomlength(FILE *fp, double *boomlength, double *boom_time);
int 	get_double(double *d, FILE *fp);
int 	get_2_doubles(double *d1, double *d2, FILE *fp);
int 	get_3_doubles(double *d1, double *d2, double *d3, FILE *fp);
int 	get_int(int *i, FILE *fp);
int 	get_N_ints(FILE *fp, int n, ...);
int 	get_char(char *c, FILE *fp);
int 	get_fastcal_1(FILE *fp, fastcal_union *cal_ptr, double *cal_time);
int 	get_fastcal_2(FILE *fp, fastcal_union *cal_ptr, double *cal_time);
int 	get_fastcal_3(FILE *fp, fastcal_union *cal_ptr, double *cal_time);
int 	get_fastcal_4(FILE *fp, fastcal_union *cal_ptr, double *cal_time);
int 	get_fastcal_5(FILE *fp, fastcal_union *cal_ptr, double *cal_time);
int 	get_fastcal_6(FILE *fp, fastcal_union *cal_ptr, double *cal_time);
int 	get_fastcal_7(FILE *fp, fastcal_union *cal_ptr, double *cal_time);
int 	get_fastcal_8(FILE *fp, fastcal_union *cal_ptr, double *cal_time);
int 	get_fastcal_11(FILE *fp, fastcal_union *cal_ptr, double *cal_time);
int 	get_fastcal_12(FILE *fp, fastcal_union *cal_ptr, double *cal_time);
int	get_smooth_cofs(fastcal_union *cal_ptr);

/* Prototypes from fastcal.c. */

int 	fastcal(void *fastcal_ptr, void *raw_data_ptr,
            	  unsigned char *data_header_ptr, int array_size);
int 	fastcal_1(void *raw_data_ptr, unsigned char *data_header_ptr,
		  fastcal_struc_type *fc_ptr, int array_size);
int 	fastcal_2(void *raw_data_ptr, unsigned char *data_header_ptr,
		  fastcal_struc_type *fc_ptr, int array_size);
int 	fastcal_3(void *raw_data_ptr, unsigned char *data_header_ptr,
		  fastcal_struc_type *fc_ptr, int array_size);
int 	fastcal_4(void *raw_data_ptr, unsigned char *data_header_ptr,
		  fastcal_struc_type *fc_ptr, int array_size);
int 	fastcal_5(void *raw_data_ptr, unsigned char *data_header_ptr,
		  fastcal_struc_type *fc_ptr, int array_size);
int 	fastcal_5_IDL(void *raw_data_ptr, unsigned char *data_header_ptr,
	      	  fastcal_struc_type *fc_ptr, int array_size);
int 	fastcal_6(void *raw_data_ptr, unsigned char *data_header_ptr,
		  fastcal_struc_type *fc_ptr, int array_size);
int 	fastcal_7(void *raw_data_ptr, unsigned char *data_header_ptr,
		  fastcal_struc_type *fc_ptr, int array_size);
int 	fastcal_8(void *raw_data_ptr, unsigned char *data_header_ptr,
		  fastcal_struc_type *fc_ptr, int array_size);
int 	fastcal_11(void *raw_data_ptr, unsigned char *data_header_ptr,
		  fastcal_struc_type *fc_ptr, int array_size);
int 	fastcal_12(void *raw_data_ptr, unsigned char *data_header_ptr,
		  fastcal_struc_type *fc_ptr, int array_size);
void 	bbf_gain(int subtype2, double *gain, unsigned char *data_header_ptr);
void 	sfa_gain(int subtype2, double *gain, unsigned char *data_header_ptr);
int 	get_sfa_sweep_config(int word_index, unsigned char
	 	*data_header_ptr);
void 	set_sfa_freqs (SfaCalibrateBin *struc_ptr, 
		fastcal_struc_type_4 *cal_ptr, unsigned char *data_header_ptr);
void 	hsbm_gain(int subtype2, double *gain, unsigned char *data_header_ptr);
double	highGainFromMode(int gain_state_type, unsigned char *highGainMode, 
			unsigned char fmode);


/* Prototypes from fastcal_utils.c. */

void 	fastcal_err_message (fastcal_struc_type *fc_ptr, char *message);
int 	get_svy_smpl_rate(unsigned char *data_header_ptr);
int 	get_adc_smpl_rate(unsigned char *data_header_ptr);
int 	get_hsbm_smpl_rate(unsigned char *data_header_ptr);
double 	get_wpc_integ_time(unsigned char *data_header_ptr);
double 	polyfit_evaluate (double x, double *A, int nterms);
void 	fpa_evaluate (float *freq, int num_freqs, 
		double *cof, int num_cofs, float *result);
void 	fpa_highpass (float *freq, int num_freqs, double *cof, int num_cofs, 
		float *amp, float *phi);
void 	fpa_lowpass (float *freq, int num_freqs, double *cof, int num_cofs, 
 		float *amp, float *phi);
void 	fpa_cap (float *freq, int num_freqs, double cross_freq, 
		double cap_gain, float *amp, float *phi);
void 	sfa_freqs (int sfa_sweep_conf, double *freq_zero, double *freq_delta, 
                float *freq_min, float *freq_max);
void	srvy_despin_z (MagDC_calibrate_struct *data_ptr, 
		fastcal_struc_despin *despin_z, int i);
void	srvy_convol (MagDC_calibrate_struct *data_ptr, float *stored_x, 
		float *stored_y, float *stored_z, double *stored_t,
		double *cof_x, double *cof_y, double *cof_z,
		int num_cof, double time_cof, double dt, int i);
void	srvy_smooth (MagDC_calibrate_struct *data_ptr, unsigned char *dhptr, 
		fastcal_struc_convol_smooth *smooth_ptr, int i);
double	lin_interp (double x1, double t1, double x2, double t2, double x);
void	rotate (double x, double y, double theta, double *xp, double *yp) ;

/* Prototypes for fopen_fast_cal.c. */

FILE 	*fopen_fast_cal (char *data_name);
FILE 	*fopen_fastcal_wrap (char *data_name);

/* Prototypes for fa_fields_lib.c. */
IDL_long ff_time_align(int argc, void * argv[]);
int 	time_align_no_interp( double *time1, double *data1, int npts1,
		double *time2, double *data2, int npts2,
		double delt, int adjust, double ratio);
int 	time_align_interp(double *time1, double *data1, int npts1,
		double *time2, double *data2, int npts2, double gap);
IDL_long ff_find_bufs(int argc, void * argv[]);
IDL_long ff_filter(int argc, void * argv[]);
void 	two_pole_filter(double *x, double *t, int npts, 
        	two_pole_filt_struc *cofs, int high_pass, double dt_min);
void 	one_pole_filter(double *x, double *t, int npts, 
		two_pole_filt_struc *cofs, int high_pass, double dt_min);

#endif
