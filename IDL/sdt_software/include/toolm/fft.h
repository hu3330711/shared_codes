/* fft.h */

#ifndef FFT_WAS_INCLUDED
#define FFT_WAS_INCLUDED

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* For SCCS */
#define SccsId_fft_h "@(#)fft.h	1.4, 08/18/97"

#ifdef ANSI_STD_C

extern void FFT(float *data, int nn, int isign) ;
extern void FFTtwo_real(float *data1, float *data2, float *fft1,
    float *fft2, int n) ;
extern void FFTreal(float *data, int n, int isign) ;
extern void FFTcorrel( float *data1, float *data2, int n, float *ans) ;

extern void dbl_FFT(double *data, int nn, int isign) ;
extern void dbl_FFTtwo_real(double *data1, double *data2, double *fft1,
    double *fft2, int n) ;
extern void dbl_FFTreal(double *data, int n, int isign) ;
extern void dbl_FFTcorrel( double *data1, double *data2, int n,
    double *ans) ;

extern boolean FFTpower( boolean (*dGet)( float *data), float p[], int m, boolean overlap);

/* Power spectrum estimate (mean square amplitude) see p 445, "spctrm"
   Uses Parzan (triangular) data windowing
   Reads in arbitrary long data array, and divides it into segments of
      length 2*m, takes the FFT of each and averages spectral
      estimate of each segment.
   Calls FFT k times with two segments of 2*m points each call, where
      number of data points = (2*k+1)*m points (overlap = TRUE)
                            = 4*k*m points (overlap = FALSE)

   Input:  dGet( &data) gets next element in arbitrary long data array, 
      returns FALSE when no more data ( data = 0.0 then)
   Input:  m = number of frequency points output (must be power of 2)
   Input:  overlap = TRUE to overlap segments (minimal spectral variance)
                   = FALSE to not overlap segments (minimal computer time)

   Output: p[j] = data's power at frequency (j-1) /(2*m), for j=1..m
   return FALSE if memory allocation failure
*/

extern float FFTperiodogram(float *data, int n);

/* "periodogram" estimate of the power spectrum of a real valued function 
    see page 438

   data[1..2*n] is a real array of length 2*n
   
   on output, data is replaced by the n+1 points:
      data[1] = P(0) = |C0|**2 / N**2
      data[k] = P(fk) = (|Ck|**2 + |CN-k|**2)/ N**2
      data[n+1] = P(fN/2) = |CN/2|**2 / N**2

      where N = 2n, fk = k/TN, T is sampling Period,
         Ck is Fourier coeeficient for fk

   return sum of all coefficients for normalization
 */

extern boolean dbl_FFTpower( boolean (*dGet)( double *data),
    double p[], int m, boolean overlap) ;
extern double dbl_FFTperiodogram(double *data, int n) ;


#else

extern void FFT() ;
extern void FFTtwo_real() ;
extern void FFTreal() ;
extern void FFTcorrel() ;

extern void dbl_FFT() ;
extern void dbl_FFTtwo_real() ;
extern void dbl_FFTreal() ;
extern void dbl_FFTcorrel() ;

extern boolean FFTpower( );
extern float FFTperiodogram( );

extern boolean dbl_FFTpower() ;
extern double dbl_FFTperiodogram() ;

#endif

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* FFT_WAS_INCLUDED */
