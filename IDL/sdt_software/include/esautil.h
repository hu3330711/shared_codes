/*
 * The Esa (FAST) SCM Support Module:
 */
#ifndef ESAUTIL_H
#define ESAUTIL_H

#define SccsId_EsaUtil_h  "@(#)esautil.h	1.3, 03/03/97"


#ifdef  __cplusplus
   extern  "C"
   {
#endif


/* ------------------------------------------------------------------ */
/* Constants: */

/* ------------------------------------------------------------------ */
/* This structure holds spin and magnetometer phase information from
 * a packet data header:
 */
struct  euPhaseHdr_struct
    {
    int  SpinNumber ;
    int  SpinPhase ;
    int  MagPhase ;
    } ;
typedef struct  euPhaseHdr_struct  euPhaseHdr ;

/* ------------------------------------------------------------------ */
/* This is the main structure for sesa burst data headers: */
struct  SesaBurstHdr_struct
    {
    euPhaseHdr  SMPhase ;

    /* The spectrograph number (0, ... , 5): */
    int       sg_num ;

    int       sc ;
    float     ttag ;

    int       marray_hdr_idx ;     /* Index of the multi-dim array hdr
				    * for the first full multi-dim array
				    * in this packet:
				    */

    /* This is used only if we organize the multi-dimensional array
     * hdr list so that one hdr entry can be used for more than one
     * mdim array (in this case, the mdim-array hdr entry contains
     * the time tag of the first mdim array it serves, a count
     * of consecutive arrays that it serves, a macro delta time which
     * gives the delta time between consecutive mdim arrays, a micro
     * delta time which gives the delta time between energy levels
     * within each mdim array that it serves, and the other things.
     * As of 94/1/27, we have not implemented the mdim array hdrs
     * in this way.
     *
     * If implemented, "marray_hdr_subcnt" indicates which subcount
     * within "marray_hdr_idx" the first mdim array in the packet
     * corresponds to.  The timetag of this mdim array would be
     *
     *     marray_headers[marray_hdr_idx].time +
     *         (macro_delta_time * marray_hdr_subcnt) ;
     *
     * The only advantage of organizing the mdim array hdrs this
     * way is that it would save space particularly if the mdim
     * arrays are "small" (1 x SESA_BLOCK_SIZE for instance).
     */
    int       marray_hdr_subcnt ;

    /* This value holds the number of SESA_BLOCK_SIZE-byte blocks (which
     * which occur at the beginning of this packet but belong to a sweep
     * begun in the previous packet:
     */
    int       blocks_belonging_to_old_sweep ;

    /* This value holds the number of SESA_BLOCK_SIZE-byte blocks (which
     * expands to SESA_BLOCK_SIZE 2-byte values) which are left in the
     * unfilled sweep the end of the packet, when the packet is decoded.
     * Note that when the "sc" field is 2, then this value is zero since
     * the third packet in a cycle never contains a partial sweep
     * at its end.
     */
    int       blocks_remaining_in_pending_sweep ;

    /* Indicates how many seconds per 16-byte block for the packet: */ 
    double    secs_per_block ;

    /* The spin_rate of the spacecraft (in seconds) as computed for this
     * packet:
     */
    double    spin_rate ;

    /* This holds (on exit for this packet) the actual location of
     * true available data slots to stuff data into.  Note that, if
     * time-tagging for a packet cannot be done while the packet is
     * being decommutated (this can happed if this is the first packet
     * of the session or if there is a significant data gap from the
     * preceding packet), then we still have to fill in the data, but
     * we must leave the DQI "In" or "CurrentPoints" index to allow
     * plotting of only those points which are time-tagged.  Therefore,
     * the decommutator uses this index to know that the data points
     * "dqi->In or dqi->CurrentPoints" to "non_ttaged_data_index" are
     * already in use but need time-tagging, when the next packet comes
     * in.  At that point, the appropriate DQI header indices can be
     * reset forward.
     *
     * Also note that this field is meaningful only if
     * "blocks_remaining_in_pending_sweep" is non-zero.
     */
    int       non_ttaged_data_index ;

    /* Saves where the sweeps in this packet begin: */
    int       data_indices[64] ;

    int       nenergy ;
    int       dwell ;
    int       accum ;
    int       diagnostic_flag ;
    int       sweep_step ;
    int       increment ;    /* Same thing as "step_size": */
    int       nbins ;
    char      dither_mode ;
    char      calibrate_mode ;
    char      top_energy[6] ;
    char      delta_energy[6] ;
    } ;
typedef struct  SesaBurstHdr_struct  SesaBurstHdr ;

/* ------------------------------------------------------------------ */
/* This is the main structure for sesa survey data headers: */
struct  SesaSurveyHdr_struct
    {
    euPhaseHdr  SMPhase ;
    int       sg_num ;
    int       sc ;

    /* Note that this time-tag is for the first average that
     * starts in the packet.  Only when sc == 0, does this
     * actually correspond to the true start of the packet.
     */
    float     ttag ;

    int       marray_hdr_idx ;
    int       marray_hdr_subcnt ;
    int       blocks_belonging_to_old_sweep ;
    int       blocks_remaining_in_pending_sweep ;
    double    secs_per_block ;
    double    spin_rate ;
    int       non_ttaged_data_index ;
    int       data_indices[64] ;

    int       nenergy ;
    int       dwell ;
    int       accum ;
    int       diagnostic_flag ;
    int       despun ;
    int       sweep_step ;
    int       increment ;    /* Same thing as "step_size": */
    int       nbins ;
    int       nsweep ;
    char      dither_mode ;
    char      calibrate_mode ;
    char      top_energy[6] ;
    char      delta_energy[6] ;

    /* which array this packet header belongs too 
    */
    int       array_idx ;
    } ;
typedef struct  SesaSurveyHdr_struct  SesaSurveyHdr ;

/* ------------------------------------------------------------------ */
/* This is the main structure for eesa, iesa burst data headers: */
struct  EIesaBurstHdr_struct
    {
    euPhaseHdr  SMPhase ;

    /* The spectrometer type (0 -> eesa, 1 -> iesa) */
    int       type ;

    int       sc ;
    float     ttag ;

    int       sweep_mode ;
    int       retrace ;
    char      top_energy ;

    int       direction ;
    int       amplitude ;
    char      calibrate_mode ;
    char      low_energy ;
    int       dwell ;
    int       accum ;

    int       nenergy ;
    int       nbins ;

    /* Added 94/03/24 */
    int       sp ;

    int       marray_hdr_idx ;
    int       marray_hdr_subcnt ;
    int       blocks_belonging_to_old_sweep ;
    int       blocks_remaining_in_pending_sweep ;
    double    secs_per_block ;
    double    spin_rate ;
    int       non_ttaged_data_index ;
    int       data_indices[32] ;
    } ;
typedef struct  EIesaBurstHdr_struct  EIesaBurstHdr ;

/* This is the main structure for eesa, iesa survey data headers: */
struct  EIesaSurveyHdr_struct
    {
    euPhaseHdr  SMPhase ;

    /* The EIesa type (0 -> eesa, 1 -> iesa) */
    int       type ;

    int       sc ;
    float     ttag ;

    int       sweep_mode ;
    int       retrace ;
    char      top_energy ;

    int       direction ;
    int       amplitude ;
    char      calibrate_mode ;
    char      low_energy ;
    int       dwell ;
    int       accum ;

    /* Added 94/03/23 */
    int       mode ;

    int       nenergy_mode ;
    int       despun ;
    int       sweeps_per_average ;

    /* Note that E and I esa survey are the only ESA packets which have
     * a variable data block size (32 or 64):
     */
    int       blocks_per_packet ;

    int       nenergy ;
    int       nangle ;
    int       nbins ;

    int       marray_hdr_idx ;
    int       marray_hdr_subcnt ;
    int       blocks_belonging_to_old_sweep ;
    int       blocks_remaining_in_pending_sweep ;
    double    secs_per_block ;
    double    spin_rate ;
    int       non_ttaged_data_index ;
    int       data_indices[32] ;

    /* which array this packet header belongs too 
    */
    int       array_idx ;
    } ;
typedef struct  EIesaSurveyHdr_struct  EIesaSurveyHdr ;

/* ------------------------------------------------------------------ */
/* Global Variables and Arrays: */

/* ------------------------------------------------------------------ */
/* Function Declarations: */

extern int  sesaBurstDecodeDataHdr (unsigned char *dptr,
    SesaBurstHdr *hptr, int apid, double ttag) ;

extern int  sesaSurveyDecodeDataHdr (unsigned char *dptr,
    SesaSurveyHdr *hptr, int apid, double ttag) ;

extern int  eiesaBurstDecodeDataHdr (unsigned char *dptr,
    EIesaBurstHdr *hptr, int apid, double ttag) ;

extern int  eiesaSurveyDecodeDataHdr (unsigned char *dptr,
    EIesaSurveyHdr *hptr, int apid, double ttag) ;

extern int  EUextractSMPhase (unsigned char *dptr, euPhaseHdr *phptr) ;


#ifdef  __cplusplus
   }
#endif


#endif /* ESAUTIL_H */
