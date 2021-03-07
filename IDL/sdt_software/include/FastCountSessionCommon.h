#ifndef FAST_COUNT_SESSION_COMMON_H
#define FAST_COUNT_SESSION_COMMON_H

/* FastCountSessionCommon.h */
/* Used by both "FastDecom" and "FastCount": */

/* For SCCS */
#define SccsId_FastCountSessionCommon_h "@(#)FastCountSessionCommon.h	1.38, 05/14/01"

#include <SDTDescription.h>
#include <FastMonitors.h>

#define  MAX_MDIM_COUNTS                   20
#define  MAX_DSP_CHAN_FIELD_COUNTS         32

#define  MaxCountSessionIdSize             460
#define  MaxCountSessionVersionSize        16

/* The types of count sessions: */
#define FAST_INTEGRATION_AND_TEST_FILES    0
#define FAST_DATA_ANALYSIS_FILES           1

/* The ways in which fields mode is determined: */
#define FAST_FMODE_FROM_APID_1086              0
#define FAST_FMODE_FROM_FIELDS_PACKETS         1
#define FAST_FMODE_FROM_APID_1032              2

/* String Ids for fields mode determination: */
#define FMODE_UNKNOWN       "FModeFromUnknown"
#define FMODE_1086          "FModeFrom1086"
#define FMODE_PACKETS       "FModeFromPackets"
#define FMODE_1032          "FModeFrom1032"

/* The ways in which packets mode is determined: */
#define FAST_PMODE_FROM_APID_1086              0
#define FAST_PMODE_FROM_ESA_PACKETS            1
#define FAST_PMODE_FROM_ESA_AND_TEAMS_PACKETS  2
#define FAST_PMODE_FROM_APID_1024              3

/* String Ids for packets mode determination: */
#define PMODE_UNKNOWN            "PModeFromUnknown"
#define PMODE_1086               "PModeFrom1086"
#define PMODE_ESA_PACKETS        "PModeFromEsa"
#define PMODE_ESA_TEAMS_PACKETS  "PModeFromEsaTeams"
#define PMODE_1024               "PModeFrom1024"

/* The ways in which survey mode is determined: */
#define FAST_SURVEY_MODE_FROM_APID_1085        0
#define FAST_SURVEY_MODE_FROM_SURVEY_PACKETS   1

/* String Ids for fields mode determination: */
#define SMODE_UNKNOWN       "SModeFromUnknown"
#define SMODE_1085          "SModeFrom1085"
#define SMODE_FROM_PACKETS  "SModeFromPackets"

/* Ids for fast/slow survery modes: */
#define SURVEY_MODE_IS_SLOW           0
#define SURVEY_MODE_IS_FAST           1
#define SURVEY_MODE_IS_BACKORBIT      2
#define SURVEY_MODE_IS_UNDETERMINED   -1

/* Appended suffix to the count files corresponding to PDU files: */
#define PDU_COUNT_SUFFIX ".cnt"

/* The types of partition modes: */
#define PDU_FILE_MODE_ID                   "PDUFiles"
#define PRODUCTION_FILE_MODE_ID            "ProductionFiles"

#define FLIGHT_VERSION_UNKNOWN             "Unknown"

/* These are the Unix times (seconds since midnight GMT 70/01/01) at
 * midnight GMT of 93/01/01 and at midnight GMT of 2000/01/01:
 *
 * The program "test", in: 
 *
 *    polar3:/mydisks/home/jackv/cprog/convert_clock_to_unix_time
 *
 * computed these values.
 */
#define  UnixSecondsAt930101     725846400
#define  UnixSecondsAtYear2000   946684800

#define  FAST_START_JULIAN_DAY   2440001

/* The apid corresponding to index 0 in:  FastCountPartion->ApidCnt */
#define  APID_COUNT_OFFSET_INDEX  1024

#define  APID_COUNT_TOTAL_INDICES 100

/* This contains the starting apid index, in arrays, of the new apids 0 - 15
 * If based on 1024, this would be 1092.
 */
#define  FAST_OUT_OF_SEQ_IDX     68

#define  FAST_LAST_DATA_APID      1072

#define  FAST_DATA_APID_RANGE FAST_LAST_DATA_APID-APID_COUNT_OFFSET_INDEX+1

#define  NUM_HSK_APIDS            8

#define  MIN_APPID_LABEL          1024
#define  MAX_APPID_LABEL          1098

#define  MIN_STD_BURST_APID       1039
#define  MAX_STD_BURST_APID       1057

/* The time delta (in seconds) which distinguishes one standard burst
 * from another:
 */
#define  STD_BURST_TIME_DELTA     2.5

#define  PKT_HIST_HDR_A_SIZE      3000

/* ------------------------------------------------------------------- */
/* Structures: */

/* ------------------------------------------------------------------- */
/* Count Storage for 2D data: */

struct MDim2DCount_struct
    {
    int   nrow ;
    int   ncol ;
    int   cnt ;
    } ;
typedef struct MDim2DCount_struct MDim2DCount ;

/* ------------------------------------------------------------------- */
/* Count Storage for 3D data: */

struct MDim3DCount_struct
    {
    int   nrow ;
    int   ncol ;
    int   nech ;
    int   cnt ;
    } ;
typedef struct MDim3DCount_struct MDim3DCount ;

/* ------------------------------------------------------------------- */
/* Count Storage for Dsp Channel,Fields pairs.  Note that we use
 * this instead of actual Qty names in case the mapping of
 * channel/field pairs to physical quantities changes:
 */

struct DspChanFieldCnt_struct
    {
    int   chan ;
    int   field ;
    int   cnt ;
    } ;
typedef struct DspChanFieldCnt_struct DspChanFieldCnt ;

struct PktHdrHist_struct
    {
    int   idx ;
    int   seqno ;
    int   type ;
#ifdef MAYBE
    int   hour ;
    int   minute ;
    int   second ;
#endif
    double  etime ;
    unsigned char hbytes[14] ;
    } ;
typedef struct PktHdrHist_struct PktHdrHist ;

/* ------------------------------------------------------------------- */
/* Contains HSBM Burst information: */

struct HsbmBurstUnit_struct
    {
    /* If 0 -> then this unit is not timeable,
     * if 1 -> then the time-packet for this burst did, in fact,
     *         occur, so the burst is timeable.
     */
    int           TimeTagFlag ;

    /* Only meaningful if "TimeTagFlag" is 1:  the start time tag of
     * the HSBM burst unit.  Note that this is the start time of the
     * first actual packet in the burst and not the start time of the
     * theoretical first packet.  If the actual first packet is the
     * theretical first packet, then the two times are the same: 
     */
    double        STimeTag ;

    /* Only meaningful if "TimeTagFlag" is 1:  the end time tag of
     * the HSBM burst unit.  Note that this is the end time of the
     * last actual packet in the burst and not the end time of the
     * theoretical last packet.  If the actual last packet is the
     * theretical last packet, then the two times are the same: 
     * NOTE:  In fact, the only case where "TimeTagFlag" is even
     * on is when the actual and theoretical last packets are the same.
     */
    double        ETimeTag ;

    /* The total number of actual packets in the burst: */
    int           TPackets ;

    /* The following 3 integers are used to determine the starting
     * packet (and its cycle (0 - 4)) of this burst:
     *
     *   SGrp     is the group of 16384 packet sequence ids that the
     *            first packet in the burst comes in.  Note that we
     *            need this because packet sequences wrap at 16384.
     *            Note that "0" is the first group of 16384 in the
     *            dataset, etc.  At the first wrap of sequences numbers,
     *            SGrp becomes "1", and so on.
     *
     *   SNo      is the actual sequence number of the first packet
     *            in the burst.
     *
     *   SOffset  is the offset, in packet counts, from the theoretical
     *            start packet of the burst.  It is via this value
     *            that the cycle value of the first packet can be
     *            computed.  If "SOffset" is 0, then the cycle number
     *            of the first packet is "0", if "1", then the cycle
     *            number is "1", ..., if "SOffset" is 4, then the
     *            cycle number is "4", and if "SOffset" is 5, then
     *            the cycle number is "0" again, etc.
     */
    int          SGrp ;
    int          SNo ;
    int          SOffset ;

    /* These three integers give the same information for the last
     * packet of the burst, that "SGrp", "SNo", and "SOffset" give
     * for the first packet.  Note that "TimeTagFlag" will be "1"
     * only if "EOffset" is "0".
     */
    int          EGrp ;
    int          ENo ;
    int          EOffset ;

    /* Indicates which packets contain data for the 8 specific
     * HSBM data quantities as follows:
     *
     *   idx:     qty:
     *
     *     0      V1 - V2
     *     1      V1 - V4
     *     2      V7 - V8
     *     3      V5 - V8
     *     4      V5 - V6
     *     5      Mag3AC
     *     6      V3 - V4
     *     7      V9 - V10
     */
    int          SQty[8] ;

    /* The byte location, in the data set, of the
     * first byte of the first actual packet of this burst:
     */
    int          SByte ;

    /* The byte location, in the data set, of the last byte
     * of the last actual packet of this burst:
     */
    int          EByte ;

    /* The theoretical number of packets in this burst:  it is
     * computed as 20 * (2**bin_exp), where bin_exp is the number
     * in the data headers for just this purpose:
     */
    int          TheoreticalPackets ;

    /* The TimeDelta value for all of the packets in this burst: */
    int          TimeDelta ;

    /* These are not used for normal counting - they were added on
     * 2001/05/08 in order to help resolve the gain/calibration
     * problem that was found by R. Strangeway for HSBM.
     */

    /* Indicate what kind of processing/counting is taking place:
     *    0  ->  normal counting
     *    1  ->  packet header history
     */
    int  PType ;

    /* The total number of elements in "phist": */
    int  TotPHistElts ;

    /* The currently used number of elements in "phist": */
    int  NPHistElts ;

    /* Pointer to the array keeping track of the Hsbm headers for
     * this Hsbm burst unit:
     */
    PktHdrHist *phist ;
    } ;
typedef struct HsbmBurstUnit_struct HsbmBurstUnit ;

/* ------------------------------------------------------------------- */
/* Contains Standard Burst information, particularly the time spans
 * of bursts.  Note that the Apids covered by "standard" bursts are:
 *
 *   EEsa          1039
 *   IEsa          1040
 *   SEsa          1041, 1042, 1043, 1044, 1045, 1046
 *   Teams         1047
 *   ADC (fields)  1048, 1049, 1050, 1051, 1052, 1053, 1054, 1055
 *   SFA           1057
 *
 * Note that HSBM (apid 1058) is NOT part of standard bursts.
 */

struct StdBurstUnit_struct
    {
    /* Start time of burst: */
    double        STimeTag ;

    /* End time of burst: */
    double        ETimeTag ;

    /* List of packet counts, for the various apid types, within
     * this burst.  Note that:
     *     PacketCnt[0]   corresponds to Apid 1039
     *     PacketCnt[1]   corresponds to Apid 1040
     *     PacketCnt[2]   corresponds to Apid 1041
     *        .
     *        .
     *        .
     *     PacketCnt[18]  corresponds to Apid 1057
     *
     * Note that indices "17" and "19" have no corresponding packets
     * (1056 (WPC) and 1058 (HSBM) are not standard burst types)
     * and so are zero.
     */
    int   PacketCnt[20] ;
    } ;
typedef struct StdBurstUnit_struct StdBurstUnit ;

/* ------------------------------------------------------------------- */
/* This structure constains the necessary information for either a
 * "fields" or "esa" engineering mode period.  As of 95/06/06, it only
 * needed to contain the Epoch start and end times for each, as well
 * as the type (fields or esa):
 */
struct FastEngineeringPeriod_struct
    {
    int     Type ;         /* 0 -> fields,  1 -> Esa */
    double  STimeTag ;
    double  ETimeTag ;
    } ;
typedef struct FastEngineeringPeriod_struct FastEngineeringPeriod ;

/* ----------------------------------------------------------------- */

/* This contains all the information and data counts for a time
 * partition of a FastCountSession, when parsing a Count file:
 */
struct  FastCountPartition_struct
    {
    /* The time span for this File/DataBase unit: */
    TimeSpanDescription   TimeSpan ;

    /* These are the start and end times of the TimeSpan in Mission
     * Epoch Seconds:
     */
    double  StartMissionEpochSecs ;
    double  EndMissionEpochSecs ;

    /* Mode periods values: */
    int  SurveyMode ;
    int  PMode ;
    int  FMode ;

    /* The start and end bytes, in the data file, where the data for
     * this partition resides.  If unused or not available, these
     * are set to "-1":
     */
    long  SByteOverall ;
    long  EByteOverall ;

 /* These are the various counters which are needed: */

    /* Keeps count of Apids - note that index 0 of this array
     * corresponds to Apid:
     *
     *   APID_COUNT_OFFSET_INDEX
     *
     */
    int ApidCnt[APID_COUNT_TOTAL_INDICES] ;

    /* Keeps track of the number of CRC errors on a apid basis: */
    int ApidCRCCnt[APID_COUNT_TOTAL_INDICES] ;

    /* The start and end bytes, in the data file, where the data for
     * each type of Apid starts and ends.  If unused or unknown, these
     * are set to "-1":
     */
    long  SByte[APID_COUNT_TOTAL_INDICES] ;
    long  EByte[APID_COUNT_TOTAL_INDICES] ;

    /* --------- For the test Sine_Wave quantities: -----------*/
    int   cnt_1100 ;
    int   cnt_1101 ;
    int   cnt_1102 ;
    int   cnt_1103 ;
    int   cnt_1104 ;
    int   cnt_1105 ;

    /* Housekeeping counts: */
    int   HskCount[MONALLOC] ;

    /* Fields Survey: */

    int   SMPhase_FieldsSurvey0_cnt ;
    int   SMPhase_FieldsSurvey1_cnt ;
    int   SMPhase_FieldsSurvey2_cnt ;
    int   V9minusV10_V9_NE9_S_cnt ;
    int   V10_NE10_S_cnt ;
    int   LFF1_S_cnt ;
    int   LFF2_S_cnt ;

    int   V1minusV4_S_cnt ;
    int   V5minusV8_S_cnt ;
    int   V9minusV10_S_cnt ;
    int   V9_S_cnt ;
    int   NE9_S_cnt ;
    int   Mag1dc_S_cnt ;
    int   Mag2dc_S_cnt ;
    int   Mag3dc_S_cnt ;
    int   THERM_cnt ;

    int   DIFF2_S_cnt ;
    int   DIFF3_S_cnt ;
    int   DIFF6_S_cnt ;
    int   DIFF7_S_cnt ;
    int   V2_S_cnt ;
    int   NE2_S_cnt ;
    int   V1minusV2_S_cnt ;
    int   V1minusV3_S_cnt ;
    int   V3_S_cnt ;
    int   NE3_S_cnt ;
    int   V3minusV4_S_cnt ;
    int   V2minusV4_S_cnt ;
    int   V6_S_cnt ;
    int   NE6_S_cnt ;
    int   V5minusV6_S_cnt ;
    int   V5minusV7_S_cnt ;
    int   V7_S_cnt ;
    int   NE7_S_cnt ;
    int   V7minusV8_S_cnt ;
    int   V6minusV8_S_cnt ;

    int   Mag1ac_S_cnt ;
    int   Mag2ac_S_cnt ;
    int   Mag3ac_S_cnt ;
    int   V10_S_cnt ;
    int   NE10_S_cnt ;
    int   V4_S_cnt ;
    int   V8_S_cnt ;
    int   V1minusV4HG_S_cnt ;
    int   V1minusV2HG_S_cnt ;
    int   V5minusV8HG_S_cnt ;
    int   V3minusV4HG_S_cnt ;

  /* Apid15 quantities (begun in June 1999): */
    int   MagApid15_cnt ;

  /* Generic 16K ADC fields quantities: */
    int  ApId1048_cnt ;
    int  ApId1049_cnt ;
    int  ApId1050_cnt ;
    int  ApId1051_cnt ;
    int  ApId1052_cnt ;
    int  ApId1053_cnt ;

    int  ApId1054_w1_cnt ;
    int  ApId1054_w2_cnt ;
    int  ApId1054_w3_cnt ;
    int  ApId1054_w4_cnt ;

    int  ApId1055_w1_cnt ;
    int  ApId1055_w2_cnt ;
    int  ApId1055_w3_cnt ;
    int  ApId1055_w4_cnt ;

  /* Specific 16K fields quantities: */
    int  V1_16k_cnt ;
    int  V1minusV2_16k_cnt ;
    int  V1minusV2HG_16k_cnt ;
    int  V1minusV3_16k_cnt ;
    int  V1minusV4_16k_cnt ;
    int  V1minusV4HG_16k_cnt ;
    int  V1plusV4minusV5plusV8_16k_cnt ;
    int  V2_16k_cnt ;
    int  NE2_16k_cnt ;
    int  V2minusV4_16k_cnt ;
    int  V3_16k_cnt ;
    int  NE3_16k_cnt ;
    int  V3minusV4_16k_cnt ;
    int  V3minusV4HG_16k_cnt ;
    int  V4_16k_cnt ;
    int  V5_16k_cnt ;
    int  V5minusV6_16k_cnt ;
    int  V5minusV7_16k_cnt ;
    int  V5minusV8_16k_cnt ;
    int  V5minusV8HG_16k_cnt ;
    int  V6_16k_cnt ;
    int  NE6_16k_cnt ;
    int  V6minusV8_16k_cnt ;
    int  V7_16k_cnt ;
    int  NE7_16k_cnt ;
    int  V7minusV8_16k_cnt ;
    int  V8_16k_cnt ;
    int  V9_16k_cnt ;
    int  NE9_16k_cnt ;
    int  V9minusV10_16k_cnt ;
    int  Mag3AC_16k_cnt ;

    int  BBF1_V1minusV2_16k_cnt ;
    int  BBF1_V1minusV4_16k_cnt ;

    int  BBF2_V5minusV8_16k_cnt ;
    int  BBF2_V7minusV8_16k_cnt ;

    int  BBF3_V5minusV6_16k_cnt ;
    int  BBF3_Mag3AC_16k_cnt ;

    int  BBF4_V3minusV4_16k_cnt ;
    int  BBF4_V9minusV10_16k_cnt ;

    int  SFA4_V1minusV2_16k_cnt ;
    int  SFA4_V1minusV4_16k_cnt ;
    int  SFA4_V3minusV4_16k_cnt ;
    int  SFA4_V9minusV10_16k_cnt ;
    int  SFA4_TRK_16k_cnt ;

  /* Specific 4K fields quantities: */
    int  V1minusV2_4k_cnt ;
    int  V1minusV3_4k_cnt ;
    int  V1minusV4_4k_cnt ;
    int  V1plusV4minusV5plusV8_4k_cnt ;
    int  V2_4k_cnt ;
    int  NE2_4k_cnt ;
    int  V2minusV4_4k_cnt ;
    int  V3_4k_cnt ;
    int  NE3_4k_cnt ;
    int  V3minusV4_4k_cnt ;
    int  V5minusV6_4k_cnt ;
    int  V5minusV7_4k_cnt ;
    int  V5minusV8_4k_cnt ;
    int  V6_4k_cnt ;
    int  NE6_4k_cnt ;
    int  V6minusV8_4k_cnt ;
    int  V7_4k_cnt ;
    int  NE7_4k_cnt ;
    int  V7minusV8_4k_cnt ;
    int  V9_4k_cnt ;
    int  NE9_4k_cnt ;
    int  V9minusV10_4k_cnt ;
    int  V10_4k_cnt ;
    int  Mag1AC_4k_cnt ;
    int  Mag2AC_4k_cnt ;
    int  Mag3AC_4k_cnt ;
    int  GND_4k_cnt ;

    int  BBF3_V5minusV6_4k_cnt ;
    int  BBF3_Mag3AC_4k_cnt ;

    int  BBF4_V3minusV4_4k_cnt ;
    int  BBF4_V9minusV10_4k_cnt ;

  /* Esa: */

    int          numSesaBurst1041 ;
    MDim2DCount  SesaBurst1041[MAX_MDIM_COUNTS] ;
    int          numSesaBurst1042 ;
    MDim2DCount  SesaBurst1042[MAX_MDIM_COUNTS] ;
    int          numSesaBurst1043 ;
    MDim2DCount  SesaBurst1043[MAX_MDIM_COUNTS] ;
    int          numSesaBurst1044 ;
    MDim2DCount  SesaBurst1044[MAX_MDIM_COUNTS] ;
    int          numSesaBurst1045 ;
    MDim2DCount  SesaBurst1045[MAX_MDIM_COUNTS] ;
    int          numSesaBurst1046 ;
    MDim2DCount  SesaBurst1046[MAX_MDIM_COUNTS] ;

    int          numSesaSurvey ;
    MDim2DCount  SesaSurvey[MAX_MDIM_COUNTS] ;

    int          numEesaBurst ;
    MDim2DCount  EesaBurst[MAX_MDIM_COUNTS] ;

    int          numEesaSurvey ;
    MDim2DCount  EesaSurvey[MAX_MDIM_COUNTS] ;

    int          numIesaBurst ;
    MDim2DCount  IesaBurst[MAX_MDIM_COUNTS] ;

    int          numIesaSurvey ;
    MDim2DCount  IesaSurvey[MAX_MDIM_COUNTS] ;

  /* Dsp: */
    int  DspCoherency1 ;
    int  DspCoherency2 ;
    int  DspData1 ;
    int  DspData2 ;
    int  DspChanCnt[16] ;
    int              numDspChanFields ;
    DspChanFieldCnt  DspChanFields[MAX_DSP_CHAN_FIELD_COUNTS] ;

  /* BBf: */
    int  ApId1035_w1_cnt ;
    int  ApId1035_w2_cnt ;
    int  ApId1035_w3_cnt ;
    int  ApId1035_w4_cnt ;
    int  bbf_v1minusv2_cnt ;
    int  bbf_v1minusv4_cnt ;
    int  bbf_v3minusv4_cnt ;
    int  bbf_v5minusv8_cnt ;
    int  bbf_v7minusv8_cnt ;
    int  bbf_v5minusv6_cnt ;
    int  bbf_v9minusv10_cnt ;
    int  bbf_mag3ac_cnt ;

  /* SFA (ave == index 0,  burst == index 1): */
    int  Sfa_w1_spec[2] ;
    int  Sfa_w2_spec[2] ;
    int  Sfa_w3_spec[2] ;
    int  Sfa_w4_spec[2] ;
    int  Sfa_V1minusV2[2] ;
    int  Sfa_V1minusV4[2] ;
    int  Sfa_V3minusV4[2] ;
    int  Sfa_V5minusV6[2] ;
    int  Sfa_V5minusV8[2] ;
    int  Sfa_V7minusV8[2] ;
    int  Sfa_V9minusV10[2] ;
    int  Sfa_Mag3AC[2] ;
    int  Sfa_w4_tseries[2] ;

  /* Hfq: */
    int  hfq_pd12 ;
    int  hfq_pd13 ;
    int  hfq_pd14 ;
    int  hfq_pd23 ;
    int  hfq_pd24 ;
    int  hfq_pd34 ;

    int  hfq_frq1 ;
    int  hfq_frq2 ;
    int  hfq_frq3 ;
    int  hfq_frq4 ;

    int  hfq_trkfrq ;

    int  hfq_V1minusV2_V5minusV8 ;
    int  hfq_V1minusV2_V7minusV8 ;
    int  hfq_V1minusV4_V5minusV8 ;
    int  hfq_V1minusV4_V7minusV8 ;

    int  hfq_V1minusV2_V5minusV6 ;
    int  hfq_V1minusV2_Mag3AC ;
    int  hfq_V1minusV4_V5minusV6 ;
    int  hfq_V1minusV4_Mag3AC ;

    int  hfq_V1minusV2_V3minusV4 ;
    int  hfq_V1minusV2_V9minusV10 ;
    int  hfq_V1minusV4_V3minusV4 ;
    int  hfq_V1minusV4_V9minusV10 ;

    int  hfq_V5minusV8_V5minusV6 ;
    int  hfq_V5minusV8_Mag3AC ;
    int  hfq_V7minusV8_V5minusV6 ;
    int  hfq_V7minusV8_Mag3AC ;

    int  hfq_V5minusV8_V3minusV4 ;
    int  hfq_V5minusV8_V9minusV10 ;
    int  hfq_V7minusV8_V3minusV4 ;
    int  hfq_V7minusV8_V9minusV10 ;

    int  hfq_V5minusV6_V3minusV4 ;
    int  hfq_V5minusV6_V9minusV10 ;
    int  hfq_Mag3AC_V3minusV4 ;
    int  hfq_Mag3AC_V9minusV10 ;

    int  hfq_V1minusV2 ;
    int  hfq_V1minusV4 ;
    int  hfq_V5minusV8 ;
    int  hfq_V7minusV8 ;
    int  hfq_V5minusV6 ;
    int  hfq_Mag3AC ;
    int  hfq_V3minusV4 ;
    int  hfq_V9minusV10 ;

  /* Wpc: */
    int  WPC_Esa_1 ;
    int  WPC_Esa_2 ;
    int  WPC_Esa_3 ;
    int  WPC_Esa_4 ;

  /* Hsbm: */
    int  hsbm_V1minusV2 ;
    int  hsbm_V1minusV4 ;
    int  hsbm_V3minusV4 ;
    int  hsbm_V5minusV6 ;
    int  hsbm_V5minusV8 ;
    int  hsbm_V7minusV8 ;
    int  hsbm_V9minusV10 ;
    int  hsbm_Mag3AC ;

  /* Teams: */

    int          numTeamsSurveyHO ;
    MDim3DCount  TeamsSurveyHOArr[MAX_MDIM_COUNTS] ;

    int          numTeamsSurveyHe ;
    MDim3DCount  TeamsSurveyHeArr[MAX_MDIM_COUNTS] ;

    int          numTeamsBurst ;
    MDim3DCount  TeamsBurstArr[MAX_MDIM_COUNTS] ;

    int          numTeamsHiMass ;
    MDim3DCount  TeamsHiMassArr[MAX_MDIM_COUNTS] ;

    int          numTeamsPoleHO ;
    MDim3DCount  TeamsPoleHOArr[MAX_MDIM_COUNTS] ;

    int          numTeamsPoleHe ;
    MDim3DCount  TeamsPoleHeArr[MAX_MDIM_COUNTS] ;

  /* Teams Hk (apid 1061): */
    /* The 20 specific qtys are counted: */
    int  TeamsHk[20] ;

  /* Esa Hk (apid 1062): */
    /* The 16 specific qtys are counted for each esa stack: */
    int  EsaHk_1[16] ;
    int  EsaHk_2[16] ;
    int  EsaHk_3[16] ;
    int  EsaHk_4[16] ;

  /* Trigger Info (apid 1065): */
    int     TrigFSMode ;
    int     TrigFSAlg ;
    int     TrigMinFLevel ;
    int     TrigMaxFLevel ;
    double  TrigAvgFLevel ;
    int TriggerRecords_cnt ;

    } ;
typedef  struct  FastCountPartition_struct  FastCountPartition ;

/* ----------------------------------------------------------------- */

/* This contains all the information and data counts for a file or
 * a data base time span while parsing a Count file:
 */
struct  FastCountSession_struct
    {
    char  id[MaxCountSessionIdSize + 2] ;

    char  version[MaxCountSessionVersionSize + 2] ;
    char  flight_version[MaxCountSessionVersionSize + 2] ;

    /* The time span for this File/DataBase unit: */
    TimeSpanDescription   TimeSpan ;

    /* These are the start and end times of the TimeSpan in Mission
     * Epoch Seconds:
     */
    double  StartMissionEpochSecs ;
    double  EndMissionEpochSecs ;

    /* Indicates whether this session is based on production data
     * (value set to:  FAST_DATA_ANALYSIS_FILES) or on I&T Pdu-type
     * files (value set to:  FAST_INTEGRATION_AND_TEST_FILES)
     */
    int     PartitionMode ;

    /* Various partition-determination modes:
     * As of 95/03/01, these are only relevant when
     * "PartitionMode" is FAST_DATA_ANALYSIS_FILES.
     *
     *    FieldsModeAlg:
     *       How Fields mode was determined - can have the following
     *       values:
     *           FAST_FMODE_FROM_APID_1086
     *           FAST_FMODE_FROM_FIELDS_PACKETS
     *
     *    ParticlesModeAlg:
     *       How Particles mode was determined - can have the following
     *       values:
     *           FAST_PMODE_FROM_APID_1086
     *           FAST_PMODE_FROM_ESA_PACKETS
     *           FAST_PMODE_FROM_ESA_AND_TEAMS_PACKETS
     *
     *    SurveyModeAlg:
     *       How Survey mode was determined - can have the following
     *       values:
     *           FAST_SURVEY_MODE_FROM_APID_1085
     *           FAST_SURVEY_MODE_FROM_SURVEY_PACKETS
     */
    int     FieldsModeAlg ;
    int     ParticlesModeAlg ;
    int     SurveyModeAlg ;

    /* In the simple, I&T case of partitions based solely on time,
     * the length of time for a partition.  Note that the data
     * analysis version will partition on the basis of Field and
     * Particle Mode periods.
     */
    double  SecondsPerPartition ;

    int     FirstTimeTagFlag ;

    /* Keeps count of Apids - note that index 0 of this array
     * corresponds to Apid:
     *
     *   APID_COUNT_OFFSET_INDEX
     *
     */
    int ApidCnt[APID_COUNT_TOTAL_INDICES] ;

    /* Keeps track of the number of CRC errors on a apid basis: */
    int ApidCRCCnt[APID_COUNT_TOTAL_INDICES] ;

    /* The start and end bytes, in the data file, where the data for
     * each type of Apid starts and ends.  If unused or unknown, these
     * are set to "-1":
     */
    long  SByte[APID_COUNT_TOTAL_INDICES] ;
    long  EByte[APID_COUNT_TOTAL_INDICES] ;

    /* This list of HSBM bursts in this data set.  Note that this
     * is not kept on a partition-by-partition basis, but on
     * the entire session:
     */
    int  NumberHsbmBursts ;
    HsbmBurstUnit  *HsbmBurstArray ;

    /* This list of Standard bursts in this data set.  Note that this
     * is not kept on a partition-by-partition basis, but on
     * the entire session:
     */
    int  NumberStdBursts ;
    StdBurstUnit   *StdBurstArray ;

    /* This list of Fields Engineeing Periods in this data set.  Note
     * that this is not kept on a partition-by-partition basis, but on
     * the entire session:
     */
    int  NumberFieldsEngineeringPer ;
    FastEngineeringPeriod   *FieldsEngPer ;

    /* This list of Esa Engineeing Periods in this data set.  Note
     * that this is not kept on a partition-by-partition basis, but on
     * the entire session:
     */
    int  NumberEsaEngineeringPer ;
    FastEngineeringPeriod   *EsaEngPer ;

    /* This is the number of FastCountPartition's: */
    int  NumberPartitions ;

    /* This is the array of FastCountPartition's: */
    FastCountPartition  *Partitions ; 
    } ;
typedef  struct  FastCountSession_struct  FastCountSession ;

/* -------------------------------------------------------------------- */
/* Export arrays and variables: */
extern int  ApidNumberList[APID_COUNT_TOTAL_INDICES] ;

/* -------------------------------------------------------------------- */
/* Function declarations: */

FastCountSession *ConstructFastCountSession (char *identifier,
    int partition_mode, int fmode_alg, int pmode_alg, int smode_alg,
    double secs_per_partition) ;

int InitFastCountSession (FastCountSession *fsess, char *identifier,
    int partition_mode, int fmode_alg, int pmode_alg, int smode_alg,
    double secs_per_partition) ;

int DestructFastCountSession (FastCountSession *fsession) ;

int ClearFastCountSession (FastCountSession *fsession) ;

int CopyFastCountSession (FastCountSession *dest, FastCountSession *src) ;

int FastCountComparePartitions (void *obj1, void *obj2) ;

FastCountPartition *ConstructFastCountPartition (
    double mss_stime, double mss_etime, int *ecode) ;

int InitFastCountPartition (FastCountPartition *fsess,
    double mss_stime, double mss_etime, int *ecode) ;

int DestructFastCountPartition (FastCountPartition *fpartition) ;

int ClearFastCountPartition (FastCountPartition *fpartition) ;

int CopyFastCountPartition (FastCountPartition *dest,
   FastCountPartition *src) ;

/* Public Count File Parsing routines: */

FILE  *init_FastCount_session (char *fname) ;

int cntparse (void) ;

FastCountSession  *ReturnFastCountSession() ;

/* HsbmBurst routines: */

HsbmBurstUnit *ConstructHsbmBurstUnit (int ptype, int in_TimeTagFlag,
    double in_STimeTag, double in_ETimeTag, int in_TPackets,
    int in_SGrp, int in_SNo, int in_SOffset,
    int in_EGrp, int in_ENo, int in_EOffset,
    int *in_SQty, int in_SByte, int in_EByte,
    int in_TheoreticalPackets, int in_TimeDelta) ;

int FillHsbmBurstUnit (HsbmBurstUnit *hunit, int ptyype,
    int in_TimeTagFlag,
    double in_STimeTag, double in_ETimeTag, int in_TPackets,
    int in_SGrp, int in_SNo, int in_SOffset,
    int in_EGrp, int in_ENo, int in_EOffset,
    int *in_SQty, int in_SByte, int in_EByte,
    int in_TheoreticalPackets, int in_TimeDelta) ;

int CopyHsbmBurstUnit (HsbmBurstUnit *dest, HsbmBurstUnit *src) ;

int DestructHsbmBurstUnit (HsbmBurstUnit *hunit) ;

int ClearHsbmBurstUnit (HsbmBurstUnit *hunit) ;

int PrintHsbmBurstUnit (HsbmBurstUnit *hunit, FILE *ofp) ;

int CompareHsbmBurstUnit (void *obj1, void *obj2) ;

int ComputeStartTimeHsbmBurstUnit (int BinExp, double ETime, int SGrp,
    int SNo, int SOffset, int EGrp, int ENo, int EOffset, int GrpSize,
    double TDelta, double *STime) ;

int AddPHdrToHsbmBurstUnit (HsbmBurstUnit *hunit, int seqno,
    int type, double orb_start_epoch_time, double epoch_time,
    unsigned char *dptr) ;

StdBurstUnit *ConstructStdBurstUnit (double in_STimeTag,
    double in_ETimeTag, int *in_PacketCnt) ;

int FillStdBurstUnit (StdBurstUnit *bunit,
    double in_STimeTag, double in_ETimeTag, int *in_PacketCnt) ;

int CopyStdBurstUnit (StdBurstUnit *dest, StdBurstUnit *src) ;

int DestructStdBurstUnit (StdBurstUnit *bunit) ;

int ClearStdBurstUnit (StdBurstUnit *bunit) ;

int PrintStdBurstUnit (StdBurstUnit *bunit, FILE *ofp) ;

int CompareStdBurstUnit (void *obj1, void *obj2) ;

FastEngineeringPeriod *ConstructFastEngineeringPeriod (double in_STimeTag,
    double in_ETimeTag, int in_Type) ;

int FillFastEngineeringPeriod (FastEngineeringPeriod *eunit,
    double in_STimeTag, double in_ETimeTag, int in_Type) ;

int CopyFastEngineeringPeriod (FastEngineeringPeriod *dest,
    FastEngineeringPeriod *src) ;

int DestructFastEngineeringPeriod (FastEngineeringPeriod *eunit) ;

int ClearFastEngineeringPeriod (FastEngineeringPeriod *eunit) ;

int PrintFastEngineeringPeriod (FastEngineeringPeriod *eunit, FILE *ofp) ;

int CompareFastEngineeringPeriod (void *obj1, void *obj2) ;

int GetApidPIndex (int apid) ;

int IsApidCountable (int apid) ;

#endif /* FAST_COUNT_SESSION_COMMON_H */
/* -------------------------------------------------------------------- */
