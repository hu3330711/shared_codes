
/* ------------------------------------------------------------- */
/*
 * efw_dcom.h
 *
 * These are the declarations required for decommutating the EFW
 * instrument data from the Cluster spacecraft.
 *
 */

#ifndef EFW_DCOM_H
#define EFW_DCOM_H

/* Here is the SCCS ID Keyword string 
 * (see page 99 of "Programming Utilities and Libraries").
 */
#define SccsId_efw_dcom_h "@(#)efw_dcom.h	1.37, 12/03/06"

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <math.h>

#ifdef  DOS_OS
#include <io.h>
#include <dos.h>
#endif

/* ------------------------------------------------------------- */
/* Constants. */

#ifdef OUT

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef NULL
#define NULL 0
#endif

/* The following definition is used to cover and uncover "extern". */
#ifdef TLM_GLB
#define hextern
#else
#define hextern extern
#endif

#endif /* OUT */

#define SF12 0xa1	/* SFRSTAT Boom_12 ready */
#define SF34 0xa3	/* SFRSTAT Boom_34 ready */ 
#define V1ANG 128+32	/* Sun Sensor to Boom1 angle (in 256 degs)*/
#define V3ANG V1ANG-128	/* V34 fits start 180 degrees out of phase*/

/* Value of unsigned 32 bit quantity - this is the value that having
 * the 33'rd bit (i.e. bit 32) ON and bits 0-31 OFF would have -
 * this is used for efw-clock computations:
 */
#define  EfwFull32BitValue            4294967296.0

/* This is the time between consecutive burst samples (1/36000) */
#define  EfwBurstVectorTDelta         0.0000277778

/* Chirp end-of-record value (an eor needs several in a row): */
#define  EfwEndOfChirpRecordId         0x7BBF

/* Default EFW boom distance (meters) at nominal full extension:
 * defined:  F. Mozer  00/09/18
 */
#define    EFW_DEF_BOOM_LEN              88.0

/* 2003/07/11:  The boom distance between probes 2 and 3: */
#define    EFW_DEF_BOOM_LEN_32           (EFW_DEF_BOOM_LEN/1.41421)

/* The number of degrees BEFORE the following SunPulse, that an
 * on-board SpinFit12 sampling period ended (135 - 26.2):
 */
#define    EFW_SFIT12_ANG_OFFSET         108.8

/* The number of degrees after the start of a SpinFit12 sampling
 * period to wait for the start of a SpinFi34 sampling period:
 */
#define    EFW_SFIT12_TO_SFIT34_ANG      180.0

/* The size of exec history buffers in the struct:
 *
 *    efw_exec_history
 *
 */
#define    EFW_EXEC_VERSION_HISTORY_SIZE  25

/* Cluster Spacecraft code. */
#define    EFW_SPACECRAFT                3

/* EFW burst header status codes - these are used in the
 * "status" word of the "tlm_start_burst_evt" structure.
 */

/* Burst header is correct: */
#define    EFW_BURST_HDR_STATUS_OK       0

/* Burst header is correctable, including exact timing: */
#define    EFW_BURST_HDR_STATUS_WLEV_1   1

/* Burst header is correctable, but exact timing is lost: */
#define    EFW_BURST_HDR_STATUS_WLEV_2   2

/* Burst header is NOT usable: */
#define    EFW_BURST_HDR_STATUS_ERROR    -1

/* The Julian Day that SC1, Probe 1 failed (2001/12/28): */
#define    EFW_SC1_P1_FAILED             2452272

/* The Julian Day that SC3, Probe 1 failed (2002/07/29): */
#define    EFW_SC3_P1_FAILED             2452485

/* Burst header error/warning message indices (into the array
 * of strings "EfwBurstHdrErrMsg"):
 */
#define    EFW_BHDR_MSG_ALL_OK           0
#define    EFW_BHDR_MSG_FREQ_CODE        1
#define    EFW_BHDR_MSG_QTY_LIST         2
#define    EFW_BHDR_MSG_SMPL_MODE        3
#define    EFW_BHDR_MSG_START_TIME       4
#define    EFW_BHDR_MSG_TOO_EARLY        5
#define    EFW_BHDR_MSG_LOST_IN_MPACK    6
#define    EFW_BHDR_MSG_REPEATED_BURST   7

#define    EFW_BHDR_NMSGS                8

/* EFW telemetry modes: */
#define    EFW_TLM_NORMAL_MODE           0
#define    EFW_TLM_TAPE_1_MODE           1
#define    EFW_TLM_TAPE_2_MODE           2
#define    EFW_TLM_TAPE_3_MODE           3

/* EFW Sweep codes (first byte in sweep header): */
#define    EFW_BIAS_SWEEP_BOOM_12        0xe1
#define    EFW_BIAS_SWEEP_BOOM_34        0xe3

/* EFW telemetry decom methodology */
#define    EFW_RETURN_ALL_EVENTS         0
#define    EFW_RETURN_ALL_BUT_BURST      1
#define    EFW_RETURN_ONLY_BURST         2

/* EFW block time spans: */
#define    EfwNormalBlockTimeSpan        1.0
#define    EfwTape1BlockTimeSpan         0.1
#define    EfwTape2BlockTimeSpan         0.1
#define    EfwTape3BlockTimeSpan         0.1

/* FDM data block constants: */
#define    FDM_PBACK_BYTE                0
#define    FDM_PBACK_MASK                0x80
#define    FDM_PBACK_SHIFT               7

#define    FDM_BURST_STATUS_BYTE         0
#define    FDM_BURST_STATUS_MASK         0x70
#define    FDM_BURST_STATUS_SHIFT        4

#define    FDM_MAIN_BURST_BYTE           0
#define    FDM_MAIN_BURST_MASK           0x08
#define    FDM_MAIN_BURST_SHIFT          3

#define    FDM_WHISPER_BYTE              0
#define    FDM_WHISPER_MASK              0x04
#define    FDM_WHISPER_SHIFT             2

#define    FDM_SWEEP_BYTE                0
#define    FDM_SWEEP_MASK                0x02
#define    FDM_SWEEP_SHIFT               1

#define    FDM_COMMAND_COUNTER_BYTE      0
#define    FDM_COMMAND_COUNTER_MASK      0x01
#define    FDM_COMMAND_COUNTER_SHIFT     0

#define    FDM_SAMPLING_MODE_BYTE        1
#define    FDM_SAMPLING_MODE_MASK        0xc0
#define    FDM_SAMPLING_MODE_SHIFT       6

#define    FDM_INTERFEROMETRIC_BYTE      1
#define    FDM_INTERFEROMETRIC_MASK      0x20
#define    FDM_INTERFEROMETRIC_SHIFT     5

#define    FDM_DSC_IDX_BYTE              1
#define    FDM_DSC_IDX_MASK              0x1f
#define    FDM_DSC_IDX_SHIFT             0

#define    FDM_SUN_ANGLE_BYTE            2
#define    FDM_SUN_ANGLE_MASK            0xff
#define    FDM_SUN_ANGLE_SHIFT           0

#define    FDM_MOTOR_STATUS_BYTE         3
#define    FDM_MOTOR_STATUS_MASK         0xf0
#define    FDM_MOTOR_STATUS_SHIFT        4

#define    FDM_E_D_MODE_BYTE             3
#define    FDM_E_D_MODE_MASK             0x0f
#define    FDM_E_D_MODE_SHIFT            0


/* These are telemetry format types: */
#define BERKELEY_EFW_FORMAT   0
#define WEC_EFW_FORMAT        1
#define ESA_EFW_FORMAT        2

/* These are locations in the bench format and within the raw buffer,
 * of the parts of an LDFS telemetry block.
 */
#define DFS_BYTE_LENGTH_FB  24
#define SCP_BUFFER_LOC_FB   2
#define FDM_BUFFER_LOC_FB   4
#define DSC_BUFFER_LOC_FB   8
#define SFR_BUFFER_LOC_FB   16
#define LX_BUFFER_LOC_FB    24
#define HX_BUFFER_LOC_FB    84
#define HX_BUFFER_LOC_X_FB  2

/* These are locations in the WEC format and within the raw buffer,
 * of the parts of an LDFS telemetry block (note:  there is no SCP in 
 * this format and there is no "EB" 2-byte prefix either).
 */
#define DFS_BYTE_LENGTH_FW  20
#define FDM_BUFFER_LOC_FW   0
#define DSC_BUFFER_LOC_FW   4
#define SFR_BUFFER_LOC_FW   12
#define LX_BUFFER_LOC_FW    20
#define HX_BUFFER_LOC_FW    80
#define HX_BUFFER_LOC_X_FW  0

/* Sizes of various fields in bytes: */
#define FDM_BUFFER_SIZE   4
#define DSC_BUFFER_SIZE   8
#define SFR_BUFFER_SIZE   8
#define LX_BUFFER_SIZE    60

/* Note that "17" was the value for CRRES.*/
#define SFR_BYTES_PER_FIT  14

/* The total number of bytes in a DSC cycle.  Note that "256" was
 * the value for POLAR as well.
 */
#define BYTES_PER_DSC  256

/* Definitions for High and Low-Rate Channel decoding: */
/* Total Number of Descriptors. */
#define HLX_NUMBER_DESCRIPTORS  64

/* Location in DSC of the start of the list of channel descriptors: */
#define HIGH_LOW_DSC_LOC   0

/* Location in DSC of current descriptor for the different modes: */
#define HX_MODE1_DSC_LOC    HLX_NUMBER_DESCRIPTORS
#define HX_MODE2_DSC_LOC    HLX_NUMBER_DESCRIPTORS + 1
#define HX_MODE3_DSC_LOC    HLX_NUMBER_DESCRIPTORS + 2
#define HX_MODE4_DSC_LOC    HLX_NUMBER_DESCRIPTORS + 3
#define LX_DSC_LOC          HLX_NUMBER_DESCRIPTORS + 4

/* End of descriptor-list byte: */
#define HLX_EOL             0x3f

/* The number of standard EFW Mux qtys: */
#define EFW_STD_MUX_QTYS    32

/* This is from Polar - it may not really be needed: */
#define RAM_FUNCTION_CODE   24

/* High and low rate channel descriptor values: */
#define HLX_PLAYBACK_BIT        0x80	/* Playbit bit. */
#define HLX_RAM_BIT_MASK        0x40    /* RAM bit. */
#define HLX_RAM_BASE_MASK       0x3c
#define HLX_RAM_BASE_SHIFT      2
#define HLX_RAM_WORDS_MASK      3
#define HLX_FUNCTION_MASK       0x1f

#define HLX_VALUE_BITS          0x70    /* Check for value types */
#define HXL_MUX_VALUE           0       /* Value of MUX type */
#define HXL_NORM_VALUE          0x10    /* Value of normal channel type */

#define HXL_V12L_V34L           0x10    /* Function "V12L" & "V34L" */
#define HXL_V12L_V3L            0x11    /* Function "V12L" & "V3L" */
#define HXL_V12L_V4L            0x12    /* Function "V12L" & "V4L" */
#define HXL_V1L_V34L            0x13    /* Function "V1L" & "V34L" */
#define HXL_V2L_V34L            0x14    /* Function "V2L" & "V34L" */
#define HXL_BPC_BP34            0x15    /* Function "BPC" & "BP34" */

#define HXL_V12M_V3M            0x16    /* Function "V12M" & "V3M" */
#define HXL_V12M_V4M            0x17    /* Function "V12M" & "V4M" */
#define HXL_V1M_V34M            0x18    /* Function "V1M" & "V34M" */
#define HXL_V2M_V34M            0x19    /* Function "V2M" & "V34M" */
#define HXL_V12M_V3M_V4M        0x1a    /* Function "V12M" & "V3M" & "V4M" */
#define HXL_V1M_V2M_V34M        0x1b    /* Function "V1M" & "V2M" & "V34M" */
#define HXL_V1M_V2M_V3M_V4M     0x1c    /* Function "V1M","V2M","V34M","V4M" */
#define HXL_V1M_V34M_V32M       0x1d    /* Function "V1M" & "V34M" & "V32M" */

#define HXL_USER0               0x1e    /* Function "USER0" */
#define HXL_USER1               0x1f    /* Function "USER1" */

#define HXL_RAM_ADDR_BITS       0x3c    /* Location of ram address. */

/* These map the Cluster burst quantities into something that SDT
 * recognizes for the burst MUX channels.
 */
#define EFW_BV1L               0
#define EFW_BV2L               1
#define EFW_BV1M               2
#define EFW_BV2M               3
#define EFW_BV1H               4
#define EFW_BV2H               5
#define EFW_BV1U               6
#define EFW_BV2U               7
#define EFW_BV3L               8
#define EFW_BV4L               9
#define EFW_BV3M              10
#define EFW_BV4M              11
#define EFW_BV3H              12
#define EFW_BV4H              13
#define EFW_BV3U              14
#define EFW_BV4U              15
#define EFW_BV12M             16
#define EFW_BV43M             17

/* "BV43H" does occur before "BV21H" */
#define EFW_BV43H             18
#define EFW_BV12H             19

#define EFW_BSCX              20
#define EFW_BSCY              21
#define EFW_BSCZ              22
#define EFW_BBP12             23

#define EFW_BSTAT1            24
#define EFW_BSTAT2            25
#define EFW_BSTAT3            26
#define EFW_BSTAT4            27
#define EFW_BADTEMP           28
#define EFW_BPWR28I           29
#define EFW_BREF              30
#define EFW_BPWRTEMP          31

/* These map the Cluster realtime quantities into something that
 * SDT recognizes for the realtime MUX channels.
 */
#define EFW_RV1L               0
#define EFW_RV2L               1
#define EFW_RV1M               2
#define EFW_RV2M               3
#define EFW_RV1H               4
#define EFW_RV2H               5
#define EFW_RV1U               6
#define EFW_RV2U               7
#define EFW_RV3L               8
#define EFW_RV4L               9
#define EFW_RV3M              10
#define EFW_RV4M              11
#define EFW_RV3H              12
#define EFW_RV4H              13
#define EFW_RV3U              14
#define EFW_RV4U              15
#define EFW_RV12M             16
#define EFW_RV43M             17

/* "RV43H" does occur before "RV21H" */
#define EFW_RV43H             18
#define EFW_RV12H             19

#define EFW_RSCX              20
#define EFW_RSCY              21
#define EFW_RSCZ              22
#define EFW_RBP12             23

#define EFW_RSTAT1            24
#define EFW_RSTAT2            25
#define EFW_RSTAT3            26
#define EFW_RSTAT4            27
#define EFW_RADTEMP           28
#define EFW_RPWR28I           29
#define EFW_RREF              30
#define EFW_RPWRTEMP          31

#define EFW_RV12L             32
#define EFW_RV34L             33
#define EFW_RBPC              34
#define EFW_RV32M             35
#define EFW_RV32L             36
#define EFW_RV23L             37
#define EFW_RV23M             38
#define EFW_RV34M             39

/* DSC description values: */
#define HXL_USER1               0x1f    /* Function "USER1" */
#define DSC_FORMAT_IDX          72
#define DSC_CPU_CLOCK_LOC       80      /* Byte 0 of 5-byte CPU clock */
#define DSC_SUN_PERIOD_H        88
#define DSC_COM_REG_0           96
#define DSC_MAG_REG_3           104
#define DSC_BURST_FREQ          112
#define DSC_PARAM_3             120
#define DSC_BIAS_DAC_1          128
#define DSC_GUARD_DAC_1         136
#define DSC_BIAS_1              144
#define DSC_STUB_3              152
#define DSC_GUARD_3             160
#define DSC_IVSTAT_3            168
#define DSC_SPHERE_TEMP_3       176
#define DSC_COVERS_3            184
#define DSC_VREF_L              192
#define DSC_LENGTH_B            200
#define DSC_SWEEP_STATE         208
#define DSC_SWEEP_RES_2         216
#define DSC_SWEEP_VSTEP_12      224
#define DSC_SWEEP_ALG_34        232
#define DSC_SWEEP_ISTEP_34      240
#define DSC_SWEEP_SPARE_34      248

/* This is the "MODE" command that is used by a GSE user to reset the
 * EWF tape mode (0 = normal,  tape modes 1, 2, 3).
 *
 * NOTE!  This value must be the same as the opcode for "MODE" in
 *        "icmds.dta".
 */
#define EFW_MODE_COMMAND        0xc000

#define  EFW_INITIAL_PMODE_STATE_LIST_SIZE   200
#define  EFW_PMODE_STATE_LIST_INCREMENT      200

#define  EFW_ED_MODE_SIDX    18

/* The number of clock times per second for the EFW CPU clock, for
 * the 4 spacecraft.  We assume, at least at first, that all update
 * at the documented rate of 1000/sec.   It may be that, later, we
 * find that some of these are not exactly at the documented rate.
 */
#define EFW_SC1_CPU_CLOCK_TICKS_PER_SECOND   1000.0
#define EFW_SC2_CPU_CLOCK_TICKS_PER_SECOND   1000.0
#define EFW_SC3_CPU_CLOCK_TICKS_PER_SECOND   1000.0
#define EFW_SC4_CPU_CLOCK_TICKS_PER_SECOND   1000.0

#define EFW_SC1_CHIRP_SEGMENT_TIME_LEN       0.2
#define EFW_SC2_CHIRP_SEGMENT_TIME_LEN       0.2
#define EFW_SC3_CHIRP_SEGMENT_TIME_LEN       0.2
#define EFW_SC4_CHIRP_SEGMENT_TIME_LEN       0.2

/* Indices into the Real-Time QUANTITY list (QTY) */
#define  IDX_V1L    0
#define  IDX_V2L    1
#define  IDX_V12L   2
#define  IDX_V3L    3
#define  IDX_V4L    4
#define  IDX_V34L   5
#define  IDX_V1M    6
#define  IDX_V2M    7
#define  IDX_V12M   8
#define  IDX_V3M    9
#define  IDX_V4M    10
#define  IDX_V34M   11
#define  IDX_V1H    12
#define  IDX_V2H    13
#define  IDX_V12H   14
#define  IDX_V3H    15
#define  IDX_V4H    16
#define  IDX_V34H   17
#define  IDX_V1U    18
#define  IDX_V2U    19
#define  IDX_V3U    20
#define  IDX_V4U    21
#define  IDX_V32M   22
#define  IDX_BPC    23
#define  IDX_BP34   24
#define  IDX_SCX    25
#define  IDX_SCY    26
#define  IDX_SCZ    27
#define  IDX_BIAS1  28
#define  IDX_BIAS3  29
#define  IDX_BIAS2  30
#define  IDX_BIAS4  31
#define  IDX_STAT1  32
#define  IDX_STAT2  33
#define  IDX_STAT3  34
#define  IDX_STAT4  35
#define  IDX_ADTEMP 36
#define  IDX_REF1   37
#define  IDX_PWR28I 38
#define  IDX_AGND   39

/* The following lists tell what mux channel is used by the hardware for
 * the raw burst quantities (shown here for documentation only):
 *
 *   Mux 0:                     Mux 1:
 *      chan:    qty:               chan:    qty:
 *        0        V1L                0        V2L
 *        1        V1M                1        V2M
 *        2        V1H                2        V2H
 *        3        V1U                3        V2U
 *        4        V3L                4        V4L
 *        5        V3M                5        V4M
 *        6        V3H                6        V4H
 *        7        V3U                7        V4U
 *        8        V12M               8        V34M
 *        9        V34H               9        V12H
 *        10       SCX                10       SCY
 *        11       SCZ                11       BP34
 *        12       BIAS1              12       BIAS2
 *        13       BIAS3              13       BIAS4
 *        14       ADTEMP             14       PWR28I
 *        15       REF1.2             15       PWRTEMP
 */

/* Indices into the Burst QUANTITY list (BQTY).  Note that the
 * location of the burst output quantities in BQTY is not the
 * same as the hardware mux channel numbers.
 */
#define  IDX_BV1L     0
#define  IDX_BV2L     1
#define  IDX_BV3L     2
#define  IDX_BV4L     3
#define  IDX_BV1M     4
#define  IDX_BV2M     5
#define  IDX_BV12M    6
#define  IDX_BV3M     7
#define  IDX_BV4M     8
#define  IDX_BV34M    9
#define  IDX_BV1H     10
#define  IDX_BV2H     11
#define  IDX_BV12H    12
#define  IDX_BV3H     13
#define  IDX_BV4H     14
#define  IDX_BV34H    15
#define  IDX_BV1U     16
#define  IDX_BV2U     17
#define  IDX_BV3U     18
#define  IDX_BV4U     19
#define  IDX_BBP34    20
#define  IDX_BSCX     21
#define  IDX_BSCY     22
#define  IDX_BSCZ     23
#define  IDX_BBIAS1   24
#define  IDX_BBIAS2   25
#define  IDX_BBIAS3   26
#define  IDX_BBIAS4   27
#define  IDX_BADTEMP  28
#define  IDX_BREF1    29
#define  IDX_BPWR28I  30
#define  IDX_BPWRTEMP 31

/* Two-byte words per one lx interrupt (5Hz) */
#define  WORDS_PER_LX_INTERRUPT   6

#define  LX_SAMPLES_PER_SECOND    30

/* Indices into GSE Plot speed list. */
#define  GSE_HX_RT_PLOT_SPEED        0
#define  GSE_LX_RT_PLOT_SPEED        1
#define  GSE_SPIN_PLOT_SPEED         2
#define  GSE_BRST_PLOT_SPEED         3
#define  GSE_MAIN_PLOT_SPEED         4

#ifdef FROM_POLAR
#define  SWP_STD_CODE_VAL    0xee
#endif /* FROM_POLAR */

/* Locations of things in Sweep header blocks. */
#define  SWP_NDATA_PTS       512
#define  SWP_NPTS_PER_QTY    128
#define  SWP_BYTES_IN_HEADER 22
#define  SWP_HDRC_LOC        0
#define  SWP_STD_CODE_LOC    1
#define  SWP_OPTS_LOC        2
#define  SWP_ANGLE_LOC       3
#define  SWP_ALGO_LOC        4
#define  SWP_ALT1_LOC        5
#define  SWP_ALT2_LOC        6
#define  SWP_RSLT1_LOC       7
#define  SWP_RSLT2_LOC       8
#define  SWP_M_LOC           9
#define  SWP_N_LOC           10
#define  SWP_NREJ_LOC        11
#define  SWP_ISTEP_LOC       12
#define  SWP_IBIAS_LOC       13
#define  SWP_IRATE_LOC       14
#define  SWP_ILEN_LOC        15
#define  SWP_VSTEP_LOC       16
#define  SWP_VBIAS_LOC       17
#define  SWP_VRATE_LOC       18
#define  SWP_VLEN_LOC        19
#define  SWP_SPARE1_LOC      20
#define  SWP_SPARE2_LOC      21

#define  SWP_EMODE_MASK      0x01
#define  SWP_DMODE_MASK      0x02
#define  SWP_DMODE_SHIFT     1

#define  EFW_BURST_IDENTIFIER  0xb2
#define  BST_BYTES_IN_HEADER 44
#define  BST_MAXIMUM_QTYS    16
#define  BST_BYTES_IN_HEADER_PRE_BQTY 36
#define  BST_FORMAT_CODE_LOC 0
#define  BST_FREQ_CODE_LOC   1
#define  BST_TRIG_CODE_LOC   2
#define  BST_BCHIRP_LOC      3
#define  BST_BPAGES_LOC      4
#define  BST_BTHRESH_LOC     5
#define  BST_PARAMS_LOC      6
#define  BST_STIME_LOC       10
#define  BST_VTIME_LOC       15
#define  BST_ETIME_LOC       20
#define  BST_SADDR_LOC       25
#define  BST_EADDR_LOC       28
#define  BST_R_LEN_LOC       31
#define  BST_SPARE_LOC       34
#define  BST_QTY_LOC         36

#define BST_HX_OVERRIDE_MASK   0x20
#define BST_HX_OVERRIDE_SHIFT  5

#define BST_SAMPLE_MODE_MASK   0x18
#define BST_SAMPLE_MODE_SHIFT  3

#define BST_FREQ_CODE_MASK     0x07
#define BST_FREQ_CODE_SHIFT    0

/* These constants are used to decode burst mux quantity descriptors. */
#define  BMUX0_MASK          0x0f
#define  BMUX1_MASK          0x70
#define  BMUX1_SHIFT         4
#define  BMUX1_HIGH          0x08

/* ------------------------------------------------------------- */
/* Telemetry event codes: */

#define  TEV_CURRENT_INSTRUMENT_STATE   0
#define  TEV_REALTIME_DATA              1
#define  TEV_REALTIME_MULTIPLE_DATA     2
#define  TEV_SPIN_FIT_RESULT            3
#define  TEV_SPIN_FIT_8_BYTE_RESULT     4
#define  TEV_SPACECRAFT_MODE_CHANGE     5
#define  TEV_END_TIME_SPAN              6
#define  TEV_RAM_DATA                   8
#define  TEV_DEPLOYMENT_DATA            9
#define  TEV_BURST_DATA                 20
#define  TEV_BURST_MULTIPLE_DATA        21
#define  TEV_START_BURST_MODE           22
#define  TEV_END_BURST_MODE             23
#define  TEV_MAIN_PLAYBACK_DATA         27
#define  TEV_SUN_PULSE_DATA             28

#define  TEV_FDM_EVENT                  30
#define  TEV_PLAYBACK_ON                31
#define  TEV_PLAYBACK_OFF               32
#define  TEV_TEST_CALIBRATE_ON          33
#define  TEV_TEST_CALIBRATE_OFF         34
#define  TEV_BURST_MODE_OFF             35
#define  TEV_BURST_MODE_SEARCHING       36
#define  TEV_BURST_MODE_COLLECTING      37
#define  TEV_BURST_MODE_WAIT            46
#define  TEV_PLAYBACK_MAIN              38
#define  TEV_PLAYBACK_BURST             39
#define  TEV_BIAS_SWEEP_ON              40
#define  TEV_BIAS_SWEEP_OFF             41
#define  TEV_COMMAND_ERROR_ON           42
#define  TEV_COMMAND_ERROR_OFF          43
#define  TEV_VOLTAGE_MODE_ON            44
#define  TEV_CURRENT_MODE_ON            45

#define  TEV_MAGNETOMETER_EVENT         50
#define  TEV_ARRAY_MAGNETOMETER_EVENT   51
#define  TEV_EFIELD_POWER_ON            52
#define  TEV_EFIELD_POWER_OFF           53
#define  TEV_MAGBU_POWER_ON             55
#define  TEV_MAGBU_POWER_OFF            56

#define  TEV_DSC_CYCLE                  60
#define  TEV_TELEMETRY_EOF              65
#define  TEV_SPECIAL_EVENT              66

#define  TEV_SWEEP_HDR_EVENT            70
#define  TEV_SWEEP_DATA_EVENT           71
#define  TEV_SWEEP_BIAS_DEF_EVENT       74
#define  TEV_SWEEP_SDT7_HDR_EVENT       72
#define  TEV_SWEEP_SDT7_DATA_EVENT      73
#define  TEV_SWEEP_SDT7_BIAS_DEF_EVENT  75

#define  TEV_BURST_HDR_EVENT            80
#define  TEV_BURST_DATA_EVENT           81

#define  TEV_ERROR                      100

#define  TEV_SWEEP_V1_TYPE              0
#define  TEV_SWEEP_V2_TYPE              1
#define  TEV_SWEEP_V1MV2_TYPE           2
#define  TEV_SWEEP_V3_TYPE              3
#define  TEV_SWEEP_V4_TYPE              4
#define  TEV_SWEEP_V3MV4_TYPE           5
#define  TEV_SWEEP_I1_TYPE              10
#define  TEV_SWEEP_I2_TYPE              11
#define  TEV_SWEEP_I1MI2_TYPE           12
#define  TEV_SWEEP_I3_TYPE              13
#define  TEV_SWEEP_I4_TYPE              14
#define  TEV_SWEEP_I3MI4_TYPE           15
#define  TEV_SWEEP_GUARD_TYPE           19
#define  TEV_SWEEP_STUB_TYPE            20

#define  TEV_DSC_DATA                   999

#define  TEV_DSC_ORB_PERIOD             48
#define  TEV_DSC_CMD_SEQ_ADDR           50
#define  TEV_DSC_ORB_CLOCK              52
#define  TEV_DSC_CMD_PTR_ADDR           54
#define  TEV_DSC_V34_TRIGGER            56

#define  TEV_DSC_EXEC_VERSION           71
#define  TEV_DSC_EXEC_SPARE_0           72
#define  TEV_DSC_EXEC_SPARE_1           73
#define  TEV_DSC_EXEC_SPARE_2           74
#define  TEV_DSC_EXEC_SPARE_3           75

#define  TEV_DSC_SUNANGLE               76
#define  TEV_DSC_SUNPERIOD              78

#define  TEV_DSC_MBURST_FREQ            80
#define  TEV_DSC_MBURST_TRIG            81
#define  TEV_DSC_MBURST_CHIRP           82
#define  TEV_DSC_MBURST_PAGES           83
#define  TEV_DSC_MBURST_THRESHO         84
#define  TEV_DSC_MBURST_PARAM0          85
#define  TEV_DSC_MBURST_PARAM1          86
#define  TEV_DSC_MBURST_PARAM2          87
#define  TEV_DSC_MBURST_PARAM3          88
#define  TEV_DSC_MBURST_EVALMAX         89
#define  TEV_DSC_MBURST_THRESHOLD       90
#define  TEV_DSC_MBURST_NMAX            91

#define  TEV_DSC_BBURST_FREQ            92
#define  TEV_DSC_BBURST_TRIG            93
#define  TEV_DSC_BBURST_CHIRP           94
#define  TEV_DSC_BBURST_PAGES           95
#define  TEV_DSC_BBURST_THRESHO         96
#define  TEV_DSC_BBURST_PARAM0          97
#define  TEV_DSC_BBURST_PARAM1          98
#define  TEV_DSC_BBURST_PARAM2          99
#define  TEV_DSC_BBURST_PARAM3          100
#define  TEV_DSC_BBURST_EVALMAX         101
#define  TEV_DSC_BBURST_THRESHOLD       102
#define  TEV_DSC_BBURST_NMAX            103

#define  TEV_DSC_MFE_STATUS             104
#define  TEV_DSC_MFE_X                  106
#define  TEV_DSC_MFE_Y                  108
#define  TEV_DSC_MFE_Z                  110

#define  TEV_DSC_SPIN_MEM_PHASE         112
#define  TEV_DSC_BANDPASS0              113
#define  TEV_DSC_BANDPASS1              114
#define  TEV_DSC_BVERSION               115

#define  TEV_DSC_SPARE_1                116
#define  TEV_DSC_SPARE_2                117
#define  TEV_DSC_CPU2_FDM               118
#define  TEV_DSC_CPU2_FAST_DATA         119
#define  TEV_DSC_CPU1_ENACTL            120
#define  TEV_DSC_V1_OFFSET              121

#define  TEV_DSC_BIAS1                  122
#define  TEV_DSC_BIAS2                  123
#define  TEV_DSC_BIAS3                  124
#define  TEV_DSC_BIAS4                  125
#define  TEV_DSC_BIAS5                  126
#define  TEV_DSC_BIAS6                  127
#define  TEV_DSC_STUB1                  128
#define  TEV_DSC_STUB2                  129
#define  TEV_DSC_STUB3                  130
#define  TEV_DSC_STUB4                  131
#define  TEV_DSC_STUB5                  132
#define  TEV_DSC_STUB6                  133
#define  TEV_DSC_GUARD1                 134
#define  TEV_DSC_GUARD2                 135
#define  TEV_DSC_GUARD3                 136
#define  TEV_DSC_GUARD4                 137
#define  TEV_DSC_GUARD5                 138
#define  TEV_DSC_GUARD6                 139

#define  TEV_DSC_BIAS_DAC_1             140
#define  TEV_DSC_BIAS_DAC_2             142
#define  TEV_DSC_BIAS_DAC_3             144
#define  TEV_DSC_BIAS_DAC_4             146
#define  TEV_DSC_BIAS_DAC_5             148
#define  TEV_DSC_BIAS_DAC_6             150

#define  TEV_DSC_STUB_DAC_1             152
#define  TEV_DSC_STUB_DAC_2             154
#define  TEV_DSC_STUB_DAC_3             156
#define  TEV_DSC_STUB_DAC_4             158
#define  TEV_DSC_STUB_DAC_5             160
#define  TEV_DSC_STUB_DAC_6             162

#define  TEV_DSC_GUARD_DAC_1            164
#define  TEV_DSC_GUARD_DAC_2            166
#define  TEV_DSC_GUARD_DAC_3            168
#define  TEV_DSC_GUARD_DAC_4            170
#define  TEV_DSC_GUARD_DAC_5            172
#define  TEV_DSC_GUARD_DAC_6            174

#define  TEV_DSC_DEPLOY_PAIR            176
#define  TEV_DSC_DEPLOY_STATUS          177
#define  TEV_DSC_DEPLOY_SWITCHES        178
#define  TEV_DSC_LENGTH_A               179
#define  TEV_DSC_LENGTH_B               180
#define  TEV_DSC_DEPLOY_LIMIT           181
#define  TEV_DSC_SAWTOOTH_OFFSET        182
#define  TEV_DSC_SAWTOOTH_DELTA         183
#define  TEV_DSC_SAWTOOTH_PERIOD        184
#define  TEV_DSC_SAWTOOTH_DIVIDER       185
#define  TEV_DSC_SAWTOOTH_OPTIONS       186
#define  TEV_DSC_SAWTOOTH_ENABLED       187

#define  TEV_DSC_LOAD_ADDR              188
#define  TEV_DSC_CHECK_LOAD             190

#define  TEV_DSC_ROM_ID                 255

#define  TEV_HK_EVT                     300

/* These indices are taken from:
 *    EFITM5.SCH,  July 4, 1992, by P. Harvey
 */
#define  TEV_HK_MEP28                   4
#define  TEV_HK_MEP5                    5
#define  TEV_HK_MEP_CNV_TEMP            6
#define  TEV_HK_MEP_TEMP_0              7
#define  TEV_HK_MEP_TEMP_1              8
#define  TEV_HK_MEP_TEMP_2              9
#define  TEV_HK_BOOM_TEMP_1             10
#define  TEV_HK_BOOM_TEMP_2             11
#define  TEV_HK_BOOM_TEMP_3             12
#define  TEV_HK_BOOM_TEMP_4             13
#define  TEV_HK_BOOM_TEMP_5             14
#define  TEV_HK_BOOM_TEMP_6             15
#define  TEV_HK_BOOM_LENGTH_1           16
#define  TEV_HK_BOOM_LENGTH_2           17
#define  TEV_HK_BOOM_LENGTH_3           18
#define  TEV_HK_BOOM_LENGTH_4           19
#define  TEV_HK_BOOM_LENGTH_5           20
#define  TEV_HK_BOOM_LENGTH_6           21
#define  TEV_HK_DEPLOY_SELECT           22
#define  TEV_HK_DEPLOY_STATUS           23
#define  TEV_HK_BOOM_SWITCHES           24
#define  TEV_HK_TURNS_COUNT_135         25
#define  TEV_HK_TURNS_COUNT_246         26
#define  TEV_HK_TURNS_COUNT_LIMIT       27
#define  TEV_HK_CPU1_CLOCK_0            28
#define  TEV_HK_CPU1_CLOCK_1            29
#define  TEV_HK_CPU1_CLOCK_2            30
#define  TEV_HK_CPU1_CLOCK_3            31
#define  TEV_HK_CPU1_CLOCK_4            32
#define  TEV_HK_CPU2_CLOCK_0            33
#define  TEV_HK_CPU2_CLOCK_1            34
#define  TEV_HK_CPU2_CLOCK_2            35
#define  TEV_HK_CPU2_CLOCK_3            36
#define  TEV_HK_CPU2_CLOCK_4            37
#define  TEV_HK_CMD_REG_0               38
#define  TEV_HK_CMD_REG_1               39
#define  TEV_HK_CMD_GOOD_CNT            40
#define  TEV_HK_CMD_TOT_CNT             41
#define  TEV_HK_MEM_LOAD_ADDR_L         42
#define  TEV_HK_MEM_LOAD_ADDR_H         43
#define  TEV_HK_MEM_LOAD_CNT_L          44
#define  TEV_HK_MEM_LOAD_CNT_H          45

/* Some of the above are combined into one event: */
#define  TEV_HK_CMD_REG_EVT             46
#define  TEV_HK_MEM_LOAD_ADDR_EVT       47
#define  TEV_HK_MEM_LOAD_CNT_EVT        48

/* The HK CPU clock events: */

#define  TEV_CPU_CLOCK_EVT              301
#define  TEV_CCLK_1_EVT                 49
#define  TEV_CCLK_2_EVT                 50

/* Aggregate V12L, V34L, V56L  events */
#define  TEV_COMPUTED_V123456           281

/* Skipped blocks detected: */
#define  TEV_SKIPPED_BLOCKS             282

/* Change in the quick scan FDM State: */
#define  TEV_QUICK_STATE_CHANGE         283

/* Filter Mode: */
#define  TEV_FILTER_MODE                284

/* ------------------------------------------------------------- */

/* The number of interrupts per second from DWP. */
#define INTERRUPTS_PER_SECOND  150

/* ------------------------------------------------------------- */
/* Typedefs. */

#ifndef int16
typedef  short  int16 ;
#endif

#ifndef uint16
typedef  unsigned short  uint16 ;
#endif

#ifndef int32
typedef  int  int32 ;
#endif

#ifndef uint32
typedef  unsigned int  uint32 ;
#endif


#ifdef OUT

#ifndef boolean
typedef int boolean ;
#endif

#endif /* OUT */

typedef int16  efw_fdm_typ ;

typedef char   efw_dsc_typ ;

/* ------------------------------------------------------------------ */
/* Interpolation: */

struct  VPoint_struct
    {   
    double   ttag ;
    double   val ;
    } ;
typedef  struct  VPoint_struct  VPoint ;

/* For holding data directories - LinkedList: */
struct  EfwInterpQtys_struct
    {
    /* 1 -> V12,  3 -> V34, 32 ->V32 */
    int            code ;

    double         LastUsedTtag ;
    VPoint         LastP13 ;
    VPoint         P24[3] ;
    int            cur_idx ;
    int            p13cnt ;
    int            p24cnt ;
    } ;
typedef  struct  EfwInterpQtys_struct  EfwInterpQtys ;

/* Stores CPU clock to GMT mapping: */
struct ClusterCPUClockMap_struct
    {
    /* This flag indicates the following:
     *    0 -> this entry is NOT time-consistent with the
     *         previous entry in the list keeping a history
     *         of EFW clocks.  Normally, this list will be:
     *         "efw_decom_contents->CPUClockMap"
     *    1 -> this entry IS time-consistent with the
     *         previous entry in the list keeping a history
     *         of EFW clocks.
     */
    int16    Consistency ;

    /* The 40-bit EFW clock value from DSC telemetry: */
    double   ClkCounts ;

    /* The computed UT for this DSC telemetry clock value
     * (see page 34 of the Cluster Command and Telemetry Description)
     */
    double   GMTTime ;
    } ;
typedef  struct ClusterCPUClockMap_struct  ClusterCPUClockMap ;

/* ------------------------------------------------------------------ */

/* FDM storage structure: */
struct  efw_fdm_contents_struct
    {
    int32   FDM ;                    /* Holds the raw, 32 bit value.*/
    int     scraft ;                 /* 1, 2, 3, or 4 */
    int     PlaybackInProgress ;
    int     Main_Burst ;
    int     BurstState ;
    int     Whisper ;
    int     Interferometer ;
    int     SweepInProgress ;
    int     CommandCounterMismatch ;
    int     DscIndex ;
    int     SunAngle ;
    int     SamplingMode ;           /* {Normal, Split, HxOnly, Null} */
    int     EDModeUnits ;            /* {4,3,2,1} */
    int     MotorStatus ;            /* {4,3,2,1} */
    int     QuickStatus ;            /* bit 0 == 1 -> I mode in at least
				      *    one sphere
				      * bit 1 == 1 -> sweeping
				      * bit 2 == 1 -> Brst collecting
				      */
    } ;
typedef struct   efw_fdm_contents_struct  efw_fdm_contents ;

/* Queue structure - mainly for high and low-rate realtime data, but
 * also used for queueing commands in the GSE.
 */
struct  hlx_queue_struct
    {
    int     size ;
    int     write_idx ;
    int     decode_idx ;
    int16   *data ;
    } ;
typedef struct   hlx_queue_struct  hlx_queue ;

/* Structure containing telemetry channel description (one for each of
 * the 64 possible elements of the telemetry list in the DSC).
 */
struct  tlm_channel_desc_struct
    {
    unsigned char     raw ;    /* The raw description as it is in the
				* DSC list.
				*/
    unsigned char     fnc ;    /* The function index as extracted from
				* "raw".
				*/
    unsigned char     nmb ;    /* The nmb of words to extract for each
				* "fnc" call.
				*/
    unsigned char     rbase ;  /* Used by ram playback only - this is
				* the offset of the start of the ram
				* from the RAM BASE in the DSC.
				*/

    unsigned char     rwords ; /* Used by ram playback only - this is
				* the number of words:
				*/
    } ;
typedef  struct  tlm_channel_desc_struct  tlm_channel_desc ;

#ifdef FROM_POLAR

/* This is the general header structure for a bias sweep playback. */
struct  efw_sweep_hdr_struct
    {
    unsigned char raw[24] ;
    double   time ;
    double   timelen ;
    int      hdr_code ;
    int      options ;
    int      eflag ;
    int      dflag ;
    int      angle ;
    int      algorithm ;
    int      alternate1 ;
    int      alternate2 ;
    int      result1 ;
    int      result2 ;
    int      m ;
    int      n ;
    int      nrejects ;
    int      vstep ;
    int      vbias ;
    int      vrate ;
    int      vlength ;
    int      istep ;
    int      ibias ;
    int      irate ;
    int      ilength ;
    int      spare1 ;
    int      spare2 ;
    int      v1max ;
    int      v1min ;
    int      v2max ;
    int      v2min ;
    int      i1max ;
    int      i1min ;
    int      i2max ;
    int      i2min ;
    int16    *v1data ;
    int16    *v2data ;
    int16    *i1data ;
    int16    *i2data ;
    } ;
typedef  struct  efw_sweep_hdr_struct  efw_sweep_hdr ;

/* This is the general header structure for an SDT7 sweep playback. */
struct  efw_sdt7_sweep_hdr_struct
    {
    unsigned char raw[24] ;
    double   time ;
    double   timelen ;
    int      hdr_code ;
    int      options ;
    int      eflag ;
    int      dflag ;
    int      angle ;
    int      algorithm ;
    int      alternate1 ;
    int      alternate2 ;

    /* Note that this stores the EFI CPU clock as follows:
     * clock[0]:  clk0  (LSB)
     * clock[1]:  clk1
     * clock[2]:  clk2
     * clock[3]:  clk3
     * clock[4]:  clk4  (MSB)
     */
    unsigned char   clock[8] ;

    int      vstep ;
    int      vbias ;
    int      vrate ;
    int      vlength ;
    int      istep ;
    int      ibias ;
    int      irate ;
    int      ilength ;
    int      Stub ;
    int      Guard ;
    int      v1max ;
    int      v1min ;
    int      v2max ;
    int      v2min ;
    int      i1max ;
    int      i1min ;
    int      i2max ;
    int      i2min ;
    int16    *v1data ;
    int16    *v2data ;
    int16    *i1data ;
    int16    *i2data ;
    } ;
typedef  struct  efw_sdt7_sweep_hdr_struct  efw_sdt7_sweep_hdr ;

#endif /* FROM_POLAR */


/* The probe-mode history structure.  There is one of these at
 * each instant a change in the mode of any of the 4 probes is
 * detected.  The fields are defined:
 *
 *   ttag:      The time, in day secs, of the change of mode.
 *   pmode[i]:  (i = 0 to 3)  If 1, then probe[i] is in current
 *              mode, otherwise probe[i] is in voltage mode.
 *              Note that probe[0] corresponds to "PROBE 1", ...
 *              probe[3] corresponds to "PROBE 4".
 */
struct  efw_pmode_state_struct
    {
    double         ttag ;
    unsigned char  pmode[4] ;
    } ;
typedef  struct  efw_pmode_state_struct  efw_pmode_state ;

struct  efw_v_val_struct
    {
    double   ttag ;
    double   val ;
    } ;
typedef  struct  efw_v_val_struct  efw_v_val ;

/* ------------------------------------------------------------- */
/* Structure to help interpolate V1,V2  V3,V4  V5,V6
 * Assume ttags will be at V1, V3, V5 times:
 */
struct  efw_interp_v_struct
    {
    int            code ;  /* 1 -> V12,  3 -> V34,  5 -> V56 */
    double         LastUsedTtag ;
    efw_v_val      LastP135 ;
    efw_v_val      P246[3] ;
    int            cur_idx ;
    int            p135cnt ;
    int            p246cnt ;
    } ;
typedef  struct  efw_interp_v_struct  efw_interp_v ;

/* ------------------------------------------------------------------ */
/* This is the general header structure for a bias sweep playback. */
struct  efw_sweep_hdr_struct
    {
    unsigned char raw[24] ;
    double      time ;
    double      timelen ;
    int         hdr_code ;
    int         options ;
    int         eflag ;
    int         dflag ;
    int		angle ;
    int		algorithm ;
    int		alternate1 ;
    int		alternate2 ;
    int		result1 ;
    int		result2 ;
    int         m ;
    int         n ;
    int		nrejects ;
    int		vstep ;
    int		vbias ;
    int		vrate ;
    int		vlength ;
    int		istep ;
    int		ibias ;
    int		irate ;
    int		ilength ;
    int		spare1 ;
    int		spare2 ;
    int         v1max ;
    int         v1min ;
    int         v2max ;
    int         v2min ;
    int         i1max ;
    int         i1min ;
    int         i2max ;
    int         i2min ;
    int16       *v1data ;
    int16       *v2data ;
    int16       *i1data ;
    int16       *i2data ;
    } ;
typedef  struct  efw_sweep_hdr_struct  efw_sweep_hdr ;

/* This is the general main_burst playback header structure. */
struct  efw_burst_hdr_struct
    {
    unsigned char        raw[BST_BYTES_IN_HEADER] ;

    /* Added 2001/03/09 - used in selected cases where
     * bad burst headers in telmetry are correctable:
     */
    unsigned char        corrected[BST_BYTES_IN_HEADER] ;

    unsigned char        format_code ;
    unsigned char        freq_code ;
    unsigned char        sample_ctrl_mode ;
    unsigned char        hx_override ;
    unsigned char	 trigger_code ;
    unsigned char	 bchirp ;
    unsigned char	 bpages ;
    unsigned char	 bthresh ;
    int16	         params[4] ;
    unsigned char        start_time[5] ;
    unsigned char        end_time[5] ;
    unsigned char        event_time[5] ;

    int32       start_addr ;
    int32       end_addr ;
    int32       rem_len ;
    int16       spare ;

    int16       n ;
    unsigned char        bqty[BST_MAXIMUM_QTYS] ;

    /* New as of 2001/03/09 - this flag indicates the
     * correctness of the header as follows:
     *
     *     0 -> correct
     *
     *     1 -> had errors, but was corrected to a usable
     *          burst hdr (in the "corrected" buffer, and
     *          exact timing was fully recoverable.
     *
     *     2 -> like "1", but exact timing was not recoverable.
     *
     *    -1 -> had fatal errors that could not be corrected,
     *          so the burst is unusable.
     */
    int         status ;

    /* New as of 2001/03/09 - the flag is meaningfull only
     * if "status" is non-zero.  It indicates the type of
     * error found in the header.
     */
    int         ecode ;

    /* Indicates if this is a reheadering of the current burst:
     *
     *    0 -> Completely new header
     *    1 -> Same as previous header
     */
    int         same_flg ;

    /* Added 2001/06/02 to allow code to check if the "start_time"
     * bytes are "real" (not all zero).
     *   0 -> start_time is not usable
     *   1 -> start_time is OK  (not all bytes are zero)
     */
    int         timing_is_ok ;

    /* Spacecraft (1,2,3, or 4): */
    int         sc ;

    /* Julian Day to which the time of the burst is relative.
     * It is not mandatory to set this, but if you do, use the
     * routine "SetJulianDayOfEFWSession", as it will also set
     * "UnixTimeOfJulianDay" for you.
     */
    int         JulianDay ;

    /* "UnixTimeOfJulianDay" is the UNIX time at 00:00 GMT of
     * "JulianDay".
     */
    double      UnixTimeOfJulianDay ;

    double      UT_ttag ;
    double      BurstFreq ;

    /* Data to help find burst playback within data files: */
    double      pback_start_time ;

    /* Since we can't return Cluster burst qtys as
     * vectors (there is no surety as to what the two
     * components of a Cluster burst vector are going
     * to be), we need to handle them as one-component
     * qtys, and there will be "2 * n" of them, which
     * is the number we put into "nqtys".  We put the
     * channel IDs into "fqtys".
     */
    int         nqtys ;
    int         fqtys[2* BST_MAXIMUM_QTYS] ;

#ifdef NEEDED_IN_CLUSTER_BURST_HEADER
    /* In chirp mode, ("bchirp" != 0), the following contains
     * flags for each of the 256, EFW_SCX_CHIRP_SEGMENT_TIME_LEN secs,
     * that may be represented by the bits in the on-board chirp
     * clock (which is AND'ed with "bchirp").  The flag for time
     * interval "idx" (0 <= idx < 256) in "chirp_flg" is as follows:
     *       1 -> burst sampling is ON
     *       0 -> burst sampling is OFF
     * See page 20, section 2.2.13 "Burst Chirp Mode Control" of
     * the "EFW Command and Telemetry" manual.
     */
    unsigned char        chirp_flg[256] ;
#endif

    /* See the same variable in "efw_decom_contents": */
    double      ChirpTimePartition ;

    /* Flag to indicate if we should, for BV12H, use interpolation
     * from BV1H and BV2H, instead of the mux qty: BV12H
     * Likewise for BV34H, BV12M, BV34M.  We only use interpolation
     * if the mux qty is NOT part of the Burst Qty List, but the
     * BV1 and BV2 are part of the list.
     */
    int      BV12HUseInterp ;
    int      BV34HUseInterp ;
    int      BV12MUseInterp ;
    int      BV34MUseInterp ;
    int      BV12LUseInterp ;
    int      BV34LUseInterp ;
    int      BV12UUseInterp ;
    int      BV34UUseInterp ;
    } ;
typedef  struct  efw_burst_hdr_struct  efw_burst_hdr ;

/* ------------------------------------------------------------- */
/* Structure which keeps a history of the Executive version of
 * a set of EFW data, while it is being processed.  The main
 * use of this information will be for handling the 8-byte
 * spin fit data that is in the WEC HK file (in the EFW "sliding-
 * window" data location).  In order to insure that we know which
 * WEC HK packets contain this SFit data, we need to now, at any
 * time, the value of the Exec Version.
 *
 * We assume that no EFW session will ever see a change of more
 * than "EFW_EXEC_VERSION_HISTORY_SIZE" Exec version values.
 */
struct  efw_exec_history_struct
    {
    int    nspans ;
    double  stime[EFW_EXEC_VERSION_HISTORY_SIZE] ;
    double  etime[EFW_EXEC_VERSION_HISTORY_SIZE] ;
    int     exec_value[EFW_EXEC_VERSION_HISTORY_SIZE] ;
    int     continuations[EFW_EXEC_VERSION_HISTORY_SIZE] ;
    } ;
typedef  struct  efw_exec_history_struct  efw_exec_history ;

/* ------------------------------------------------------------------ */
/* Telemetry events: */

struct tlm_struct_any_event
    {
    int16                  type ;
    int16                  space_craft ;
    } ;
typedef struct tlm_struct_any_event  tlm_any_event ;

/* FDM event: */
struct tlm_struct_fdm_evt
    {
    int16            type ;          /* Type of event. */
    int16            space_craft ;   /* Spacecraft code. */
    double           time ;          /* Time of playback off. */
    int16            f_event ;       /* Specific event type. */
    int32            fdm ;           /* New FDM. */
    int32            old_fdm ;       /* Old FDM. */
    } ;
typedef struct tlm_struct_fdm_evt  tlm_fdm_evt ;

struct tlm_struct_spmode_typ
    {
    int16            type ;      /* Type of event. */
    int16            space_craft ;   /* Spacecraft code. */
    int16            sub_type ;  /*  18 -> combined value (all probes)
				  *  19 -> Probe 1
				  *  20 -> Probe 2
				  *  21 -> Probe 3
				  *  22 -> Probe 4
				  */
    int16            new_mode ;  /* If "sub_type" == 18 (combined),
				  *   this is a 4-bit number.
				  * Otherwise, this is either:
				  *   0  ->  Probe is now in Efield mode.
				  *   1  ->  Probe is now in Density mode.
				  */
    double           time ;      /* Time Tag of the mode change. */
    } ;
typedef struct tlm_struct_spmode_typ  tlm_spmode_typ ;

struct tlm_struct_rdata_typ
    {
    int16            type ;    /* Type of event. */
    int16            space_craft ;   /* Spacecraft code. */
    int16            m_chan ;  /* Mux Channel. */
    int              nvals ;   /* The nmb of values (1-4) returned */
    float            value[4] ;  /* Data value(s). */
    int              VImode[4] ; /* 0 -> component is V, 1 -> is I */
    int              Calib[4] ;  /* 0 = !calibrated, 1 = calibrated */
    int16            gain ;    /* Gain bit (True -> on). */
    int16            hx_q ;    /* FALSE -> LX qty;  TRUE -> HX qty */
    double           time ;    /* Time Tag of the data. */
    } ;
typedef struct tlm_struct_rdata_typ  tlm_rdata_typ ;

struct tlm_struct_start_burst_evt
    {
    int16            type ;          /* Type of event. */
    int16            space_craft ;   /* Spacecraft code. */
    double           time ;          /* Time tag of the sweep hdr;
				      * This is the exact start time
				      * of the sweep:
				      */
    double           epoch_time ;    /* EFW Epoch Time tag of the hdr */
    void             *ptr ;
    } ;
typedef struct tlm_struct_start_burst_evt  tlm_start_burst_evt ;

struct tlm_struct_burst_data_evt
    {
    int16            type ;          /* Type of event. */
    int16            space_craft ;   /* Spacecraft code. */
    int16            m_chan ;        /* Mux Channel. */
    double           time ;          /* Time tag of the data */
    float            value ;         /* (Raw) Value of the data */
    unsigned char    VImode ;        /* 0 -> component is V, 1 -> is I */
    int              Calib ;     /* 0 = !calibrated, 1 = calibrated */
    int              data_type ;

    } ;
typedef struct tlm_struct_burst_data_evt  tlm_burst_data_evt ;

struct tlm_struct_deploydata_typ
    {
    int16            type ;    /* Type of event. */
    int16            space_craft ;   /* Spacecraft code. */
    int16            m_chan ;  /* Data Channel. */
    int16            pair ;    /* 1 -> booms 1,2    3 -> booms 3,4 */
    int              nvals ;   /* The nmb of values (1-4) returned */
    float            value[4] ;   /* Data value(s). */
    int16            ivalue[4] ;  /* Data value(s) in int16. */
    int              Calib[4] ;   /* 0 = !calibrated, 1 = calibrated */
    int16            gain ;    /* Gain bit (True -> on). */
    int16            hx_q ;    /* FALSE -> LX qty;  TRUE -> HX qty */
    double           time ;    /* Time Tag of the data. */
    } ;
typedef struct tlm_struct_deploydata_typ  tlm_deploydata_typ ;

struct tlm_struct_dsc_typ
    {
    int16            type ;          /* Type of event. */
    int16            space_craft ;   /* Spacecraft code. */
    double           time ;          /* Time tag of the DSC data. */
    int16            data_type ;     /* Should be one of the following:
				      *    DQ_STORE_CHAR
				      *    DQ_STORE_UCHAR
				      *    DQ_STORE_INT16
				      *    DQ_STORE_UINT16
				      */
    int16            dsc_ident ;     /* The DSC index (0-255)
				      * of the first byte of this
				      * value.  i.e. SPHERE_TEMP_1
				      * starts at index 172.
				      * See the table on page 43 of
				      * the CTM (titled: "Cluster EFW
				      * Instrument Digital Subcom
				      * Telemetry").
				      */
    unsigned char    msb ;           /* If 2-byte qty, the MSByte.
				      * If 1-byte qty, meaningless.
				      */
    unsigned char    lsb ;           /* If 2-byte qty, the LSByte.
				      * If 1-byte qty, the byte itself.
				      */
    } ;
typedef struct tlm_struct_dsc_typ  tlm_dsc_typ ;

struct tlm_struct_dsc_cycle
    {
    int16            type ;          /* Type of event. */
    int16            space_craft ;   /* Spacecraft code. */
    double           time ;          /* Tags the beginning of the new DSC
				      * cycle.  The information in the DSC
				      * cycle bin is for the previous cycle.
				      */
    int              nbytes ;        /* Number bytes in DSC buffer: */
    unsigned char    dsc[BYTES_PER_DSC] ;   /* DSC bytes: */ 
    } ;
typedef struct tlm_struct_dsc_cycle  tlm_dsc_cycle ;

struct tlm_struct_sweep_hdr_evt
    {
    int16            type ;          /* Type of event. */
    int16            space_craft ;   /* Spacecraft code. */
    double           time ;          /* Time tag of the sweep hdr;
				      * This is the exact start time
				      * of the sweep:
				      */
    efw_sweep_hdr    *hdr_ptr ;
    } ;
typedef struct tlm_struct_sweep_hdr_evt  tlm_sweep_hdr_evt ;

struct tlm_struct_sweep_bias_evt
    {
    int16            type ;          /* Type of event. */
    int16            space_craft ;   /* Spacecraft code. */
    double           STime1 ;        /* StartTime tag of the IBias */
    double           STime2 ;        /* StartTime tag of the VBias */
    double           DTime1 ;        /* DeltaTime of the IBias */
    double           DTime2 ;        /* DeltaTime of the VBias */
    double           SVal1 ;         /* StartValue of the IBias */
    double           SVal2 ;         /* StartValue of the VBias */
    double           DVal1 ;         /* DeltaValue of the IBias */
    double           DVal2 ;         /* DeltaValue of the VBias */
    float            Ibias[256] ;    /* Differs from Polar - we
				      * have specific values returned
				      * from the decom engine (because
				      * of the way the EFW cal tables
				      * are set up), rather than a
				      * simple start, delta procedure
				      * (which we can still use for
				      * time) for computing them at
				      * a higher level.
				      */
    float            Vbias[256] ;    /* Same comments as for Ibias */
    int              NPts1 ;         /* Number of IBias points */
    int              NPts2 ;         /* Number of VBias points */
    int              EFlag ;         /* 1 -> Voltage mode in sweep: */
    int              DFlag ;         /* 1 -> Density mode in sweep: */
    } ;
typedef struct tlm_struct_sweep_bias_evt  tlm_sweep_bias_evt ;

struct tlm_struct_sweep_data_evt
    {
    int16            type ;          /* Type of event. */
    int16            space_craft ;   /* Spacecraft code. */
    double           time ;          /* Time tag of the data */
    float            value ;         /* Calibrated Value of the data */
    float            bias ;          /* Corresponding bias value */
    int              data_type ;     /* As follows:
				      *   0   -> V1
				      *   1   -> V2
				      *   2   -> V1 - V2
				      *   3   -> V3
				      *   4   -> V4
				      *   5   -> V3 - V4
				      *   10  -> I1
				      *   11  -> I2
				      *   12  -> I1 - I2
				      *   13  -> I3
				      *   14  -> I4
				      *   15  -> I3 - I4
				      *   19  -> Guard
				      *   20  -> Stub
				      */
    } ;
typedef struct tlm_struct_sweep_data_evt  tlm_sweep_data_evt ;

struct tlm_struct_cclk_typ
    {
    int16            type ;          /* Type of event. */
    int16            space_craft ;   /* Spacecraft code. */
    double           time ;          /* Time tag of the packet
				      * carrying this clock value.
				      * It is the number of seconds
				      * since midnight of the date
				      * of the first data set being
				      * decoded in the current session.
				      */
    long             jday ;          /* Julian Day of the date of the
				      * first data set being decoded
				      * in the current sessiion, from
				      * which this clock value was
				      * read.
				      */
    ClusterCPUClockMap ClkData ;     /* Structure containing clock
				      * data.
				      */
    unsigned char    cbytes[8] ;     /* Storage holding the CPU clock
				      * bytes - typically 5 bytes are
				      * used.  The storage is always
				      * cybte[0] == lsb and up.  Unused
				      * bytes are zero-ed.
				      */
    } ;
typedef struct tlm_struct_cclk_typ  tlm_cclk_typ ;

/* This is a spin-fit data structure. */
struct  sfr_data_struct
    {
    double         st_time ;   /* Time at start of spin fit. */
    double         mid_time ;  /* Time at mid-point of spin fit. */
    int16          type ;      /* 0 for Boom12, 1 for Boom34. */
    int16          n ;         /* Number of points used for the fit. */
    float          a ;
    float          b ;
    float          c ;
    float          std_deviation ;

    /* Holds the 14-byte raw buffer: */
    unsigned char  raw[SFR_BYTES_PER_FIT + 2] ;
    } ;
typedef struct sfr_data_struct  sfr_data_typ ;

struct tlm_struct_sfr_typ
    {
    int16            type ;         /* Type of event. */
    int16            space_craft ;  /* Spacecraft code. */
    sfr_data_typ     data ;         /* SFR data structure. */
    } ;
typedef struct tlm_struct_sfr_typ  tlm_sfr_typ ;

/* This is the 8-byte spin-fit data structure. */
struct  sfr8_data_struct
    {
    double         st_time ;   /* Time at start of spin fit. */
    double         mid_time ;  /* Time at mid-point of spin fit. */
    int16          type ;      /* 0 for Boom12, 1 for Boom34. */
    float          b ;
    float          c ;
    float          std_deviation ;
    float          V1L ;

    /* Holds the 8-byte raw buffer: */
    unsigned char  raw[10] ;
    } ;
typedef struct sfr8_data_struct  sfr8_data_typ ;

struct tlm_struct_sfr8_typ
    {
    int16            type ;         /* Type of event. */
    int16            space_craft ;  /* Spacecraft code. */
    sfr8_data_typ     data ;         /* SFR data structure. */
    } ;
typedef struct tlm_struct_sfr8_typ  tlm_sfr8_typ ;

struct tlm_struct_telemetry_eof
    {
    int16            type ;          /* Type of event. */
    int16            space_craft ;   /* Spacecraft code. */
    double           time ;          /* Time tag of the EOF. */
    } ;
typedef struct tlm_struct_telemetry_eof  tlm_telemetry_eof ;

struct tlm_struct_filter_mode
    {
    int16            type ;          /* Type of event. */
    int16            space_craft ;   /* Spacecraft code. */
    int32            fmode ;         /*  0 -> L, 1 -> M, 2 -> H,
                                      *  3 -> U 
                                      */
    double           time ;          /* Time tag of the EOF. */
    } ;
typedef struct tlm_struct_filter_mode  tlm_filter_mode ;

struct tlm_struct_error_typ
    {
    int16            type ;          /* Type of event. */
    int16            space_craft ;   /* Spacecraft code. */
    int32            error_code ;    /* Error code. */
    int32            severity_code ; /* Error code. */
    int32            module_code ;   /* Module code. */
    int32            line_code ;     /* Line code. */
    int32            routine_code ;  /* Routine code. */
    double           time ;          /* Time Tag of the error. */
    } ;
typedef struct tlm_struct_error_typ  tlm_error_typ ;

/* This is the general event structure. */
union  tlm_evt_union
    {
    int16                  type ;             /* Event type. */
    tlm_any_event          any_event ;

    /* Fast Digital Monitor change event. */
    tlm_fdm_evt            fdm_event ;

    tlm_spmode_typ         efw_ed_mode ;      /* V/I mode. */

    /* Realtime data event: */
    tlm_rdata_typ          rdata ;

    tlm_start_burst_evt    start_burst ;
    tlm_burst_data_evt     burst_data ;

    /* Deployment data event: */
    tlm_deploydata_typ     deploydata ;

    /* Sweep events: */
    tlm_sweep_hdr_evt      sweep_hdr ;
    tlm_sweep_bias_evt     sweep_bias ;
    tlm_sweep_data_evt     sweep_data ;

    tlm_dsc_typ            dsc_data;          /* DSC data. */
    tlm_dsc_cycle          dsc_cycle ;

    tlm_sfr_typ            sfrdata ;          /* Spin-Fit data. */

    /* 8-byte spin fit data: */
    tlm_sfr8_typ           sfr8data ;

    tlm_cclk_typ           cclk_data ;        /* CPU clock data. */

    tlm_telemetry_eof      telemetry_eof ;

    tlm_filter_mode        filter_mode ;

    /* Decommutation error. */
    tlm_error_typ          dcom_error ;
    } ;
typedef union tlm_evt_union  tlm_evt_typ ;

/* ------------------------------------------------------------------ */
/* This is the general structure containing all necessary status
 * and other runtime information for an EFW decom session.  This
 * exists so that no globals are needed - thus enabling use of
 * threads.
 */
struct  efw_decom_contents_struct
    {
    int             telemetry_format ;
    int             TEMode ;
    int             spacecraft ;

    /* This contains the Julian Day of the start of the time span. */
    long            JulianDay ;

    /* Unix time at 00:00 GMT of "JulianDay": */
    double          UnixTimeOfJulianDay ;

    int             RealtimeOkayFlag ;
    int             BurstOkayFlag ;

    /* Keeps track of the number of packets of each telemetry
     * mode.  Indices are as follows:
     *    0 -> NSD,  1 -> BSD1,  2 -> BSD2,  3 -> BSD3
     */
    int             PacketTypeCount[4] ;

    /* Flag which is used as follows:
     *  0 -> Get burst timing via interpolation into the array of
     *       clock times by use of the routine "ClusterCPUClockToGMT".
     *  1 -> Get the burst time explicitly from the field:
     *       "TimeOfCurrentBurst", which has to be supplied
     *       at a higher level than in the decom engine.  This
     *       feature is used in conjunction with burst history files
     *       that have already captured the start of burst times.
     */
    int     UseExplicitBurstTiming ;

    /* Flag indicating whether or not CPU clock values should be
     * passed back to the caller of the decom engine.
     * The default is "0" - do not pass back the clock count data.
     * Use the routine:  SetReturnCPUClockCountsFlag   to set this
     * flag ON.
     */
    int     ReturnCPUClockCounts ;

    /* See the comments for "UseExplicitBurstTiming".
     * Note that "TimeOfCurrentBurst" is ignored unless we are
     * explicitly setting burst times from the EFI burst history.
     */
    double  TimeOfCurrentBurst ;

    /* The number of values in one burst data cycle.  This is the
     * number of Bqtys times 2 (since each Bqty is a 2-vector).
     * This, in fact, equals "bhdr->nqtys".
     */
    int    BurstDataCycleSize ;

    /* The current data cycle. This number wraps to 0 every
     * "BurstDataCycleSize" cycles:
     */
    int     BurstCurrentDataCycle ;

    /* The number of data cycles, returned so far, in the current
     * burst.  This number does not wrap to zero (until the
     * beginning of the next burst):
     */
    int     BurstNumberDataCycles ;

    /* The current vector component index of the current data
     * cycle (2 components per data cycle):
     */
    int     BurstDVecIdx ;

    /* Keeps count of the number of chirps in the current burst. */
    int     BurstChirpCount ;

    /* We need to accumulate all vectors in each
     * record before re-xmitting them.  do that in here
     * (note that add a bit extra here with the "3" instead of
     * a "2" although use of "2" should be sufficient).
     */
    int16   BurstStoreRecord[BST_MAXIMUM_QTYS * 3] ;

    /* The current telemetry mode: */
    int             current_tlm_mode ;

    /* The previous telemetry mode: */
    int             prev_current_tlm_mode ;

    /* The current secs per telemetry packet: */
    double          secs_per_packet ;

    /* The number of packets seen so far: */
    int             packet_cnt ;

    /* In GSE mode, there are 4 bytes prepended to EFW telmetry
     * blocks.  In project data files, there are none.  This
     * offset allows the code to know where that actual packet
     * data starts:
     */
    int             IdxOffset ;

    /* Added 2001/05/31:  Keeps track of whether or not we are still
     * expecting the first byte of a burst playback.  This is different
     * from "pback_counter", which keeps track of which byte, in the
     * playback of the header, we are currently at.
     *
     *    0 -> We are still waiting for the starting byte of the next
     *         burst.
     *
     *    1 -> We have started a burst playback.
     *
     * As of 2001/05/31, if this value is "0" and the first playback
     * byte is not the burst header signature value:
     *
     *    EFW_BURST_IDENTIFIER
     *
     * we assume that the start of the playback was in a missing packet,
     * so the burst cannot be used.   In the case where the header is
     * missing, we turn "BurstPBackIsOK" OFF (e.g. to "0") and ignore
     * the rest of the burst.
     */
    int             BurstPBackHasStarted ;

    /* Indicates if the current burst is OK (i.e. we were able to get
     * the header bytes).  If this value is "0", we completely ignore
     * the current burst playback.
     */
    int             BurstPBackIsOK ;

    /* Keeps track of which byte we are at, in the burst header that
     * is currently in playback.
     */
    int             pback_counter ;

    /* Keeps track of how many data points have been read from
     * the current burst:
     */
    int             burst_data_cnt ;

    /* The number of 2-byte values that have been played back in
     * a main playback (header and data):
     */
    int main_pback_counter ;

    /* Keeps track of how many data points have been read from
     * the current sweep:
     */
    int sweep_data_cnt ;

    /* The number of data-points in the current sweep.  This will
     * always be (ilen + vlen) * 2 points (usually "ilen" and
     * "vlen" will be 128 so the total number of data points will
     * usually be 512).  Each data-point is a 2-byte integer (so
     * there will usually be a total of 512 * 2 == 1024 bytes in
     * the data section of a sweep playback):
     */
    int    SweepNPts ;

    /* Start and End times of the latest Sweep: */
    double  SweepStartTime ;
    double  SweepEndTime ;

    /* These are saved from the Sweep header to aid in handling
     * Sweep data:
     */
    double BiasSTime1 ;
    double BiasSTime2 ;
    double BiasDTime1 ;
    double BiasDTime2 ;
    double BiasSVal1 ;
    double BiasSVal2 ;
    double BiasDVal1 ;
    double BiasDVal2 ;
    int    BiasNPts1 ;
    int    BiasNPts2 ;

    /* Indicates when, within the SweepNPts sweep data points, we go
     * to from the V to the I data:
     */
    int    VToITransitionIdx ;

    /* Data type being sent in Sweep data: */
    int    VFirstType ;
    int    VSecondType ;
    int    VDiffType ;
    int    IFirstType ;
    int    ISecondType ;
    int    IDiffType ;
    int    VFirstValue ;
    int    IFirstValue ;

    /* This stores the time-tag of the block in which the playback
     * for the current Burst began.  This may be used if it is
     * decided to time-tag the burst at playback time (to make sure
     * it shows up in a plot progam).
     */
    double BurstTmpPBackStartTime ;

    /* Holds the current sweep hdr info: */
    efw_sweep_hdr sweep_hdr ;

    int             main_pback_v1count ;
    int             main_pback_v2count ;
    int             main_pback_i1count ;
    int             main_pback_i2count ;
    int16           main_pback_v1data[SWP_NPTS_PER_QTY] ;
    int16           main_pback_v2data[SWP_NPTS_PER_QTY] ;
    int16           main_pback_i1data[SWP_NPTS_PER_QTY] ;
    int16           main_pback_i2data[SWP_NPTS_PER_QTY] ;
    unsigned char   hx_chan_desc[4] ;
    unsigned char   lx_chan_desc ;

    /* Telemetry handler and  auxiliary pointer: */
    void (*tlm_event_fnc) (tlm_evt_typ *event, void *ptr) ;
    void  *tlm_aux_ptr ;

    /* Contains the size (in 2-byte words) of the current
     * telemetry packet buffer (90 is normal mode):
     */
    int             BufferLen ;

    /* The current dsc index (extracted from the FDM bytes: */
    int             current_dsc_idx ;

    /* The previous dsc index (extracted from the FDM bytes: */
    int             prev_dsc_idx ;

    /* Indicates whether the start of the DSC cycle has been seen
     * so far during this telemetry session:
     */
    int             dsc_start_seen ;

    /* Flag indicating whether or not this session has set its
     * TLM tables from the DSC:
     */
    int             tlm_table_updated ;

    /* The timetag of the start of the current DSC cycle: */
    double          dsc_start_ttag ;

    /* The time-tag of the packet: */
    double          packet_ttag ;

    /* The time-tag of the previous packet: */
    double          prev_packet_ttag ;

    efw_fdm_contents  CurrentFdm ;
    efw_fdm_contents  OldFdm ;

    int              hx_number_descriptors[4] ;
    int              lx_number_descriptors ;

    /* This holds the current sweep hdr buffer: */
    unsigned char    MHDRPTR[24] ;

    /* This indicates which, out of ten blocks per 1 second record,
     * we are currently processing, when we are in any of the tape
     * modes.  When in mode "EFW_TLM_NORMAL_MODE", this will always
     * be "0".  In the tape modes, "0" is the first of the ten blocks
     * (one every 0.1 seconds), "9" is the last block.
     */
    int              tape_mode_block_idx ;

    /* The time-tag of the "0"'th block in the current 10 block
     * tape mode set:
     */
    double          tape_mode_block_stime ;

    /* This holds the current telemetry event to be passed up: */
    tlm_evt_typ  t_evt ;

    /* Points to the current telemetry packet buffer: */
    unsigned char   *CURRENT_BUFFER ;
    unsigned char   *INPTR ;

    unsigned char   SaveSpinPeriodL ;

    /* This holds the current DSC table - this gets incrementally
     * updated (8 bytes per packet)
     : */
    unsigned char   DSC[256] ;
    unsigned char   *DSCP ;

    /* This stores hdr bytes when collecting a burst header. */
    unsigned char BHEADER[48] ;

    /* This holds the current decoded burst header: */
    efw_burst_hdr  burst_hdr ;

    tlm_channel_desc  efw_chan_desc[HLX_NUMBER_DESCRIPTORS] ;

    /* This integer allows us to utlize alternate "clock" algorithms.
     * As of 00/05/12, there is only one (EfwClockAlg = 0).
     */
    int            EfwClockAlg ;

    /* This integer keeps track of the number of array elements
     * allocated to "CPUClockMap".
     */
    int            CPUClockMapSize ;

    /* This integer keeps track of the number of elements currently
     * used in "CPUClockMap".  Note that some of these may be popped
     * off of "CPUClockMap", depending on consistency checking.  It
     * does, however, always indicate where to put the next entry,
     * whether it eventually checks out for consistency or not.
     */
    int            CPUClockMapCnt ;

    /* This integer keeps track of the number of know consistent
     * clock entries in "CPUClockMap", which are all at the
     * beginnning of the list.  So entries:
     *
     *    0 - "CPUClockConsistentCnt - 1"
     *
     * will never be popped from "CPUClockMap".
     */
    int            CPUClockConsistentCnt ;

    /* Holds arrays of cpu clock times and corresponding GMTs: */
    ClusterCPUClockMap   *CPUClockMap ;

    /* Contains the number of blocks, time-wise, since the previous
     * block.  This will almost always be 1, otherwise it will be
     * greater than 1, indicating that there were skipped blocks
     * in telemetry, and the number of skipped blocks will be this
     * value minus 1.
     */
    int   NBlksSinceLastBlk ;

    /* If a boom deployment is underway, this is set to:
     *      1 -> if booms 1,2 are deploying
     *      3 -> if booms 3,4 are deploying
     *  Otherwise, it is 0.
     */
    int   DeployingPair ;

    /* Temporary storage areas to consolidate HX RAM-pback from
     * 0x40, 0x50 into a single block of bytes.
     */
    unsigned char  CacheDeployingBytes[16] ;
    double   DeployingTTag ;

    /* Index of the number of "virtual" blocks that the current data
     * stream has seen.  "virtual" means that "skipped" blocks are
     * counted as well as the one's that we've seen.  So this is
     * used with "NBlksSinceLastBlk":
     */
    int   virtual_block ;

    /* Current Exec Version: */
    int   EVersion ;

    /* Indicates format of on-board spin fits:
     *   0  ->   14 byte
     *   1  ->    8 byte
     */
    int   SFitFormat ;

    /* When using the 8-byte spin fit format, we need to keep
     * track of the time and raw-byte values of the last unique
     * 8-bytes (these get repeated 2 or 3 time, unlike the 14-
     * byte format).  In fact, we only should compare the first
     * 6-bytes of the 8-byte format:
     */
    unsigned char SFitCurrentBytes[8] ;
    double  SFitCurrentTime ;

    /* This structure keeps a history of the Exec version for
     * the session:
     */
    efw_exec_history   EVersionHist ;

    /* This integer keeps count of how many times we find that we
     * need more than "XXX"
     * entries in "efw_exec_history".  It is used as much for
     * controlling the number of warning messages as any thing
     * else.
     */
    int  EVersionHistTooMany ;

    /* Frequency of the current burst: */
    double   BurstFreq ;

    /* Inverse of "BurstFreq" - gives the number of seconds between
     * consecutive burst records (i.e. a "burst record" consists of
     * one complete cycle of the Burst Qty List).
     */
    double   BurstTimeDelta ;

    /* This is the time delta between consecutive vectors within a
     * burst record (i.e. between 2 consecutive vectors in one
     * cycle of a Burst Qty List):
     */
    double   BurstVectorTimeIncr ;

    /* In chirp mode, ("bchirp" != 0), the following contains
     * flags for each of the 256, EFW_SCX_CHIRP_SEGMENT_TIME_LEN secs,
     * that may be represented by the bits in the on-board chirp
     * clock (which is AND'ed with "bchirp").  The flag for time
     * interval "idx" (0 <= idx < 256) in "chirp_flg" is as follows:
     *       1 -> burst sampling is ON
     *       0 -> burst sampling is OFF
     * See page 20, section 2.2.13 "Burst Chirp Mode Control" of
     * the "EFW Command and Telemetry" manual.
     */
    unsigned char        chirp_flg[256] ;

    /* Indicates the timespan, in seconds, of a segment of Chirp
     * burst mode.  Recall that a burst chirp consists of groups of
     * consecutive 0.2 second ON/OFF partitions.  This value should
     * nominally be 0.2 seconds, but is kept as a variable in case
     * any of the spacecraft clocks are a bit off and require
     * adjustment.  See "Cluster Command and Telemetry", p. 20,
     * section 2.2.13, "Burst Chirp Mode Control".
     */
    double  ChirpTimePartition ;

    /* An index, rotating from 0 to 255, indicating which of the
     * Chirp partitions we are currently in.  This is used in
     * conjunction with "chirp_flg" to determine the timing of
     * the next group of chirp burst data values.  This must
     * be initialized to zero at the start of each burst.
     */
    int     ChirpPartitionIdx ;

    /* Flag indicating whether or not to time-tag a burst at
     * playback time (flag == 1) or in their realtime, i.e. when
     * they were actually accumulated, (flag == 0).
     * We will probably only time-tag at playback for test
     * data, particularly before launch and during commissioning.
     */
    int     Burst_PBackTTagMode ;

    /* The start of the current burst: */
    double  BurstStartTime ;

    /* The start of the current burst chirp: */
    double  BurstStartChirp ;

    efw_burst_hdr   prev_burst_hdr ;

    /* Build-up an Burst data point for transmission here: */
    tlm_burst_data_evt  burst_data ;

    /* The following keep a history of the decom session's probe
     * modes.  This is required to make sure that burst handling
     * has an available history to determine whether values are
     * V or I.
     */

    /* Holds the up-to-date  efield/density mode from the FDM: */
    int   EfieldDensityMode ;
    int   ProbeEfieldDensityMode[4] ;

    /* The number of PMode states currently set: */
    int    NumPModes ;

    /* The number of PMode states available for filling in 
     * "PModeHistory":
     */
    int    TotalPModes ;

    /* The number of PMode states to add to "PModeHistory" when
     * "NumPModes" gets up to "TotalPModes":
     */
    int    PModesToAdd ;

    /* This is used by the burst to determine probe modes: */
    int  BurstPModeIdx ;

    /* This points to the array of states containing the entire
     * Pmode history for the current decom session:
     */
    efw_pmode_state   *PModeHistory ;

    /* TimeRange of current burst PMode: */
    double  BurstPModeStart ;
    double  BurstPModeEnd ;

    /* Flag indicating whether or not the current burst is
     * producing usable or timeable data:
     *     0 -> data is usable and timeable
     *   < 0 -> data is not usable or timeable.
     */
    int     BurstDataIsUsable ;

    /* Keeps track of the number of error messages a burst has
     * output.  Note that we have not begun to use this as of
     * 2000/09/10.
     */
    int     BurstErrorMessageCount ;

    /* Large accumulation buffer for records in the "Tape" or "BSD"
     * telemetry modes.  We get 10 blocks per second in these modes
     * and there is no information that indicates which block is
     * which in this 10-packet cycle (except that we can distinguish
     * the first and last blocks by their sise in 16-bit words).
     * So we have to accumulate all 10 and insure that their are
     * no skipped packets before we can process them.  Otherwise,
     * we might end up generating unsynchronized, incorrect data.
     *
     * "BStrgPtr" is a convenience pointer to the location, within 
     * "BsdStorage", for the next block.
     */
    unsigned char   *BStrgPtr ;
    uint16   BsdStorage[2000] ;

    /* Time offsets for realtime muxes.  In particular, as of
     * 2000/10/20, VL qtys are delayed by 80 ms and VM qtys
     * are delayed by 4 ms.  Note that we SUBTRACT these (non-
     * negative) values from the input ttag in "decode_main_mux".
     */
    double   MuxTTagDelays[32] ;

    /* Use for handling on-board spinfit data: */
    int     sfr_index ;
    int     sfr_collecting ;
    int     sfr_current_boom ;
    double  sfr_stime_12 ;
    double  sfr_stime_34 ;
    char    sfr_buffer[SFR_BYTES_PER_FIT + 2] ;

    int (*decode_fnc[2][HLX_NUMBER_DESCRIPTORS]) (int type,
	int idx, unsigned short *data, int nvals,
	tlm_evt_typ *event, void *vptr, double ttag) ;

    int (*extract_playback)(int type, int idx,
	unsigned short *data, int nvals, tlm_evt_typ *event,
	void *vptr, double ttag) ;
    } ;
typedef  struct  efw_decom_contents_struct  efw_decom_contents ;

/* ------------------------------------------------------------- */
/* Variables and arrays. */

/* The EFW clock rate for the four spacecraft: */
extern double EfwClockTicksPerSecond[4] ;

/* See "Cluster Command and Telemety", p 17, section 2.2.10,
 * "Burst Frequency / Sample Mode Control".
 */
extern double EfwBurstFrequencies[7] ;

/* See "Cluster Command and Telemety", p 16, section 2.2.9,
 * * "Burst Sample List Definition".   Burst Mux0,1 to Quantity
 * look-up table:
 */
extern int EfwBurstQtyMuxLookup[16][2] ;

/* Mostly for diagnostic code purposes - the names of burst qtys,
 * corresponding to the indices "EFW_BV1L", "EFW_BV2L", ...,
 * "EFW_BPWRTEMP".
 */
extern char *EfwBurstQtyMuxNames[32] ;

/* The lengths of packets (in 16-bit words) for the 10 packets
 * in each of the 3 tape (BSD) modes.  So the array is 3 x 10:
 */
extern int EfwTapeModePLen[3][10] ;

/* The total length, in 16-bit words, of records of the 4
 * telemetry modes.
 */
extern int EfwTapeModeTLen[4] ;

/* Error/warning messages related to errors in the EFW burst
 * headers:
 */
extern char *EfwBurstHdrErrMsg[EFW_BHDR_NMSGS] ;

/* ------------------------------------------------------------- */
/* Type declaration for Cluster telemetry handling functions: */
typedef void (*ClsTlmFnc) (tlm_evt_typ *, void *) ;

/* ------------------------------------------------------------- */
/* Function declations. */

extern void set_tlm_handler (void (*fnc) (tlm_evt_typ *event, void *ptr),
    void *ptr, efw_decom_contents *EfwState) ;

extern int init_tlm_decode (efw_decom_contents *EfwState,
    int TelemFormat, int Spacecraft, int TypeOfDecom) ;

extern int end_tlm_decode (efw_decom_contents *EfwState) ;

extern int initialize_efw_decom_contents (efw_decom_contents *dcnts,
    int TlmForm, int Spacecraft, int TypeOfDecom) ;

extern int clear_efw_decom_contents (efw_decom_contents *dcnts) ;

extern int copy_efw_decom_contents (efw_decom_contents *dcnts,
    efw_decom_contents *scnts) ;

extern int efw_process_clock_time (unsigned char *dsc_ptr,
    efw_decom_contents *EfwState) ;

extern int InitClusterEfwCPUClockData (efw_decom_contents *CurEfwState,
    double NSecs) ;

extern int ClearClusterEfwCPUClockData (
    efw_decom_contents *CurEfwState) ;

extern int CopyClusterEfwCPUClockData (efw_decom_contents *istate,
    efw_decom_contents *ostate) ;

extern double ClusterClockUnits (unsigned char *s1) ;

extern double ClusterCPUClockToGMT (unsigned char *clkbytes,
    efw_decom_contents  *CurEfwState) ;

extern int EfwAddPModeToHistory (efw_decom_contents *EfwState,
    int *probe_flgs, double in_ttag) ;

extern int SetEfwBurstTimingMode (int flg,
    efw_decom_contents *EfwState) ;

extern int EFW_BSDPacket (efw_decom_contents *EfwState,
    unsigned short *dptr, int DLen, double DTime, int TMode) ;

extern int DecodeEFWPacket (efw_decom_contents *EfwState,
    unsigned short *dptr, int DLen, double DTime, int TMode) ;

extern EfwInterpQtys *ConstructEfwInterpQtys (int icode) ;

extern int FillEfwInterpQtys (EfwInterpQtys *eq, int icode) ;

EfwInterpQtys *CopyConstructEfwInterpQtys (EfwInterpQtys *seq) ;

extern int CopyEfwInterpQtys (EfwInterpQtys *seq, EfwInterpQtys *deq) ;

extern int DestructEfwInterpQtys (EfwInterpQtys *eq) ;

extern int ClearEfwInterpQtys (EfwInterpQtys *eq) ;

extern int efw_unpack_sfr8 (unsigned char *sfrb,
    sfr8_data_typ *sfr8_ptr, int boom_id) ;

extern int ComputeOBSpinFitMode (efw_decom_contents *EfwState,
    int eversion) ;

extern int UpdateEVersionHistory (efw_decom_contents *EfwState) ;

extern int ProcessType13 (EfwInterpQtys *eq, double ittag,
    double ival) ;

extern int ProcessType24 (EfwInterpQtys *eq, double ittag,
    double ival, double *ntime, double *nval) ;

extern int copy_efw_tlm_descriptions (efw_decom_contents *scnts,
      efw_decom_contents *dcnts) ;

extern int copy_efw_exec_version_history (efw_decom_contents *scnts,
      efw_decom_contents *dcnts) ;

extern int SetJulianDayOfEFWSession (efw_decom_contents *EfwState,
      long JDay) ;

extern int SetReturnCPUClockCountsFlag (efw_decom_contents *EfwState,
      int flag) ;

extern int DecodeRealtimeMuxChannel (unsigned char mchan,
    efw_decom_contents *dcnts,
    char **qnames, int *nnames, int *rfnc, int *pback_ena,
    int *ram_ena) ;

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif  /* EFW_DCOM_H */

/* ------------------------------------------------------------- */
/*  end:  efw_dcom.h  */
