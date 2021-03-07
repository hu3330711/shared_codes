/* ------------------------------------------------------------- */
/*
 * thm_dcom.h
 *
 * These are the declarations required for decommutating THEMIS
 * telemetry.
 *
 */

#ifndef THM_DCOM_H
#define THM_DCOM_H

/* Here is the SCCS ID Keyword string 
 * (see page 99 of "Programming Utilities and Libraries").
 */
#define SccsId_thm_dcom_h "@(#)thm_dcom.h	1.1, 04/21/07"

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <memory.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <errno.h>
#include <dirent.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#ifdef UTILIZE_SUN_THREADS
#include <thread.h>
#else
#include <pthread.h>
#endif

#include "thm_lvl0.h"

/* ------------------------------------------------------------- */
/* Constants. */

/* ------------------------------------------------------------- */
/* Themis frame/packet decoding constants: */


/* Realtime Themis frame sync bytes: */
#define THEMIS_REALTIME_SYNC_SEQ                   0x1acffc1d

/* The size of a realtime I/O buffer in units of "doubles". */
#define  THM_SOCKET_NDBL                           3004

/* The size of a realtime I/O buffer block (into which each
 * record will be read):
 */
#define  THM_DEFAULT_RTIME_BLK_SIZE                2000

/* The number of apids that are available: */
#define  THM_NMB_APIDS                             200

/* The highest APID number of the BAU and IDPU HK packets. */
#define   THM_LAST_HK_APID           1031

/* The initial APID number.  This means that the actual apid
 * identity numbers will have a range of:
 *
 *    THM_FIRST_APID   to:  (THM_FIRST_APID + THM_NMB_APIDS - 1)
 *
 */
#define  THM_FIRST_APID                            1024

/* The Maximum THEMIS APID number:
 * This must be at least:  THM_FIRST_APID + THM_NMB_APIDS - 1
 */
#define  MAX_THEMIS_APID                           1250

/* Types of Frame "streams": */
#define  THM_FRAME_STREAM_DATA_FILE                0
#define  THM_FRAME_STREAM_SOCKET                   1
#define  THM_LEVEL_ZERO_DATA_FILES                 2

/* The standard length of all THEMIS telemetry frames - see:
 *
 *  THEMIS Telemetry Data Format Specification
 *   Draft. In Review  (thm_sys_115d.doc)
 *   January 28, 2004
 *
 *    paragraph 3.1
 */
#define  THM_FRAME_LENGTH                          1100

/* The number of total bytes in a THEMIS frame primary header: */
#define  THM_FRAME_PRIMARY_HDR_SIZE                6

/* The number of total bytes in a THEMIS frame secondary header: */
#define  THM_FRAME_SECONDARY_HDR_SIZE              6

/* The number of total bytes in a THEMIS frame (primary + secondary)
 * header:
 */
#define  THM_FRAME_HDR_SIZE                        12

/* The size of the frame CLCW section: */
#define  THM_FRAME_CLCW_SIZE                       4

/* The size of the frame data section: */
#define  THM_FRAME_DATA_SIZE                       1084

#define  THM_FRM_PHDR_PVERSION_BYTE             0
#define  THM_FRM_PHDR_PVERSION_MASK             0xc0
#define  THM_FRM_PHDR_PVERSION_SHIFT            6

#define  THM_FRM_PHDR_SCRAFT_ID_BYTE            0
#define  THM_FRM_PHDR_SCRAFT_ID_MASK            0x3ff0
#define  THM_FRM_PHDR_SCRAFT_ID_SHIFT           4

#define  THM_FRM_PHDR_VIRTUAL_CHAN_BYTE         1
#define  THM_FRM_PHDR_VIRTUAL_CHAN_MASK         0x0e
#define  THM_FRM_PHDR_VIRTUAL_CHAN_SHIFT        1

#define  THM_FRM_PHDR_OP_CTRL_FLG_BYTE          1
#define  THM_FRM_PHDR_OP_CTRL_FLG_MASK          0x01
#define  THM_FRM_PHDR_OP_CTRL_FLG_SHIFT         0

#define  THM_FRM_PHDR_MAST_CHAN_FRM_CNT_BYTE    2

#define  THM_FRM_PHDR_VIRT_CHAN_FRM_CNT_BYTE    3

#define  THM_FRM_PHDR_TF_SCND_HDR_FLG_BYTE      4
#define  THM_FRM_PHDR_TF_SCND_HDR_FLG_MASK      0x80
#define  THM_FRM_PHDR_TF_SCND_HDR_FLG_SHIFT     7

#define  THM_FRM_PHDR_SYNCH_FLAG_BYTE           4
#define  THM_FRM_PHDR_SYNCH_FLAG_MASK           0x40
#define  THM_FRM_PHDR_SYNCH_FLAG_SHIFT          6

#define  THM_FRM_PHDR_PACKET_ORDER_BYTE         4
#define  THM_FRM_PHDR_PACKET_ORDER_MASK         0x20
#define  THM_FRM_PHDR_PACKET_ORDER_SHIFT        5

#define  THM_FRM_PHDR_SEGMENT_LENGTH_ID_BYTE    4
#define  THM_FRM_PHDR_SEGMENT_LENGTH_ID_MASK    0x18
#define  THM_FRM_PHDR_SEGMENT_LENGTH_ID_SHIFT   3

#define  THM_FRM_PHDR_FRST_PKT_OFFSET_BYTE      4
#define  THM_FRM_PHDR_FRST_PKT_OFFSET_MASK      0x07ff

#define  THM_FRM_SHDR_VERSION_BYTE              0
#define  THM_FRM_SHDR_VERSION_MASK              0xc0
#define  THM_FRM_SHDR_VERSION_SHIFT             6

#define  THM_FRM_SHDR_HDR_LENGTH_BYTE           0
#define  THM_FRM_SHDR_HDR_LENGTH_MASK           0x3f
#define  THM_FRM_SHDR_HDR_LENGTH_SHIFT          0

#define  THM_FRM_SHDR_TRANSMIT_TIME_SECS_BYTE   1
#define  THM_FRM_SHDR_TRANSMIT_TIME_SSECS_BYTE  5

/* If the "FirstPacketOffset" field in the FrameHdr is this
 * value, there is no packet header in the frame.
 */
#define  THM_FRM_HAS_NO_PKT_HDR_OFFSET_FLAG     2047

/* The maximum number of bytes in a packet (Hdr + Data).  As
 * of August 2004, we allow a packet to encompass a maximum of 16
 * telemetry frames.
 */
#define  THM_PKT_MAX_SIZE                       16400

/* For ultimate byte alignment, buffers containing the
 * packet contents should be stored in a large data type -
 * we use "double".  The number of these should then be:
 *
 *   THM_PKT_MAX_SIZE / 8
 *
 */
#define  THM_PKT_MAX_IN_DOUBLE_TYPE             2050

/* The maximum number of bytes in the data section of a packet.
 * This must be set to less than "THM_PKT_MAX_SIZE - THM_PKT_HDR_SIZE"
 */
#define  THM_PKT_DATA_MAX_SIZE                  16384

/* The number of total bytes in a THEMIS packet primary header: */
#define  THM_PKT_PRIMARY_HDR_SIZE               6

/* The number of total bytes in a THEMIS frame secondary header: */
/* Be careful on usage - this ONLY refers to non-HK packets! */
#define  THM_PKT_SECONDARY_HDR_SIZE             10

/* The Secondary Header size for IDPU HK and BAU HK packets: */
#define   THM_HK_PKT_SCND_HDR_SIZE   6

/* The number of total bytes in a THEMIS packet (primary + seconday)
 * header:
 * Be careful on usage - this ONLY refers to non-HK packets!
 */
#define  THM_PKT_HDR_SIZE                       16


#define  THM_PKT_PHDR_PVERSION_BYTE             0
#define  THM_PKT_PHDR_PVERSION_MASK             0xe0
#define  THM_PKT_PHDR_PVERSION_SHIFT            5

#define  THM_PKT_PHDR_PTYPE_BYTE                0
#define  THM_PKT_PHDR_PTYPE_MASK                0x10
#define  THM_PKT_PHDR_PTYPE_SHIFT               4

#define  THM_PKT_PHDR_P_SCND_HDR_FLG_BYTE       0
#define  THM_PKT_PHDR_P_SCND_HDR_FLG_MASK       0x08
#define  THM_PKT_PHDR_P_SCND_HDR_FLG_SHIFT      3

#define  THM_PKT_PHDR_APID_BYTE_1               0
#define  THM_PKT_PHDR_APID_MASK_1               0x07
#define  THM_PKT_PHDR_APID_SHIFT_1              0

#define  THM_PKT_PHDR_APID_BYTE_2               1
#define  THM_PKT_PHDR_APID_MASK_2               0xff
#define  THM_PKT_PHDR_APID_SHIFT_2              0

#define  THM_PKT_PHDR_SEQ_CTL_GRP_BYTE          2
#define  THM_PKT_PHDR_SEQ_CTL_GRP_MASK          0xc0
#define  THM_PKT_PHDR_SEQ_CTL_GRP_SHIFT         6

#define  THM_PKT_PHDR_SEQ_CTL_CNT_BYTE_1        2
#define  THM_PKT_PHDR_SEQ_CTL_CNT_MASK_1        0x3f
#define  THM_PKT_PHDR_SEQ_CTL_CNT_SHIFT_1       0

#define  THM_PKT_PHDR_SEQ_CTL_CNT_BYTE_2        3
#define  THM_PKT_PHDR_SEQ_CTL_CNT_MASK_2        0xff
#define  THM_PKT_PHDR_SEQ_CTL_CNT_SHIFT_2       0

#define  THM_PKT_PHDR_PKT_LEN_BYTE              4

#define  THM_PKT_SHDR_TIME_SECONDS_BYTE         0
#define  THM_PKT_SHDR_TIME_SUB_SECONDS_BYTE     4

#define  THM_PKT_SHDR_COMPRESSION_FLG_BYTE      6
#define  THM_PKT_SHDR_COMPRESSION_FLG_MASK      0x80
#define  THM_PKT_SHDR_COMPRESSION_FLG_SHIFT     7

#define  THM_PKT_SHDR_CONFIG_ID_BYTE            6
#define  THM_PKT_SHDR_CONFIG_ID_MASK            0x7f
#define  THM_PKT_SHDR_CONFIG_ID_SHIFT           0

/* Note that the MASK for AVAILABLE is set up to be applied
 * AFTER the SHIFT.
 */
#define  THM_PKT_SHDR_AVAILABLE_BYTE            7
#define  THM_PKT_SHDR_AVAILABLE_MASK            0xef
#define  THM_PKT_SHDR_AVAILABLE_SHIFT           1

#define  THM_PKT_SHDR_FGM_OR_SPIN_BYTE_1        8
#define  THM_PKT_SHDR_FGM_OR_SPIN_MASK_1        0x01
#define  THM_PKT_SHDR_FGM_OR_SPIN_SHIFT_1       0

#define  THM_PKT_SHDR_FGM_OR_SPIN_BYTE_2        9

/* ------------------------------------------------------------- */
/* Default boom lengths (meters): */

#define  THM_DEFAULT_12_BOOM_LENGTH             100.0
#define  THM_DEFAULT_34_BOOM_LENGTH             100.0
#define  THM_DEFAULT_56_BOOM_LENGTH             8.0

/* ------------------------------------------------------------- */
/* Epoch times, offsets: */

/* The number of seconds in the 1900's
 * (00:00:00 Jan 1 1900  -  23:59:59 Dec 31 1999)
 */
#define THM_MSOFT_2000_EPOCH_DELTA              3155673600

/* The OLD number of seconds between 1900 and Mission Epoch Time
 * (1968/05/24)
 */
#define THM_MSOFT_MISSION_EPOCH_OLD_DELTA           2158272000

/* The number of seconds between 1900 and Mission Epoch Time
 * (2001/01/01)
 */
#define THM_MSOFT_MISSION_EPOCH_DELTA           3187296000

/* Offset, in seconds, between Microsoft epoch time (Jan 1, 1900),
 * and UNIX time (Jan 1, 1970).  This is 25567 days X 86400.
 */
#define THM_MSOFT_UNIX_EPOCH_DELTA              2208988800

/* Offset, in seconds, between Mission Epoch Time (Jan 1, 2001),
 * and UNIX time (Jan 1, 1970).  This is 11323 days X 86400.
 */
#define THM_MISSION_UNIX_EPOCH_DELTA            978307200

/* Offset, in seconds, between UNIX time
 * (Jan 1, 1970), and Jan 1, 2000.
 * This is 10957 days X 86400.
 */
#define THM_UNIX_2000_EPOCH_DELTA               946684800

/* Number of days in the 1900's
 * (Jan 1 1900 - Dec 31 1999)
 */
#define THM_DAYS_MSOFT_TO_2000                  36524

/* Number of days from Microsoft epoch time
 * until UNIX time:
 */
#define THM_DAYS_MSOFT_TO_UNIX                  25567

/* Number of days from UNIX time to Jan 1, 2000.  Note that this
 * value, plus "THM_DAYS_MSOFT_TO_UNIX", is the number of days in
 * the 20'th Century (== "THM_DAYS_MSOFT_TO_2000"):
 */
#define THM_DAYS_UNIX_TO_2000                   10957

/* The Julian Day of January 1, 2000: */
#define THM_2000_1_1_JULIAN_DAY                 2451545

/* The original Julian Day of the Mission Epoch (1968/05/24): */
#define THM_MISSION_EPOCH_JULIAN_OLD_DAY            2440001

/* The Julian Day of the Mission Epoch (2001/01/21): */
#define THM_MISSION_EPOCH_JULIAN_DAY            2451911

/* The Start Julian Day of Unix Time (1970/01/01): */
#define THM_1970_1_1_JULIAN_DAY                 2440588

/* The Start Julian Day of 1900: */
#define THM_1900_1_1_JULIAN_DAY                 2415021

/* ------------------------------------------------------------- */
/* Apid specific constants: */

/* The number of V123456 data groupings in a packet (apids 441, 442,
 * 445, 446, 449, 44A).
 */
#define THM_V123456_GRPS_PER_PKT                256

/* The number of EFld data groupings in a packet (apids 443, 447,
 * 44B).
 */
#define THM_EFLD_GRPS_PER_PKT                   512

/* The number of SCM data groupings in a packet (apids 444, 448,
 * 44C).
 */
#define THM_SCM_GRPS_PER_PKT                    512

/* The number of bytes in a SMEX hdr of a frame: */
#define THM_SMEX_HEADER_LENGTH                  10

/* The number of bytes in the Reed-Solomon section of a frame: */
#define THM_REED_SOLOMON_LENGTH                 160

/* The maximum number of socket connections for a
 * "thm_decom_session":
 */
#define THM_MAX_SOCKETS_IN_DECOM_SESSION        100

/* The maximum number of decompression algorithms.  Since this
 * is a 2-bit value, it is four.  In fact, as of 2006/12/07,
 * there are only two algorithm types defined.
 */
#define THM_MAX_NDALGS                          4

/* Number of different types of packet decompression errors,
 * which include the following types:
 *
 *    0 -> unknown decompression algorithm (not 1 or 2). 
 *    1 -> Huffman:  packet data size doesn't match size keys (-2)
 *    1 -> Delta:    original data length != a multiple of 64 bytes (-2)
 *    2 -> Huffman:  huffman_blkdecompress error (-1)
 *    2 -> Delta:    packet data size doesn't match width keys (-3)
 *
 * Note the the negative integer following the error textual
 * description, is the corresponding error return value from
 * the appropriate routine:
 *
 *    huffman_decompress
 *    delta_decompress
 *
 */
#define THM_NTYPES_DECOMPRESS_ERRS              3

#define THM_MIN_DCMPR_APID                   1024
#define THM_NMB_DCMPR_APIDS                   120

/* ------------------------------------------------------------- */

/* The Julian Day of Jan 1 1950: */
#define   JAN_1_1950_JULIAN_DAY            2433283

/* The Julian Day of Jan 1 1958: */
#define   JAN_1_1958_JULIAN_DAY            2436205

/* The Julian Day of Jan 1 1970: */
#define   JAN_1_1970_JULIAN_DAY            2440588

/* The Julian Day of Jan 1 2000: */
#define   JAN_1_2000_JULIAN_DAY            2451545

/* The Julian Day of Dec 31 2099: */
#define   DEC_31_2099_JULIAN_DAY           2488069

/* The Julian Day of Jan 1 2100: */
#define   JAN_1_2100_JULIAN_DAY            2488070

/* The Modified Julian Day (JD2000) time of Jan 1 1950 (0 GMT) : */
#define   JAN_1_1950_JD2000_TIME           -18262.0

/* The Modified Julian Day (JD2000) time of Jan 1 1958 (0 GMT) : */
#define   JAN_1_1958_JD2000_TIME           -15340.0

/* The Modified Julian Day (JD2000) time of Jan 1 1970 (0 GMT) : */
#define   JAN_1_1970_JD2000_TIME           -10957.0

/* The Modified Julian Day (JD2000) time of Jan 1 2100 (0 GMT) : */
#define   JAN_1_2100_JD2000_TIME           36525.0

/* The number of seconds between 1970/01/01 and 2000/01/01: */
#define   SECS_1970_2000                   946684800

/* Radians per Degree: */
#define   RAD_PER_DEG                      0.017453292

/* Degrees per Radian: */
#define   DEG_PER_RAD                      57.29578122

/* Earth's equatorial radius (KM): */
#define    EARTH_EQUATORIAL_RADIUS         6378.135

/* Earth's mean radius (KM): */
#define    EARTH_MEAN_RADIUS               6371.2

/* ------------------------------------------------------------- */
/* Typedefs. */

#ifndef int16
#define int16 short
#endif

#ifndef int32
#define int32 long
#endif

/* Used only to keep track of decompression statistics.  There will
 * usually be one of these structure for each Apid, DecompressAlgorithm
 * pair.
 */
struct  ThmCompressData_struct
    {
    /* The number of packets marked as compressed:
     * Note that this should equal "ok_cnt" plus the sum of
     * the counts in "ecnt".
     */
    int   ncomp ;

    /* Counts of successful decompressions: */
    int   ok_cnt ;

    /* Counts of errors: */
    int   ecnt[THM_NTYPES_DECOMPRESS_ERRS] ;
    } ;
typedef   struct  ThmCompressData_struct  ThmCompressData ;

/* Keeps a list of all Themis files required for a
 * timespan for a spacecraft.
 */
struct  ThmFileGrp_struct
    {
    int     gcnt ;
    char    **fname ;
    char    **date ;
    int     *julian_day ;
    double  *stime ;
    double  *etime ;
    } ;
typedef   struct  ThmFileGrp_struct  ThmFileGrp ;

/* The layout of the THEMIS Frame Primary Header: */
struct ThmFramePrimaryHdr_struct
    {
    unsigned char   Version ;
    unsigned short  SpacecraftId ;    /*  0x151 - 0x155 */
    unsigned char   VirtualChannel ;
    unsigned char   OperationalControlFlag ;
    unsigned char   MasterChannelFrameCount ;
    unsigned char   VirtualChannelFrameCount ;
    unsigned char   TFSecondaryHeaderFlag ;
    unsigned char   SynchFlag ;
    unsigned char   PacketOrder ;
    unsigned char   SegmentLengthId ;
    unsigned short  FirstPacketOffset ;
    unsigned char   raw[THM_FRAME_PRIMARY_HDR_SIZE] ;
    } ;
typedef  struct ThmFramePrimaryHdr_struct  ThmFramePrimaryHdr ;

/* The layout of the THEMIS Frame Secondary Header: */
struct ThmFrameSecondaryHdr_struct
    {
    unsigned char   Version ;
    unsigned char   SecondaryHeaderLength ;
    unsigned int32  TransmitTimeSeconds ;
    unsigned int32  TransmitTimeSubSeconds ;
    unsigned char   raw[THM_FRAME_SECONDARY_HDR_SIZE] ;
    } ;
typedef  struct ThmFrameSecondaryHdr_struct  ThmFrameSecondaryHdr ;

/* Information that is read out of the internal telemetry frame
 * headers:
 */
struct ThmFrameHdr_struct
    {
    /* According to:
     *
     *   THEMIS  Telemetry Data Format Specification DRAFT. In Review
     *     (January 28, 2004)
     *
     *   paragraph 3.1 (The Transfer Frame Header Fields)
     *
     * the Secondary header is optional, although it will always
     * exist for VC 2 and 3.  Therefore this flag exists to indicate
     * whether or not "SHdr" has viable data.  Note that the Primary
     * Header has an internal bit "TF Secondary Header Flag" which
     * is said to be always "1" - so it may not be a valid indicator.
     */
    int                      SecondaryHdrExists ;

    ThmFramePrimaryHdr       PHdr ;
    ThmFrameSecondaryHdr     SHdr ;
    } ;
typedef  struct ThmFrameHdr_struct  ThmFrameHdr ;

/* Frame status/information: it contains information beyond that
 * in the internal frame header that is required for processing.
 */
struct ThmFrameInfo_struct
    {
    /* The internal THM frame header: */
    ThmFrameHdr     fhdr ;

    /* The time of the frame, in UNIX time (seconds from 00:00 GMT,
     * of Jan 1, 1970), as computed from the time in "fhdr":
     */
    unsigned long   ut_secs ;
    double          ut_psecs ;

    /* Flag indicating whether or not the frame has the preceding
     * 10-byte SMEX hdr
     *     0 -> No SMEX hdr bytes.
     *     1 -> SMEX hdr bytes exist.
     */
    int             smex_hdr_flag ;

    /* Flag indicating whether or not the frame has the preceding
     * four Sync bytes:
     *     0 -> No Sync bytes.
     *     1 -> Sync bytes exist.
     */
    int             sync_bytes_flag ;

    /* Flag indicating whether or not the frame has the Reed-Solomon
     * bytes:
     *     0 -> No Reed-Solomon bytes.
     *     1 -> Reed-Solomon bytes exist.
     */
    int             reed_solomon_flag ;

    /* The SMEX hdr bytes of the frame. Only defined if "smex_hdr_flag"
     * is ON.
     */
    unsigned char   smex_hdr_bytes[THM_SMEX_HEADER_LENGTH] ;

    /* The sync bytes of the frame. Only defined if "rt_sync_flag"
     * is ON.
     */
    unsigned char   rt_sync_bytes[4] ;

    /* The data bytes (1100 of them) of the frame: */
    unsigned char   data_bytes[THM_FRAME_LENGTH] ;

    /* The Reed-Solomon bytes of the frame.  Only defined if
     * "reed_solomon_flag" is ON.
     */
    unsigned char   reed_solomon_bytes[THM_REED_SOLOMON_LENGTH] ;
    } ;
typedef  struct ThmFrameInfo_struct  ThmFrameInfo ;

/* The layout of the THEMIS Packet Primary Header: */
struct ThmPacketPrimaryHdr_struct
    {
    unsigned short  SpacecraftId ;    /*  0x151 - 0x155 */

    unsigned short  VirtualChannel ;  /*  0 - 7, 255 -> unknown */

    /* According to the document, this should always be "000". */
    unsigned char   Version ;

    /* "PktType" is as follows:
     *    0 -> packet contains data, not commands.
     * According to the document, this should always be "0".
     */
    unsigned char   PktType ;

    /* According to the document, this should always be "1",
     * indicating that the Packet Hdr does contain a secondary
     * header.
     */
    unsigned char   SecHdrFlag ;

    /* The APID to which this packet belongs. */
    int16           Apid ;

    /* According to the document, this should always be "11". */
    unsigned char   SeqCtlGrp ;

    /* Counts packets for this apid: */
    int16           SeqCtlCnt ;

    /* Length (in bytes) of the data segment of the packet AND the
     * Packet Secondary header (4 bytes) and the Collection Time
     * section (6 bytes), then minus "1".
     *
     * NOTE:  This changed in December 2004 - before, the 10 bytes
     * from the packet header were not encompassed by this number
     * and the "minus 1" was not left intact at that time.
     */
    int16           PktLen ;

    /* Length (in bytes) of the data segment of the packet.
     * In essence, this will be "Pktlen - 10 + 1".
     */
    int16           DataLen ;

    unsigned char   raw[THM_PKT_PRIMARY_HDR_SIZE] ;
    } ;
typedef   struct ThmPacketPrimaryHdr_struct  ThmPacketPrimaryHdr ;

/* The layout of the THEMIS Packet Secondary Header: */
struct ThmPacketSecondaryHdr_struct
    {
    /* The UTC collection time of the packet:
     *
     * See:
     *
     *   THEMIS  Telemetry Packet Data Format Specification 
     *     (September 25, 2003)
     *
     *   page 7,  paragraph 2.1.2.1 for details.
     */
    unsigned int32  Secs ;
    unsigned int16  SubSecs ;

    unsigned char   CompressionFlag ;

    unsigned char   ConfigId ;

    unsigned int16  Available ;

    unsigned int16  FgmOrSpin ;

    unsigned char   raw[THM_PKT_SECONDARY_HDR_SIZE] ;
    } ;
typedef   struct ThmPacketSecondaryHdr_struct  ThmPacketSecondaryHdr ;

/* Information that is read out of the packet headers: */
struct ThmPacketHdr_struct
    {
    /* According to:
     *
     *   THEMIS  Telemetry Packet Data Format Specification 
     *     (September 25, 2003)
     *
     *   paragraph 3.1 (Packet Format)
     *
     * there are two different Packet Header formats:
     *
     *    The DFB needs 9 bits of "FGM prescalers"
     *
     *    The ETC needs a "spin_number"
     *
     * The type of header is indicated in "ThmPacketHdrType"
     * as follows:
     *
     *     0  ->  DFB type (contains FGM prescalers)
     *     1  ->  ETC type (contains spin numbers)
     *
     */
    int                       ThmPacketHdrType ;

    /* Flag indicating whether or not this packet was
     * originally compressed, but has now been decompressed.
     * Note that if "CompressionFlag" in the secondary header
     * is "1", then "HasBeenDecompressed" will be "0".  If
     * the original packet was decompressed, then
     * "HasBeenDecompressed" will be "0".  The only time
     * "HasBeenDecompressed" will be "1" is if the orginal
     * packet was compressed, then was decompressed by the
     * current process (so that "CompressionFlag" is now "0").
     * The main use for this flag is statistical - it may be
     * useful to know how many packets, whether or not they've
     * been decompressed, were originally compressed.
     */
    int                       HasBeenDecompressed ;

    /* If the packet has been decompressed, (that is, the field
     * "HasBeenDecompressed" is "1"), then we store the original
     * "DataLen" (in Phdr) of the compressed packet here.  The
     * main purposed is informational - for instance, a program
     * running a stream of packets may wish to compare the data
     * sizes of the compressed packets, with the corresponding
     * sizes of the uncompressed packets.  If "HasBeenDecompressed"
     * is "0", then this value will be set to "-1" to indicate
     * that it is not defined.
     */
    int16                     CompressedDataLen ;

    /* If the packet has been decompressed ("HasBeenDecompressed"
     * is set to "1"), then this value contains the compression
     * algorithm that had been used.  The values are as follows:
     *
     *    -1 -> not a compressed packet, or not decompressed
     *     1 -> Huffman
     *     2 -> Delta
     *
     */
    int                       CompressionAlgorithm ;


    /* This indicates how many "data" bytes for this packet have
     * already been packed into data storage, while this packet
     * is being assembled from frames.
     */
    int                       ndata_bytes ;

    ThmPacketPrimaryHdr       PHdr ;
    ThmPacketSecondaryHdr     SHdr ;
    } ;
typedef  struct ThmPacketHdr_struct  ThmPacketHdr ;

/* Packet status/information: it contains information beyond that
 * in the internal packet header that is required for processing.
 */
struct ThmPacketInfo_struct
    {
    /* The internal THEMIS packet header: */
    ThmPacketHdr     PHdr ;

    /* Pointer to the packet data, beginning at the start of the
     * packet header.  The "data" section of the packet starts at:
     *
     *    php + THM_PKT_HDR_SIZE
     *
     */
    unsigned char    *php ;

    /* Pointer to the packet data: */
    unsigned char    *pdata ;

    /* Buffer containing the entire packet (Hdr AND Data):
     * Note that, to insure usable byte-alignment for all
     * types, the actual storage buffer is a "double" array, but
     * the pointer "data" is an unsigned char.  Also note that
     * the length of the Header is "THM_PKT_HDR_SIZE", which is
     * 16, so both the Hdr sections and Data Sections are aligned
     * on at least "double" data types.
     */
    unsigned char    *data ;
    double store_data[THM_PKT_MAX_IN_DOUBLE_TYPE] ;
    } ;
typedef  struct ThmPacketInfo_struct  ThmPacketInfo ;

/* Function Ptr type to defined the THEMIS packet handling
 * functions:
 */
typedef void (*ThmPacketFnc) (ThmPacketInfo *, int, void **) ;


/* This structure is used to define and keep track of the processing
 * of one or more THM data files for a given spacecraft (1 - 4), listed
 * in time-ascending order, that are of the same type.
 *
 * This structure allows us to process a "stream of frames" for like
 * data without having to worry, at higher levels of software, about
 * the time boundaries of the associated files.
 */
struct thm_frame_stream_struct
    {
    /* 1, 2, 3, or 4 */
    int   spacecraft ;

    /* Type of frame stream:  as of 2004/03/30, there is only one
     * type.
     */
    int   type ;

    /* Flag indicating whether or not to output WARNING messages.
     * Usually, this will be turned OFF.   It must be explicitly
     * turned ON to be used.
     */
    int   output_messages ;

    /* The number of files in "flist" - used only if:
     *   "type" == "THM_FRAME_STREAM_DATA_FILE"
     */
    int   nfiles ;

    /* The index, 0 ... (nfiles - 1), of the currently open file in
     * "flist".  If no files have yet been opened, this will be -1.
     */
    int   fidx ;

    /* Array of char ptrs which will store the names of the files to
     * be processed in this frame stream.  This is used only if:
     *   "type" == "THM_FRAME_STREAM_DATA_FILE"
     */
    char  **flist ;

    /* FILE ptr, used only if "type" is "THM_FRAME_STREAM_DATA_FILE",
     * to the currently open file in "flist" - only one
     * file needs to be open at any time.
     */
    FILE  *ifp ;

    /* The socket port, used only if:
     *   "type" == "THM_FRAME_STREAM_DATA_FILE"
     */
    int   ip_port ;

    /* The socket ip address of the source, used only if:
     *   "type" == "THM_FRAME_STREAM_DATA_FILE"
     */
    char  *ip_addr ;

    /* File Descriptor, used only if "type" is
     * "THM_FRAME_STREAM_SOCKET", to the current open socket.
     */
    int   sock_fd ;

    /* Flags indicating the expected format of the frames to be
     * read by this session.  As of 2006/01/01, the largest
     * frame format is as follows (in the order shown):
     *
     *     THM_SMEX_HEADER_LENGTH-byte (10) SMEX header
     *     4-byte Sync bytes
     *     THM_FRAME_LENGTH-byte (1100) data section
     *     THM_REED_SOLOMON_LENGTH-byte (160) Reed-Solomon bytes
     *
     * In order to specify what the frames of this session are
     * expected to contain, the following flags exist.  Note that
     * we allows expect the data section, so there is not flag
     * for it.   If the flag is OFF (0), we do not expect the
     * corresponding frame section.  If it is ON (1), we do.
     */
    int             smex_hdr_flag ;
    int             sync_bytes_flag ;
    int             reed_solomon_flag ;

    /* Flag indicating whether or not compressed packets are to
     * be decompressed during a frame stream session:
     *
     *     0 -> do NOT decompress packets.
     *     1 -> decompress packets.
     */
    int             decompress_flag ;

    /* The following keep count of the number of packets which
     * the stream attempted to decompress, and the number of
     * failed attempts.  Of course, the number of successful
     * attempts is "decompress_attempts - decompress_failures".
     */
    int             decompress_attempts ;
    int             decompress_failures ;

    /* The expected length of the frames.  This will be:
     *   THM_FRAME_LENGTH
     *   + THM_SMEX_HEADER_LENGTH   if "smex_hdr_flag" is ON
     *   + 4                        if "sync_bytes_flag" is ON
     *   + THM_REED_SOLOMON_LENGTH  if "reed_solomon_flag" is ON
     */
    int             bytes_per_frame ;

    /* Thread for reading from a realtime socket.  We want the
     * realtime data gathering (i.e. the socket read I/O) to run
     * asynchonously with the decom session.  It is to be hoped
     * that the decommutator will keep up with the speed of frame
     * collection from the realtime server.
     */
#ifdef UTILIZE_SUN_THREADS
    thread_t    rt_thread_id ;
#else
    pthread_t   rt_thread_id ;
#endif

    /* Flag indicating whether or not the realtime thread has
     * been invoked:
     */
    int         rt_thread_flag ;

    /* Mutex to control access to the realtime socket I/O buffer.
     * The decommutator must get the lock to extract data from
     * the buffer and the realtime gathering thread must get the
     * lock to write into the buffer.
     */
#ifdef UTILIZE_SUN_THREADS
    mutex_t           realtime_buffer_lock ;
#else
    pthread_mutex_t   realtime_buffer_lock ;
#endif

    /* The main realtime I/O buffer information:
     *
     * First note that we denote the actual TCP socket "packets"
     * received from the realtime server as:
     *
     *    frame packets
     *
     * so that we don't confuse the concept of "packet" from TCP
     * nomenclature, with that of a THEMIS "telemetry" packet.
     *
     * A "frame packet" is a TCP packet from the realtime server.
     * It is expected that it contains one THEMIS frame along with
     * other information.  As of 2005/04/19, the definition of the
     * expected "frame packet" is:
     *
     *    THM_SMEX_HEADER_LENGTH (10) byte SMEX header:
     *       In our case only the first two bytes are needed.  They
     *       contain the number of bytes in the frame in MSB format.
     *
     *    4 byte Sync Master:
     *
     *    THM_FRAME_LENGTH (1100) byte Telemetry Frame:
     *
     *    THM_REED_SOLOMON_LENGTH (160) byte Reed-Solomon encoding:
     *
     *    Total of 1274 bytes.
     *
     *    rt_buf:       The I/O buffer.  We align it on a "double"
     *                  to insure absolute alignment.  This buffer
     *                  of "doubles" is "rt_dbl".
     *
     *    rt_dbl:       An array of "doubles" to which "rt_buf"
     *                  points.  The only purpose in doing things
     *                  this way, is to assure maximum type
     *                  alignment.
     *
     *    rt_blk_size:  The size (in bytes) of each block within 
     *                  "rt_buf" that will receive realtime frame
     *                  packets.  This number must be > 0.  All
     *                  frame packets will be read into the start
     *                  of a block.  In order to insure maximum
     *                  alignment, this value should be a multiple
     *                  of 8 (the size of a double).  It should
     *                  also be big enough to assure that it can
     *                  hold any frame packet that a realtime
     *                  server will send.
     *
     *    rt_tblks      Integer specifying the number of integral
     *                  blocks (each of size "rt_blk_size") that
     *                  exist in "rt_buf".  All frame packets will
     *                  be read into "rt_buf" in one of blocks:
     *                      0 through "rt_tblks - 1".
     *
     *    rt_next_blk   Integer indicating the next block in which
     *                  a frame packet will be read.
     *
     *    rt_n_pkts     Integer indicating the total number of
     *                  frame packets received from the realtime
     *                  server.
     *              
     *    rt_out_next_blk   Integer indicating the last block which
     *                      was processed by the decommutator.
     *
     *    rt_out_n_pkts     Integer indicating the total number of
     *                      frame packets, from the realtime server,
     *                      which have been processed by the
     *                      decommutator.
     *
     *    rt_max_frm_lag    Integer indicating the maximum difference
     *                      seen, during a decom session, between 
     *                      "rt_n_pkts" and "rt_out_n_pkts".  It is
     *                      an indicator as to how well decommutation
     *                      is keeping up with the availability of
     *                      server-supplied frames.  If this number
     *                      surpasses "rt_tblks", the decommutator is
     *                      NOT keeping up.
     *
     *    rt_sync_flag      Integer indicating whether the frame
     *                      stream has synced with the realtime
     *                      data producer (1 -> synced).  By
     *                      "In Sync", we mean that the sub-thread
     *                      receiving the data has found the "Sync"
     *                      bytes (4) as defined by the constant:
     *
     *                          THEMIS_REALTIME_SYNC_SEQ
     *
     *    rt_sync_bytes     Unsigned char buf (4 bytes long) which
     *                      will contain the 4 sync bytes defined in:
     *
     *                          THEMIS_REALTIME_SYNC_SEQ
     *              
     * Regarding record size (i.e. the size of the frame packets):
     *    We could just assume that all frame packets are the same
     *    length, but, as of 2005/04/21, we  expect to get the
     *    size of the incoming frame packet from the first two
     *    bytes (MSB first).
     *
     */
    unsigned char   *rt_buf ;
    double          *rt_dbl ;
    int             rt_blk_size ;
    int             rt_tblks ;
    int             rt_next_blk ;
    int             rt_n_pkts ;
    int             rt_out_next_blk ;
    int             rt_out_n_pkts ;
    int             rt_max_frm_lag ;
    int             rt_sync_flag ;
    unsigned char   rt_sync_bytes[4] ;

    /* This array holds the current and previous frame information,
     * on an alternating basis.
     */
    ThmFrameInfo finfo[2] ;

    /* Ptrs to the "current" and "previous" frame info's, which
     * alternately point between "finfo[0]" and "finfo[1]".
     * These are pointers of convenience - they should never be
     * deleted by the destructor for this structure.
     */
    ThmFrameInfo  *cfrm ;
    ThmFrameInfo  *pfrm ;

    /* This array holds the current and previous packet information,
     * on an alternating basis.
     */
    ThmPacketInfo pinfo[2] ;

    /* Ptrs to the "current" and "previous" packet info's, which
     * alternately point between "pinfo[0]" and "pinfo[1]".
     * These are pointers of convenience - they should never be
     * deleted by the destructor for this structure.
     */
    ThmPacketInfo  *cpkt ;
    ThmPacketInfo  *ppkt ;

    /*  1 -> the previous frame ended in the middle of a packet hdr: */ 
    int            PktHdrCarriedOver ;

    /* If "PktHdrCarriedOver" is "1", this is the number of packet
     * header words carried over from the previous frame, and
     * stored in "PktHdrSavedWord":
     */
    int            NPktHdrSavedWords ;

    /* If "PktHdrCarriedOver" is "1", these are the packet header
     * words from the previous frame.  Only the first
     * "NPktHdrSavedWords" in this array were saved from the previous
     * frame.
     */
    unsigned char PktHdrSavedWord[THM_PKT_HDR_SIZE + 4] ;

    /* Main I/O buffer: */
    unsigned char isbuf[THM_FRAME_LENGTH + 32] ;

    /* Count information: */

    /* Current number of frames: */
    int  fcnt ;

    /* Current number of out-of_time_seq frames: */
    int  fcnt_tseq ;

    /* JD2000 ttag of the previous frame: */
    double  prev_frm_ltime ;

    /* JD2000 accumulated delta_t between consecutive frames: */
    double  total_delta_t ;

    /* Current number of packets (timeable and untimeable): */
    int  pcnt ;

    /* Current number of untimeable packets: */
    int  untimeable_pcnt ;

    /* The number of APIDs asked for - should be between 1
     * and "THM_NMB_APIDS":
     */
    int            NApids ;

    /* The list of APIDs requested - only the first "NApids" will
     * be used.  The APIDS will be stored in order. 
     */
    int            ApidList[THM_NMB_APIDS] ;

    /* Flag indicating if the currently building packet contains
     * skipped frames.
     */
    int            PacketHasFrameSkips ;

    /* Flag indicating whether or not we are supposed to skip
     * packets with missing data.
     */
    int            IgnorePacketsWithMissingData ;

    /* Flag indicating if the currently building packet contains

    /* Packet decompression statistics: */
    ThmCompressData   DcmpStats[THM_NMB_DCMPR_APIDS][THM_MAX_NDALGS] ;

    /* The function to process packets: */
    ThmPacketFnc     pfnc ;

    /* The number of elements in "pfnc_data" */
    int              n_pfnc_data ;

    /* Ptrs to working data that will be passed into "pfnc": */
    void             **pfnc_data ;
    } ;
typedef  struct thm_frame_stream_struct  thm_frame_stream ;

/* ------------------------------------------------------------- */

/* Linked-list node (based on "SDTListObj": */
struct thm_llist_obj_struct
    {
    void   *object ;

    struct thm_llist_obj_struct  *previous ;
    struct thm_llist_obj_struct  *next ;
    } ;
typedef   struct thm_llist_obj_struct   thm_llist_obj ;

/* Linked-list (based on "SDTListClass": */
struct thm_llist_class_struct
    {
    /* The current number of elements in the list: */
    int             nobjects ;

    /* A diagnostic feature - it indicates the maximum number of
     * elements that have ever been contained in this list.
     * Whenever "nobjects" is incremented, its new value is compared
     * to "max_objects" and, if bigger, then "max_objects" is set to
     * the new value.
     */
    int             max_objects ;

    /* A diagnostic feature - indicates the total number of elements
     * that, at one time or another, was added to the list.
     */
    int             tot_objects ;

    /* The current "head" of the list: */
    thm_llist_obj   *head ;

    /* The current "tail" of the list: */
    thm_llist_obj   *tail ;

    int (*compare)(void *obj1, void *obj2) ;
    } ;
typedef   struct thm_llist_class_struct   thm_llist_class ;

/* ------------------------------------------------------------------ */
/* Deployment (as of 2005/01/12, this is not well defined): */

/* Structure containing information about boom deployments: */
struct ThmDeployInfo_struct
    {
    /*  1, 2, 3, 4, 5 */
    int  sc ;

    /* 1 -> 12,  3 -> 34, 5 -> 56 */
    int  pair ;

    /* Flag indicating if directly measured V12 or V34 were already
     * in the telemetry:
     *     0 -> No direct V12 or V34 in telemetry 
     *     1 -> Direct V12 or V34 were in telemetry 
     */
    int  dprobe_exists ;

    double  StartBLen ;

    double  FactorBLen ;

    /* Times from 00:00 GMT of the first date of the current
     * time span.
     */
    int     StartJulianDay ;
    double  DayStartTime ;
    double  DayEndTime ;
    } ;
typedef   struct ThmDeployInfo_struct  ThmDeployInfo ;

/* Structure used with decom sessions to hold boom deployment
 * information for construction of V12. V34, V56 from single probe
 * measurements.
 */
struct ThmDeploySpan_struct
    {
    /*  1, 2, 3, 4, 5 */
    int  sc ;

    /* 1 -> 12,  3 -> 34, 5 -> 56 */
    int  pair ;

    /* Flag indicating if directly measured V12 or V34 were already
     * in the telemetry:
     *     0 -> No direct V12 or V34 in telemetry 
     *     1 -> Direct V12 or V34 were in telemetry 
     */
    int  dprobe_exists ;

    double  StartBLen ;

    double  FactorBLen ;

    /* Times from 00:00 GMT of the first date of the current
     * time span.
     */
    int     StartJulianDay ;
    double  DayStartTime ;
    double  DayEndTime ;

    double  SessionStartTime ;
    double  SessionEndTime ;
    } ;
typedef   struct ThmDeploySpan_struct  ThmDeploySpan ;

/* ------------------------------------------------------------------ */
/* Telemetry event declarations: */

/* ------------------------------------------------------------------ */
/* Constants: */

#define TEVT_V123456                            101
#define TEVT_EFLD_12_34_56                      102
#define TEVT_SCM_1_2_3                          103

/* ------------------------------------------------------------------ */
/* Structures: */

/* Structure for the V1...V6 events (Apids 441, 442, 445, 446,
 * 449, 44A).
 */
struct  V123456_data_struct
    {
    int16  V1 ;
    int16  V2 ;
    int16  V3 ;
    int16  V4 ;
    int16  V5 ;
    int16  V6 ;
    } ;
typedef struct V123456_data_struct  V123456_data ;

/* Structure for the E12, E34, E56 events (Apids 443, 447,
 * 44B).
 */
struct  EFld_data_struct
    {
    int16  E12 ;
    int16  E34 ;
    int16  E56 ;
    } ;
typedef struct EFld_data_struct  EFld_data ;

/* Structure for the SCM, SCM2, SCM3 events (Apids 444, 448,
 * 44C).
 */
struct  SCM_data_struct
    {
    int16  SCM1 ;
    int16  SCM2 ;
    int16  SCM3 ;
    } ;
typedef struct SCM_data_struct  SCM_data ;

/* TlmEvt return for Apids 441, 442, 445, 446, 449, 44A: */
struct tlm_struct_v123456_typ
    {
    int              Apid ;            /* The Apid. */
    int              WasDecompressed ; /* Flag indicating if the
					* packet required decompression.
					*/
    int              ConfigCode ;      /* Configuration Code. */
    int              Enabled[6]  ;     /* Sample Enabled flags:
				        * Enabled[0] -> V1
				        *    ...
				        * Enabled[5] -> V6
				        */
    double           STime ;   /* UNIX time of the first data pt. */
    double           DTime ;       /* TDelta between samples. */
    int              NVecs ;       /* Number of data vectors. */
    V123456_data     Data[260] ;   /* The array of data vectors. */
    ThmPacketInfo    *pkt ;        /* Ptr to the source packet. */
    } ;
typedef struct tlm_struct_v123456_typ  tlm_v123456_typ ;

/* TlmEvt return for Apids 443, 447, 44B: */
struct tlm_struct_efld_typ
    {
    int              Apid ;            /* The Apid. */
    int              WasDecompressed ; /* Flag indicating if the
					* packet required decompression.
					*/
    int              ConfigCode ;      /* Configuration Code. */
    int              AC_Enabled[3]  ;  /* Sample AC Enabled flags:
				        * AC_Enabled[0] -> E12
				        * AC_Enabled[1] -> E34
				        * AC_Enabled[2] -> E56
				        */
    int              DC_Enabled[3]  ;  /* Sample DC Enabled flags:
				        * DC_Enabled[0] -> E12
				        * DC_Enabled[1] -> E34
				        * DC_Enabled[2] -> E56
				        */
    int              AC_Coupling ;
    int              EDotB ;
    int              ExB ;
    double           STime ;   /* UNIX time of the first data pt. */
    double           DTime ;       /* TDelta between samples. */
    int              NVecs ;       /* Number of data vectors. */
    EFld_data        Data[520] ;   /* The array of data vectors. */
    ThmPacketInfo    *pkt ;        /* Ptr to the source packet. */
    } ;
typedef struct tlm_struct_efld_typ  tlm_efld_typ ;

/* TlmEvt return for Apids 444, 448, 44C: */
struct tlm_struct_scm_typ
    {
    int              Apid ;            /* The Apid. */
    int              WasDecompressed ; /* Flag indicating if the
					* packet required decompression.
					*/
    int              ConfigCode ;      /* Configuration Code. */
    int              SCM_Enabled[3] ;  /* Sample SCM Enabled flags:
				        * SCM_Enabled[0] -> SCM1
				        * SCM_Enabled[1] -> SCM2
				        * SCM_Enabled[2] -> SCM3
				        */
    int              SCMDotB ;
    int              SCMxB ;
    double           STime ;   /* UNIX time of the first data pt. */
    double           DTime ;       /* TDelta between samples. */
    int              NVecs ;       /* Number of data vectors. */
    SCM_data         Data[520] ;   /* The array of data vectors. */
    ThmPacketInfo    *pkt ;        /* Ptr to the source packet. */
    } ;
typedef struct tlm_struct_scm_typ  tlm_scm_typ ;

/* TlmEvt return for General packets - if there is ever need for
 * general information about packets, it can be handled via this
 * event:
 */
struct tlm_struct_pkt_type
    {
    ThmPacketInfo    *pkt ;        /* Ptr to the source packet. */
    } ;
typedef struct tlm_struct_pkt_type  tlm_pkt_type ;

struct tlm_struct_rdata_typ
    {
    int16            m_chan ;  /* Mux Channel. */
    int              nvals ;   /* The nmb of values (1-4) returned */
    float            value[4] ;  /* Data value(s). */
    int              VImode[4] ; /* 0 -> component is V, 1 -> is I */
    int              Calib[4] ;  /* 0 = !calibrated, 1 = calibrated */
    int16            gain ;    /* Gain bit (True -> on). */
    double           time ;    /* Time Tag of the data. */
    } ;
typedef struct tlm_struct_rdata_typ  tlm_rdata_typ ;

struct tlm_struct_deploydata_typ
    {
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

struct tlm_struct_error_typ
    {
    int32            error_code ;    /* Error code. */
    int32            severity_code ; /* Error code. */
    int32            module_code ;   /* Module code. */
    int32            line_code ;     /* Line code. */
    int32            routine_code ;  /* Routine code. */
    double           time ;          /* Time Tag of the error. */
    } ;
typedef struct tlm_struct_error_typ  tlm_error_typ ;

/* The union of various types of events: */
union  tlm_evt_union
    {
    /* DFB V1...V6:  Apids:  441, 442, 445, 446, 449, 44A: */
    tlm_v123456_typ        v123456 ;

    /* DFB E12, E34, E56:  Apids:  443, 447, 44B: */
    tlm_efld_typ           efld ;

    /* DFB SCM:  Apids:  444, 448, 44C: */
    tlm_scm_typ            scm ;

    /* Generic packet: */
    tlm_pkt_type           pkt ;

    /* Realtime data event: */
    tlm_rdata_typ          rdata ;

    /* Deployment data event: */
    tlm_deploydata_typ     deploydata ;

    /* Decommutation error. */
    tlm_error_typ          dcom_error ;
    } ;
typedef union tlm_evt_union  tlm_u_evt ;

/* This is the general event structure. */
struct  tlm_evt_struct
    {
    int                    type ;         /* Event type. */
    int                    spacecraft ;   /* Spacecraft code. */
    double                 unix_time ;    /* Unix time-tag. */
    double                 epoch_time ;   /* Mission Epoch time-tag. */
    tlm_u_evt              u ;            /* The union of types. */
    } ;
typedef struct tlm_evt_struct  tlm_evt_typ ;

typedef void (*ThmPktDataFnc) (tlm_evt_typ *, void *) ;

/* ------------------------------------------------------------------ */
/* Structure for decommutation of specified THM packets for a specified
 * spacecraft.
 *
 * When set up, the structure will be told which spacecraft and how
 * many and which APIDs are to be returned.  The number of APIDs
 * will be stored in "NApids" and the array:  ApidList
 * will be filled with the list of requested Apids.
 *
 * This structure contains two types of linked-lists:
 *
 *    A linked-list of "thm_frame_stream"'s.  Unlike CLUSTER CIS,
 *        which has three "cis_frame_streams" (NSD, BSD, HKD), THEMIS
 *        is known to have only one type - but we use this scheme to
 *        provide the infrastructure of generality that we may,
 *        unexpectedly, need later.  For instance, we may want to
 *        have a separate "thm_frame_stream" for each file, instead
 *        of putting them all in one.  As of 2004/03/30, it is not
 *        certain whether or not we will ever wish to do that.  It
 *        is also unclear if we can expect the frames within each
 *        stream to be time-ordered.  So don't count on it yet.
 *
 *    For each requested APID, a linked-list of packets.  As of
 *        2004/03/30, we don't make use of this list.  If we ever
 *        wish to queue up packets in time order (and we have no
 *        surety that packets are time-ordered in the initial
 *        telemetry stream), we can, as is done for the CLUSTER CIS
 *        code in:
 *
 *          /disks/polar4/disk1/jackv/ws/src/Cluster/CIS/history_prog
 *
 *        make use of these lists to time-order packets for the APID
 *        corresponding to each list, before outputting them.  If you
 *        do this, you have to set a maximum delay after the time of
 *        the packet before outputting it.  This works for CIS
 *        because, in that case, we emulate the CIS L1 software which
 *        only gathers for 200 spins (800 seconds).
 *    
 */
struct thm_decom_session_struct
    {
    /* Current spacecraft being decoded:   1, 2, 3, or 4 */
    int            spacecraft ;

    /* Type of decom session:
     * Must be one of:
     *
     *    THM_FRAME_STREAM_DATA_FILE
     *    THM_FRAME_STREAM_SOCKET
     *    THM_LEVEL_ZERO_DATA_FILES
     */
    int            type ;

    /* Available to externally "quit" the session: */
    int            Quit ;

    /* The number of APIDs asked for - should be between 1
     * and "THM_NMB_APIDS":
     */
    int            NApids ;

    /* The list of APIDs requested - only the first "NApids" will
     * be used.  The APIDS will be stored in order. 
     */
    int            ApidList[THM_NMB_APIDS] ;

    /* Reverse list of Apids:  For each apid:
     *
     *   ApidV == THM_FIRST_APID + Idx
     *
     * where "Idx" is in the range of 0  to: (THM_NMB_APIDS - 1),
     * the value in "ApidRev[Idx]" is defined as:
     *
     *     -1 -> Apid == ApidV is not in "ApidList".
     *
     *   >= 0 -> The index in "ApidList" where ApidV is defined.
     *
     */
    int            ApidRev[THM_NMB_APIDS] ;

    /* Keeps track of the current number of packets, of a particular
     * APID, in the linked-list of packets:
     */
    int  cur_packets[THM_NMB_APIDS] ;

    /* Keeps track of duplicate packets: */
    int  dpl_packets[THM_NMB_APIDS] ;

    /* A diagnostic feature - it can be used to keep track of the
     * maximum number of packets for this APID that have been
     * stored in a general linked-list.
     */
    int  max_packets[THM_NMB_APIDS] ;

    /* A diagnostic feature which can be used to keep track of the
     * total number of packets for this APID which have been
     * stored in a general linked-list.
     */
    int  tot_packets[THM_NMB_APIDS] ;

    /* The start and end times (in seconds since 1970) for data of
     * interest:
     */
    double         StartUnixTime ;
    double         EndUnixTime ;

    /* "UnixTimeFirstDay" and "UnixTimeEndFrame" are for
     * internal use.
     */
    /* The Unix time at midnight GMT of the first day of the
     * requested time (so "StartUnixTime" - "UnixTimeFirstDay"
     * will be a value between 0.0 and 86400.0.
     */
    double         UnixTimeFirstDay ;

    /* When we reach a frame time >= this value, we end processing: */
    double         UnixTimeEndFrame ;

    /* This is the unix time of the current frame being processed: */
    double         CurrentFrameUnixTime ;

    /* Time of the most-recently processed packet: */
    double         CurrentPacketUnixTime ;

    /* Earliest and latest unix packet times seen in this session:
     * they are initialized to -1.0.
     */
    double         EarliestPacketUnixTime ;
    double         LatestPacketUnixTime ;

    /* This will contain the full pathnames of all THM files
     * relevent to the requested timespan:
     */
    ThmFileGrp     ThmFiles ;

    /* Used if this is an LZ ("type" == THM_LEVEL_ZERO_DATA_FILES)
     * decom session.   It maintains a list of all the LZ Apid files
     * that require being read.
     */
    ThmLZFGroup     LZFiles ;

    /* The following keep count of the number of packets which
     * the decom session attempted to decompress, and the number of
     * failed attempts.  Of course, the number of successful
     * attempts is "decompress_attempts - decompress_failures".
     */
    int             decompress_attempts ;
    int             decompress_failures ;

    /* These are the actual "thm_frame_stream"'s.
     * We maintain the frame streams here.
     */
    int                 nstreams ;
    thm_frame_stream    **stream_list ;

    /* Linked-list of the "thm_frame_stream"'s.  Note that actual
     * maintenance of the streams (creating, deleting, etc.) is
     * performed on "nstreams" and "stream_list" above.  These
     * only serve as linked-list pointers.
     */
    thm_llist_class  *fstrms ;

    /* Ptr-of-convenience to the current "thm_frame_stream" that
     * is being processed from.   This pointer is set within
     * "ThemisDecodeDataSession" just before calling the routine:
     *
     *    ProcessThmFramePackets
     *
     * It exists in case "pfnc" needs to refer to the current
     * file stream for any reason.
     *
     * Note that the structure being pointed to should NOT
     * be deleted by the "Clear" and "Delete" routine for
     * "thm_decom_session".
     */
    thm_frame_stream    *cfst ;

    /* Used, when science data is to be extracted, to pass data
     * up to higher levels of an application via the function
     * pointer:
     *    pkt_data_fnc
     */
    tlm_evt_typ  t_evt ;

    /* Ptr to the function which will be fed packets from either
     * thm_frame_stream's or lz files:
     */
    ThmPacketFnc     pfnc ;

    /* The number of elements in "pfnc_data" */
    int              n_pfnc_data ;

    /* Ptrs to working data that will be passed into "pfnc": */
    void             **pfnc_data ;

    /* Packet decompression statistics: */
    ThmCompressData   DcmpStats[THM_NMB_DCMPR_APIDS][THM_MAX_NDALGS] ;

    /* Ptr to the function which data "events" will be sent to
     * higher levels of applications.  This is different
     * from "pfnc".  In most cases, the path to this function
     * call will be:
     *
     *     ProcessThmDecomSessionFramePackets (thm_dcm0.c) ->
     *     AppendDataToThmFramePackets (thm_dcm0.c) -> 
     *     pfnc ->
     *     ThmPktDataExtractor_APID (thm_ptk0.c) ->
     *     pkt_data_fnc
     *
     * Note that not all "pfnc" calls will invoke the
     * "ThmPktDataExtractor_APID" routines.  The software to
     * create the Level 0 packet files, for instance, only handle
     * the packets as whole units and do not extract individual
     * science data.
     * 
     * Also, "pkt_data_fnc" is not set by the standard constructor
     * routines for "thm_decom_session", which set it to NULL.
     * To set it to point to a function, software must call the
     * routine:
     *
     *    set_pkt_data_fnc
     *
     */
#ifdef OUT
    void (*pkt_data_fnc) (tlm_evt_typ *evt, void *ptr) ;
#endif
    ThmPktDataFnc    pkt_data_fnc ;
    } ;
typedef  struct thm_decom_session_struct  thm_decom_session ;

/* ------------------------------------------------------------- */
/* Variables and arrays. */

/* ------------------------------------------------------------- */
/* Function declations. */

extern int DecodeThmFrameHdr (unsigned char *sbuf,
    ThmFrameHdr *fhdr) ;

extern int DecodeThmFramePrimaryHdr (unsigned char *sbuf,
    ThmFramePrimaryHdr *phdr) ;

extern int DecodeThmFrameSecondaryHdr (unsigned char *sbuf,
    ThmFrameSecondaryHdr *shdr) ;

extern int PrintThmFrameHdr (ThmFrameHdr *fhdr, FILE *ofp) ;

extern int DecodeThmPacketHdr (unsigned short spc_id,
    unsigned short v_chan, unsigned char *pbuf, ThmPacketHdr *phdr) ;

extern int DecodeThmPacketPrimaryHdr (unsigned short spc_id,
    unsigned short v_chan, unsigned char *sbuf,
    ThmPacketPrimaryHdr *phdr) ;

extern int DecodeThmPacketSecondaryHdr (unsigned char *sbuf,
    ThmPacketSecondaryHdr *shdr, int Apid) ;

extern int CopyThmPacketHdr (ThmPacketHdr *iph, ThmPacketHdr *oph) ;
extern int PrintThmPacketHdr (ThmPacketHdr *phdr, FILE *ofp) ;

extern int initialize_thm_frame_stream (thm_frame_stream *fs,
    int Spacecraft, int itype, int expect_smex_hdrs,
    int expect_sync_bytes, int expect_reed_solomon_bytes,
    int decompress_pkts, int nf, char **fnames,
    int in_napids, int *apid_list, ThmPacketFnc fptr,
    int n_dptr, void **fptr_data, int sock_port, char *sock_ip) ;
extern int clear_thm_frame_stream (thm_frame_stream *fs) ;
extern int destruct_thm_frame_stream (thm_frame_stream *fs) ;
extern int copy_thm_frame_stream (thm_frame_stream *src,
    thm_frame_stream *dest) ;
extern int print_thm_frame_stream (thm_frame_stream *fs, FILE *ofp) ;

extern int  InitThmFrameInfo (ThmFrameInfo *cfi) ;
extern int  CopyThmFrameInfo (ThmFrameInfo *iinfo, ThmFrameInfo *oinfo) ;
extern int  ClearThmFrameInfo (ThmFrameInfo *cfi) ;
extern int  PrintThmFrameInfo (ThmFrameInfo *cfi, FILE *ofp) ;

extern int  InitThmPacketInfo (ThmPacketInfo *pfi) ;
extern int  CopyThmPacketInfo (ThmPacketInfo *ipk, ThmPacketInfo *opk) ;
extern int  PrintThmPacketInfo (ThmPacketInfo *pfi, FILE *ofp) ;

extern int  ProcessThmFrame (thm_frame_stream *fs) ;

extern int  ProcessThmFrameHeader (thm_frame_stream *fs) ;

extern int  ProcessThmFramePackets (thm_frame_stream *fs) ;

extern int  AppendDataToThmFramePackets (thm_frame_stream *fs,
    int nbytes, unsigned char *ibuf, ThmPacketInfo *opkt,
    int switch_cur_prev) ;

extern thm_llist_obj *Construct_thm_llist_obj (void *obj) ;

extern int Destruct_thm_llist_obj (thm_llist_obj *lptr) ;

extern thm_llist_class *Construct_thm_llist_class (int (*fptr)(void *,
    void *)) ;

extern int Destruct_thm_llist_class (thm_llist_class *listp) ;

extern int Clear_thm_llist_class (thm_llist_class *listp) ;

extern thm_llist_obj *FindObj_thm_llist_class (thm_llist_class *lclass,
    void *obj) ;

extern thm_llist_obj *GetEntryByIndex_thm_llist_class (
    thm_llist_class *lclass, int index) ;

extern int InsertObj_thm_llist_class (thm_llist_class *lclass,
    void *obj) ;

extern int InsertObjAtIndex_thm_llist_class (thm_llist_class *lclass,
    void *obj, int index) ;

extern int RemoveObj_thm_llist_class (thm_llist_class *lclass,
    void *obj) ;

extern int RemoveObjAtIndex_thm_llist_class (thm_llist_class *lclass,
    int index) ;

extern int UnlinkObj_thm_llist_class (thm_llist_class *lclass,
    thm_llist_obj *lptr) ;

extern int ReInsertObj_thm_llist_class (thm_llist_class *lclass,
    thm_llist_obj *lobj) ;

extern int Compare_thm_frame_streams (void *v1, void *v2) ;

extern int InitThmFileGrp (ThmFileGrp *cgrp) ;

extern int CopyThmFileGrp (ThmFileGrp *igrp, ThmFileGrp *ogrp) ;

extern int DestructThmFileGrp (ThmFileGrp *cgrp) ;

extern int ClearThmFileGrp (ThmFileGrp *cgrp) ;

extern int PrintThmFileGrp (ThmFileGrp *cgrp, FILE *ofp) ;

extern thm_decom_session *Construct_thm_decom_session (int sc,
    int fstype, int expect_smex_hdrs, int expect_sync_bytes,
    int expect_reed_solomon_bytes, int decompress_pkts,
    double stime, double etime,
    int in_napids, int *apid_list, int ndsets, char **fnames,
    int n_sockets, int *sock_ports, char **sock_ips,
    ThmPacketFnc fptr, int n_dptr, void **fptr_data) ;

extern int Fill_thm_decom_session (thm_decom_session *cds, int sc,
    int fstype, int expect_smex_hdrs, int expect_sync_bytes,
    int expect_reed_solomon_bytes, int decompress_pkts,
    double stime, double etime,
    int in_napids, int *apid_list, int ndsets, char **fnames,
    int n_sockets, int *sock_ports, char **sock_ips,
    ThmPacketFnc fptr, int n_dptr, void **fptr_data) ;

extern thm_decom_session *Construct1_thm_decom_session (int sc,
    int fstype, int expect_smex_hdrs, int expect_sync_bytes,
    int expect_reed_solomon_bytes, int decompress_pkts,
    double stime, double etime,
    int in_napids, int *apid_list, ThmFileGrp *fgrp,
    int n_sockets, int *sock_ports, char **sock_ips,
    ThmPacketFnc fptr, int n_dptr, void **fptr_data) ;

extern int Fill1_thm_decom_session (thm_decom_session *cds, int sc,
    int fstype, int expect_smex_hdrs, int expect_sync_bytes,
    int expect_reed_solomon_bytes, int decompress_pkts,
    double stime, double etime,
    int in_napids, int *apid_list, ThmFileGrp *fgrp,
    int n_sockets, int *sock_ports, char **sock_ips,
    ThmPacketFnc fptr, int n_dptr, void **fptr_data) ;

extern int Copy_thm_decom_session (thm_decom_session *isess,
    thm_decom_session *osess) ;

extern int Clear_thm_decom_session (thm_decom_session *cds) ;

extern int Destruct_thm_decom_session (thm_decom_session *cds) ;

extern int Print_thm_decom_session (thm_decom_session *cds,
    FILE *ofp) ;

extern void *ThemisDecodeDataSession (void *ptr) ;

extern int ThmLevelZeroDecomSession (thm_decom_session *cds) ;

extern int ThmGetNextLZPacket (FILE *ifp, int spc_id,
    ThmPacketInfo *ppkt) ;

extern int GetNextThmFrameHdr (thm_frame_stream *fs) ;

extern int ProcessThmDecomSessionFramePackets (
    thm_decom_session *cds) ;

extern double update_thm_decom_session_times (thm_decom_session *tsess,
    ThmPacketHdr *hptr) ;

extern void update_thm_decom_session_stats (thm_decom_session *tsess,
    thm_frame_stream *fst) ;

extern int set_pkt_data_fnc (thm_decom_session *tsess,
    ThmPktDataFnc fptr) ;

extern int compare_thm_frame_streams (void *v1, void *v2) ;

extern int Convert_1900_To_JDay_Time (unsigned int32 isecs,
    unsigned int32 usecs, long *jday, double *dsecs) ;

extern int SDTTimeStampToThemisTime (int JulianDay,
    double DayTime, double *PTime) ;

extern int ThemisTimeToSDTTimeStamp (double PTime,
    int *JulianDay, double *DayTime) ;

extern int ThemisRawSpcIdxToOfficialSpcIdx (int raw_idx) ;

extern int GetUnixTimeSpanOfThemisGSEFile (char *fname,
    unsigned int32 *SSecs, unsigned int32 *SMicros,
    unsigned int32 *ESecs, unsigned int32 *EMicros) ;

extern void ThmGSEUnixStartEndSecs (ThmPacketInfo *tinfo, int n_ptr,
    void **ptr) ;

extern thm_decom_session *ThmCreateGSEScienceDataSession (int sc,
    double stime, double etime, int in_napids, int *apid_list,
    int type, char *fname, int in_nsocks, int *in_ports,
    char **in_ip_addrs, void *aptr, ThmPktDataFnc fs_ptr) ;

extern int ThmFillGSEScienceDataSession (thm_decom_session *tsess,
    int sc, double stime, double etime, int in_napids,
    int *apid_list, int type, char *fname,
    int in_nsocks, int *in_ports, char **in_ip_addrs,
    void *aptr, ThmPktDataFnc fs_ptr) ;

/* --------------------------------------------------------------- */
/* Forward declarations of functions in "thm_pkt0.c": */

extern void ThmExtractScienceData (ThmPacketInfo *pkt, int nptr,
    void **ptr) ;

extern void ThmPktDataExtracter_448 (ThmPacketInfo *pkt, int nptr,
    void **ptr) ;

extern void Thm_V123456_PktExtractor (ThmPacketInfo *pkt, int nptr,
    void **ptr) ;

extern void Thm_EFld_PktExtractor (ThmPacketInfo *pkt, int nptr,
    void **ptr) ;

extern void Thm_Scm_PktExtractor (ThmPacketInfo *pkt, int nptr,
    void **ptr) ;

extern void Thm_Spectra_PktExtractor (ThmPacketInfo *pkt, int nptr,
    void **ptr) ;

extern int ConvertThmPktTime (ThmPacketHdr *phdr, double *tunix,
    double *tepoch) ;

extern double GetThmPktPSecs (unsigned int16 ival) ;

extern int GetThmPktCompressionFlag (ThmPacketInfo *pkt,
    int *compress_flg) ;

extern int GetThmPktConfigCode (ThmPacketInfo *pkt, int *config_code) ;

extern int GetThmDFBSpeedSelect (ThmPacketInfo *pkt, double *rval,
    double *tdelta) ;

extern int GetThmDFBV123456Enabled (ThmPacketInfo *pkt, int *eflgs) ;

extern int GetThmDFBEFld_AC_DC_Enabled (ThmPacketInfo *pkt,
    int *ac_flgs, int *dc_flgs, int *ac_cpl, int *edotb, int *exb) ;

extern int GetThmDFBSCMEnabled (ThmPacketInfo *pkt, int *scm_flgs,
    int *scmdotb, int *scmxb) ;

extern int pkt_decompress (ThmPacketInfo *cmp, ThmPacketInfo *raw,
    thm_frame_stream *fst) ;

extern long ThmConvertCalendarToJulianDay (int year, int month,
    int day) ;

extern int ThmConvertJulianDayToCalendar (long julian_day,
    int *year, int *month, int *day, int *day_of_year) ;

extern int ThmConvertTimeToDateTime (double thm_time,
    int *year, int *month, int *mday, int *yday, int *jday,
    int *hour, int *minute, int *second, int *usec) ;

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif  /* THM_DCOM_H */

/* ------------------------------------------------------------- */
/*  end:  thm_dcom.h  */
