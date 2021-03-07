/*
 * @(#)ted.h	1.2, 05/12/06
 *
 * Project:         CLUSTER WEC TED Package
 *
 * Item Name:       src/ted/ted.h
 *
 * Item Version:    (see RCS header)
 *
 * Item Type:       c-header
 *
 * Author:          Toby Champion
 *
 * Modified for thread-safeness and SCCS (98/07):  Jack Vernetti SSL/UCB
 *
 */
#define  SccsId_ted_h  "@(#)ted.h	1.2, 05/12/06"

#ifndef _ted_ted_h
#define _ted_ted_h

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include <stdio.h>
#include <setjmp.h>

#ifdef TRUE
#undef TRUE
#endif
#define TRUE 1

#ifdef FALSE
#undef FALSE
#endif
#define FALSE 0

#ifdef NULL
#undef NULL
#endif
#define NULL 0

/* Maximum size of DSD header in words */
#define MAXDSDHEADER 32

/* Size of DDS Packet Header in bytes */
#define DDSP_HEADER_SIZE 15

/* Size of WEC HK block in bytes */
#define HKBLOCK_SIZE 192

/* Size of largest possible NSD packet (including header) in bytes */
#define MAX_PACKET_NSD (DDSP_HEADER_SIZE + 10 * 336)
 
/* Size of largest possible BSD packets (including header) in bytes */
#define MAX_PACKET_BSD (DDSP_HEADER_SIZE + 62 * 948)
  
/* Size of HKD packets (including header) in bytes */
#define MAX_PACKET_HKD (DDSP_HEADER_SIZE + 192)

/* Maximum length of science packet (inc timetag) in words */
#define SCIENCEPACKET_MAX ((474-1)*62)

/* Size of DDS Packet Header in bytes */
#define DDSP_HEADER_SIZE 15

/* Size of WEC HK block in bytes */
#define HKBLOCK_SIZE 192

#define WORD_SIZE 2

/* Size of DDS Packet Timestamp in bytes */
#define DDSP_TIMESTAMP_SIZE 8

typedef unsigned short WORD;

typedef int            BOOL ;

typedef unsigned char  BYTE ;

typedef enum {
  TED_OK,
  TED_UNIMPLEMENTED,
  TED_MEMORY,
  TED_READ_SIZE,
  TED_READ_EARLIER,
  TED_READ_WRONGTYPE,
  TED_READ_EOF,
  TED_READ_FILE,
  TED_UNPACK_SPACECRAFT,
  TED_UNPACK_DIAGEND,
  TED_UNPACK_FATAL,
  TED_UNPACK_WRONGTYPE,
  TED_CONVERT_SOURCEINVALID,
  TED_CONVERT_SPACECRAFTINVALID,
  TED_CONVERT_SPACECRAFTMATCH,
  TED_CONVERT_STREAMINVALID,
  TED_CONVERT_GROUNDINVALID,
  TED_CONVERT_CHANNELTYPE_MATCH,
  TED_CONVERT_QUALITYINVALID,
  TED_CONVERT_SEQUENCEINVALID,
  TED_CONVERT_DDSP_WRITE_HEADER_ERR,
  TED_CONVERT_DDSP_WRITE_DATA_ERR,
  TED_CONVERT_TMP_WRITE_HEADER_ERR,
  TED_CONVERT_TMP_WRITE_DATA_ERR,
  TED_CONVERT_TOOLONG,
  TED_CONVERT_WRONGVERSION,
  TED_CONVERT_WRONGTYPE,
  TED_CONVERT_SEGWRONG,
  TED_CONVERT_APIDINVALID,
  TED_CONVERT_FLAGWRONG,
  TED_CONVERT_PFIELDWRONG,
  TED_DSD_DEFINITIONSETINVALID,
  TED_NOTINITIALISED,
  TED_TSESS_NOT_VALID,
  TED_HKEXTRACT_PARSE_HPD_ARGS,
  TED_HKEXTRACT_PARSE_HPD_HEADER,
  TED_HKEXTRACT_PARSE_HPD_DATA,
  TED_HKEXTRACT_PARSE_ALIASES_ARGS,
  TED_HKEXTRACT_PARSE_ALIASES_TABLE,
  TED_HKEXTRACT_PARSE_ALIASES_STRING,
  TED_HKEXTRACT_PARSE_ALIASES_DATA,
  TED_HKEXTRACT_PARSE_ALIASES_SYMBOL,
  TED_HKEXTRACT_VALIDATE_HPD_HEADER_ARGS,
  TED_HKEXTRACT_VALIDATE_HPD_HEADER_ADID,
  TED_HKEXTRACT_VALIDATE_HPD_HEADER_SCID,
  TED_HKEXTRACT_VALIDATE_HPD_HEADER_STREAM,
  TED_HKEXTRACT_VALIDATE_HPD_HEADER_GSID,
  TED_HKEXTRACT_VALIDATE_HPD_HEADER_SCID_ADID_MATCH,
  TED_HKEXTRACT_VALIDATE_HPD_HEADER_TCAL,
  TED_HKEXTRACT_VALIDATE_HPD_HEADER_TASI,
  TED_HKEXTRACT_VALIDATE_HKD_HEADER_ARGS,
  TED_HKEXTRACT_VALIDATE_HKD_HEADER_ADID,
  TED_HKEXTRACT_VALIDATE_HKD_HEADER_STREAM,
  TED_HKEXTRACT_VALIDATE_HKD_HEADER_GSID,
  TED_HKEXTRACT_VALIDATE_HKD_HEADER_TCAL,
  TED_HKEXTRACT_VALIDATE_HKD_HEADER_TASI,
  TED_HKEXTRACT_VALIDATE_HKD_HEADER_SCID,
  TED_HKEXTRACT_VALIDATE_HKD_HEADER_SCID_ADID_MATCH,
  TED_HKEXTRACT_VALIDATE_HKD_HEADER_STREAM_ADID_MATCH,
  TED_HKEXTRACT_VALIDATE_DDS_HEADER_ARGS,
  TED_HKEXTRACT_VALIDATE_DDS_HEADER,
  TED_HKEXTRACT_PARSE_HPD_DATA_ARGS,
  TED_HKEXTRACT_PARSE_HPD_DATA_SYMBOL,
  TED_HKEXTRACT_PARSE_HPD_DATA_LEX_ERROR,
  TED_HKEXTRACT_BUILD_TABLE_ARGS,
  TED_HKEXTRACT_BUILD_TABLE_HPD_HEADER,
  TED_HKEXTRACT_CHECK_CORR_ARGS,
  TED_HKEXTRACT_CHECK_CORR_HPD_HEADER,
  TED_HKEXTRACT_CHECK_CORR_HKD_HEADER,
  TED_HKEXTRACT_CHECK_CORR_SCID,
  TED_HKEXTRACT_CHECK_CORR_ADID_MATCH,
  TED_HKEXTRACT_CHECK_SCET_TIME_RULE,
  TED_HKEXTRACT_LOOKUP_PARAM_ARGS,
  TED_HKEXTRACT_LOOKUP_PARAM_MNEM,
  TED_HKEXTRACT_LOOKUP_PARAM_TASI,
  TED_HKEXTRACT_LOOKUP_PARAM_NOT_FOUND,
  TED_HKEXTRACT_DECODE_PARAM_ARGS,
  TED_HKEXTRACT_DECODE_PARAM_SYMBOL_WIDTH,
  TED_HKEXTRACT_DECODE_PARAM_VALUE_WIDTH,
  TED_HKEXTRACT_DECODE_PARAM_SYMBOL_OFFBY,
  TED_HKEXTRACT_DECODE_PARAM_VALUE_OFFBY,
  TED_HKEXTRACT_DECODE_PARAM_SYMBOL_OFFBI,
  TED_HKEXTRACT_DECODE_PARAM_VALUE_OFFBI,
  TED_HKEXTRACT_DECODE_PARAM_WIDTH_RANGE,
  TED_HKEXTRACT_DECODE_PARAM_OFFBY_RANGE,
  TED_HKEXTRACT_DECODE_PARAM_OFFBI_RANGE,
  TED_HKEXTRACT_CALIBRATE_ARGS,
  TED_HKEXTRACT_CALIBRATE_SYMBOL_CALINF,
  TED_HKEXTRACT_CALIBRATE_SYMBOL_UNIT,
  TED_HKEXTRACT_CALIBRATE_SYMBOL_CATEG,
  TED_HKEXTRACT_CALIBRATE_SYMBOL_DESCR,
  TED_HKEXTRACT_CALIBRATE_NO_MAPPING,
  TED_HKEXTRACT_EXTRACT_ARGS,
  TED_HKEXTRACT_EXTRACT_HKD_HEADER,
  TED_HKEXTRACT_EXTRACT_TABLE_AND_PACKET,
  TED_HKEXTRACT_EXTRACT_LOOKUP,
  TED_HKEXTRACT_EXTRACT_DECODE,
  TED_HKEXTRACT_EXTRACT_CALIBRATE,
  TED_HKEXTRACT_PARSE_AND_BUILD_ARGS,
  TED_HKEXTRACT_PARSE_AND_BUILD_HPD,
  TED_HKEXTRACT_PARSE_AND_BUILD_ALIASES,
  TED_HKEXTRACT_PARSE_AND_BUILD_TABLE
} TED_STATUS;

typedef enum {
  TED_DIAG_DDSSOURCEINVALID,
  TED_DIAG_DDSSPACECRAFTINVALID,
  TED_DIAG_DDSSPACECRAFTMATCH,
  TED_DIAG_DDSSTREAMINVALID,
  TED_DIAG_DDSGROUNDINVALID,
  TED_DIAG_DDSCHANNELTYPEMATCH,
  TED_DIAG_DDSQUALITYINVALID,
  TED_DIAG_DDSSEQUENCEINVALID,
  TED_DIAG_DDSSPACECRAFTWRONG,
  TED_DIAG_DDSDATATYPEWRONG,
  TED_DIAG_DDSTIMESAME,
  TED_DIAG_HKLENGTHWRONG,
  TED_DIAG_HKEW5MOTAGINVALID,
  TED_DIAG_HKEW5MOTAGMATCH,
  TED_DIAG_DATALENGTHWRONG,
  TED_DIAG_MPTRUNCATED,
  TED_DIAG_MPSOURCEINVALID,
  TED_DIAG_MPSOURCEWRONG,
  TED_DIAG_MPCOUNTWRONG,
  TED_DIAG_MPLENGTHEXTENDS,
  TED_DIAG_MPMIDDLEFOUND,
  TED_DIAG_SPTOOLONG,
  TED_DIAG_MPLENGTHZERO
} TED_UNPACK_DIAGCODE;

typedef enum {
  CLUSTER_X,
  CLUSTER_1,
  CLUSTER_2,
  CLUSTER_3,
  CLUSTER_4
  } TED_SPACECRAFT;

typedef enum {
  TED_TRANSMISSION_REALTIME,
  TED_TRANSMISSION_PLAYBACK,
  TED_TRANSMISSION_RECALL,
  TED_TRANSMISSION_RECALLPLAYBACK
  } TED_TRANSMISSION;

typedef enum {
  TED_QUALITY_ACTUAL,
  TED_QUALITY_EXTRAPOLATED,
  TED_QUALITY_CONTINGENCY
  } TED_QUALITY;

typedef enum {
  TED_GROUND_ODENWALD,
  TED_GROUND_REDU,
  TED_GROUND_KOUROU,
  TED_GROUND_PERTH,
  TED_GROUND_MALINDI,
  TED_GROUND_CANBERRA,
  TED_GROUND_GOLDSTONE,
  TED_GROUND_VILLAFRANCA2,
  TED_GROUND_MASPALOMAS,
  TED_GROUND_NA
  } TED_GROUND;

typedef enum {
  nsd = 1,
  bsd = 2,
  hkd = 3
} filenumber;

typedef struct {
  int day;	/* Number of days since 1 Jan 1958 */
  int ms;	/* Milliseconds of day */
  int us;	/* Microseconds of millisecond */
} TED_TIME;

typedef struct {
  TED_TIME time;		/* SCET of relevant packet */
  TED_UNPACK_DIAGCODE code;	/* Diagnostic code */
  char *details;		/* Details or NULL */
} TED_UNPACK_DIAG;

typedef struct {
  WORD version;
  WORD revision;
  WORD patch;
  WORD userpatch;
  WORD spacecraft;
  WORD ground;
  WORD source;
  WORD dw;
  WORD length;
  WORD year;
  WORD month;
  WORD day;
  WORD hour;
  WORD min;
  WORD sec;
  WORD msec;
  WORD usec;
  WORD dwpcount;
  WORD obdhmod8;
  WORD udef0;
  WORD udef1;
} TED_DSD_ITEMS;

/* Stuff for ted_unpack_decom() */
struct packet_info_struct {
  int transmission_mode;        /* eg RealTime */
  int virtual_channel;          /* eg VC2 */
  int data_type;                /* eg NSD */
  int acquisition_sequence;     /* eg NM2 */
  int acquisition_mode;         /* eg NormalMode2 */
  } ;
typedef struct packet_info_struct packet_info ;

/* Stuff for the science packet buffer */
struct ted_science_struct {
  unsigned short packet[SCIENCEPACKET_MAX];        /* The packet */
  int length;                   /* Length in 16-bit words */
  } ;
typedef struct ted_science_struct ted_science ;

/* TED Version */
struct ted_vrsn_struct {
  int version;
  int revision;
  int patch;
  int userpatch;
  } ;
typedef struct ted_vrsn_struct ted_vrsn ;

/* Options specified to ted_unpack_init() */

struct initial_options_struct {
  int spacecraft;
  int instruments;
  int ccs_converted;
  int model_tag_table[5];
  } ;
typedef struct initial_options_struct initial_options ;


struct unpack_reset_info_struct {
  int append_science;
  int ted_unpack_decom;
  int process_packet_header;
  int process_spacecraft_block;
  int process_exppacket;
  int process_hkpacket;
  } ;
typedef struct unpack_reset_info_struct unpack_reset_info ;

struct hkentry_struct {
  /* HKD packet containing HK block */
  unsigned char packet[DDSP_HEADER_SIZE + HKBLOCK_SIZE];

  /* TRUE iff there is a HK block in this entry */
  int gotit;

  /* TRUE iff DDS timestamp is same as previous in file */
  int timesame;
  } ;
typedef struct hkentry_struct hkentry ;

struct TedTable_struct {
  int max ;               /* Size of largest possible packet, in bytes */
  FILE *stream;           /* Stream */
  unsigned char *packet;  /* Packet */
  int next;               /* (See algorithm documentation) */
  int needs_sucking;      /* TRUE iff this file needs sucking */
  int been_read;     /* TRUE iff a packet has been read from this file */
  } ;
typedef struct TedTable_struct TedTable ;

/* Stuff for the diagnostics queue */

struct queue_element_struct {
  TED_UNPACK_DIAG diagnostic;
  struct queue_element_struct *next;
  };
typedef struct queue_element_struct queue_element;

struct ted_queue_struct {
  queue_element *front;
  queue_element *rear;
  } ;
typedef struct ted_queue_struct ted_queue ;

struct SeqCountInfo_struct {
  int    convert_tmph_seq_hk ;
  int    convert_tmph_seq_n1 ;
  int    convert_tmph_seq_n2 ;
  int    convert_tmph_seq_n3 ;
  int    convert_tmph_seq_b1 ;
  int    convert_tmph_seq_b2 ;
  int    convert_tmph_seq_b3 ;
  } ;
typedef struct SeqCountInfo_struct SeqCountInfo ;

struct TedSession_struct {
  int             status ;
  int             debug_level ;
  int             definition_set ;
  int             ted_read_file_no ;

  int             ted_read_packet_flag ;

  /* TRUE iff we've seen a HK packet */
  int             had_hk_packet ;

  /* What to do in the next "ted_unpack_decom" call: */
  int             unpack_state ;

  /* TRUE iff we have a buffered experiment packet */
  int             have_buffered ;

  /* TRUE iff this is the first packet received since a call to init() */
  int             first_packet ;

  /* Timestamp of previous packet if above is TRUE */
  TED_TIME        previous_hk_timestamp ;

  /* Receipt time of most recent housekeeping packet
   * read, or all zeros if none read yet.
   */
  TED_TIME        last_hk_time;

  /* Receipt time of most recent experiment packet
   * read, or all zeros if none read yet.
   */
  TED_TIME        last_experiment_time;

  /* Timestamp from most recent corresponding HK block
   * returned by ted_unpack_decom()
   */
  unsigned char   last_corresponding_hk_timestamp[8];

  /* NB Subscripts of following are 0-2 */
  /* TRUE iff got a packet for file n+1 to return */
  int             gotpacket[3] ;

  unsigned char   zero_hkblock[HKBLOCK_SIZE] ;
  WORD            dsdheader[MAXDSDHEADER];

  unsigned char   nsd_packet[MAX_PACKET_NSD] ;
  unsigned char   bsd_packet[MAX_PACKET_BSD] ;
  unsigned char   hkd_packet[MAX_PACKET_HKD] ;
  unsigned char   *mypacket[3] ;

  /* Buffered experiment packet iff have_buffered=TRUE */
  unsigned char   *buffered_exppacket ;

  TedTable        table[4] ;

  hkentry         hktable[8] ;

  unpack_reset_info  reset_unpack_info ;

  ted_vrsn        ted_version ;

  initial_options init_options;

  ted_science     science ;

  /* Information on NSD/BSD packet */
  packet_info     expinfo;

  /* Information on HKD packet */
  packet_info     hkinfo;

  /* Buffered experiment packet info iff have_buffered=TRUE */
  packet_info     buffered_expinfo;

  /* The DSD Header Items we'll pass back */
  TED_DSD_ITEMS   dsd_items;

  /* Time of previous reset pulse, or 0 if there was none */
  TED_TIME        previous_time;

  /* Similar to above but for debugging and represented in seconds */
  double          previous_lv0_time ;

  /* Reset pulse count of previous pulse, or -1 if there was none */
  int             previous_count ;

  /* Timestamp (us) from previous LV1 packet per instrument,
   * or 0 if none
   */
  double          previous_lv1_time[6] ;

  /* The following are indexed by DataFileType (NSD or BSD) */
  int             had_exp_packet[2] ;

  /* Timestamp of previous packet if above is TRUE */
  TED_TIME        previous_exp_timestamp[2] ;

  /* Stuff for the diagnostics queue */
  ted_queue       queue ;

  /* 1 iff the previous call to "process_exppacket" returned again=1 */
  int             wanted_again ;

  /* Number of block we're on: */
  int             block_number ;

  int             number_of_blocks ;

  int             block_length ;

 /* fStartingNewBlock is set on entry to the function if we're looking at
  * a scb for the first time. Otherwise, we're already in it * (perhaps
  * even still at the start).
  */
  BOOL            fStartingNewBlock ;

  /* iscbMpCur stores the index into the scb of the current mp. mpCur,
   * inside the do() loop, is computed from this each call. Note that for
   * historical reasons, it was deemed insufficient just to store mpCur,
   * because the function preconditions to ted_unpack_decom() -- which
   * prior to SMR-53 didn't make its own copy of "exppacket" to work from
   * -- do not guarantee that the experiment packet will be in the same
   * position in memory for each call. cmpCur is maintained just for use
   * by show_minipacket_info().
   */
  int             iscbMpCur ;

  /* count of this mp within the block */
  int             cmpCur ;

  /* fInReassemblySequence is TRUE iff we are in a re-assembly
   * sequence. Note that for our purposes, a SINGLE mini-packet is a
   * re-assembly sequence all on its own.
   */
  BOOL             fInReassemblySequence ;

  /* fAwaitingSecondMp is TRUE iff we are awaiting the second mp of a new
   * re-assembly sequence. It's necessary because the sequence count must
   * be started from that in the first MIDDLE/LAST packet -- it is not
   * defined as starting at zero (though it apparently always (does).
   * nExpectedSequenceCount records the value expected of the count
   * in any subsequent mp.
   */
  BOOL              fAwaitingSecondMp ;

  int               nExpectedSequenceCount ;

  /* mpFirstDescriptor records the descriptor (the first word) of the
   * first mp in a re-assembly sequence, ready for inclusion in
   * "pitms". The "source" field is used for the MPSOURCEINVALID and
   * MPSOURCEWRONG checks, too.
   */
  BYTE              mpFirstDescriptor[WORD_SIZE];

  /* Debug flag: */
  int               debug ;

  /* Non-local jump target for ted_unpack_decom() */
  jmp_buf           return_from_ted_unpack_decom;

  /* number of lookahead file or 0 if not looked at yet or
   * -<status> if problem.
   */
  filenumber         lookahead;

  /* subscript into mypacket[] of last gotpacket */
  int                last_lookahead ;

 } ;
typedef struct TedSession_struct TedSession ;

/* --------------------------------------------------------------- */
/* Exported functions: */

extern TED_STATUS TedSessionInit (TedSession *tsess) ;
extern TED_STATUS TedSessionClear (TedSession *tsess) ;
extern int PrintTedSession (TedSession *tsess, FILE *ofp) ;

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /*!_ted_ted_h*/
