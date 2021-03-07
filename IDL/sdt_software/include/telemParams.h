/* ---------------------------  telemParams.h -------------------------------
 * - header containing telem paramerters and constatnts for FAST programs  --*/

#ifndef _TELEMPARAMSH_
#define _TELEMPARAMSH_
static char sccsidTelemParamsH[] = "@(#)telemParams.h	1.3 09/01/93 UCB SSL";

/*-------------------------------------------------------------------------
 * telemetry sizes and ranges
 */

#define SHM_SIZE (ANOT_HDR_SIZE + CCSDS_SIZE + PDU_LEN) /* shm block size */

#define MESS_HDR_SIZE 16            /* size in bytes of message header */
#define ANOT_HDR_SIZE 12            /* size in bytes of anotation header */
#define CCSDS_SIZE 6                /* size of CCSDS header */
#define PDU_LEN 1048                /* packet data len (data + sec hdr) */
#define SEC_HDR_SIZE 8		    /* secondary hdr. size (incl. in PDU len) */
#define DATA_HDR_SIZE 14            /* data hdr size */
#define ERR_STAT_SIZE 2             /* error statistics word */

/*--------------------------------------------------------------------------
 * telemetry identifier constants 
 */

#define M_CLASS 40	       	    /* message class: 40: telemetry data */
#define M_TYPE_NORM 0		    /* message type 0: normal message w/ PDU */
#define M_TYPE_LAST 1		    /* message type 1: last message (18 bytes 
				     * long) */
#define M_SUBTYPE1 0x5cf7	    /* first (high) word of message subtype */
#define M_SUBTYPE2 0xa6d3	    /* second (low) word of message subtype */
#define M_SUBTYPE_L 0x5cf7a6d3L     /* longword version of the above */

#define S_C_ID 179		    /* FAST spacecraft ID */

#define MAX_APP_ID_SEQ 0x3fff       /* max 14-bit number for apid seq count */

#define SEC_HDR_ID 0x07		    /* secondary header ID */
#define	TIME_PREAMBLE 0x2e	    /* time preamble in secondary header */

/*--------------------------------------------------------------------------
 * some macros to get interesting and necessary values from the data stream 
 */

/* message -- good for setting or getting because don't involve masking, etc. */
#define MESS_LEN(Buf) (*((unsigned short *)(Buf)))
#define MESS_TYPE(Buf) (*( (unsigned short *)( (Buf)+4 ) ))
#define MESS_SEQ(Buf) (*( (unsigned short *)( (Buf)+6 ) ))
#define PDU_COUNT(Buf) (*( (unsigned short *)( (Buf)+12 ) ))

/* PDU */
#define GET_PDU_LEN(Buf) (*((unsigned short *)((Buf) + ANOT_HDR_SIZE + 4)) +1)
#define GET_APP_ID(Buf) ((*((unsigned short *)((Buf) + ANOT_HDR_SIZE)))&0x7ff)
#define GET_VC_ID(Buf) ((*((unsigned short *) Buf) >> 1 ) & 0x0007)
#define GET_SEQ_CTR(Buf) ((*((unsigned short*)((Buf)+ANOT_HDR_SIZE+2))) & \
			  MAX_APP_ID_SEQ)

/*--------------------------------------------------------------------------
 * some information about the apid's we'll be dealing with
 */

#define MIN_MIN_APP_ID 1024
#define MAX_N_APP_IDS 100

#define N_VC_IDS 8

#endif   /*_TELEMPARAMSH_*/
