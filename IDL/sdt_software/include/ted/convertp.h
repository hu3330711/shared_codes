/*
 * @(#)convertp.h	1.1, 05/23/00
 *
 * Project:         CLUSTER WEC TED Package
 *
 * Item Name:       src/lib/convertp.h
 *
 * Item Version:    (see RCS header)
 *
 * Item Type:       c-header
 *
 * Author:          Simon Davis
 *
 * Modified for thread-safeness and SCCS (98/07):  Jack Vernetti SSL/UCB
 *
 */
#define  SccsId_convertp_h  "@(#)convertp.h	1.1, 05/23/00"

#ifndef convertP_h
#define convertP_h

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include "ted.h"
#include "convert.h"

#define CONVERT_TMP_DDSP_TIME_DIFF 378691200
#define CONVERT_DDSP_FSEC_RATIO  (0x1000000 / (double) 1000000)
#define CONVERT_ITEM_SIZE 1

/* {{{ convert-spacecraft-enumeration */

enum CONVERT_CLUSTER_SCID {
  CONVERT_CLUSTER_DEFAULT_SCID = 1,
  CONVERT_CLUSTER_SCID_NA = -1,
  CONVERT_CLUSTER_SCID_1 = 1,
  CONVERT_CLUSTER_SCID_2 = 2,
  CONVERT_CLUSTER_SCID_3 = 3,
  CONVERT_CLUSTER_SCID_4 = 4
  };
typedef enum CONVERT_CLUSTER_SCID CONVERT_CLUSTER_SCID;

/* }}} */
/* {{{ convert-status-enumeration */

typedef enum {
  CONVERT_ERR_OK,
  CONVERT_ERR_DDSP_WRITE_DATA,
  CONVERT_ERR_DDSP_WRITE_HEADER,
  CONVERT_ERR_TMP_DATA_WRITE,
  CONVERT_ERR_TMPH_WRITE
  } CONVERT_STATUS;

/* }}} */
/* {{{ tmp-field-type-definitions */

typedef struct
{
  long tv_sec;
  long tv_usec;
} CONVERT_TMP_PDF_TIME;

typedef char * CONVERT_TMP_DATA;
typedef int CONVERT_TMP_LEN;
typedef int CONVERT_TMPH_SEQ;
typedef unsigned char * CONVERT_TM_PACKET;
typedef int CONVERT_TMPH_LEN;

#define CONVERT_TMP_PDF_LEN 8
#define CONVERT_TMPH_SIZE 6
#define CONVERT_TMP_MAX_LEN 65536 + CONVERT_TMP_PDF_LEN + CONVERT_TMPH_SIZE
#define CONVERT_TMPH_LEN_MIN CONVERT_TMP_PDF_LEN - 1
#define CONVERT_TMPH_LEN_MAX 0xffff

/* }}} */
/* {{{ tmp-pdf-header-time-format-enumeration */

enum CONVERT_TMP_PDF_PFIELD {
  CONVERT_TMP_PDF_PFIELD_20 = 0x20,  
  CONVERT_TMP_PDF_PFIELD_21 = 0x21,
  CONVERT_TMP_PDF_PFIELD_22 = 0x22,  
  CONVERT_TMP_PDF_PFIELD_23 = 0x23,
  CONVERT_TMP_PDF_PFIELD_24 = 0x24,
  CONVERT_TMP_PDF_PFIELD_25 = 0x25,
  CONVERT_TMP_PDF_PFIELD_26 = 0x26,  
  CONVERT_TMP_PDF_PFIELD_27 = 0x27,
  CONVERT_TMP_PDF_PFIELD_28 = 0x28,  
  CONVERT_TMP_PDF_PFIELD_29 = 0x29,
  CONVERT_TMP_PDF_PFIELD_2A = 0x2a,  
  CONVERT_TMP_PDF_PFIELD_2B = 0x2b,
  CONVERT_TMP_PDF_PFIELD_2C = 0x2c,  
  CONVERT_TMP_PDF_PFIELD_2D = 0x2d,
  CONVERT_TMP_PDF_PFIELD_2E = 0x2e,  
  CONVERT_TMP_PDF_PFIELD_2F = 0x2f
  };
typedef enum CONVERT_TMP_PDF_PFIELD CONVERT_TMP_PDF_PFIELD;

/* }}} */
/* {{{ tmph-version-enumeration */

enum CONVERT_TMPH_VER {
  CONVERT_TMPH_VER_NA  = -1,
  CONVERT_TMPH_VER_CUR = 0x04
  };
typedef enum CONVERT_TMPH_VER CONVERT_TMPH_VER;

/* }}} */
/* {{{ tmph-type-field-enumeration */

enum CONVERT_TMPH_TYPE {
  CONVERT_TMPH_TYPE_NA = -1,
  CONVERT_TMPH_TYPE_TC = 1,
  CONVERT_TMPH_TYPE_TM = 0
  };
typedef enum CONVERT_TMPH_TYPE CONVERT_TMPH_TYPE;

/* }}} */
/* {{{ tmph-pdf-flag-enumeration */

enum CONVERT_TMPH_PDF_FLAG {
  CONVERT_TMPH_PDF_FLAG_NA     = -1,
  CONVERT_TMPH_PDF_FLAG_EMPTY  = 0,
  CONVERT_TMPH_PDF_FLAG_EXISTS = 1
  };
typedef enum CONVERT_TMPH_PDF_FLAG CONVERT_TMPH_PDF_FLAG;

/* }}} */
/* {{{ tmph-apid-enumeration */

enum CONVERT_TMPH_APID {
  CONVERT_TMPH_APID_NA           = -1,
  CONVERT_TMPH_APID_HK           = 0x0701,  
  CONVERT_TMPH_APID_N1           = 0x0703,
  CONVERT_TMPH_APID_N2           = 0x0703,  
  CONVERT_TMPH_APID_N3           = 0x0703,
  CONVERT_TMPH_APID_B1           = 0x0714,  
  CONVERT_TMPH_APID_B2           = 0x0724,
  CONVERT_TMPH_APID_B3           = 0x0734
  };
typedef enum CONVERT_TMPH_APID CONVERT_TMPH_APID;

/* }}} */
/* {{{ tmph-segmetation-field-enumeration */

enum CONVERT_TMPH_SEG {
  CONVERT_TMPH_SEG_NA = -1,
  CONVERT_TMPH_SEG_FIRST = 1,
  CONVERT_TMPH_SEG_MIDDLE = 0,
  CONVERT_TMPH_SEG_LAST = 2,
  CONVERT_TMPH_SEG_ALONE = 3
  };
typedef enum CONVERT_TMPH_SEG CONVERT_TMPH_SEG;

/* }}} */
/* {{{ tmp-and-ddsp-info-type-access-macros */

typedef enum {
	CONVERT_TMPH_VER_INFO,
	CONVERT_TMPH_TYPE_INFO,
	CONVERT_TMPH_PDF_HEADER_FLAG_INFO,
	CONVERT_TMPH_APID_INFO,
	CONVERT_TMPH_SEG_INFO,
	CONVERT_TMP_DFH_PFIELD_INFO,
	CONVERT_DDSP_ADID_INFO,
	CONVERT_DDSP_SPACECRAFT_INFO,
	CONVERT_DDSP_STREAM_INFO,
	CONVERT_DDSP_TCAL_INFO,
	CONVERT_DDSP_GROUND_INFO
	} CONVERT_SYMBOL_TABLE;

#define convert_tmph_version_symbol( ver )\
       convert_symbol_select(ver,CONVERT_TMPH_VER_INFO)
#define convert_tmph_type_symbol( type )\
       convert_symbol_select(type,CONVERT_TMPH_TYPE_INFO)
#define convert_tmph_pdf_flag_symbol( pdf )\
       convert_symbol_select(pdf,CONVERT_TMPH_PDF_HEADER_FLAG_INFO)
#define convert_tmph_apid_symbol( apid )\
       convert_symbol_select(apid,CONVERT_TMPH_APID_INFO)
#define convert_tmph_seg_flag_symbol( seg )\
       convert_symbol_select(seg,CONVERT_TMPH_SEG_INFO)
#define convert_tmph_spsc_symbol( spsc ) ( spsc )
#define convert_tmph_length_symbol( len ) ( long ) len
#define convert_tmp_dfh_pfield_symbol( pfield )\
       convert_symbol_select(pfield,CONVERT_TMP_DFH_PFIELD_INFO)
#define convert_tmp_pdf_header_ut_sec_symbol( sec ) ( long ) sec
#define convert_tmp_pdf_header_ut_usec_symbol( usec ) ( long ) usec
#define convert_ddsp_adid_symbol( source )\
   convert_symbol_select(source,CONVERT_DDSP_ADID_INFO)
#define convert_ddsp_scid_symbol(scid)\
   convert_symbol_select(scid,CONVERT_DDSP_SPACECRAFT_INFO)
#define convert_ddsp_stream_symbol(stream)\
   convert_symbol_select(stream,CONVERT_DDSP_STREAM_INFO)
#define convert_ddsp_tcal_symbol(tcal)\
   convert_symbol_select(tcal,CONVERT_DDSP_TCAL_INFO)
#define convert_ddsp_ground_symbol(ground)\
   convert_symbol_select(ground,CONVERT_DDSP_GROUND_INFO)

/* }}} */
/* {{{ convert-tmph-spsc-definitions */

#define CONVERT_TMPH_SPSC_RADIX 16384
#define CONVERT_TMPH_SPSC_MIN  0
#define CONVERT_TMPH_SPSC_MAX ( CONVERT_TMPH_SPSC_RADIX - 1 )
#define CONVERT_TMPH_SPSC_RESET TED_SPSC_RESET
#define CONVERT_TMPH_SPSC_INTERNAL TED_SPSC_INTERNAL
#define SPSC_INTERNAL CONVERT_TMPH_SPSC_INTERNAL
#define SPSC_RESET CONVERT_TMPH_SPSC_RESET

/* }}} */
/* {{{ tmp-field-access-macros */

#define  CONVERT_TMPH_GET_VER( p )\
       ( int ) ( ( p ) [ 0 ] & 0xe0 ) / 0x20
#define  CONVERT_TMPH_SET_VER( p, v )\
       ( p ) [ 0 ] = ( ( ( p ) [ 0 ] & 0x1f ) | ( v * 0x20 ) )
#define  CONVERT_TMPH_SET_TYPE( p, t )\
       ( ( p ) [ 0 ] = ( ( ( p ) [ 0 ] & 0xef ) | ( t * 0x10 ) ) )
#define  CONVERT_TMPH_GET_TYPE( p )\
       ( int ) ( ( p ) [ 0 ] & 0x10 ) / 0x10
#define  CONVERT_TMPH_SET_PDF( p, f )\
       ( ( p ) [ 0 ] = ( ( ( p ) [ 0 ] & 0xf7 ) | ( f * 0x08 ) ) )
#define  CONVERT_TMPH_GET_PDF( p )\
       ( int ) ( ( p ) [ 0 ] & 0x08 ) / 0x08
#define  CONVERT_TMPH_SET_APID( p, a )\
       ( p ) [ 0 ] = ( ( p ) [ 0 ] & 0xf8 ) | ( ( a / 0x0100 ) & 0x07 );\
       ( p ) [ 1 ] = ( a % 0x0100 )
#define  CONVERT_TMPH_GET_APID( p )\
       ( int ) ( ( ( ( p ) [ 0 ] & 0x07 ) * 0x0100 ) + ( p ) [ 1 ] )
#define  CONVERT_TMPH_GET_SEG( p )\
       ( int ) ( ( p ) [ 2 ] & 0xc0 ) / 0x40
#define  CONVERT_TMPH_SET_SEG( p, s )\
       ( ( p ) [ 2 ] = ( ( ( p ) [ 2 ] & 0x3f ) |  ( s * 0x40 ) ) )
#define  CONVERT_TMPH_SET_SEQ( p, c )\
       ( ( p ) [ 2 ] = ( ( ( p ) [ 2 ] & 0xc0 ) | ( ( c / 0x0100 ) & 0x03 ) ),\
       ( p ) [ 3 ] = ( ( c % 0x0100 ) & 0xff ) )
#define  CONVERT_TMPH_GET_SEQ( p )\
       ( int ) ( ( ( ( p ) [ 2 ] & 0x3f ) * 0x100 ) + ( p ) [ 3 ] )
#define  CONVERT_TMPH_GET_LEN( p )\
       ( ( p ) [ 4 ] * 256 ) + ( ( p ) [ 5 ] )
#define  CONVERT_TMPH_SET_LEN( p, l )\
       ( p ) [ 4 ] = ( ( l / 0x0100 ) & 0xff );\
       ( p ) [ 5 ] = ( ( l % 0x0100 ) & 0xff )
#define  CONVERT_TMPH_GET_TM_LEN( p )\
       ( int ) CONVERT_TMPH_GET_LEN ( p ) - CONVERT_TMPH_PDF_LEN
#define CONVERT_TMP_GET_PDF_FORMAT( p )\
       ( int ) ( p ) [ 6 ]
#define CONVERT_TMP_SET_PDF_FORMAT( p )\
       ( p ) [ 6 ] = CONVERT_TMP_PDF_PFIELD_2F % 0x0100;
#define CONVERT_TMP_GET_PDF_SEC( p )\
       ( ( long ) ( p ) [ 10 ] +\
       ( ( long ) ( p ) [ 9 ] * 256L ) +\
       ( ( long ) ( p ) [ 8 ] * 256L * 256L ) +\
       ( ( long ) ( p ) [ 7 ] * 256L * 256L * 256L ) )
#define CONVERT_TMP_SET_PDF_SEC( p, s )\
       ( p ) [ 10 ] =   ( s % 0x0100     );\
       ( p ) [ 9 ]  = ( ( s / 0x0100     ) % 0x0100 );\
       ( p ) [ 8 ]  = ( ( s / 0x010000   ) % 0x0100 );\
       ( p ) [ 7 ]  = ( ( s / 0x01000000 ) % 0x0100 );
#define CONVERT_TMP_GET_PDF_USEC( p )\
	( ( long ) ( ( long ) ( p ) [ 13 ] +\
		   ( ( long ) ( p ) [ 12 ] * 256L ) +\
		   ( ( long ) ( p ) [ 11 ] * 256L * 256L ) )\
	 / CONVERT_DDSP_FSEC_RATIO )
#define CONVERT_TMP_SET_PDF_USEC( p, u )\
       ( p ) [ 11 ] = ( ( long )\
             ( u * CONVERT_DDSP_FSEC_RATIO ) / 0x010000 ) % 0x0100;\
       ( p ) [ 12 ] = ( ( long )\
             ( u * CONVERT_DDSP_FSEC_RATIO ) / 0x0100 ) % 0x0100;\
       ( p ) [ 13 ] = ( long )\
              ( u * CONVERT_DDSP_FSEC_RATIO ) % 0x0100
#define CONVERT_TMP_SET_PDF( p, t )\
       CONVERT_TMP_SET_PDF_FORMAT( ( p ) );\
       CONVERT_TMP_SET_PDF_SEC ( p, ( t )->tv_sec  );\
       CONVERT_TMP_SET_PDF_USEC( p, ( t )->tv_usec )
#define CONVERT_TMP_GET_LEN( p )\
       CONVERT_TMPH_GET_LEN( p ) - CONVERT_TMP_PDF_LEN
#define CONVERT_TMP_SET_LEN( p, l )\
       CONVERT_TMPH_SET_LEN ( p, ( l + CONVERT_TMP_PDF_LEN ) )
#define CONVERT_TMP_GET_DATA( p )\
       ( p ) + CONVERT_TMPH_SIZE + CONVERT_TMP_PDF_LEN
#define CONVERT_TMP_SET_DATA( p, d, l )\
       memcpy ( ( ( char * ) ( p ) ) + CONVERT_TMPH_SIZE + CONVERT_TMP_PDF_LEN, ( char * ) ( d ), l );\
       CONVERT_TMPH_SET_LEN ( ( p ), l )

/* }}} */
/* {{{ convert-sis-tmp-len-and-ratio-enum */

enum CONVERT_SIS_TMP_LEN {
  CONVERT_SIS_TMP_HK_LEN = 192+8,
  CONVERT_SIS_TMP_N1_LEN = 336+8,
  CONVERT_SIS_TMP_N2_LEN = 336+8,
  CONVERT_SIS_TMP_N3_LEN = 336+8,
  CONVERT_SIS_TMP_B1_LEN = 456+8,
  CONVERT_SIS_TMP_B2_LEN = 948+8,
  CONVERT_SIS_TMP_B3_LEN = 306+8
};
typedef enum CONVERT_SIS_TMP_LEN CONVERT_SIS_TMP_LEN;

enum CONVERT_SIS_TMP_RATIO {
  CONVERT_SIS_TMP_HK_RATIO =  1,
  CONVERT_SIS_TMP_N1_RATIO = 10,
  CONVERT_SIS_TMP_N2_RATIO = 10,
  CONVERT_SIS_TMP_N3_RATIO = 10,
  CONVERT_SIS_TMP_B1_RATIO = 62,
  CONVERT_SIS_TMP_B2_RATIO = 62,
  CONVERT_SIS_TMP_B3_RATIO = 62,
  CONVERT_SIS_TMP_DEFAULT_RATIO = 1
};
typedef enum CONVERT_SIS_TMP_RATIO CONVERT_SIS_TMP_RATIO;

/* }}} */

/* ------------ DDS-packet-definitions ----------------------- */

/* {{{ convert-ddsp-field-type-definitions */

#define CONVERT_DDSP_MAX_LEN                  16777216
#define CONVERT_DDSP_HEADER_SIZE              15

typedef struct
{
  long days;
  long ms;
  long us; 
} CONVERT_DDSP_TIME;

typedef char * CONVERT_DDSP_DATA;
typedef int CONVERT_DDSP_SOURCE;
typedef unsigned char * CONVERT_DDS_PACKET;
typedef int    CONVERT_DDSP_LEN;
typedef int    CONVERT_DDSP_TAM;

/* }}} */
/* {{{ convert-ddsp-field-access-macros */

#define CONVERT_DDSP_GET_SCET_DAY( p )\
       ( int ) ( p ) [ 0 ] * 256 + ( p ) [ 1 ]
#define CONVERT_DDSP_SET_SCET_DAY( d, p )\
       ( p ) [ 0 ] = ( d / 256 ) % 256; ( p ) [ 1 ] =   d % 256
#define CONVERT_DDSP_GET_SCET_MS( p )\
       ( ( long ) ( p ) [ 5 ] +\
       ( ( long ) ( p ) [ 4 ] *   256L ) +\
       ( ( long ) ( p ) [ 3 ] * ( 256L * 256L ) ) +\
       ( ( long ) ( p ) [ 2 ] * ( 256L * 256L * 256L ) ) )
#define CONVERT_DDSP_SET_SCET_MS( m, p )\
       ( p ) [ 2 ] = ( m / 256 / 256 / 256 ) % 256;\
       ( p ) [ 3 ] = ( m / 256 / 256 ) % 256;\
       ( p ) [ 4 ] = ( m / 256 ) % 256;\
       ( p ) [ 5 ] =   m %  256
#define CONVERT_DDSP_GET_SCET_US( p )\
       ( ( long ) ( p ) [ 7 ] +\
       ( ( long ) ( p ) [ 6 ] * 256L ) )
#define CONVERT_DDSP_SET_SCET_US( u, p )\
       ( p ) [ 6 ] = ( u / 256 ) % 256;\
       ( p ) [ 7 ] =   u % 256
#define CONVERT_DDSP_GET_SCET( t, p )\
       ( t )->days = ( long ) CONVERT_DDSP_GET_SCET_DAY ( p );\
       ( t )->ms   = ( long ) CONVERT_DDSP_GET_SCET_MS ( p );\
       ( t )->us   = ( long ) CONVERT_DDSP_GET_SCET_US ( p )
#define CONVERT_DDSP_SET_SCET( t, p )\
       CONVERT_DDSP_SET_SCET_DAY( ( t )->days, p );\
       CONVERT_DDSP_SET_SCET_MS(  ( t )->ms, p );\
       CONVERT_DDSP_SET_SCET_US(  ( t )->us, p )
#define CONVERT_DDSP_GET_SOURCE( p )\
       ( int ) ( p ) [ 8 ]
#define CONVERT_DDSP_SET_SOURCE( a, p )\
       ( p ) [ 8 ] = a % 256
#define CONVERT_DDSP_GET_ADID( p )\
       ( int ) ( p ) [ 8 ]
#define CONVERT_DDSP_SET_ADID( a, p )\
       ( p ) [ 8 ] = a % 256
#define CONVERT_DDSP_GET_LEN( p )\
       ( ( long ) ( p ) [ 11 ] +\
       ( ( long ) ( p ) [ 10 ] * 256L ) +\
       ( ( long ) ( ( p ) [ 9  ] & 0x02 ) * ( 256L * 256L ) ) )
#define CONVERT_DDSP_SET_LEN( l, p )\
       ( p ) [ 9 ]  =  ( l / 256 / 256 ) % 256;\
       ( p ) [ 10 ] = ( l / 256 ) % 256;\
       ( p ) [ 11 ] = l % 256
#define      CONVERT_DDSP_GSID_MASK           0x0f
#define CONVERT_DDSP_GET_GSID( p )\
       ( int ) ( ( p ) [ 12 ] & CONVERT_DDSP_GSID_MASK )
#define CONVERT_DDSP_SET_GSID( g, p )\
       ( p ) [ 12 ] = ( ( p ) [ 12 ] & ~CONVERT_DDSP_GSID_MASK ) |\
       ( g &  CONVERT_DDSP_GSID_MASK )
#define CONVERT_DDSP_SCID_MASK                0xf0
#define CONVERT_DDSP_GET_SCID( p )\
       ( int ) ( ( p ) [ 12 ] & CONVERT_DDSP_SCID_MASK ) / 0x10
#define CONVERT_DDSP_SET_SCID( s, p )\
       ( p ) [ 12 ] = ( ( s * 16 ) &  CONVERT_DDSP_SCID_MASK ) |\
       ( ( p ) [ 12 ] & ~CONVERT_DDSP_SCID_MASK ) 
#define CONVERT_DDSP_GET_STREAM( p )\
       ( int )   ( p ) [ 13 ]
#define CONVERT_DDSP_SET_STREAM( a, p )\
       ( p ) [ 13 ] = ( a % 256 )
#define CONVERT_DDSP_TCAL_MASK                0xf0
#define CONVERT_DDSP_GET_TCAL( p )\
       ( int ) ( ( p ) [ 14 ] & CONVERT_DDSP_TCAL_MASK ) / 0x10
#define CONVERT_DDSP_SET_TCAL( t, p )\
       ( p ) [ 14 ] = ( ( t * 16 ) & CONVERT_DDSP_TCAL_MASK ) |\
       ( ( p ) [ 14 ] & ~CONVERT_DDSP_TCAL_MASK )
#define CONVERT_DDSP_TAM_MASK                 0x0f
#define CONVERT_DDSP_GET_TAM( p )\
       ( int ) ( ( p ) [ 14 ] & CONVERT_DDSP_TAM_MASK )
#define CONVERT_DDSP_SET_TAM( t, p )\
       ( p ) [ 14 ] = ( ( p ) [ 14 ] & ~CONVERT_DDSP_TAM_MASK ) |\
       ( t &  CONVERT_DDSP_TAM_MASK )
#define CONVERT_DDSP_GET_DATA( p )\
        ( p ) + CONVERT_DDSP_HEADER_SIZE
#define CONVERT_DDSP_SET_DATA( d, l, p )\
   memcpy( ( ( char * ) p ) + CONVERT_DDSP_HEADER_SIZE,\
            ( char * ) d,\
                       l );\
   CONVERT_DDSP_SET_LEN( ( l ), ( p ) )

/* }}} */
/* {{{ convert-ddsp-src-or-adid-enumeration */

typedef enum {
  CONVERT_DDSP_ADID_NA =  255,

  CONVERT_DDSP_ADID_ECLUD001 =  20, /* Cluster CD-ROM directory tree listing format */
  CONVERT_DDSP_ADID_ECLUD002 =  21, /* Cluster CD-ROM cumulative index format */
  CONVERT_DDSP_ADID_ECLUD003 =  22, /* Catalogue format for Cluster Off-line delivery */
  CONVERT_DDSP_ADID_ECLUD004 =  23, /* Catalogue format for Cluster on-line delivery */
  CONVERT_DDSP_ADID_ECLUD005 =  24, /* Acknowledgement format for Cluster on-line delivery */
  CONVERT_DDSP_ADID_ECLUD006 =  25, /* Acknowledgement format for Cluster off-line delivery */
  CONVERT_DDSP_ADID_ECLUD007 =   0, /* Master Catalogue of all data available */
    /* AUX */
  CONVERT_DDSP_ADID_ECLUA001 =   1, /* LTOF Long Term Orbit File */
  CONVERT_DDSP_ADID_ECLUA002 =   2, /* LTEF Long Term Event File*/
  CONVERT_DDSP_ADID_ECLUA003 =   3, /* Short Term Orbit File */
  CONVERT_DDSP_ADID_ECLUA004 =   4, /* Short Term Event File */
  CONVERT_DDSP_ADID_ECLUA005 =   5, /* Spacecraft Attitude and Spin Rate Entry */
  CONVERT_DDSP_ADID_ECLUA006 =   6, /* Time Calibration File Entry */
  CONVERT_DDSP_ADID_ECLUA007 =   7, /* Command History File Entry */

  CONVERT_DDSP_ADID_ECLUP101 = 200, /* Housekeeping Parameter Definition File for EDI on CLUSTER 1   */
  CONVERT_DDSP_ADID_ECLUP102 = 201, /* Housekeeping Parameter Definition File for FGM on CLUSTER 1   */
  CONVERT_DDSP_ADID_ECLUP103 = 202, /* Housekeeping Parameter Definition File for CIS on CLUSTER 1   */
  CONVERT_DDSP_ADID_ECLUP104 = 203, /* Housekeeping Parameter Definition File for PEACE on CLUSTER 1 */
  CONVERT_DDSP_ADID_ECLUP105 = 204, /* Housekeeping Parameter Definition File for RAPID on CLUSTER 1 */
  CONVERT_DDSP_ADID_ECLUP106 = 205, /* Housekeeping Parameter Definition File for WEC on CLUSTER 1   */
  CONVERT_DDSP_ADID_ECLUP107 = 206, /* Housekeeping Parameter Definition File for ASPOC on CLUSTER 1 */
  CONVERT_DDSP_ADID_ECLUP108 = 207, /* Housekeeping Parameter Definition File for Cluster 1 Platform */

  CONVERT_DDSP_ADID_ECLUP201 = 208, /* Housekeeping Parameter Definition File for EDI on CLUSTER 2   */
  CONVERT_DDSP_ADID_ECLUP202 = 209, /* Housekeeping Parameter Definition File for FGM on CLUSTER 2   */
  CONVERT_DDSP_ADID_ECLUP203 = 200, /* Housekeeping Parameter Definition File for CIS on CLUSTER 2   */
  CONVERT_DDSP_ADID_ECLUP204 = 211, /* Housekeeping Parameter Definition File for PEACE on CLUSTER 2 */
  CONVERT_DDSP_ADID_ECLUP205 = 212, /* Housekeeping Parameter Definition File for RAPID on CLUSTER 2 */
  CONVERT_DDSP_ADID_ECLUP206 = 213, /* Housekeeping Parameter Definition File for WEC on CLUSTER 2   */
  CONVERT_DDSP_ADID_ECLUP207 = 214, /* Housekeeping Parameter Definition File for ASPOC on CLUSTER 2 */
  CONVERT_DDSP_ADID_ECLUP208 = 215, /* Housekeeping Parameter Definition File for Cluster 2 Platform */

  CONVERT_DDSP_ADID_ECLUP301 = 216, /* Housekeeping Parameter Definition File for EDI on CLUSTER 3   */
  CONVERT_DDSP_ADID_ECLUP302 = 217, /* Housekeeping Parameter Definition File for FGM on CLUSTER 3   */
  CONVERT_DDSP_ADID_ECLUP303 = 218, /* Housekeeping Parameter Definition File for CIS on CLUSTER 3   */
  CONVERT_DDSP_ADID_ECLUP304 = 219, /* Housekeeping Parameter Definition File for PEACE on CLUSTER 3 */
  CONVERT_DDSP_ADID_ECLUP305 = 220, /* Housekeeping Parameter Definition File for RAPID on CLUSTER 3 */
  CONVERT_DDSP_ADID_ECLUP306 = 221, /* Housekeeping Parameter Definition File for WEC on CLUSTER 3   */
  CONVERT_DDSP_ADID_ECLUP307 = 222, /* Housekeeping Parameter Definition File for ASPOC on CLUSTER 3 */
  CONVERT_DDSP_ADID_ECLUP308 = 223, /* Housekeeping Parameter Definition File for CLUSTER 3 Platform */

  CONVERT_DDSP_ADID_ECLUP401 = 224, /* Housekeeping Parameter Definition File for EDI on CLUSTER 4   */
  CONVERT_DDSP_ADID_ECLUP402 = 225, /* Housekeeping Parameter Definition File for FGM on CLUSTER 4   */
  CONVERT_DDSP_ADID_ECLUP403 = 226, /* Housekeeping Parameter Definition File for CIS on CLUSTER 4   */
  CONVERT_DDSP_ADID_ECLUP404 = 227, /* Housekeeping Parameter Definition File for PEACE on CLUSTER 4 */
  CONVERT_DDSP_ADID_ECLUP405 = 228, /* Housekeeping Parameter Definition File for RAPID on CLUSTER 4 */
  CONVERT_DDSP_ADID_ECLUP406 = 229, /* Housekeeping Parameter Definition File for WEC on CLUSTER 4   */
  CONVERT_DDSP_ADID_ECLUP407 = 230, /* Housekeeping Parameter Definition File for ASPOC on CLUSTER 4 */
  CONVERT_DDSP_ADID_ECLUP408 = 231, /* Housekeeping Parameter Definition File for CLUSTER 4 Platform */
  
  /* NSD CLUSTER 0 */
  CONVERT_DDSP_ADID_ECLUN001 = -1, /* Normal science data for EDI   on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUN002 = -1, /* Normal science data for FGM   on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUN003 = -1, /* Normal science data for CIS   on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUN004 = -1, /* Normal science data for PEACE on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUN005 = -1, /* Normal science data for RAPID on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUN006 = -1, /* Normal science data for WEC   on Cluster 0 */
  
  /* BSD CLUSTER 0 */
  CONVERT_DDSP_ADID_ECLUB001 = -1, /* Burst science for EDI   on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUB002 = -1, /* Burst science for FGM   on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUB003 = -1, /* Burst science for CIS   on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUB004 = -1, /* Burst science for PEACE on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUB005 = -1, /* Burst science for RAPID on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUB006 = -1, /* Burst science for WEC   on Cluster 0 */

  /* HKD CLUSTER 0 */
  CONVERT_DDSP_ADID_ECLUH001 = -1, /* Housekeeping data for EDI    on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUH002 = -1, /* Housekeeping data for FGM    on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUH003 = -1, /* Housekeeping data for CIS    on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUH004 = -1, /* Housekeeping data for PEACE  on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUH005 = -1, /* Housekeeping data for RAPID  on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUH006 = -1, /* Housekeeping data for WEC    on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUH007 = -1, /* Housekeeping data for ASPOC  on Cluster 0 */
  CONVERT_DDSP_ADID_ECLUH008 = -1, /* Housekeeping data for SC     on Cluster 0 */
  
  /* NSD CLUSTER 1 */
  CONVERT_DDSP_ADID_ECLUN101 = 30, /* Normal science data for EDI   on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUN102 = 31, /* Normal science data for FGM   on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUN103 = 32, /* Normal science data for CIS   on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUN104 = 33, /* Normal science data for PEACE on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUN105 = 34, /* Normal science data for RAPID on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUN106 = 35, /* Normal science data for WEC   on Cluster 1 */
  
  /* BSD CLUSTER 1 */
  CONVERT_DDSP_ADID_ECLUB101 = 37, /* Burst science for EDI   on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUB102 = 38, /* Burst science for FGM   on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUB103 = 39, /* Burst science for CIS   on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUB104 = 40, /* Burst science for PEACE on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUB105 = 41, /* Burst science for RAPID on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUB106 = 42, /* Burst science for WEC   on Cluster 1 */

  /* HKD CLUSTER 1 */
  CONVERT_DDSP_ADID_ECLUH101 = 44, /* Housekeeping data for EDI    on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUH102 = 45, /* Housekeeping data for FGM    on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUH103 = 46, /* Housekeeping data for CIS    on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUH104 = 47, /* Housekeeping data for PEACE  on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUH105 = 48, /* Housekeeping data for RAPID  on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUH106 = 49, /* Housekeeping data for WEC    on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUH107 = 50, /* Housekeeping data for ASPOC  on Cluster 1 */
  CONVERT_DDSP_ADID_ECLUH108 = 51, /* Housekeeping data for SC     on Cluster 1 */
  
  /* NSD CLUSTER 2 */
  CONVERT_DDSP_ADID_ECLUN201 = 70, /* Normal science data for EDI   on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUN202 = 71, /* Normal science data for FGM   on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUN203 = 72, /* Normal science data for CIS   on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUN204 = 73, /* Normal science data for PEACE on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUN205 = 74, /* Normal science data for RAPID on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUN206 = 75, /* Normal science data for WEC   on Cluster 2 */
  
  /* BSD CLUSTER 2 */
  CONVERT_DDSP_ADID_ECLUB201 = 77, /* Burst science for EDI   on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUB202 = 78, /* Burst science for FGM   on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUB203 = 79, /* Burst science for CIS   on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUB204 = 80, /* Burst science for PEACE on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUB205 = 81, /* Burst science for RAPID on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUB206 = 82, /* Burst science for WEC   on Cluster 2 */

  /* HKD CLUSTER 2 */
  CONVERT_DDSP_ADID_ECLUH201 = 84, /* Housekeeping data for EDI    on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUH202 = 85, /* Housekeeping data for FGM    on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUH203 = 86, /* Housekeeping data for CIS    on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUH204 = 87, /* Housekeeping data for PEACE  on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUH205 = 88, /* Housekeeping data for RAPID  on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUH206 = 89, /* Housekeeping data for WEC    on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUH207 = 90, /* Housekeeping data for ASPOC  on Cluster 2 */
  CONVERT_DDSP_ADID_ECLUH208 = 91, /* Housekeeping data for SC     on Cluster 2 */
  
  /* NSD CLUSTER 3 */
  CONVERT_DDSP_ADID_ECLUN301 = 110, /* Normal science data for EDI   on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUN302 = 111, /* Normal science data for FGM   on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUN303 = 112, /* Normal science data for CIS   on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUN304 = 113, /* Normal science data for PEACE on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUN305 = 114, /* Normal science data for RAPID on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUN306 = 115, /* Normal science data for WEC   on Cluster 3 */
  
  /* BSD CLUSTER 3 */
  CONVERT_DDSP_ADID_ECLUB301 = 117, /* Burst science for EDI   on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUB302 = 118, /* Burst science for FGM   on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUB303 = 119, /* Burst science for CIS   on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUB304 = 120, /* Burst science for PEACE on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUB305 = 121, /* Burst science for RAPID on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUB306 = 122, /* Burst science for WEC   on Cluster 3 */

  /* HKD CLUSTER 2 */
  CONVERT_DDSP_ADID_ECLUH301 = 124, /* Housekeeping data for EDI    on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUH302 = 125, /* Housekeeping data for FGM    on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUH303 = 126, /* Housekeeping data for CIS    on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUH304 = 127, /* Housekeeping data for PEACE  on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUH305 = 128, /* Housekeeping data for RAPID  on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUH306 = 129, /* Housekeeping data for WEC    on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUH307 = 130, /* Housekeeping data for ASPOC  on Cluster 3 */
  CONVERT_DDSP_ADID_ECLUH308 = 131, /* Housekeeping data for SC     on Cluster 3 */
  
  /* NSD CLUSTER 4 */
  CONVERT_DDSP_ADID_ECLUN401 = 150, /* Normal science data for EDI   on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUN402 = 151, /* Normal science data for FGM   on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUN403 = 152, /* Normal science data for CIS   on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUN404 = 153, /* Normal science data for PEACE on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUN405 = 154, /* Normal science data for RAPID on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUN406 = 155, /* Normal science data for WEC   on Cluster 4 */
  
  /* BSD CLUSTER 4 */
  CONVERT_DDSP_ADID_ECLUB401 = 157, /* Burst science for EDI   on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUB402 = 158, /* Burst science for FGM   on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUB403 = 159, /* Burst science for CIS   on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUB404 = 160, /* Burst science for PEACE on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUB405 = 161, /* Burst science for RAPID on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUB406 = 162, /* Burst science for WEC   on Cluster 4 */

  /* HKD CLUSTER 4 */
  CONVERT_DDSP_ADID_ECLUH401 = 164, /* Housekeeping data for EDI    on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUH402 = 165, /* Housekeeping data for FGM    on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUH403 = 166, /* Housekeeping data for CIS    on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUH404 = 167, /* Housekeeping data for PEACE  on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUH405 = 168, /* Housekeeping data for RAPID  on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUH406 = 169, /* Housekeeping data for WEC    on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUH407 = 170, /* Housekeeping data for ASPOC  on Cluster 4 */
  CONVERT_DDSP_ADID_ECLUH408 = 171  /* Housekeeping data for SC     on Cluster 4 */

  } CONVERT_DDSP_ADID;

/* }}} */
/* {{{ convert-ddsp-spacecraft-enumeration */

typedef enum {
  CONVERT_DDSP_SOURCE_SCID_0_OFF = 0,
  CONVERT_DDSP_SOURCE_SCID_1_OFF = 30,
  CONVERT_DDSP_SOURCE_SCID_2_OFF = 70,
  CONVERT_DDSP_SOURCE_SCID_3_OFF = 110,
  CONVERT_DDSP_SOURCE_SCID_4_OFF = 150
  } CONVERT_DDSP_SOURCE_SCID_OFF;

/* }}} */
/* {{{ convert-adid-or-src-processing-enumerations */

typedef enum {
  CONVERT_DDSP_SOURCE_EDI_OFF = 0,
  CONVERT_DDSP_SOURCE_FGM_OFF = 1,
  CONVERT_DDSP_SOURCE_CIS_OFF = 2,
  CONVERT_DDSP_SOURCE_PEA_OFF = 3,
  CONVERT_DDSP_SOURCE_RAP_OFF = 4,
  CONVERT_DDSP_SOURCE_WEC_OFF = 5,
  CONVERT_DDSP_SOURCE_ASP_OFF = 6
  } CONVERT_DDSP_SOURCE_EXP_OFF;

typedef enum {
  CONVERT_DDSP_SOURCE_NM_OFF = 0,
  CONVERT_DDSP_SOURCE_BM_OFF = 7,
  CONVERT_DDSP_SOURCE_HK_OFF = 14
  } CONVERT_DDSP_SOURCE_TYPE_OFF;

#define CONVERT_DDSP_SOURCE_NA -1

/* }}} */
/* {{{ convert-ddsp-gsid-enumeration */

typedef enum {
  CONVERT_DDSP_GSID_ODENWALD =  1,
  CONVERT_DDSP_GSID_REDU =  2,
  CONVERT_DDSP_GSID_KOUROU =  3,
  CONVERT_DDSP_GSID_PERTH =  4,
  CONVERT_DDSP_GSID_MALINDI =  5,
  CONVERT_DDSP_GSID_CANBERRA =  6,
  CONVERT_DDSP_GSID_GOLDSTONE =  7,
  CONVERT_DDSP_GSID_NA = 15,
  CONVERT_DDSP_DEFAULT_GSID =  CONVERT_DDSP_GSID_ODENWALD
  } CONVERT_DDSP_GSID;

/* }}} */
/* {{{ convert-ddsp-stream-enumeration */

typedef enum {
  /* RT ::= REAL TIME */
  CONVERT_DDSP_STREAM_RT_VC0 = 0x00,
  CONVERT_DDSP_STREAM_RT_VC2 = 0x02,
  CONVERT_DDSP_STREAM_RT_VC3 = 0x03,
  
  /* PB ::= PLAY BACK */
  CONVERT_DDSP_STREAM_PB_VC0 = 0x40,
  CONVERT_DDSP_STREAM_PB_VC2 = 0x42,
  CONVERT_DDSP_STREAM_PB_VC3 = 0x43,
  
  /* RE ::= RECALL */
  CONVERT_DDSP_STREAM_RE_VC0 = 0xf0,
  CONVERT_DDSP_STREAM_RE_VC2 = 0xf2,
  CONVERT_DDSP_STREAM_RE_VC3 = 0xf3,
  
  /* RP ::= RECALL PLAY BACK */
  CONVERT_DDSP_STREAM_RP_VC0 = 0x4f,
  CONVERT_DDSP_STREAM_RP_VC2 = 0xe2,
  CONVERT_DDSP_STREAM_RP_VC3 = 0xe3,
  
  /* INVALID */
  CONVERT_DDSP_STREAM_NA = 0xFF
  } CONVERT_DDSP_STREAM;

typedef enum {
  CONVERT_DDSP_DEFAULT_STREAM_BASE = 0x00,
  CONVERT_DDSP_STREAM_NA_BASE = 0xff,
  CONVERT_DDSP_STREAM_RT_BASE = 0x00,
  CONVERT_DDSP_STREAM_PB_BASE = 0x40,
  CONVERT_DDSP_STREAM_RE_BASE = 0xf0,
  CONVERT_DDSP_STREAM_RP_BASE = 0xe0
  } CONVERT_DDSP_STREAM_CLASS;

/* }}} */
/* {{{ convert-ddsp-tcal-enumeration */

typedef enum {
  CONVERT_DDSP_DEFAULT_TCAL =  0,
  CONVERT_DDSP_TCAL_NA = -1,
  CONVERT_DDSP_TCAL_ACTUAL =  0,
  CONVERT_DDSP_TCAL_EXTRAPOLATED =  1,
  CONVERT_DDSP_TCAL_CONTINGENCY =  2
  } CONVERT_DDSP_TCAL;

/* }}} */
/* {{{ convert-ddsp-tam-or-asid-enumeration */

enum CONVERT_DDSP_TAM {
  CONVERT_DDSP_TAM_NA  = 0x00,
  CONVERT_DDSP_TAM_MIN = 0x01,
  CONVERT_DDSP_TAM_MAX = 0x0f
  };

typedef enum {
  CONVERT_DDSP_TAM_HK = 1,
  CONVERT_DDSP_TAM_N1 = 1,
  CONVERT_DDSP_TAM_N2 = 1,
  CONVERT_DDSP_TAM_N3 = 1,
  CONVERT_DDSP_TAM_B1 = 1,
  CONVERT_DDSP_TAM_B2 = 2,
  CONVERT_DDSP_TAM_B3 = 3
  } CONVERT_DDSP_DEFAULT_TAM;

/* }}} */

#ifndef CONVERT_C

extern
int
convert_isa_ddsp_source(
CONVERT_DDSP_SOURCE
);

extern
int
convert_isa_ddsp_scid(
CONVERT_CLUSTER_SCID
);

extern
int
convert_ddsp_match_scid_and_source(
CONVERT_CLUSTER_SCID,
CONVERT_DDSP_SOURCE
);

extern
int
convert_isa_ddsp_stream(
CONVERT_DDSP_STREAM
);

extern
int
convert_isa_ddsp_ground(
CONVERT_DDSP_GSID
);

extern
int
convert_match_ddsp_source_and_stream(
CONVERT_DDSP_SOURCE,
CONVERT_DDSP_STREAM
);

extern
int
convert_isa_ddsp_tcal(
CONVERT_DDSP_TCAL		/* TIME QUALITY */
);

extern
int
convert_isa_ddsp_tam(
CONVERT_DDSP_TAM		/* ACQUISITION SEQUENCE ID 1 - 15, 0 = NA */
);

extern
int
convert_ddsp_too_long(
CONVERT_DDSP_LEN		/* LENGTH IN BYTE <= 2^24 - 1 */
);

extern
unsigned char *
convert_create_dds_packet(
CONVERT_DDSP_TIME *,		/* SCET */
CONVERT_DDSP_ADID,		/* DATA SOURCE */
CONVERT_DDSP_LEN,		/* DATA LENGTH */
CONVERT_CLUSTER_SCID,		/* SAPCECRAFT */
CONVERT_DDSP_GSID,		/* GROUND SEGMENT */
CONVERT_DDSP_STREAM,		/* STREAM */
CONVERT_DDSP_TCAL,		/* TIME QUALITY */
CONVERT_DDSP_TAM,		/* ACQUISITION SEQUENCE ID */
unsigned char *,		/* DATA */
unsigned char *
);

extern
unsigned char *
convert_create_tm_packet(
CONVERT_TMPH_VER,		/* VERSION 3 bit = 100 */
CONVERT_TMPH_TYPE,		/* TC/TM FLAG */
CONVERT_TMPH_PDF_FLAG,		/* DATA FIELD HEADER */
CONVERT_TMPH_APID,		/* APPLICATION IDENTIFIER */
CONVERT_TMPH_SEG,		/* SEGMENTATION FLAG (ALONE) */
CONVERT_TMPH_SEQ,		/* SEQUENCE # */
CONVERT_TMPH_LEN,		/* ESA TM LENGTH */
CONVERT_TMP_PDF_TIME *,		/* ESA TM PACKET TIME STRUCTURE */
unsigned char *,		/* PACKET DATA */
unsigned char *
);

extern
CONVERT_CLUSTER_SCID
ted_2_convert_spacecraft(
TED_SPACECRAFT
);

extern
CONVERT_DDSP_GSID
ted_2_convert_ground(
TED_GROUND
);

extern
CONVERT_DDSP_STREAM_CLASS
ted_2_convert_transmission_mode(
TED_TRANSMISSION
);

extern
CONVERT_DDSP_TCAL
ted_2_convert_time_quality(
TED_QUALITY
);

extern
char *
convert_symbol_select(
int,
CONVERT_SYMBOL_TABLE
);

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* CONVERT_C */
#endif /* CONVERTP_H */

/* Local variables: */
/* folded-file: t */
/* end: */

