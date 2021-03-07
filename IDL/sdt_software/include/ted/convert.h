/*
 * @(#)convert.h	1.1, 05/23/00
 *
 * Project:         CLUSTER WEC TED Package
 *
 * Item Name:       src/convert/convert.h
 *
 * Item Version:    (see RCS header)
 *
 * Item Type:       c-source
 *
 * Author:          Simon Davis
 *
 * Modified for thread-safeness and SCCS (98/07):  Jack Vernetti SSL/UCB
 *
 */
#define  SccsId_convert_h  "@(#)convert.h	1.1, 05/23/00"

#ifndef _convert_convert_h
#define _convert_convert_h

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include "ted.h"
#include "tedsys.h"

/* ted-convert-spsc-control-constants */

#define TED_SPSC_RESET		-1
#define TED_SPSC_INTERNAL	-2

/* ted-and-convert-info-type-access-macros */

typedef enum {
	TED_STATUS_INFO,
	TED_SPACECRAFT_INFO,
	TED_GROUND_INFO,
	TED_TRANSMISSION_INFO,
	TED_QUALITY_INFO
	} TED_SYMBOL_TABLE;

#define ted_status_symbol( status )\
		ted_symbol_select(status,TED_STATUS_INFO)
#define ted_scid_symbol( scid )\
		ted_symbol_select(scid,TED_SPACECRAFT_INFO)
#define ted_ground_symbol( ground )\
		ted_symbol_select(ground,TED_GROUND_INFO)
#define ted_transmission_symbol( transmission )\
		ted_symbol_select(transmission,TED_TRANSMISSION_INFO)
#define ted_quality_symbol( quality )\
		ted_symbol_select(quality,TED_QUALITY_INFO)

/* convert-interface-definition */

#ifndef CONVERT_C

extern void convert_display_dds_packet(
unsigned char * packet
);

extern void convert_display_tm_packet(
unsigned char * packet
);

extern char *ted_symbol_select(
int value,
TED_SYMBOL_TABLE table
);

#ifndef TED_NO_FD

extern TED_STATUS ted_convert_write_ddsp(
int file,
unsigned char *packet
);

extern TED_STATUS ted_convert_write_tmp(
int file,
unsigned char *packet
);

extern unsigned char *ted_convert_read_ddsp(
int file
);

extern unsigned char *ted_convert_read_tmp(
int file,
unsigned char *packet
);

#endif

extern TED_STATUS ted_convert_stream_write_ddsp(
FILE *file,
unsigned char *packet
);

extern TED_STATUS ted_convert_stream_write_tmp(
FILE *file,
unsigned char * packet
);

extern TED_STATUS ted_convert_stream_read_ddsp(
FILE *file,
unsigned char **bpacket,
unsigned char *pkt
);

extern TED_STATUS ted_convert_stream_read_tmp( FILE *file,
    unsigned char **packet,
    unsigned char *pkt 
);

extern void convert_display_dds_packet_time(
unsigned char * packet
);

extern void convert_display_tm_packet_time(
unsigned char * packet
);

extern TED_STATUS ted_convert_sis_tm_patch (
unsigned char *hk,
unsigned char *sc
);

extern TED_STATUS ted_convert_stream_read_sis_tmp(
FILE *file,
unsigned char **packet,
unsigned char *pkt,
unsigned char *sis
);

extern TED_STATUS ted_convert_dds2esatm(
unsigned char *ddspacket,
unsigned char **esatmpacket,
int sequence_count,
SeqCountInfo *cnt_info
);

extern TED_STATUS ted_convert_esatm2dds(
unsigned char * esatmpacket,
unsigned char **ddspacket,
TED_SPACECRAFT spacecraft,
TED_TRANSMISSION transmission_mode,
TED_QUALITY time_quality,
TED_GROUND ground
);

#endif

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /*!_convert_convert_h*/
