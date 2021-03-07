/*
 * @(#)xmacros.h	1.1, 05/23/00
 *
 * Project:         CLUSTER WEC TED Package
 *
 * Item Name:       src/unpack/xmacros.h
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
#define  SccsId_xmacros_h  "@(#)xmacros.h	1.1, 05/23/00"

#ifndef _unpack_xmacros_h
#define _unpack_xmacros_h

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include "ted.h"

typedef struct {
  int value;
  char *desc;
} field_info;

extern char *get_desc(field_info info[], int value);
extern int search_enumeration(field_info info[], int value);

/*
 * ESA Telemetry Packet
 */

/* Version Number */

#define name_tmph_version "Version Number"
#define text_tmph_version(val) get_desc(info_tmph_version, (val))
#define check_tmph_version(val) search_enumeration(info_tmph_version, (val))
#define tmph_version(p) ((0x1*((((p)[0])>>5)&0x7)))
#define put_tmph_version(p,val) ((p)[0] = (((p)[0]&~0xe0)|(((val)*0x20)&0xe0)))

extern field_info info_tmph_version[];

#define tmph_version_version2 4

/* Type */

#define name_tmph_type "Type"
#define text_tmph_type(val) get_desc(info_tmph_type, (val))
#define check_tmph_type(val) search_enumeration(info_tmph_type, (val))
#define tmph_type(p) ((0x1*((((p)[0])>>4)&0x1)))
#define put_tmph_type(p,val) ((p)[0] = (((p)[0]&~0x10)|(((val)*0x10)&0x10)))

extern field_info info_tmph_type[];

#define tmph_type_telemetry 0
#define tmph_type_telecommand 1

/* Data Field Header Flag */

#define name_tmph_headerflag "Data Field Header Flag"
#define text_tmph_headerflag(val) get_desc(info_tmph_headerflag, (val))
#define check_tmph_headerflag(val) search_enumeration(info_tmph_headerflag, (val))
#define tmph_headerflag(p) ((0x1*((((p)[0])>>3)&0x1)))
#define put_tmph_headerflag(p,val) ((p)[0] = (((p)[0]&~0x8)|(((val)*0x8)&0x8)))

extern field_info info_tmph_headerflag[];

#define tmph_headerflag_absent 0
#define tmph_headerflag_present 1

/* Application Process ID */

#define name_tmph_apid "Application Process ID"
#define text_tmph_apid(val) get_desc(info_tmph_apid, (val))
#define check_tmph_apid(val) search_enumeration(info_tmph_apid, (val))
#define tmph_apid(p) ((0x1*((((p)[1])>>0)&0xff))+(0x100*((((p)[0])>>0)&0x7)))
#define put_tmph_apid(p,val) ((p)[1] = (((p)[1]&~0xff)|(((val)*0x1)&0xff)),(p)[0] = (((p)[0]&~0x7)|(((val)/0x100)&0x7)))

extern field_info info_tmph_apid[];

#define tmph_apid_hk 1793
#define tmph_apid_nm 1795
#define tmph_apid_bm1 1812
#define tmph_apid_bm2 1828

/* Segmentation Flags */

#define name_tmph_segment "Segmentation Flags"
#define text_tmph_segment(val) get_desc(info_tmph_segment, (val))
#define check_tmph_segment(val) search_enumeration(info_tmph_segment, (val))
#define tmph_segment(p) ((0x1*((((p)[2])>>6)&0x3)))
#define put_tmph_segment(p,val) ((p)[2] = (((p)[2]&~0xc0)|(((val)*0x40)&0xc0)))

extern field_info info_tmph_segment[];

#define tmph_segment_continuation 0
#define tmph_segment_first 1
#define tmph_segment_last 2
#define tmph_segment_unsegmented 3

/* Original Source Packet Source Sequence Count */

#define name_tmph_count "Original Source Packet Source Sequence Count"
#define text_tmph_count(val) get_desc(info_tmph_count, (val))
#define check_tmph_count(val) search_enumeration(info_tmph_count, (val))
#define tmph_count(p) ((0x1*((((p)[3])>>0)&0xff))+(0x100*((((p)[2])>>0)&0x3f)))
#define put_tmph_count(p,val) ((p)[3] = (((p)[3]&~0xff)|(((val)*0x1)&0xff)),(p)[2] = (((p)[2]&~0x3f)|(((val)/0x100)&0x3f)))

/* Packet Length */

#define name_tmph_length "Packet Length"
#define text_tmph_length(val) get_desc(info_tmph_length, (val))
#define check_tmph_length(val) search_enumeration(info_tmph_length, (val))
#define tmph_length(p) ((0x1*((((p)[5])>>0)&0xff))+(0x100*((((p)[4])>>0)&0xff)))
#define put_tmph_length(p,val) ((p)[5] = (((p)[5]&~0xff)|(((val)*0x1)&0xff)),(p)[4] = (((p)[4]&~0xff)|(((val)/0x100)&0xff)))

/* SCET P-Field */

#define name_tmph_pfield "SCET P-Field"
#define text_tmph_pfield(val) get_desc(info_tmph_pfield, (val))
#define check_tmph_pfield(val) search_enumeration(info_tmph_pfield, (val))
#define tmph_pfield(p) ((0x1*((((p)[6])>>0)&0xff)))
#define put_tmph_pfield(p,val) ((p)[6] = (((p)[6]&~0xff)|(((val)*0x1)&0xff)))

extern field_info info_tmph_pfield[];

#define tmph_pfield_expected 47

/* SCET Seconds */

#define name_tmph_sec "SCET Seconds"
#define text_tmph_sec(val) get_desc(info_tmph_sec, (val))
#define check_tmph_sec(val) search_enumeration(info_tmph_sec, (val))
#define tmph_sec(p) ((0x1*((((p)[10])>>0)&0xff))+(0x100*((((p)[9])>>0)&0xff))+(0x10000*((((p)[8])>>0)&0xff))+(0x1000000*((((p)[7])>>0)&0xff)))
#define put_tmph_sec(p,val) ((p)[10] = (((p)[10]&~0xff)|(((val)*0x1)&0xff)),(p)[9] = (((p)[9]&~0xff)|(((val)/0x100)&0xff)),(p)[8] = (((p)[8]&~0xff)|(((val)/0x10000)&0xff)),(p)[7] = (((p)[7]&~0xff)|(((val)/0x1000000)&0xff)))

/* SCET Fractions */

#define name_tmph_frac "SCET Fractions"
#define text_tmph_frac(val) get_desc(info_tmph_frac, (val))
#define check_tmph_frac(val) search_enumeration(info_tmph_frac, (val))
#define tmph_frac(p) ((0x1*((((p)[13])>>0)&0xff))+(0x100*((((p)[12])>>0)&0xff))+(0x10000*((((p)[11])>>0)&0xff)))
#define put_tmph_frac(p,val) ((p)[13] = (((p)[13]&~0xff)|(((val)*0x1)&0xff)),(p)[12] = (((p)[12]&~0xff)|(((val)/0x100)&0xff)),(p)[11] = (((p)[11]&~0xff)|(((val)/0x10000)&0xff)))

/* Source data */

#define name_tmph_data "Source data"
#define text_tmph_data(val) get_desc(info_tmph_data, (val))
#define check_tmph_data(val) search_enumeration(info_tmph_data, (val))
#define tmph_data(p) ((p)+14)

/*
 * DDS Packet
 */

/* SCET DAY */

#define name_ddsp_day "SCET DAY"
#define text_ddsp_day(val) get_desc(info_ddsp_day, (val))
#define check_ddsp_day(val) search_enumeration(info_ddsp_day, (val))
#define ddsp_day(p) ((0x1*((((p)[1])>>0)&0xff))+(0x100*((((p)[0])>>0)&0xff)))
#define put_ddsp_day(p,val) ((p)[1] = (((p)[1]&~0xff)|(((val)*0x1)&0xff)),(p)[0] = (((p)[0]&~0xff)|(((val)/0x100)&0xff)))

/* SCET ms of day */

#define name_ddsp_msec "SCET ms of day"
#define text_ddsp_msec(val) get_desc(info_ddsp_msec, (val))
#define check_ddsp_msec(val) search_enumeration(info_ddsp_msec, (val))
#define ddsp_msec(p) ((0x1*((((p)[5])>>0)&0xff))+(0x100*((((p)[4])>>0)&0xff))+(0x10000*((((p)[3])>>0)&0xff))+(0x1000000*((((p)[2])>>0)&0xff)))
#define put_ddsp_msec(p,val) ((p)[5] = (((p)[5]&~0xff)|(((val)*0x1)&0xff)),(p)[4] = (((p)[4]&~0xff)|(((val)/0x100)&0xff)),(p)[3] = (((p)[3]&~0xff)|(((val)/0x10000)&0xff)),(p)[2] = (((p)[2]&~0xff)|(((val)/0x1000000)&0xff)))

/* SCET us of ms */

#define name_ddsp_usec "SCET us of ms"
#define text_ddsp_usec(val) get_desc(info_ddsp_usec, (val))
#define check_ddsp_usec(val) search_enumeration(info_ddsp_usec, (val))
#define ddsp_usec(p) ((0x1*((((p)[7])>>0)&0xff))+(0x100*((((p)[6])>>0)&0xff)))
#define put_ddsp_usec(p,val) ((p)[7] = (((p)[7]&~0xff)|(((val)*0x1)&0xff)),(p)[6] = (((p)[6]&~0xff)|(((val)/0x100)&0xff)))

/* Data source/type ID */

#define name_ddsp_source "Data source/type ID"
#define text_ddsp_source(val) get_desc(info_ddsp_source, (val))
#define check_ddsp_source(val) search_enumeration(info_ddsp_source, (val))
#define ddsp_source(p) ((0x1*((((p)[8])>>0)&0xff)))
#define put_ddsp_source(p,val) ((p)[8] = (((p)[8]&~0xff)|(((val)*0x1)&0xff)))

extern field_info info_ddsp_source[];

#define ddsp_source_hkwecdes 13
#define ddsp_source_wecnsd1 35
#define ddsp_source_wecbsd1 42
#define ddsp_source_wechkd1 49
#define ddsp_source_wecnsd2 75
#define ddsp_source_wecbsd2 82
#define ddsp_source_wechkd2 89
#define ddsp_source_wecnsd3 115
#define ddsp_source_wecbsd3 122
#define ddsp_source_wechkd3 129
#define ddsp_source_wecnsd4 155
#define ddsp_source_wecbsd4 162
#define ddsp_source_wechkd4 169

/* Packet length */

#define name_ddsp_length "Packet length"
#define text_ddsp_length(val) get_desc(info_ddsp_length, (val))
#define check_ddsp_length(val) search_enumeration(info_ddsp_length, (val))
#define ddsp_length(p) ((0x1*((((p)[11])>>0)&0xff))+(0x100*((((p)[10])>>0)&0xff))+(0x10000*((((p)[9])>>0)&0xff)))
#define put_ddsp_length(p,val) ((p)[11] = (((p)[11]&~0xff)|(((val)*0x1)&0xff)),(p)[10] = (((p)[10]&~0xff)|(((val)/0x100)&0xff)),(p)[9] = (((p)[9]&~0xff)|(((val)/0x10000)&0xff)))

/* Spacecraft ID */

#define name_ddsp_spacecraft "Spacecraft ID"
#define text_ddsp_spacecraft(val) get_desc(info_ddsp_spacecraft, (val))
#define check_ddsp_spacecraft(val) search_enumeration(info_ddsp_spacecraft, (val))
#define ddsp_spacecraft(p) ((0x1*((((p)[12])>>4)&0xf)))
#define put_ddsp_spacecraft(p,val) ((p)[12] = (((p)[12]&~0xf0)|(((val)*0x10)&0xf0)))

extern field_info info_ddsp_spacecraft[];

#define ddsp_spacecraft_cluster1 1
#define ddsp_spacecraft_cluster2 2
#define ddsp_spacecraft_cluster3 3
#define ddsp_spacecraft_cluster4 4

/* Ground station ID */

#define name_ddsp_ground "Ground station ID"
#define text_ddsp_ground(val) get_desc(info_ddsp_ground, (val))
#define check_ddsp_ground(val) search_enumeration(info_ddsp_ground, (val))
#define ddsp_ground(p) ((0x1*((((p)[12])>>0)&0xf)))
#define put_ddsp_ground(p,val) ((p)[12] = (((p)[12]&~0xf)|(((val)*0x1)&0xf)))

extern field_info info_ddsp_ground[];

#define ddsp_ground_odenwald 1
#define ddsp_ground_redu 2
#define ddsp_ground_kourou 3
#define ddsp_ground_perth 4
#define ddsp_ground_malindi 5
#define ddsp_ground_canberra 6
#define ddsp_ground_goldstone 7
#define ddsp_ground_na 15

/* Data stream */

#define name_ddsp_stream "Data stream"
#define text_ddsp_stream(val) get_desc(info_ddsp_stream, (val))
#define check_ddsp_stream(val) search_enumeration(info_ddsp_stream, (val))
#define ddsp_stream(p) ((0x1*((((p)[13])>>0)&0xff)))
#define put_ddsp_stream(p,val) ((p)[13] = (((p)[13]&~0xff)|(((val)*0x1)&0xff)))

extern field_info info_ddsp_stream[];

#define ddsp_stream_rtvc0 0
#define ddsp_stream_rtvc2 2
#define ddsp_stream_rtvc3 3
#define ddsp_stream_pbvc0 64
#define ddsp_stream_pbvc2 66
#define ddsp_stream_pbvc3 67
#define ddsp_stream_revc0 240
#define ddsp_stream_revc2 242
#define ddsp_stream_revc3 243
#define ddsp_stream_rpvc0 79
#define ddsp_stream_rpvc2 226
#define ddsp_stream_rpvc3 227
#define ddsp_stream_na 255

/* Time Calibration */

#define name_ddsp_quality "Time Calibration"
#define text_ddsp_quality(val) get_desc(info_ddsp_quality, (val))
#define check_ddsp_quality(val) search_enumeration(info_ddsp_quality, (val))
#define ddsp_quality(p) ((0x1*((((p)[14])>>4)&0xf)))
#define put_ddsp_quality(p,val) ((p)[14] = (((p)[14]&~0xf0)|(((val)*0x10)&0xf0)))

extern field_info info_ddsp_quality[];

#define ddsp_quality_actual 0
#define ddsp_quality_extrapolated 1
#define ddsp_quality_contingency 2

/* Acquisition Sequence ID */

#define name_ddsp_sequence "Acquisition Sequence ID"
#define text_ddsp_sequence(val) get_desc(info_ddsp_sequence, (val))
#define check_ddsp_sequence(val) search_enumeration(info_ddsp_sequence, (val))
#define ddsp_sequence(p) ((0x1*((((p)[14])>>0)&0xf)))
#define put_ddsp_sequence(p,val) ((p)[14] = (((p)[14]&~0xf)|(((val)*0x1)&0xf)))

extern field_info info_ddsp_sequence[];

#define ddsp_sequence_hk1 1
#define ddsp_sequence_nm1 1
#define ddsp_sequence_nm2 2
#define ddsp_sequence_nm3 3
#define ddsp_sequence_bm1 1
#define ddsp_sequence_bm2 2
#define ddsp_sequence_bm3 3

/* Packet data */

#define name_ddsp_data "Packet data"
#define text_ddsp_data(val) get_desc(info_ddsp_data, (val))
#define check_ddsp_data(val) search_enumeration(info_ddsp_data, (val))
#define ddsp_data(p) ((p)+15)

/*
 * Mini-Packet
 */

/* Type */

#define name_minipd_type "Type"
#define text_minipd_type(val) get_desc(info_minipd_type, (val))
#define check_minipd_type(val) search_enumeration(info_minipd_type, (val))
#define minipd_type(p) ((0x1*((((p)[0])>>6)&0x3)))
#define put_minipd_type(p,val) ((p)[0] = (((p)[0]&~0xc0)|(((val)*0x40)&0xc0)))

extern field_info info_minipd_type[];

#define minipd_type_single 0
#define minipd_type_first 1
#define minipd_type_last 2
#define minipd_type_middle 3

/* Source Instrument */

#define name_minipd_source "Source Instrument"
#define text_minipd_source(val) get_desc(info_minipd_source, (val))
#define check_minipd_source(val) search_enumeration(info_minipd_source, (val))
#define minipd_source(p) ((0x1*((((p)[0])>>3)&0x7)))
#define put_minipd_source(p,val) ((p)[0] = (((p)[0]&~0x38)|(((val)*0x8)&0x38)))

extern field_info info_minipd_source[];

#define minipd_source_efw 0
#define minipd_source_staffsa 1
#define minipd_source_staffmwf 2
#define minipd_source_whisper 3
#define minipd_source_wbd 4
#define minipd_source_dwp 5

/* Mini-Packet Count or UDEFs */

#define name_minipd_count "Mini-Packet Count or UDEFs"
#define text_minipd_count(val) get_desc(info_minipd_count, (val))
#define check_minipd_count(val) search_enumeration(info_minipd_count, (val))
#define minipd_count(p) ((0x1*((((p)[0])>>1)&0x3)))
#define put_minipd_count(p,val) ((p)[0] = (((p)[0]&~0x6)|(((val)*0x2)&0x6)))

/* Length of Mini-Packet */

#define name_minipd_length "Length of Mini-Packet"
#define text_minipd_length(val) get_desc(info_minipd_length, (val))
#define check_minipd_length(val) search_enumeration(info_minipd_length, (val))
#define minipd_length(p) ((0x1*((((p)[1])>>0)&0xff))+(0x100*((((p)[0])>>0)&0x1)))
#define put_minipd_length(p,val) ((p)[1] = (((p)[1]&~0xff)|(((val)*0x1)&0xff)),(p)[0] = (((p)[0]&~0x1)|(((val)/0x100)&0x1)))

/* Science data */

#define name_minipd_data "Science data"
#define text_minipd_data(val) get_desc(info_minipd_data, (val))
#define check_minipd_data(val) search_enumeration(info_minipd_data, (val))
#define minipd_data(p) ((p)+2)

/*
 * Science Packet
 */

/* OBDH Reset Pulse Count (modulus 8) */

#define name_sciencepacket_obdhmod8 "OBDH Reset Pulse Count (modulus 8)"
#define text_sciencepacket_obdhmod8(val) get_desc(info_sciencepacket_obdhmod8, (val))
#define check_sciencepacket_obdhmod8(val) search_enumeration(info_sciencepacket_obdhmod8, (val))
#define sciencepacket_obdhmod8(p) ((0x1*((((p)[0])>>5)&0x7)))
#define put_sciencepacket_obdhmod8(p,val) ((p)[0] = (((p)[0]&~0xe0)|(((val)*0x20)&0xe0)))

/* DWP Master Clock Pulse Count */

#define name_sciencepacket_dwpcount "DWP Master Clock Pulse Count"
#define text_sciencepacket_dwpcount(val) get_desc(info_sciencepacket_dwpcount, (val))
#define check_sciencepacket_dwpcount(val) search_enumeration(info_sciencepacket_dwpcount, (val))
#define sciencepacket_dwpcount(p) ((0x1*((((p)[1])>>0)&0xff))+(0x100*((((p)[0])>>0)&0x1f)))
#define put_sciencepacket_dwpcount(p,val) ((p)[1] = (((p)[1]&~0xff)|(((val)*0x1)&0xff)),(p)[0] = (((p)[0]&~0x1f)|(((val)/0x100)&0x1f)))

/* Science data */

#define name_sciencepacket_data "Science data"
#define text_sciencepacket_data(val) get_desc(info_sciencepacket_data, (val))
#define check_sciencepacket_data(val) search_enumeration(info_sciencepacket_data, (val))
#define sciencepacket_data(p) ((p)+2)

/*
 * DSD Header and Data
 */

/* VERSION */

#define name_dsd_version "VERSION"
#define text_dsd_version(val) get_desc(info_dsd_version, (val))
#define check_dsd_version(val) search_enumeration(info_dsd_version, (val))
#define dsd_version(p) ((0x1*((((p)[0])>>0)&0xff)))
#define put_dsd_version(p,val) ((p)[0] = (((p)[0]&~0xff)|(((val)*0x1)&0xff)))

/* REVISION */

#define name_dsd_revision "REVISION"
#define text_dsd_revision(val) get_desc(info_dsd_revision, (val))
#define check_dsd_revision(val) search_enumeration(info_dsd_revision, (val))
#define dsd_revision(p) ((0x1*((((p)[1])>>0)&0xff)))
#define put_dsd_revision(p,val) ((p)[1] = (((p)[1]&~0xff)|(((val)*0x1)&0xff)))

/* PATCH */

#define name_dsd_patch "PATCH"
#define text_dsd_patch(val) get_desc(info_dsd_patch, (val))
#define check_dsd_patch(val) search_enumeration(info_dsd_patch, (val))
#define dsd_patch(p) ((0x1*((((p)[2])>>0)&0xff)))
#define put_dsd_patch(p,val) ((p)[2] = (((p)[2]&~0xff)|(((val)*0x1)&0xff)))

/* USERPATCH */

#define name_dsd_userpatch "USERPATCH"
#define text_dsd_userpatch(val) get_desc(info_dsd_userpatch, (val))
#define check_dsd_userpatch(val) search_enumeration(info_dsd_userpatch, (val))
#define dsd_userpatch(p) ((0x1*((((p)[3])>>0)&0xff)))
#define put_dsd_userpatch(p,val) ((p)[3] = (((p)[3]&~0xff)|(((val)*0x1)&0xff)))

/* SPACECRAFT */

#define name_dsd_spacecraft "SPACECRAFT"
#define text_dsd_spacecraft(val) get_desc(info_dsd_spacecraft, (val))
#define check_dsd_spacecraft(val) search_enumeration(info_dsd_spacecraft, (val))
#define dsd_spacecraft(p) ((0x1*((((p)[5])>>0)&0xff))+(0x100*((((p)[4])>>0)&0xff)))
#define put_dsd_spacecraft(p,val) ((p)[5] = (((p)[5]&~0xff)|(((val)*0x1)&0xff)),(p)[4] = (((p)[4]&~0xff)|(((val)/0x100)&0xff)))

/* GROUND */

#define name_dsd_ground "GROUND"
#define text_dsd_ground(val) get_desc(info_dsd_ground, (val))
#define check_dsd_ground(val) search_enumeration(info_dsd_ground, (val))
#define dsd_ground(p) ((0x1*((((p)[7])>>0)&0xff))+(0x100*((((p)[6])>>0)&0xff)))
#define put_dsd_ground(p,val) ((p)[7] = (((p)[7]&~0xff)|(((val)*0x1)&0xff)),(p)[6] = (((p)[6]&~0xff)|(((val)/0x100)&0xff)))

/* SOURCE */

#define name_dsd_source "SOURCE"
#define text_dsd_source(val) get_desc(info_dsd_source, (val))
#define check_dsd_source(val) search_enumeration(info_dsd_source, (val))
#define dsd_source(p) ((0x1*((((p)[9])>>0)&0xff))+(0x100*((((p)[8])>>0)&0xff)))
#define put_dsd_source(p,val) ((p)[9] = (((p)[9]&~0xff)|(((val)*0x1)&0xff)),(p)[8] = (((p)[8]&~0xff)|(((val)/0x100)&0xff)))

/* DW */

#define name_dsd_dw "DW"
#define text_dsd_dw(val) get_desc(info_dsd_dw, (val))
#define check_dsd_dw(val) search_enumeration(info_dsd_dw, (val))
#define dsd_dw(p) ((0x1*((((p)[11])>>0)&0xff))+(0x100*((((p)[10])>>0)&0xff)))
#define put_dsd_dw(p,val) ((p)[11] = (((p)[11]&~0xff)|(((val)*0x1)&0xff)),(p)[10] = (((p)[10]&~0xff)|(((val)/0x100)&0xff)))

/* LENGTH */

#define name_dsd_length "LENGTH"
#define text_dsd_length(val) get_desc(info_dsd_length, (val))
#define check_dsd_length(val) search_enumeration(info_dsd_length, (val))
#define dsd_length(p) ((0x1*((((p)[13])>>0)&0xff))+(0x100*((((p)[12])>>0)&0xff)))
#define put_dsd_length(p,val) ((p)[13] = (((p)[13]&~0xff)|(((val)*0x1)&0xff)),(p)[12] = (((p)[12]&~0xff)|(((val)/0x100)&0xff)))

/* YEAR */

#define name_dsd_year "YEAR"
#define text_dsd_year(val) get_desc(info_dsd_year, (val))
#define check_dsd_year(val) search_enumeration(info_dsd_year, (val))
#define dsd_year(p) ((0x1*((((p)[15])>>0)&0xff))+(0x100*((((p)[14])>>0)&0xff)))
#define put_dsd_year(p,val) ((p)[15] = (((p)[15]&~0xff)|(((val)*0x1)&0xff)),(p)[14] = (((p)[14]&~0xff)|(((val)/0x100)&0xff)))

/* MONTH */

#define name_dsd_month "MONTH"
#define text_dsd_month(val) get_desc(info_dsd_month, (val))
#define check_dsd_month(val) search_enumeration(info_dsd_month, (val))
#define dsd_month(p) ((0x1*((((p)[17])>>0)&0xff))+(0x100*((((p)[16])>>0)&0xff)))
#define put_dsd_month(p,val) ((p)[17] = (((p)[17]&~0xff)|(((val)*0x1)&0xff)),(p)[16] = (((p)[16]&~0xff)|(((val)/0x100)&0xff)))

/* DAY */

#define name_dsd_day "DAY"
#define text_dsd_day(val) get_desc(info_dsd_day, (val))
#define check_dsd_day(val) search_enumeration(info_dsd_day, (val))
#define dsd_day(p) ((0x1*((((p)[19])>>0)&0xff))+(0x100*((((p)[18])>>0)&0xff)))
#define put_dsd_day(p,val) ((p)[19] = (((p)[19]&~0xff)|(((val)*0x1)&0xff)),(p)[18] = (((p)[18]&~0xff)|(((val)/0x100)&0xff)))

/* HOUR */

#define name_dsd_hour "HOUR"
#define text_dsd_hour(val) get_desc(info_dsd_hour, (val))
#define check_dsd_hour(val) search_enumeration(info_dsd_hour, (val))
#define dsd_hour(p) ((0x1*((((p)[21])>>0)&0xff))+(0x100*((((p)[20])>>0)&0xff)))
#define put_dsd_hour(p,val) ((p)[21] = (((p)[21]&~0xff)|(((val)*0x1)&0xff)),(p)[20] = (((p)[20]&~0xff)|(((val)/0x100)&0xff)))

/* MIN */

#define name_dsd_min "MIN"
#define text_dsd_min(val) get_desc(info_dsd_min, (val))
#define check_dsd_min(val) search_enumeration(info_dsd_min, (val))
#define dsd_min(p) ((0x1*((((p)[23])>>0)&0xff))+(0x100*((((p)[22])>>0)&0xff)))
#define put_dsd_min(p,val) ((p)[23] = (((p)[23]&~0xff)|(((val)*0x1)&0xff)),(p)[22] = (((p)[22]&~0xff)|(((val)/0x100)&0xff)))

/* SEC */

#define name_dsd_sec "SEC"
#define text_dsd_sec(val) get_desc(info_dsd_sec, (val))
#define check_dsd_sec(val) search_enumeration(info_dsd_sec, (val))
#define dsd_sec(p) ((0x1*((((p)[25])>>0)&0xff))+(0x100*((((p)[24])>>0)&0xff)))
#define put_dsd_sec(p,val) ((p)[25] = (((p)[25]&~0xff)|(((val)*0x1)&0xff)),(p)[24] = (((p)[24]&~0xff)|(((val)/0x100)&0xff)))

/* MSEC */

#define name_dsd_msec "MSEC"
#define text_dsd_msec(val) get_desc(info_dsd_msec, (val))
#define check_dsd_msec(val) search_enumeration(info_dsd_msec, (val))
#define dsd_msec(p) ((0x1*((((p)[27])>>0)&0xff))+(0x100*((((p)[26])>>0)&0xff)))
#define put_dsd_msec(p,val) ((p)[27] = (((p)[27]&~0xff)|(((val)*0x1)&0xff)),(p)[26] = (((p)[26]&~0xff)|(((val)/0x100)&0xff)))

/* USEC */

#define name_dsd_usec "USEC"
#define text_dsd_usec(val) get_desc(info_dsd_usec, (val))
#define check_dsd_usec(val) search_enumeration(info_dsd_usec, (val))
#define dsd_usec(p) ((0x1*((((p)[29])>>0)&0xff))+(0x100*((((p)[28])>>0)&0xff)))
#define put_dsd_usec(p,val) ((p)[29] = (((p)[29]&~0xff)|(((val)*0x1)&0xff)),(p)[28] = (((p)[28]&~0xff)|(((val)/0x100)&0xff)))

/* OBDHMOD8 */

#define name_dsd_obdhmod8 "OBDHMOD8"
#define text_dsd_obdhmod8(val) get_desc(info_dsd_obdhmod8, (val))
#define check_dsd_obdhmod8(val) search_enumeration(info_dsd_obdhmod8, (val))
#define dsd_obdhmod8(p) ((0x1*((((p)[30])>>5)&0x7)))
#define put_dsd_obdhmod8(p,val) ((p)[30] = (((p)[30]&~0xe0)|(((val)*0x20)&0xe0)))

/* DWPCOUNT */

#define name_dsd_dwpcount "DWPCOUNT"
#define text_dsd_dwpcount(val) get_desc(info_dsd_dwpcount, (val))
#define check_dsd_dwpcount(val) search_enumeration(info_dsd_dwpcount, (val))
#define dsd_dwpcount(p) ((0x1*((((p)[31])>>0)&0xff))+(0x100*((((p)[30])>>0)&0x1f)))
#define put_dsd_dwpcount(p,val) ((p)[31] = (((p)[31]&~0xff)|(((val)*0x1)&0xff)),(p)[30] = (((p)[30]&~0x1f)|(((val)/0x100)&0x1f)))

/* _ */

#define name_dsd__ "_"
#define text_dsd__(val) get_desc(info_dsd__, (val))
#define check_dsd__(val) search_enumeration(info_dsd__, (val))
#define dsd__(p) ((0x1*((((p)[33])>>2)&0x3f))+(0x40*((((p)[32])>>0)&0xff)))
#define put_dsd__(p,val) ((p)[33] = (((p)[33]&~0xfc)|(((val)*0x4)&0xfc)),(p)[32] = (((p)[32]&~0xff)|(((val)/0x40)&0xff)))

/* UDEF1 */

#define name_dsd_udef1 "UDEF1"
#define text_dsd_udef1(val) get_desc(info_dsd_udef1, (val))
#define check_dsd_udef1(val) search_enumeration(info_dsd_udef1, (val))
#define dsd_udef1(p) ((0x1*((((p)[33])>>1)&0x1)))
#define put_dsd_udef1(p,val) ((p)[33] = (((p)[33]&~0x2)|(((val)*0x2)&0x2)))

/* UDEF0 */

#define name_dsd_udef0 "UDEF0"
#define text_dsd_udef0(val) get_desc(info_dsd_udef0, (val))
#define check_dsd_udef0(val) search_enumeration(info_dsd_udef0, (val))
#define dsd_udef0(p) ((0x1*((((p)[33])>>0)&0x1)))
#define put_dsd_udef0(p,val) ((p)[33] = (((p)[33]&~0x1)|(((val)*0x1)&0x1)))

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /*!_unpack_xmacros_h*/
