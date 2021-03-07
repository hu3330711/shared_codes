
#define  SccsId_hkstrmh_h  "@(#)hkstrmh.h	1.1, 05/23/00"

#ifndef _hkstream_h
#define _hkstream_h
typedef enum {
	HkextractStreamRT_VC0 = 0x00,	/* Real Time VC0 Stream */
	HkextractStreamRT_VC2 = 0x02,	/* Real Time VC2 Stream */
	HkextractStreamRT_VC3 = 0x03,	/* Real Time VC3 Stream */
	HkextractStreamPB_VC0 = 0x40,	/* Playback VC0 Stream */
	HkextractStreamPB_VC2 = 0x42,	/* Playback VC2 Stream */
	HkextractStreamPB_VC3 = 0x43,	/* Playback VC3 Stream */
	HkextractStreamRE_VC0 = 0xf0,	/* Recall VC0 Stream */
	HkextractStreamRE_VC2 = 0xf2,	/* Recall VC2 Stream */
	HkextractStreamRE_VC3 = 0xf3,	/* Recall VC3 Stream */
	HkextractStreamRP_VC0 = 0x4f,	/* Recall Playback VC0 Stream */
	HkextractStreamRP_VC2 = 0xe2,	/* Recall Playback VC2 Stream */
	HkextractStreamRP_VC3 = 0xe3,	/* Recall Playback VC3 Stream */
	HkextractStreamNA = 0xff	/* Not Applicable */
} HkextractStream;
#endif
