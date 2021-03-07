
#define  SccsId_hkgsidh_h  "@(#)hkgsidh.h	1.1, 05/23/00"

#ifndef _hkgsid_h
#define _hkgsid_h
typedef enum {
	HkextractGsidUNKNOWN = 0,	 /* Unknown */
	HkextractGsidODENWALD = 1,	 /* Odenwald */
	HkextractGsidREDU = 2,	 /* Redu */
	HkextractGsidKOUROU = 3,	 /* Kourou */
	HkextractGsidPERTH = 4,	 /* Perth */
	HkextractGsidMALINDI = 5,	 /* Malindi */
	HkextractGsidCANBERRA = 6,	 /* Canberra */
	HkextractGsidGOLDSTONE = 7,	 /* Goldstone */
	HkextractGsidNA = 15	 /* N/A */
} HkextractGsid;
#endif
