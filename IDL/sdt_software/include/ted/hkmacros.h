/*
 * @(#)hkmacros.h	1.1, 05/23/00
 *
 * Project:         CLUSTER WEC TED Package
 *
 * Item Name:       src/ted/hkmacros.h
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
#define  SccsId_hkmacros_h  "@(#)hkmacros.h	1.1, 05/23/00"

#ifndef _dsd_hkmacros_h
#define _dsd_hkmacros_h

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include "ted.h"
#include "xmacros.h"

#define hkblock_EW1AMODE(p) ((0x1*((((p)[112])>>0)&0xf)))
#define put_hkblock_EW1AMODE(p,val) ((p)[112] = (((p)[112]&~0xf)|(((val)*0x1)&0xf)))
#define hkblock_EW1TMON(p) ((0x1*((((p)[101])>>0)&0xff)))
#define put_hkblock_EW1TMON(p,val) ((p)[101] = (((p)[101]&~0xff)|(((val)*0x1)&0xff)))
#define hkblock_EW1VMON0(p) ((0x1*((((p)[93])>>0)&0xff)))
#define put_hkblock_EW1VMON0(p,val) ((p)[93] = (((p)[93]&~0xff)|(((val)*0x1)&0xff)))
#define hkblock_EW1VMON1(p) ((0x1*((((p)[92])>>0)&0xff)))
#define put_hkblock_EW1VMON1(p,val) ((p)[92] = (((p)[92]&~0xff)|(((val)*0x1)&0xff)))
#define hkblock_EW1VMON2(p) ((0x1*((((p)[95])>>0)&0xff)))
#define put_hkblock_EW1VMON2(p,val) ((p)[95] = (((p)[95]&~0xff)|(((val)*0x1)&0xff)))
#define hkblock_EW2CALMD(p) ((0x1*((((p)[112])>>7)&0x1)))
#define put_hkblock_EW2CALMD(p,val) ((p)[112] = (((p)[112]&~0x80)|(((val)*0x80)&0x80)))
#define hkblock_EW2CALST(p) ((0x1*((((p)[113])>>0)&0xff)))
#define put_hkblock_EW2CALST(p,val) ((p)[113] = (((p)[113]&~0xff)|(((val)*0x1)&0xff)))
#define hkblock_EW2TMON0(p) ((0x1*((((p)[97])>>0)&0xff)))
#define put_hkblock_EW2TMON0(p,val) ((p)[97] = (((p)[97]&~0xff)|(((val)*0x1)&0xff)))
#define hkblock_EW2TMON1(p) ((0x1*((((p)[96])>>0)&0xff)))
#define put_hkblock_EW2TMON1(p,val) ((p)[96] = (((p)[96]&~0xff)|(((val)*0x1)&0xff)))
#define hkblock_EW2VMON0(p) ((0x1*((((p)[87])>>0)&0xff)))
#define put_hkblock_EW2VMON0(p,val) ((p)[87] = (((p)[87]&~0xff)|(((val)*0x1)&0xff)))
#define hkblock_EW2VMON1(p) ((0x1*((((p)[86])>>0)&0xff)))
#define put_hkblock_EW2VMON1(p,val) ((p)[86] = (((p)[86]&~0xff)|(((val)*0x1)&0xff)))
#define hkblock_EW2VMON2(p) ((0x1*((((p)[89])>>0)&0xff)))
#define put_hkblock_EW2VMON2(p,val) ((p)[89] = (((p)[89]&~0xff)|(((val)*0x1)&0xff)))
#define hkblock_EW2VMON3(p) ((0x1*((((p)[88])>>0)&0xff)))
#define put_hkblock_EW2VMON3(p,val) ((p)[88] = (((p)[88]&~0xff)|(((val)*0x1)&0xff)))
#define hkblock_EW3PCFBS(p) ((0x1*((((p)[46])>>6)&0x3)))
#define put_hkblock_EW3PCFBS(p,val) ((p)[46] = (((p)[46]&~0xc0)|(((val)*0x40)&0xc0)))
#define hkblock_EW3SRPOF(p) ((0x1*((((p)[46])>>0)&0xf)))
#define put_hkblock_EW3SRPOF(p,val) ((p)[46] = (((p)[46]&~0xf)|(((val)*0x1)&0xf)))
#define hkblock_EW5ACQMD(p) ((0x1*((((p)[23])>>0)&0xff)))
#define put_hkblock_EW5ACQMD(p,val) ((p)[23] = (((p)[23]&~0xff)|(((val)*0x1)&0xff)))
#define hkblock_EW5CORDS(p) ((0x1*((((p)[9])>>2)&0x1)))
#define put_hkblock_EW5CORDS(p,val) ((p)[9] = (((p)[9]&~0x4)|(((val)*0x4)&0x4)))
#define hkblock_EW5EMCST(p) ((0x1*((((p)[21])>>2)&0x1)))
#define put_hkblock_EW5EMCST(p,val) ((p)[21] = (((p)[21]&~0x4)|(((val)*0x4)&0x4)))
#define hkblock_EW5MOTAG(p) ((0x1*((((p)[1])>>0)&0xff))+(0x100*((((p)[0])>>0)&0xff)))
#define put_hkblock_EW5MOTAG(p,val) ((p)[1] = (((p)[1]&~0xff)|(((val)*0x1)&0xff)),(p)[0] = (((p)[0]&~0xff)|(((val)/0x100)&0xff)))
#define hkblock_EW5PRCTL(p) ((0x1*((((p)[41])>>0)&0xff))+(0x100*((((p)[40])>>0)&0xff)))
#define put_hkblock_EW5PRCTL(p,val) ((p)[41] = (((p)[41]&~0xff)|(((val)*0x1)&0xff)),(p)[40] = (((p)[40]&~0xff)|(((val)/0x100)&0xff)))
#define hkblock_EW5RSCNT(p) ((0x1*((((p)[3])>>0)&0xff))+(0x100*((((p)[2])>>0)&0xff)))
#define put_hkblock_EW5RSCNT(p,val) ((p)[3] = (((p)[3]&~0xff)|(((val)*0x1)&0xff)),(p)[2] = (((p)[2]&~0xff)|(((val)/0x100)&0xff)))
#define hkblock_EW5VMON(p) ((0x1*((((p)[94])>>0)&0xff)))
#define put_hkblock_EW5VMON(p,val) ((p)[94] = (((p)[94]&~0xff)|(((val)*0x1)&0xff)))
#define hkblock_EW5ESTMR(p) ((0x1*((((p)[7])>>6)&0x1)))
#define put_hkblock_EW5ESTMR(p,val) ((p)[7] = (((p)[7]&~0x40)|(((val)*0x40)&0x40)))
#define hkblock_EW5SSOFF(p) ((0x1*((((p)[5])>>0)&0xff))+(0x100*((((p)[4])>>0)&0xff)))
#define put_hkblock_EW5SSOFF(p,val) ((p)[5] = (((p)[5]&~0xff)|(((val)*0x1)&0xff)),(p)[4] = (((p)[4]&~0xff)|(((val)/0x100)&0xff)))
#define hkblock_EW5SCFRQ(p) ((0x1*((((p)[21])>>1)&0x1)))
#define put_hkblock_EW5SCFRQ(p,val) ((p)[21] = (((p)[21]&~0x2)|(((val)*0x2)&0x2)))

/* OBDH Acquisition Mode */

extern field_info info_hkblock_EW5ACQMD[];

#define text_hkblock_EW5ACQMD(val) get_desc(info_hkblock_EW5ACQMD, (val))

#define hkblock_EW5ACQMD_NormalMode1 150
#define hkblock_EW5ACQMD_NormalMode2 151
#define hkblock_EW5ACQMD_NormalMode3 152
#define hkblock_EW5ACQMD_BurstMode1 153
#define hkblock_EW5ACQMD_BurstMode2 154
#define hkblock_EW5ACQMD_BurstMode3 155

/* DWP Model Tag */

extern field_info info_hkblock_EW5MOTAG[];

#define text_hkblock_EW5MOTAG(val) get_desc(info_hkblock_EW5MOTAG, (val))

#define hkblock_EW5MOTAG_FM1 52481
#define hkblock_EW5MOTAG_FM2 52482
#define hkblock_EW5MOTAG_FM3 52483
#define hkblock_EW5MOTAG_FM4 52484
#define hkblock_EW5MOTAG_FM5 52485
#define hkblock_EW5MOTAG_PEM 52633
#define hkblock_EW5MOTAG_EM1 52634
#define hkblock_EW5MOTAG_EM2 52635

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /*!_dsd_hkmacros_h*/
