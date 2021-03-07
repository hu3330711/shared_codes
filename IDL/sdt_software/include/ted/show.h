/*
 * @(#)show.h	1.1, 05/23/00
 *
 * Project:         CLUSTER WEC TED Package
 *
 * Item Name:       src/levelone/show.h
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
#define  SccsId_show_h  "@(#)show.h	1.1, 05/23/00"

#ifndef _levelone_show_h
#define _levelone_show_h

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include "ted.h"
#include "tedsys.h"
#include "dsd.h"
#include "misc.h"
#include "xmacros.h"

void ddsp_show_header(char *packet);
void dsdheader_show(char *header);
void minipd_show(char *mp);
void dsditems_show(TED_DSD_ITEMS *items);

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /*!_levelone_show_h*/
