/*
 * @(#)toolbox.h	1.1, 05/23/00
 *
 * Project:         CLUSTER WEC TED Package
 *
 * Item Name:       src/toolbox/toolbox.h
 *
 * Item Version:    (see RCS header)
 *
 * Item Type:       c-source
 *
 * Author:          Toby Champion
 *
 * Modified for thread-safeness and SCCS (98/07):  Jack Vernetti SSL/UCB
 *
 */
#define  SccsId_toolbox_h  "@(#)toolbox.h	1.1, 05/23/00"

#ifndef _toolbox_toolbox_h
#define _toolbox_toolbox_h

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include "ted.h"

extern TED_STATUS ted_toolbox_status2string(
TED_STATUS status,
char **string
);

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /*!_toolbox_toolbox_h*/
