/*
 * @(#)version.h	1.1, 05/23/00
 *
 * Project:         CLUSTER WEC TED Package
 *
 * Item Name:       src/version/version.h
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
#define  SccsId_version_h  "@(#)version.h	1.1, 05/23/00"

#ifndef _version_version_h
#define _version_version_h

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

extern TED_STATUS ted_version_ted(
int *version,
int *revision,
int *patch,
int *userpatch
);

extern TED_STATUS ted_version_lib(
int *version,
int *revision,
int *patch,
int *userpatch
);

extern TED_STATUS ted_version_ted_userpatch(
int userpatch
);

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /*!_version_version_h*/
