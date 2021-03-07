/* TDefsForLinux.h */

#ifndef TDEFSFORLINUX_H
#define TDEFSFORLINUX_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* This file exists to define things that are part of the Solaris include
 * files, but not Linux.  E.g.  boolean_t
 */

/* SCCS ID string: */
#define SccsId_TDefsForLinux_h "@(#)TDefsForLinux.h	1.1, 06/19/99"

/* "boolean_t" is in /usr/include/sys/types.h in Solaris but not Linux: */
typedef enum { B_FALSE, B_TRUE } boolean_t;

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* TDEFSFORLINUX_H */
