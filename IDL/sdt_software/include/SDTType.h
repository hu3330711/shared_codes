/* SDTType.h */
/* This file contains declarations about miscellaneous SDT data types: */

#ifndef SDTTYPE_H
#define SDTTYPE_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* SCCS ID string: */
#define SccsId_SDTType_h "@(#)SDTType.h	1.5, 12/02/06"

/* SSL Data Tools - basic data types. */

/* These are type extensions */
#define int16	short
#define int32	int

/* 2006/03/19:  Required for the port to 64-bits.  We can no longer
 * assume that memory addressing is limited to 32-bits, so we create
 * a new type with which all memory addresses (particularly SHM and
 * MMapped) will be referred to.
 */
#ifndef AddrT
typedef  long  AddrT ;
#endif

struct  vecn_struct
    {
    float     time ;
    float     *v ;
    } ;
typedef  struct  vecn_struct  vecn_typ ;

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* SDTTYPE_H */

