/* SDTMemory.h */
/* This file contains declarations for Memory widgits (SHM and Mmaped): */

#ifndef SDTMEMORY_H
#define SDTMEMORY_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* SCCS ID string: */
#define SccsId_SDTMemory_h "@(#)SDTMemory.h	1.6, 12/02/06"

#include <SDTType.h>

/* ----------------------------------------------------------------------- */
/* Constants: */

/* This is the header size of every MemoryUnit.  This is found at the very
 * beginning of the buffer space belonging to a Memory Unit.
 *
 * As of 93/3/21, this header area is not used.  If used later, it may need
 * to be increased in size.
 *    JBV
 */
#define  SizeOfMemoryUnitHeader  512

#define SharedMemoryType    0
#define MappedMemoryType    1
#define DefaultMemoryType   2


/* ----------------------------------------------------------------------- */
/* Structures: */


/* ----------------------------------------------------------------------- */
struct SharedMemoryUnit_struct
   {
   int           ShmKey ;    /* Used only if "type" is "SharedMemoryType".
			      * The key of the shared-memory segment.
			      */
   int           ShmId ;     /* Used only if "type" is "SharedMemoryType".
			      * The shared-memory identifier of the
			      * shared-memory segment.  If -1, then this
			      * shared memory segment is not yet attached.
			      */
   } ;
typedef  struct SharedMemoryUnit_struct  SharedMemoryUnit ;

/* ----------------------------------------------------------------------- */
struct MappedMemoryUnit_struct
   {
   String        MmapName ;  /* Used only if "type" is "MappedMemoryType".
			      * The name of the file used for the memory
			      * mapping.
			      */
   AddrT         MmapStart ; /* Used only if "type" is "MappedMemoryType".
			      * The starting byte in the file for this
			      * memory mapping.  Usually will be zero
			      * (i.e. the start of the file).
			      */
   } ;
typedef  struct MappedMemoryUnit_struct  MappedMemoryUnit ;

/* ----------------------------------------------------------------------- */
union MemoryUnitType_union
    {
    SharedMemoryUnit    shar ;
    MappedMemoryUnit    mmap ;
    } ;
typedef  union MemoryUnitType_union  MemoryUnitType ;

/* ----------------------------------------------------------------------- */
/* The following structure is used to keep track of the Shared-Memory
 * Segments or Memory-Mapped Files, collectively known as MemoryUnits,
 * are currently open:
 */
struct MemoryUnit_struct
   {
   int           Type ;      /* Indicates what type of memory unit
			      * (Shared or Mmap'ed) this is:
			      *    SharedMemoryType -> SysV shared-memory
			      *    MappedMemoryType -> memory mapped files
			      */
   MemoryUnitType  mt ;      /* Stores the type-dependent MemoryUnit
			      * information.
			      */
   AddrT         Size ;      /* The total size of the memory unit, in bytes. */
   char          *Address ;  /* The local, mapped address of the start of the
			      * memory unit.
			      */
   int           DQICount ;  /* The number of DQI's whose data buffers
			      * currently reside inside the MemoryUnit.
			      * This is for use only by DQH client usage
			      * of the DQH Interface, so that it will know
			      * when it no longer needs a MemoryUnit
			      * attachment.
			      */
   } ;
typedef  struct MemoryUnit_struct  MemoryUnit ;

/* ----------------------------------------------------------------------- */
/* The following structure describes a "partition" or "segment" of memory. 
 * This is an offset and size of a segment of memory, usually within a
 * MemoryUnit.
 */
struct MemorySegment_struct
   {
   AddrT        Size ;      /* The size of the segment, in bytes. */
   AddrT        Offset ;    /* The byte-offset of the starting byte of
			     * this memory segment, with respect to the
			     * first byte of a MemoryUnit.
			     */
   } ;
typedef  struct MemorySegment_struct  MemorySegment ;

/* ----------------------------------------------------------------------- */

extern MemoryUnit  *SDTConstructMemoryUnit (int in_Type,
    char * in_MmapName, AddrT in_MmapStart, int in_ShmKey, int in_ShmId,
    AddrT in_Size, char *in_Address) ;

extern MemoryUnit  *SDTCopyConstructMemoryUnit (MemoryUnit *in_munit) ;

extern int  SDTFillMemoryUnit (MemoryUnit *in_munit, int in_Type,
    char * in_MmapName, AddrT in_MmapStart, int in_ShmKey, int in_ShmId,
    AddrT in_Size, char *in_Address) ;

extern int  SDTCopyMemoryUnit (MemoryUnit *in_munit,
    MemoryUnit *out_munit) ;

extern int  SDTDestructMemoryUnit (MemoryUnit *munit) ;

extern int  SDTClearMemoryUnit (MemoryUnit *munit) ;

extern int  SDTPrintMemoryUnit (MemoryUnit *munit, FILE *ofp) ;

extern MemorySegment  *SDTConstructMemorySegment (AddrT in_Size,
    AddrT in_Offset) ;

extern MemorySegment  *SDTCopyConstructMemorySegment (
    MemorySegment *in_msgmt) ;

extern int  SDTFillMemorySegment (MemorySegment *in_msgmt,
    AddrT in_Size, AddrT in_Offset) ;

extern int  SDTCopyMemorySegment (MemorySegment *in_msgmt,
    MemorySegment *out_msgmt) ;

extern int  SDTDestructMemorySegment (MemorySegment *msgmt) ;

extern int  SDTClearMemorySegment (MemorySegment *msgmt) ;

extern int  SDTPrintMemorySegment (MemorySegment *msgmt, FILE *ofp) ;

extern int  SDTSetMemoryUnitAddress (MemoryUnit *munit) ;

extern int  SDTDetachMemoryUnit (MemoryUnit *munit) ;

extern int  SDTGenerateMemoryUnit (MemoryUnit *munit) ;

extern int  SDTRemoveMemoryUnit (MemoryUnit *munit) ;

#ifdef OLD
extern int  SDTAttachMemorySegment (MemoryUnit *mu, MemorySegment *md) ;
#endif /* OLD */

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* SDTMEMORY_H */

