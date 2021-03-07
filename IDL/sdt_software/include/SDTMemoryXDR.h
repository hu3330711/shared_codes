/* SDTMemoryXDR.h */
/* This file contains declarations for RPC-XDR for SDTMemory widgits: */

#ifndef SDTMEMORYXDR_H
#define SDTMEMORYXDR_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include <rpc/rpc.h>
#include <SDTDescription.h>
#include <SDTMemory.h>

/* SCCS ID string: */
#define SccsId_SDTMemoryXDR_h "@(#)SDTMemoryXDR.h	1.5, 02/24/04"

#ifndef _RPC_HNDL_FNC
#define _RPC_HNDL_FNC

/* Type declaration for SDT RPC Handler functions: */
typedef void (*SDTRPCHandlerFnc) (struct svc_req *rqstp, SVCXPRT *transp) ;
#endif /* ifndef _RPC_HNDL_FNC */

extern bool_t xdr_SharedMemoryUnit_struct(XDR *, struct SharedMemoryUnit_struct *);
extern bool_t xdr_SharedMemoryUnit(XDR *, SharedMemoryUnit *);

extern bool_t xdr_MappedMemoryUnit_struct(XDR *, struct MappedMemoryUnit_struct *);
extern bool_t xdr_MappedMemoryUnit(XDR *, MappedMemoryUnit *);

extern bool_t xdr_MemoryUnit_struct(XDR *, struct MemoryUnit_struct *);
extern bool_t xdr_MemoryUnit(XDR *, MemoryUnit *);

extern bool_t xdr_MemorySegment_struct(XDR *, struct MemorySegment_struct *);
extern bool_t xdr_MemorySegment(XDR *, MemorySegment *);

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* SDTMEMORYXDR_H */

