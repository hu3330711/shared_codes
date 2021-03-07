/* SDTDataBaseXDR.h */
/* This file contains declarations for RPC-XDR for SDTDataBase widgits: */

#ifndef SDTDATABASEXDR_H
#define SDTDATABASEXDR_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include <rpc/rpc.h>
#include <SDTDataBase.h>

/* SCCS ID string: */
#define SccsId_SDTDataBaseXDR_h "@(#)SDTDataBaseXDR.h	1.7, 02/24/04"

#ifndef _RPC_HNDL_FNC
#define _RPC_HNDL_FNC

/* Type declaration for SDT RPC Handler functions: */
typedef void (*SDTRPCHandlerFnc) (struct svc_req *rqstp, SVCXPRT *transp) ;
#endif /* ifndef _RPC_HNDL_FNC */

extern bool_t xdr_DataBaseResponse_struct(XDR *, struct DataBaseResponse_struct *);
extern bool_t xdr_DataBaseResponse(XDR *, DataBaseResponse *);

extern bool_t xdr_DataBaseResponseList_struct(XDR *, struct DataBaseResponseList_struct *);
extern bool_t xdr_DataBaseResponseList(XDR *, DataBaseResponseList *);

extern bool_t xdr_QueryTimeSpanIsCountedResponse_struct(XDR *, struct QueryTimeSpanIsCountedResponse_struct *);
extern bool_t xdr_QueryTimeSpanIsCountedResponse(XDR *, QueryTimeSpanIsCountedResponse *);

extern bool_t xdr_DataBaseEntryIsReady_struct(XDR *, struct DataBaseEntryIsReady_struct *) ;
extern bool_t xdr_DataBaseEntryIsReady(XDR *, DataBaseEntryIsReady *) ;

extern bool_t xdr_NRTSyncInformation_struct(XDR *xdrs, struct NRTSyncInformation_struct *objp) ;

extern bool_t xdr_NRTSyncInformation(XDR *xdrs, NRTSyncInformation *objp) ;


#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* SDTDATABASEXDR_H */

