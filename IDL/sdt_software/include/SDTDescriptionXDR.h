/* SDTDescriptionXDR.h */
/* This file contains declarations for RPC-XDR for SDTDescription widgits: */

#ifndef SDTDESCRIPTIONXDR_H
#define SDTDESCRIPTIONXDR_H

#include <rpc/rpc.h>

#include <SDTDescription.h>

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* SCCS ID string: */
#define SccsId_SDTDescriptionXDR_h "@(#)SDTDescriptionXDR.h	1.14, 12/02/06"

#ifndef _RPC_HNDL_FNC
#define _RPC_HNDL_FNC

/* Type declaration for SDT RPC Handler functions: */
typedef void (*SDTRPCHandlerFnc) (struct svc_req *rqstp, SVCXPRT *transp) ;
#endif /* ifndef _RPC_HNDL_FNC */

#ifndef SUNOS4

extern bool_t xdr_VoidList(XDR *, VoidList *);
extern bool_t xdr_IntList(XDR *, IntList *);
extern bool_t xdr_Int16List(XDR *, Int16List *);
extern bool_t xdr_Int32List(XDR *, Int32List *);
extern bool_t xdr_AddrTList(XDR *, AddrTList *);
extern bool_t xdr_FloatList(XDR *, FloatList *);
extern bool_t xdr_DoubleList(XDR *, DoubleList *);
extern bool_t xdr_String(XDR *, String *);
extern bool_t xdr_StringList(XDR *, StringList *);
extern bool_t xdr_SpaceCraftDescription(XDR *, SpaceCraftDescription *);
extern bool_t xdr_TimeSpanDescription(XDR *, TimeSpanDescription *);
extern bool_t xdr_DataQuantityIdentifier(XDR *, DataQuantityIdentifier *);
extern bool_t xdr_DataQuantityIdentifierList(XDR *, DataQuantityIdentifierList *);
extern bool_t xdr_ComponentSubDimension(XDR *, ComponentSubDimension *);
extern bool_t xdr_ComponentSubDimensionList(XDR *, ComponentSubDimensionList *);
extern bool_t xdr_ComponentDescription(XDR *, ComponentDescription *);
extern bool_t xdr_ComponentDescriptionList(XDR *, ComponentDescriptionList *);
extern bool_t xdr_DataQuantityDescription(XDR *, DataQuantityDescription *);
extern bool_t xdr_DataQuantityDescriptionList(XDR *, DataQuantityDescriptionList *);
extern bool_t xdr_ComponentUse(XDR *, ComponentUse *);
extern bool_t xdr_ComponentUseList(XDR *, ComponentUseList *);

extern bool_t xdr_ComponentAccumulation_struct(XDR *xdrs, struct ComponentAccumulation_struct *) ;
extern bool_t xdr_ComponentAccumulation(XDR *xdrs, ComponentAccumulation *) ;
extern bool_t xdr_ComponentAccumulationList_struct(XDR *xdrs, struct ComponentAccumulationList_struct *) ;
extern bool_t xdr_ComponentAccumulationList(XDR *xdrs, ComponentAccumulationList *) ;

extern bool_t xdr_DrawingUnit_struct(XDR *xdrs, struct DrawingUnit_struct *) ;
extern bool_t xdr_DrawingUnit(XDR *xdrs, DrawingUnit *) ;
extern bool_t xdr_DrawingUnitList_struct(XDR *xdrs, struct DrawingUnitList_struct *) ;
extern bool_t xdr_DrawingUnitList(XDR *xdrs, DrawingUnitList *) ;

extern bool_t xdr_AxisUnit_struct(XDR *xdrs, struct AxisUnit_struct *) ;
extern bool_t xdr_AxisUnit(XDR *xdrs, AxisUnit *) ;
extern bool_t xdr_AxisUnitList_struct(XDR *xdrs, struct AxisUnitList_struct *) ;
extern bool_t xdr_AxisUnitList(XDR *xdrs, AxisUnitList *) ;

extern bool_t xdr_ColorLine_struct(XDR *xdrs, struct ColorLine_struct *) ;
extern bool_t xdr_ColorLine(XDR *xdrs, ColorLine *) ;

extern bool_t xdr_PlotQuantityDescription(XDR *, PlotQuantityDescription *);
extern bool_t xdr_PlotQuantityDescriptionList(XDR *, PlotQuantityDescriptionList *);

extern bool_t xdr_DataPlotQuantityDescription(XDR *, DataPlotQuantityDescription *);

#else

EXTERN_FUNCTION(bool_t xdr_VoidList_struct,(XDR *, VoidList_struct *));
EXTERN_FUNCTION(bool_t xdr_VoidList,(XDR *, VoidList *));
EXTERN_FUNCTION(bool_t xdr_IntList_struct,(XDR *, IntList_struct *));
EXTERN_FUNCTION(bool_t xdr_IntList,(XDR *, IntList *));
EXTERN_FUNCTION(bool_t xdr_Int16List_struct,(XDR *, Int16List_struct *));
EXTERN_FUNCTION(bool_t xdr_Int16List,(XDR *, Int16List *));
EXTERN_FUNCTION(bool_t xdr_Int32List_struct,(XDR *, Int32List_struct *));
EXTERN_FUNCTION(bool_t xdr_Int32List,(XDR *, Int32List *));
EXTERN_FUNCTION(bool_t xdr_AddrTList_struct,(XDR *, AddrTList_struct *));
EXTERN_FUNCTION(bool_t xdr_AddrTList,(XDR *, AddrTList *));
EXTERN_FUNCTION(bool_t xdr_FloatList_struct,(XDR *, FloatList_struct *));
EXTERN_FUNCTION(bool_t xdr_FloatList,(XDR *, FloatList *));
EXTERN_FUNCTION(bool_t xdr_DoubleList_struct,(XDR *, DoubleList_struct *));
EXTERN_FUNCTION(bool_t xdr_DoubleList,(XDR *, DoubleList *));
EXTERN_FUNCTION(bool_t xdr_String_struct,(XDR *, String_struct *));
EXTERN_FUNCTION(bool_t xdr_String,(XDR *, String *));
EXTERN_FUNCTION(bool_t xdr_StringList_struct,(XDR *, StringList_struct *));
EXTERN_FUNCTION(bool_t xdr_StringList,(XDR *, StringList *));
EXTERN_FUNCTION(bool_t xdr_SpaceCraftDescription_struct,(XDR *, SpaceCraftDescription_struct *));
EXTERN_FUNCTION(bool_t xdr_SpaceCraftDescription,(XDR *, SpaceCraftDescription *));
EXTERN_FUNCTION(bool_t xdr_TimeSpanDescription_struct,(XDR *, TimeSpanDescription_struct *));
EXTERN_FUNCTION(bool_t xdr_TimeSpanDescription,(XDR *, TimeSpanDescription *));
EXTERN_FUNCTION(bool_t xdr_DataQuantityIdentifier_struct,(XDR *, DataQuantityIdentifier_struct *));
EXTERN_FUNCTION(bool_t xdr_DataQuantityIdentifier,(XDR *, DataQuantityIdentifier *));
EXTERN_FUNCTION(bool_t xdr_DataQuantityIdentifierList_struct,(XDR *, DataQuantityIdentifierList_struct *));
EXTERN_FUNCTION(bool_t xdr_DataQuantityIdentifierList,(XDR *, DataQuantityIdentifierList *));

EXTERN_FUNCTION(bool_t xdr_ComponentSubDimension_struct,(XDR *, ComponentSubDimension_struct *));
EXTERN_FUNCTION(bool_t xdr_ComponentSubDimension,(XDR *, ComponentSubDimension *));
EXTERN_FUNCTION(bool_t xdr_ComponentSubDimensionList_struct,(XDR *, ComponentSubDimensionList_struct *));
EXTERN_FUNCTION(bool_t xdr_ComponentSubDimensionList,(XDR *, ComponentSubDimensionList *));

EXTERN_FUNCTION(bool_t xdr_ComponentDescription_struct,(XDR *, ComponentDescription_struct *));
EXTERN_FUNCTION(bool_t xdr_ComponentDescription,(XDR *, ComponentDescription *));
EXTERN_FUNCTION(bool_t xdr_ComponentDescriptionList_struct,(XDR *, ComponentDescriptionList_struct *));
EXTERN_FUNCTION(bool_t xdr_ComponentDescriptionList,(XDR *, ComponentDescriptionList *));

EXTERN_FUNCTION(bool_t xdr_DataQuantityDescription_struct,(XDR *, DataQuantityDescription_struct *));
EXTERN_FUNCTION(bool_t xdr_DataQuantityDescription,(XDR *, DataQuantityDescription *));
EXTERN_FUNCTION(bool_t xdr_DataQuantityDescriptionList_strc,(XDR *, DataQuantityDescriptionList_strc *));
EXTERN_FUNCTION(bool_t xdr_DataQuantityDescriptionList,(XDR *, DataQuantityDescriptionList *));
EXTERN_FUNCTION(bool_t xdr_ComponentUse_struct,(XDR *, ComponentUse_struct *));
EXTERN_FUNCTION(bool_t xdr_ComponentUse,(XDR *, ComponentUse *));
EXTERN_FUNCTION(bool_t xdr_ComponentUseList_struct,(XDR *, ComponentUseList_struct *));
EXTERN_FUNCTION(bool_t xdr_ComponentUseList,(XDR *, ComponentUseList *));

EXTERN_FUNCTION(bool_t xdr_ComponentAccumulation_struct,(XDR *, xdr_ComponentAccumulation_struct *));
EXTERN_FUNCTION(bool_t xdr_ComponentAccumulation,(XDR *, xdr_ComponentAccumulation *));
EXTERN_FUNCTION(bool_t xdr_ComponentAccumulationList_struct,(XDR *, xdr_ComponentAccumulationList_struct *));
EXTERN_FUNCTION(bool_t xdr_ComponentAccumulationList,(XDR *, xdr_ComponentAccumulationList *));

EXTERN_FUNCTION(bool_t xdr_DrawingUnit_struct,(XDR *, xdr_DrawingUnit_struct *));
EXTERN_FUNCTION(bool_t xdr_DrawingUnit,(XDR *, xdr_DrawingUnit *));
EXTERN_FUNCTION(bool_t xdr_DrawingUnitList_struct,(XDR *, xdr_DrawingUnitList_struct *));
EXTERN_FUNCTION(bool_t xdr_DrawingUnitList,(XDR *, xdr_DrawingUnitList *));

EXTERN_FUNCTION(bool_t xdr_AxisUnit_struct,(XDR *, xdr_AxisUnit_struct *));
EXTERN_FUNCTION(bool_t xdr_AxisUnit,(XDR *, xdr_AxisUnit *));
EXTERN_FUNCTION(bool_t xdr_AxisUnitList_struct,(XDR *, xdr_AxisUnitList_struct *));
EXTERN_FUNCTION(bool_t xdr_AxisUnitList,(XDR *, xdr_AxisUnitList *));

EXTERN_FUNCTION(bool_t xdr_ColorLine_struct,(XDR *, xdr_ColorLine_struct *));
EXTERN_FUNCTION(bool_t xdr_ColorLine,(XDR *, xdr_ColorLine *));

EXTERN_FUNCTION(bool_t xdr_PlotQuantityDescription_struct,(XDR *, PlotQuantityDescription_struct *));
EXTERN_FUNCTION(bool_t xdr_PlotQuantityDescription,(XDR *, PlotQuantityDescription *));
EXTERN_FUNCTION(bool_t xdr_PlotQuantityDescriptionList_struct,(XDR *, PlotQuantityDescriptionList_struct *));
EXTERN_FUNCTION(bool_t xdr_PlotQuantityDescriptionList,(XDR *, PlotQuantityDescriptionList *));

EXTERN_FUNCTION(bool_t xdr_DataPlotQuantityDescription_struct,(XDR *, DataPlotQuantityDescription_struct *));

#endif

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* SDTDESCRIPTIONXDR_H */

