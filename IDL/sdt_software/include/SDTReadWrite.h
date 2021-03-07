/* SDTReadWrite.h */

#ifndef SDTREADWRITE_H
#define SSDTREADWRITE_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* SCCS ID string: */
#define SccsId_SDTReadWrite_h "@(#)SDTReadWrite.h	1.3, 09/18/06"

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>

/* Local includes: */

#include <SDTDescription.h>
#include <SDTSpacecraft.h>

/* ------------------------------------------------------------- */

int  SDTWriteCharString (char *cptr, FILE *of) ;
char  *SDTReadCharString (FILE *of) ;
int  SDTWriteString (String *sptr, FILE *of) ;
int  SDTReadString (String *sptr, FILE *of) ;
int  SDTWriteStringList (StringList *slist, FILE *of) ;
int  SDTReadStringList (StringList *slist, FILE *of) ;

int  SDTWriteSpaceCraftDescription (SpaceCraftDescription *scraft,
    FILE *of) ;
int  SDTReadSpaceCraftDescription (SpaceCraftDescription *scraft,
    FILE *of) ;

int  SDTWriteDataQuantityIdentifier (DataQuantityIdentifier *dqid,
    FILE *of) ;
int  SDTReadDataQuantityIdentifier (DataQuantityIdentifier *dqid,
    FILE *of) ;
int  SDTWriteDataQuantityIdentifierList (
    DataQuantityIdentifierList *dqidl, FILE *of) ;
int  SDTReadDataQuantityIdentifierList (
    DataQuantityIdentifierList *dqidl, FILE *of) ;

int  SDTWriteComponentSubDimension (ComponentSubDimension *csd,
    FILE *of) ;
int  SDTReadComponentSubDimension (ComponentSubDimension *csd,
    FILE *of) ;
int  SDTWriteComponentSubDimensionList (
    ComponentSubDimensionList *csdl, FILE *of) ;
int  SDTReadComponentSubDimensionList (
    ComponentSubDimensionList *csdl, FILE *of) ;

int  SDTWriteComponentDescription (ComponentDescription *cdesc,
    FILE *of) ;
int  SDTReadComponentDescription (ComponentDescription *cdesc,
    FILE *of) ;
int  SDTWriteComponentDescriptionList (
    ComponentDescriptionList *cmdl, FILE *of) ;
int  SDTReadComponentDescriptionList (
    ComponentDescriptionList *cmdl, FILE *of) ;

int  SDTWriteDataQuantityDescription (DataQuantityDescription *dqd,
    FILE *of) ;
int  SDTReadDataQuantityDescription (DataQuantityDescription *dqd,
    FILE *of) ;
int  SDTWriteDataQuantityDescriptionList (
    DataQuantityDescriptionList *dqdl, FILE *of) ;
int  SDTReadDataQuantityDescriptionList (
    DataQuantityDescriptionList *dqdl, FILE *of) ;

int  SDTWriteComponentUse (ComponentUse *cu, FILE *of) ;
int  SDTReadComponentUse (ComponentUse *cu, FILE *of) ;
int  SDTWriteComponentUseList (ComponentUseList *cul, FILE *of) ;
int  SDTReadComponentUseList (ComponentUseList *cul, FILE *of) ;

int  SDTWriteComponentAccumulation (ComponentAccumulation *ca,
    FILE *of) ;
int  SDTReadComponentAccumulation (ComponentAccumulation *ca,
    FILE *of) ;
int  SDTWriteComponentAccumulationList (
    ComponentAccumulationList *cal, FILE *of) ;
int  SDTReadComponentAccumulationList (
    ComponentAccumulationList *cal, FILE *of) ;

int  SDTWriteDrawingUnit (DrawingUnit *drwp, FILE *of) ;
int  SDTReadDrawingUnit (DrawingUnit *drwp, FILE *of) ;
int  SDTWriteDrawingUnitList (DrawingUnitList *drwl, FILE *of) ;
int  SDTReadDrawingUnitList (DrawingUnitList *drwl, FILE *of) ;

int  SDTWriteAxisUnit (AxisUnit *au, FILE *of) ;
int  SDTReadAxisUnit (AxisUnit *au, FILE *of) ;
int  SDTWriteAxisUnitList (AxisUnitList *aul, FILE *of) ;
int  SDTReadAxisUnitList (AxisUnitList *aul, FILE *of) ;

int  SDTWriteColorLine (ColorLine *cln, FILE *of) ;
int  SDTReadColorLine (ColorLine *cln, FILE *of) ;

int  SDTWritePlotQuantityDescription (PlotQuantityDescription *pqd,
    FILE *of) ;
int  SDTReadPlotQuantityDescription (PlotQuantityDescription *pqd,
    FILE *of) ;
int  SDTWritePlotQuantityDescriptionList (
    PlotQuantityDescriptionList *pqdl, FILE *of) ;
int  SDTReadPlotQuantityDescriptionList (
    PlotQuantityDescriptionList *pqdl, FILE *of) ;


#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* SDTREADWRITE_H */
