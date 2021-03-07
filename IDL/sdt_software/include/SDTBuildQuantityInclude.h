/* SDTBuildQuantityInclude.h */

#ifndef SDTBUILDQUANTITYINCLUDE_H
#define SDTBUILDQUANTITYINCLUDE_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* SCCS ID string: */
#define SccsId_SDTBuildQuantityInclude_h "@(#)SDTBuildQuantityInclude.h	1.7, 12/02/06"

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#include <SDTDescription.h>
#include <SDTSpacecraft.h>
#include <SDTLinkedList.h>

/* -------------------------------------------------------------------- */
/* Constants: */

/* -------------------------------------------------------------------- */
/* TypeDefs: */

/* The main purpose of this structure is to insure that subsidiary
 * structures and/or arrays of all types use unique integers to
 * assure unique structure and/or array names:
 */
struct SDTQtySessionId_struct
    {
    int   QSessId ;
    int   DQDIdx ;
    int   DQDListIdx ;
    int   CDIdx ;
    int   CDListIdx ;
    int   SubDimIdx ;
    int   SubDimListIdx ;
    int   AncIdx ;
    int   AncListIdx ;
    int   StringIdx ;
    int   StringListIdx ;
    int   IntIdx ;
    int   IntListIdx ;
    int   AddrTIdx ;
    int   AddrTListIdx ;

    int   CUIdx ;
    int   CUListIdx ;
    int   CAIdx ;
    int   CAListIdx ;
    int   DUIdx ;
    int   DUListIdx ;
    int   AxIdx ;
    int   AxListIdx ;
    int   CLIdx ;
    } ;
typedef  struct SDTQtySessionId_struct  SDTQtySessionId ;


/* -------------------------------------------------------------------- */
/* Variables: */

/* -------------------------------------------------------------------- */
/* Functions: */

int SDTInitSDTQtySessionId (int SessId, SDTQtySessionId *qsess) ; 

String *SDTOutputDataQuantityDescriptionList (FILE *ofp,
   char *DqdArrayNamePtr, SDTQtySessionId *qsess,
   DataQuantityDescriptionList *ilist) ;

String *SDTOutputDataQuantityDescription (FILE *ofp,
   DataQuantityDescription *dqd, SDTQtySessionId *qsess, int idx) ;

String *SDTOutputComponentDescriptionList (FILE *ofp,
   SDTQtySessionId *qsess, ComponentDescriptionList *ilist) ;

String *SDTOutputComponentDescription (FILE *ofp,
   ComponentDescription *entry, SDTQtySessionId *qsess, int idx) ;

String *SDTOutputComponentSubDimensionList (FILE *ofp,
   SDTQtySessionId *qsess, ComponentSubDimensionList *ilist) ;

String *SDTOutputComponentSubDimension (FILE *ofp,
   ComponentSubDimension *entry, SDTQtySessionId *qsess, int idx) ;

String *SDTOutputDataQuantityIdentifierList (FILE *ofp,
   SDTQtySessionId *qsess, DataQuantityIdentifierList *ilist) ;

String *SDTOutputDataQuantityIdentifier (FILE *ofp,
   DataQuantityIdentifier *entry, SDTQtySessionId *qsess, int idx) ;

String *SDTOutputStringList (FILE *ofp, SDTQtySessionId *qsess,
   StringList *ilist) ;

String *SDTOutputString (FILE *ofp,
    String *entry, SDTQtySessionId *qsess, int idx) ;

String *SDTOutputIntList (FILE *ofp, SDTQtySessionId *qsess,
   IntList *ilist) ;

String *SDTOutputAddrTList (FILE *ofp, SDTQtySessionId *qsess,
   AddrTList *ad_list) ;

String *SDTOutputParseDQDExtraInfo (FILE *ofp, ParseDQDExtraInfo *entry,
   SDTQtySessionId *qsess, int idx) ;


/* ----------------------------------------------------------------- */
String *SDTOutputComponentUseList (FILE *ofp, SDTQtySessionId *qsess,
   ComponentUseList *ilist) ;

String *SDTOutputComponentUse (FILE *ofp, ComponentUse *entry,
   SDTQtySessionId *qsess, int idx) ;

String *SDTOutputComponentAccumulationList (FILE *ofp,
    SDTQtySessionId *qsess, ComponentAccumulationList *ilist) ;

String *SDTOutputComponentAccumulation (FILE *ofp,
    ComponentAccumulation *entry, SDTQtySessionId *qsess, int idx) ;

String *SDTOutputDrawingUnitList (FILE *ofp,
    SDTQtySessionId *qsess, DrawingUnitList *ilist) ;

String *SDTOutputDrawingUnit (FILE *ofp,
    DrawingUnit *entry, SDTQtySessionId *qsess, int idx) ;

String *SDTOutputAxisUnitList (FILE *ofp,
    SDTQtySessionId *qsess, AxisUnitList *ilist) ;

String *SDTOutputAxisUnit (FILE *ofp,
    AxisUnit *entry, SDTQtySessionId *qsess, int idx) ;

String *SDTOutputColorLine (FILE *ofp,
    ColorLine *entry, SDTQtySessionId *qsess, int idx) ;

String *SDTOutputPlotQuantityDescriptionList (FILE *ofp,
    char *PqdArrayNamePtr, SDTQtySessionId *qsess,
    PlotQuantityDescriptionList *ilist) ;

String *SDTOutputPlotQuantityDescription (FILE *ofp,
    PlotQuantityDescription *entry, SDTQtySessionId *qsess, int idx) ;

String *SDTOutputParsePQDExtraInfo (FILE *ofp, ParsePQDExtraInfo *entry,
   SDTQtySessionId *qsess, int idx) ;

int SDTVacuousQtyCompare (void *a, void *b) ;

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

/* ----------------------------------------------------------------- */
#endif /* SDTBUILDQUANTITYINCLUDE_H */
