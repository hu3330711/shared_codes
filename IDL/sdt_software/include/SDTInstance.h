/* SDTInstance.h */
/* This file contains declarations for SDT Instances: */

#ifndef SDTINSTANCE_H
#define SDTINSTANCE_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* SCCS ID string: */
#define SccsId_SDTInstance_h "@(#)SDTInstance.h	1.21, 12/02/06"

/* SSL Data Tools Instances File: */

#include <SDTType.h>
#include <SDTDescription.h>
#include <SDTSpacecraft.h>
#include <SDTMemory.h>
#include <SDTLinkedList.h>

/* -------------------------------------------------------------------- */
/* Constants: */

/* This is the size of the header section of the allocated memory for
 * a DataQuantityInstance, in bytes:
 */
#define  SizeOfDQIHeader        512

/* This is the size of the header section, in bytes, for the header
 * section of an Array Description component for a multi-dimensional
 * DataQuantityInstance.  Note that an "ArrDescHdr" is set up in this
 * header section of the Array Description component.
 */
#define  SizeOfArrDescHeader    512

/* If an Array Description exists for a multi-dimensional DQI, it
 * will always be this component of the DQI:
 */
#define  ArrDescCompIdx         2

/* The tolerance fuzz for checking array description bin limits
 * for sameness.  It is important to have some fuzz since there
 * are routines that check doubles vrs. floats:
 */
#define  ARR_DESC_TOL  .05

#define  DQH_ALTERNATE_LABEL_MAX_CHARS 48
#define  DQH_ALTERNATE_UNITS_MAX_CHARS 48

/* -------------------------------------------------------------------- */
/* This is the DQIHeader for a DataQuantityInstance. */

struct DQIHeader_struct
   {
   int   TotalPoints ;        /* Total number of points for this qty. */
   int   CurrentPoints ;      /* Currently used number of points. */
   int   In ;                 /* If "Circular" is 0, this has no meaning.
			       * If "Circular" is 1, then this is the next
			       * index to write a point into.
			       */
   int   Out ;                /* If "Circular" is 0, this has no meaning.
			       * If "Circular" is 1, then this is the next
			       * index to read a point from.
			       */
   int32 Done ;               /* 0 -> still in decommutation
	                       * 1 -> decommutation is finished
			       */
   int Shading ;              /* This flag indicates if this plot is
			       * to be capable of having "shaded"
			       * horizontal regions to distinguish
			       * different regions of the shown time span.
			       */
   int Circular ;             /* This flag indicates if this memory is
			       * being used as a circular buffer:
			       *   0:  Not used as a circular buffer
			       *   1:  Used as a circular buffer
			       */
   int32 JulianDay ;          /* This is used ONLY by Realtime Sources. It
			       * allows the Source to tell the UI what the
			       * day is, AT THE START of the data.  Look
			       * at the Fast Decommutator to see how it
			       * is used.  The UI, in realtime, can then
			       * use this to know:
			       *   1. That data is now available.
			       *   2. What the date of the data is
			       *      (the UI has no apriori knowledge
			       *       of the date of realtime data).
			       */
   int Error ;                /* This flag indicates if there is an error:
			       *   0:  No error has occurred in creating
			       *       the data
			       *   1:  An error has occurred in creating
			       *       the data
			       */
   int ArrDescExists ;        /* This flag is relates only to multi-
			       * dimensional data:
			       *   0:  This multi-dimensional data
			       *       has NO Array Description component.
			       *   1:  This multi-dimensional data
			       *       HAS an Array Description component.
			       */
   int AlternateLabelFlg ;    /* This flag indicates if the alternate
			       * plot label in the field "AlternateLabel"
			       * is to be used.  This can be controlled
			       * either by SDT or the source of the data.
			       */
   int AlternateUnitsFlg ;    /* This flag indicates if the alternate
			       * plot units in the field "AlternateUnits"
			       * is to be used.  This can be controlled
			       * either by SDT or the source of the data.
			       */
   char AlternateLabel[DQH_ALTERNATE_LABEL_MAX_CHARS] ;
			      /* String containing the alternate label -
			       * it is used by plots on SDT if the flag:
			       * "AlternateLabelFlg" is ON.
			       */
   char AlternateUnits[DQH_ALTERNATE_UNITS_MAX_CHARS] ;
			      /* String containing the alternate label -
			       * it is used by plots on SDT if the flag:
			       * "AlternateLabelFlg" is ON.
			       */

   double  ZoomStartTime ;    /* Current SDT Zoom information: */
   double  ZoomEndTime ;
   int32   ZoomStartJDay ;
   int32   ZoomEndJDay ;

   double  LeftFiducialTime ;  /* Current SDT L,R fiducial times */
   double  RightFiducialTime ;
   } ;
typedef  struct DQIHeader_struct  DQIHeader ;

/* -------------------------------------------------------------------- */
/* This is the header for an Array Description component for a multi-
 * dimensional DQI.  Not that this is setup at the start of the first
 * "SizeOfArrDescHeader" bytes of the Array Description component:
 */

struct DQIArrDescHdr_struct
   {
   int    NumberDesc ;        /* The current number of descriptions
			       * ("MDimArrDesc"'s) for the DQI.
			       */
   AddrT  FirstByte ;         /* Points to the byte offset of the FIRST
			       * "MDimArrDesc" in this Array Description
			       * List.
			       */
   AddrT  NextByte   ;        /* Points to the next available byte,
			       * within the Array Description component,
			       * for a new "MDimArrDesc".
			       */
   AddrT  RemainingBytes   ;  /* Contains the number of bytes remaining
			       * in the Array Description component.
			       */
   } ;
typedef  struct DQIArrDescHdr_struct  DQIArrDescHdr ;

/* -------------------------------------------------------------------- */
/* Structures: */

struct DataQuantityInstance_struct
   {
   DataQuantityDescription DQD ;    /* The DataQuantityDescription for this
				     * instance.
				     */
   TimeSpanDescription TimeSpan ;   /* The TimeSpan Description for this
				     * instance.
				     */
   MemoryUnit          MUnit ;      /* The MemoryUnit (Shared-Memory segment
				     * or Memory-Mapped file) in which the
				     * data buffer for this DQI resides.
				     */
   MemorySegment       MSegment ;   /* The MemorySegment which describes
				     * where in "MUnit", the data buffer
				     * for this DQI resides.
				     */
   DQIHeader           *MHeader ;   /* Points to the "DQIHeader" for this
				     * DQI.
				     */
   AddrTList    ComponentOffsets ;  /* This list contains the byte offsets
				     * from the start of the memory segment
				     * for each of the components of the DQI.
				     * NOTE that these offsets are relative
				     * to the beginning of the data section
				     * of the MemorySegment (they do not
				     * include the DQIHeader).
				     */

   IntList      ComponentElts ;     /* This list contains the number of
				     * elements of the data type for
				     * the corresponding component in
				     * its section of the memory unit.
				     * This is useful for multi-dimen.
				     * arrays which vary in array size
				     * in a particular data quantity.
				     */

   int          QltFlagUse ;        /* If "1", then there are Quality
				     * bits for at least one component
				     * of the DQI.  If "0", then there
				     * are NO Quality bits for this DQI.
				     */
   IntList      ComponentQltFlags ; /* This list contains a list of flags,
				     * one for each component of the DQI,
				     * as follows:
				     *   flag[cmp] = 0:
				     *       NO Quality bits are stored
				     *       for this component
				     *   flag[cmp] = 1:
				     *       Quality bits are stored
				     *       for this component
				     */
   VoidList            Data ;       /* This list contains pointers to the
				     * buffer locations of all of the
				     * data components (which are described
				     * in the DQD sub-field:  "CDList") for
				     * this DQI.
				     */
   char                *QltBits ;   /* If any Quality bits are in use
				     * by this DQI, this points to the
				     * starting location in memory.
				     * Otherwise, it's NULL.
				     */
   } ;
typedef struct DataQuantityInstance_struct DataQuantityInstance ;

struct DataQuantityInstanceList_struct {
	int nqtys;
	struct {
		u_int qty_len;
		DataQuantityInstance *qty_val;
	} qty;
};
typedef  struct DataQuantityInstanceList_struct  DataQuantityInstanceList ;

/* Note that no-one but the UI needs to know about the PlotQuantityInstance. */
/* So we can and should get rid of it here. */
#ifdef PLOT_QUANTITY_INSTANCE_ELSEWHERE
struct PlotQuantityInstance_struct
   {
   PlotQuantityDescription PQD ;
   DataQuantityInstanceList DQIList ;
   PlotPanelAttributes PPA ;            /* Attributes needed to plot position
					 * this plot on the screen
			                 */
   } ;
typedef  struct PlotQuantityInstance_struct PlotQuantityInstance ;
#endif /* PLOT_QUANTITY_INSTANCE_ELSEWHERE */

int  SDTCompareDataQuantityInstance( DataQuantityInstance *dqi1, 
					    DataQuantityInstance *dqi2) ;

extern DataQuantityInstance *SDTConstructDataQuantityInstance (
    DataQuantityDescription *in_dqd, TimeSpanDescription *in_tspan,
    MemoryUnit *in_munit, MemorySegment *in_mseg, void *in_memhdr,
    AddrTList *comp_offs, VoidList *in_data,
    int in_qflag, IntList *comp_qlt, char *qbits) ;

extern DataQuantityInstance  *SDTCopyConstructDataQuantityInstance (
    DataQuantityInstance *in_dqi) ;

extern int  SDTFillDataQuantityInstance (DataQuantityInstance *in_dqi,
    DataQuantityDescription *in_dqd, TimeSpanDescription *in_tspan,
    MemoryUnit *in_munit, MemorySegment *in_mseg, void *in_memhdr,
    AddrTList *comp_offs, VoidList *in_data,
    int in_qflag, IntList *comp_qlt, char *qbits) ;

extern int  SDTCopyDataQuantityInstance (DataQuantityInstance *in_dqi,
    DataQuantityInstance *out_dqi) ;

extern int  SDTDestructDataQuantityInstance (
    DataQuantityInstance *in_dqi) ;

extern int  SDTClearDataQuantityInstance (
    DataQuantityInstance *in_dqi) ;

extern int  SDTPrintDataQuantityInstance (
    DataQuantityInstance *dqi, FILE *ofp) ;

extern int  SDTAssignDQIMemoryPointers (DataQuantityInstance *dqi) ;

extern DataQuantityInstanceList
    *SDTConstructDataQuantityInstanceList (
    int nqtys, int ntspans, TimeSpanDescription *in_tspan,
    DataQuantityDescription *in_dqd, int nmunits, MemoryUnit *in_munit,
    MemorySegment *in_mseg, void **in_memhdr, AddrTList *in_coffs,
    VoidList *in_data, int *in_qflag, IntList *comp_qlt, char **qbits) ;

extern DataQuantityInstanceList
    *SDTCopyConstructDataQuantityInstanceList (
    DataQuantityInstanceList *in_dqil) ;

extern int  SDTFillDataQuantityInstanceList (
    DataQuantityInstanceList *dqil,
    int nqtys, int ntspans, TimeSpanDescription *in_tspan,
    DataQuantityDescription *in_dqd, int nmunits, MemoryUnit *in_munit,
    MemorySegment *in_mseg, void **in_memhdr, AddrTList *in_coffs,
    VoidList *in_data, int *in_qflag, IntList *comp_qlt, char **qbits) ;

extern int  SDTCopyDataQuantityInstanceList (
    DataQuantityInstanceList *in_dqil,
    DataQuantityInstanceList *out_dqil) ;

extern int  SDTDestructDataQuantityInstanceList (
    DataQuantityInstanceList *dqil) ;

extern int  SDTDestructDataQuantityInstanceLinkedList (
    SDTListClass *dlist) ;

extern int  SDTClearDataQuantityInstanceList (
    DataQuantityInstanceList *dqil) ;

extern int  SDTArrayFillDataQuantityInstanceList (int ndqi,
    DataQuantityInstance *in_dqi, DataQuantityInstanceList *out_dqil) ;

extern int  SDTPtrArrayFillDataQuantityInstanceList (int ndqi,
    DataQuantityInstance **in_dqi, DataQuantityInstanceList *out_dqil) ;

extern int  SDTPrintDataQuantityInstanceList (
    DataQuantityInstanceList *dqil, FILE *ofp) ;

extern int  SDTCompareDataQuantityDescriptions (
    DataQuantityDescription *dq1, DataQuantityDescription *dq2) ;

extern int  SDTBuildSelectedDQIList (
    DataQuantityDescriptionList *dqdl,
    DataQuantityInstanceList *in_dqil,
    DataQuantityInstanceList *out_dqil)  ;

extern AddrT  SDTComputeQualityStorage (
    DataQuantityDescription *in_dqd, int npts, IntList *comp_qlt) ;

extern int  SDTInitializeQualityStorage (DataQuantityInstance *in_dqi) ;

extern int  SDTReadQualityBit (DataQuantityInstance *dqi, int rec,
    int comp) ;

extern int  SDTWriteQualityBit (DataQuantityInstance *dqi, int rec,
    int comp, int val) ;

extern IntList *SDTReturnQualityRanges (DataQuantityInstance *dqi,
    int comp, int istate, int fidx, int lidx, int *ecode) ;

extern IntList *SDTExtractDQDQltList (DataQuantityDescription *dqd,
    IntList *in_list, int dqd_idx) ;

extern MDimVal *SDTGetMDimVal (MDimArrDesc *mptr, int SubDim) ;

extern int SDTGetAllMDimVals (MDimArrDesc *mptr, MDimVal **mdv) ;

extern int  SDTGetMDimArrDesc (DataQuantityInstance *dqi,
    AddrT *Idx, MDimArrDesc **mvals) ;

extern int SDTSetOneDimArrDescDouble (DataQuantityInstance *dqi, int siz1,
   int ndims, double **min1arr, double **max1arr, int *QFarr,
   double *Wrap, AddrT *rval) ;

extern int SDTSetTwoDimArrDescDouble (DataQuantityInstance *dqi,
   int siz1, int ndim1, double **min1arr, double **max1arr,
   int siz2, int ndim2, double **min2arr, double **max2arr,
   int *QF1, int *QF2, double *Wrap1, double *Wrap2,
   AddrT *rval1, AddrT *rval2) ;

extern int SDTSetThreeDimArrDescDouble (DataQuantityInstance *dqi,
   int siz1, int ndim1, double **min1arr, double **max1arr,
   int siz2, int ndim2, double **min2arr, double **max2arr,
   int siz3, int ndim3, double **min3arr, double **max3arr,
   int *QF1, int *QF2, int *QF3, double *Wrap1, double *Wrap2,
   double *Wrap3, AddrT *rval1, AddrT *rval2, AddrT *rval3) ;

extern int SDTSetOneDimArrDescFloat (DataQuantityInstance *dqi, int siz1,
   int ndims, float **min1arr, float **max1arr, int *QFarr,
   double *Wrap, AddrT *rval) ;

extern int SDTSetTwoDimArrDescFloat (DataQuantityInstance *dqi,
   int siz1, int ndim1, float **min1arr, float **max1arr,
   int siz2, int ndim2, float **min2arr, float **max2arr,
   int *QF1, int *QF2, double *Wrap1, double *Wrap2,
   AddrT *rval1, AddrT *rval2) ;

extern int SDTSetThreeDimArrDescFloat (DataQuantityInstance *dqi,
   int siz1, int ndim1, float **min1arr, float **max1arr,
   int siz2, int ndim2, float **min2arr, float **max2arr,
   int siz3, int ndim3, float **min3arr, float **max3arr,
   int *QF1, int *QF2, int *QF3, double *Wrap1, double *Wrap2,
   double *Wrap3, AddrT *rval1, AddrT *rval2, AddrT *rval3) ;

extern int SDTPrintArrayDescriptionList (DataQuantityInstance *dqi,
   FILE *ofp) ;

extern int SDTGetMDimIndexRange (MDimArrDesc *mptr, int msize,
    double *lvalp, double *hvalp, int imode, int asiz,
    int *ntervs, int *istart, int *iend) ;

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* SDTINSTANCE_H */

