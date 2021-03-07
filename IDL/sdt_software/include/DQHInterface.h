/* DQHInterface.h */

/* This file contains declarations for asking the DQH for memory and DQI
 * assignments.
 */

#ifndef DQHINTERFACE_H
#define DQHINTERFACE_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include <rpc/rpc.h>
#include <SDTDescription.h>
#include <SDTMemory.h>
#include <SDTInstance.h>
#include <SDTInstanceXDR.h>
#include <SDTLinkedList.h>

/* SCCS ID string: */
#define SccsId_DQHInterface_h "@(#)DQHInterface.h	1.18, 12/02/06"

/* ------------------------------------------------------------------------ */
/* Constants: */

#define  DQICreateError   -1
#define  DQIExists         0
#define  DQICreated        1

/* The RPC Procedure ID for querying the DQH for existing DQIS: */
#define  QUERY_DATA_QUANTITY_INSTANCES_PROC   121

/* The RPC Procedure ID for querying the DQH for ALL existing DQIS: */
#define  QUERY_ALL_DATA_QUANTITY_INSTANCES_PROC   122

/* The RPC Procedure ID for commanding the DQH to change the names,
 * and create a new empty DQI for, a list of DQIs.
 */
#define  REPLACE_DATA_QUANTITY_INSTANCES_PROC     123

/* The maximum number of DQI's one may query with the
 * "QUERY_DATA_QUANTITY_INSTANCES_PROC" RPC function.
 */
#define  MAX_DQHQUERY_DQIS  20000

/* ----------------------------------------------------------------------- */
/* This structure is for later use by FAST to set pre-allocated, pre-assigned
 * memory information to the DQH.  Note that is undefined as of March 10, 1993.
 */
struct PreAllocatedMemoryInfo_struct {
	int      dummy ;        /* Unused. */
};
typedef struct  PreAllocatedMemoryInfo_struct  PreAllocatedMemoryInfo ;

extern bool_t xdr_PreAllocatedMemoryInfo_struct(XDR *, struct PreAllocatedMemoryInfo_struct *);
extern bool_t xdr_PreAllocatedMemoryInfo(XDR *, PreAllocatedMemoryInfo *);


/* -------------------------------------------------------------------- */
/* Structure to handle number of elements requested in a DQH request: */
struct NPointsReq_struct 
    {
    /* The number of points in the corresponding component: */
    int   npts ;

    /* Probably only for use by Multi-dimensional arrays:  the
     * average number of elements in each mdim array:
     */
    int   narrpts ;
    } ;
typedef struct NPointsReq_struct  NPointsReq ;

extern bool_t xdr_NPointsReq_struct(XDR *xdrs,
    struct NPointsReq_struct *objp) ;
extern bool_t xdr_NPointsReq(XDR *xdrs, NPointsReq *objp) ;

struct NPointsReqList_struct
    {
    int  nqtys ;
    struct {
	u_int qty_len;
	NPointsReq *qty_val;
	} qty ;
    } ;
typedef struct NPointsReqList_struct  NPointsReqList ;

extern bool_t xdr_NPointsReqList_struct(XDR *xdrs,
    struct NPointsReqList_struct *objp) ;
extern bool_t xdr_NPointsReqList(XDR *xdrs, NPointsReqList *objp) ;

struct NPointsReqListList_struct
    {
    int  nqtys ;
    struct {
	u_int qty_len;
	NPointsReqList *qty_val;
	} qty ;
    } ;
typedef struct NPointsReqListList_struct  NPointsReqListList ;

extern bool_t xdr_NPointsReqListList_struct(XDR *xdrs,
    struct NPointsReqListList_struct *objp) ;
extern bool_t xdr_NPointsReqListList(XDR *xdrs,
    NPointsReqListList *objp) ;


/* ----------------------------------------------------------------------- */
/* This structure is sent, by a source, to the DQH, so that it (the DQH) can
 * determine where to allocate memory for each requested DQD for the requested
 * time span.  The choice between shared and mapped memory will be made via
 * the "MemoryType" flag.
 *
 * Note the the flag:
 *
 *     memory_pre_exists
 *
 * is unused as of 3/10/93 and exists for FAST:  it would allow the client
 * (who normally doesn't care where the DQH assigns data arrays) to actually
 * tell the DQH where the memory (either SHM or MMAP) actually is and how
 * that data is to be assigned.  This is important in FAST because there
 * will be a TCP program receiving FAST virtual channel packets and stuffing
 * them into mmap-ed memory OF ITS OWN making!  Therefore, the DQH will not,
 * in this case, actually allocate the memory, but simply be told where the
 * memory and data assignments already are, so that it (the DQH) can pass
 * on that information to other programs (e.g. the User Interface).
 *
 * Note that, when FAST requires this capability, more information will have
 * to be passed inside the sub-structure "mlist" to tell the DQH where the
 * assignments actually are (perhaps a list of MemoryUnit-MemorySegment's
 * will be needed).
 */
struct DQHRequestControl_struct
    {
    int        MemoryType ;           /* Integer which determines the type
				       * of memory to use.  The following
				       * are the possible values:
				       *     SharedMemoryType
				       *     MappedMemoryType
				       */
    int        memory_pre_exists ;    /* This flag indicates the
				       * following:
				       *    0 -> the DQH must allocate and
				       *         assign the memory.
				       *    1 -> the memory and assignment
				       *         scheme is already
				       *         determined.
				       */
    PreAllocatedMemoryInfo  mlist ;   /* This will be defined later
				       * for use by FAST.
				       */
    } ;
typedef struct DQHRequestControl_struct DQHRequestControl ;

extern bool_t xdr_DQHRequestControl_struct(XDR *, struct DQHRequestControl_struct *);
extern bool_t xdr_DQHRequestControl(XDR *, DQHRequestControl *);


/* ----------------------------------------------------------------------- */
/* This is the basic request, directly to the DQH, for a
 * DataQuantityInstance and data assignment to memory:
 */
struct DQHRequest_struct
    {
    TimeSpanDescription TimeSpan ;         /* The TimeSpan to provide
			                    * instances for.
					    */
    DataQuantityDescriptionList DQDList ;  /* The DQD's to provide
					    * instances for.
					    */

    /* This contains a description of how many points to allocate
     * in each component of each DQI:
     */
    NPointsReqListList             NPointsInDqtys ;

    IntList  QualityFlagUsage ;            /* This list indicates which
					    * components of which DQD's in
					    * "DQDList" will have an
					    * associated bitmap of quality
					    * bits (one per data point).
					    *
					    * If the size of this list
					    * (QualityFlagUsage->nqtys)
					    * is ZERO, then NONE of the
					    * components of the DQDs will
					    * have quality bit flags.
					    *
					    * Otherwise, the size must be
					    * an even number and the list
					    * consists of pairs of numbers,
					    * the first being the DQD index
					    * in "DQDList" and the second
					    * being the component of that
					    * DQD which will have quality
					    * bits.  Thus, there are:
					    * "size/2" DQD/Component pairs
					    * which will have quality bits.
					    *
					    * Those DQD/Component pairs
					    * NOT in  the "QualityFlagUsage"
					    * list are assumed NOT to have
					    * quality bits.
					    *
					    * Note that, when a DQD has
					    * NO components with quality
					    * bits, then NO space in the
					    * corresponding DQI will be
					    * created for quality bits.
					    * If even just one component
					    * of a DQD is to have quality
					    * bits, then space for all of
					    * the components in the DQI is
					    * created, even though some of
					    * the bits will not be used.
					    */
    DQHRequestControl RequestCtl;          /* Indicates how much and what
					    * type of memory (shared or
					    * mapped) to allocate and assign
					    * for each DQD in "DQDList".
					    */
    } ;
typedef struct DQHRequest_struct DQHRequest ;

extern bool_t xdr_DQHRequest_struct(XDR *, struct DQHRequest_struct *);
extern bool_t xdr_DQHRequest(XDR *, DQHRequest *);

/* ----------------------------------------------------------------------- */
struct DQHRequestList_struct
    {
    int nqtys;
    struct
	{
	u_int qty_len ;
	DQHRequest *qty_val ;
	} qty ;
    } ;
typedef struct DQHRequestList_struct DQHRequestList ;

extern bool_t xdr_DQHRequestList_struct(XDR *, struct DQHRequestList_struct *);
extern bool_t xdr_DQHRequestList(XDR *, DQHRequestList *);


/* ----------------------------------------------------------------------- */
/* This is the basic response from the DQH, to a request for
 * DataQuantityInstance and data assignment to memory:
 */
struct DQHResponse_struct
    {
    DataQuantityInstanceList    DQIList ;  /* The DQI's which were generated
					    * by a preceding request.
					    * These structures include the
					    * information necessary to compute
					    * the "Data" pointers by the
					    * client.
					    */
    IntList          ResultByDQD ;         /* This integer list indicates
					    * how things went when generating,
					    * for each DQD in the request,
					    * the corresponding DQI.  Note that
					    * there will be one integer for
					    * each of the DQD's in the
					    * DQHRequest.  The value of this
					    * integer is as follows:
					    *
					    *    1:
					    *       A new DQI and memory
					    *       assignment was created
					    *       for the DQD.
					    *
					    *    0:
					    *       A previously existing DQI
					    *       for the DQD is returned.
					    *
					    *    < 0:
					    *        An error code - the DQI
					    *        was not generated.
					    */
    } ;
typedef struct DQHResponse_struct DQHResponse ;

extern bool_t xdr_DQHResponse_struct(XDR *, struct DQHResponse_struct *);
extern bool_t xdr_DQHResponse(XDR *, DQHResponse *);

struct DQHResponseList_struct
    {
    int nqtys;
    struct
	{
	u_int qty_len ;
	DQHResponse *qty_val ;
	} qty ;
    } ;
typedef struct DQHResponseList_struct DQHResponseList ;

extern bool_t xdr_DQHResponseList_struct(XDR *, struct DQHResponseList_struct *);
extern bool_t xdr_DQHResponseList(XDR *, DQHResponseList *);

/* ----------------------------------------------------------------------- */
/* This can be used by a DQH client as an argument to:
 *   "OpenDQHInterface"
 * to control various things:  Only a STUB as of 3/28/93:
 */
struct DQHInitializeParameters_struct
    {
    /* 2004/03/04:  Multi-run SDT requires this value: */
    int    SdtRunIdx ;
    } ;
typedef struct DQHInitializeParameters_struct DQHInitializeParameters ;

/* ----------------------------------------------------------------------- */
/* This is the structure to be passed from a client to the DQH to tell it
 * to get rid of all Data storage for a given TimeSpan-Source pair:
 */
struct DQHSourceTimeSpan_struct
    {
    int32                  SourceId ;
    TimeSpanDescription    TimeSpan ;
    } ;
typedef struct DQHSourceTimeSpan_struct DQHSourceTimeSpan ;

extern bool_t xdr_DQHSourceTimeSpan_struct(XDR *, struct DQHSourceTimeSpan_struct *);
extern bool_t xdr_DQHSourceTimeSpan(XDR *, DQHSourceTimeSpan *);

/* ----------------------------------------------------------------------- */
/* This is the structure to be passed from a client to the DQH to tell it
 * to get rid of all Data storage for a given TimeSpan-Source pair:
 */
struct DQHQueryDQI_struct
    {
    StringList             DQDNames ;
    int32                  SourceId ;
    TimeSpanDescription    TimeSpan ;
    } ;
typedef struct DQHQueryDQI_struct DQHQueryDQI ;

extern bool_t xdr_DQHQueryDQI_struct(XDR *, struct DQHQueryDQI_struct *);
extern bool_t xdr_DQHQueryDQI(XDR *, DQHQueryDQI *);

/* ----------------------------------------------------------------------- */
/* This is the structure to be passed from a client to the DQH to use
 * procedure:
 *
 *    REPLACE_DATA_QUANTITY_INSTANCES_PROC
 *
 */
struct DQHReplaceDQIs_struct
    {
    /* As of 2002/02/02, "Dir" is used to indicate which direction
     * the "replacement"'s are to be made as follows:
     *    0 -> replace the originals (DQDNames) with the new.
     *    1 -> restore the original versions (and delete the new).
     * There should be one integer for each element in "DQDNames".
     */
    IntList                Dir ;

    StringList             DQDNames ;
    int32                  SourceId ;
    TimeSpanDescription    TimeSpan ;

    /* There should be one element in "new_DQDNames" for each
     * element in "DQDNames".
     */
    StringList             new_DQDNames ;
    } ;
typedef struct DQHReplaceDQIs_struct DQHReplaceDQIs ;

extern bool_t xdr_DQHReplaceDQIs_struct(XDR *,
    struct DQHReplaceDQIs_struct *);
extern bool_t xdr_DQHReplaceDQIs(XDR *, DQHReplaceDQIs *);

struct DQHReplaceDQIsRet_struct
    {
    /* "Flg" is not used as of 2002/01/29: */
    int                       Flg ;

    /* As of 2002/02/02, "Dir" is used to indicate which direction
     * the "replacement"'s were made as follows:
     *    0 -> replace the old versions (OldList) with the new
     *         replaced versions.
     *    1 -> replace the new versions (NewList) with the original
     *         unreplaced versions.
     * There should be one integer for each element in "DQDNames".
     */
    IntList                Dir ;

    DataQuantityInstanceList  OldList ;
    DataQuantityInstanceList  NewList ;
    } ;
typedef struct DQHReplaceDQIsRet_struct DQHReplaceDQIsRet ;

extern bool_t xdr_DDQHReplaceDQIsRet_struct(XDR *,
    struct DQHReplaceDQIsRet_struct *);
extern bool_t xdr_DQHReplaceDQIsRet(XDR *, DQHReplaceDQIsRet *);

/* ----------------------------------------------------------------------- */
/* Externals: */

extern  SDTListClass *ListOfOpenMemoryUnits ;

/* ----------------------------------------------------------------------- */
/* Function Declarations: */

extern NPointsReq *DQHConstructNPointsReq (int npts, int narrpts) ;

extern NPointsReq  *DQHCopyConstructNPointsReq (NPointsReq *in_nreqptr) ;

extern int  DQHFillNPointsReq (NPointsReq *nreqptr, int npts,
    int narrpts) ;

extern int  DQHCopyNPointsReq (NPointsReq *in_nreqptr,
    NPointsReq *out_nreqptr) ;

extern int  DQHDestructNPointsReq (NPointsReq *nreqptr) ;

extern int  DQHClearNPointsReq (NPointsReq *nreqptr) ;

extern int  DQHPrintNPointsReq (NPointsReq *nreqptr, FILE *ofp) ;

extern NPointsReqList  *DQHConstructNPointsReqList (int nreqs,
    int n1, int *in_npts, int *in_narrpts) ;

extern NPointsReqList  *DQHCopyConstructNPointsReqList (
    NPointsReqList *in_nlist) ;

extern int  DQHFillNPointsReqList (NPointsReqList *nlist,
    int nreqs, int n1, int *in_npts, int *in_narrpts) ;

extern int  DQHCopyNPointsReqList (NPointsReqList *in_nlist,
    NPointsReqList *out_nlist) ;

extern int  DQHDestructNPointsReqList (NPointsReqList *nlist) ;

extern int  DQHClearNPointsReqList (NPointsReqList *nlist) ;

extern int  DQHArrayFillNPointsReqList (int ncd,
    NPointsReq *in_cd, NPointsReqList *out_cdl) ;

extern int  DQHPtrArrayFillNPointsReqList (int ncd,
    NPointsReq **in_cd, NPointsReqList *out_cdl) ;

extern int  DQHPrintNPointsReqList (NPointsReqList *nlist, FILE *ofp) ;


extern DQHRequestControl  *SDTConstructDQHRequestControl (int memory_type,
    int use_existing_memory, PreAllocatedMemoryInfo *in_premem) ;

extern DQHRequestControl  *SDTCopyConstructDQHRequestControl (
    DQHRequestControl *in_dqhreqctl) ;

extern int  SDTFillDQHRequestControl (DQHRequestControl *dqhreqctl,
    int memory_type, int use_existing_memory,
    PreAllocatedMemoryInfo *in_premem) ;

extern int  SDTCopyDQHRequestControl (DQHRequestControl *in_dqhreq,
    DQHRequestControl *out_dqhreq) ;

extern int  SDTDestructDQHRequestControl (DQHRequestControl *dqhreqctl) ;

extern int  SDTClearDQHRequestControl (DQHRequestControl *dqhreqctl) ;

extern int  SDTPrintDQHRequestControl (DQHRequestControl *dqhreqctl,
    FILE *ofp) ;

extern DQHRequest  *SDTConstructDQHRequest (TimeSpanDescription *in_tspan,
    DataQuantityDescriptionList *in_dlist,
    NPointsReqList *in_nlist, int memory_type, IntList *q_usage,
    PreAllocatedMemoryInfo *in_premem) ;

extern DQHRequest  *SDTAlternateConstructDQHRequest (
    TimeSpanDescription *in_tspan,
    int  ndqd, DataQuantityDescription **in_dlist,
    NPointsReqList *in_nlist, int memory_type, IntList *q_usage,
    PreAllocatedMemoryInfo *in_premem) ;

extern DQHRequest  *SDTCopyConstructDQHRequest (DQHRequest *in_dqhreq) ;

extern int  SDTFillDQHRequest (DQHRequest *dqhreq,
    TimeSpanDescription *in_tspan, DataQuantityDescriptionList *in_dlist,
    NPointsReqList *in_nlist, int memory_type, IntList *q_usage,
    PreAllocatedMemoryInfo *in_premem) ;

extern int  SDTCopyDQHRequest (DQHRequest *in_dqhreq, DQHRequest *out_dqhreq) ;

extern int  SDTDestructDQHRequest (DQHRequest *dqhreq) ;

extern int  SDTClearDQHRequest (DQHRequest *dqhreq) ;

extern int  SDTPrintDQHRequest (DQHRequest *dqhreq, FILE *ofp) ;

extern DQHRequestList  *SDTConstructDQHRequestList (int ndqhreqs,
    DQHRequest **in_dqhreq) ;

extern DQHRequestList  *SDTCopyConstructDQHRequestList (
    DQHRequestList *in_dqhreql) ;

extern int  SDTFillDQHRequestList (DQHRequestList *dqhreql,
    int ndqhreqs, DQHRequest **in_dqhreq) ;

extern int  SDTCopyDQHRequestList (DQHRequestList *in_dqhreql,
    DQHRequestList *out_dqhreql) ;

extern int  SDTDestructDQHRequestList (DQHRequestList *dqhreql) ;

extern int  SDTClearDQHRequestList (DQHRequestList *dqhreql) ;

extern int  SDTArrayFillDQHRequestList (int ndqhreq, DQHRequest *in_dqhreq,
    DQHRequestList *out_dqhreql) ;

extern int  SDTPtrArrayFillDQHRequestList (int ndqhreq,
    DQHRequest **in_dqhreq, DQHRequestList *out_dqhreql) ;

extern int  SDTPrintDQHRequestList (DQHRequestList *dqhreql, FILE *ofp) ;

extern DQHResponse  *SDTConstructDQHResponse (int ndqis,
    DataQuantityInstance **in_dqi, int *in_results) ;

extern DQHResponse  *SDTCopyConstructDQHResponse (DQHResponse *in_dqhrsp) ;

extern int  SDTFillDQHResponse (DQHResponse *dqhrsp, int ndqis,
    DataQuantityInstance **in_dqi, int *in_results) ;

extern int  SDTCopyDQHResponse (DQHResponse *in_dqhrsp,
    DQHResponse *out_dqhrsp) ;

extern int  SDTDestructDQHResponse (DQHResponse *dqhrsp) ;

extern int  SDTClearDQHResponse (DQHResponse *dqhrsp) ;

extern int  SDTPrintDQHResponse (DQHResponse *dqhrsp, FILE *ofp) ;

extern DQHResponseList  *SDTConstructDQHResponseList (int ndqhrsps,
    DQHResponse **in_dqhrsp) ;

extern DQHResponseList  *SDTCopyConstructDQHResponseList (
    DQHResponseList *in_dqhrspl) ;

extern int  SDTFillDQHResponseList (DQHResponseList *dqhrspl,
    int ndqhrsps, DQHResponse **in_dqhrsp) ;

extern int  SDTCopyDQHResponseList (DQHResponseList *in_dqhrspl,
    DQHResponseList *out_dqhrspl) ;

extern int  SDTDestructDQHResponseList (DQHResponseList *dqhrspl) ;

extern int  SDTClearDQHResponseList (DQHResponseList *dqhrspl) ;

extern int  SDTArrayFillDQHResponseList (int ndqhrsp, DQHResponse *in_dqhrsp,
    DQHResponseList *out_dqhrspl) ;

extern int  SDTPtrArrayFillDQHResponseList (int ndqhrsp,
    DQHResponse **in_dqhrsp, DQHResponseList *out_dqhrspl) ;

extern int  SDTPrintDQHResponseList (DQHResponseList *dqhrspl, FILE *ofp) ;

extern int  compare_MemoryUnit (void *obj1, void *obj2) ;

extern DQHSourceTimeSpan  *SDTConstructDQHSourceTimeSpan (
    int32 in_source_id, TimeSpanDescription *in_tspan) ;

extern DQHSourceTimeSpan  *SDTCopyConstructDQHSourceTimeSpan (
    DQHSourceTimeSpan *in_SoTsp) ;

extern int  SDTFillDQHSourceTimeSpan (DQHSourceTimeSpan *SoTsp,
    int32 in_source_id, TimeSpanDescription *in_tspan) ;

extern int  SDTCopyDQHSourceTimeSpan (DQHSourceTimeSpan *in_SoTsp,
    DQHSourceTimeSpan *out_SoTsp) ;

extern int  SDTDestructDQHSourceTimeSpan (DQHSourceTimeSpan *SoTsp) ;

extern int  SDTClearDQHSourceTimeSpan (DQHSourceTimeSpan *SoTsp) ;

extern int  SDTPrintDQHSourceTimeSpan (DQHSourceTimeSpan *SoTsp,
    FILE *ofp) ;

extern DQHReplaceDQIs *SDTConstructDQHReplaceDQIs (int ndqds,
    int *idir, char **cur_names, char **new_names,
    int32 i_source_id, TimeSpanDescription *tspan) ;

extern DQHReplaceDQIs *SDTCopyConstructDQHReplaceDQIs (
   DQHReplaceDQIs *i_rdqi) ;

extern int SDTFillDQHReplaceDQIs (DQHReplaceDQIs *rdqi, int ndqds,
    int *idir, char **cur_names, char **new_names,
    int32 i_source_id, TimeSpanDescription *tspan) ;

extern int SDTCopyDQHReplaceDQIs (DQHReplaceDQIs *i_rdqi,
    DQHReplaceDQIs *o_rdqi) ;

extern int SDTDestructDQHReplaceDQIs (DQHReplaceDQIs *rdqi) ;

extern int SDTClearDQHReplaceDQIs (DQHReplaceDQIs *rdqi) ;

extern int SDTPrintDQHReplaceDQIs (DQHReplaceDQIs *rdqi, FILE *ofp) ;

extern DQHReplaceDQIsRet *SDTConstructDQHReplaceDQIsRet (
    int in_Flg, int *idir,
    DataQuantityInstanceList *in_OldList,
    DataQuantityInstanceList *in_NewList) ;

extern DQHReplaceDQIsRet *SDTCopyConstructDQHReplaceDQIsRet (
    DQHReplaceDQIsRet *i_rdqi) ;

extern int SDTFillDQHReplaceDQIsRet (DQHReplaceDQIsRet *rdqi,
    int in_Flg, int *idir,
    DataQuantityInstanceList *in_OldList,
    DataQuantityInstanceList *in_NewList) ;

extern int SDTCopyDQHReplaceDQIsRet (DQHReplaceDQIsRet *rdqi,
    DQHReplaceDQIsRet *o_rdqi) ;

extern int SDTDestructDQHReplaceDQIsRet (DQHReplaceDQIsRet *rdqi) ;

extern int SDTClearDQHReplaceDQIsRet (DQHReplaceDQIsRet *rdqi) ;

extern int SDTPrintDQHReplaceDQIsRet (DQHReplaceDQIsRet *rdqi,
    FILE *ofp) ;

extern DQHQueryDQI *SDTConstructDQHQueryDQI (StringList *in_names,
    int32 in_source_id, TimeSpanDescription *in_tspan) ;

extern DQHQueryDQI *SDTCopyConstructDQHQueryDQI (DQHQueryDQI *in_qdqis) ;

extern int  SDTFillDQHQueryDQI (DQHQueryDQI *qdqis,  StringList *in_names,
    int32 in_source_id, TimeSpanDescription *in_tspan) ;

extern int  SDTCopyDQHQueryDQI (DQHQueryDQI *in_qdqis,
    DQHQueryDQI *out_qdqis) ;

extern int  SDTDestructDQHQueryDQI (DQHQueryDQI *qdqis) ;

extern int  SDTClearDQHQueryDQI (DQHQueryDQI *qdqis) ;

extern int  SDTPrintDQHQueryDQI (DQHQueryDQI *qdqis, FILE *ofp) ;

extern int  OpenDQHInterface (DQHInitializeParameters *in_dqhinit) ;
extern int  CloseDQHInterface () ;
extern int  DetachFromAllDQHMemoryUsage () ;
extern int  PrintCurrentMemoryUnits (FILE *ofp) ;

extern DataQuantityInstanceList *DQHCreateDataQuantityInstances (
    int nrequests, DQHRequest **requests, IntList **Results) ;

extern int DQHReplaceDataQuantityInstances (DQHReplaceDQIs *dqdqi,
    DQHReplaceDQIsRet *OutDQIs, IntList **Results) ;

extern DataQuantityInstanceList *DQHQueryDataQuantityInstanceList (
    int nrequests, char **DQDNames, TimeSpanDescription *tspan,
    int32 source_id, IntList **Results) ;
extern DataQuantityInstanceList *DQHQueryAllDataQuantityInstanceList () ;
extern  IntList  *DQHRemoveDataQuantityInstances (int nrequests,
    DQHRequest **requests) ;
extern  int  DQHRemoveSourceTimeSpan (DQHSourceTimeSpan *SoTsp) ;
extern  int  DQHShutdown () ;
extern  int  SDTAttachDataQuantityInstanceList (
    DataQuantityInstanceList *in_dqil) ;
extern  int  SDTAttachDataQuantityInstanceLinkedList (
    SDTListClass *in_llist) ;
extern  int  SDTDetachDataQuantityInstanceList (
    DataQuantityInstanceList *in_dqil) ;
extern  int  SDTDetachDataQuantityInstanceLinkedList (
    SDTListClass *in_llist) ;
extern  int  SDTDetachDQHReplaceDQIs (DQHReplaceDQIs *dqiqi,
    int32 source_id) ;
extern DQHRequest  **SDTRecombineDQIListToDQHRequests (
    DataQuantityInstanceList *in_dqil, int *ndqhreq, int *ecode) ;

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* DQHINTERFACE_H */
