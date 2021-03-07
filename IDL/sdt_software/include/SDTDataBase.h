/* SDTDataBase.h */

#ifndef SDTDATABASE_H
#define SDTDATABASE_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* SSL Data Tools Data Base Module:  declarations: */

/* SCCS ID string: */
#define SccsId_SDTDataBase_h "@(#)SDTDataBase.h	1.13, 03/22/06"

#include <stdio.h>
#include <rpc/types.h>
#include <SDTType.h>
#include <SDTDescription.h>
#include <SDTSpacecraft.h>

/* ----------------------------------------------------------------------- */
/* Constants: */
#define  SDT_DBASE_MOVE_DIRECTORY   6185

#define  SDT_PARSE_QUERY_TIMESPAN   "TimeSpan:"

/* ----------------------------------------------------------------------- */
/* Structures and Typedefs: */

struct DataBaseResponse_struct
   {
   TimeSpanDescription  TimeSpan ;
   String               FileName ; /* Mainly for Fast Decom of I&T files */
   int                  Flg1 ;     /* Reserved - 94/03/15 */
   } ;
typedef struct DataBaseResponse_struct DataBaseResponse ;

struct DataBaseResponseList_struct {
	int nqtys;
	struct {
		u_int qty_len;
		DataBaseResponse *qty_val;
	} qty;
};
typedef struct DataBaseResponseList_struct DataBaseResponseList ;

struct QueryTimeSpanIsCountedResponse_struct
   {
   /* 0 -> No count session required,  else indicates the id number
    * of the session started by the source to count the data
    * in the requested time span:
    */
   int          CountSessionId ;

   /* 0 -> count session is not done,  1 -> count session is done */
   int		IsDone ;

   /* 0 -> count session done percentage is NOT available, 
    * 1 -> count session is available and the current value is in the
    *     field:  PercentDoneValue
    */
   int		PercentDoneIsAvailable ;

   /* Only meaningful if "PercentDoneIsAvailable" is 1:
    *   Reports the percentage (0-100) that the source computes
    *   is currently completed of the count session in progress.
    */
   int		PercentDoneValue ;
   } ;
typedef struct QueryTimeSpanIsCountedResponse_struct QueryTimeSpanIsCountedResponse ;

struct DataBaseEntryIsReady_struct
   {
   DataBaseResponse                EntryInfo ;
   QueryTimeSpanIsCountedResponse  QueryStatus ;
   } ;
typedef struct DataBaseEntryIsReady_struct DataBaseEntryIsReady ;

/* Structure for RPC communication for Near-Realtime ISTP data
 * handling.  When installed, it was not clear what is exactly
 * needed so this is very general purpose and serves as both
 * the input and output structure for the NRT_SYNCHRONIZE_RPC
 * call.
 *
 * NOTE:  2005/05/07:  It turns out that this structure will
 * also serve well as the return for the new RPC call:
 *
 *    GET_DATA_FROM_DBASE_REQUEST_PROC
 *
 * which allows "sdt_batch" to ask for a TimeSpan/OrbitRange
 * data set without requiring that the batch file know where
 * the data is located.  In this usage, "DBInfo", "SList",
 * and possibly "Id" and or "Stat1" are used.  It was decided
 * to use this structure since all of the support routines are
 * already in place.
 */
struct NRTSyncInformation_struct
   {
   DataBaseResponse                DBInfo ;
   int                             Id ;
   int                             Stat1 ;
   int                             Stat2 ;
   int                             Stat3 ;
   int                             Stat4 ;
   int                             Stat5 ;
   int                             Stat6 ;
   int                             Stat7 ;
   StringList                      SList ;
   } ;
typedef struct NRTSyncInformation_struct NRTSyncInformation ;

/* Added 2004/07/16:  This will be required to support multiple
 * spacecraft in a single SDT plot window.  We need a consolidated
 * structure containing "DataBaseResponse" and SpaceCraft.
 *
 * NOTE:  As of July 2004, no RPC/XDR routines were required for
 *        this structure - so they were not implemented.
 */
struct DBRespAndSCraft_struct
   {
   DataBaseResponse            DB ;
   SpaceCraftDescription       Spc ;
   } ;
typedef struct DBRespAndSCraft_struct DBRespAndSCraft ;

struct DBRespAndSCraftList_struct {
	int nqtys;
	struct {
		u_int qty_len;
		DBRespAndSCraft *qty_val;
	} qty;
};
typedef struct DBRespAndSCraftList_struct DBRespAndSCraftList ;

/* ----------------------------------------------------------------------- */
/* Function Declarations: */

#ifdef ANSI_STD_C

extern DataBaseResponse *SDTConstructDataBaseResponse (
    TimeSpanDescription *in_tspan, char *in_File, int in_Flg1) ;

extern DataBaseResponse *SDTCopyConstructDataBaseResponse (
    DataBaseResponse *in_dbrsp) ;

extern int SDTFillDataBaseResponse (DataBaseResponse *dbrsp,
    TimeSpanDescription *in_tspan, char *in_File, int in_Flg1) ;

extern int SDTCopyDataBaseResponse (DataBaseResponse *in_dbrsp,
    DataBaseResponse *out_dbrsp) ;

extern int SDTDestructDataBaseResponse (DataBaseResponse *dbrsp) ;

extern int SDTClearDataBaseResponse (DataBaseResponse *dbrsp) ;

extern int SDTPrintDataBaseResponse (DataBaseResponse *dbrsp, FILE *ofp) ;

extern int SDTCompareDataBaseResponse( DataBaseResponse *first_dbr, 
    DataBaseResponse *second_dbr) ;

extern DataBaseResponseList  *SDTConstructDataBaseResponseList (int ndbrsp,
    TimeSpanDescription  **in_tspans, char **in_Files, int *in_Flg1s) ;

extern DataBaseResponseList  *SDTCopyConstructDataBaseResponseList (
    DataBaseResponseList *in_dblist) ;

extern int  SDTFillDataBaseResponseList (DataBaseResponseList *dblist,
    int ndbrsp, TimeSpanDescription  **in_tspans, char **in_Files,
    int *in_Flg1s) ;

extern int  SDTCopyDataBaseResponseList (DataBaseResponseList *in_dblist,
    DataBaseResponseList *out_dblist) ;

extern int  SDTDestructDataBaseResponseList (DataBaseResponseList *dblist) ;

extern int  SDTClearDataBaseResponseList (DataBaseResponseList *dblist) ;

extern int  SDTArrayFillDataBaseResponseList (int ndb,
    DataBaseResponse *in_db, DataBaseResponseList *out_dbl) ;

extern int  SDTPrintDataBaseResponseList (DataBaseResponseList *dblist,
    FILE *ofp) ;

extern NRTSyncInformation *SDTConstructNRTSyncInformation (
    int in_Id, TimeSpanDescription *in_tspan, char *in_File,
    int *in_Flgs, int nstr, char **istr) ;

extern NRTSyncInformation *SDTCopyConstructNRTSyncInformation (
    NRTSyncInformation *in_ninfo) ;

extern int SDTFillNRTSyncInformation (NRTSyncInformation *ninfo,
    int in_Id, TimeSpanDescription *in_tspan, char *in_File,
    int *in_Flgs, int nstr, char **istr) ;

extern int SDTCopyNRTSyncInformation (NRTSyncInformation *in_ninfo,
    NRTSyncInformation *out_ninfo) ;

extern int SDTDestructNRTSyncInformation (NRTSyncInformation *ninfo) ;

extern int SDTClearNRTSyncInformation (NRTSyncInformation *ninfo) ;

extern int SDTPrintNRTSyncInformation (NRTSyncInformation *ninfo, FILE *ofp) ;

extern int SDTCompareNRTSyncInformation( NRTSyncInformation *first_ninfo, 
    NRTSyncInformation *second_ninfo) ;

extern int SDTParseQueryDataTimeSpan (char *istr, int16 SpCraft, int imode,
    DataBaseResponse *odbres, char **ostr, int *ostat) ;

extern StringList *SDTEncodeBatchDBaseRequest (int itype, int sorb,
    int eorb, TimeSpanDescription *itspan, StringList *idqdinfo) ;

extern int SDTDecodeBatchDBaseRequest (StringList *rlist, int *itype,
    int *sorb, int *eorb, TimeSpanDescription *itspan,
    StringList *idqdinfo) ;

extern DBRespAndSCraft *SDTConstructDBRespAndSCraft (
    TimeSpanDescription *in_tspan, char *in_File, int in_Flg1,
    SpaceCraftDescription *in_Spc) ;

extern DBRespAndSCraft *SDTCopyConstructDBRespAndSCraft (
    DBRespAndSCraft *in_dbrsp_sc) ;

extern int SDTFillDBRespAndSCraft (DBRespAndSCraft *dbrsp_sc,
    TimeSpanDescription *in_tspan, char *in_File, int in_Flg1,
    SpaceCraftDescription *in_Spc) ;

extern int SDTCopyDBRespAndSCraft (DBRespAndSCraft *in_dbrsp_sc,
    DBRespAndSCraft *out_dbrsp_sc) ;

extern int SDTDestructDBRespAndSCraft (DBRespAndSCraft *dbrsp_sc) ;

extern int SDTClearDBRespAndSCraft (DBRespAndSCraft *dbrsp_sc) ;

extern int SDTPrintDBRespAndSCraft (DBRespAndSCraft *dbrsp_sc,
    FILE *ofp) ;

extern int SDTCompareDBRespAndSCraft (DBRespAndSCraft *first_dbrsp_sc, 
    DBRespAndSCraft *second_dbrsp_sc) ;

extern DBRespAndSCraftList  *SDTConstructDBRespAndSCraftList (int ndbrsp,
    TimeSpanDescription  **in_tspans, char **in_Files, int *in_Flg1s,
    SpaceCraftDescription **in_Spcs) ;

extern DBRespAndSCraftList  *SDTCopyConstructDBRespAndSCraftList (
    DBRespAndSCraftList *in_dblist) ;

extern int  SDTFillDBRespAndSCraftList (DBRespAndSCraftList *dblist,
    int ndbrsp, TimeSpanDescription  **in_tspans, char **in_Files,
    int *in_Flg1s, SpaceCraftDescription **in_Spcs) ;

extern int  SDTCopyDBRespAndSCraftList (DBRespAndSCraftList *in_dblist,
    DBRespAndSCraftList *out_dblist) ;

extern int  SDTDestructDBRespAndSCraftList (DBRespAndSCraftList *dblist) ;

extern int  SDTClearDBRespAndSCraftList (DBRespAndSCraftList *dblist) ;

extern int  SDTArrayFillDBRespAndSCraftList (int ndb,
    DBRespAndSCraft *in_db, DBRespAndSCraftList *out_dbl) ;

extern int  SDTPtrArrayFillDBRespAndSCraftList (int ndb,
    DBRespAndSCraft **in_db, DBRespAndSCraftList *out_dbl) ;

extern int  SDTPrintDBRespAndSCraftList (DBRespAndSCraftList *dblist,
    FILE *ofp) ;

#else

extern DataBaseResponse *SDTConstructDataBaseResponse () ;
extern DataBaseResponse *SDTCopyConstructDataBaseResponse () ;
extern int SDTFillDataBaseResponse () ;
extern int SDTCopyDataBaseResponse () ;
extern int SDTDestructDataBaseResponse () ;
extern int SDTClearDataBaseResponse () ;
extern int SDTPrintDataBaseResponse () ;
extern int SDTCompareDataBaseResponse () ;

extern DataBaseResponseList  *SDTConstructDataBaseResponseList () ;
extern DataBaseResponseList  *SDTCopyConstructDataBaseResponseList () ;
extern int  SDTFillDataBaseResponseList () ;
extern int  SDTCopyDataBaseResponseList () ;
extern int  SDTDestructDataBaseResponseList () ;
extern int  SDTClearDataBaseResponseList () ;
extern int  SDTArrayFillDataBaseResponseList () ;
extern int  SDTPrintDataBaseResponseList () ;

extern NRTSyncInformation *SDTConstructNRTSyncInformation () ;
extern NRTSyncInformation *SDTCopyConstructNRTSyncInformation () ;
extern int SDTFillNRTSyncInformation () ;
extern int SDTCopyNRTSyncInformation () ;
extern int SDTDestructNRTSyncInformation () ;
extern int SDTClearNRTSyncInformation () ;
extern int SDTPrintNRTSyncInformation () ;
extern int SDTCompareNRTSyncInformation() ;

extern int SDTParseQueryDataTimeSpan () ;

extern DBRespAndSCraft *SDTConstructDBRespAndSCraft () ;
extern DBRespAndSCraft *SDTCopyConstructDBRespAndSCraft () ;
extern int SDTFillDBRespAndSCraft () ;
extern int SDTCopyDBRespAndSCraft () ;
extern int SDTDestructDBRespAndSCraft () ;
extern int SDTClearDBRespAndSCraft () ;
extern int SDTPrintDBRespAndSCraft () ;
extern int SDTCompareDBRespAndSCraft () ;

extern DBRespAndSCraftList  *SDTConstructDBRespAndSCraftList () ;
extern DBRespAndSCraftList  *SDTCopyConstructDBRespAndSCraftList () ;
extern int  SDTFillDBRespAndSCraftList () ;
extern int  SDTCopyDBRespAndSCraftList () ;
extern int  SDTDestructDBRespAndSCraftList () ;
extern int  SDTClearDBRespAndSCraftList () ;
extern int  SDTArrayFillDBRespAndSCraftList () ;
extern int  SDTPtrArrayFillDBRespAndSCraftList () ;
extern int  SDTPrintDBRespAndSCraftList () ;

#endif

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* SDTDATABASE_H */
