#ifndef JCS_Inst_h
#define JCS_Inst_h
// Inst.h

#ifndef JCS_ERROR_LOG
#define JCS_ERROR_LOG "Inst.log"
#endif // JCS_ERROR_LOG

#define SccsId_Inst_h "@(#)Inst.h	1.18, 02/04/07"

#ifdef OLD_STYLE_STR_HDRS
#include <iostream.h>
#else
#include <iostream>
using std::cin ;
using std::cout ;
using std::flush ;
using std::endl ;
#endif

#include <signal.h>

#ifdef SOLARIS
#include <sysent.h>
#endif

#include <sys/unistd.h>
#include <sys/wait.h>
#include <rpc/rpc.h>
#include <rpc/types.h>
#include <rpc/xdr.h>

#include <General.h>
#include <Boolean.h>
#include <Error1.h>
#include <Data.h>
#include <SDTSpacecraft.h>
#include <SDTMemory.h>
#include <SDTDescription.h>
#include <SDTInstance.h>
#include <LinkList.h>
#include <SDTSourceDefs.h>
#include <SDTSourceDescription.h>
#include <SnapOnXDR.h>
#include <SDTInstanceXDR.h>

#include <DQHInterface.h>


class InstUnit: public Unit
   {
private:
   char  *Name ;
   SpaceCraftDescription ScId ;
   int32 SrcId ;
   DataQuantityInstanceList *DQIList ;
   DQHRequest *DQHR ;
   DataBaseResponse *DBRes ;

   int ErrorStatus ;
public:
   InstUnit() ;
   ~InstUnit() ;
   void Error( void) ;
   int GetArraySize (void) ;
   int GetArrDescExists (void) ;
   int GetArrayDescriptionSize (void) ;
   int GetError (void) ;
   char *GetName (void) ;
   SpaceCraftDescription *GetSpaceCraft (void) ;
   TimeSpanDescription *GetTimeSpan (void) ;
   DataBaseResponse *GetDataBaseResponse (void) ;
   int GetCircular (void) ;
   void *GetComp (int comp) ;
   int GetCompCount (void) ;
   int GetCompType (int comp) ;
   int GetCompNRepeats (int comp) ;
   int GetCurrent (void) ;
   int GetDone (void) ;
   int GetShading (void) ;
   int GetTotal (void) ;
   int SetCircular (int32 julian) ;
   int SetCurrent (int currentpoints) ;
   int SetIn (int in) ;
   int SetDone (void) ;
   int SetShading (void) ;
   DataQuantityInstance *GetDQI() ;
   int GetQuantFmt (void) ;

   friend class ExistInstUnit ;
   friend class NewInstUnit ;
   friend class SCMInstUnit ;
   } ;

class ExistInstUnit: public InstUnit 
   {
private:
public:
   ExistInstUnit( SpaceCraftDescription *spacecraft, int32 source,
       char *name, DataBaseResponse *InDBRes) ;
   ~ExistInstUnit() ;
   } ;

class NewInstUnit: public InstUnit
   {
private:
public:
   NewInstUnit (int MemType, DataQuantityDescription *DQD,
       DataBaseResponse *InDBRes, int size, int arraysize) ;
   ~NewInstUnit() ;
   } ;

class SCMInstUnit: public InstUnit
   {
private:
   long int RPCProg ;
   int RPCVers ;
   char *Executable ;
   int ProcessId ;
   int Use ;
   int SnapRPCOffset ;

#ifdef MULTI_RUN_SDT
   int SdtRunIdx ;
#endif // MULTI_RUN_SDT

public:

#ifdef MULTI_RUN_SDT
   SCMInstUnit( int MemType, DataUnit *DU, DataBaseResponse *InDBRes,
       ParameterReqList *PL, int rpc_offset, int sdt_rpc_offset,
       int in_sdt_run_idx) ;
#else
   SCMInstUnit (int MemType, DataUnit *DU,
       DataBaseResponse *InDBRes, ParameterReqList *PL,
       int rpc_offset, int sdt_rpc_offset) ;
#endif

   ~SCMInstUnit() ;
   void Print (ostream &os) ;
   int16 GetSpaceCraft (void) ;

#ifdef MULTI_RUN_SDT
   int GetSdtRunIdx( void) ;
   int GetProcessId( void) ;
   long int GetRPCProg( void) ;
#endif // MULTI_RUN_SDT

   char *GetName (void) ;
   TimeSpanDescription *GetTimeSpan (void) ;
   DataBaseResponse *GetDataBaseResponse (void) ;

   int CleanUpZombies (void) ;
   int GetSize (SetUpValuesRequest *GSR,
       SetUpValuesResponse *SUVRes) ;
   int FillBuffers (void) ;
   int ShutDown (void) ;

   void IncUse (void) ;
   int DecUse (void) ;
   int GetSnapRPCOffset (void) ;
   } ;

class InstList: public LinkList
   {
private:
public:
   InstList() ;
   ~InstList() ;
   InstUnit *Find (SpaceCraftDescription *spacecraft, char *name,
       DataBaseResponse *InDBRes) ;
   } ;

#endif // JCS_Inst_h
