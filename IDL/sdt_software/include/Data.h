
#ifndef JCS_Data_h
#define JCS_Data_h
// Data.h

#ifndef JCS_ERROR_LOG
#define JCS_ERROR_LOG "Data.log"
#endif // JCS_ERROR_LOG

#define SccsId_Data_h "@(#)Data.h	1.21, 04/26/07"

#include <stdio.h>
#include <ctype.h>

#ifdef OLD_STYLE_STR_HDRS
#include <iostream.h>
#else
#include <iostream>
using std::cin ;
using std::cout ;
using std::flush ;
using std::endl ;
#endif

#include <General.h>
#include <Error1.h>
#include <LinkList.h>
#include <Files.h>

#include <SDTSourceDescription.h>
#include <SDTSourceDefs.h>
#include <SDTReadWrite.h>

// This is the name of the new, binary version, of SCM.Data.dat
// (JBV - 2000/05/06)
#define  DEFAULT_SCM_DATA_BIN_FNAME   "SCM.Data.bin"

class DataUnit: public Unit
   {
private:
   DataQuantityDescription *DQD ;
   float Scale ;
   float Offset ;
   char *OrigExecute ;
   char *Execute ;
   char *ComputeFunction ;
   int UseShading ;
   class ParamLinkList *Param ;
public:
   DataUnit() ;
   DataUnit (char *name, SpaceCraftDescription *spc,
             int32 src_id, 
	     class ComponentLinkList *ComponentList, 
	     double scale, double offset, char *execute,
	     char *origexecute, int shade,
	     int16 NumAlg, int16 CurAlg,
	     class ParamLinkList *ParamList,
             DataQuantityIdentifierList DQIdList) ;
   DataUnit (char *name, SpaceCraftDescription *spc,
             int32 src_id, 
	     char *quantfmt, char *quanttype, char *derived,
	     class ComponentLinkList *ComponentList, 
	     double scale, double offset, char *execute,
	     char *origexecute, int shade,
	     int16 NumAlg, int16 CurAlg,
	     class ParamLinkList *ParamList,
             DataQuantityIdentifierList DQIdList) ;
   DataUnit (UItoSCMDataUnit *uidp) ;
   ~DataUnit() ;
   void Print( ostream &os) ;
   DataQuantityDescription *GetDQD( void) ;
   char *GetName( void) ;
   float GetScale( void) ;
   float GetOffset( void) ;
   char *GetOrigExecute( void) ;
   char *GetExecute( void) ;
   char *GetComputeFunction( void) ;
   int GetInputCount( void) ;
   char *GetInputName( int n) ;
   int32 GetInputSourceId( int n) ;
   int GetParamCount( void) ;
   char *GetParamPrompt( int param) ;
   int GetParamType( int param) ;
   int GetParamSubType( int param) ;
   char *GetParamData( int param) ;
   SpaceCraftDescription *GetSpaceCraft( void) ;
   int GetQuantFmt( void) ;
   int GetQuantType( void) ;
   int GetDerived( void) ;
   int GetShading( void) ;
   int WriteToFile (FILE *of) ;
   int ReadFromFile (FILE *of) ;
   } ;

class DataList: public LinkList
   {
private:
public:
   int       DUSearchListSize ;
   DataUnit  **DUSearchList ;
   DataList() ;
   DataList( char *filename, int printIt, ostream &os) ;
   ~DataList() ;
   void AddFile (char *filename, int printIt, ostream &os) ;
   DataUnit *Find (char *n) ;
   int AddToSearchList (DataUnit *idu) ;
   int print (FILE *ofp) ;
   } ;

class DataFile: public Files
   {
private:
   // Since the method "GetEntry" can only return one "DataUnit"
   // at a time, the advent of Templates (Feb 2007) requires that
   // we keep a queue.  Each call to "GetEntry" reads one entry
   // in the input file, which will either be a direct "DataUnit",
   // or a Template.   If it is a Template, then more than one
   // "DataUnit" will be generated, requiring the queue.
   //
   // The "queue" mechanism works as follows:
   //
   //    If on a call to "GetEntry", the queue is non-empty,
   //    then the first element in the queue will be returned,
   //    immediately (and that element, of course, will be taken
   //    off of the queue).
   //
   //    ONLY when there are no elements in the queue, is the next
   //    entry in the input file (either a direct "DataUnit" or a
   //    Template) processed.  This will result in a "DataUnit" being
   //    returned.  In the Template case, only the first "DataUnit"
   //    is returned - the others are queued.
   //
   //    Please note that the queue NEVER wraps around "0".  The
   //    pending queue elements, the number of which are stored in
   //    "NumberQElts", are always contiguous, starting from
   //    "FirstQElt".  So the last element on the queue is at index
   //    "FirstQElt + NumberQElts - 1" in "DataQueue".  So the code
   //    must insure that "DataQueue" always contains at least:
   // 
   //       FirstQElt + NumberQElts
   //    
   //    elements.  The current size of the "DataQueue" array is
   //    always stored in "AllocatedQElts".   The internal code
   //    grows and shrinks "DataQueue" as required.
   //
   int       FirstQElt ;
   int       NumberQElts ;
   int       AllocatedQElts ;
   DataUnit  **DataQueue ;
public:
   DataFile( char* filename) ;
   ~DataFile() ;
   Unit* GetEntry( void) ;
   int   GetSubSpacecraftDefinitions (char *ibuf, char *sname,
            int *nsc, char **spc_labels) ;
   int   GetDataUnitSpcList (char *spcs, int *nscs, int *scbuf) ;
   int   ParseDQDSubIdentName (char *ibuf, int *plen,
            char *prefix, int *slen, char *suffix,
	    int *sub_id_found, int *spc_id_found) ;

   int   ExtractMultipleParamValues (char *istr, int *nparams,
            char ***plist) ;
   } ;

class ParamUnit: public Unit
   {
private:
   char *Prompt ;
   int Type ;
   int SubType ;
   char *Data ;
public:
   ParamUnit() ;
   ParamUnit( char *p, char *t, char *st, char *d) ;
   ParamUnit (char *prompt, int type, int stype, char *data) ;
   ~ParamUnit() ;
   void Print( ostream &os) ;
   char *GetPrompt( void) ;
   char *GetData( void) ;
   int GetType( void) ;
   int GetSubType( void) ;
   int WriteToFile (FILE *of) ;
   int ReadFromFile (FILE *of) ;
   Unit *Copy( void) ;
   } ;

class SubComponentUnit: public Unit
   {
private:
   int DiscreteBins ;
   char *Name ;
   char *Units ;
public:
   SubComponentUnit() ;
   SubComponentUnit( int db, char *n, char *u) ;
   ~SubComponentUnit() ;
   Unit *Copy() ;
   int GetDiscreteBins( void) ;
   char *GetName( void) ;
   char *GetUnits( void) ;
   } ;

class SubComponentLinkList: public LinkList
   {
private:
public:
   SubComponentLinkList() ;
   ~SubComponentLinkList() ;
   SubComponentUnit *GetSubComponent( int n) ;
   SubComponentLinkList *Copy( void) ;
   } ;

class ComponentUnit: public Unit
   {
private:
   int Type ;
   int NRepeats ;
   SubComponentLinkList *SubCompList ;

public:
   ComponentUnit() ;
   ComponentUnit( char *t, int n) ;
   ComponentUnit( int t, int n, SubComponentLinkList *SubList) ;
   ComponentUnit( char *t, int n, SubComponentLinkList *SubList) ;
   ~ComponentUnit() ;
   Unit *Copy() ;
   int GetType( void) ;
   int GetNRepeats( void) ;
   SubComponentLinkList *GetSubCompList( void) ;
   } ;

class ParamLinkList: public LinkList
   {
private:
public:
   ParamLinkList() ;
   ParamLinkList( LinkList *ll) ;
   ~ParamLinkList() ;
   ParamUnit *GetParam( int n) ;
   } ;

class ComponentLinkList: public LinkList
   {
private:
public:
   ComponentLinkList() ;
   ~ComponentLinkList() ;
   ComponentUnit *GetComponent( int n) ;
   } ;


// -----------------------------------------------------------------

int WriteParamLinkListToFile (ParamLinkList *PL, FILE *of) ;
ParamLinkList *ReadParamLinkListFromFile (FILE *of) ;

int WriteDataListToFile (DataList *DL, FILE *of) ;
DataList *ReadDataListFromFile (FILE *of) ;

int WriteDataUnitToFile (DataUnit *du, FILE *of) ;
DataUnit *ReadDataUnitFromFile (FILE *of) ;
int AppendToDataListFromFile (DataList *DL, FILE *of) ;

#endif  // JCS_Data_h

