
#ifndef JCS_Plot_h
#define JCS_Plot_h
// Plot.h

#ifndef JCS_ERROR_LOG
#define JCS_ERROR_LOG "Plot.log"
#endif // JCS_ERROR_LOG

#define SccsId_Plot_h "@(#)Plot.h	1.16, 04/17/07"

#ifdef OLD_STYLE_STR_HDRS
#include <iostream.h>
#else
#include <iostream>
using std::cin ;
using std::cout ;
using std::flush ;
using std::endl ;
#endif

#include <SDTDescription.h>
#include <SDTSourceDefs.h>
#include <General.h>
#include <Error1.h>
#include <LinkList.h>
#include <Files.h>
#include <Data.h>

// This is the name of the new, binary version, of SCM.Plot.dat
// (JBV - 2000/05/06)
#define  DEFAULT_SCM_PLOT_BIN_FNAME   "SCM.Plot.bin"

#define  MAX_PLOTUNIT_GROUP_NAME_LEN  100

#define  PLOT_GROUP_OUTPUT_BY_SPC     "OutputBySC"
#define  PLOT_GROUP_OUTPUT_BY_PLOT    "OutputByPQD"

// ----------------------------------------------------------
// The main structure for filling a PQD or PQD template:
//
class PlotUnit: public Unit
   {
private:
   PlotQuantityDescription *PQD ;
public:
   int   spc_id ;
   int   GroupingFlg ;
   int   is_pqd_template ;
   PlotUnit( char *title, char *plottype, int rstrct, 
	     DataQuantityIdentifierList *DQIdList, 
	     class ConstantLinkList *ConstantList, 
	     char *linespoints, int pointsplot,
	     class AttributeLinkList *AttributeList,
             class DataList *datalist,
             char *mlstyle, int nsymbols, char *contourstyle,
             char *brkflag, double brkinterval, char *ebarstate,
             char *useyellow, double *yellow, char *usered, double *red) ;
   PlotUnit() ;
   PlotUnit( PlotQuantityDescription *ThePQD) ;
   PlotUnit *copy() ;
   ~PlotUnit() ;
   void Print( ostream &os) ;
   PlotQuantityDescription *GetPQD( void) ;
   char *GetName( void) ;
   int  WriteToFile (FILE *of) ;
   int  ReadFromFile (FILE *of) ;
   int  GetIsPQDTemplate() ;

   } ;

// ----------------------------------------------------------
class PlotList: public LinkList
   {
private:
   int   Restrictions ;
public:
   PlotList() ;

   PlotList( char *filename, DataList *datalist, int printIt,
       int RestrictFlag, ostream &os) ;

   ~PlotList() ;

   void AddFile( char *filename, DataList *datalist, int printIt,
       ostream &os) ;

   PlotUnit *Find( char *n) ;
   int  print (FILE *ofp) ;
   } ;

// ----------------------------------------------------------
/* A PlotUnit "Group".  This contains a "grouped" list of "PlotUnits".
 * This is used with Templated PlotUnits to allow us to specify that
 * they are to be ordered, in the final PlotUnit list, by iterating
 * over plot, in the inner loop, instead of by iterating by SCraft
 * in the inner loop.  If not grouped, then two or more templated
 * spacecraft are order by spacecraft in the inner loop.
 */
struct PlotUnitInfoGroup_struct
    {
    char  grp_name[MAX_PLOTUNIT_GROUP_NAME_LEN + 1] ;

    int   npqdis ;
    int   nalloc ;

    // Note that we keep a list of "PlotUnit" pointers, not
    // an array of "PlotUnits".
    PlotUnit  **plist ;
    } ;
typedef  struct PlotUnitInfoGroup_struct  PlotUnitInfoGroup ;

struct PlotUnitInfoGroupList_struct
    {
    int   ngrps ;
    int   nalloc ;
    PlotUnitInfoGroup **glist ;
    } ;
typedef  struct PlotUnitInfoGroupList_struct  PlotUnitInfoGroupList ;

// ----------------------------------------------------------
class PlotFile: public Files
   {
private:
   DataList *DL ;
   int      lcnt ;   // current line in the file (1'st line is "1")

   // For Grouping in the multi-spacecraft template case:
   int      WeAreNowGrouping ;
   PlotUnitInfoGroup      *CurrGroup ;
   PlotUnitInfoGroupList  *CurrGroupList ;

   // Since the method "GetEntry" can only return one "PlotUnit"
   // at a time, the advent of Templates (Feb 2007) requires that
   // we keep a queue.  Each call to "GetEntry" reads one entry
   // in the input file, which will either be a direct "PlotUnit",
   // or a Template.   If it is a Template, then more than one
   // "PlotUnit" will be generated, requiring the queue.  As
   // indicated below, the above description is not what happens
   // when "Grouping" is one.
   //
   // The "queue" mechanism works as follows:
   //
   //    If on a call to "GetEntry", the queue is non-empty,
   //    then the first element in the queue will be returned,
   //    immediately (and that element, of course, will be taken
   //    off of the queue).
   //
   //    ONLY when there are no elements in the queue, is the next
   //    entry in the input file (either a direct "PlotUnit" or a
   //    Template) processed.  This will result in a "PlotUnit" being
   //    returned (unless Grouping is turned on).  In the Template
   //    case, only the first "PlotUnit" is returned - the others
   //    are queued.
   //
   //    If Grouping is on, then "GetEntry" returns a pointer to
   //    a "Delay PlotUnit".  This is required because the
   //    C++ base class "File" forces an end to the file processing
   //    when a NULL is returned.   So we have to return something
   //    to prevent premature File closure.   The loop which calls
   //    "GetEntry" checks the return and, if it detects that this
   //    return is the "Delay PlotUnit", it does nothing.
   //
   //    Please note that the queue NEVER wraps around "0".  The
   //    pending queue elements, the number of which are stored in
   //    "NumberQElts", are always contiguous, starting from
   //    "FirstQElt".  So the last element on the queue is at index
   //    "FirstQElt + NumberQElts - 1" in "PlotQueue".  So the code
   //    must insure that "PlotQueue" always contains at least:
   // 
   //       FirstQElt + NumberQElts
   //    
   //    elements.  The current size of the "PlotQueue" array is
   //    always stored in "AllocatedQElts".   The internal code
   //    grows and shrinks "PlotQueue" as required.
   //
   int       FirstQElt ;
   int       NumberQElts ;
   int       AllocatedQElts ;
   PlotUnit  **PlotQueue ;

public:
   PlotFile (char *filename, DataList *datalist) ;
   ~PlotFile() ;
   Unit* GetEntry( void) ;
   int   GetPlotUnitSpcList (char *spcs, int *nscs, int *scbuf) ;
   int   ParsePQDSubIdentName (char *ibuf, int *plen, char *prefix,
             int *slen, char *suffix, int *sub_id_found) ;
   int   GetSubSpacecraftDefinitions (char *ibuf, char *sname,
             int *nsc, char **spc_labels) ;
   void  StartGroup (char *gname, int lno) ;
   void  EndGroup (char *istr, int lno) ;
   void  InsertGroup (char *gname, int lno) ;
   int   Output_PlotUnits_in_PlotUnitInfoGroup (
             PlotUnitInfoGroup *pgr, char *spcs, int order) ;
   } ;

// ----------------------------------------------------------
class ConstantUnit: public Unit
   {
private:
   char *Label ;
   char *Units ;
   int DIndex ;
   int CIndex ;
   int AIndex ;
   int ACIndex ;
public:
   ConstantUnit() ;
   ConstantUnit( char *l, char *u, int di, int ci, int ai, int aci) ;
   ~ConstantUnit() ;
   Unit *Copy() ;
   char *GetLabel() ;
   char *GetUnits() ;
   int GetDataIndex() ;
   int GetComponentIndex() ;
   int GetArrayIndex() ;
   int GetAssociatedIndex() ;
   } ;

class AttributeUnit: public Unit
   {
private:
   double MinLimit ;
   double MaxLimit ;
public:
   AttributeUnit() ;
   AttributeUnit( double mil, double mal) ;
   ~AttributeUnit() ;
   Unit *Copy() ;
   double GetMinLimit( void) ;
   double GetMaxLimit( void) ;
   } ;

class ConstantLinkList: public LinkList
   {
private:
public:
   ConstantLinkList() ;
   ~ConstantLinkList() ;
   ConstantUnit *GetConstant( int n) ;
   } ;

class AttributeLinkList: public LinkList
   {
private:
public:
   AttributeLinkList() ;
   ~AttributeLinkList() ;
   AttributeUnit *GetAttribute( int n) ;
   } ;

// ---------------------------------------------------------------
int WritePlotListToFile (PlotList *PL, FILE *of) ;
PlotList *ReadPlotListFromFile (FILE *of) ;
int AppendToPlotListFromFile (PlotList *PL, FILE *of) ;

#endif  // JCS_Plot_h

