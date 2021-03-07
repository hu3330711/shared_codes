// clDescription.hh

#ifndef CLDESCRIPTION_HH
#define CLDESCRIPTION_HH

// C++ SSL Data Tools Decriptions (Widgits) Declaration File:

// SCCS ID string:
#define SccsId_clDescription_hh "@(#)clDescription.hh	1.14, 04/17/07"

#ifdef OLD_STYLE_STR_HDRS
#include <iostream.h>
#else
#include <iostream>
using std::cout ;
using std::flush ;
using std::endl ;
#endif

#include <SDTDescription.h>


// Class of a list of void pointers:
// Note!  NEVER try to send a VoidList through an RPC call.  It just
//        doen't make sense.  The VoidList is for local process use only!
//
class clVoidList
    {
    private:
    public:
	VoidList    cs ;

	// Constructor(s):
	clVoidList () ;
	clVoidList (VoidList *in_vlist) ;
	clVoidList (int nvoids, void **in_void) ;
	clVoidList (const clVoidList &) ;

	// "Assignment" operator:
        clVoidList& operator = (const clVoidList &) ;

	// Destructor:
	~clVoidList () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class of an integer list:
class clIntList
    {
    private:
    public:
	IntList    cs ;

	// Constructor(s):
	clIntList () ;
	clIntList (IntList *in_ilist) ;
	clIntList (int nints, int *in_int) ;
	clIntList (const clIntList &) ;

	// "Assignment" operator:
        clIntList& operator = (const clIntList &) ;

	// Destructor:
	~clIntList () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class for a list of "int16"s:
class clInt16List
    {
    private:
    public:
	Int16List    cs ;

	// Constructor(s):
	clInt16List () ;
	clInt16List (Int16List *in_i16list) ;
	clInt16List (int nint16s, int16 *in_int16) ;
	clInt16List (const clInt16List &) ;

	// "Assignment" operator:
        clInt16List& operator = (const clInt16List &) ;

	// Destructor:
	~clInt16List () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class for a list of "int32"s:
class clInt32List
    {
    private:
    public:
	Int32List    cs ;

	// Constructor(s):
	clInt32List () ;
	clInt32List (Int32List *in_i32list) ;
	clInt32List (int nint32s, int32 *in_int32) ;
	clInt32List (const clInt32List &) ;

	// "Assignment" operator:
        clInt32List& operator = (const clInt32List &) ;

	// Destructor:
	~clInt32List () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class of a memory address list:
class clAddrTList
    {
    private:
    public:
	AddrTList    cs ;

	// Constructor(s):
	clAddrTList () ;
	clAddrTList (AddrTList *in_addrlist) ;
	clAddrTList (int nints, AddrT *in_addr) ;
	clAddrTList (const clAddrTList &) ;

	// "Assignment" operator:
        clAddrTList& operator = (const clAddrTList &) ;

	// Destructor:
	~clAddrTList () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;


// Class for a list of "float"s:
class clFloatList
    {
    private:
    public:
	FloatList    cs ;

	// Constructor(s):
	clFloatList () ;
	clFloatList (FloatList *in_flist) ;
	clFloatList (int nflts, float *in_flt) ;
	clFloatList (const clFloatList &) ;

	// "Assignment" operator:
        clFloatList& operator = (const clFloatList &) ;

	// Destructor:
	~clFloatList () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class for a list of "doubles"s:
class clDoubleList
    {
    private:
    public:
	DoubleList    cs ;

	// Constructor(s):
	clDoubleList () ;
	clDoubleList (DoubleList *in_dlist) ;
	clDoubleList (int ndbls, double *in_dbl) ;
	clDoubleList (const clDoubleList &) ;

	// "Assignment" operator:
        clDoubleList& operator = (const clDoubleList &) ;

	// Destructor:
	~clDoubleList () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// "String" Class:
class clString
    {
    private:
    public:
	String  cs ;

	// Constructor(s):
	clString () ;
	clString (String *str_ptr) ;
	clString (char *cptr) ;
	clString (const clString &) ;

	// "Assignment" operator:
        clString& operator = (const clString &) ;

	// Destructor:
	~clString () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class for a list of "String"s:
class clStringList
    {
    private:
    public:
	StringList    cs ;

	// Constructor(s):
	clStringList () ;
	clStringList (StringList *in_str) ;
	clStringList (int nstrings, char **str_ptr) ;
	clStringList (int nstrings, String *in_str) ;
	clStringList (int nstrings, String **in_str) ;
	clStringList (int nstrings, clString *in_str) ;
	clStringList (int nstrings, clString **in_str) ;
	clStringList (const clStringList &) ;

	// "Assignment" operator:
        clStringList& operator = (const clStringList &) ;

	// Destructor:
	~clStringList () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class for a SpaceCraftDescription:
class clSpaceCraftDescription
    {
    private:
    public:
        SpaceCraftDescription  cs ;

	// Constructor(s):
	clSpaceCraftDescription () ;
	clSpaceCraftDescription (SpaceCraftDescription *scrft) ;
	clSpaceCraftDescription (int16 id, int16 sub_id) ;
	clSpaceCraftDescription (const clSpaceCraftDescription &) ;

	// "Assignment" operator:
        clSpaceCraftDescription& operator = (
	    const clSpaceCraftDescription &) ;

	// Destructor:
	~clSpaceCraftDescription () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class for a TimeSpanDescription:
class clTimeSpanDescription
    {
    private:
    public:
        TimeSpanDescription  cs ;

	// Constructor(s):
	clTimeSpanDescription () ;
	clTimeSpanDescription (TimeSpanDescription *tspan) ;
	clTimeSpanDescription (long  start_julian_day,
	    long  end_julian_day, double  start_time, double  end_time) ;
	clTimeSpanDescription (const clTimeSpanDescription &) ;

	// "Assignment" operator:
        clTimeSpanDescription& operator = (const clTimeSpanDescription &) ;

	// Destructor:
	~clTimeSpanDescription () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class for a DataQuantityIdentifier:
class clDataQuantityIdentifier
    {
    private:
    public:
        DataQuantityIdentifier  cs ;

	// Constructor(s):
	clDataQuantityIdentifier () ;
	clDataQuantityIdentifier (DataQuantityIdentifier *in_dqid) ;
	clDataQuantityIdentifier (char *qty_id, int32 source_id) ;
	clDataQuantityIdentifier (const clDataQuantityIdentifier &) ;

	// "Assignment" operator:
        clDataQuantityIdentifier& operator = (
	    const clDataQuantityIdentifier &) ;

	// Destructor:
	~clDataQuantityIdentifier () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class for a list of DataQuantityIdentifier's:
class clDataQuantityIdentifierList
    {
    private:
    public:
	DataQuantityIdentifierList    cs ;

	// Constructor(s):
	clDataQuantityIdentifierList () ;
	clDataQuantityIdentifierList (
	    DataQuantityIdentifierList *in_dqidl) ;
	clDataQuantityIdentifierList (int ndqids, int single_source,
	    char **qty_id, int32 *source_id) ;
	clDataQuantityIdentifierList (int ndqids,
	    DataQuantityIdentifier *in_dqids) ;
	clDataQuantityIdentifierList (int ndqids,
	    DataQuantityIdentifier **in_dqids) ;
	clDataQuantityIdentifierList (int ndqids,
	    clDataQuantityIdentifier *in_dqids) ;
	clDataQuantityIdentifierList (int ndqids,
	    clDataQuantityIdentifier **in_dqids) ;
	clDataQuantityIdentifierList (
	    const clDataQuantityIdentifierList &) ;

	// "Assignment" operator:
        clDataQuantityIdentifierList& operator = (
	    const clDataQuantityIdentifierList &) ;

	// Destructor:
	~clDataQuantityIdentifierList () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class for a ComponentDescription:
class clComponentDescription
    {
    private:
    public:
        ComponentDescription  cs ;

	// Constructor(s):
	clComponentDescription () ;
	clComponentDescription (ComponentDescription *in_cd) ;
	clComponentDescription (int type_id, int nrepeats,
	    ComponentSubDimensionList *subdiml) ;
	clComponentDescription (const clComponentDescription &) ;

	// "Assignment" operator:
        clComponentDescription& operator = (
	    const clComponentDescription &) ;

	// Destructor:
	~clComponentDescription () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class for a list of ComponentDescription's:
class clComponentDescriptionList
    {
    private:
    public:
	ComponentDescriptionList    cs ;

	// Constructor(s):
	clComponentDescriptionList () ;
	clComponentDescriptionList (ComponentDescriptionList *in_cdl) ;
	clComponentDescriptionList (int ncomps, int *type_id,
	    int *nrepeats, ComponentSubDimensionList *subdiml) ;
	clComponentDescriptionList (int ncd, ComponentDescription *in_cd) ;
	clComponentDescriptionList (int ncd, ComponentDescription **in_cd) ;
	clComponentDescriptionList (int ncd, clComponentDescription *in_cd) ;
	clComponentDescriptionList (int ncd,
	    clComponentDescription **in_cd) ;
	clComponentDescriptionList (const clComponentDescriptionList &) ;

	// "Assignment" operator:
        clComponentDescriptionList& operator = (
	    const clComponentDescriptionList &) ;

	// Destructor:
	~clComponentDescriptionList () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class for a DataQuantityDescription:
class clDataQuantityDescription
    {
    private:
    public:
        DataQuantityDescription  cs ;

	// Constructor(s):
	clDataQuantityDescription () ;
	clDataQuantityDescription (DataQuantityDescription *in_dqd) ;
	clDataQuantityDescription (char *in_name,
	    clSpaceCraftDescription *in_scraft, int32 in_sourceid,
	    int in_quantfmt, int in_quanttype, int in_deriv,
	    int16 in_nalgs, int16 in_calg,
	    clComponentDescriptionList *in_clist,
	    clDataQuantityIdentifierList *in_dqids) ;
	clDataQuantityDescription (const clDataQuantityDescription &) ;

	// "Assignment" operator:
        clDataQuantityDescription& operator = (
	    const clDataQuantityDescription &) ;

	// Destructor:
	~clDataQuantityDescription () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class for a list of DataQuantityDescription's:
class clDataQuantityDescriptionList
    {
    private:
    public:
	DataQuantityDescriptionList    cs ;

	// Constructor(s):
	clDataQuantityDescriptionList () ;
	clDataQuantityDescriptionList (
	    DataQuantityDescriptionList *in_dqdl) ;
	clDataQuantityDescriptionList (int nqtys, char **in_name,
	    int nspcraft, clSpaceCraftDescription *in_scraft,
	    int32 *in_sourceid, int *in_quantfmt, int *in_quanttype,
	    int *in_deriv, int16 *in_nalgs, int16 *in_calg,
	    clComponentDescriptionList **in_clist,
	    clDataQuantityIdentifierList **in_dqids) ;
	clDataQuantityDescriptionList (int ndqd,
	    DataQuantityDescription *in_dqd) ;
	clDataQuantityDescriptionList (int ndqd,
	    DataQuantityDescription **in_dqd) ;
	clDataQuantityDescriptionList (int ndqd,
	    clDataQuantityDescription *in_dqd) ;
	clDataQuantityDescriptionList (int ndqd,
	    clDataQuantityDescription **in_dqd) ;
	clDataQuantityDescriptionList (
	    const clDataQuantityDescriptionList &) ;

	// "Assignment" operator:
        clDataQuantityDescriptionList& operator = (
	    const clDataQuantityDescriptionList &) ;

	// Destructor:
	~clDataQuantityDescriptionList () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class for a ComponentUse:
class clComponentUse
    {
    private:
    public:
        ComponentUse  cs ;

	// Constructor(s):
	clComponentUse () ;
	clComponentUse (ComponentUse *in_cu) ;
	clComponentUse (int qty_idx, int comp_idx,
	   int idx_inside_comp, int assoc_idx) ;
	clComponentUse (const clComponentUse &) ;

	// "Assignment" operator:
        clComponentUse& operator = (const clComponentUse &) ;

	// Destructor:
	~clComponentUse () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class for a list of ComponentDescription's:
class clComponentUseList
    {
    private:
    public:
	ComponentUseList    cs ;

	// Constructor(s):
	clComponentUseList () ;
	clComponentUseList (ComponentUseList *in_cul) ;
	clComponentUseList (int nuses, int *qty_idx, int *comp_idx,
	    int *within_idx, int *assoc_idx) ;
	clComponentUseList (int ncu, ComponentUse *in_cu) ;
	clComponentUseList (int ncu, ComponentUse **in_cu) ;
	clComponentUseList (int ncu, clComponentUse *in_cu) ;
	clComponentUseList (int ncu, clComponentUse **in_cu) ;
	clComponentUseList (const clComponentUseList &) ;

	// "Assignment" operator:
        clComponentUseList& operator = (const clComponentUseList &) ;

	// Destructor:
	~clComponentUseList () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class for a PlotQuantityDescription:
class clPlotQuantityDescription
    {
    private:
    public:
        PlotQuantityDescription  cs ;

	// Constructor(s):
	clPlotQuantityDescription () ;
	clPlotQuantityDescription (PlotQuantityDescription *in_pqd) ;
	clPlotQuantityDescription (
            char *in_title, int in_ptype, int in_phu, int in_rstrct,
            DataQuantityDescriptionList *in_dqdl,
            AxisUnitList *in_axes, DrawingUnitList *in_drawl,
            ColorLine *in_yline, ColorLine *in_rline) ;
	clPlotQuantityDescription (const clPlotQuantityDescription &) ;

	// "Assignment" operator:
        clPlotQuantityDescription& operator = (
	    const clPlotQuantityDescription &) ;

	// Destructor:
	~clPlotQuantityDescription () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// Class for a list of ComponentDescription's:
class clPlotQuantityDescriptionList
    {
    private:
    public:
	PlotQuantityDescriptionList    cs ;

	// Constructor(s):
	clPlotQuantityDescriptionList () ;
	clPlotQuantityDescriptionList (
	    PlotQuantityDescriptionList *in_pqdl) ;
	clPlotQuantityDescriptionList (int nplots,
           char **in_tlist, int *in_ptype, int *in_phu, int *in_rstrct,
           DataQuantityDescriptionList *in_dqdl, AxisUnitList *in_axes,
           DrawingUnitList *in_drawl, ColorLine *in_yline,
	   ColorLine *in_rline) ;
	clPlotQuantityDescriptionList (int npqd,
	    PlotQuantityDescription *in_pqd) ;
	clPlotQuantityDescriptionList (int npqd,
	    PlotQuantityDescription **in_pqd) ;
	clPlotQuantityDescriptionList (int npqd,
	    clPlotQuantityDescription *in_pqdl) ;
	clPlotQuantityDescriptionList (int npqd,
	    clPlotQuantityDescription **in_pqdl) ;
	clPlotQuantityDescriptionList (
	   const clPlotQuantityDescriptionList &) ;

	// "Assignment" operator:
        clPlotQuantityDescriptionList& operator = (
	    const clPlotQuantityDescriptionList &) ;

	// Destructor:
	~clPlotQuantityDescriptionList () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;


// ------------------------------------------------------------------------
// Function Declarations:

#endif // CLDESCRIPTION_HH
