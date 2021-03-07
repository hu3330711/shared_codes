// clInstance.hh

// C++ SSL Data Tools Instances (Widgits) Declaration File:

#ifndef CLINSTANCE_HH
#define CLINSTANCE_HH

// SCCS ID string:
#define SccsId_clInstance_hh "@(#)clInstance.hh	1.5, 12/03/06"

#ifdef OLD_STYLE_STR_HDRS
#include <iostream.h>
#else
#include <iostream>
using std::cout ;
using std::flush ;
using std::endl ;
#endif

#ifdef OLD
#include <SDTDescription.h>
#include <SDTSpacecraft.h>
#include <SDTMemory.h>
#include <SDTInstance.h>
#endif // OLD

#include <clDescription.hh>
#include <clMemory.hh>
#include <SDTInstance.h>
#include <SDTSpacecraft.h>

// -------------------------------------------------------------------------
// Constants:

const int ByteAlignmentSize = 16 ;

// -------------------------------------------------------------------------
// Class of DataQuantityInstances:
//
class clDataQuantityInstance
    {
    private:
    public:
	DataQuantityInstance    cs ;

	// Constructor(s):
	clDataQuantityInstance () ;
	clDataQuantityInstance::clDataQuantityInstance (
	    DataQuantityInstance *in_dqi) ;
	clDataQuantityInstance::clDataQuantityInstance (
	    clDataQuantityDescription *in_dqi, clTimeSpanDescription *in_tspan,
	    clMemoryUnit *in_munit, clMemorySegment *in_mseg, void *in_memhdr,
	    clAddrTList *in_cmpoffs, clVoidList *in_data, int in_qflag,
	    clIntList *comp_qlt, char *qbits) ;
	clDataQuantityInstance::clDataQuantityInstance (
	    DataQuantityDescription *in_dqi, TimeSpanDescription *in_tspan,
	    MemoryUnit *in_munit, MemorySegment *in_mseg, void *in_memhdr,
	    AddrTList *in_cmpoffs, VoidList *in_data, int in_qflag,
	    IntList *comp_qlt, char *qbits) ;
	clDataQuantityInstance::clDataQuantityInstance (
	    const clDataQuantityInstance &in_instance) ;

	// "Assignment" operator:
	clDataQuantityInstance& clDataQuantityInstance::operator = (
	    const clDataQuantityInstance &in_instance) ;

	// Destructor:
	~clDataQuantityInstance () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;

        // Function to set the "MHeader" and "Data" addresses:
	int  clDataQuantityInstance::AssignDQIMemoryPointers () ;

        // Function to initialize QBit Storage:
	int  clDataQuantityInstance::InitializeQualityStorage () ;

        // Function to read a QBit:
	int  clDataQuantityInstance::ReadQualityBit (int rec, int comp) ;

        // Function to write a QBit:
	int  clDataQuantityInstance::WriteQualityBit (int rec,
		int comp, int val) ;
    } ;

// -------------------------------------------------------------------------
// Class for a list of DataQuantityInstance's:
//
class clDataQuantityInstanceList
    {
    private:
    public:
	DataQuantityInstanceList    cs ;

	// Constructor(s):
	clDataQuantityInstanceList () ;
	clDataQuantityInstanceList (DataQuantityInstanceList *in_dqil) ;
	clDataQuantityInstanceList (int nqtys, int ntspans,
	    clTimeSpanDescription *in_tspan,
	    clDataQuantityDescription *in_dqd,
	    int nmunits, clMemoryUnit *in_mdesc,
	    clMemorySegment *in_mseg, void **in_memhdr,
	    clAddrTList *in_coffs, clVoidList *in_data, int *qflag,
	    clIntList *comp_qlt, char **qbits) ;
	clDataQuantityInstanceList (int ndqd,
	    DataQuantityInstance *in_dqi) ;
	clDataQuantityInstanceList (int ndqd,
	    DataQuantityInstance **in_dqi) ;
	clDataQuantityInstanceList (int ndqd,
	    clDataQuantityInstance *in_dqi) ;
	clDataQuantityInstanceList (int ndqd,
	    clDataQuantityInstance **in_dqi) ;
	clDataQuantityInstanceList (const clDataQuantityInstanceList &) ;

	// "Assignment" operator:
        clDataQuantityInstanceList& operator = (
	    const clDataQuantityInstanceList &) ;

	// Destructor:
	~clDataQuantityInstanceList () ;

	// Clear internal storage:
	void clear () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// -------------------------------------------------------------------------
#ifdef PLOT_QUANTITY_INSTANCE_ELSEWHERE
/* Note that no-one but the UI needs to know about the PlotQuantityInstance. */
/* So we can and should get rid of it here. */
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

#endif // CLINSTANCE_HH

