#ifndef DQILLIST_H
#define DQILLIST_H

// For SCCS:
#define SccsId_DQILList_h "@(#)DQILList.h	1.2, 10/28/94"

// --------------------------------------------------------------------------
// Derived class (from ListObject) for storing a list of DQI's:
//
class  DQILListNode : public ListObject
    {
    private:
    public:
	// Stores the DQI for this node
	DataQuantityInstance   DQI ;

        // Constructor(s):
	DQILListNode () ;
        DQILListNode (DataQuantityInstance *in_dqi) ;
	DQILListNode (const DQILListNode &in_instance) ;

	// Assignment Operator for a DQILListNode:
	DQILListNode& operator = (const DQILListNode &in_instance) ;

        // Destructor:
	~DQILListNode () ;

        // Clears out space:
	void clear () ;

	// "copy" function:
	ListObject *copy () ;

	// Print:
	void print (FILE *ofp) ;
    } ;

// --------------------------------------------------------------------------
// Linked-list of DQILListNode:
//
class  DQILList : public ListClass
    {
    private:
    public:
	// Constructor(s):
	DQILList () ;

	// Note that we do NOT need an initializer-constructor or
	// assignment = operator.  The ListClass versions will work
	// since we have not added any data fields.

	// Destructor:
	~DQILList () ;

	// The "compare" function:  (inputs are two DQILListNode ptrs):
	int compare (void *obj1, void *obj2) ;

	// "print" function:
	void print (FILE *ofp) ;

	// "clear" function:
        void clear (void) ;

        // Detach all the DQI's in the list from shared-memory:
        int  DetachFromMemory () ;
    } ;

// --------------------------------------------------------------------
// Utility Function Declarations:

int CompareDQDandTimeSpans (TimeSpanDescription *tsp1,
        DataQuantityDescription *dqd1, TimeSpanDescription *tsp2,
        DataQuantityDescription *dqd2) ;

int CheckTimeSpanSubset (TimeSpanDescription *tsp1,
	TimeSpanDescription *tsp2) ;

#endif // DQILLIST_H
// --------------------------------------------------------------------
