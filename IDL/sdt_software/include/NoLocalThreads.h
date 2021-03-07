#ifndef NOLOCALTHREADS_H
#define NOLOCALTHREADS_H

/* For SCCS */
#define SccsId_NoLocalThreads_h "@(#)NoLocalThreads.h	1.3, 05/28/04"

#ifdef LINUX_A
#include <TDefsForLinux.h>
#endif

#include <LinkedList.hh>


// ----------------------------------------------------------------------
// Constants:

// ----------------------------------------------------------------------
// Typedefs:

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

typedef void * (*SimpleThreadFunc) (void *) ;
typedef void (*SimpleThreadDestructor) (void *) ;

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

// ----------------------------------------------------------------------
// Classes:

// ----------------------------------------------------------------------
// Holds a simple thread session.  Only mutex management and
// synchronization is required
class  SimpleThread : public ListObject
    {
    private:
    public:
	int         Id ;

	int         thread ;
	int         lock ;
	boolean_t   Quit ;

        // Constructor(s):
	SimpleThread () ;
	SimpleThread (const int in_Id) ;
        SimpleThread (const SimpleThread &in_instance) ;

        // Assignment Operator for a DataDirectoryObj:
        SimpleThread& operator = (const SimpleThread &in_instance) ;

        // Destructor:
	~SimpleThread () ;

	// Clear function:
	void clear () ;

	// Copy function:
	ListObject *copy () ;

	// Diagnostic print:
	void print (FILE *ofp) ;

	// Create and Run a thread:
	int RunThread (SimpleThreadFunc fptr, int &ecode) ;

        // Mutex lock and unlock:
	int Lock() ;
	int UnLock() ;

        // Various access routines:
	int GetId () ;

        int Getthread() ;

	int SetQuit (boolean_t bval) ;
	boolean_t GetQuit () ;
    } ;

// ----------------------------------------------------------------------
// For holding data directories - LinkedList:
class  SimpleThreadList : public ListClass
    {
    private:
    public:

	int   lock ;

	// The next available Id for a SimpleThread in this list:
	int   NextId ;


        // Use this to utilize a destructor for thread-specific
	// data:
	int   data_key ;

	// Pointer to specific-thread-data-destructor:
	// If NULL, then there is none.
	SimpleThreadDestructor dfnc ;

        // Constructor(s):
	SimpleThreadList () ;
	SimpleThreadList (SimpleThreadDestructor in_dfnc,
	    int FirstId) ;

        // Destructor:
	~SimpleThreadList () ;

	// compare:
	int compare (void *obj1, void *obj2) ;

	// Diagnostic print:
	void print (FILE *ofp) ;

        // Mutex lock and unlock this list:
	int Lock() ;
	int UnLock() ;

        // Return the next available SimpleThread Id for this list:
	int GetNextId() ;

        // Create and Add a new "SimpleThread" to this list:
	SimpleThread *NewSimpleThread () ;

	// Register thread-specific data for destruction:
	int RegisterSpecificThreadData (SimpleThread *sthr) ;

	// Sample thread-specific data destructor:
	void SampleDestructThreadSpecificData (void *in_vptr) ;
    } ;


// --------------------------------------------------------------------
// Utility Function Declarations:

#endif // NOLOCALTHREADS_H
// --------------------------------------------------------------------
