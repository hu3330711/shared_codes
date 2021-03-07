//
//  This file contains inclusions for the doubly-linked list class library.
//
#ifndef LINKEDLIST_HH
#define LINKEDLIST_HH

#include <string.h>

// For SCCS:
#define SccsId_LinkedList_hh "@(#)LinkedList.hh	1.2, 07/05/00"

// ----------------------------------------------------------------------
// The following class is a general, linked-list "node".
//
// It is assumed that this will be used as a base class for objects with
// more definition.
//
class  ListObject
    {
    private:
        ListObject  *previous ;   // Points to previous entry in the list.
			          // (NULL if this is the first entry).
        ListObject  *next ;       // Points to the next entry in the list.
			          // (NULL if this is the last entry).
    public:
	// Constructor(s)
        ListObject () ;
        ListObject (const ListObject &in_instance) ;

        // "Assignment" operator:
        ListObject& operator = (const ListObject&) ;

	// Destructor (NOTE that it is virtual because we assume we will
	// use this as a base class):
	virtual ~ListObject() ;      // Destructor (empty)

	// We need the "copy" command (much like Winder's "Stack", p. 260)
	// and it should be virtual:
	virtual ListObject * copy() ;

	// This returns the "previous" member.
	inline ListObject *get_previous () const
	    {
	    return (previous) ;
	    } ;

	// This returns the "next" member.
	inline ListObject *get_next () const
	    {
	    return (next) ;
	    } ;

	// This sets the "previous" member.
	inline void set_previous (ListObject *l)
	    {
	    previous = l ;
	    } ;

	// This sets the "next" member.
	inline void set_next (ListObject *l)
	    {
	    next = l ;
	    } ;
    } ;

// ----------------------------------------------------------------------
// This is the general linked-list class.
class ListClass
    {
    private:
	ListObject  *head ;     // Head of the linked list
	ListObject  *tail ;     // Tail of the linked list

	int       nobjects ;  // The number of objects currently linked
			      // into the list.

	// Used to unlink nodes:
	int unlink_node (ListObject *lptr) ;

    public:
         // Constructor to initialize.  Note that it sets the head and
	 // tail pointers to NULL.
         ListClass () ;

         // Copy Constructor:
	 ListClass (const ListClass &in_instance) ;

	 // Assignment Operator for a list class:
	 ListClass& ListClass::operator = (const ListClass &in_instance) ;

	 // Destructor (NOTE that it is virtual because we assume we will
	 // use this as a base class):
	 virtual ~ListClass() ;

         // This clears a statically-defined ListClass:
	 virtual void clear() ;

	 // This is the "compare" or "order" function for the objects of
	 // the class.  It is also virtual as it is assumed that the
	 // derived class will define its own.
	 // Note that it sends the following return value:
	 //    return <  0    ->  "obj1" is "less than" "obj2".
	 //    return == 0    ->  "obj1" is "equal to" "obj2".
	 //    return >  0    ->  "obj1" is "greater than" "obj2".
	 virtual int compare(void *obj1, void *obj2) ;

	 // Initialize private variables - mostly for the use of
	 // Derived classes:
	 void  initialize_private () ;

	 // Return the head of the list.
	 inline ListObject *get_head () const
	     {
	     return (head) ;
	     } ;

	 // Return the tail of the list.
	 inline ListObject *get_tail () const
	     {
	     return (tail) ;
	     } ;

         // Function to locate a node within a list of
	 // objects.  It returns a pointer to the node in the list
	 // if found and NULL otherwise.
         ListObject *find (ListObject *) ;

         // Function to append an object to the linked list of
	 // objects.  It returns 1 if successful, else zero.
         int add (ListObject *) ;

         // Same as "add" except it appends whether or not the object
	 // is already in the list.
         int append (ListObject *) ;

         // Function to link an object into the linked list of
	 // objects, positioning it at the position that the "compare"
	 // function would place it.
         int insert (ListObject *obj) ;

         // Function to link an object into the linked list of
	 // objects, positioning it at the position that the "compare"
	 // function would place it.  It differs from "insert" only
	 // in that there is an extra argument "idx" in which, if
	 // "obj" is successfully inserted, the index of insertion
	 // is returned.
         int insert1 (ListObject *obj, int *idx) ;

         // Function to link an object into the linked list of
	 // objects, making it entry "index".  It returns 1 if successful,
	 // else zero.
         int insert_at_index (ListObject *obj, int index) ;

         // Returns the number of objects currently in the list:
         inline int size ()
	     {
	     return (nobjects) ;
	     } ;

	 // This returns a pointer to the "index'th" object in the list:
	 ListObject *entry_at_index (int index) ;

	 // This returns a copy of the "index'th" object in the list:
	 ListObject *copy_entry_at_index (int index) ;

         // Function to find and unlink a "ListObject" from the linked list
	 // and deallocate the storage of the node:
	 // It returns 1 if successful, else zero.
         int subtract (ListObject *obj) ;

         // Function to unlink the "index'th" object from the linked list
	 // and deallocate the storage of the node:
	 int subtract_entry_at_index (int index) ;
    } ;

#endif // LINKEDLIST_HH
