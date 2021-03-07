/*
 *  This file contains inclusions for the doubly-linked list class library.
*/
#ifndef SDTLINKEDLIST_H
#define SDTLINKEDLIST_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include <string.h>

/* For SCCS: */
#define SccsId_SDTLinkedList_h "@(#)SDTLinkedList.h	1.7, 02/20/04"

/* ---------------------------------------------------------------------- */
/* The following structure is a general, linked-list "node" which
 * includes a pointer to the object that belongs to the linked-list.
 * It makes no assumption as to the structure of the object itself.
 * We cannot put the linked-list fields in the objects themselves
 * because it can be a member of an arbitrary number of linked-lists. 
 * Therefore, we use this structure as sort of a general linked-list
 * "place-holder" for objects.
 */
struct  SDTListObj_struct
    {
    void      *object ;            /* Points to the object itself:  this
			            * can be a general sort of object
				    */
    struct SDTListObj_struct  *previous ; /* Points to previous entry in the list.
			            * (NULL if this is the first entry).
				    */
    struct SDTListObj_struct  *next ;     /* Points to the next entry in the list.
			            * (NULL if this is the last entry).
				    */
    } ;
typedef  struct  SDTListObj_struct  SDTListObj ;

/* ---------------------------------------------------------------------- */
/* This is the general linked-list structure. */
struct SDTListClass_struct
    {
    SDTListObj  *head ;   /* Head of the linked list. */
    SDTListObj  *tail ;   /* Tail of the linked list. */

    int       nobjects ;  /* The number of objects currently linked
			   * into the list.
			   */

    /* Function to "compare" two objects in the list.
     * Note that it the following return value:
     *    return <  0    ->  "obj1" is "less than" "obj2".
     *    return == 0    ->  "obj1" is "equal to" "obj2".
     *    return >  0    ->  "obj1" is "greater than" "obj2".
     */
    int (*compare)(void *obj1, void *obj2) ;
    } ;
typedef  struct  SDTListClass_struct  SDTListClass ;

/* Type declaration for comparison functions in the SDTListClass: */
typedef int (*SDTListClassCmpFnc) (void *, void *) ;

/* Functions: */

/* ---------------------------------------------------------------------- */
#ifdef ANSI_STD_C

extern SDTListObj *SDTConstructSDTListObj (void *obj) ;
extern int SDTDestructSDTListObj (SDTListObj *lptr) ;

extern SDTListClass *SDTConstructSDTListClass (
    int (*fptr)(void *, void *)) ;
extern int SDTDestructSDTListClass (SDTListClass *listp) ;
extern int SDTClearSDTListClass (SDTListClass *listp) ;
extern SDTListObj *SDTListClassFindObj (SDTListClass *lclass,
    void *obj) ;
extern SDTListObj *SDTListClassGetEntryByIndex (SDTListClass *lclass,
    int index) ;
extern int SDTListClassInsertObj (SDTListClass *lclass, void *obj) ;
extern int SDTListClassInsertObjAtIndex (SDTListClass *lclass,
    void *obj, int index) ;
extern int SDTListClassRemoveObj (SDTListClass *lclass, void *obj) ;
extern int SDTListClassRemoveObjAtIndex (SDTListClass *lclass,
int index) ;
extern int SDTListClassUnlinkObj (SDTListClass *lclass,
    SDTListObj *lptr) ;
extern int SDTListClassReInsertObj (SDTListClass *lclass,
   SDTListObj *lobj) ;

#else

extern SDTListObj *SDTConstructSDTListObj () ;
extern int SDTDestructSDTListObj () ;

extern SDTListClass *SDTConstructSDTListClass () ;
extern int SDTDestructSDTListClass () ;
extern int SDTClearSDTListClass () ;
extern SDTListObj *SDTListClassFindObj () ;
extern SDTListObj *SDTListClassGetEntryByIndex () ;
extern int SDTListClassInsertObj () ;
extern int SDTListClassInsertObjAtIndex () ;
extern int SDTListClassRemoveObj () ;
extern int SDTListClassRemoveObjAtIndex () ;
extern int SDTListClassUnlinkObj () ;
extern int SDTListClassReInsertObj () ;

#endif /* ANSI_STD_C */

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* SDTLINKEDLIST_H */
