/* dlm.h */

#ifndef DLM_WAS_INCLUDED
#define DLM_WAS_INCLUDED

/* For SCCS */
#define SccsId_dlm_h "@(#)dlm.h	1.1, 11/14/92"

/* DOUBLE LINKED LIST MANAGER - DLM

   This module implements double linked lists. The structure is:

      head
        v
      node1 ->  data1
        v
      node2 ->  data2
        v
        ...
      DLMnil -> DMLnil

    Usually the user does not need to manipulate nodes, but simply
    passes in and gets back pointers to the data.

    To use DLMdelete, or DLMdestroy, the data structures must be allocated
    by MEM, and given a "structure id", so that MEM can look for memory
    overwrites. If you dont want to use this, call DLMdestroyHead and
    DLMdestroyList when done with the list, and free the data structures
    yourself.

*/




/************* typedefs **************/
#ifdef ANSI_STD_C

typedef  boolean (*DLMorder_fn)(void *data1,void *data2);
   /* the order_fn must define a fully ordered relation such that
      it returns TRUE when data1 < data2 */

typedef  boolean (*DLMequal_fn)(void *data,void *param);
   /* the equal_fn must return TRUE when data equals the desired node.
      param is an arbitrary pointer used by equal_fn */

#else

typedef  boolean (*DLMorder_fn)();
typedef  boolean (*DLMequal_fn)();

#endif /* ANSI_STD_C */

typedef struct  DLMhead_ DLMhead;

struct  DLMnode_
   {
      void     *data;           /* ptr to data           */
      struct   DLMnode_ *prev;  /* ptr to previous node  */
      struct   DLMnode_ *next;  /* ptr to next node      */
      DLMhead  *list_id;        /* ptr to list this node belongs to */
   } ;
typedef struct  DLMnode_ DLMnode;

struct  DLMhead_
   {
      int16    length;       /* number of nodes in list. */
      DLMnode  *first,       /* pointer to first node        */
               *last,        /* pointer to last node         */
               *current;     /* pointer to current node      */
      int16    status;       /* traversal status */
   } ;


/*********** exported variables *********/
extern  DLMnode  *DLMnil;
   /* node == DLMnil at end of list */


/********** exported routines ***********/

#ifdef ANSI_STD_C

extern  DLMnode *DLMaddAfter(DLMhead *list,void *data);
   /*  Create a node, put the specified data in it, and insert it after the 
       current node 
       This makes the new node the current node.
    */

extern  DLMnode *DLMappend(DLMhead *list, void *data);
   /*  Create a node, put the specified data in it, and append it to the
       specified list.  
       This makes the new node the current node.
    */


extern  DLMhead *DLMclone(DLMhead *original);
  /*
    Copy an existing List Header, and return a pointer to the
    new List Header
    This allows independent traversal of the same list
    It is assumed that the list doesnt change during this traversal
    Use DLMdestroyHead when you're done

   e.g:
            read through the list
         clone = DLMclone( Flag_list);

         DLMinitGet( clone);
         while ( DLMgetNext( clone, (anytype **) &data) != DLMnil)
            {
            process data 
            }

         DLMdestroyHead( clone);

  */

extern  DLMhead *DLMcreateList(void);
   /* initialize an empty list.
      Return NULL on error */


extern  DLMnode *DLMdelete(DLMhead *list,DLMnode *node,unsigned size,int16 struct_id);
/*
   Unlink and delete the specified node and free the memory
   associated with the data.
        The deletion does NOT affect the currency of the list UNLESS 
   the node to be deleted is the current node, in which case the
   current node becomes the previous node.

   size, struct_id is for MEM processing (see MEMfree() for more details)
 */


extern  DLMhead *DLMdestroy(DLMhead *list, unsigned size, int16 struct_id);
/*
   Unconditionally destroy a list and release ALL the resources
   associated with it including the data pointed to by the link nodes.

   size, struct_id is for MEM processing (see MEMfree() for more details)
 */


extern  void DLMdestroyHead(DLMhead *list);
  /*  Destroy a list header and release its memory. use with DLMclone */


extern  void DLMdestroyList(DLMhead *list);
   /* free memory associated with list header and nodes;
      caller must delete the data structures */


extern  void DLMinit(void);
   /* call once at the start of the program, before any other calls to DLM
      called by TOOL1init() */

extern  void     DLMinitGet(DLMhead *list);
extern  DLMnode *DLMgetNext(DLMhead *list,void * *data);

extern  void     DLMinitGetBackwards(DLMhead *list);
extern  DLMnode *DLMgetPrev(DLMhead *list,void * *data);

  /*
   These functions are for traversing the linked list. The initGet
   and initGetBackwards initialize the traversal; they do not fetch any data.
   The calls to getNext and getPrev do the actual traversal.
        Returns a pointer to the current node. Also return a pointer to the 
   data of the current node. 
        DLMnil is returned if the list is at the end. In that case
   the data pointer is not affected, and the current node is left 
   on the last/first node.

   Example:

           forward traversal
        DLMinitGet(list);
        while ( DLMgetNext(list, &data) != DLMnil)
           {
           manipulate data record
           }

           backward traversal, with use of node pointer 
        DLMinitGetBackwards(list);
        while ( (node = DLMgetPrev(list, &data)) != DLMnil)
           {
           manipulate data record and node
           }

  */


extern  DLMnode *DLMgetCurrent(DLMhead *list,void * *data);
   /*  Fetch the data/node without changing the currency. */


extern  DLMnode *DLMinsert(DLMhead *list,void *data,DLMorder_fn order_fn);
  /*   Create a node, put the specified data in it, and insert it in the 
       list determined by the ordering function order_fn (see above DLMorder_fn).
       The list must already be in that order.
       This makes the new node the current node.

   e.g.:
      boolean LessTh( Flagstruct *data1, Flagstruct *data2)
         {
         return (data1->flag < data2->flag);
         }

      Flagstruct *this;

      this = MEMalloc( Flagstruct);
      this->flag = 12;
      DLMinsert(Flag_list, &this, (DLMorder_fn) LessTh);
   */

extern  DLMnode *DLMsearch(DLMhead *list,void * *data,
                  DLMequal_fn equal_fn, void *param);
extern  DLMnode *DLMsearchC(DLMhead *list,void * *data,
                  DLMequal_fn equal_fn, void *param);

   /* Search through the list until you find the node, specified by the 
   search function.  Return NULL if not found.
      For DLMsearchC, the found node becomes current; for DLMsearch, the 
   currency of the list is not affected.
      The search function is passed the data and the param, and it must 
   return true when the node is found (see above def of DLMequal_fn).

   e.g.:
      boolean ChkEqFlag( Flagstruct *data, int *want)
         {
         return (data->flag == *want);
         }

      Flagstruct *this;
      want_flag = 12;
      DLMsearch(Flag_list, &this, (DLMequal_fn) ChkEqFlag, &want_flag);

   */


extern  void DLMsetCurrent(DLMhead *list,DLMnode *node);
   /*  set the current node */


/*** these three functions implement a stack */

extern  DLMnode *DLMpush(DLMhead *list, void *data);
/* Create a node and prepend it to the specified list.
   Prepending makes the first node the current node.
*/

extern  DLMnode *DLMpop(DLMhead *list, unsigned size, int16 struct_id);
/* Delete the first node and free the memory associated with the data.
   the following node becomes the current node, and is returned
 */

extern  DLMnode *DLMpromote(DLMhead *list, void *this_data);
/* Find the node whose data is ppointed to by "this_data", 
 * make it the first node
 * it also becomes the current node and is returned 
 */

void DLMdump( DLMhead *head);
/* debugging dump of head and nodes addresses */

#else

extern  DLMnode *DLMaddAfter();
extern  DLMnode *DLMappend();
extern  DLMhead *DLMclone();
extern  DLMhead *DLMcreateList();
extern  DLMnode *DLMdelete();
extern  DLMhead *DLMdestroy();
extern  void DLMdestroyHead();
extern  void DLMdestroyList();
extern  void DLMinit();
extern  void     DLMinitGet();
extern  DLMnode *DLMgetNext();
extern  void     DLMinitGetBackwards() ;
extern  DLMnode *DLMgetPrev();
extern  DLMnode *DLMgetCurrent();
extern  DLMnode *DLMinsert(); 
extern  DLMnode *DLMsearch();
extern  DLMnode *DLMsearchC();
extern  void DLMsetCurrent();
extern  DLMnode *DLMpush();
extern  DLMnode *DLMpop();
extern  DLMnode *DLMpromote();
void DLMdump();

#endif /* ANSI_STD_C */

#endif /* DLM_WAS_INCLUDED */
