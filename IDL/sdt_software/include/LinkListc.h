#ifndef LINKLIST_H
#define LINKLIST_H

/* every housekeeping apid has a linked list of the following structures: 
 * (with any luck this structure might have generic usefulness) */
struct LinkList_struct
    {
      void *data ;
      struct LinkList_struct *prev ;
      struct LinkList_struct *next ;
    } ;
typedef struct LinkList_struct LinkList ;

/* LinkList Constructor/Destructor */
LinkList *ConstructLinkList (void) ;
void DestructLinkList (LinkList *l) ;

/* LinkListEntry Constructor/Destructor */
LinkList *ConstructLinkListEntry (LinkList **lEntryPtr) ;
int DestructLinkListEntry (LinkList **lPtr, LinkList *lEntry) ;

LinkList *InsertLinkListEntry (LinkList **lPtr, LinkList *lEntry) ;

#endif /* LINKLIST_H */
