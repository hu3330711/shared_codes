#ifndef JCS_LinkList_h
#define JCS_LinkList_h
// LinkList.h

#define SccsId_LinkList_h "@(#)LinkList.h	1.4, 02/04/07"

#ifndef JCS_ERROR_LOG
#define JCS_ERROR_LOG "LinkList.log"
#endif /* JCS_ERROR_LOG */

#ifdef OLD_STYLE_STR_HDRS
#include <iostream.h>
#else
#include <iostream>
using std::cout ;
using std::flush ;
using std::endl ;
using std::ostream ;
#endif

#include <Error1.h>

class Unit
   {
private:
   Unit* Prev ;
   Unit* Next ;
public:
   Unit( void) ;
   virtual ~Unit( void)
      { Prev = NULL ; Next = NULL; }
   virtual Unit *Copy( void)
      { return new Unit ; }
   virtual void Print( ostream &os)
      { os << "Print not defined for this unit type" ; }
   virtual void Put( char *buffer)
      { 
      if( buffer) ;
      LOG_IT( 1, (char *) "Put not defined for this unit type") ; 
      }
   friend class LinkList ;
   } ;

class LinkList
   {
private:
   Unit Mark ;
public:
   LinkList( void) ;
   LinkList( LinkList *ll) ;
   virtual ~LinkList( void) 
      { while( !IsEmpty()) delete Remove() ; }
   int IsEmpty( void) ;
   Unit* NextOne( void) ;
   Unit* NextOne( Unit* p) ;
   Unit* PrevOne( void) ;
   Unit* PrevOne( Unit* p) ;
   virtual void LinkList::Add( Unit* p)
      {
      p->Next = &Mark ;
      p->Prev = Mark.Prev ;
      Mark.Prev->Next = p ;
      Mark.Prev = p ;
      }
   virtual Unit* LinkList::Remove( void)
      {
      if( IsEmpty())
         {  
         LOG_IT( 1, (char *) "LinkList::Remove() on empty stack") ;
 	 return( NULL) ;
         }  
      else
         {
         Unit* toGo = Mark.Prev ;
         Mark.Prev = toGo->Prev ;
	 Mark.Prev->Next = &Mark ;
         return( toGo) ;
         }  
      }  
   virtual Unit* LinkList::Excise( Unit* p)
      {
      p->Prev->Next = p->Next ;
      p->Next->Prev = p->Prev ;
      return( p) ;
      }
   virtual Unit* LinkList::Insert( Unit* b, Unit *p)
      {
      if( b == NULL)
	 Add( p) ;
      else
	 {
         p->Prev = b->Prev ;
         b->Prev = p ;
         p->Prev->Next = p ;
         p->Next = b ;
	 }

      return( p) ;
      }
   virtual int LinkList::Count( void)
      {
      int count = 0 ;

      Unit *u = NextOne() ;
      while( u != NULL)
	 {
	 count++ ;
	 u = NextOne( u) ;
	 }

      return( count) ;
      }
   virtual Unit *LinkList::Get( int index)
      {
      Unit *u = NULL ;

      if( index >= 0)
         u = NextOne() ;

      while( index > 0)
	 {
	 index-- ;
	 u = NextOne( u) ;
	 }

      return( u) ;
      }
   virtual void LinkList::Print( ostream &os)
      {
      Unit *u = NextOne() ;

      while( u != NULL)
	 {
	 u->Print( os) ;
	 u = NextOne( u) ;
	 }

      os << flush ;
      }
   } ;

/**************************************************************************/
#endif /* JCS_LinkList_h */

