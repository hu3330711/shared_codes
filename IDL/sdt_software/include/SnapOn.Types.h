#ifndef SnapOn_Types_h
#define SnapOn_Types_h
#define SccsId_SnapOn_Types_h "@(#)SnapOn.Types.h	1.10, 12/03/06"
/* SnapOn.Types.h */

/* *************************************************************************
   this file includes types and defines used by the following files
   SnapOn.C++.cc
   SnapOn.ANSI-C.c
   ************************************************************************* */

struct ArrayInfo_struct
   {
   int nrows ;
   int ncols ;
   int nechs ;
   double delta_t ;
   int array_type ;
   void *array_ptr ;
   AddrT array_desc_id[3] ;
   } ;
typedef struct ArrayInfo_struct ArrayInfo ;

#define SO_UNUSED_ARRAY_INDEX -1

#endif /* SnapOn_Types_h */

