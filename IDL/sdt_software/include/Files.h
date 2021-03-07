#ifndef JCS_Files_h
#define JCS_Files_h
// Files.h

#define SccsId_Files_h "@(#)Files.h	1.9, 02/04/07"

#ifndef JCS_ERROR_LOG
#define JCS_ERROR_LOG "Files.log"
#endif /* JCS_ERROR_LOG */

#include <stdio.h>
#include <stdlib.h>
#include <Boolean.h>
#include <General.h>
#include <Error1.h>
#include <LinkList.h>

class Files
   {
private:
   char* FileName ;
   FILE* File ;
   BOOLEAN End ;
public:
   Files( char *filename) ;
   virtual ~Files() ;
   virtual Unit* GetEntry( void)
      { 
      LOG_IT( 1, (char *) "GetEntry() not defined for this File") ;
      return( (Unit *) NULL) ; 
      }
   BOOLEAN Eof() ;

   friend class SnapOnFile ;
   friend class DataFile ;
   friend class PlotFile ;
   friend class SourcesFile ;
   friend class ExecsFile ;
   friend class ConfigInfoFile ;
   friend class BatchInfoFile ;
   friend class LZFileFile ;
   friend class StarterExecsFile ;
   } ;

#endif  /* JCS_Files_h */

