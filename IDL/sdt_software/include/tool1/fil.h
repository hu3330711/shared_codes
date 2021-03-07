/* FIL.h  This is a cover for the file stream standard i/o routines.  
   
   The main reason for the cover is to centralize directory relocation.
   Secondarily it provides centralized debugging and error handling
*/

#ifndef FIL_WAS_INCLUDED
#define FIL_WAS_INCLUDED

/* For SCCS */
#define SccsId_fil_h "@(#)fil.h	1.1, 11/14/92"

#ifndef FILE
#include <stdio.h>
#endif

#ifdef ANSI_STD_C
#include <unistd.h>
#endif

/* file handle is a "hidden" type */
typedef hidden  FILhandle;

/* max size of a file name */
#define FIL_FNAME_MAX 100

/* directory types */
#define FIL_DIR_DEFAULT   0
#define FIL_DIR_DATA      1
#define FIL_DIR_AUX       2
#define FIL_DIR_AUX2      3

/* input to seek; "from" parameter */
#ifndef ANSI_STD_C

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#endif

#define FIL_SEEK_FROM_BEG               SEEK_SET
#define FIL_SEEK_FROM_CURRENT           SEEK_CUR
#define FIL_SEEK_FROM_END               SEEK_END

#ifdef ANSI_STD_C

/* most of these routines work exactly as their standard C library
 * counterparts, except that the type is FILhandle rather than FILE.
 * Exceptions are noted:
 */

extern  int  FILclose(FILhandle *file);
/* just like fclose; return 0 on success */

extern  char *FILdirAdd(int dtype,char *fname,char *result);
/* prepend one of the default directories to the given filename, if
   the filename does not start with a "\".
   dtype = FIL_DIR_DEFAULT, FIL_DATA, etc (see list above) */

extern  void FILdirSet(int dtype,char *dname);
/* set the default directory dtype to the given directory name. A trailing
   "\" is added if not present */

extern  char *FILgets(char *buffer,int size,FILhandle *stream,int remove_lf);
/* just like fgets, but optionally removes carraige return/line feeds.
   Return NULL on EOF or failure. Message to ERR on failure */

extern  FILhandle *FILopen(char *fname, char *open_type);
/* just like fopen, except prepends the default directory to the filename, 
   if the first char of the filename is not "\" ("/" for UNIX).
   for a different directory, call FILdirAdd, or prepend it yourself
 */

extern  int FILread(void *buffer,unsigned int size,unsigned int count,FILhandle *stream);
/* just like fread, but return TRUE on success, FALSE on EOF or failure. 
   Message to ERR on failure */

extern  int FILseek(FILhandle *stream, long offset, int from);
/* just like fseek; from = FIL_SEEK_FROM_BEG _CURRENT or _END */

extern  long FILtell(FILhandle *fileh);
/* just like ftell: get current offset; on error return -1 */

/* extern  boolean FILputs(FILhandle *file, char *buffer); */
/* just like fputs, but return TRUE on success. Message to ERR on failure */
#define FILputs( f, b)  FILwrite( b, strlen(b), 1, f)

extern  int FILwrite(void *buffer,unsigned int size,unsigned int count,FILhandle *stream);
/* just like fwrite, but return TRUE on success. Message to ERR on failure */

extern boolean FILopenDir( char *dirpath);
/* open the directory given by dirpath, in order to read the files
   using readDir. Return TRUE on success */

extern boolean FILreadDir( char *filename, int max_name);
/* read the next filename in the directory passed to FILopenDir.
   place the result in filename[ max_name].
   return TRUE on success, FALSE when no more to read */

extern char   *FILreadLine (char *buffer, int size, FILhandle *stream,
    boolean remove_lf, int *number_returned, int32 *location);
/* This routine uses cr/lf to delineate "lines" in an ascii file.
   It reads the requested number of characters (max 132), and then
   insures that the file is positioned at the start of the next
   line.  The number of characters returned in the output buffer
   is returned in "number_returned", the current location (after
   the read) is returned in "location" and the function returns
   NULL if the EOF was reached and no characters were read.
   It returns the pointer to the output buffer otherwise.
 */

extern FILE   *FILreturnPTR (FILhandle *stream);
/* this returns the standard c library FILE pointer of a FILhandle structure */

#else


extern  int  FILclose();
extern  char *FILdirAdd();
extern  void FILdirSet();
extern  char *FILgets();
extern  FILhandle *FILopen();
extern  int FILread();
extern  int FILseek();
extern  long FILtell();
extern  int FILwrite();
extern  boolean FILopenDir();
extern  boolean FILreadDIr();
extern  char   *FILreadLine ();
extern  FILE   *FILreturnPTR ();

/* extern  boolean FILputs();
/* just like fputs, but return TRUE on success. Message to ERR on failure */
#define FILputs( f, b)  FILwrite( b, strlen(b), 1, f)

#endif /* ANSI_STD_C */

#endif /* FIL_WAS_INCLUDED */
