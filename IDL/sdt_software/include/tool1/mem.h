/* mem.h */

#ifndef MEM_WAS_INCLUDED
#define MEM_WAS_INCLUDED

/* For SCCS */
#define SccsId_mem_h "@(#)mem.h	1.1, 11/14/92"

/* MEM - cover for malloc routines.

   This routine does the following things:
      1) sends message to ERR if malloc fails
      2) allocates two extra bytes and places a user-supplied "structure id"
         at the end of the allocated memory. When the memory is freed, MEM
         checks if the id has been overwritten. If so, it sends a 
         message to ERR.  You can also call MEMcheck to look for overwritten
         memory.
      3) keeps track of maximum memory used, and how much memory was freed
         if some memory was not freed, an ERR message is sent. 
         MEMexit will optionally send ERR message about memory use.
 */

/* structure id constants for toolbox routines 
   other libraries should define their own list
 */

extern char   *MEMss;   /* see static string manager, below */


#ifdef ANSI_STD_C

extern  void MEMinit( void);
/* initialize MEM module */

extern  void MEMexit( boolean dump);
/* exit MEM module; optionally send message to ERR to output the
   maximum memory used */

/* allocate structure : use the macro forms as 
      sptr = MEMalloc( type, id)
      ...
      MEMfree( sptr, type, id)
 */
#define MEMalloc(tN,id)  MEMallocAry_(1, sizeof(tN), id)
#define MEMfree(sptr,tN,id)  MEMfreeAry_(sptr,1,sizeof(tN),id)
#define MEMcheck(sptr,tN,id)  MEMcheckAry_(sptr,1,sizeof(tN),id)

/* allocate array : use the macro forms as 
      aptr = MEMallocAry( size, type, id)
      ...
      MEMfreeAry( aptr, size, type, id)
 */
#define MEMallocAry(siz,tN,id)     MEMallocAry_(siz,sizeof(tN),id)
#define MEMfreeAry(ary,siz,tN,id)  MEMfreeAry_(ary,siz,sizeof(tN),id)
extern  void *MEMallocAry_(int nelem, unsigned typeSize, int16 id);
extern  void *MEMfreeAry_(void *ary, int nelem, unsigned typeSize, int16 id);
extern  boolean MEMcheckAry_(void *ary, int nelem, unsigned typeSize, int16 id);

/* call this routine to look for general memory damage;
   if print, send message to ERR if theres a problem;
   returns TRUE if ok */
extern boolean MEMcheckDamage( boolean print);


/* the following are a static string management package.
   initialize it with maximum bytes to store:
         MEMssInit( 1000);

   store strings like
         blah_int = MEMssStore("blah blah");

   retrieve 
         printf("string is %s", MEMss( blah_int));
         
 */
extern boolean MEMssInit( int size);
extern void MEMssExit( void);
extern int MEMssStore( char *s);

#else

extern  void MEMinit();
extern  void MEMexit();

#define MEMalloc(tN,id)  MEMallocAry_(1, sizeof(tN), id)
#define MEMfree(sptr,tN,id)  MEMfreeAry_(sptr,1,sizeof(tN),id)
#define MEMcheck(sptr,tN,id)  MEMcheckAry_(sptr,1,sizeof(tN),id)
#define MEMallocAry(siz,tN,id)     MEMallocAry_(siz,sizeof(tN),id)
#define MEMfreeAry(ary,siz,tN,id)  MEMfreeAry_(ary,siz,sizeof(tN),id)

extern  void *MEMallocAry_();
extern  void *MEMfreeAry_() ;
extern  boolean MEMcheckAry_();

extern boolean MEMcheckDamage();

extern boolean MEMssInit();
extern void MEMssExit();
extern int MEMssStore();

#endif /* ANSI_STD_C */

#endif /* MEM_WAS_INCLUDED */
