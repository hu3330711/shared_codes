/* xmm.h */

#ifndef XMM_WAS_DECLARED
#define XMM_WAS_DECLARED

/* For SCCS */
#define SccsId_xmm_h "@(#)xmm.h	1.1, 11/14/92"

/* this is an "extended memory management" module.  It is used to manage
   multiple arbitrarily long arrays of data; it is not known beforehand how 
   long any of the arrays will be.  It uses malloc on UNIX machines and XMEM 
   on DOS machines; it uses all available memory if need be.

   There are a maximum of num_chan arrays of data.
   XMMstore is used to store data into the arrays. 
   XMMmark will mark the positions of all the arrays with a time tag;
   XMMretrieve will retrieve all the data starting at a given mark,
   up until the next mark was made.
 */

 /* hidden data type */
#define XMMretStruct char

#ifdef ANSI_STD_C
extern  int XMMinit(int num_chan);
   /* initialize XMM to store num_chan arrays of data */

extern  void XMMexit(void);
   /* free XMM memory */

extern boolean  XMMreinit();
  /* throw away everything, start again */


extern  int XMMmark(long time);
   /* mark the current positions of all of the data arrays at this time.
      All data stored is part of this time mark until another mark
      or no more data
    */

extern  int XMMstore(void *data,int32 bytes,int chan);
   /* store words # data points for this channel */

extern  boolean XMMsetMark( boolean forward);
   /* set the current mark one forward or backward.
      return TRUE if success; FALSE if no more marks.
   */

extern  XMMretStruct *XMMretrieveInit(void *data,long max_bytes,int chan,long time);
extern  int XMMretrieve( XMMretStruct *rets,long *read_bytes,int *more);
extern  void XMMretrieveExit( XMMretStruct *rets);

   /* retrieve all of the data for this channel at the given time mark.
      time = -1 means use the current mark. Set the current mark to the
      given time mark.
      Place data in data array, maximum max_words at a time. put number of
      words that were placed there in read_words. 
      more of the same data. more is set to TRUE if theres more to go.
      Typical calling sequence:

            char           data[ MAX_DATA];
            boolean        again;
            long           time_mark;
            int            chan;
            XMMretStruct   *rets;

            rets = XMMretrieveInit( data, MAX_DATA, chan, time_mark);
            do
               {
               XMMretrieve( rets, &nread, &again);
               <process nread elements of data>
               }
            while (again);
            XMMexit( rets);
   
   */

extern  boolean XMMstoreMods( XMMretStruct *rets);
   /* write modified data back into the XMM arrays.
      only affects the last buffer from XMMretrieve

      Example:

            rets = XMMretrieveInit( data, MAX_DATA, chan, time_mark);
            do
               {
               XMMretrieve( rets, &nread, &again);

               <process nread elements of data>
               XMMstoreMods( rets);
               }
            while (again);
            XMMexit( rets);
   */      

extern void XMMresetChan( int chan);
/* reset the data storage of this channel back to the current mark;
   in effect, "throw out" any thing that has been stored since the last mark.
 */


#else

extern  int XMMinit();
extern  int XMMmark();
extern  int XMMstore();
extern  boolean XMMsetMark();
extern  XMMretStruct *XMMretrieveInit();
extern  int XMMretrieve();
extern  void XMMretrieveExit();
extern  void XMMexit();
extern boolean  XMMreinit();
extern void XMMresetChan( );

#endif

#endif /* XMM_WAS_DECLARED */
