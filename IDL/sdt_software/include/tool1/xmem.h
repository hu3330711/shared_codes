/* xmem.h */

#ifndef XMEM_WAS_DECLARED
#define XMEM_WAS_DECLARED

/* For SCCS */
#define SccsId_xmem_h "@(#)xmem.h	1.1, 11/14/92"

/* used only on MS DOS machines with 640K limitations;  manages memory
   above 640K using BIOS calls Int15 Function 87 and 88.
   memory is managed in chunks of 64K.
   This program will conflict with any other programs that use extended
   memory, such as "virtual disks", caching, etc.
 */

extern  int XMEMavail(void);
   /* return number of 64K segments available in extended memory */

extern  int XMEMretrieve(void *dest,unsigned int words,int segment,unsigned int offset);
   /* retrieve info from extended memory 
    * dest = destination array
    * words = number of words to retrieve
    * segment = which extended segment to use (numbering starts at zero)
    *    call XMEMavail to see how many are available 
    * offset = byte offset in extended segment to start storing at
    */
   

extern  int XMEMstore(void *source, unsigned int words, int segment, unsigned int offset);
   /* store info into extended memory 
    * source = source array
    * words = number of words to store
    * segment = which extended segment to use (numbering starts at zero)
    *    call XMEMavail to see how many are available 
    * offset = byte offset in extended segment to start storing at
    */


#endif /* XMEM_WAS_DECLARED */
