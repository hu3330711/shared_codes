/* globals.h */

#ifndef GLOBALS_WAS_INCLUDED
#define GLOBALS_WAS_INCLUDED

/* For SCCS */
#define SccsId_globals_h "@(#)globals.h	1.1, 11/14/92"

#ifndef DEFboolean
#define DEFboolean
#define boolean int
#endif

#ifndef DEFBOOLEAN
#define DEFBOOLEAN
#define BOOLEAN int
#endif

#define hidden char
#define TRUE 1
#define FALSE 0
#ifndef NULL
#define NULL 0L
#endif
#define EOS 0

#define F0 (float) 0.0
#define F1 (float) 1.0
#define F5 (float) 0.5

#ifndef RADIAN
#define  RADIAN (float) 57.29577
#endif

#ifndef PI
#define  PI (float) 3.1415927
#endif

#ifndef DEFint16
#define DEFint16
#define int16  short
#endif

#ifndef DEFint32
#define DEFint32
#define int32  long
#endif


#define BIT0 1
#define BIT1 2
#define BIT2 4
#define BIT3 8
#define BIT4 16
#define BIT5 32
#define BIT6 64 
#define BIT7 128 

/* useful for defining library/module name only once */
#define Library(s) static char LibName[] =s
#define Module(s) static char ModName[] =s

#ifdef ANSI_STD_C
#ifdef SUN_A
/* "memmove" is supposed to be in the ANSI C library, but is NOT on the SUN. */
#define memmove(d, s, n) memcpy((char *) d, (char *) s, n)
#endif
#else
#define memmove(d, s, n) memcpy((char *) d, (char *) s, n)
#endif

/* useful macros */
#ifndef max
#define max(a,b)	(((a) > (b)) ?  (a) : (b))
#endif
#ifndef min
#define min(a,b)	(((a) < (b)) ?  (a) : (b))
#endif
#ifndef abs
#define abs(a)		(((a) < (0)) ? -(a) : (a))
#endif
#ifndef f_abs
#define f_abs(a)		(((a) < F0) ? -(a) : (a))
#endif
#ifndef d_abs
#define d_abs(a)		(((a) < (double) 0.0) ? -(a) : (a))
#endif


/********************************************************************/
/* Conversion Macros from blaise - 80x86 specific?                  */
/********************************************************************/

#define uthiword(a)   (((a)>>16)&0xffffL)   /* High word of long a. */
#define utloword(a)   ((a)&0xffffL)	    /* Low  word of long a. */

/* Combine high word a, low  word b.*/
#define utwdlong(a,b) ((((0xffffL&(long)(a)))<<16)|		     \
		       (0xffffL&(long)(b)))

#define uthibyte(a)   (((a)>>8)&0x00ff) /* High byte of word a.     */
#define utlobyte(a)   ((a)&0x00ff)	/* Low	byte of word a.     */

/* Combine high byte a, low byte b. */
#define utbyword(a,b) ((((a)&0x00ff)<<8)|((b)&0x00ff))

#define uthinyb(a)    (((a)>>4)&0x000f) /* High nybble of byte a.   */
#define utlonyb(a)    ((a)&0x000f)	/* Low nybble  of byte a.   */

/* Combine high nybble a, low	    */
/* nybble b.			    */
#define utnybbyt(a,b) ((((a)&0x000f)<<4)|((b)&0x000f))

/* Put byte in location pointed to.	    */
#ifdef DOS_OS
#define utpokeb(p,bval) (*((unsigned char far *) (p)) = (bval))
#else
#define utpokeb(p,bval) (*((unsigned char *) (p)) = (bval))
#endif /* DOS_OS */

/* test,set bit n */
#define utbitest(w,n) ((((w) >> (n)) & 1) == 1)
#define utbitset(w,n) (w |= (1 << (n)))

/********************************************************************/
/* Macros for pointer manipulation.	 80x86 specific                 */
/********************************************************************/

			/* Construct far void pointer.		    */
#ifdef DOS_OS

#define uttofaru(seg,off) ((void far *) 			     \
			   ((((unsigned long) (unsigned int)	     \
			     (seg)) << 16L) |			     \
			   ((unsigned long) (unsigned int) (off))))
#else

#define uttofaru(seg,off) ((void *) 			     \
			   ((((unsigned long) (unsigned int)	     \
			     (seg)) << 16L) |			     \
			   ((unsigned long) (unsigned int) (off))))
#endif /* DOS_OS */

			/* Compute offset  of memory pointed to.    */
#define utoff(p) ((unsigned int) (p))

			/* Compute segment of memory pointed to.    */
#ifdef DOS_OS
#define utseg(p) ((unsigned int)				     \
		  (((unsigned long) (void far *) (p)) >> 16L))
#else
#define utseg(p) ((unsigned int)				     \
		  (((unsigned long) (void *) (p)) >> 16L))
#endif /* DOS_OS */


			/* Make a far pointer to a type.	    */
#ifdef DOS_OS
#define uttofar(seg,off,type) ((type far *) uttofaru((seg),(off)))
#else
#define uttofar(seg,off,type) ((type *) uttofaru((seg),(off)))
#endif /* DOS_OS */

			/* Normalize a pointer. 		    */
#define utnorm(p,type) (uttofar (utseg (p) + (utoff (p) >> 4),	     \
				(utoff (p) & 0x000f),		     \
				type))

			/* Return 20-bit address in a pointer.	    */
#define utplong(p) ((((unsigned long) utseg (p)) << 4L) +	     \
		    ((unsigned long) utoff (p)) &		     \
		    0xfffffL)

#endif /* GLOBALS_WAS_INCLUDED */
