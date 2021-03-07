/*
 * This file contains the header file for the blocked, circular, shared
 * memory library MTShmCirLib.c. See comments in that file.
 */

#ifndef _MTSHMCIRBUF_
#define _MTSHMCIRBUF_

#ifdef LINUX_A
#ifndef _MTSHMCIRBUFUNISTD__
#define _MTSHMCIRBUFUNISTD__
#include "TDefsForLinux.h"
#endif  /* _MTSHMCIRBUFUNISTD__ */
#endif  /* LINUX_A */

#ifndef _MTSHMCIRBUFUNISTD__
#define _MTSHMCIRBUFUNISTD__
#include <unistd.h>
#endif  /* _MTSHMCIRBUFUNISTD__ */

#ifndef _MTSHMCIRBUFTYPES_
#define _MTSHMCIRBUFTYPES_
#include <sys/types.h>
#endif  /* _MTSHMCIRBUFTYPES_ */

#ifndef _MTSHMCIRBUFIPC_
#define _MTSHMCIRBUFIPC_
#include <sys/ipc.h>
#endif  /* _MTSHMCIRBUFIPC_ */

#ifndef _MTSHMCIRBUFSHM_
#define _MTSHMCIRBUFSHM_
#include <sys/shm.h>
#endif  /* _MTSHMCIRBUFSHM_ */

#ifndef _MTSHMCIRBUFMEMORY_
#define _MTSHMCIRBUFMEMORY_
#include <memory.h>
#endif  /* _MTSHMCIRBUFMEMORY_ */

#ifndef _MTSHMCIRBUFTHREADH_
#define _MTSHMCIRBUFTHREADH_
#ifdef UTILIZE_SUN_THREADS
#include <thread.h>
#else
#include <pthread.h>
#endif
#endif  /* _MTSHMCIRBUFTHREADH_ */

#ifndef LINUX_A
#ifndef _MTSHMCIRBUFSYNCHH_
#define _MTSHMCIRBUFSYNCHH_
#include <synch.h>
#endif  /* _MTSHMCIRBUFSYNCHH_ */
#endif  /* NOT LINUX_A */

#ifndef _MTSHMCIRBUFSIGNALH_
#define _MTSHMCIRBUFSIGNALH_
#include <signal.h>
#endif  /* _MTSHMCIRBUFSIGNALH_ */

#ifndef _MTSHMCIRBUFERRNOH_
#define _MTSHMCIRBUFERRNOH_
#include <errno.h>
#endif  /* _MTSHMCIRBUFERRNOH_ */

#ifndef _MTSHMCIRBUFERROR_
#define _MTSHMCIRBUFERROR_
#include "error.h"
#endif  /* _MTSHMCIRBUFERROR_ */

/* shm permissions og+rw (0666) */

#define PERMS 0666

/* magic number that flags shm was pre-allocated */

#define PRE_EXISTED ~0

#ifdef	__cplusplus
extern "C" {
#endif

/*-----------------------------------------------------------------------------
 * A useful boolean type: use enum boolean (boolean_t) from <sys/types.h>
 */

/*------------------------------------------------------------------- */
/* Note that only Solaris 2.7 seems to support the POSIX
 * version of read/write locking.  Solaris 2.5 and 2.6, as
 * well as RedHat Linux 5.2 do not make it available in pthreads.
 * Until we have rwlock's available for pthreads, we simply typedef
 * to pthread_mutex_t which will help make things compile but
 * we won't be able to use FAST realtime in the meantime.
 * JBV 99/06/15
 */
#ifdef UTILIZE_SUN_THREADS
#else
  typedef pthread_mutex_t       rwlock_t ;
#endif

/*-----------------------------------------------------------------------------
 * MTBlkShmRWData contains read and write locations for one thread
 */

struct MTBlkShmRWData
{
  int 	currentWriteBlock ;
  int 	currentReadBlock ;
  int 	MTShmId ;
} ;


/*-----------------------------------------------------------------------------
 * MTBlkShmStatus contains to the layout of the shared memory buffer.
 */


struct MTBlkShmStatus
{
  int                     blockSize ;
  int                     numberBlocks ;
  int                     numberThreads ;
  int                     lastWriteThread ;
  int                     maxThreads ;
  int                     dataStartOffset ;
  boolean_t               quitFlag ;
  int			  existFlag ;
#ifdef UTILIZE_SUN_THREADS
  mutex_t		  hskLock ;
#else
  pthread_mutex_t   	  hskLock ;
  pthread_mutexattr_t  	  hskLock_attr ;
#endif
} ;


/*-----------------------------------------------------------------------------
 * MTShmId is the shared memory buffer id for a given thread.
 */

struct MTShmId
{
  int 				id ;
  int 				threadNumber ;
  struct MTBlkShmStatus * 	statPtr ;
#ifdef UTILIZE_SUN_THREADS
  rwlock_t *			rwPtr ;
#else
  rwlock_t *			rwPtr ;
#endif
} ;



/*****************************************************************************
 * Attach to a shared memory segment, initializing, if necessary.
 */

struct MTShmId MTBShmGet(key_t shmKey, int blockSize, int nBlocks, 
			 int maxThreads, int flags);

/*****************************************************************************
 * Detach from our shared memory segment.  if this is the last reference,
 * remove it from the system.
 */

int MTBShmDetach(struct MTShmId id) ;

/*****************************************************************************
 * Force shared memory buffer to be removed from the system.
 */

int MTBShmRemove(struct MTShmId id) ;

/*****************************************************************************
 * Read from shared memory into a buffer from the current read location for
 * this thread.
 */

int MTBShmReadBlock(struct MTShmId id, char *readToBuf, int pointToNext) ;

/*****************************************************************************
 * Write from a buffer into shared memory at this current write location
 * for this thread.
 */

int MTBShmWriteBlock(struct MTShmId id, char * writeToBuf, int pointToNext) ;

/*****************************************************************************
 * Write multiple buffers to shared memory from an array of bufpointer 
 * locations starting at the current write location for this thread.
 */
 
int MTBShmWriteMultBlocks(struct MTShmId id, char * writeToBufs[], 
		       int numToWrite, int pointToNext) ;

/*****************************************************************************
 * get the pointer to the shared memory block for the current read or write 
 * location for this thread.
 */

char * MTBShmGetBlockPtr (struct MTShmId id, int pointToNext,
		       boolean_t Writer, rwlock_t ** blockLock) ;

/*****************************************************************************
 * Seek the current read index for this thread.  This will be delta 
 * relative to the current position.
 */

int MTBShmMoveCurRead(struct MTShmId id, int moveBy) ;

/*****************************************************************************
 * Seek the current write index for this thread.  This will be delta 
 * relative to the current position.
 */

int MTBShmMoveCurWrite(struct MTShmId id, int moveBy) ;

/*****************************************************************************
 * set the current write block for the specified thread to setTo.
 */

int MTBShmSetWriteForThread(struct MTShmId id, int threadNumber, 
			    int setTo) ;

/*****************************************************************************
 * Get the current write index for this thread.  
 */

int MTBShmGetCurWriteBlock(struct MTShmId id) ;

/*****************************************************************************
 * Get the current read index for this thread.  
 */

int MTBShmGetCurReadBlock(struct MTShmId id) ;

/*****************************************************************************
 * Get the number of thread attatched to the shared memory buffer.  
 */

int MTBShmGetNumThreads(struct MTShmId id) ;

/*****************************************************************************
 * Get the index of the last thread to write shm.
 */

int MTBShmGetLastWriteThread(struct MTShmId id) ;

/*****************************************************************************
 * Set the index of the last thread to write shm.  
 */

int  MTBShmSetWriteThread (struct MTShmId, int threadNumber) ;

/*****************************************************************************
 * Get the current read block for the thread with index given.
 */

int MTBShmGetReadBlockForThread(struct MTShmId id, int threadIndex) ;

/*****************************************************************************
 * Get the current write block for the thread with index given.
 */

int MTBShmGetWriteBlockForThread(struct MTShmId id, int threadIndex) ;

/*****************************************************************************
 * Set quit flag in status area
 */

int MTBShmSignalQuit(struct MTShmId id, boolean_t quitFlag) ;

/*****************************************************************************
 * Get the quit status 
 */

boolean_t MTBShmGetQuitStat(struct MTShmId id);

#ifdef	__cplusplus
}
#endif

#endif    /* _MTSHMCIRBUF_ */
