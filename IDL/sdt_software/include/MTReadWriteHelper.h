/*
 * This file contains the header file for the shared memory helper library
 * MTReadWriteHelper.c. See comments in that file.
 */

#ifndef _MTRWHLPH_
#define _MTRWHLPH_


#ifndef _MTRWSHMCIRBUF_
#define _MTRWSHMCIRBUF_
#include "MTShmCirBuf.h"
#endif    /*  _MTRWSHMCIRBUF_ */

/* return values from MTRWWaitForBlocks () */

#define MTRW_BLOCK_READY  0  /* a block has become ready */
#define MTRW_RELEASED     1  /* lock was released by MTRWReleaseThread() */
#define MTRW_FAILED      -1  /* the call caused an error. */

#ifdef	__cplusplus
extern "C" {
#endif

/*-----------------------------------------------------------------------------
 * RWType enumeration, distiguishes reader and writer threads.
 */

enum RWType {WRITER = 0, READER = 1} ;


/*-----------------------------------------------------------------------------
 * MTRWLocStat is used to hold the lag counter and it's access locks, one for 
 * each thead.
 */

struct MTRWLocStat
{
#ifdef UTILIZE_SUN_THREADS
  mutex_t  	lagMutex ;
  cond_t  	condition ;
#else
  pthread_mutex_t  lagMutex ;
  pthread_mutexattr_t  lagMutex_attr ;
  pthread_cond_t   condition ;
  pthread_condattr_t  condition_attr ;
#endif
  int 		adjusts ;
  int 		lag ;
  boolean_t	used ;
  boolean_t	released ;
#ifdef UTILIZE_SUN_THREADS
  cond_t	failSafeCond ;
#else
  pthread_cond_t   failSafeCond ;
  pthread_condattr_t  failSafeCond_attr ;
#endif
  boolean_t	failSafeIgnore ;
} ;


/*-----------------------------------------------------------------------------
 * MTRWShmStatus is the stucture at the begining of shm  and holds
 * the informatin about the status of the buffer.
 */

struct MTRWShmStatus
{
  int			numberBlocks ;
  int			numberReaders ;
  int			numberThreads ;
  int			maxReaders ;
  boolean_t		haveWriter ;
  int			writerCirBufThread ;
  int			existFlag ;
#ifdef UTILIZE_SUN_THREADS
  mutex_t		hskLock ;
#else
  pthread_mutex_t       hskLock ;
  pthread_mutexattr_t   hskLock_attr ;
#endif
  boolean_t		failSafeMode ;
} ;


/*-----------------------------------------------------------------------------
 * MTRWManageId is the id structure that each thread used to access the 
 * libary.
 */

struct MTRWManageId
{
  int 				id ;
  int 				threadNumber ;
  enum RWType			readWriter ;
  struct MTRWShmStatus *	shmStatPtr ;
  struct MTRWLocStat * 		locStatPtr ;
#ifdef UTILIZE_SUN_THREADS
  mutex_t * 			hskLockPtr ;
#else
  pthread_mutex_t *             hskLockPtr ;
#endif
} ;
 


/*****************************************************************************
 * Attach to the helper library.
 */

struct MTRWManageId MTRWManageGet (int, int, int, enum RWType, 
				   struct MTShmId *) ;

/*****************************************************************************
 * Detach from the helper library.
 */

int MTRWManageDetach (struct MTRWManageId) ;


/*****************************************************************************
 * Remove the shared memory buffer, by force.
 */

int MTRWManageRemove (struct MTRWManageId) ;


/*****************************************************************************
 * block thread till blocks are signaled as ready.
 */

int MTRWWaitForBlocks (struct MTRWManageId) ;


/*****************************************************************************
 * signal all threads that blocks are ready, handling memory laps, if nec.
 */

int MTRWBlocksReady (struct MTRWManageId, int) ;


/*****************************************************************************
 * return the current value of lag.
 */

int MTRWGetLag (struct MTRWManageId) ; 


/*****************************************************************************
 * return the current value of adjusts.
 */

int MTRWGetAdjusts (struct MTRWManageId) ; 


/*****************************************************************************
 * release the thread given by id that is waiting on a call to 
 * MTRWWaitForBlocks().
 */

int MTRWReleaseThread(struct MTRWManageId id) ;


/*****************************************************************************
 * toggle failsafe mode
 */

int MTRWFailSafe (struct MTRWManageId id, boolean_t on) ;


/*****************************************************************************
 * set the failsafe ignore flag for this thread
 */

int MTRWFailSafeIgnore (struct MTRWManageId id, boolean_t ignore) ;


/*****************************************************************************
 * wait for readers to clear lap danger in failsafe mode
 */

int MTRWFailSafeWait (struct MTRWManageId id, int numToWrite) ;


/*****************************************************************************
 * signal blocks read for failsafe mode
 */

int MTRWFailSafeSignal (struct MTRWManageId id) ;

#ifdef	__cplusplus
}
#endif

#endif    /* _MTRWHLPH_ */
