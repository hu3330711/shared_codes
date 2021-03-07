/*
 * This file contains the header file for the blocked, circular, shared
 * memory library shmCirLib.c. See comments in that file.
 */

#ifndef _SHMCIRBUF_
#define _SHMCIRBUF_

#ifdef LINUX_A
#ifndef _MTSHMCIRBUFTDEFSLINUX__
#define _MTSHMCIRBUFTDEFSLINUX__
#include "TDefsForLinux.h"
#endif  /* _MTSHMCIRBUFTDEFSLINUX__ */
#endif  /* LINUX_A */

#ifndef _SHMCIRBUFUNISTD__
#define _SHMCIRBUFUNISTD__
#include <unistd.h>
#endif  /* _SHMCIRBUFUNISTD__ */

#ifndef _SHMCIRBUFTYPES_
#define _SHMCIRBUFTYPES_
#include <sys/types.h>
#endif  /* _SHMCIRBUFTYPES_ */

#ifndef _SHMCIRBUFIPC_
#define _SHMCIRBUFIPC_
#include <sys/ipc.h>
#endif  /* _SHMCIRBUFIPC_ */

#ifndef _SHMCIRBUFSHM_
#define _SHMCIRBUFSHM_
#include <sys/shm.h>
#endif  /* _SHMCIRBUFSHM_ */

#ifndef _SHMCIRBUFMEMORY_
#define _SHMCIRBUFMEMORY_
#include <memory.h>
#endif  /* _SHMCIRBUFMEMORY_ */

#ifndef _SHMCIRBUFSEMUTIL_
#define _SHMCIRBUFSEMUTIL_
#include "semUtil.h"
#endif  /* _SHMCIRBUFSEMUTIL_ */

#ifndef _SHMCIRBUFERROR_
#define _SHMCIRBUFERROR_
#include "error.h"
#endif  /* _SHMCIRBUFERROR_ */

/* shm permissions og+rw (0666) */

#define PERMS 0666

#ifdef	__cplusplus
extern "C" {
#endif

/*-----------------------------------------------------------------------------
 * A useful boolean type: use boolean_t defined in <sys/types.h>
 */

/*-----------------------------------------------------------------------------
 * BlockShmRWData contains read and write locations for one process
 */

struct BlockShmRWData
{
  int currentWriteBlock ;
  int currentReadBlock ;
  int shmId ;
  int lockSemId ;
} ;


/*-----------------------------------------------------------------------------
 * BlockShmStatus contains to the layout of the shared memory buffer.
 */


struct BlockShmStatus
{
  int blockSize ;
  int numberBlocks ;
  int numberProcesses ;
  int lastWriteProc ;
  int maxProcs ;
  int infoOffset ;
  int dataStartOffset ;
  boolean_t quitFlag ;
} ;


/*-----------------------------------------------------------------------------
 * ShmId is the shared memory buffer id for a given process.
 */

struct ShmId
{
  int id ;
  int procNumber ;
  struct BlockShmStatus * statPtr ;
} ;



/*****************************************************************************
 * Attach to a shared memory segment, initializing, if necessary.
 */

struct ShmId bShmGet(key_t shmKey, key_t lockKey, int blockSize, 
	      int nBlocks, int maxProcs, int flags);

/*****************************************************************************
 * Detach from our shared memory segment.  if this is the last reference,
 * remove it from the system.
 */

int bShmDetach(struct ShmId id) ;

/*****************************************************************************
 * Force shared memory buffer to be removed from the system.
 */

int bShmRemove(struct ShmId id) ;

/*****************************************************************************
 * Read from shared memory into a buffer from the current read location for
 * this process.
 */

int bShmReadBlock(struct ShmId id, char *readToBuf, int pointToNext, 
		  void (*optFunction)(void *), void * funcArg, 
		  boolean_t useLock) ;

/*****************************************************************************
 * Write from a buffer into shared memory at this current write location
 * for this process.
 */

int bShmWriteBlock(struct ShmId id, char * writeToBuf, int pointToNext, 
		   void (*optFunction)(void *), void * funcArg, 
		   boolean_t useLock) ;

/*****************************************************************************
 * Write multiple buffers to shared memory from an array of bufpointer 
 * locations starting at the current write location for this process.
 */
 
int bShmWriteMultBlocks(struct ShmId id, char * writeToBufs[], 
		       int numToWrite, int pointToNext, 
		       void (*optFunction)(void *), void * funcArg, 
		       boolean_t useLock) ;

/*****************************************************************************
 * Seek the current read index for this process.  This will be delta 
 * relative to the current position.
 */

int bShmMoveCurRead(struct ShmId id, int moveBy, boolean_t useLock) ;

/*****************************************************************************
 * Seek the current write index for this process.  This will be delta 
 * relative to the current position.
 */

int bShmMoveCurWrite(struct ShmId id, int moveBy, boolean_t useLock) ;

/*****************************************************************************
 * Get the current write index for this process.  
 */

int bShmGetCurWriteBlock(struct ShmId id, boolean_t useLock) ;

/*****************************************************************************
 * Get the current read index for this process.  
 */

int bShmGetCurReadBlock(struct ShmId id, boolean_t useLock) ;

/*****************************************************************************
 * Get the number of process atatched to the shared memory buffer.  
 */

int bShmGetNumProcs(struct ShmId id) ;

/*****************************************************************************
 * Get the index of the last process to write shm.
 */

int bShmGetLastWriteProc(struct ShmId id, boolean_t useLock) ;

/*****************************************************************************
 * Set the index of the last process to write shm.  
 */

int  bShmSetWriteProc (struct ShmId, int procNumber, boolean_t useLock ) ;

/*****************************************************************************
 * Get the current read block for the process with index given.
 */

int bShmGetReadBlockForProc(struct ShmId id, int procIndex, boolean_t useLock);

/*****************************************************************************
 * Get the current write block for the process with index given.
 */

int bShmGetWriteBlockForProc(struct ShmId id, int procIndex,boolean_t useLock);

/*****************************************************************************
 * Set quit flag in status area
 */

int bShmSignalQuit(struct ShmId id, boolean_t quitFlag, boolean_t useLock) ;

/*****************************************************************************
 * Get the quit status 
 */

boolean_t bShmGetQuitStat(struct ShmId id, boolean_t useLock);

#ifdef	__cplusplus
}
#endif

#endif    /* _SHMCIRBUF_ */
