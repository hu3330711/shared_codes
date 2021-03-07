#ifndef JCS_Boolean_h
#define JCS_Boolean_h
/* Boolean.h */

#define SccsId_Boolean_h "@(#)Boolean.h	1.3, 05/28/04"

#ifdef SOLARIS
#include <sys/types.h>
#endif

#ifdef LINUX_A
#include <sys/types.h>
#endif

#ifndef BOOLEAN
#define BOOLEAN int
#endif /* BOOLEAN */

#ifdef SOLARIS

#ifndef TRUE
#define TRUE B_TRUE
#endif /* TRUE */

#ifndef FALSE
#define FALSE B_FALSE
#endif /* FALSE */

#else

#ifndef boolean
#define boolean int
#endif /* boolean */

#ifndef TRUE
#define TRUE 1
#endif /* TRUE */

#ifndef FALSE
#define FALSE 0
#endif /* FALSE */

#endif

#endif /* JCS_Boolean_h */

