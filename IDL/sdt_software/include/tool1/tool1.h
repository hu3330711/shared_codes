/* TOOL1.h  initialize tool1 toolbox module routines */

#ifndef TOOL1_WAS_INCLUDED
#define TOOL1_WAS_INCLUDED

/* For SCCS */
#define SccsId_tool1_h "@(#)tool1.h	1.1, 11/14/92"

extern boolean TOOL1debug_ok;   /* true when ok to call DBG */

#ifdef ANSI_STD_C

boolean TOOL1init( int argc, char *argv[]);
void TOOL1exit( void);

#else

boolean TOOL1init();
void TOOL1exit();

#endif /* ANSI_STD_C */

#endif /* TOOL1_WAS_INCLUDED */

