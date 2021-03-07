#ifndef FastConfig_h_def
#define FastConfig_h_def
static char SccsId_FastConfig_h[] = "@(#)FastConfig.h	1.1, 01/12/94 UCB SSL" ;

#include <stdio.h>

FILE *env_openfile(char *env_dir, char *file) ;

FILE *open_config(char *file, char *standard_file) ;

#endif
