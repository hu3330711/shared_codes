
#ifndef IDL_AACGM_H
#define IDL_AACGM_H

/* These MUST match the corresponding entries in idl_aacgm.c */
/* message numbers... */
#define idl_aacgm_ERROR		0
#define idl_aacgm_NOSTRINGARRAY -1

/* Handy macro */
#define ARRLEN(arr) (sizeof(arr)/sizeof(arr[0]))

extern IDL_MSG_BLOCK msg_block;

/*Define the startup function that adds the C functions to IDL and the */
/*Exit handler.*/
extern int idl_aacgm_startup(void);

#endif

