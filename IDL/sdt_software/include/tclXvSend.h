#ifndef _TXVSEND_H
#define _TXVSEND_H

#include <sys/types.h>
#include <sys/time.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/notify.h>
#include <tcl.h>

#ifdef __cplusplus
extern "C" {
#endif

char * 
TclXvSend_SetAppName(Tcl_Interp *interp, char *name, Frame toplevel);

#ifdef __cplusplus
}
#endif

#endif /*  _TXVSEND_H */
