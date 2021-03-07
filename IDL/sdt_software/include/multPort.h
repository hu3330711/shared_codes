#ifndef _MULTPORTH_
#define _MULTPORTH_

#if     !defined(lint)
static char sccsidMultPortH[] = "@(#)multPort.h	1.8 06/04/04 UCB SSL";
#endif
/*---------------------------- multPort.h -----------------------------
 */

#include <stdio.h>

#ifdef	__cplusplus
extern "C" {
#endif

int udpPortInfoCall_1(char *host, udpPortInfo_res *result_1);

/* for use by the rpc server only: */
FILE *fopenPortConfig(char *config_dir, char *lib_dir) ;
  
#define SOCKET_APID_PORT(upi, apId) ((upi)->apIdTable[((apId)-MIN_MIN_APP_ID)])
#define SOCKET_VCID_PORT(upi, vcId) ((upi)->vcIdTable[(vcId)])

int portsFromIds(udpPortInfo *upi, int nApId, int ApIdRequest[], 
		 int nVCId, int VCIdRequest[], int port[], char **errStrPtr);
int apIdList(udpPortInfo *upi, int theList[]);

#ifdef	__cplusplus
}
#endif

#endif /* _MULTPORTH_ */
