#ifndef APIDCOM
#define APIDCOM

static char sccsidApidComH[] = "@(#)apidCom.h	1.7 10/28/93 UCB SSL";

#include <time.h>

#ifdef	__cplusplus
extern "C" {
#endif

#define TIME_STAMP_SIZE 6
#define HI_RES 0.025 /* generated data resolution in msec */
#define LO_RES 0.2
/* max dt of a pdu: */
#define MAX_PDU_DT ((PDU_LEN - SEC_HDR_SIZE - DATA_HDR_SIZE - ERR_STAT_SIZE) \
		    * HI_RES) 

#define PERIOD (.05) /* 50 ms period */

#define FIRST_APP_ID 1100
#define APP_ID_COUNT 6
extern double resolutions[];
#define RES(APP_ID) resolutions[(APP_ID) - FIRST_APP_ID]

extern float abs1charA[];/* decommutate char to value in range +/-1 */
extern int Nabs1charA;

double decom_TIME42(char *tmptr);
char *com_TIME42(double tm, char *tmptr);
void setOffset(void);
double unixToMe(time_t tm);
time_t meToUnix(double tm);
double getStarttime(void);
float decom_poly(char *x, int size,  float a[], int na);
long com_poly(float y, float a0, float a1);

#ifdef	__cplusplus
}
#endif

#endif

