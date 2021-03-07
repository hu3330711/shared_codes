/********************************************************************/
#ifndef rtc_istp_h
#define rtc_istp_h "@(#)rtc_istp.h	1.3, 01/16/96"

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include <stdio.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

int Rtc_Connect(char *server) ;

int Rtc_GetPacket(char *databuf, long *size) ;

int Rtc_LoginUser(char *username, char *password) ;

int Rtc_SetPacket(char *packet_types, char *Rtc_PacketStatus) ;

/************************************************************************
*	Functions to convert from Multinet to  Unix support		*
*************************************************************************/

int socket_read(unsigned int socket,char *buf,int size) ;

int socket_write(unsigned int socket,char *command,int size) ;

int socket_perror(char *msg) ;


#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* rtc_istp_h */
