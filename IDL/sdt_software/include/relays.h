/* ---------------------------  relays.h ------------------------------- *
 * --------- header file for programs that are relay endpoints  -------- */

#ifndef _RELAYSH_
#define _RELAYSH_

#if     !defined(lint)
static char sccsidRelaysH[] = "@(#)relays.h	1.7 01/13/94 UCB SSL";
#endif

/*---------------------------------------------------------------------------
 * The following defines determine network and port specific info
 */

#define UDP_SERVICE_FORMAT "fastrelay%02d"
#define UDP_BROADCAST_PORT 6666   /* base port for UDP broadcast */
#define UDP_BROADCAST_ADDR "192.107.8.255"  /* default broadcast address */

#define N_PORTS 100    /* max number of ports on which to broadcast */

#define CONFIG_FILE "Relay_port_config"

#endif       /* _RELAYSH_ */
