/* dbg.h */

#ifndef DBG_WAS_INCLUDED
#define DBG_WAS_INCLUDED

/* DBG - DEBUG module 

   This module allows you to embed debug statements in your code
   and activate them from the command line, or from a subroutine

   On the command line, use -DBG < command string>.  The command string has
   the syntax:
      [ON|OFF] Module [flagno | ALL]  turn on/off flag "module", flagno
      [ON|OFF] STDERR                 turn on/off output to standard error
      [ON|OFF] LOG                    turn on/off output to debug logger
      DUMP                            dump all flags that have been set

      E.g:  GSS 1, 2, 3    turn on flags 1,2,3 in module GSS
      E.g:  GSS 1 ABC 2   turn on flag 1 in module GSS, flag 2 in ABC

   Messages that are sent to DBG may be sent to standard error
   or logged in the file DBGLOG. Default is to standard error.

   A global variable DBGdebug is used to avoid overhead for non-debugging
   runs.

   The macro Module( "name") is provided in globals.h to facilitate the
        setting of the Module name parameter to DBGflag;
        e.g:  Module( "name");
              if (DBGflag( ModName, 2))
                 ...

 */

/* For SCCS */
#define SccsId_dbg_h "@(#)dbg.h	1.1, 11/14/92"

#define DBGflag(name, n)  ( DBGdebug && DBGflag_( name, n) )

/***************** exported variables */

extern boolean DBGdebug;

/***************** exported functions */

#ifdef ANSI_STD_C

extern  void DBGinit(void);
/* initialize the DBG module */

extern  void DBGexit(void);
/* exit the DBG module */

extern  int DBGcontrolTokn(int targc, char * *targv);
/* parse commands to DBG using an array of tokens. 
   These are sent from TOOL1init() */

extern  void DBGcontrol(char *s);
/* parse commands to DBG using a string of commands. */

extern  int DBGflag_(char *module_name,int flag_number);
/* return TRUE if the debug flag is set 
   use the magro version:
       if (DBGflag(ModName, 1)) 
            DBGprint(....);
 */

 
extern  void DBGprint(char *message);
/* send message to the DBG logger */

#ifdef LINUX_A
extern void DBGprintf( int nargs, ...);
#else
extern void DBGprintf( ...);
#endif
/* send DBG message with variable parameter list: 
   eg: DBGprint( "x= %f a = %d", x , a);
   format cannot expand to greater than 300 chars
*/ 

#else

extern  void DBGinit();
extern  void DBGexit();
extern  int DBGcontrolTokn();
extern  void DBGcontrol();
extern  int DBGflag_();
extern  void DBGprint();

#endif /* ANSI_STD_C */


#endif /* DBG_WAS_INCLUDED */
