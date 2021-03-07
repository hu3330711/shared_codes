/* err.h */

#ifndef ERR_WAS_INCLUDED
#define ERR_WAS_INCLUDED

/* For SCCS */
#define SccsId_err_h "@(#)err.h	1.1, 11/14/92"

/* ERR.h : error processing module
 *
 * This module create a centralized place for programming/system errors
 * to go through. There are several levels you can use this:
 * 
 *    0)  In the default case, you simply call ERRout with a
 *  library name, routine number, error number, and 
 *  optional parameter string (use sprintf for general parameter passing).  
 *  The message
 *    " Library mmm routine d got an error x
 *      parameter = sssss" (optional)
 * is sent to stderr.
 *
 *    1)  You may place error messages in an error message file in the format:
 *          ROUTINE 1 routine_name
 *           1  1  routine one error 1 message
 *           1  2  routine one error 2 message
 *           ...
 *          ROUTINE 2 routine_name
 *           2  1  routine two error 1 message
 *           2  2  routine two error 2 message
 *           ...
 *    The default error message file is "errmsg". The error message is then:
 *       " Library mmm routine routine_name got an error x
 *         routine one error 2 message
 *         parameter = sssss" (optional)
 *
 *    3) You may create a seperate error message file for a subsystem, in
 *    the same format, and direct ERR to use it by calling 
 *          ERRsetMessageFile( library, filename);
 *    All calls to ERRout with the same library name will use the error messages
 *    file given by "filename".
 *
 *    4) You may turn on/off the error message being sent to stderr, to
 *    the error logging file "errlog", or to your own processing routine
 *    by calling:
 *          ERRcontrol( mode, val, xproc);
 *    where
 *          mode = ERR_STDERR, ERR_LOGGING or ERR_XTRA
 *          val  = 0 (off) or 1 (on)
 *          xproc = used only when mode = ERR_XTRA, in which case it is a 
 *                  pointer to function of type (void (*)( char *)). The
 *                  type ERRxpr_fptr is provided for casting. This routine 
 *                  is then called whenever an ERR message is received.
 *
 *
 *    The macro Library( "name") is provided in globals.h to facilitate the
 *       setting of the Library name parameter to ERRout;
 *       e.g:  Library( "name");
 *             ERRout( LibName, ...);
 */

#define ERR_STDERR 1
#define ERR_LOGGING 2
#define ERR_XTRA 3

#ifdef ANSI_STD_C

typedef void (*ERRxpr_fptr)( char *);


extern  void ERRcontrol(int mode,int val,ERRxpr_fptr xproc);
/* control how ERR messages are handled:
      mode = ERR_STDERR  send to standard error
             ERR_LOGGING send to file "ERRLOG"
             ERR_XTRA    call routine "xproc"
      val = 0 (turn off) or 1 (turn on)

      default is standard error only
 */

extern  void ERRout(char *library,int routine,int err,char *params);
/* send ERR message: library name, routine number, error number,
   and optional parameter string */

#ifdef LINUX_A
extern void ERRoutf(int nargs, ...);
#else
extern void ERRoutf(...);
#endif
/* send ERR message with variable parameter list: 
   library name, routine number, error number, format, parameters.
   eg: ERRoutf( LibName, 1, 1, "x= %f a = %d", x , a);
   format cannot expand to greater than 300 chars
*/ 

extern  void ERRsetMessageFile( char *library, char *fname);
/* tell ERR to look up messages for the given library in the file fname */

#else

typedef void (*ERRxpr_fptr)();
extern  void ERRcontrol();
extern  void ERRout();
extern void ERRoutf();
extern  void ERRsetMessageFile();

#endif /* ANSI_STD_C */

#endif /* ERR_WAS_INCLUDED */
