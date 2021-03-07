/* str.h */

#ifndef STR_WAS_DEFINED
#define STR_WAS_DEFINED

/* For SCCS */
#define SccsId_str_h "@(#)str.h	1.1, 11/14/92"

#ifdef ANSI_STD_C

/*  STR:  These are extensions to the standard string library.

    the parameter maxs1 is often used as the maximum size of a string.
    this includes the zero terminator; i.e. it is the dimension of the
    string: char s1[ maxs1];  */


extern  char *STRbinary(int num, int bits, char *s);
/* format "bits" number of bits from "num" into a string of zeros and ones
   bit 0 is in char 0, etc
   place result in s and return s */

extern  int  STRblnk(char *s);
/* Remove leading and trailing white space from the string s
   Return the length of the resulting string */

extern  void STRbpad(char *s,int w);
/* Pad string with blanks, until string has width w (length w+1). 
   Positive w means pad on right (left justify)
   Negetive w means pad on left (right justify) 
   Do nothing if s already has length >= w */

extern  char *STRconcat(char *s1,const char *s2,int maxs1);
/* concat s2 to end of s1, check s1 maximum = maxs1.  return s1 */

extern  char *STRdelete(char *s1,int ndelete);
/* delete the first ndelete chars from s1 */

extern  int  STRequal(char *s1, char *s2);
/* returns true if s1 matches s2.
   leading and trailing blanks are not counted.
   upper/lower case ignored */

extern char *STRfind( char *s1, char *s2);
/*  find first occurence of s2 in s1, else return NULL */

extern  int  STRgood(char *s);
/* returns true if s is a good (printable) string */

extern  char *STRinsert(char *s1,char *s2,int maxs1);
/* insert s2 into the start of s1; s1 cannot get bigger than maxs1 */

extern  void *STRmove(void *dst, void *src, int nbytes);
/*  move nbytes from src to dst; guarenteed to work even
    if overlapping regions */

extern  char *STRncopy(char *s1,char *s2,int maxs1);
/* copy up to maxs1-1 chars from s2 to s1. guarentee s1 is zero terminated */

extern  void STRpastoc(char *s);
/* convert pascal string (len in byte 0) to c string (zero terminated) */

extern  int  STRpos(char *s,char *c);
/* Find the (0 reletive) position of the first occurence of any of  
   the characters in string c in the string s.
   Return -1 if not found.
 */

extern  char *STRremove( char *s, char c);
/* remove all chars 'c' from s */

extern  void *STRswap(void *src2, void *src1, int nbytes);
/*  swap nbytes from src to dst; guarenteed to work even
    if overlapping regions */

extern  char *STRtokn(char **str_ptr,char *token,int max_toksiz,char *delim);
/* find tokens in string, delimited by any char in the string "delim".
   return found token in "token".
   Return pointer to the original spot in the string where the token 
   was found, and NULL if no token was found.
   str_ptr is updated past the token and delim(s), or set to NULL
   when theres nothing more to scan.

   Example:
      char  token[ MAX_TOKN];
      char  *ptr;

      ptr = original_string;
      while (NULL != STRtokn( &ptr, token, MAX_TOKN, " ")
         {
         if (STRequal( token, "some"))
            do_something;
         else if (STRequal( token, "any"))
            do_anything;
         };

 */

#else

extern  int  STRblnk();
extern  void STRbpad();
extern  char *STRconcat();
extern  char *STRdelete();
extern  int  STRequal();
extern  char *STRfind();
extern  int  STRgood();
extern  char *STRinsert();
extern  char *STRmove();
extern  char *STRncopy();
extern  void STRpastoc();
extern  int  STRpos();
extern  char *STRremove();
extern  char *STRswap();
extern  char *STRtokn();

#endif /* ANSI_STD_C */

#endif /* STR_WAS_DEFINED */
