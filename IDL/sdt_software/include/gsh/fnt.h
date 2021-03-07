/*--------------------------------------------------------------------------*/
/* fnt.h
 *
 * This include file contains the constants, typedefs, structures,
 * and variables required for stroked text font activity.
 *
 */
#ifndef FNT_H
#define FNT_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include "gsh.h"

/* For SCCS */
#define SccsId_fnt_h "@(#)fnt.h	1.3, 12/11/03"

/*--------------------------------------------------------------------------*/
/* Symbols */

/* Number of characters in ASCII */
#define LAST_ASCII		95

/* This is the maximum number of special symbols that can be defined. */
#define IGES_SPCL_SYMB_SIZE     58

#define GREEK_SYMBOL_SIZE       48

/* Size of the Preview font file header sequence (in number of bytes) */
#define FNT_FIL_HDR_SIZ 8

/* The maximum number of regular fonts that Preview may be aware of. */
#define MAX_RGL_FNT	3

/* The maximum number of special fonts that Preview may be aware of. */
#define MAX_SPCL_FNT	3

/* The maximum number of fonts which can be in core at one time.
 * Note that the value is 2:   we can have one regular font and
 * one special font in core at the same time.
 */
#define MAX_FNT_TYP 3

/* Constants indicating text font types. */
#define REGULAR_FONT_TYPE	0
#define SPECIAL_FONT_TYPE	1
#define FNT_REG_FNT_DATA_SIZE	3740

/*--------------------------------------------------------------------------*/
/* Typedefs */

/* Stroked text font typedefs. */
typedef   int16  fnt_buf_typ_lng ;
typedef   char   fnt_buf_typ ;

/* Structure for header of each character. */
typedef struct  {
	int16		asc_cod ; /* The ascii code for the char. */
	fnt_buf_typ 	xnext ;   /* x offset to next char origin. */
	fnt_buf_typ 	ynext ;   /* y offset to next char origin. */
	fnt_buf_typ 	strokes ; /* Number of strokes in char. */
	} fnt_chr_hdr_typ ;

typedef struct  {
	fnt_chr_hdr_typ  header ;
        int32            stroke_ptr ;
	} fnt_chr_typ ;

/* Header structure for a font. */
typedef struct  {
	fnt_chr_typ  *tbl ;     /* Font character definition table. */
	fnt_buf_typ  *strk ;    /* Font character stoke array. */
	int16	font_code ;	/* The font code */
	int16	scale ;		/* Font scale (char height above baseline) */
	int16	descender ;	/* Font desc. hgt. (dist. below baseline) */
	int16	num_chr ;	/* Number of characters in this font */
	} fnt_hdr_typ ;

typedef struct  {
	fnt_chr_hdr_typ  header ;
        int32            *stroke_ptr ;
	} fnt_chr_typ_lng ;

/* Structure for each stroke of a character. */
typedef struct  {
	fnt_buf_typ	pen ;	/* pen flag: 0->up  1->down */
	fnt_buf_typ 	x ;	/* x position */
	fnt_buf_typ 	y ;	/* y position */
	} fnt_strk_typ ;

/* Structure for header of each character in long format. */
typedef struct  {
	int16		asc_cod ; /* The ascii code for the char. */
	fnt_buf_typ_lng xnext ; /* x offset to next char origin. */
	fnt_buf_typ_lng ynext ; /* y offset to next char origin. */
	fnt_buf_typ_lng strokes ; /* Number of strokes in char. */
	} fnt_chr_hdr_typ_lng ;

/* Structure for each stroke of a character in long format. */
typedef struct  {
	fnt_buf_typ_lng 	pen ; /* pen flag: 0->up  1->down */
	fnt_buf_typ_lng 	x ;	/* x position */
	fnt_buf_typ_lng 	y ;	/* y position */
	} fnt_strk_typ_lng ;

/* Note that these include files require the above typedefs. */
#ifdef GSH_GLB
#include "fnt_rsmp.h"
#include "fnt_rcmp.h"
#include "fnt_egth.h"
#include "fnt_ital.h"
#include "fnt_spcl.h"
#include "fnt_grek.h"
#endif

/*--------------------------------------------------------------------------*/
/* Variables */

/* The current font index type */
hextern	int16		fnt_cur_typ ;

/* The current regular font index */
hextern	int16		fnt_cur_rgl ;

/* The current special font index */
hextern	int16		fnt_cur_spcl ;

/* Text font descender heights (text.baseline - text.bottom). */
hextern	int16		fnt_dsc_hgt[MAX_FNT_TYP] ;

/* Text font definitions */
hextern	int32 		*fnt_chr_idx[MAX_FNT_TYP] ;
hextern	fnt_buf_typ	*fnt_buf[MAX_FNT_TYP] ;
hextern	fnt_hdr_typ	fnt_hdr_buf[MAX_FNT_TYP] ;
hextern	char		*fnt_nam[MAX_FNT_TYP] ;

/* Text font system file names. */
hextern char		*fnt_fnt_fil[MAX_RGL_FNT] ;
hextern char		*fnt_spcl_fil[MAX_SPCL_FNT] ;

/* Flags to see if a font index is usable. */
hextern	int     	fnt_avail[MAX_FNT_TYP] ;

/* This holds the name of the iges special symbols sub-file. */
hextern char		*igs_spcl_fil ;

/* Headers for fonts. */
hextern fnt_hdr_typ  fnt_roman_simplex
#ifdef GSH_GLB
#ifndef lint
    =
    {rsmp_fnt_tbl, rsmp_chr, ROMAN_SIMPLEX, 21, -7, LAST_ASCII}
#endif
#endif
    ;

hextern fnt_hdr_typ  fnt_roman_complex
#ifdef GSH_GLB
#ifndef lint
    =
    {rcmp_fnt_tbl, rcmp_chr, ROMAN_COMPLEX, 21, -7, LAST_ASCII}
#endif
#endif
    ;

hextern fnt_hdr_typ  fnt_english_gothic
#ifdef GSH_GLB
#ifndef lint
    =
    {egth_fnt_tbl, egth_chr, ENGLISH_GOTHIC, 21, -7, LAST_ASCII}
#endif
#endif
    ;

hextern fnt_hdr_typ  fnt_italic_complex
#ifdef GSH_GLB
#ifndef lint
    =
    {icmp_fnt_tbl, icmp_chr, ITALIC_COMPLEX, 21, -7, LAST_ASCII}
#endif
#endif
    ;

hextern fnt_hdr_typ  fnt_special_symbols
#ifdef GSH_GLB
#ifndef lint
    =
    {igss_fnt_tbl, igss_chr, SPECIAL_SYMBOLS, 21, -7, IGES_SPCL_SYMB_SIZE}
#endif
#endif
    ;

hextern fnt_hdr_typ  greek_symbols
#ifdef GSH_GLB
#ifndef lint
    =
    {grek_fnt_tbl, grek_chr, GREEK_SYMBOLS, 21, -7, GREEK_SYMBOL_SIZE}
#endif
#endif
    ;

/*--------------------------------------------------------------------------*/
/* Function declarations: */

#ifdef ANSI_STD_C
void fnt_initialize (void) ;
int16 fnt_get_descender (int16 font) ;
#else
void fnt_initialize () ;
int16 fnt_get_descender () ;
#endif

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif /* FNT_H */
