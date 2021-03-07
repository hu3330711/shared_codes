/* ------------------------------------------------------------------ */
/*
 *
 * gsh.h
 *
 * These are the public declarations for the graphics shell.
 *
 */
#ifndef GSH_H
#define GSH_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

/* ------------------------------------------------------------------ */
/* Included files. */
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <fcntl.h>
#include <math.h>

#ifdef SUNVIEW_A
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <sunwindow/cms_rainbow.h>
#endif /* SUNVIEW_A */

#ifdef XWIN_A
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#endif /* XWIN_A */

/* For SCCS */
#define SccsId_gsh_h "@(#)gsh.h	1.34, 12/03/06"

/* ------------------------------------------------------------------ */
/* Internal constants. */

#ifdef XWIN_A
#define WIN_WIDTH_DEF		1152
#define WIN_HEIGHT_DEF		772
#define WIN_X_DEF			0
#define WIN_Y_DEF			0
#define GSH_PIXEL_FONT        "9x15"
#endif /* XWIN_A */

#define GSH_NULL_CURSOR_INDEX	-1
#define GSH_FIRST_CURSOR_INDEX	1

/* ------------------------------------------------------------------ */
/* Useful constants. */

#ifndef PI
#define   PI   3.141592654
#endif

/* Radians per degree. */
#ifndef RAD_PER_DEG
#define   RAD_PER_DEG   0.017453292
#endif

/* Degrees per radian. */
#ifndef DEG_PER_RAD
#define   DEG_PER_RAD   57.29578122
#endif

/* Millimeters per inch. */
#ifndef MM_PER_INCH
#define   MM_PER_INCH   25.4
#endif

#ifdef SOLARIS

#ifndef TRUE
#define TRUE B_TRUE
#endif

#ifndef FALSE
#define FALSE B_FALSE
#endif

#else

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#endif

#ifndef NULL
#define NULL 0
#endif

/* The following definition is used to cover and uncover "extern". */
#ifdef GSH_GLB
#define hextern
#else
#define hextern extern
#endif

/* These typedefs are to enhance portability. */
#ifndef int32
typedef int	int32 ;
#endif

#ifndef uint32
typedef unsigned int	uint32 ;
#endif

#ifndef int16
typedef short   int16 ;
#endif

#ifndef byte
typedef unsigned char  byte ;
#endif

/* ------------------------------------------------------------------ */
/* Macros. */

#ifndef min
#define min(x,y)   (((x) < (y)) ? (x) : (y))
#endif

#ifndef max
#define max(x,y)   (((x) > (y)) ? (x) : (y))
#endif

/* The following is the declaration to a function to perform floating
 * point divides on the Sun.  This is so that division-by-zero may be
 * trapped on the Sun, instead of letting it slip through as a NAN.
 * On other systems, this is merely the division itself.
 */
#ifdef SUN_A
#ifndef DIVREAL
#ifdef ANSI_STD_C
hextern double	div_flt (double, double) ;
#else
hextern double	div_flt () ;
#endif /* ANSI_STD_C */
#define DIVFLT(a,b)  div_flt ((double) (a), (double) (b))
#else
#define DIVFLT(a,b)  (((double) (a)) / ((double) (b)))
#endif /* DIVREAL */
#else
#define DIVFLT(a,b)  (((double) (a)) / ((double) (b)))
#endif /* SUN_A */ 

/* ------------------------------------------------------------------ */
/* Constants. */

/* Preferences for Display/Visual type (mostly for X-windows): */
#define GSH_PSEUDO_COLOR  			0
#define GSH_TRUE_COLOR    			1
#define GSH_DIRECT_COLOR                        2
#define GSH_STATIC_COLOR                        3
#define GSH_STATIC_GRAY                         4
#define GSH_GRAY_SCALE                          5

/* Display write modes. */
#define GSH_OVERWRITE_MODE			0
#define GSH_NOOP_MODE				1
#define GSH_PAINT_MODE				2
#define GSH_MASK_MODE				3
#define GSH_ERASE_MODE				4
#define GSH_INVERT_MODE				5
#define GSH_XOR_MODE				6
#define GSH_UNKNOWN_MODE			7

/* Line styles. */
#define GSH_LINE_STYLE_SOLID		0
#define GSH_LINE_STYLE_DASHED		1
#define GSH_LINE_STYLE_PHANTOM		2
#define GSH_LINE_STYLE_CENTERLINE	3
#define GSH_LINE_STYLE_USER		100

/* Event types. */
#define GSH_KEY_STROKE			0
#define GSH_BUTTON_PRESS		1
#define GSH_MOUSE_MOVE			2
#define GSH_WINDOW_MOVE			3
#define GSH_WINDOW_RESIZE		4
#define GSH_WINDOW_REPAINT		5
#define GSH_WINDOW_HIDE			6
#define GSH_WINDOW_EXPOSE		7
#define GSH_WINDOW_CLOSE		8
#define GSH_WINDOW_OPEN			9
#define GSH_SHIFT_DOWN			10
#define GSH_SHIFT_UP			11
#define GSH_BUTTON_RELEASE		12

/* Special Key Identifiers. */
#define ESCAPE_KEY_ID                   27

#define GSH_KEY_ESCAPE		        27
#define GSH_KEY_ENTER		        13
#define GSH_KEY_BACKSPACE		8

#define GSH_KEY_LEFT_ARROW		9000
#define GSH_KEY_RIGHT_ARROW		9001
#define GSH_KEY_UP_ARROW		9002
#define GSH_KEY_DOWN_ARROW		9003
#define GSH_KEY_HOME		        9004
#define GSH_KEY_PAGE_UP		        9005
#define GSH_KEY_PAGE_DOWN		9006
#define GSH_KEY_INSERT		        9007
#define GSH_KEY_DELETE		        9008
#define GSH_KEY_END		        9009
#define GSH_KEY_F1		        9011
#define GSH_KEY_F2		        9012
#define GSH_KEY_F3		        9013
#define GSH_KEY_F4		        9014
#define GSH_KEY_F5		        9015
#define GSH_KEY_F6		        9016
#define GSH_KEY_F7		        9017
#define GSH_KEY_F8		        9018
#define GSH_KEY_F9		        9019
#define GSH_KEY_F10		        9020
#define GSH_KEY_F11		        9021
#define GSH_KEY_F12		        9022
#define GSH_KEY_PRINT_SCREEN		9031

#define GSH_KEY_LEFT_ARROW_SHIFT	9040
#define GSH_KEY_RIGHT_ARROW_SHIFT	9041
#define GSH_KEY_UP_ARROW_SHIFT   	9042
#define GSH_KEY_DOWN_ARROW_SHIFT	9043

#define GSH_KEY_UNKNOWN		        9999

/* Types of mouse cursor images. */
#define GSH_PIXMAP_CURSOR		0
#define GSH_GLYPH_CURSOR		1
#define GSH_FONT_CURSOR			2

/* Constants defining plot orientation. */
#define  PLT_LANDSCAPE     0
#define  PLT_PORTRAIT      1
#define  PLT_BIGGEST_FIT   2

/* Constants defining plotter devices. */
#define  PLT_HP7475       0
#define  PLT_HP7550       1
#define  PLT_HP7570       2
#define  PLT_HP7580       3
#define  PLT_HP7585       4
#define  PLT_HP7586       5
#define  PLT_HP7595       6
#define  PLT_HP7596       7
#define  PLT_POSTSCRIPT   8
#define  PLT_HPLJET       9
#define  PLT_COLOR_POSTSCRIPT      10
#define  PLT_CGMB         11
#define  PLT_ODF          12
#define  PLT_GIF          13
#define  PLT_COLOR_GIF    14
#define  PLT_JPEG         15
#define  PLT_COLOR_JPEG   16
#define  PLT_COLOR_POSTSCRIPT_L2   17

/* Constants defining plotter output destination. */
#define  PLT_REMOTE       0
#define  PLT_SERIAL_A     1
#define  PLT_SERIAL_B     2
#define  PLT_SERIAL_C     3
#define  PLT_FILE         4

/* Constants defining plot sizes. */
#define  PLT_SIZE_A       0 
#define  PLT_SIZE_B       1 
#define  PLT_SIZE_C       2 
#define  PLT_SIZE_D       3 
#define  PLT_SIZE_E       4 
#define  PLT_SIZE_A4      5 
#define  PLT_SIZE_A3      6 
#define  PLT_SIZE_A2      7 
#define  PLT_SIZE_A1      8 
#define  PLT_SIZE_A0      9 
#define  PLT_SIZE_OTHER   100 

/* Constants defining plot unit types. */
#define  PLT_UNIT_INCH    0 
#define  PLT_UNIT_MM      1 

/* Constants defining plotter color capabilities. */
#define  PLT_RGB_COLORS         0 
#define  PLT_CMY_COLORS         1 
#define  PLT_GRAYSCALE_COLORS   2 
#define  PLT_PEN_COLORS         3 

/* Constants defining pixel-type plot width and height: */
#define  PLT_DEFAULT_PIXEL_PLOT_WIDTH         800 
#define  PLT_DEFAULT_PIXEL_PLOT_HEIGHT        800 

/* Constants defining min/max graph_line_width_factor: */
#define  PLT_MIN_GRAPH_LINE_WIDTH_FACTOR      0.001 
#define  PLT_MAX_GRAPH_LINE_WIDTH_FACTOR      50.0 

/* Available stroked fonts (used by "wld_stroke_text"
 * and "wld_text".
 */
#define  ROMAN_SIMPLEX   0
#define  ROMAN_COMPLEX   1
#define  ITALIC_COMPLEX  8
#define  ENGLISH_GOTHIC  10
#define  SPECIAL_SYMBOLS 18
#define  GREEK_SYMBOLS   19

/* Line styles */
#define SOLID_LINE		0
#define DASHED_LINE		1
#define DASHED_DOT_LINE		2
#define DASHED_DOT_DOT_LINE	3
#define USER_DASHED_LINE	4
#define USE_GSH_STYLE	        5

/* Drawing orientations. */
#define ORIENT_STANDARD	                0
#define ORIENT_ROTATE_90_CNTCWISE	1
#define ORIENT_ROTATE_180            	2
#define ORIENT_ROTATE_90_CWISE          3
#define ORIENT_MIRROR_ABOUT_X_AXIS      4
#define ORIENT_MIRROR_ABOUT_Y_AXIS      5
#define ORIENT_MIRROR_ABOUT_LL_TO_UR    6
#define ORIENT_MIRROR_ABOUT_UL_TO_LR    7

/* User symbol vertical-justifications. */
#define VERTICAL_LOW_JUSTIFY            0
#define VERTICAL_BASELINE_JUSTIFY       1
#define VERTICAL_CENTER_JUSTIFY         2
#define VERTICAL_TOP_JUSTIFY            3

/* User symbol horizontal-justifications. */
#define HORIZONTAL_LEFT_JUSTIFY         0
#define HORIZONTAL_CENTER_JUSTIFY       1
#define HORIZONTAL_RIGHT_JUSTIFY        2

/* Line printer locations: */
#define  PRT_TO_DEVICE    0
#define  PRT_SERIAL_A     1
#define  PRT_SERIAL_B     2
#define  PRT_SERIAL_C     3
#define  PRT_TO_FILE      5

/* For GIF: */
#define GIF_HSIZE  5003            /* 80% occupancy */


/* ------------------------------------------------------------------ */
/* Typedefs. */

typedef int32			gsh_cds ;	/* gsh device coord. space units. */
typedef int32			gsh_col_typ ;	/* Color type. */

/* The type for bitmasks is 32 bits long - I've heard of expensive 24
 * bitplane graphics devices so we cover everything here.
 */
typedef int32			gsh_msk_typ ;	
typedef int16			gsh_lin_typ ;	/* Line style. */
typedef float			gsh_lin_wid ;	/* Line width. */
typedef int32			gsh_lin_def ;	/* Line style. */
typedef int32			gsh_dev_exp ;	/* Long format coord. type. */
typedef int16			gsh_mod_typ ;	/* Write mode. */
typedef gsh_msk_typ		gsh_pix_typ ;	/* Pixel type. */

typedef int32			gsh_drawable_typ ;

/* gsh coordinate xy pair */
struct gsh_cds_xy_struct
	{
	gsh_cds		x ;
	gsh_cds		y ;
	} ;
typedef struct  gsh_cds_xy_struct  gsh_cds_xy ;
	
/* gsh rectangle */
struct gsh_cds_rect_struct
	{
	gsh_cds		orgx ;
	gsh_cds		orgy ;
	gsh_cds		wid ;
	gsh_cds		hgt ;
	} ;
typedef struct  gsh_cds_rect_struct  gsh_cds_rect ;
	
/* Long format coordinate xy pair */
struct  gsh_xy_exp_struct
	{
	gsh_dev_exp		x ;
	gsh_dev_exp		y ;
	} ;
typedef struct  gsh_xy_exp_struct  gsh_xy_exp ;
	
/* Typedefs for color tables and maps. */
typedef int16	gsh_opc_typ ;	/* Opacity storage type. */
typedef float	gsh_hue_typ ;	/* Hue storage type. */
typedef float	gsh_sat_typ ;	/* Saturation storage type. */
typedef float	gsh_int_typ ;	/* Intensity storage type. */
typedef float	gsh_rgb_typ ;	/* RGB storage type. */

typedef gsh_drawable_typ  window_typ ;
typedef gsh_drawable_typ  pixmap_typ ;
typedef int32	cursor_typ ;	/* Cursor id type. */

/* Bitmap storage ("image") typedefs. */
struct gsh_image_struct
	{

#ifdef XWIN_A	
	XImage		*x_image ;
#endif /* XWIN_A */

        window_typ              window ;
	gsh_msk_typ		planes ;
	gsh_cds			orgx ;
	gsh_cds			orgy ;
	gsh_cds			width ;
	gsh_cds			height ;
	} ;
typedef struct  gsh_image_struct  gsh_image_typ ;

/* World-Space Typedefs: */

/* The form of a world coordinate. */
typedef double  wld_cds ;

/* World xy coordinates. */
struct wld_cds_xy_struct
    {
    wld_cds    x ;
    wld_cds    y ;
    } ;
typedef struct  wld_cds_xy_struct   wld_cds_xy ;

/* World coordinate rectangle. */
struct wld_cds_rect_struct
    {
    wld_cds    llx ;
    wld_cds    lly ;
    wld_cds    urx ;
    wld_cds    ury ;
    } ;
typedef struct  wld_cds_rect_struct   wld_cds_rect ;

/* This is the 2-D world-to-device transformation structure.
 *
 * Note that we use the convention of multiplying a vector on
 * the left by the transform matrix.  In other words, the
 * matrix (in homogeneous form) is on the left, followed by
 * the vector, viewed as a column vector (1 x 3 matrix).
 *
 * The following indicates how we transform a point.  Note
 * again that we use homogeneous notation here.
 *
 *    |  a  c  e  |   | x |   |ax + cy + e|
 *    |  b  d  f  | * | y | = |bx + dy + f|
 *    |  0  0  1  |   | 1 |   |     1     |
 *
 * Of course, in most cases, "b" and "c" are 0 (no rotation
 * involved in going from world to device space) so that the
 * resulting column-vector calculation is simplified.  The
 * structure contains a flag indicating when simplifications
 * can be used to eliminate unnecessary floating point
 * arithmetic from the transformation.
 */
struct wld_2d_xfm_struct
	{
	double	a ;	/* matrix entry:  a11 */
	double	b ;	/* matrix entry:  a21 */
	double	c ;	/* matrix entry:  a12 */
	double	d ;	/* matrix entry:  a22 */
	double	e ;	/* matrix entry:  a13 */
	double	f ;	/* matrix entry:  a23 */
	int16 type ;	/* Type of matrix:
			 *   0 -> b,c are 0  (0 or 180 rotation in xform)
			 *   1 -> a,d are 0  (90 or -90 rotation in xform)
			 *   2 -> general case  (non-trivial rotation)
			 */
	} ;
typedef struct  wld_2d_xfm_struct   wld_2d_xfm ;

/* This is the structure for defined world-coordinate line styles. */
struct wld_lin_def_struct
    {
    int16     seg_cnt ;
    float     *seg_lst ;
    } ;
typedef struct  wld_lin_def_struct   wld_lin_def ;

/* This is the type for line attributes. */
struct wld_lin_att_struct
    {
    wld_lin_def   *line_dashes ;
    wld_cds       line_width ;
    int           thick_flag ;
    int           solid_flag ;
    int           simple_style ;
    int16         line_style ;
    } ;
typedef struct  wld_lin_att_struct   wld_lin_att ;

/* This is the structure which contains world-space to device-space 
 * mapping information.
 */
struct wld_map_typ_struct
    {
    window_typ     window ;        /* The corresponding device window. */
    wld_2d_xfm     xfm ;           /* World-to-device transformation. */
    wld_2d_xfm     inv_xfm ;       /* Inverse of "xfm" */
    wld_2d_xfm     a_xfm ;         /* World to ideal-world transformation.
				    * It is used in text and axis routines
				    * to reduce reversed_x,y spaces to 
				    * unreversed, which is much easier to
				    * work with.  It also creates an aspect-
				    * preserving space.
				    */
    wld_2d_xfm     b_xfm ;         /* xfm from idealized (aspect preserving
				    * and no reversed x or y) space
				    * to device space.
				    */
    wld_cds_xy     clip_lleft ;    /* Lower left corner of world clip rect. */
    wld_cds_xy     clip_uright ;   /* Upper right corner of world clip rect. */
    wld_cds_xy     center ;        /* World space rectangle center. */
    double         scale ;         /* Device units per world unit. */
    int16          orientation ;   /* One of 8 orthogonal orientations. */
    int            reverse_x ;     /* "left" x is larger than "right" x. */
    int            reverse_y ;     /* "lower" y is larger than "upper" y. */
    int            rotate ;        /* TRUE -> drawing rotated 90 deg cwise. */
    int            soft_clip ;     /* TRUE -> clip in world space. */
    int            preserve_aspect ;     /* TRUE -> aspect ratio of x, y is
                                          * 1, FALSE otherwise.
                                          */
    int            use_x_aspect ;  /* TRUE -> use x for text adjustment hgt. */
    int            ideal_space ;   /* TRUE -> need a_xfm for text. */
    double         aspect_adjust ; /* Text aspect ratio adjustment. */
    gsh_cds_xy     r_origin ;      /* Rectangle origin relative to window. */
    gsh_cds        r_width ;       /* Rectangle width. */
    gsh_cds        r_height ;      /* Rectangle height. */
    } ;
typedef struct  wld_map_typ_struct   wld_map_typ ;

struct gsh_axis_time_label_typ_struct
    {
    /* The following fields indicate the date stamp to what time values
     * are relative (to midnight UT of this day):
     */
    int16    base_year ;
    int16    base_month ;     /* 1, 2, 3, ... , 12 */
    int16    base_day ;       /* 1, 2, 3, ... , 31 */

    int16    in_units ;       /* As follows:
			       *   -4 = years
			       *   -3 = days
			       *   -2 = hours
			       *   -1 = minutes
			       *    0 = seconds
			       *    1 = 1/10 seconds
			       *    2 = 1/100 seconds
			       *    3 = milliseconds
			       *       etc.
			       */
    int16    high_unit_out ;  /* 0 -> hours, 1 -> minutes, 2 -> seconds,
			       * 3 -> 1/10 secs, 4 -> 1/100 secs, etc.
			       */
    int16    out_format ;     /* -1 -> hour:minute:second format
			       *  0 -> high unit only (no decimal point or
			       *       rounding.
			       *  1 -> high unit to 1 decimal point.
			       *  2 -> high unit to 2 decimal points.
			       *  etc.
			       */
    } ;
typedef struct gsh_axis_time_label_typ_struct  gsh_axis_time_label_typ ;

/* The following structure contains information to draw an axis and put
 * in major and minor ticks, scaled numbers on major ticks, and the axis
 * label.
 */
struct gsh_axis_definition_struct
    {
    double a1 ;          /* Value represented at the start of the axis. */
    double a2 ;          /* Value represented at the end of the axis.
                            a2 must be greater than a1. */
    double major_delta ; /* Size of major partitions (relative to (a1,a2)).
                            Must be greater than 0.0. */
    double major_offset ; /* Offset of first major partition from the axis start
                            (relative to (a1, a2)).  Must be non-negative. */
    int16 n_minor ;      /* Number of minor partitions per major partition.
                            Note that n_minor = 0 or 1 implies no tick marks,
                            2 inplies 1 tick mark, 3 implies 2 ticks, etc. */
    int     log_flag ;   /* Flag indicating if the axis scale is to be
                            logarithmic (base 10) or not.
                            log_flag = FALSE  ->  use a linear scale
                            log_flag = TRUE   ->  use a log10 scale */
    int     lnr_base ;   /* If "log_flag" is FALSE, this indicates which
			  * base we are dealing with.  "10" and "0" both
			  * indicate base 10.  Must be greater or equal
			  * than 0.
			  */
    wld_cds tick_hgt ;   /* Height of the major tick marks. */
    int     tick_ori ;   /* Flag indicating the orientation of the axis ticks.
                            tick_ori = FALSE  ->  counterclockwise
                            tick_ori = TRUE   ->  clockwise */
    int16   n_rpt ;      /* Indicates whether and what scale numbers on
                            the major tick marks are output.
                            n_rpt <= 0  ->  none are output.
                            n_rpt = 1  ->  every one is printed
                            n_rpt = 2  -> every 2'nd one is printed
                            n_rpt = 3  -> every 3'rd one is printed
                            etc. */
    int16   n_dec ;      /* Indicates the number of decimal places in the
                            scale numbers at the major tick marks:
                               n_dec > 0  ->  n decimal points
                               0    ->  decimal point but no following digits
                               n_dec < 0  ->  no decimal point */
    int16   end_flag ;   /* Indicates if to output the start and end
                            scale numbers on major ticks:
                               0 -> output both
                               1 -> suppress both
                               2 -> suppress the start number only
                               3 -> suppress the end number only */
    wld_cds num_hgt  ;   /* Height of characters for scale numbers. */
    int     num_ori ;    /* Indicates which side of the axis the text
                            is to be drawn on.
                                num_ori = FALSE  ->  counterclockwise
                                num_ori = TRUE   ->  clockwise */
    int16   num_just ;   /* Indicates how a scale number text box should
                            be justified with respect to its tick mark:
                                0 ->  centered
                                1 ->  left-justified
                                2 ->  right-justified */
    int16   num_rot ;    /* Indicates how the text should be rotated.
                                0 ->  parallel to the axis
                                -1 ->  rotated by +90 from the axis
                                 1 ->  rotated by -90 from the axis */
    gsh_axis_time_label_typ  *time_labelling  ;
			    /*  Indicates how and if time scales should be
				output instead of just numbers.
                                If this argument is NULL, there is no time
                                scaling and the output of scale numbers is
				handled by "n_dec" and the major tick numbers
				as calculated from "a1", "a2", "major_delta",
				and "major_offset".  Otherwise, these numbers
				are reset into time scales.  See the above
				description of "gsh_axis_time_label_typ"
				for more information. */
    char  *label_text ;  /* The axis-label text string. */
    int      label_suppress  ;  /* Indicates whether the label is to be output.
                                   FALSE -> output the label.
                                   TRUE -> don't output the label. */
    int16  label_rot ;   /* Indicates how the label should be rotated.
                            0 ->  parallel to the axis
                            -1 ->  rotated by +90 from the axis
                             1 ->  rotated by -90 from the axis */

    } ;
typedef struct gsh_axis_definition_struct  gsh_axis_definition ;

/* ------------------------------------------------------------------- */
/* General (non-device related) internal typedefs. */

/* This is the internal cursor structure. */
struct  cursor_info_strct
    {
    struct cursor_info_strct   *previous ; /* Cursor list back-pointer. */
    struct cursor_info_strct   *next ;     /* Cursor list forward-pointer. */
    int16		type ;     /* Type of Cursor:
			            *   0 = GSH_PIXMAP_CURSOR
			            *   1 = GSH_GLYPH_CURSOR
			            *   2 = GSH_FONT_CURSOR
			            */
    pixmap_typ		image ;    /* Cursor image pixmap (for
				    * GSH_PIXMAP_CURSOR only).
			            */
    pixmap_typ		mask ;     /* Cursor mask pixmap (for
				    * GSH_PIXMAP_CURSOR only).
			            */
    gsh_cds             width ;    /* cursor width. */
    gsh_cds             height ;   /* cursor height. */
    gsh_cds             x_hot ;    /* x-hotspot coordinate. */
    gsh_cds             y_hot ;    /* y-hotspot coordinate. */
    cursor_typ          id ;

#ifdef XWIN_A
    Cursor		 x_cursor ;
#endif /* XWIN_A */
    } ;

typedef struct cursor_info_strct  cursor_info_typ ;

/* These are required for X-Windows. */
#ifdef XWIN_A

/* Internal color RGB storage type. */
typedef int16        gsh_int_rgb ;

#endif /* XWIN_A */     

/*
 * a code_int must be able to hold 2**BITS values of type int,
 * and also -1
 */
typedef int             code_int;

/* For GIF output: */
struct gsh_gif_session_struct
    {
    int Width ;
    int Height ;
    int BitsPerPixel ;
    int nbytes_row ;
    int curx ;
    int cury;
    long CountDown;
    int Pass ;
    int Interlace;

    code_int free_ent ;        /* first unused entry */
    int clear_flg ;
    int offset;
    long in_count ;        /* length of input */
    long out_count;       /* # of codes output (for debugging) */

    int g_init_bits;
    FILE* g_outfile;

    int ClearCode;
    int EOFCode;

    unsigned long cur_accum ;
    int cur_bits ;

    int n_bits;            /* number of bits/code */
    int maxbits ;          /* user settable max # bits/code */
    code_int maxcode;      /* maximum code, given n_bits */
    code_int maxmaxcode ;  /* should NEVER generate this code */

    /*
     * Number of characters so far in this 'packet'
     */
    int a_count;

    /*
     * Added 2004/07/29:  the GNome window manager on i86 LINUX
     * returns xi_image->bitmap_bit_order as LSBFirst, and
     * not MSBFirst as on SOLARIS.  So we'd better have these
     * indicators around:
     */
    int byte_order;
    int bitmap_bit_order;

    /* Pointer to the actual image in the XImage structure - this
     * works on Sun, maybe not elsewhere:
     */
    unsigned char  *data_image ;

    /*
     * Define the storage for the packet accumulator
     */
    char accum[256] ;
    code_int htab [GIF_HSIZE];
    unsigned short codetab [GIF_HSIZE];

    } ;
typedef  struct gsh_gif_session_struct  gsh_gif_session ;

/* ------------------------------------------------------------------- */
/* Main graphics structure: */

/* This is the main internal information structure for a "gsh" session,
 * which would also directly correspond to a "gsh" thread.
 */
struct  gsh_session_strct
    {

    /* These fields are PUBLIC: */

    int    gsh_plot_only ;	  /* TRUE -> no display, only plots. */
    int    gsh_color_flag ;  /* TRUE -> color,  FALSE -> B & W */

    /* This holds the size of the color table. */
    gsh_col_typ   gsh_color_table_size ;

    /* This holds the number of bitplanes. */
    int16	gsh_number_bitplanes ;

    /* The current write mode. */
    gsh_mod_typ    gsh_current_write_mode ;

    /* This flag indicates if long format is needed for the particular
     * graphics output device (more than 16 bit resolution).  This
     * should happen only in * the case of a huge plotter (Calcomp,
     * Versatec).
     */
    int     gsh_long_device_fmt ;

    /* Suggested default line width - this will always be 0 EXCEPT
     * possibly when rendering a bitmap for plotter output.
     * In particular, Postscript width 1 pixel is too fine:
     */
    gsh_lin_wid	gsh_graph_line_width ;

    /* When plotting, this is the size, in gsh_cds, of a gsh-"pixel"
     * for the currently selected plotter:
     */
    gsh_cds    gsh_plt_pix_siz ;

    /* This points to a work area for expanded format. */
    gsh_xy_exp	*gsh_exp_dev_work ;

    /* gsh_ypx_xpx is the real ratio of ypixel density to xpixel
     * density.
     */
    double    gsh_ypx_xpx ;
	
    /* gsh_ypx_xpx_inv is the inverse of gsh_ypx_xpx */
    double	gsh_ypx_xpx_inv ;

    /* gsh_xpx_mil is the number of x-pixels per millimeter on the
     * display.
     */
    double	gsh_xpx_mil ;

    /* Factor of virtual pixels to real pixels when plotting,  This
     * is usually 1.0 and was added 2000/11/24 to allow for smaller
     * PostScript II pixel file sizes.
     */
    double	gsh_plt_pixel_expansion_factor ;
	
    gsh_col_typ    gsh_foreground_color ;
    gsh_col_typ    gsh_background_color ;
    gsh_msk_typ    gsh_full_mask ;

    gsh_lin_typ    gsh_current_line_style ;
    gsh_lin_wid    gsh_current_line_width ;
    gsh_msk_typ    gsh_write_mask ;
    gsh_col_typ    gsh_color ;

    /* Indicates which bitplanes are to be turned off (by color map
     * manipulation).
     */ 
    gsh_msk_typ    gsh_lvl_msk ;

    /* TRUE  ->  use gsh_lvl_msk to set the color map.
     * FALSE ->  ignore it.
     */
    int    gsh_use_lvl_msk ;

    /* Holds the current window's origin in absolute
     * device coordinates.
     */
    gsh_cds    gsh_win_x ;
    gsh_cds    gsh_win_y ;

    /* These store the current RGB color map values.  The RGB intensities
     * run from 0.0 (no contribution) to 1.0 (full intensity
     * contribution).
     */
    gsh_rgb_typ    *gsh_red ;
    gsh_rgb_typ    *gsh_green ;
    gsh_rgb_typ    *gsh_blue ;

    /* These are the colors in hsv coordinates.  See Foley and Van Dam,
     * pp. 616 for a description.
     */
    gsh_opc_typ    *gsh_cur_opc ;
    gsh_hue_typ	   *gsh_cur_hue ;
    gsh_sat_typ	   *gsh_cur_sat ;
    gsh_int_typ    *gsh_cur_int ;

    /* Current cursor. */
    cursor_typ     gsh_current_cursor ;

    window_typ  gsh_root_window ;    /* Structure holding root window. */
    window_typ  gsh_current_window ; /* Points to current draw window. */

    /* World-Space Variables: */

    /* Current world-to-screen transformation and inverse.
     * These are pointers only and are not allocated or deleted by
     * the "gsh" shell.
     */
    wld_2d_xfm   *wld_xfm ;
    wld_2d_xfm   *wld_inv_xfm ;

    /* Current world-space clipping rectangle.
     * These are pointers only and are not allocated or deleted by
     * the "gsh" shell.
     */
    wld_cds_xy   *wld_lleft ;
    wld_cds_xy   *wld_uright ;

    /* If the following flag is TRUE, all entities must be clipped
     * in world space by the software (e.g. this is done for all
     * plotters).  If FALSE, no clipping is required and we let
     * the output device do its own clipping in device space.
     * Note that when the world-to-device scale factor would cause
     * device coordinate overflows (zoomed-in a long way) by the
     * world-to-device transformation, then world space clipping
     * must also be used.
     */
    int    wld_clip_flag ;

    /* This flag indicates if we are using text-threshholding. */
    int    wld_text_thresh ;

    /* These flags indicate whether the line fonts should be
     * stroked (TRUE) or if the corresponding intrinsic device
     * line style should be used (FALSE).
     */
    int    wld_stroke_line_styles ;

    /* This flag indicates if thick lines should be "stroked" (i.e.
     * created as the appropriate series of filled polygons) (TRUE)
     * or if the intrinsic device thick lines should be used (FALSE).
     */
    int    wld_stroke_thick_lines ;

    /* This flag indicates whether or not text should be stroked
     * using the graphics software Hershey stroked font (TRUE) or
     * if the intrinsic device text should be used (FALSE).  As of
     * 3/89, all text will be stroked.
     */
    int    wld_to_stroke_text ;

    /* This holds the current line attributes. */
    wld_lin_att  wld_current_lin_att ;

    /* Points to current wld-window map. */
    /* NOTE that this is just a pointer - it is not allocated
     * by "gsh_construct_gsh_session".  It usually points to
     * a structure maintained by the calling application.  For
     * example, "GraphMap", "GraphMapBase" in SDT.
     */
    wld_map_typ  *wld_current_map ;

    /* Points to the root window. */
    window_typ  root_window ;

    /* This flag indicates whether or not display graphics should use
     * soft clipping.  Call "wld_display_clip_off" to turn off soft
     * clipping and call "wld_display_clip_on" to turn clipping on.
     * The default will be clipping "on" for display graphics:
     */
    int           wld_display_clip_flag ;

    /* This is actually a ptr to a "fnt_hdr_typ", but we don't
     * need to bring the definition of that structure to this
     * level.  However, we do need to store the pointer at this
     * level.
     */
    void          *wld_current_stroked_font ;

#ifdef WE_PROBABLY_DONT_NEED_THIS_991210
    wld_lin_fnt_typ   *wld_line_font1[WLD_MAX_FNT_1] ;
    wld_lin_fnt_typ   *wld_line_font2[WLD_MAX_FNT_2] ;
#endif /* WE_PROBABLY_DONT_NEED_THIS_991210 */

    /* Transform and its inverse for drawing the current text string. */
    wld_2d_xfm   wld_txt_xfm, wld_txt_inv_xfm ;

    /* The current value of world cds per x-pixel on the device. */
    wld_cds      wld_per_pixel ;

    /* The current line style. */
    int16        wld_line_style ;

    /* The current line width. */
    float        wld_line_width ;

    /* Line font info - the first four are unchangeable.  The last
     * (wld_user_dashed), is changeable and its line definition
     * is stored in:  "user_dashed_line_template":
     */
    wld_lin_def   wld_solid ;
    wld_lin_def   wld_dashed ;
    wld_lin_def   wld_dashed_dot ;
    wld_lin_def   wld_dashed_dot_dot ;
    wld_lin_def   wld_user_dashed ;
    float         *user_dashed_line_template ;


    /* These fields are PRIVATE: */

#ifdef XWIN_A

    Status  gsh_X_status ;
    Display *gsh_X_display ;
    int     gsh_X_screen ;
    Window  gsh_X_screen_root ;
    Window  gsh_X_root_window ;
    Window  gsh_X_current_window ;
    GC      gsh_X_GC ;
    GC      gsh_X_GC_save ;
    GC      gsh_X_GC_single_bitplane ;
    GC      gsh_X_image_GC ;
    Drawable   gsh_X_drawable ;
    XGCValues      gsh_X_GC_values ;
    int     gsh_X_border_width ;
    int     gsh_current_drawable_is_pixmap ;
    int     gsh_X_bitmapping_okay ;
    uint32  gsh_X_black_pixel ;
    uint32  gsh_X_white_pixel ;
    XWindowAttributes gsh_X_window_attributes ;

    Region gsh_X_clip_region ;

    XRectangle gsh_X_clip_rectangle[2] ;
    int gsh_X_clip_x_org, gsh_X_clip_y_org ;

    double gsh_screen_width_mm, gsh_screen_height_mm ;

#ifdef NEED_TO_DEFINE_CURSOR_SHAPE
     Pixmap  gsh_X_cursor_pixmap ;
#endif  /* NEED_TO_DEFINE_CURSOR_SHAPE */

    Cursor gsh_X_cursor ;

#ifndef XQUERYCOLOR_WORKS
    /* These store the current cursor foreground and
     * background colors.
     */
    XColor gsh_X_cursor_fg, gsh_X_cursor_bg ;
#endif /* XQUERYCOLOR_WORKS */

    Font gsh_X_text_font ;
    XFontStruct *gsh_X_text_font_struct ;
    gsh_cds gsh_X_text_y_offset ;

    /* This is used for gsh_write_pixel. */
    XImage  *write_pixel_image ;
    int32   write_pixel_pixel[10] ;

    /* Set to point to "write_pixel_pixel" in gsh_initialize. */
    char    *write_pixel_data ;

    /* Holds the type of polygon filling algorithm. */
    int     gsh_X_polygon_fill_type ;

    /* Here are the default textured line patterns for X-Windows. */
    int  gsh_dashed_len ;
    unsigned char gsh_dashed_texture[2] ;

    int  gsh_centerline_len ;
    unsigned char gsh_centerline_texture[4] ;

    int  gsh_phantom_len ;
    unsigned char gsh_phantom_texture[6] ;

    unsigned char *user_texture ;
    int user_line_dash_offset ;
    int user_texture_size ;

    /* Current X-Window line attributes: */
    unsigned int gsh_X_line_width ;
    int          gsh_X_line_style ;
    int          gsh_X_cap_style ;
    int          gsh_X_join_style ;

    /* Current X-Window logical function attribute: */
    int          gsh_X_draw_function ;

    /* Current X-Window visual in use. */
    Visual       *gsh_X_visual ;
    int          gsh_X_visual_class ;
    int          gsh_X_visual_map_entries ;

    /* Current X-Window colormap in use. */
    Colormap     gsh_X_colormap ;
    int          gsh_X_color_allocate ;

    /* Internal storage of current colors. */
    gsh_int_rgb  *gsh_int_red ;
    gsh_int_rgb  *gsh_int_green ;
    gsh_int_rgb  *gsh_int_blue ;

    /* This stores the current foreground cursor color. */
    gsh_col_typ  gsh_X_cursor_color ;

    /* Stores the current X pointer (i.e. mouse) location. */
    gsh_cds      gsh_X_pointer_x, gsh_X_pointer_y ;  

    /* The current X-Window event mask that all windows use. */
    uint32       gsh_X_selected_events ;

    /* This is the default format for x-images. */
    int          gsh_X_default_image_format ;

#endif /* XWIN_A */     

    /* The remaining fields in this structure are common to
     * all graphics:
     */
    double		gsh_rgb_scale ;

    /* The default text font file pathname: */
    char   *gsh_font_string ;

    char	*gsh_font_nam ;
    int16	pix_dep ;	/* Number of bitplanes in display. */
    int16	gsh_number_of_mouse_buttons ;

    /* Indicates if we are currently creating a (hardcopy) plot: */
    int     gsh_plot_flag ;

    /* Indicates if we need to use the plotter versions of the standard
     * drawing primitives.  Note that this is not synonymous with hardcopy.
     * We sometimes create a bitmap first (using a gsh_pixmap_typ) then
     * pixelize to the plotter:
     */
    int      gsh_use_plot_primitives ;

    int      gsh_top_down ;	/* TRUE -> top of device is 0. */
    int16    disp_size[4] ;
    int      gsh_current_cursor_status ;
    gsh_cds  gsh_window_left ;
    gsh_cds  gsh_window_top ;
    gsh_cds  gsh_window_width ;
    gsh_cds  gsh_window_height ;
    gsh_cds  gsh_clip_orgx, gsh_clip_orgy ;
    gsh_cds  gsh_clip_width, gsh_clip_height ;
    gsh_cds  gsh_current_window_x, gsh_current_window_y ;

    /* This points to the list of currently defined cursors. */
    cursor_info_typ   *gsh_internal_cursor_list ;

#ifdef  TEMP_DEL
    xfm_typ   xfm, xfm_inv, *sav_xfm, *sav_inv_xfm ;
    int       sav_clip_flag ;
#endif /* TEMP_DEL */

    /* Current plot color. */
    gsh_col_typ  gsh_plt_color ;

    /* Current size of the color map. */
    gsh_col_typ  gsh_col_map_siz ;

    /* Current size of the color map. */
    gsh_lin_wid  gsh_default_line_width ;

    /* Current width of Legend text line stoking: */
    gsh_lin_wid  gsh_legend_line_width ;

    /* Keyboard emulation of mouse flag. */
    int     gsh_emulate_mouse ;

    /* Flag indicating if the currently selected plotter has its own
     * internal gsh driver or if we are using another driver (e.g. a GSS-
     * supplied driver on the PC).
     */
    int     gsh_plotter_uses_our_driver ;

    /* PRINTING specifics: */

    /* General: */

    /* Minimum and Maximum current plotter device coordinate values. */
    gsh_cds     gsh_plot_min_x ;
    gsh_cds     gsh_plot_min_y ;
    gsh_cds     gsh_plot_max_x ;
    gsh_cds     gsh_plot_max_y ;

    /* These hold the pixel aspect ratio of the currently
     * selected plotter (and its inverse).
     */
    double      gsh_plot_ypx_xpx ;
    double      gsh_plot_ypx_xpx_inv ;

    int         gsh_shading_set ;

    /* File pointer for plotting. */
    FILE        *fp_plot ;

    /* The following flag is TRUE is a plot has been initialized but
     * not ended.  It is FALSE in all other cases.  This aids the
     * graphics shell in deciding whether or not it is necessary
     * to initialize a plotter when the current graphics window
     * is changed (in particular, if a different plot sub-window is
     * made the current graphics window).   1/6/90  jbv
     */
    int         plot_in_progress ;

    /* Current output file name: */
    char        plt_work_file[40] ;

    /* Current plot orientation.
     * Note that this value is the same as a window orientation
     * (values 0-7) wrt the parent window.  This variable is used
     * when plotting to align device output objects (i.e. geometric
     * objects that are not stroked but sent to the plotter as its
     * own object type) such as device text strings within the
     * plot window.
     */
    int16       plt_orientation ;

    /* These are set to the values of the corresponding GLOBAL
     * variables when this hardcopy session was started:
     */
    int         lcv_plt_dev_idx ;
    int         lcv_plt_paper_size ;
    int         lcv_plt_fit_type ;
    int         lcv_plt_destination ;
    int16       lcv_plt_vector_levels ;
    int16       lcv_plt_raster_levels ;
    char        *lcv_plt_dsk_fil ;
    char        *lcv_plt_hp_printcap_name ;
    char        *lcv_plt_ps_printcap_name ;

    /* Hardcopy Function pointers: */
    int         (*plt_internal_define_color_map) (
		    struct gsh_session_strct *gg,
                    int16 typ_colormap, int32 ncolors,
                    float *gray, float *redcyan,
                    float *greenyellow, float *bluemagenta,
                    int32 *pens) ;
    void        (*plt_set_clip_rectangle) (struct gsh_session_strct *gg,
                    gsh_cds orgx, gsh_cds orgy,
                    gsh_cds wid, gsh_cds hgt) ;
    void        (*plt_set_line_style) (struct gsh_session_strct *gg,
                    gsh_lin_typ lstyle) ;
    void        (*plt_polyline) (struct gsh_session_strct *gg,
                    gsh_cds_xy *pts, int32 n) ;
    void        (*plt_polygon) (struct gsh_session_strct *gg,
                    gsh_cds_xy *pts, int32 n, gsh_col_typ color) ;
    void        (*plt_set_color) (struct gsh_session_strct *gg,
                    gsh_col_typ color) ;
    void        (*plt_text) (struct gsh_session_strct *gg,
                    gsh_cds orgx, gsh_cds orgy, char *text) ;

    /* POSTSCRIPT: */
    int           plt_postscript_color_flag ;
    int           pscr_color_map_type ;
    int           pscr_number_colors ;
    float        *pscr_gray ;
    float        *pscr_redcyan ;
    float        *pscr_greenyellow ;
    float        *pscr_bluemagenta ;
    gsh_col_typ  current_pscript_color ;

    /* GIF: */
    gsh_gif_session   CurrentGifSession ;
    int           gif_ncolors ;
    float        *gif_redcyan ;
    float        *gif_greenyellow ;
    float        *gif_bluemagenta ;

    /* HPGL: */
    /* Current location of pen: */
    gsh_cds       hp_x_cur ;
    gsh_cds       hp_y_cur ;
    } ;
typedef struct gsh_session_strct  gsh_session ;


/* ------------------------------------------------------------------ */
/* Function declarations. */

#ifdef ANSI_STD_C

hextern int             gsh_startup (void) ;
hextern int             gsh_initialize_X_window (Display *dsp,
			    Window in_xwin, GC in_gc, Visual *in_visual,
			    int visual_preference,
			    int number_bitplanes_preference,
			    Visual **vis_ptr, gsh_session **ret_gg) ;
hextern int    		gsh_initialize (gsh_cds root_orgx,
			    gsh_cds root_orgy, gsh_cds root_width,
			    gsh_cds root_height, gsh_session **ret_gg) ;
hextern int    		gsh_initialize_plot_only (int use_x,
			    gsh_session **ret_gg) ;
hextern int    		gsh_close_plot_only (gsh_session *gg) ;
hextern Visual          *gsh_determine_X_visual (Display *dsp,
			    int vis_type, int depth,
			    XVisualInfo **vip) ;
hextern void		gsh_get_screen_size (gsh_session *gg,
			    gsh_cds *orgx, gsh_cds *orgy,
			    gsh_cds *width, gsh_cds *height) ;
hextern int16		gsh_get_screen_depth (gsh_session *gg) ;
hextern int16		gsh_get_color_table_size (gsh_session *gg) ;
hextern int    		gsh_set_text_font (gsh_session *gg,
			    char *fontname, int preserve) ;
hextern void		gsh_set_clip_rectangle (gsh_session *gg,
			    gsh_cds orgx,
			    gsh_cds orgy, gsh_cds wid, gsh_cds hgt) ;
hextern void		gsh_get_clip_rectangle (gsh_session *gg,
			    gsh_cds *orgx, gsh_cds *orgy,
			    gsh_cds *wid, gsh_cds *hgt) ;
hextern void		gsh_color_rect (gsh_session *gg,
			    gsh_cds orgx, gsh_cds orgy,
			    gsh_cds wid, gsh_cds hgt, 
			    gsh_col_typ color) ;
hextern gsh_pix_typ	gsh_read_pixel (gsh_session *gg,
			    gsh_cds x, gsh_cds y) ;
hextern void		gsh_write_pixel (gsh_session *gg,
			    gsh_cds x, gsh_cds y, gsh_pix_typ value) ;
hextern void		gsh_text (gsh_session *gg,
			    gsh_cds orgx, gsh_cds orgy, char *text) ;
hextern void		gsh_reversed_text (gsh_session *gg,
			    gsh_cds orgx, gsh_cds orgy, char *text) ;
hextern void  		gsh_get_text_font_metrics (gsh_session *gg,
			    gsh_cds *character_ascent,
    			    gsh_cds *character_descent,
			    gsh_cds *maximum_character_width) ;
hextern void  		gsh_get_text_extent (gsh_session *gg,
			    gsh_cds orgx, gsh_cds orgy,
			    char *text, gsh_cds_xy *ll, gsh_cds_xy *lr,
			    gsh_cds_xy *ur, gsh_cds_xy *ul) ;
hextern void  		gsh_clear_text (gsh_session *gg,
			    gsh_cds orgx, gsh_cds orgy, char *text) ;
hextern void		gsh_polyline (gsh_session *gg,
			    gsh_cds_xy *crd, int32 n) ;
hextern void		gsh_line_segment (gsh_session *gg,
			    gsh_cds_xy *crd) ;
hextern void		gsh_fill_polygon (gsh_session *gg,
			    gsh_cds_xy *crd, int32 n, gsh_col_typ color) ;
hextern void		gsh_reverse_video_rect (gsh_session *gg,
			    gsh_cds orgx, gsh_cds orgy,
			    gsh_cds wid, gsh_cds hgt) ;
hextern void		gsh_set_line_style (gsh_session *gg,
			    gsh_lin_typ lstyle) ;
hextern void		gsh_set_line_width (gsh_session *gg,
			    gsh_lin_wid lwidth) ;
hextern gsh_lin_typ	gsh_get_line_style (gsh_session *gg) ;
hextern gsh_lin_wid	gsh_get_line_width (gsh_session *gg) ;
hextern void		gsh_set_user_line_style (gsh_session *gg,
			    gsh_cds *dashes, int n, int offset) ;
hextern void		gsh_set_current_color (gsh_session *gg,
			    gsh_col_typ color) ;
hextern void		gsh_set_current_write_mask (gsh_session *gg,
			    gsh_msk_typ mask) ;
hextern gsh_col_typ	gsh_get_current_color (gsh_session *gg) ;
hextern gsh_msk_typ	gsh_get_current_write_mask (gsh_session *gg) ;
hextern void            gsh_set_current_background_color (gsh_session *gg,
			    gsh_col_typ color) ;
hextern gsh_col_typ     gsh_get_background_color (gsh_session *gg) ;
hextern void		gsh_set_color_level_on (int16 level) ;
hextern void		gsh_set_cursor_color (gsh_session *gg,
			    gsh_col_typ col_idx) ;
hextern void		gsh_hsv_rgb (gsh_session *gg,
			    gsh_col_typ col_map_siz,
			    gsh_col_typ low_entity,
			    gsh_hue_typ *hue, gsh_int_typ *intensity,
			    gsh_sat_typ *saturation) ;
hextern void		gsh_set_color_map (gsh_session *gg,
			    int16 num_bit_pln,
			    int16 low_bit_pln, gsh_opc_typ *opc_lst,
			    gsh_hue_typ *hue_lst, gsh_sat_typ *sat_lst,
			    gsh_int_typ *int_lst) ;
hextern void		gsh_set_device_color_map (gsh_session *gg,
			    gsh_col_typ siz,
			    gsh_rgb_typ *red, gsh_rgb_typ *green,
			    gsh_rgb_typ *blue) ;
hextern void		gsh_filter_color_map (gsh_session *gg,
			    gsh_col_typ  col_map_siz, int16 num_pln) ;
#ifdef NOT_NEEDED_WITH_GUI
hextern void		gsh_get_input (void) ;
#endif /* NOT_NEEDED_WITH_GUI */
hextern void		gsh_ring_bell (gsh_session *gg) ;
hextern int16		gsh_get_number_of_mouse_buttons (
			    gsh_session *gg) ;
#ifdef NOT_NEEDED_WITH_GUI
hextern int    		gsh_is_input_pending (void) ;
#endif /* NOT_NEEDED_WITH_GUI */
hextern void		gsh_begin_interrupt_control (void) ;
hextern void		gsh_end_interrupt_control (void) ;
hextern void		gsh_lock (void) ;
hextern void		gsh_unlock (void) ;
hextern void		gsh_flush (gsh_session *gg) ;
hextern void		gsh_close (void) ;
hextern int    		gsh_plot_on (gsh_session *gg) ;
hextern void		gsh_set_cursor_status (gsh_session *gg,
			    int cursor_flag) ;
hextern int    		gsh_get_cursor_status (gsh_session *gg) ;
hextern int    		gsh_set_current_cursor (gsh_session *gg,
			    cursor_typ cursor) ;
hextern cursor_typ	gsh_get_current_cursor (gsh_session *gg) ;
hextern cursor_typ	gsh_create_cursor (gsh_session *gg,
			    pixmap_typ image, pixmap_typ mask,
			    gsh_cds x_hot, gsh_cds y_hot) ;
hextern int    		gsh_destroy_cursor (gsh_session *gg,
			    cursor_typ cursor) ;
hextern void		gsh_set_write_mode (gsh_session *gg,
			    gsh_mod_typ write_mode) ;
hextern gsh_mod_typ	gsh_get_write_mode (gsh_session *gg) ;
hextern void		gsh_end_plot (gsh_session *gg,
			    gsh_drawable_typ draw_id) ;
hextern int    		gsh_set_current_plotter (int16 plotter_type) ;
#ifdef NOT_NEEDED_WITH_GUI
hextern int    		gsh_get_control_c (void) ;
#endif /* NOT_NEEDED_WITH_GUI */
hextern void            gsh_begin_no_delay (void) ;
hextern void            gsh_end_no_delay (void) ;

hextern int             gsh_complete_time_axis_setup (
			    gsh_axis_definition *axis_def,
			    double *a1, double *a2) ;
hextern void            gsh_copy_axis_time_label_typ (
                            gsh_axis_time_label_typ *src,
                            gsh_axis_time_label_typ *dest) ;
hextern int             gsh_bitmapping_available (gsh_session *gg) ;

/* User accessible plot routines. */
hextern void		plt_initialize (void) ;
hextern int             plt_get_pixels_mm (gsh_session *gg,
			    int16 plotter, int16 paper_size,
			    double *xpx_mm, double *ypx_mm) ;
hextern int             plt_get_paper_size_mm (int16 paper_size,
			    double *xlen, double *ylen) ;
hextern void            plt_set_network_plotter (char *plotter_name) ;
hextern int     	plt_set_paper_orientation (int16 orientation) ;
hextern int     	plt_set_plot_destination (int16 destination) ;
hextern int     	plt_set_plot_file (char *filename) ;
hextern int             plt_define_color_map (
                            int16 typ_colormap, int32 ncolors,
                            float *gray, float *redcyan,
                            float *greenyellow, float *bluemagenta,
                            int32 *pens) ;

hextern int             gsh_init_plotter (gsh_session *gg) ;
hextern void	        gsh_close_plotter (gsh_session *gg,
	                    gsh_drawable_typ draw_id) ;

hextern int             plt_set_pixel_type_plot_width (gsh_cds wid) ;
hextern int             plt_set_pixel_type_plot_height (gsh_cds hgt) ;
hextern gsh_cds         plt_get_pixel_type_plot_width (void) ;
hextern gsh_cds         plt_get_pixel_type_plot_height (void) ;

hextern int             plt_adjust_graph_line_width_factor (double lfac) ;
hextern double          plt_get_graph_line_width_factor (void) ;

hextern int             plt_otype_has_scalable_linear_resolution (
			    int otype) ;
hextern int             plt_otype_has_adjustable_graphics_line_width (
			    int otype) ;
hextern int             plt_render_bit_map (gsh_session *gg,
			    gsh_drawable_typ draw_id,
                            gsh_cds orgx, gsh_cds orgy,
			    gsh_cds wid, gsh_cds hgt,
			    wld_cds_rect *page_location, int orient) ;
hextern int             plt_render_entire_bit_map (gsh_session *gg,
			    gsh_drawable_typ draw_id) ;

/* Windows. */
hextern window_typ	win_create_window (gsh_session *gg,
			    window_typ parent,
			    gsh_cds_xy *origin, gsh_cds width,
			    gsh_cds height, gsh_cds border_width,
			    gsh_msk_typ mask) ;
hextern window_typ	win_create_v_window (window_typ parent,
			    gsh_cds_xy *origin, gsh_cds width,
			    gsh_cds height, gsh_cds border_width,
			    gsh_msk_typ mask) ;
hextern void		win_destroy_window (gsh_session *gg,
			    window_typ window) ;
hextern void		win_clear_window (gsh_session *gg,
			    window_typ window) ;
hextern void		win_clear_area (gsh_session *gg,
			    window_typ window, gsh_cds orgx,
			    gsh_cds orgy, gsh_cds width, gsh_cds height,
			    int exposure) ;
hextern int    		win_set_current_graphics_window (
			    gsh_session *gg, window_typ window,
			    gsh_cds x_origin, gsh_cds y_origin) ;
hextern window_typ	win_get_current_graphics_window (
			    gsh_session *gg) ;
hextern int             win_get_window_size (window_typ window,
			    gsh_cds *orgx, gsh_cds *orgy, gsh_cds *width,
			    gsh_cds *height, gsh_cds *border_width,
			    gsh_msk_typ *mask) ;

hextern int    		win_map_window (gsh_session *gg,
			    window_typ window) ;
hextern int    		win_unmap_window (gsh_session *gg,
			    window_typ window) ;
hextern int    		win_raise_window (gsh_session *gg,
			    window_typ window) ;
hextern int    		win_map_raised (gsh_session *gg,
			    window_typ window) ;

/* For plotting. */
hextern window_typ	win_create_plot_window (gsh_session *gg,
			    int16 plotter, int16 paper_size,
			    int use_pixmap, double pct_res,
			    double pixel_expansion_factor) ;

hextern int      	win_close_plot_window (gsh_session *gg,
			    gsh_drawable_typ draw_id) ;

/* For Images. */
hextern gsh_image_typ   *gsh_store_image (gsh_session *gg,
			    window_typ window,
			    gsh_cds_xy *origin, gsh_cds width,
			    gsh_cds height, gsh_msk_typ mask) ;
hextern int    		gsh_restore_image (gsh_session *gg,
			    gsh_image_typ *image) ;
hextern int             gsh_destroy_image (gsh_session *gg,
			    gsh_image_typ *image) ;

/* For Pixmaps. */
hextern pixmap_typ 	gsh_create_pixmap (gsh_session *gg,
			    gsh_cds width, gsh_cds height, int16 depth) ;
hextern pixmap_typ	gsh_create_pixmap_from_bitmap (gsh_session *gg,
			    gsh_cds width,
			    gsh_cds height, int16 depth, gsh_col_typ fg,
			    gsh_col_typ bg, char *data) ;
hextern void		gsh_destroy_pixmap (gsh_session *gg,
			    pixmap_typ pixmap_index) ;
hextern int     	gsh_copy_area (gsh_session *gg,
			    gsh_drawable_typ src,
			    gsh_cds src_orgx, gsh_cds src_orgy,
			    gsh_cds width, gsh_cds height,
			    gsh_drawable_typ dest, gsh_cds dest_orgx,
			    gsh_cds dest_orgy) ;

hextern int             gsh_get_date_time_stamp (
			    gsh_axis_time_label_typ *tdef,
                            double ival, int *year, int *month,
			    int *mday, int32 *julian_day, int *hour,
			    int *minute, int *second, double *psecs) ;

hextern int    		wld_polyline (gsh_session *gg,
			    wld_cds_xy *pts, int32 n) ;
hextern int    		wld_segment (gsh_session *gg,
			    wld_cds_xy *pt0, wld_cds_xy *pt1) ;
hextern int    		wld_fill_polygon (gsh_session *gg,
			    wld_cds_xy *pts, int32 n, gsh_col_typ color) ;

hextern int    		wld_symbol (gsh_session *gg, char *text,
			    wld_cds_xy *pt, wld_cds height,
			    double angle) ;
hextern int    		wld_symbol_extent (gsh_session *gg, char *text,
			    wld_cds_xy *pt, wld_cds height,
			    double angle, wld_cds_xy *lleft,
			    wld_cds_xy *lright, wld_cds_xy *uright,
			    wld_cds_xy *uleft) ;
hextern int             wld_stroke_symbol (gsh_session *gg,
			    char *text, wld_cds_xy *pt,
			    wld_cds height, wld_cds width,
                            int space_flag, int16 v_just, int16 h_just,
			    double angle) ;
hextern int             wld_stroke_symbol_extent (gsh_session *gg,
			    char *text, wld_cds_xy *pt,
                            wld_cds height, wld_cds width, int space_flag,
			    int16 v_just, int16 h_just, double angle,
                            wld_cds_xy *lleft, wld_cds_xy *lright,
			    wld_cds_xy *uright, wld_cds_xy *uleft) ;
hextern int    		wld_device_text (gsh_session *gg,
			    char *text, wld_cds_xy *pt) ;
hextern int    		wld_device_reversed_text (gsh_session *gg,
			    char *text, wld_cds_xy *pt) ;
hextern int    		wld_clear_device_text (gsh_session *gg,
			    char *text, wld_cds_xy *pt) ;
hextern int    		wld_text (gsh_session *gg, char *text,
			    wld_cds_xy *pt, wld_cds height,
			    double angle) ;
hextern int    		wld_stroke_text (gsh_session *gg, char *text,
			    wld_cds_xy *pt, wld_cds height,
			    wld_cds width, double angle, double slant,
			    int rotation, int space_flag, int16 mirror) ;
hextern int    		wld_text_extent (gsh_session *gg, char *text,
			    wld_cds_xy *pt, wld_cds height, double angle,
			    wld_cds_xy *ll, wld_cds_xy *lr,
			    wld_cds_xy *ur, wld_cds_xy *ul) ;
hextern int    		wld_stroke_text_extent (gsh_session *gg,
			    char *text, int count, wld_cds_xy *start,
			    wld_cds height, wld_cds width,
			    double angle, double slant, int rotation,
			    int space_flag, int16 mirror,
			    wld_cds_xy *lleft, wld_cds_xy *lright,
			    wld_cds_xy *uright, wld_cds_xy *uleft) ;

hextern int    		wld_initialize (gsh_cds root_orgx,
			    gsh_cds root_orgy, gsh_cds root_width,
			    gsh_cds root_height, gsh_session **ret_gg) ;
hextern int    		wld_initialize_plot_only (int use_x,
			    gsh_session **ret_gg) ;
hextern void		wld_set_current_line_width (gsh_session *gg,
			    wld_cds width) ;
hextern int    		wld_set_current_line_style (gsh_session *gg,
			    int16 line_style) ;
hextern int    		wld_rescale_dashes (gsh_session *gg,
			    int16 line_style, float scale) ;
hextern int    		wld_set_user_line_style (gsh_session *gg,
			    int16  seg_cnt, float *seg_lst) ;
hextern int    		wld_set_current_stroked_font (gsh_session *gg,
			    int16 font) ;

hextern int    		wld_map_device_window (gsh_session *gg,
			    window_typ window,
			    gsh_cds_xy *offset, gsh_cds width,
			    gsh_cds height, wld_cds_xy *wll,
			    wld_cds_xy *wur, int preserve_aspect,
			    int rotate_drawing, wld_map_typ *map) ;
hextern int    		wld_map_window (gsh_session *gg,
			    window_typ window, double win_llx,
			    double win_lly, double win_urx,
			    double win_ury, wld_cds_xy *wll,
			    wld_cds_xy *wur, int preserve_aspect,
			    int rotate_drawing, wld_map_typ *map) ;
hextern int    		wld_map_on_map (gsh_session *gg,
			    wld_map_typ *input_map,
			    wld_cds_xy *map_lleft, wld_cds_xy *map_uright,
			    wld_cds_xy *wll, wld_cds_xy *wur,
			    int preserve_aspect, int rotate_drawing,
			    wld_map_typ *map) ;
hextern int    		wld_use_map (gsh_session *gg, wld_map_typ *map) ;
hextern wld_cds		wld_return_wld_mm (wld_map_typ *map, double mms) ;
hextern void		wld_fit_in_device_rect (gsh_session *gg,
			    gsh_cds_xy *origin,
			    gsh_cds width, gsh_cds height,
			    wld_cds_xy *lleft, wld_cds_xy *uright,
			    int16 orientation, int preserve_aspect,
			    int rotate_drawing, wld_map_typ *map) ;

hextern void		wld_2d_xfm_pt (wld_cds x, wld_cds y,
			    wld_cds_xy *new_pt, wld_2d_xfm *xfm) ;
hextern int    		gsh_to_wld_xform (wld_map_typ *map,
			    gsh_cds_xy *in_point,
			    wld_cds_xy *transformed_point) ;
hextern int    		wld_to_gsh_xform (wld_map_typ *map,
			    wld_cds_xy *in_point,
			    gsh_cds_xy *transformed_point) ;

hextern int    		wld_draw_axis (gsh_session *gg,
			    gsh_axis_definition *axis_def,
			    wld_cds_xy *start, wld_cds_xy *end) ;
hextern int    		wld_draw_box (gsh_session *gg,
			    gsh_axis_definition *x_axis_def,
			    gsh_axis_definition *y_axis_def,
			    wld_cds_xy *lleft, wld_cds_xy *uright) ;
hextern int    		wld_draw_grid (gsh_session *gg,
			    gsh_axis_definition *x_axis_def,
			    gsh_axis_definition *y_axis_def,
			    gsh_lin_typ maj_lstyle, gsh_lin_typ min_lstyle,
			    wld_cds_xy *lleft, wld_cds_xy *uright) ;
hextern int    		wld_get_axis_time_string (
			    gsh_axis_time_label_typ *ltype,
			    double inval, char *str) ;

hextern void		wld_display_clip_off (gsh_session *gg) ;
hextern void		wld_display_clip_on (gsh_session *gg) ;

hextern int             wld_point_in_map (wld_cds_xy *pt,
			    wld_map_typ *map) ;

hextern int    		prt_set_print_destination (int16 destination) ;
hextern int    		prt_set_print_file (char *filename) ;
hextern int             prt_set_print_device (char *devicename) ;
hextern int    		prt_print_file (FILE *infile_fp,
			    int32 start, int32 end) ;

#ifdef NOT_NEEDED_WITH_GUI
/* This routine must be provided by the programmer, even if it does
 * nothing!
 */
hextern void		event_routine (int16 et, gsh_cds x, gsh_cds y,
			    int16 pressed, int16 new_key,
			    int16 new_but, int16 *quit) ;	
#endif /* NOT_NEEDED_WITH_GUI */

hextern gsh_session     *gsh_construct_gsh_session () ;
hextern int             gsh_destruct_gsh_session (gsh_session *gg) ;
hextern int             gsh_clear_gsh_session (gsh_session *gg) ;

#else

hextern int             gsh_startup () ;
hextern int             gsh_initialize_X_window () ;
hextern int    		gsh_initialize () ;
hextern int    		gsh_initialize_plot_only () ;
hextern int    		gsh_close_plot_only () ;
hextern Visual          *gsh_determine_X_visual () ;
hextern void		gsh_get_screen_size () ;
hextern int16		gsh_get_screen_depth () ;
hextern int16		gsh_get_color_table_size () ;
hextern int    		gsh_set_text_font () ;
hextern void		gsh_set_clip_rectangle () ;
hextern void		gsh_get_clip_rectangle () ;
hextern void		gsh_color_rect () ;
hextern gsh_pix_typ	gsh_read_pixel () ;
hextern void		gsh_write_pixel () ;
hextern void		gsh_text () ;
hextern void		gsh_reversed_text () ;
hextern void  		gsh_get_text_font_metrics () ;
hextern void  		gsh_get_text_extent () ;
hextern void  		gsh_clear_text () ;
hextern void		gsh_polyline () ;
hextern void		gsh_line_segment () ;
hextern void		gsh_fill_polygon () ;
hextern void		gsh_reverse_video_rect () ;
hextern void		gsh_set_line_style () ;
hextern void		gsh_set_line_width () ;
hextern gsh_lin_typ	gsh_get_line_style () ;
hextern gsh_lin_wid	gsh_get_line_width () ;
hextern void		gsh_set_user_line_style () ;
hextern void		gsh_set_current_color () ;
hextern void		gsh_set_current_write_mask () ;
hextern gsh_col_typ	gsh_get_current_color () ;
hextern gsh_msk_typ	gsh_get_current_write_mask () ;
hextern void            gsh_set_current_background_color () ;
hextern gsh_col_typ     gsh_get_background_color () ;
hextern void		gsh_set_color_level_on () ;
hextern void		gsh_set_cursor_color () ;
hextern void		gsh_hsv_rgb () ;
hextern void		gsh_set_color_map () ;
hextern void		gsh_set_device_color_map () ;
hextern void		gsh_filter_color_map () ;
#ifdef NOT_NEEDED_WITH_GUI
hextern void		gsh_get_input () ;
#endif /* NOT_NEEDED_WITH_GUI */
hextern void		gsh_ring_bell () ;
hextern int16		gsh_get_number_of_mouse_buttons () ;
#ifdef NOT_NEEDED_WITH_GUI
hextern int    		gsh_is_input_pending () ;
#endif /* NOT_NEEDED_WITH_GUI */
hextern void		gsh_begin_interrupt_control () ;
hextern void		gsh_end_interrupt_control () ;
hextern void		gsh_lock () ;
hextern void		gsh_unlock () ;
hextern void		gsh_flush () ;
hextern void		gsh_close () ;
hextern int    		gsh_plot_on () ;
hextern void		gsh_set_cursor_status () ;
hextern int    		gsh_get_cursor_status () ;
hextern int    		gsh_set_current_cursor () ;
hextern cursor_typ	gsh_get_current_cursor () ;
hextern cursor_typ	gsh_create_cursor () ;
hextern int    		gsh_destroy_cursor () ;
hextern void		gsh_set_write_mode () ;
hextern gsh_mod_typ	gsh_get_write_mode () ;
hextern void		gsh_end_plot () ;
hextern int    		gsh_set_current_plotter () ;
#ifdef NOT_NEEDED_WITH_GUI
hextern int    		gsh_get_control_c () ;
#endif /* NOT_NEEDED_WITH_GUI */
hextern void            gsh_begin_no_delay () ;
hextern void            gsh_end_no_delay () ;

hextern int             gsh_complete_time_axis_setup () ;
hextern void            gsh_copy_axis_time_label_typ () ;
hextern int             gsh_bitmapping_available () ;

/* User accessible plot routines. */
hextern void		plt_initialize () ;
hextern int             plt_get_pixels_mm () ;
hextern int             plt_get_paper_size_mm () ;
hextern void            plt_set_network_plotter () ;
hextern int     	plt_set_paper_orientation () ;
hextern int     	plt_set_plot_destination () ;
hextern int     	plt_set_plot_file () ;

hextern int             gsh_init_plotter () ;
hextern void	        gsh_close_plotter () ;

hextern int             plt_set_pixel_type_plot_width () ;
hextern int             plt_set_pixel_type_plot_height () ;
hextern gsh_cds         plt_get_pixel_type_plot_width () ;
hextern gsh_cds         plt_get_pixel_type_plot_height () ;
hextern int             plt_adjust_graph_line_width_factor () ;
hextern double          plt_get_graph_line_width_factor () ;
hextern int             plt_otype_has_scalable_linear_resolution () ;
hextern int             plt_otype_has_adjustable_graphics_line_width () ;
hextern int             plt_render_bit_map () ;
hextern int             plt_render_entire_bit_map () ;

/* Windows. */
hextern window_typ	win_create_window () ;
hextern window_typ	win_create_v_window () ;
hextern void		win_destroy_window () ;
hextern void		win_clear_window () ;
hextern void		win_clear_area () ;
hextern int    		win_set_current_graphics_window () ;
hextern window_typ	win_get_current_graphics_window () ;
hextern int             win_get_window_size () ;

hextern int    		win_map_window () ;
hextern int    		win_unmap_window () ;
hextern int    		win_raise_window () ;
hextern int    		win_map_raised () ;

/* For plotting. */
hextern window_typ	win_create_plot_window () ;
hextern int      	win_close_plot_window () ;

/* For Images. */
hextern gsh_image_typ   *gsh_store_image () ;
hextern int             gsh_restore_image () ;
hextern int             gsh_destroy_image () ;

/* For Pixmaps. */
hextern pixmap_typ 	gsh_create_pixmap () ;
hextern pixmap_typ	gsh_create_pixmap_from_bitmap () ;
hextern void		gsh_destroy_pixmap () ;
hextern int     	gsh_copy_area () ;

hextern int             gsh_get_date_time_stamp () ;

hextern int    		wld_polyline () ;
hextern int    		wld_segment () ;
hextern int    		wld_fill_polygon () ;

hextern int    		wld_symbol () ;
hextern int    		wld_symbol_extent () ;
hextern int             wld_stroke_symbol () ;
hextern int             wld_stroke_symbol_extent () ;
hextern int    		wld_device_text () ;
hextern int    		wld_device_reversed_text () ;
hextern int    		wld_clear_device_text () ;
hextern int    		wld_text () ;
hextern int    		wld_stroke_text () ;
hextern int    		wld_text_extent () ;
hextern int    		wld_stroke_text_extent () ;

hextern int    		wld_initialize () ;
hextern int    		wld_initialize_plot_only () ;
hextern void		wld_set_current_line_width () ;
hextern int    		wld_set_current_line_style () ;
hextern int    		wld_rescale_dashes () ;
hextern int    		wld_set_user_line_style () ;
hextern int    		wld_set_current_stroked_font () ;

hextern int    		wld_map_device_window () ;
hextern int    		wld_map_window () ;
hextern int    		wld_map_on_map () ;
hextern int    		wld_use_map () ;
hextern wld_cds		wld_return_wld_mm () ;
hextern void		wld_fit_in_device_rect () ;

hextern void		wld_2d_xfm_pt () ;
hextern int    		gsh_to_wld_xform () ;
hextern int    		wld_to_gsh_xform () ;

hextern int    		wld_draw_axis () ;
hextern int    		wld_draw_box () ;
hextern int    		wld_draw_grid () ;
hextern int    		wld_get_axis_time_string () ;

hextern void		wld_display_clip_off () ;
hextern void		wld_display_clip_on () ;

hextern int             wld_point_in_map () ;

hextern int    		prt_set_print_destination () ;
hextern int    		prt_set_print_file () ;
hextern int             prt_set_print_device () ;
hextern int    		prt_print_file () ;

#ifdef NOT_NEEDED_WITH_GUI
/* This routine must be provided by the programmer, even if it does
 * nothing!
 */
hextern void		event_routine () ;
#endif /* NOT_NEEDED_WITH_GUI */

hextern gsh_session     *gsh_construct_gsh_session () ;
hextern int             gsh_destruct_gsh_session () ;
hextern int             gsh_clear_gsh_session () ;

#endif /* ANSI_STD_C */

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif

#endif  /* GSH_H */

/* ------------------------------------------------------------------ */
/* ---- End of File:  gsh.h ---- */ 
