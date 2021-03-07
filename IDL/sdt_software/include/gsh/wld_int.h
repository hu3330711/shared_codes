/* ------------------------------------------------------------- */
/*
 * wld_int.h
 *
 * These are the declarations required internally by the "wld"
 * modules.
 *
 */
#ifndef WLD_INT_H
#define WLD_INT_H

#ifndef SUNOS4
#ifdef __cplusplus
extern "C" {
#endif
#endif

#include "gsh.h"
#include "fnt.h"

/* For SCCS */
#define SccsId_wld_int_h "@(#)wld_int.h	1.4, 12/11/03"

/* ------------------------------------------------------------- */
/* Constants */

/* Used for line font rendering. */
#define WLD_BIT_MSK			0x80
#define WLD_BIT_MSK_SIZ			8
#define HORIZONTAL_SLOPE		0
#define VERTICAL_SLOPE			1
#define GENERAL_SLOPE			2

#define WLD_ZERO_TOL                    .00001

/* Maximum number of points in a world space polygon. */
#define WLD_MAX_PTS			362

/* Maximum number of segments in a world space line font. */
#define MAX_WLD_LINE_STYLE_SEGMENTS	32



#ifdef WE_PROBABLY_DONT_NEED_THIS_991210

/* Maximum number of line fonts of types 1 and 2. */
#define WLD_MAX_FNT_1		32
#define WLD_MAX_FNT_2		32

/* ------------------------------------------------------------- */
/* Typedefs */
/*  TEMP!!!!!!! */
typedef int wld_lin_fnt_typ ;

#endif /* WE_PROBABLY_DONT_NEED_THIS_991210 */



/* ------------------------------------------------------------- */
/* Variables */

#ifdef WE_PROBABLY_DONT_NEED_THIS_991210

/* Pointers to line font definitions. */
hextern	wld_lin_fnt_typ   *wld_line_font1[WLD_MAX_FNT_1] ;
hextern	wld_lin_fnt_typ   *wld_line_font2[WLD_MAX_FNT_2] ;

#endif /* WE_PROBABLY_DONT_NEED_THIS_991210 */

/* ------------------------------------------------------------- */
/* Routines. */

#ifdef ANSI_STD_C

hextern void get_intercepts (wld_cds_xy *pre, wld_cds_xy *cur,
    wld_cds_xy *pst, int16 flg, wld_cds wid, wld_cds_xy *rgt,
    wld_cds_xy *lft, double *slope, int16 *m_type) ;
hextern void get_intersection (double m0, int16 t0, double intcp0,
    double m1, int16 t1, double intcp1, wld_cds_xy *def_pt,
    wld_cds_xy *pt) ;
hextern void get_y_intercept (wld_cds_xy  *pt, double m, int16 type,
    double *intcpt) ;
hextern void get_slope (wld_cds_xy *start, wld_cds_xy *end,
    wld_cds_xy *pos, double wid, int16 *type, double *m,
    wld_cds_xy *rgt, wld_cds_xy *lft) ;
hextern int     start_point (wld_cds_xy *oldp1, wld_cds_xy *p2,
    wld_cds_xy *lower_left, wld_cds_xy *upper_right,
    wld_cds_xy *newp1) ;
hextern int     next_point (wld_cds_xy *p1, wld_cds_xy *oldp2,
    wld_cds_xy *lower_left, wld_cds_xy *upper_right,
    wld_cds_xy *newp2) ;
hextern void clip_point (wld_cds_xy *oldpt, wld_cds_xy *pt,
    wld_cds_xy *lower_left, wld_cds_xy *upper_right,
    double deltax, double deltay) ;
hextern void clip_polygon (wld_cds_xy *in_pts, int in_n,
    wld_cds_xy *out_pts, int *out_n, wld_cds_xy *lower_left,
    wld_cds_xy *upper_right) ;
hextern int     x_in_window (wld_cds x, wld_cds_xy *ll, wld_cds_xy *ur) ;
hextern int     y_in_window (wld_cds y, wld_cds_xy *ll, wld_cds_xy *ur) ;
hextern void wld_temp_clip_window (wld_cds_xy *lower_left,
    wld_cds_xy *upper_right, wld_cds_xy *out_lleft,
    wld_cds_xy *out_uright, wld_2d_xfm *xfm) ;

hextern void initialize_2d_matrix (wld_2d_xfm *xfm) ;
hextern void invert_2d_matrix (wld_2d_xfm *xfm, wld_2d_xfm *inv_xfm) ;
hextern void multiply_2d_matrices (wld_2d_xfm *xfm1, wld_2d_xfm *xfm2,
    wld_2d_xfm *xfm) ;
hextern void reflect_2d_matrix_x (wld_2d_xfm *xfm) ;
hextern void reflect_2d_matrix_y (wld_2d_xfm *xfm) ;
hextern void rotate_2d_matrix (double angle, wld_2d_xfm *xfm,
    int16 flag) ;
hextern void scale_2d_matrix (double xscale, double yscale,
    wld_2d_xfm *xfm) ;
hextern void translate_2d_matrix (double x, double y, wld_2d_xfm *xfm) ;
hextern void set_matrix_type (wld_2d_xfm *xfm) ;
hextern void set_xfm (double cx, double cy, int16 orientation,
    double xscale, double yscale, double dx, double dy,
    int invert_vert, int top_down, wld_2d_xfm *xfm) ;

hextern int     wld_scale_dashes (wld_lin_def *style_ptr,
    float *dash_template, float scale) ;
hextern void wld_set_device_clip_bounds (gsh_session *gg,
    wld_map_typ *map, wld_cds_xy *wll, wld_cds_xy *wur,
    gsh_cds_xy *origin, gsh_cds width, gsh_cds height,
    int plot_flag) ;
hextern int      remap_to_root_window (gsh_session *gg,
    window_typ win, wld_cds_xy *lleft, wld_cds_xy *uright,
    wld_2d_xfm *xfm, int preserve_aspect, int rotate,
    wld_map_typ *out_map) ;

hextern void wldx_thick_solid (gsh_session *gg,
    wld_cds_xy *pts, int32 n, wld_cds wid,
    wld_cds_xy *prept, wld_cds_xy *pstpt) ;
hextern void wldx_fonted_line (gsh_session *gg,
    wld_cds_xy *pts, int32 n, wld_cds wid,
    int thick_flag, wld_cds_xy *prept, wld_cds_xy *pstpt,
    int in_seg, int *out_seg, double in_lseg,
    double *out_lseg, int end_flag) ;

#else

hextern void get_intercepts () ;
hextern void get_intersection () ;
hextern void get_y_intercept () ;
hextern void get_slope () ;
hextern int     start_point () ;
hextern int     next_point () ;
hextern void clip_point () ;
hextern void clip_polygon () ;
hextern int     x_in_window () ;
hextern int     y_in_window () ;
hextern void wld_temp_clip_window () ;

hextern void initialize_2d_matrix () ;
hextern void invert_2d_matrix () ;
hextern void multiply_2d_matrices () ;
hextern void reflect_2d_matrix_x () ;
hextern void reflect_2d_matrix_y () ;
hextern void rotate_2d_matrix () ;
hextern void scale_2d_matrix () ;
hextern void translate_2d_matrix () ;
hextern void set_matrix_type () ;
hextern void set_xfm () ;

hextern int     wld_scale_dashes () ;
hextern void wld_set_device_clip_bounds () ;
hextern int      remap_to_root_window () ;

hextern void wldx_thick_solid () ;
hextern void wldx_fonted_line () ;

#endif /* ANSI_STD_C */

#ifndef SUNOS4
#ifdef __cplusplus
}
#endif
#endif


/* ------------------------------------------------------------- */
#endif  /* WLD_INT_H */
