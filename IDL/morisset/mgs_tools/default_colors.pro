;-------------------------------------------------------------
;+
; NAME:
;        DEFAULT_COLORS (include file)
;
; PURPOSE:
;        set variables for default plotting colors 
;        as defined with myct.pro
;
; CATEGORY:
;        color table manipulation
;
; CALLING SEQUENCE:
;        @DEFAULT_COLORS
;
; INPUTS:
;        None
;
; KEYWORD PARAMETERS:
;        None
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        None
;
; REQUIREMENTS:
;        Assumes MYCT colortable has been loaded.
;
; NOTES:
;
; EXAMPLE:
;        @default_colors
;        plot, x, y, color=BLACK
;
;            ; Uses BLACK to refer to the MYCT index for BLACK.
;
; MODIFICATION HISTORY:
;        mgs, 12 Nov 1998: VERSION 1.00
;        bmy, 08 Dec 1998: - added DEFAULT_TABLE 
;        bmy, 14 Jan 1999: - added DEFAULT_BOTTOM, DEFAULT_NCOLORS
;                            DEFAULT_RANGE, DEFAULT_VAL, DEFAULT_SAT
;                          - added standard header
;        bmy, 19 Jan 1999: - renamed DEFAULT_* to DEFAULT_MYCT_* to
;                            avoid conflicts with other function names
;   
;-
; Copyright (C) 1997, 1998, Martin Schultz and Bob Yantosca,
; Harvard University
; This software is provided as is without any warranty
; whatsoever. It may be freely used, copied or distributed
; for non-commercial purposes. This copyright notice must be
; kept with any copy of this software. If this software shall
; be used commercially or sold as part of a larger package,
; please contact the author to arrange payment.
; Bugs and comments should be directed to mgs@io.harvard.edu
; or bmy@io.harvard.edu with subject "IDL routine default_colors"
;-------------------------------------------------------------

WHITE      = 0
BLACK      = 1
RED        = 2
GREEN      = 3
BLUE       = 4
YELLOW     = 5
MAGENTA    = 6
CYAN       = 7
LIGHTRED   = 8
LIGHTGREEN = 9
LIGHTBLUE  = 10
DARKGREY   = 13
MEDIUMGREY = 14
LIGHTGREY  = 15
GREY       = 15     ; as default


; bmy added default colortable setting 
; Color Table #27 is EOS-B (12/8/98, 1/19/99)
DEFAULT_MYCT_TABLE   = 27

; bmy added other defaults for MYCT (1/14/99, 1/19/99)
DEFAULT_MYCT_BOTTOM  = 20
DEFAULT_MYCT_NCOLORS = 120
DEFAULT_MYCT_RANGE   = [ 0.1, 0.7 ]
DEFAULT_MYCT_SAT     = 0.97
DEFAULT_MYCT_VAL     = 1.3
