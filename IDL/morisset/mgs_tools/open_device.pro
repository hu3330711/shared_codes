; $Id: open_device.pro,v 1.10 1999/01/22 20:12:17 mgs Stab $
;-------------------------------------------------------------
;+
; NAME:
;        OPEN_DEVICE
;
; PURPOSE:
;        If hard copy is to be generated, OPEN_DEVICE opens the 
;        PostScript device.  Otherwise OPEN_DEVICE opens an Xwindow. 
;
; CATEGORY:
;        Input/Output
;
; CALLING SEQUENCE:
;        OPEN_DEVICE [,OLD_DEVICE] [,keywords]
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;        PS        (int)  -> will send PostScript file to printer
;
;        COLOR     (int)  -> will enable PostScript color mode
;
;        LANDSCAPE (int)  -> will enable PostScript landscape mode
;
;        PORTRAIT  (int)  -> will enable PostScript portrait mode
;
;        FILENAME  (str)  -> The name to be given the PostScript file.
;                  Default: idl.ps
;
;        WINPARAM  (int)  -> An integer vector with up to 5 elements:
;                  WINPARAM(0) = window number  (if negative, a window
;                       will be opened with the /FREE option.
;                  WINPARAM(1) = X dimension of window in pixels (width)
;                  WINPARAM(2) = Y dimension of window in pixels (height)
;                  WINPARAM(3) = X offset of window (XPOS)
;                  WINPARAM(4) = Y offset of window (YPOS)
;
;        TITLE -> window title
;         
;        _EXTRA -> additional keywords that are passed to the call to
;                  the DEVICE routine (e.g. /ENCAPSULATED)
;
; OUTPUTS:
;        OLD_DEVICE (str) -> stores the previous value of !D.NAME for
;            use in CLOSE_DEVICE. Note that CLOSE_DEVICE will automatically
;            set the default screen device if OLD_DEVICE is not provided,
;            hence it will only rarely be used.
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;        If PS=0 then  
;            Open Xwindow WINPARAM(0), which is WINPARAM(1) pixels wide
;            in the X-direction, and WINPARAM(2) pixels wide in the
;            Y-direction. 
;
;        If PS=1 then 
;           depending on /PORTRAIT or /LANDSCAPE and /COLOR
;           postscript is enabled in either portrait or landscape
;           mode as color or b/w plot
;
;        The key parameter which determines whether to open a postscript
;        file or a screen window is PS. Therefore, e.g. in a widget 
;        application, you can pass a standard set of parameters for both,
;        postscript and screen, to this routine, and determine the device
;        to be chosen by a button state or checkbox which is passed into PS.
;              
;
; EXAMPLE:
;        OPEN_DEVICE, WINPARAM=[0,800,800]  
;            opens a screen window of size 800x800 pixels at the default 
;            position
;
;        OPEN_DEVICE, OLD_DEVICE, /LANDSCAPE, FILENAME='myplot.ps'
;            opens a postscript file named myplot.ps in b/w and landscape
;            orientation
;
;        OPEN_DEVICE, OLDDEV, PS=PS, /PORTRAIT, /COLOR, WIN=2
;            depending on the value of PS either a color postscript file 
;            named idl.ps is opened or screen window number 2 in default
;            size.
;
;
; MODIFICATION HISTORY:
;        bmy  15 Aug 1997: VERSION 1.00
;        bmy, 19 Aug 1997: VERSION 1.01
;        mgs, 20 Aug 1997: VERSION 1.02
;        mgs, 09 Apr 1998: VERSION 1.10 
;            - added 2 more parameters for WINPARAM and TITLE keyword
;                                         
;
;-
; Copyright (C) 1997, 1998, Bob Yantosca and Martin Schultz, 
; Harvard University
; This software is provided as is without any warranty
; whatsoever. It may be freely used, copied or distributed
; for non-commercial purposes. This copyright notice must be
; kept with any copy of this software. If this software shall
; be used commercially or sold as part of a larger package,
; please contact the author to arrange payment.
; Bugs and comments should be directed to mgs@io.harvard.edu
; with subject "IDL routine open_device"
;-------------------------------------------------------------


pro open_device, OLD_DEVICE,                              $
                 PS=PS, COLOR=COLOR, FILENAME=FILENAME,   $  
                 LANDSCAPE=LANDSCAPE, PORTRAIT=PORTRAIT,  $ 
                 WINPARAM=WINPARAM, TITLE=TITLE, _EXTRA=E

      on_error, 2   ; return to caller
	
      OLD_DEVICE = !D.NAME    ; retrieve current device


      if (not keyword_set(FILENAME)) then FILENAME = 'idl.ps'
      if (not keyword_set(COLOR))    then COLOR    = 0
      if (not keyword_set(PORTRAIT)) then LANDSCAPE = 1 ; default

      if (keyword_set(PS)) then begin
          set_plot, 'PS'

          if (keyword_set(LANDSCAPE)) then begin       ;Landscape mode
              device, /landscape, color=COLOR, $
                  bits=8, filename=FILENAME, _EXTRA=e

          endif else begin    ; Portrait mode
              device, color=COLOR, bits=8, /portrait,   $
                  /inches, xoffset=0.25, yoffset=0.25,   $
                  xsize=8.0, ysize=10, filename=FILENAME, _EXTRA=e
          endelse


      endif else begin      ; no postscript desired
                            ; only action if winparam given
                            ; and device supports windows

          if ((!D.FLAGS AND 256) gt 0 AND $
              n_elements(WINPARAM) gt 0) then begin 
          ; WINPARAM must have 1, 3, or 5 elements, otherwise, default
          ; values for YSIZE and YOFFSET are used
              if (n_elements(winparam) gt 5) then winparam = winparam(0:4)

              if (n_elements(winparam) eq 4) then $
                  winparam = [ winparam, 500 ]

              if (n_elements(winparam) eq 2) then $
                  winparam = [ winparam, fix(winparam(1)/1.41) ]

              case n_elements(winparam) of
                5 :  window,winparam(0),FREE=(winparam(0) lt 0), $
                            xsize=winparam(1)>60, ysize=winparam(2)>10, $
                            xpos=winparam(3), ypos=winparam(4), $
                            TITLE=TITLE
                3 :  window,winparam(0),FREE=(winparam(0) lt 0), $
                            xsize=winparam(1)>60, ysize=winparam(2)>10, $
                            TITLE=TITLE
                1 :  window,winparam(0),FREE=(winparam(0) lt 0), $
                            TITLE=TITLE
                else : print,'*** OPEN_DEVICE: UNEXPECTED ERROR ! ***'
              endcase

          endif 

      endelse


return	
end

