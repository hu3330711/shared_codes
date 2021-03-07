; $Id: close_device.pro,v 1.10 1999/01/22 20:12:17 mgs Stab $
;-------------------------------------------------------------
;+
; NAME:
;        CLOSE_DEVICE
;
; PURPOSE:
;        CLOSE_DEVICE closes the PostScript device and spawns
;        a print job to the printer specified by the user or
;        it can be used to close a graphics window.
;
; CATEGORY:
;        Input/Output
;
; CALLING SEQUENCE:
;        CLOSE_DEVICE [,OLD_DEVICE] [,keywords]
;
; INPUTS:
;        OLD_DEVICE (str) -> Name of device that shall become the active
;             plotting device. If omitted, "X", "WIN" or "MAC" will be
;             set depending on !VERSION.OS_FAMILY. Hence, specification of
;             OLD_DEVICE is only rarely needed.
;             This parameter works together with the OLD_DEVICE parameter
;             of OPEN_DEVICE which returns the device name before the
;             postscript device (or a graphics device) is opened.
;             The OLD_DEVICE parameter can be misused to set the output 
;             device to anything! Therefore, it's probably safest to not 
;             use it and stick with the operating system dependent default.
;
; KEYWORD PARAMETERS:
;        LABEL -> a string that contains an annotation for a postscript
;             plot (usually a user name and/or filename). The current 
;             data and time will be appended if TIMESTAMP is set. 
;             NOTE: TIMESTAMP will produce an annotation even if LABEL
;             is not provided. The annotation is only added to postscript 
;             plots and only if the ps file is actually open.
;
;        /TIMESTAMP  -> add date and time to the LABEL on the plot.
;             If no LABEL is provided, the username and filename (value 
;             of FILENAME will be used or the filename of the current 
;             postscript file will be added). The timestamp is only added 
;             to postscript plots and only on the last page.
;
;        PRINTER    (str) -> name of the printer queue to send output to.
;             Default is 'none', i.e. the postscript file will only be 
;             closed and can then be manually printed e.g. using the Unix 
;             lpr command. Direct printing only works in Unix environments.
;
;        WINDOW -> window number to be closed (or -1 if current)
;
;        _EXTRA -> any additional keywords to CLOSE_DEVICE will be
;             passed on to STRDATE which is used to format the date
;             and time string. Possible values are: /SHORT, /SORTABLE,
;             /GERMAN.
;
;        LCOLOR -> the color value for the LABEL (default 1).
;
;        LPOSITION -> the position of the LABEL in normalized coordinates
;             (a two-element vector with default [0.98,0.007]).
;
;        LSIZE -> the charcter size of the LABEL (default 0.7).
;
;        LALIGN -> the alignment of the LABEL (default 1).
;
;        FILENAME   (str) -> name of the PostScript file.
;             This keyword is now obsolete since the name of the postscript 
;             file is determined automatically. It is kept as a surrogate 
;             for LABEL to ensure compatibility with older implementations. 
;
; OUTPUTS:
;        If postscript device is active, a *.ps file will be closed and 
;        optionally sent to the printer.
;
; SUBROUTINES:
;
; REQUIREMENTS:
;        Needs STRDATE for the TIMESTAMP option.
;
; NOTES: 
;        The WINDOW keyword is only evaluated if the current device supports 
;        windows [!D.FLAGS AND 256) GT 0]. If you only want to close a 
;        postscript file and don't fuss around with your screen windows
;        then simply don't set this keyword.
;
; EXAMPLE:
;        CLOSE_DEVICE
;            closes a postscript file (if opened) and resets the current
;            plotting device to 'X', 'WIN', or 'MAC' depending on the
;            OS_FAMILY.
;
;        CLOSE_DEVICE, PRINTER='hplj4', LABEL='mgs', /TIMESTAMP
;            If current device name is PS then the postscript file will
;            be closed, a user, date and time label will be added and 
;            the file will be spooled to the  printer queue 'hplj4'. 
;            NOTE: Printing of the file fails if you specified FILENAME as 
;            a relative path in the OPEN_DEVICE command and you changed your 
;            working directory while the device was opened.
;  
;        CLOSE_DEVICE, WIN=-1
;            If current device name is PS then the postscript file will
;            be closed. If the current device is a screen device (that 
;            supports windows), then the active window will be deleted.
; 
; MODIFICATION HISTORY:
;        bmy, 18 Aug 1997: VERSION 1.00
;        bmy, 19 Aug 1997: VERSION 1.01
;        mgs, 20 Aug 1997: VERSION 1.02
;        mgs, 08 Apr 1998: VERSION 2.00 
;           - automatic filename detection
;           - default device depending on OS_FAMILY
;        mgs, 21 May 1998: 
;           - added L.. keywords to control label and timestamp output
;        mgs, 18 Nov 1998:
;           - added output of username as default in timestamp
;
;-
; Copyright (C) 1997, 1998, Bob Yantosca and Martin Schultz, 
; Harvard University.
; This software is provided as is without any warranty
; whatsoever. It may be freely used, copied or distributed
; for non-commercial purposes. This copyright notice must be
; kept with any copy of this software. If this software shall
; be used commercially or sold as part of a larger package,
; please contact the author to arrange payment.
; Bugs and comments should be directed to mgs@io.harvard.edu
; with subject "IDL routine close_device"
;-------------------------------------------------------------


pro close_device, OLD_DEVICE, PRINTER=PRINTER,  $
                  LABEL=LABEL, TIMESTAMP=TIMESTAMP, LCOLOR=LCOLOR, $
                  LPOSITION=LPOSITION,LSIZE=LSIZE, $
                  LALIGN=LALIGN, $
                  _EXTRA=e, WINDOW=WINDOW,  $
                  FILENAME=FILENAME

      on_error, 2


      ; determine default device
      case strupcase(!VERSION.OS_FAMILY) of
         'UNIX'    : defdev = 'X'
         'WINDOWS' : defdev = 'WIN'
         'MACOS'   : defdev = 'MAC'
         else      : begin
                     print,'*** CLOSE_DEVICE: unknown operating system ! ***'
                     defdev = 'NULL'
                     end
      endcase

      ; set default value of OLD_DEVICE
      if (n_params() le 0) then OLD_DEVICE = defdev

      ; set default for printer queue
      if (n_elements(PRINTER) eq 0) then PRINTER  = '' 

      ; set default position, charsize and color for label
      if (n_elements(LPOSITION) ne 2) then LPOSITION = [0.98,0.007]
      if (n_elements(LSIZE) eq 0) then LSIZE = 0.7
      if (n_elements(LCOLOR) eq 0) then LCOLOR = 1
      if (n_elements(LALIGN) eq 0) then LALIGN = 1

      ; ============================================================ 
      if (!d.name eq 'PS') then begin   ; if postscript device active

          ; determine if ps file was opened
          if (!D.UNIT gt 0) then begin
              ; extract current filename
              r = fstat(!D.UNIT)
              CFILENAME = r.name

              ; add label and timestamp if desired
              addlabel = 0   ; default: no label
              if (n_elements(LABEL) eq 0) then begin
                  ; get user name
                  user_name = getenv('USER')
                  if (user_name ne '') then user_name = user_name+' '

                  ; for compatibility set FILENAME as default label
                  if (n_elements(FILENAME) gt 0) then $
                       LABEL = user_name + FILENAME $
                  else $    ; use actual filename as label
                       LABEL = user_name + CFILENAME
              endif else $
                  addlabel = 1   ; add label in any case
              
              if(keyword_set(TIMESTAMP)) then begin
                  LABEL = LABEL + ', ' + strdate(_EXTRA=e)
                  addlabel = 1   ; add label
              endif

              if (addlabel) then $
                 xyouts,lposition(0),lposition(1),LABEL,color=lcolor,   $
                      align=lalign,/norm,charsize=lsize

              ; close postscript file
	        device, /close    

              ; spawn postscript file to printer
              if (PRINTER ne '') then begin 
  	            TRIM_PRINTER = strtrim(PRINTER,2)
                  print, 'Sending output to printer ' + TRIM_PRINTER
                  spawn, 'lpr -P ' + TRIM_PRINTER + ' ' + CFILENAME
              endif
          endif else $   ; ps file was not open
              device,/close   ; only close it


      ; ============================================================ 
      endif else begin     ; no postscript device active

          ; check if device supports windows and if a window 
          ; shall be closed
          if(n_elements(window) gt 0) then begin
              if(window lt 0) then window = !D.WINDOW 
              if( (!D.FLAGS AND 256) GT 0 AND window ge 0) then $
                  wdelete,window
          endif

      endelse
      ; ============================================================ 


      ; make OLD_DEVICE (usually default screen) active
      set_plot, OLD_DEVICE

return
end


