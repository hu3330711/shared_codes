;+
;PROCEDURE: 	wi, wnum
;		       
;PURPOSE:	Switch or open windows.
; 
;INPUT:   
;	wnum - the window number.
;
;CREATED BY:	REE, 95-10-23
;FILE: wi.pro
;VERSION: 1.5
;LAST MODIFICATION: 97/01/02
;-

pro wi, wnum , limits=lim

if data_type(lim) eq 8 then begin
   str_element,lim,'window',value=wnum
   if n_elements(wnum) eq 0 then return
endif

i = 0
catch, no_window
IF no_window eq 0 then begin
    if n_elements(wnum) eq 0 then wshow,icon=0 else begin
       wset, wnum
       wshow,wnum, iconic=0
    endelse
endif else BEGIN
    if i then begin & print, "WI: Can't change window." & return & endif
    i = 1
    window, wnum
    wshow, wnum, iconic=0
ENDELSE

catch,/cancel
return

END

