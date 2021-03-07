; $Id: strrepl.pro,v 1.10 1999/01/22 20:12:17 mgs Stab $
;-------------------------------------------------------------
;+
; NAME:
;        STRREPL (function)
;
; PURPOSE:
;        replace one (or more) character(s) in a string
;
; CATEGORY:
;        string routines
;
; CALLING SEQUENCE:
;        Result = STRREPL(str, index, rchar)
;
; INPUTS:
;        STR -> the string to be changed
;
;        INDEX -> position of the character(s) to be replaced
;
;        RCHAR -> replacement character
;
; KEYWORD PARAMETERS:
;        none
;
; OUTPUTS:
;        another string of same length as input string
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;        Known shortcoming: if index is an array, it must contain all
;        valid elements (only the first entry is checked).
;
; EXAMPLE:
;        ; Convert one letter into upper case
;
;        abc = 'abcdefghijklmnopqrstuvwxyz'
;        print,strrepl(abc,strpos(abc,'m'),'M')
;
;        ; prints "abcdefghijklMnopqrstuvwxyz"
;
;
;        ; Use with strwhere function
;        a = 'abcabcabc'
;        print,strrepl(a,strwhere(a,'a'),'#')
;
;        ; prints  "#bc#bc#bc#bc#bc"
;
; MODIFICATION HISTORY:
;        mgs, 02 Jun 1998: VERSION 1.00
;
;-
; Copyright (C) 1998, Martin Schultz, Harvard University
; This software is provided as is without any warranty
; whatsoever. It may be freely used, copied or distributed
; for non-commercial purposes. This copyright notice must be
; kept with any copy of this software. If this software shall
; be used commercially or sold as part of a larger package,
; please contact the author to arrange payment.
; Bugs and comments should be directed to mgs@io.harvard.edu
; with subject "IDL routine strrepl"
;-------------------------------------------------------------


function strrepl,str,index,rchar
 
 
     if (n_elements(str) eq 0) then return,''
 
 
     ; convert strign and replace character to byte
     BStr = byte(str)
     BRC  = (byte(rchar))[0]
 
     ; make sure index is in range
     if (index[0] lt 0 OR index[0] ge n_elements(BStr)) then $
        return,Str
 
     ; replace indexed characters in string
     BStr[index] = BRC
 
 
     ; return result as string
     return,string(BStr)
 
end
 
