; $Id: mfindfile.pro,v 1.10 1999/01/22 20:12:17 mgs Stab $
;-------------------------------------------------------------
;+
; NAME:
;        MFINDFILE
;
; PURPOSE:
;        find all the files that match a given specification.
;        On our system, the IDL findfile function does not
;        work correctly!!
;
; CATEGORY:
;        System routines
;
; CALLING SEQUENCE:
;        listing = MFINDFILE(filemask)
;
; INPUTS:
;        FILEMASK -> a path and filename specification to look
;           for.
;
; KEYWORD PARAMETERS:
;        none
;
; OUTPUTS:
;        A string list containing all the files that match the 
;        specification.
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;        Spawns a unix ls -1 command !
;
; EXAMPLE:
;        list = mfindfile('~mgs/terra/chem1d/code/*.f')
;
;        ; returns all fortran files in Martin's chem1d directory.
;
; MODIFICATION HISTORY:
;        mgs, 14 Sep 1998: VERSION 1.00
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
; with subject "IDL routine mfindfile"
;-------------------------------------------------------------


function mfindfile,mask
 


    if (!version.os_family eq 'unix') then begin
    ; make my own findfile
 
       path = extract_path(mask,filename=fname)
       path = expand_path(path)
       newpath = path+fname
    
;   print,'fname:',fname,' path:',path,' newpath:',newpath
 
       command = 'ls -1'
       cstr = command+' '+newpath
       spawn,cstr,listing
 
       return,listing

    endif else begin      ; other OS - use IDL's original

       return,findfile(mask)

    endelse
 
end
 
