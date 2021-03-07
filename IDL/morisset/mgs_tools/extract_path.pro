; $Id: extract_path.pro,v 1.10 1999/01/22 20:12:17 mgs Stab $
;-------------------------------------------------------------
;+
; NAME:
;        EXTRACT_PATH
;
; PURPOSE:
;        extract the file path from a full qualified filename
;
; CATEGORY:
;        file handling
;
; CALLING SEQUENCE:
;        path=EXTRACT_PATH(FULLANME [,keywords])
;
; INPUTS:
;        FULLNAME --> a fully qualified filename. If this input is
;           already a path it must end with the delimiter '/' (Unix)
;           or '\' (Windows).
;
; KEYWORD PARAMETERS:
;        FILENAME --> a named variable that returns the name of the
;           file. This can be used if both, the path and the name
;           of the file will be used. Otherwise it is recommended to
;           use EXTRACT_FILENAME instead.
;
; OUTPUTS:
;        A string containing the path to the file given.
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;        See also EXTRACT_FILENAME
;
; EXAMPLE:
;        print,extract_path('~mgs/IDL/tools/extract_path.pro')
;
;             will print  '~mgs/IDL/tools/'
;
;        print,extract_path('example.dat',filename=filename)
;
;             will print  '', and filename will contain 'example.dat'
;
;
; MODIFICATION HISTORY:
;        mgs, 18 Nov 1997: VERSION 1.00
;        mgs, 21 Jan 1999: - added extra check for use of '/' path specifiers
;              in Windows OS
;
;-
; Copyright (C) 1997, Martin Schultz, Harvard University
; This software is provided as is without any warranty
; whatsoever. It may be freely used, copied or distributed
; for non-commercial purposes. This copyright notice must be
; kept with any copy of this software. If this software shall
; be used commercially or sold as part of a larger package,
; please contact the author to arrange payment.
; An exception is the use o fthis routine within an IDL distribution.
; Bugs and comments should be directed to mgs@io.harvard.edu
; with subject "IDL routine extract_path"
;-------------------------------------------------------------


function extract_path,fullname,filename=filename

; determine path delimiter
    if (!version.os_family eq 'Windows') then sdel = '\' else sdel = '/'

    path = ''
    filename = ''

retry:
; look for last occurence of sdel and split string fullname
    p = rstrpos(fullname,sdel)

; extra Windows test: if p=-1 but fullname contains '/', retry
    if (p lt 0 AND strpos(fullname,'/') ge 0) then begin
       sdel = '/'
       goto,retry
    endif

    if (p ge 0) then begin
       path = strmid(fullname,0,p+1)
       filename = strmid(fullname,p+1,strlen(fullname)-1)
    endif else $
       filename = fullname


return,path

end

