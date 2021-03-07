function  mean,var
;+
; ROUTINE:                mean
;
; AUTHOR:                 Terry Figel, ESRG, UCSB 2-21-93
;
; CALLING SEQUENCE:        mean,var
;
; INPUT:   
;              var         an array
;
; PURPOSE:                 return the mean of var
;-
retval=total(var)/n_elements(var)
return,retval
end

