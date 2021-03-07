;+
;FUNCTION:   minmax_range,array 
;PURPOSE:  returns a two element array of min, max values
;INPUT:  array
;KEYWORDS:
;  MAX_VALUE:  ignore all numbers greater than this value
;  MIN_VALUE:  ignore all numbers less than this value
;  POSITIVE:   forces MINVALUE to 0
;
;CREATED BY:	Davin Larson
;LAST MODIFICATION:	@(#)minmax_range.pro	1.4 95/11/07
;-
;  MXSUBSCRIPT:  Named variable in which maximum subscript is returned NOT WORKING

function minmax_range,tdata,  $
   MAX_VALUE = max_value,  $ ; ignore all numbers >= max_value
   MIN_VALUE = min_value,  $ ; ignore all numbers <= min_value
   POSITIVE = positive, $   ;  forces min_value to 0
   MXSUBSCRIPT=subs

if keyword_set(positive) then min_value = 0

w = where(finite(tdata),count)
if count eq 0 then return,[0.,0.]
data = tdata(w)

if n_elements(max_value) then begin
   w = where( data lt max_value ,count)
   if count eq 0 then return,[0.,0.]
   data = data(w)
endif

if n_elements(min_value) then begin
   w = where( data gt min_value, count)
   if count eq 0 then return,[0.,0.]
   data = data(w)
endif

mx = max(data,MIN=mn)
return,[mn,mx]
end

