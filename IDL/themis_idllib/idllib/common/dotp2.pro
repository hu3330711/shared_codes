;+
;FUNCTION:	dotp2(a,b)
;INPUT:	
;	a,b:	real(n,3)	vector arrays dimension (n,3) or (3)
;PURPOSE:
;	performs dot product on arrays
;CREATED BY:
;	J.McFadden	97-3-14
;Modifications
;	J.McFadden	05-2-7 changed first if to "if ndimen(a) eq 1 and ndimen(b) eq 1" 
;	Y.Nishimura	08-10-28 from crossp2
;-
function dotp2,a,b

c=0
if n_params() ne 2 then begin
	print,' Wrong format, Use: dotp2(a,b)'
	return,c
endif

if ndimen(a) eq 1 and ndimen(b) eq 1 then begin
	return,a(0)*b(0)+a(1)*b(1)+a(2)*b(2)
endif

if ndimen(a) eq 2 and ndimen(b) eq 2 then begin
	c=fltarr(n_elements(a(*,0)))
	c(*)=a(*,0)*b(*,0)+a(*,1)*b(*,1)+a(*,2)*b(*,2)
endif else if ndimen(a) eq 1 and ndimen(b) eq 2 then begin
	c=fltarr(n_elements(b(*,0)))
	c(*)=a(0)*b(*,0)+a(1)*b(*,1)+a(2)*b(*,2)
endif else if ndimen(a) eq 2 and ndimen(b) eq 1 then begin
	c=fltarr(n_elements(a(*,0)))
	c(*)=a(*,0)*b(0)+a(*,1)*b(1)+a(*,2)*b(2)
endif

return,c
end
