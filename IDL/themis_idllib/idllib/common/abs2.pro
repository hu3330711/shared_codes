function abs2,a

c=0
if ndimen(a) eq 1 then begin
	return,sqrt(a(0)^2+a(1)^2+a(2)^2)
endif

if ndimen(a) eq 2 then begin
	c=fltarr(n_elements(a(*,0)))
	c(*)=sqrt(a(*,0)^2+a(*,1)^2+a(*,2)^2)
endif

return,c
end
