pro akima,x,xgrid,coeff,result=result

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if(n_elements(x) eq 1) then begin
temp=x
x=fltarr(1)
x(0)=temp
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
index=intarr(n_elements(x))
distance=fltarr(n_elements(x))
result=fltarr(n_elements(x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
for loopx=0,n_elements(x)-1,1 do begin
  for loop=0,n_elements(xgrid)-2,1 do begin
    if(xgrid(loop) le x(loopx) and x(loopx) lt xgrid(loop+1)) then begin
      index(loopx)=loop
      distance(loopx)=(x(loopx)-xgrid(loop))/(xgrid(loop+1)-xgrid(loop))
      break
    endif
  endfor

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  if(x(loopx) gt max(xgrid)) then begin
    print,'x is out of range'
    print,loopx,x(loopx),max(xgrid)
    return
  endif

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  result(loopx)=coeff[index(loopx),0] $
               +coeff[index(loopx),1]*(distance(loopx))^1 $
               +coeff[index(loopx),2]*(distance(loopx))^2 $
               +coeff[index(loopx),3]*(distance(loopx))^3
endfor

end
