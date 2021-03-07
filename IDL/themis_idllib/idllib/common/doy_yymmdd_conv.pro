pro doy_yymmdd_conv,year,doy,month,day


Wdoy=0
yy=year

mm=1
while (mm le 12) do begin
  dd=31
  if(mm eq 2 or mm eq 4 or mm eq 6 or mm eq 9 or mm eq 11) then dd=dd-1
  if(mm eq 2) then dd=dd-1

  if(mm eq 2) then begin
    if(yy mod 4 eq 0) then begin
      if(yy/100 eq 0) then begin
        if(yy/400 ne 0) then dd=dd-1
      endif
    endif else begin
      dd=dd-1
    endelse
  endif

  if(doy le Wdoy+dd) then begin
    break
  endif else begin
    Wdoy=Wdoy+dd
  endelse

  mm=mm+1
endwhile

dd=doy-Wdoy
month=mm
day=dd



end