pro doy_yymmdd_conv2,year,doy,month,day

if(n_elements(doy) eq 1) then begin
  temp=doy
  doy=intarr(1)
  doy(0)=temp
endif

if(n_elements(year) eq 1 and n_elements(doy) ge 2) then begin
  temp=year
  year=intarr(n_elements(doy))
  year(*)=temp
endif

month=intarr(n_elements(doy))
day=intarr(n_elements(doy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
for loop=0,n_elements(doy)-1,1 do begin

  Wdoy=0
  yy=year(loop)

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

    if(doy(loop) le Wdoy+dd) then begin
      break
    endif else begin
      Wdoy=Wdoy+dd
    endelse

    mm=mm+1
  endwhile

  dd=doy(loop)-Wdoy
  month(loop)=mm
  day(loop)=dd

endfor

end