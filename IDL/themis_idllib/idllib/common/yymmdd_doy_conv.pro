;yymmdd_doy_conv,yymmdd,doy
pro yymmdd_doy_conv,yymmdd,doy

yy=0
mm=0
dd=0

doy=0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;//yymmdd->yy,mm,dd
yy=yymmdd/10000;
mm=yymmdd/100-yy*100;
dd=yymmdd-yy*10000-mm*100;
print,yy,mm,dd,doy
if(dd eq 31 and (mm eq 2 or mm eq 4 or mm eq 6 or mm eq 9 or mm eq 11)) then begin
yy=0
mm=0
dd=0
endif
if (mm eq 2  and  dd eq 30) then begin
yy=0
mm=0
dd=0
endif
if(mm eq 2  and  dd eq 29) then begin
  if(yy mod 4 eq 0) then begin
    if(yy/100 eq 0) then begin
      if(yy/400 ne 0) then begin
        yy=0
        mm=0
        dd=0
      endif
    endif
  endif else begin
  yy=0
  mm=0
  dd=0
  endelse
endif
;print,yy,mm,dd,doy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
doy=31*(mm-1)
doy=doy+dd
;print,yy,mm,dd,doy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if(mm ge 2 +1) then doy=doy-1
if(mm ge 4 +1) then doy=doy-1
if(mm ge 6 +1) then doy=doy-1
if(mm ge 9 +1) then doy=doy-1
if(mm ge 11+1) then doy=doy-1
;print,yy,mm,dd,doy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if(mm ge 2 +1) then doy=doy-1
;print,yy,mm,dd,doy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if(mm ge 2+1  and  yy mod 4 ne 0) then doy=doy-1
if(mm ge 2+1  and  yy mod 4 eq 0) then begin
  if(yy/100 eq 0) then begin
    if(yy/400 ne 0)  then doy=doy-1
  endif
endif
print,'yy,mm,dd,doy=',yy,mm,dd,doy

;return,doy
end