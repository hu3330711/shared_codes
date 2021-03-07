pro akima_init,DATA,datanum,coeff
{
;;===================================================
;;“ñŽŸŒ³”z—ñ‚Ì“®“IŠm•Û
m = dblarr(datanum+4)
t = dblarr(datanum+4)
p = dblarr(datanum,4)

;;=================
akima_main(DATA,datanum,m,t,p);

;;=================
for loop1=0,datanum-1,1 do begin
  for loop2=0,3,1 do coeff[loop1][loop2]=p[loop1][loop2];
endfor

end