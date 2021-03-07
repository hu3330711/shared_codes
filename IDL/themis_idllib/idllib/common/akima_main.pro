pro akima_main,xvalue,data,coeff=coeff

datanum=n_elements(data)
m = dblarr(datanum+4)
t = dblarr(datanum+4)
coeff = dblarr(datanum,4)

loop=0
;;;定義域外の4点
DATAm1=0
DATAm2=0
DATAp1=0
DATAp2=0

f =fltarr(datanum+4)
;;==================================
;;定義域外4点の補間
calc_2nd_order_curve,-1,0,data[0],1,data[1],2,data[2],result=DATAm1
calc_2nd_order_curve,-2,0,data[0],1,data[1],2,data[2],result=DATAm2
calc_2nd_order_curve,datanum,datanum-3,data[datanum-3],datanum-2,data[datanum-2],datanum-1,data[datanum-1],result=DATAp1
calc_2nd_order_curve,datanum+1,datanum-3,data[datanum-3],datanum-2,data[datanum-2],datanum-1,data[datanum-1],result=DATAp2

for loop=0,datanum-1,1 do  f[loop+2]=data[loop]
f[0]=DATAm2;
f[1]=DATAm1;
f[datanum+2]=DATAp1;
f[datanum+3]=DATAp2;
;;==================================
;;微分係数m
for loop=0,datanum+3-1,1 do  m[loop]=f[loop+1]-f[loop]
;;==================================
;;微分係数t
for loop=2,datanum+3-2,1 do begin
  if(abs(m[loop+1]-m[loop])+abs(m[loop-1]-m[loop-2]) ge 1e-10) then begin
    t[loop]=(abs(m[loop+1]-m[loop])*m[loop-1]+abs(m[loop-1]-m[loop-2])*m[loop]) $
           /(abs(m[loop+1]-m[loop])          +abs(m[loop-1]-m[loop-2])         )
  endif else begin
    t[loop]=(m[loop-1]+m[loop])/2.
  endelse
endfor
;;==================================
;;補間曲線係数
for loop=0,datanum-1,1 do begin
  coeff[loop,0]=f[loop+2];
  coeff[loop,1]=t[loop+2];
  coeff[loop,2]=( 3*m[loop+2]-2*t[loop+2]-t[loop+2+1])/(2.-1.);
  coeff[loop,3]=(-2*m[loop+2]+  t[loop+2]+t[loop+2+1])/(2.-1.);
endfor

end

