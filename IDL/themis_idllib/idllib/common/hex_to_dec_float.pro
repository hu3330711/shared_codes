function hex_to_dec_float,bytearray

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if n_elements(bytearray) eq 4 then begin
  ;get bit
  bit=intarr(8,8)
  array=bytearray
  for jj=0,3 do begin
    bit(jj,0)= array(jj)/128
    bit(jj,1)=(array(jj)-128*bit(jj,0))/64
    bit(jj,2)=(array(jj)-128*bit(jj,0)-64*bit(jj,1))/32
    bit(jj,3)=(array(jj)-128*bit(jj,0)-64*bit(jj,1)-32*bit(jj,2))/16
    bit(jj,4)=(array(jj)-128*bit(jj,0)-64*bit(jj,1)-32*bit(jj,2)-16*bit(jj,3))/8
    bit(jj,5)=(array(jj)-128*bit(jj,0)-64*bit(jj,1)-32*bit(jj,2)-16*bit(jj,3)-8*bit(jj,4))/4
    bit(jj,6)=(array(jj)-128*bit(jj,0)-64*bit(jj,1)-32*bit(jj,2)-16*bit(jj,3)-8*bit(jj,4)-4*bit(jj,5))/2
    bit(jj,7)=(array(jj)-128*bit(jj,0)-64*bit(jj,1)-32*bit(jj,2)-16*bit(jj,3)-8*bit(jj,4)-4*bit(jj,5)-2*bit(jj,6))
  endfor

  ;plusminus, index
  pm=bit(0,0)
  ind=bit(0,1)*128+bit(0,2)*64+bit(0,3)*32+bit(0,4)*16+bit(0,5)*8+bit(0,6)*4+bit(0,7)*2+bit(1,0)-127

  ;value of float
  floatparam=1.0*2.0^(0+ind)
  for jj=1,7 do floatparam=floatparam+float(bit(1,jj))*2.0^(-(jj-0)+ind)
  for jj=0,7 do floatparam=floatparam+float(bit(2,jj))*2.0^(-(jj+8)+ind)
  for jj=0,7 do floatparam=floatparam+float(bit(3,jj))*2.0^(-(jj+16)+ind)

  if(pm eq 1) then floatparam=-floatparam
  return,floatparam
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if n_elements(bytearray) eq 8 then begin
  ;get bit
  bit=intarr(8,8)
  array=bytearray
  for jj=0,7 do begin
    bit(jj,0)= array(jj)/128
    bit(jj,1)=(array(jj)-128*bit(jj,0))/64
    bit(jj,2)=(array(jj)-128*bit(jj,0)-64*bit(jj,1))/32
    bit(jj,3)=(array(jj)-128*bit(jj,0)-64*bit(jj,1)-32*bit(jj,2))/16
    bit(jj,4)=(array(jj)-128*bit(jj,0)-64*bit(jj,1)-32*bit(jj,2)-16*bit(jj,3))/8
    bit(jj,5)=(array(jj)-128*bit(jj,0)-64*bit(jj,1)-32*bit(jj,2)-16*bit(jj,3)-8*bit(jj,4))/4
    bit(jj,6)=(array(jj)-128*bit(jj,0)-64*bit(jj,1)-32*bit(jj,2)-16*bit(jj,3)-8*bit(jj,4)-4*bit(jj,5))/2
    bit(jj,7)=(array(jj)-128*bit(jj,0)-64*bit(jj,1)-32*bit(jj,2)-16*bit(jj,3)-8*bit(jj,4)-4*bit(jj,5)-2*bit(jj,6))
  endfor

  ;plusminus, index
  pm=bit(0,0)
  ind=bit(0,1)*1024+bit(0,2)*512+bit(0,3)*256+bit(0,4)*128+bit(0,5)*64+bit(0,6)*32+bit(0,7)*16
    $+bit(1,0)*8+bit(1,1)*4+bit(1,2)*2+bit(1,3)
    $-1023

  ;value of float
  floatparam=1.0*2.0^(0+ind)
  for jj=4,7 do floatparam=floatparam+float(bit(1,jj))*2.0^(-(jj-0)+ind)
  for kk=2,7 do begin
    for jj=0,7 do floatparam=floatparam+float(bit(kk,jj))*2.0^(-(jj+8*(kk-1))+ind)
  endfor

  if(pm eq 1) then floatparam=-floatparam
  return,floatparam
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if n_elements(bytearray) eq 10 then begin
  ;get bit
  bit=intarr(10,8)
  array=bytearray
  for jj=0,9 do begin
    bit(jj,0)= array(jj)/128
    bit(jj,1)=(array(jj)-128*bit(jj,0))/64
    bit(jj,2)=(array(jj)-128*bit(jj,0)-64*bit(jj,1))/32
    bit(jj,3)=(array(jj)-128*bit(jj,0)-64*bit(jj,1)-32*bit(jj,2))/16
    bit(jj,4)=(array(jj)-128*bit(jj,0)-64*bit(jj,1)-32*bit(jj,2)-16*bit(jj,3))/8
    bit(jj,5)=(array(jj)-128*bit(jj,0)-64*bit(jj,1)-32*bit(jj,2)-16*bit(jj,3)-8*bit(jj,4))/4
    bit(jj,6)=(array(jj)-128*bit(jj,0)-64*bit(jj,1)-32*bit(jj,2)-16*bit(jj,3)-8*bit(jj,4)-4*bit(jj,5))/2
    bit(jj,7)=(array(jj)-128*bit(jj,0)-64*bit(jj,1)-32*bit(jj,2)-16*bit(jj,3)-8*bit(jj,4)-4*bit(jj,5)-2*bit(jj,6))
  endfor
  ;plusminus, index
  pm=bit(0,0)
  ind=bit(0,1)*16384+bit(0,2)*8192+bit(0,3)*4096+bit(0,4)*2048+bit(0,5)*1024+bit(0,6)*512+bit(0,7)*256
    $+bit(1,0)*128+bit(1,1)*64+bit(1,2)*32+bit(1,3)*16+bit(1,4)*8+bit(1,5)*4+bit(1,6)*2+bit(1,7)
    $-16383

  ;value of float
  floatparam=1.0*2.0^(0+ind)
stop
  for kk=2,9 do begin
    for jj=0,7 do floatparam=floatparam+float(bit(kk,jj))*2.0^(-(jj+8*(kk-1))+ind)
  endfor

  if(pm eq 1) then floatparam=-floatparam
  return,floatparam
endif

return,float('NaN')
end