
function hex_to_dec_int,bytearray,sign=sign

intparam=long64(0)
;for loop=0,n_elements(bytearray)-1,1 do print,intparam,bytearray(loop),loop,n_elements(bytearray)-1-loop,bytearray(loop)*long64(256)^(n_elements(bytearray)-1-loop)
for loop=0,n_elements(bytearray)-1,1 do intparam+=bytearray(loop)*long64(256)^(n_elements(bytearray)-1-loop)

;complement number if the initial bit is 1
if(keyword_set(sign) and bytearray(0) ge long64(2)^7) then return,intparam-long64(2)^(8*4)
return,intparam


end
