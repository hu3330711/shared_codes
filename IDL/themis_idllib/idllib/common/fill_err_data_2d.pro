;fill nan using surrounding data in 2 dimensional
;nan_fill_2d,'tha_sinfit_E-vxb-Ecor'
pro fill_err_data_2d,tplot_name_init,nan=nan,err=err,width=width

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;median data exists -> use this ylimit
get_data,tplot_name_init,data=data,index=index,lim=lim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;1 dimension data
if ndimen(data.y) ne 2 then begin
  print,'not 2dim data'
  return
endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,tplot_name_init,data=data,dlim=dlim,index=index,lim=lim
if(index eq 0) then return

if not keyword_set(width) then width=2
data_m=median(data.y,width)

if keyword_set(nan) then begin
where_temp=where(strtrim(string(data.y),1) ne 'NaN',count)
if count ge 1 then data_m(where_temp)=data.y(where_temp)
endif

if keyword_set(err) then begin
  where_temp=where(abs(data.y-err) ge 1e-10,count)
  if count ge 1 then data_m(where_temp)=data.y(where_temp)
endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
data.y=data_m
store_data,tplot_name_init+'_fill',data=data,dlim=dlim,lim=lim

end