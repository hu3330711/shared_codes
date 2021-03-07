;take median in time-value domain
;thm_median2,'test',3
pro thm_median_timevalue,tplot_name_init,points

tplot_names,tplot_name_init,names=names

for loop=0,n_elements(names)-1 do begin
  tplot_name_init=names[loop]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;median data exists -> use this ylimit
get_data,tplot_name_init,data=data,index=index,dlim=dlim,lim=lim
get_data,tplot_name_init+'_m',index=index_m,lim=lim_m
get_data,tplot_name_init+'_md',index=index_m,lim=lim_md

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if ndimen(data.y) gt 3 then begin
  print,'not applicable'
  return
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,tplot_name_init,data=data,dlim=dlim,index=index,lim=lim
if(index eq 0) then return

data_m=median(data.y,points,/even)
data_d=data.y-data_m

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
data.y=data_m
store_data,tplot_name_init+'_m' ,data=data,dlim=dlim,lim=lim
data.y=data_d
store_data,tplot_name_init+'_md',data=data,dlim=dlim,lim=lim

endfor


end
