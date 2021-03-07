;thm_median,'tha_sinfit_E-vxb-Ecor',300
pro thm_subtract_first,tplot_name_init

get_timespan,t

tplot_names,tplot_name_init,names=names
tplot_name_init=names

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
for loop=0,n_elements(tplot_name_init)-1,1 do begin
  print,loop,tplot_name_init(loop),n_elements(tplot_name_init)-1

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;median data exists -> use this ylimit
  get_data,tplot_name_init(loop)+'_md',data=data,index=index_m,lim=lim_md
  get_data,tplot_name_init(loop),data=data,dlim=dlim,index=index,lim=lim
  if(index eq 0) then return
  str_element,data,'y',float(data.y),/add_replace

  if not keyword_set(lim_md) then lim_md=lim

  dimension_input=n_elements(data.y(0,*))

  temp=min(abs(data.x-t[0]),index)

  for xx=0,dimension_input-1 do begin
    data.y [*,xx]=data.y [*,xx]-data.y[index,xx]
  endfor
  store_data,tplot_name_init(loop)+'_md',data=data,dlim=dlim,lim=lim_md
endfor

end