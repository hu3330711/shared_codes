;thm_median,'tha_sinfit_E-vxb-Ecor',300
pro thm_median,tplot_name_init,period,onemed=onemed,ulimit=ulimit,llimit=llimit

tplot_names,tplot_name_init,names=names
tplot_name_init=names

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
for loop=0,n_elements(tplot_name_init)-1,1 do begin
  print,loop,tplot_name_init(loop),n_elements(tplot_name_init)-1

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;median data exists -> use this ylimit
  get_data,tplot_name_init(loop)+'_m',data=data,index=index_m,lim=lim_m
  get_data,tplot_name_init(loop)+'_md',data=data,index=index_m,lim=lim_md
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  get_data,tplot_name_init(loop),data=data,dlim=dlim,index=index,lim=lim
  if(index eq 0) then return
  str_element,data,'y',float(data.y),/add_replace

  dimension_input=n_elements(data.y(0,*))

  data_m =data.y
  data_d =data.y

  if(index_m ne 0) then begin
  lim2=lim_m
  lim3=lim_md
  endif else begin
  lim2=lim
  lim3=lim
  endelse

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  if keyword_set(ulimit) then begin
    index=where(data.y ge ulimit,count)
    if count ge 1 then begin
      data.y(index)='NaN'
    endif
  endif
  if keyword_set(llimit) then begin
    index=where(data.y le llimit,count)
    if count ge 1 then begin
      data.y(index)='NaN'
    endif
  endif
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  if(n_elements(data.x) ge 104) then begin
  wdw=ceil(period/find_datarate(data.x(100:104)))
  endif else begin
  wdw=ceil(period/find_datarate(data.x(2:4)))
  endelse

  if keyword_set(onemed) then wdw=n_elements(data.x)

  print,'x=',n_elements(data.x),'wdw=',wdw,'dimension=',dimension_input
  if(wdw gt n_elements(data.x)) then print,wdw,n_elements(data.x)
  if(wdw gt n_elements(data.x)) then return
  if(wdw le 1) then begin
    copy_data,tplot_name_init(loop),tplot_name_init(loop)+'_m'
    copy_data,tplot_name_init(loop),tplot_name_init(loop)+'_md'
    continue
  endif
  for xx=0,dimension_input-1 do begin
    if not keyword_set(onemed) then begin
      data_m [*,xx]=median(data.y [*,xx],wdw,/even)
      data_d [*,xx]=data.y[*,xx]-data_m[*,xx]
    endif else begin
        data_m [*,xx]=median(data.y [*,xx],dimension=1,/even)
        data_d [*,xx]=data.y[*,xx]-data_m[*,xx]
    endelse
  endfor


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  str_element,data,'y',data_m,/add_replace
  store_data,tplot_name_init(loop)+'_m' ,data=data,dlim=dlim,lim=lim2
  str_element,data,'y',data_d,/add_replace
  store_data,tplot_name_init(loop)+'_md',data=data,dlim=dlim,lim=lim3
endfor

end
