;thm_detrend4,'tha_sinfit_E-vxb-Ecor',300,10
pro thm_detrend4,tplot_name_init,period_long,period_short,nos=nos,nob=nob,nod=nod

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if period_long le period_short then begin
  print,'period input wrong'
  return
endif

tplot_names,tplot_name_init,names=names

for loop=0,n_elements(names)-1,1 do begin

  if n_elements(names) eq 1 then tplot_name=names
  if n_elements(names) ge 2 then tplot_name=names(loop)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  get_data,tplot_name,data=data,dlim=dlim,index=index,lim=lim
  index_s=0
  index_b=0
  index_d=0
  if not keyword_set(nos) then get_data,tplot_name+'_s',data=data_s,lim=lim_s,index=index_s
  if not keyword_set(nob) then get_data,tplot_name+'_b',data=data_b,lim=lim_b,index=index_b
  if not keyword_set(nod) then get_data,tplot_name+'_d',data=data_d,lim=lim_d,index=index_d
  if(index eq 0) then return
  str_element,data,'x',success=success
  if(success eq 0) then return
  if(n_elements(data.x) le 4) then return

  dimension_input=n_elements(data.y(0,*))

  data_s =data.y
  data_bd=data.y
  data_d =data.y
  data_b =data.y

  if(index_s eq 0) then lim_s=lim
  if(index_b eq 0) then lim_b=lim
  if(index_d eq 0) then lim_d=lim
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  if(n_elements(data.x) ge 100) then begin
  wdw=ceil(period_long/find_datarate(data.x(97:99)))
  endif else begin
  wdw=ceil(period_long/find_datarate(data.x(2:4)))
  endelse

  print,'wdw=',wdw
  if(wdw ge n_elements(data.x)) then return
  if(wdw le 1) then begin
    copy_data,tplot_name_init(loop),tplot_name_init(loop)+'_s'
    copy_data,tplot_name_init(loop),tplot_name_init(loop)+'_b'
    copy_data,tplot_name_init(loop),tplot_name_init(loop)+'_d'
    return
  endif
  for xx=0,dimension_input-1 do data_s [*,xx]=smooth(data.y [*,xx],wdw,/NaN)
  for xx=0,dimension_input-1 do data_bd[*,xx]=data.y (*,xx)-data_s(*,xx)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  if(n_elements(data.x) ge 100) then begin
  wdw=ceil(period_short/find_datarate(data.x(97:99)))
  endif else begin
  wdw=ceil(period_short/find_datarate(data.x(2:4)))
  endelse
  print,'wdw=',wdw
  for xx=0,dimension_input-1 do data_b [*,xx]=smooth(data_bd[*,xx],wdw,/NaN)
  for xx=0,dimension_input-1 do data_d [*,xx]=data_bd(*,xx)-data_b(*,xx)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  data.y=data_s
  if not keyword_set(nos) then store_data,tplot_name+'_s',data=data,dlim=dlim,lim=lim_s
  data.y=data_b
  if not keyword_set(nob) then store_data,tplot_name+'_b',data=data,dlim=dlim,lim=lim_b
  data.y=data_d
  if not keyword_set(nod) then store_data,tplot_name+'_d',data=data,dlim=dlim,lim=lim_d

  ;get_timespan,t
  ;get_ylimits,tplot_name_init,ylimits,t
  ;print,ylimits.yrange(0),ylimits.yrange(1)
  ;ylim,tplot_name_init+'_s',ylimits.yrange(0),ylimits.yrange(1)
  ;ylim,tplot_name_init+'_b',ylimits.yrange(0),ylimits.yrange(1)
  ;ylim,tplot_name_init+'_d',ylimits.yrange(0),ylimits.yrange(1)

endfor

end