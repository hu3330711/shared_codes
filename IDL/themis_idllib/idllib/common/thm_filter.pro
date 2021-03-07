pro thm_filter,tplot_name_init,period_long,period_short

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
  get_data,tplot_name+'_s',data=data_s,lim=lim_s,index=index_s
  get_data,tplot_name+'_b',data=data_b,lim=lim_b,index=index_b
  get_data,tplot_name+'_d',data=data_d,lim=lim_d,index=index_d
  if(index eq 0) then return
  str_element,data,'x',success=success
  if(success eq 0) then return
  if(n_elements(data.x) le 4) then return

  dimension_input=n_elements(data.y(0,*))

  ;remove nan
  for comp=0,dimension_input-1 do begin
    ;where_temp=where(strtrim(string(data.y(*,comp)),1) eq 'NaN',count)
    ;if count ge 1 then begin
    ;  for loop=0,count-1 do begin
    ;    for loop_before=where_temp(loop)-1,0 do begin
    ;      if(strtrim(string(data.y(loop_before,comp)),1) ne 'NaN') then break
    ;    endfor
    ;    if loop_before le -1 then loop_before=0
    ;    for loop_after=where_temp(loop)+1,n_elements(data.x)-1 do begin
    ;      if(strtrim(string(data.y(loop_after,comp)),1) ne 'NaN') then break
    ;    endfor
    ;    if loop_after ge n_elements(data.x)-1 then loop_after=n_elements(data.x)-1
    ;    data.y(where_temp(loop),comp)=(data.y(loop_before,comp)+data.y(loop_after,comp))/2
    ;  endfor
    ;endif
    where_temp=where(strtrim(string(data.y(*,comp)),1) eq 'NaN',count)
    if count ge 1 then begin
      data.y(where_temp,comp)=median(data.y(*,comp))
    endif
  endfor


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
  if(wdw gt n_elements(data.x)) then return
  ;for xx=0,dimension_input-1 do data_s [*,xx]=filter(data.y [*,xx],wdw,/LOWPASS)
  for xx=0,dimension_input-1 do data_s [*,xx]=filter_fft(data.y [*,xx],wdw,/time,/lowpass)
  for xx=0,dimension_input-1 do data_bd[*,xx]=data.y (*,xx)-data_s(*,xx)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  if(n_elements(data.x) ge 100) then begin
  wdw=ceil(period_short/find_datarate(data.x(97:99)))
  endif else begin
  wdw=ceil(period_short/find_datarate(data.x(2:4)))
  endelse
  print,'wdw=',wdw
  ;for xx=0,dimension_input-1 do data_b [*,xx]=filter(data_bd[*,xx],wdw,/LOWPASS)
  for xx=0,dimension_input-1 do data_b [*,xx]=filter_fft(data_bd[*,xx],wdw,/time,/lowpass)
  for xx=0,dimension_input-1 do data_d [*,xx]=data_bd(*,xx)-data_b(*,xx)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  store_data,tplot_name+'_s',data={x:data.x,y:data_s},dlim=dlim,lim=lim_s
  store_data,tplot_name+'_b',data={x:data.x,y:data_b},dlim=dlim,lim=lim_b
  store_data,tplot_name+'_d',data={x:data.x,y:data_d},dlim=dlim,lim=lim_d

  ;get_timespan,t
  ;get_ylimits,tplot_name_init,ylimits,t
  ;print,ylimits.yrange(0),ylimits.yrange(1)
  ;ylim,tplot_name_init+'_s',ylimits.yrange(0),ylimits.yrange(1)
  ;ylim,tplot_name_init+'_b',ylimits.yrange(0),ylimits.yrange(1)
  ;ylim,tplot_name_init+'_d',ylimits.yrange(0),ylimits.yrange(1)

endfor

end
