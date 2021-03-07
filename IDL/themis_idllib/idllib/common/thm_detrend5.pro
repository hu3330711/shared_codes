;2 dimensional data.
;thm_detrend5,'tha_sinfit_E-vxb-Ecor',300,10
pro thm_detrend5,tplot_name_init,period_long,period_short

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,tplot_name_init,data=data,dlim=dlim,index=index,lim=lim
get_data,tplot_name_init+'_s',data=data_s,lim=lim_s,index=index_s
get_data,tplot_name_init+'_b',data=data_b,lim=lim_b,index=index_b
get_data,tplot_name_init+'_d',data=data_d,lim=lim_d,index=index_d
if(index eq 0) then return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;1 dimension data
if ndimen(data.y) eq 2 then begin
  print,'thm_detrend4'
  thm_detrend4,tplot_name_init,period_long,period_short
  return
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;2 dimension data
if ndimen(data.y) gt 3 then begin
  print,'not applicable'
  return
endif

dimension1_input=n_elements(data.y(0,*,0))
dimension2_input=n_elements(data.y(0,0,*))

data_s =data.y
data_bd=data.y
data_d =data.y
data_b =data.y
data_s (*)=0
data_bd(*)=0
data_d (*)=0
data_b (*)=0

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
for xx=0,dimension1_input-1 do begin
  for yy=0,dimension2_input-1 do begin
    data_s [*,xx,yy]=smooth(data.y [*,xx,yy],wdw,/NaN)
  endfor
endfor

for xx=0,dimension1_input-1 do begin
  for yy=0,dimension2_input-1 do begin
    data_bd[*,xx,yy]=data.y [*,xx,yy]-data_s[*,xx,yy]
  endfor
endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if(n_elements(data.x) ge 100) then begin
wdw=ceil(period_short/find_datarate(data.x(97:99)))
endif else begin
wdw=ceil(period_short/find_datarate(data.x(2:4)))
endelse
print,'wdw=',wdw

for xx=0,dimension1_input-1 do begin
  for yy=0,dimension2_input-1 do begin
    data_b [*,xx,yy]=smooth(data_bd[*,xx,yy],wdw,/NaN)
  endfor
endfor

for xx=0,dimension1_input-1 do begin
  for yy=0,dimension2_input-1 do begin
    data_d [*,xx,yy]=data_bd[*,xx,yy]-data_b[*,xx,yy]
  endfor
endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
data.y=data_s
store_data,tplot_name_init+'_s',data=data,dlim=dlim,lim=lim_s
data.y=data_bd
store_data,tplot_name_init+'_bd',data=data,dlim=dlim,lim=lim_b
data.y=data_b
help,data,/st
store_data,tplot_name_init+'_b',data=data,dlim=dlim,lim=lim_b
data.y=data_d
store_data,tplot_name_init+'_d',data=data,dlim=dlim,lim=lim_d

;get_timespan,t
;get_ylimits,tplot_name_init,ylimits,t
;print,ylimits.yrange(0),ylimits.yrange(1)
;ylim,tplot_name_init+'_s',ylimits.yrange(0),ylimits.yrange(1)
;ylim,tplot_name_init+'_b',ylimits.yrange(0),ylimits.yrange(1)
;ylim,tplot_name_init+'_d',ylimits.yrange(0),ylimits.yrange(1)

end