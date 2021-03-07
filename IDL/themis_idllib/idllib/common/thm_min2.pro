;2 dimensional time series data.
;thm_median2,'tha_sinfit_E-vxb-Ecor',300,10
pro thm_min2,tplot_name_init,period,onemed=onemed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;median data exists -> use this ylimit
get_data,tplot_name_init,data=data,index=index,lim=lim
get_data,tplot_name_init+'_m',index=index_m,lim=lim_m
get_data,tplot_name_init+'_md',index=index_m,lim=lim_md
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;1 dimension data
if ndimen(data.y) eq 2 then begin
  print,'1dim data'
  thm_median,tplot_name_init,period
  return
endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;2 dimension data
if ndimen(data.y) gt 3 then begin
  print,'not applicable'
  return
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,tplot_name_init,data=data,dlim=dlim,index=index,lim=lim
if(index eq 0) then return

dimension1_input=n_elements(data.y(0,*,0))
dimension2_input=n_elements(data.y(0,0,*))

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
if(n_elements(data.x) ge 200) then begin
wdw=ceil(period/find_datarate(data.x(100:104)))
endif else if(n_elements(data.x) ge 5) then begin
wdw=ceil(period/find_datarate(data.x(2:4)))
endif else begin
wdw=ceil(period/find_datarate(data.x(0:1)))
endelse

if keyword_set(onemed) then wdw=n_elements(data.x)

print,'wdw=',wdw,'dimension=',dimension1_input,dimension2_input
if(wdw gt n_elements(data.x)) then return
for xx=0,dimension1_input-1 do begin
  for yy=0,dimension2_input-1 do begin
    if not keyword_set(onemed) then begin
      data_m [*,xx,yy]=min(data.y [*,xx,yy],wdw)
      data_d [*,xx,yy]=data.y[*,xx,yy]-data_m[*,xx,yy]
    endif else begin
      data_m [*,xx,yy]=min(data.y [*,xx,yy],dimension=1)
      data_d [*,xx,yy]=data.y[*,xx,yy]-data_m[*,xx,yy]
    endelse
  endfor
endfor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
data.y=data_m
store_data,tplot_name_init+'_m' ,data=data,dlim=dlim,lim=lim2
data.y=data_d
store_data,tplot_name_init+'_md',data=data,dlim=dlim,lim=lim3


end