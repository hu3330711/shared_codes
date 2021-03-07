;thm_detrend4,'tha_sinfit_E-vxb-Ecor',300,10
pro thm_detrend_2,tplot_name_init,period_long,period_short

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,tplot_name_init,data=data,dlim=dlim,index=index,lim=lim
get_data,tplot_name_init+'_s',data=data_s,lim=lim_s,index=index_s
get_data,tplot_name_init+'_b',data=data_b,lim=lim_b,index=index_b
get_data,tplot_name_init+'_d',data=data_d,lim=lim_d,index=index_d
if(index eq 0) then return

dimension_input=n_elements(data.y(0,*))

str_element,data,'y',float(data.y),/add_replace
data_s =data.y
data_bd=data.y
data_d =data.y
data_b =data.y

if(index_s eq 0) then lim_s=lim
if(index_b eq 0) then lim_b=lim
if(index_d eq 0) then lim_d=lim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if(n_elements(data.x) ge 100) then begin
wdw=ceil(period_long/(data.x(99)-data.x(98)))
endif else begin
wdw=ceil(period_long/(data.x(4)-data.x(3)))
endelse

print,'wdw=',wdw
if(wdw gt n_elements(data.x)) then return
for xx=0,dimension_input-1 do data_s [*,xx]=smooth(data.y [*,xx],wdw,/NaN)
for xx=0,dimension_input-1 do data_bd[*,xx]=data.y (*,xx)-data_s(*,xx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if(n_elements(data.x) ge 100) then begin
wdw=ceil(period_short/(data.x(99)-data.x(98)))
endif else begin
wdw=ceil(period_short/(data.x(4)-data.x(3)))
endelse
print,'wdw=',wdw
for xx=0,dimension_input-1 do data_b [*,xx]=smooth(data_bd[*,xx],wdw,/NaN)
for xx=0,dimension_input-1 do data_d [*,xx]=data_bd(*,xx)-data_b(*,xx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
str_element,data,'y',data_s,/add_replace
store_data,tplot_name_init+'_s',data=data,dlim=dlim,lim=lim_s
str_element,data,'y',data_b,/add_replace
store_data,tplot_name_init+'_b',data=data,dlim=dlim,lim=lim_b
str_element,data,'y',data_d,/add_replace
store_data,tplot_name_init+'_d',data=data,dlim=dlim,lim=lim_d

;get_timespan,t
;get_ylimits,tplot_name_init,ylimits,t
;print,ylimits.yrange(0),ylimits.yrange(1)
;ylim,tplot_name_init+'_s',ylimits.yrange(0),ylimits.yrange(1)
;ylim,tplot_name_init+'_b',ylimits.yrange(0),ylimits.yrange(1)
;ylim,tplot_name_init+'_d',ylimits.yrange(0),ylimits.yrange(1)

end