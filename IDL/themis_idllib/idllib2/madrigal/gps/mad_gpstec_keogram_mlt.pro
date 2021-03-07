pro mad_gpstec_keogram_mlt,regrid=regrid

get_timespan,t
Syear=''
Smonth=''
Sday=''
Shour=''
Smin=''
Ssec=''
cdum=''
print,time_string(t[0])
reads,time_string(t[0]),Syear,cdum,Smonth,cdum,Sday,cdum,Shour,cdum,Smin,cdum,Ssec,format='(a4,a1,a2,a1,a2,a1,a2,a1,a2,a1,a2)'
Syear2=strmid(Syear,2,2)
Smin2=strmid(Smin,0,1)+'*'

if not keyword_set(regrid) then get_data,'gpstec_grid',data=data,index=index
if     keyword_set(regrid) then get_data,'gpstec_regrid',data=data,index=index
if index eq 0 then begin
  print,'need gpstec_grid or gpstec_regrid'
  return
endif

mlt_range=fltarr(4,2)
mlt_name=strarr(4)
mlt_name[0]='noon'
mlt_range[0,0]=8
mlt_range[0,1]=16
mlt_name[1]='night'
mlt_range[1,0]=20
mlt_range[1,1]=4
mlt_name[2]='dawn'
mlt_range[2,0]=3
mlt_range[2,1]=9
mlt_name[3]='dusk'
mlt_range[3,0]=15
mlt_range[3,1]=21

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
for loop_mlt=0L,n_elements(mlt_range[*,0])-1 do begin
  test=data.aacgmmlat[0,0:88]
  test2=data.aacgmmlat[0,1:89]
  grid=median(test2-test)
  keogram_max=fltarr(n_elements(data.x),180/grid+1)
  keogram_med=fltarr(n_elements(data.x),180/grid+1)
  mlat=fltarr(n_elements(keogram_max[0,*]))
  mlt_2d_m24=data.aacgmmlt-24.

  mlt_m24=data.aacgmmlt
  mlt_m24[where(data.aacgmmlt gt 12)]-=24.

  for loop=0L,n_elements(keogram_max[0,*])-1 do begin
    mlat[loop]=loop*grid-90.
    if loop mod 30 eq 0 then print,mlt_name[loop_mlt],mlat[loop]
    for loop2=0L,n_elements(keogram_max[*,0])-1 do begin
      dmlt=(data.x[loop2]-(data.x[0]-(data.x[0] mod 86400)))/3600
      data_aacgmmlt=data.aacgmmlt+dmlt
      temp=where(data_aacgmmlt ge 24,count)
      if count ge 1 then data_aacgmmlt[temp]-=24
      if mlt_range[loop_mlt,0] le mlt_range[loop_mlt,1] then temp=where(data.aacgmmlat ge mlat[loop]-grid/2. and data.aacgmmlat le mlat[loop]+grid/2. and data_aacgmmlt ge mlt_range[loop_mlt,0] and data_aacgmmlt le mlt_range[loop_mlt,1],count)
      if mlt_range[loop_mlt,0] ge mlt_range[loop_mlt,1] then temp=where(data.aacgmmlat ge mlat[loop]-grid/2. and data.aacgmmlat le mlat[loop]+grid/2. and (data_aacgmmlt ge mlt_range[loop_mlt,0] or data_aacgmmlt le mlt_range[loop_mlt,1]),count)
      if count ge 1 then begin
        data_temp=reform(data.y[loop2,*,*])
        keogram_max[loop2,loop]=max(data_temp[temp],/nan)
        keogram_med[loop2,loop]=median(data_temp[temp])
      endif
    endfor
  endfor
  store_data,'gpstec_grid_keogram_max_'+mlt_name[loop_mlt],data={x:data.x,y:keogram_max,v:mlat},dlim={spec:1}
  store_data,'gpstec_grid_keogram_med_'+mlt_name[loop_mlt],data={x:data.x,y:keogram_med,v:mlat},dlim={spec:1}
endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;omni_load,/noplot
ylim,'gpstec_grid_keogram*',60,90
zlim,'gpstec_grid_keogram*',2,20
;tplot,['IMF','SYM-ASY','AU-AL','gpstec_grid_keogram_max_noon','gpstec_grid_keogram_max_night']
;makepng,'gpstec_grid_keogram_'+Syear+Smonth+Sday+Shour+Smin

end

