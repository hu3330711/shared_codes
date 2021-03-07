pro mad_gpstec_keogram,regrid=regrid

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

glon_range=fltarr(5,2)
glon_name=strarr(5)
glon_name[0]='america'
glon_range[0,0]=190.
glon_range[0,1]=350.
glon_name[1]='alaska'
glon_range[1,0]=190.
glon_range[1,1]=220.
glon_name[2]='canada'
glon_range[2,0]=220.
glon_range[2,1]=310.
glon_name[3]='south_america'
glon_range[3,0]=280.
glon_range[3,1]=350.
glon_name[4]='risr'
glon_range[4,0]=250.
glon_range[4,1]=280.
;glon_name[4]='europe'
;glon_range[4,0]=-20.
;glon_range[4,1]=60.
;glon_name[5]='asia'
;glon_range[5,0]=90.
;glon_range[5,1]=180.

mlt_range=fltarr(2,2)
mlt_name=strarr(2)
mlt_name[0]='noon'
mlt_range[0,0]=9
mlt_range[0,1]=15
mlt_name[1]='night'
mlt_range[1,0]=21
mlt_range[1,1]=3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
for loop_glon=0L,n_elements(glon_range[*,0])-1 do begin
grid=data.glat[1]-data.glat[0]
keogram_max=fltarr(n_elements(data.x),180/grid+1)
keogram_med=fltarr(n_elements(data.x),180/grid+1)
mlat=fltarr(n_elements(keogram_max[0,*]))
glon_2d_m360=data.glon_2d-360.

mlon_m360=data.aacgmmlon
mlon_m360[where(data.aacgmmlon gt 180)]-=360.

if glon_range[loop_glon,0] ge 0 then temp=where(data.aacgmmlat ge 65.-grid/2. and data.aacgmmlat le 65.+grid/2. and data.glon_2d ge glon_range[loop_glon,0] and data.glon_2d le glon_range[loop_glon,1],count)
if glon_range[loop_glon,0] lt 0 then temp=where(data.aacgmmlat ge 65.-grid/2. and data.aacgmmlat le 65.+grid/2. and (glon_2d_m360 ge glon_range[loop_glon,0] or data.glon_2d le glon_range[loop_glon,1]),count)
mlon_range=[min(data.aacgmmlon[temp],/nan),max(data.aacgmmlon[temp],/nan)]
if mlon_range[0] le 10 and mlon_range[1] ge 350 then begin
  mlon_range=[min(mlon_m360[temp],/nan),max(mlon_m360[temp],/nan)]
  if mlon_range[0] lt 0 then mlon_range[0]+=360
endif

for loop=0L,n_elements(keogram_max[0,*])-1 do begin
  mlat[loop]=loop*grid-90.
;  if glon_range[loop_glon,0] ge 0 then temp=where(data.aacgmmlat ge mlat[loop]-grid/2. and data.aacgmmlat le mlat[loop]+grid/2. and data.glon_2d ge glon_range[loop_glon,0] and data.glon_2d le glon_range[loop_glon,1],count)
;  if glon_range[loop_glon,0] lt 0 then temp=where(data.aacgmmlat ge mlat[loop]-grid/2. and data.aacgmmlat le mlat[loop]+grid/2. and (glon_2d_m360 ge glon_range[loop_glon,0] or data.glon_2d le glon_range[loop_glon,1]),count)
  if mlon_range[0] le mlon_range[1] then temp=where(data.aacgmmlat ge mlat[loop]-grid/2. and data.aacgmmlat le mlat[loop]+grid/2. and data.aacgmmlon ge mlon_range[0] and data.aacgmmlon le mlon_range[1],count)
  if mlon_range[0] ge mlon_range[1] then temp=where(data.aacgmmlat ge mlat[loop]-grid/2. and data.aacgmmlat le mlat[loop]+grid/2. and (data.aacgmmlon ge mlon_range[0] or data.aacgmmlon le mlon_range[1]),count)
if loop mod 30 eq 0 then print,glon_name[loop_glon],mlat[loop],count
  if count ge 1 then begin
    for loop2=0L,n_elements(keogram_max[*,0])-1 do begin
      data_temp=reform(data.y[loop2,*,*])
      keogram_max[loop2,loop]=max(data_temp[temp],/nan)
      keogram_med[loop2,loop]=median(data_temp[temp])
    endfor
  endif
endfor
store_data,'gpstec_grid_keogram_max_'+glon_name[loop_glon],data={x:data.x,y:keogram_max,v:mlat},dlim={spec:1}
store_data,'gpstec_grid_keogram_med_'+glon_name[loop_glon],data={x:data.x,y:keogram_med,v:mlat},dlim={spec:1}
endfor

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
    if loop mod 30 eq 0 then print,mlt_name[loop_mlt],mlat[loop],count
    mlat[loop]=loop*grid-90.
    for loop2=0L,n_elements(keogram_max[*,0])-1 do begin
      dmlt=(data.x[loop2]-data.x[0])/3600
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
;tplot,['IMF','SYM-ASY','AU-AL','gpstec_grid_keogram_max_america','gpstec_grid_keogram_max_canada','gpstec_grid_keogram_max_risr','gpstec_grid_keogram_max_south_america','gpstec_grid_keogram_med_risr']
;makepng,'gpstec_grid_keogram_'+Syear+Smonth+Sday+Shour+Smin

end

