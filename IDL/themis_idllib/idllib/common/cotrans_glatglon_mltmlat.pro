pro cotrans_glatglon_mltmlat,tplot_var,no24boundary=no24boundary

if not keyword_set(tplot_var) then return

get_data,tplot_var,data=data

glat=data.glat
glon=data.glon
alt=data.alt
GEOx=(6372+alt)*cos(glat*!pi/180)*cos(glon*!pi/180)
GEOy=(6372+alt)*cos(glat*!pi/180)*sin(glon*!pi/180)
GEOz=(6372+alt)*sin(glat*!pi/180)
store_data,tplot_var+'_geo',data={x:data.x,y:[[GEOx],[GEOy],[GEOz]]}

cotrans,tplot_var+'_geo',tplot_var+'_gei',/GEO2GEI,/ignore_dlimits
cotrans,tplot_var+'_gei',tplot_var+'_gse',/GEI2GSE,/ignore_dlimits
cotrans,tplot_var+'_gse',tplot_var+'_gsm',/GSE2GSM,/ignore_dlimits
cotrans,tplot_var+'_gsm',tplot_var+'_sm' ,/GSM2SM,/ignore_dlimits

get_data,tplot_var+'_sm',data=SM
mlat2=atan(SM.y(*,2)/sqrt(SM.y(*,0)^2+SM.y(*,1)^2))*180/!pi
mlt2=atan(SM.y(*,1)/SM.y(*,0))*180/!pi/15+12
index=where(SM.y(*,0) lt 0,count)
if(count ge 1) then mlt2(index)=mlt2(index)+12
index=where(mlt2(*) ge 24,count)
if(count ge 1) then mlt2(index)=mlt2(index)-24

;crossing 0-24 boundary
if keyword_set(no24boundary) then begin
  if(max(mlt2,/nan) gt 23.5 and min(mlt2,/nan) lt 0.5) then mlt2(where(mlt2(*) lt 6))=mlt2(where(mlt2(*) lt 6))+24
endif
store_data,tplot_var+'_mltmlat',data={x:SM.x,mlat:mlat2,mlt:mlt2}


end