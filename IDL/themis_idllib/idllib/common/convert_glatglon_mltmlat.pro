
pro convert_glatglon_mltmlat

get_data,'glatglon',data=data
time=data.x
glat=data.glat
glon=data.glon
GEO=fltarr(n_elements(time),3)
GEO(*,0)=(6372.+110)*cos(glat*!pi/180)*cos(glon*!pi/180)
GEO(*,1)=(6372.+110)*cos(glat*!pi/180)*sin(glon*!pi/180)
GEO(*,2)=(6372.+110)*sin(glat*!pi/180)
store_data,'geo',data={x:time,y:GEO}
cotrans,'geo','gei',/GEO2GEI,/IGNORE_DLIMITS
cotrans,'gei','gse',/GEI2GSE,/IGNORE_DLIMITS
cotrans,'gse','gsm',/GSE2GSM,/IGNORE_DLIMITS
cotrans,'gsm','sm' ,/GSM2SM,/IGNORE_DLIMITS
get_data,'sm',data=SM
mlat2=atan(SM.y(*,2)/sqrt(SM.y(*,0)^2+SM.y(*,1)^2))*180/!pi
mlt2=atan(SM.y(*,1)/SM.y(*,0))*180/!pi/15+12
if(n_elements(where(SM.y(*,0) lt 0)) ge 2) then mlt2(where(SM.y(*,0) lt 0))=mlt2(where(SM.y(*,0) lt 0))+12
mlt2=mlt2 mod 24

store_data,'mltmlat',data={x:data.x,mlat:mlat2,mlt:mlt2}

end