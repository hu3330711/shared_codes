pro convert_mag_geo_dipole,r,mlat,mlon,glat,glon

get_timespan,t
Syear=''
Smonth=''
Sday=''
Shour=''
Smin=''
Ssec=''
cdum=''
;print,time_string(t[0])
reads,time_string(t[0]),Syear,cdum,Smonth,cdum,Sday,cdum,Shour,cdum,Smin,cdum,Ssec,format='(a4,a1,a2,a1,a2,a1,a2,a1,a2,a1,a2)'
Sdate1=Syear+'-'+Smonth+'-'+Sday

reads,Syear,year,format='(i)'
reads,Smonth,month,format='(i)'
reads,Sday,day,format='(i)'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GEOPACK INIT
;yymmdd_doy_conv,yymmdd,Sdoy
Sdoy=get_doy(day,month,year)
  IYEAR=Syear
  IDAY=Sdoy
  IHOUR=Shour
  IMIN=Smin
  ISEC=Ssec

  ;geopack_recalc,IYEAR,IDAY,IHOUR,IMIN,ISEC
  recalc,IYEAR,IDAY,IHOUR,IMIN,ISEC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  COLAT=(90.-MLAT)*.01745329
  XLON=MLON*.01745329
  NOM_REV=1
  if(n_elements(r) eq 1) then sphcar       ,R     ,COLAT ,XLON  ,MAGX,MAGY,MAGZ,NOM_REV
  if(n_elements(r) ge 2) then SPHCAR_vector,R     ,COLAT ,XLON  ,MAGX,MAGY,MAGZ,NOM_REV
  NOM_REV=-1
  GEOMAG,GEOX,GEOY,GEOZ,MAGX,MAGY,MAGZ,NOM_REV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  NOM_REV=-1
  if(n_elements(r) eq 1) then sphcar       ,R     ,COLAT ,XLON  ,GEOX,GEOY,GEOZ,NOM_REV
  if(n_elements(r) ge 2) then SPHCAR_vector,R     ,COLAT ,XLON  ,GEOX,GEOY,GEOZ,NOM_REV
  GLAT=90.-COLAT/.01745329
  GLON=XLON/.01745329

if(n_elements(r) eq 1) then begin
  if(GLON lt 0)   then GLON=GLON+360
  if(GLON ge 360) then GLON=GLON-360
endif else begin
  temp=where(GLON lt 0,count)
  if(count ge 1) then GLON(temp)=GLON(temp)+360
  temp=where(GLON ge 360,count)
  if(count ge 1) then GLON(temp)=GLON(temp)-360
endelse


end

