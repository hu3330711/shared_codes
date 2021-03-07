pro convert_geo_mag_dipole,r,glat,glon,mlat,mlon

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

  geopack_recalc,IYEAR,IDAY,IHOUR,IMIN,ISEC
  recalc,IYEAR,IDAY,IHOUR,IMIN,ISEC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;GLAT,GLON -> GSM 
  COLAT=(90.-GLAT)*.01745329
  XLON=GLON*.01745329
  NOM_REV=1
  sphcar,R     ,COLAT ,XLON  ,GEOX,GEOY,GEOZ,NOM_REV
  GEOMAG,GEOX,GEOY,GEOZ,MAGX,MAGY,MAGZ,NOM_REV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  NOM_REV=-1
  if(n_elements(r) eq 1) then sphcar,R     ,COLAT ,XLON  ,MAGX,MAGY,MAGZ,NOM_REV
  if(n_elements(r) ge 2) then SPHCAR_vector,R     ,COLAT ,XLON  ,MAGX,MAGY,MAGZ,NOM_REV
  MLAT=90.-COLAT/.01745329
  MLON=XLON/.01745329

if(n_elements(r) eq 1) then begin
  if(MLON lt 0)   then MLON=MLON+360
  if(MLON ge 360) then MLON=MLON-360
endif else begin
  temp=where(MLON lt 0,count)
  if(count ge 1) then MLON(temp)=MLON(temp)+360
  temp=where(MLON ge 360,count)
  if(count ge 1) then MLON(temp)=MLON(temp)-360
endelse

end

