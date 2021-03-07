pro search_midnight_mlon,time,mlon_midnight

get_timespan,t
t(0)=time_double(time)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MLT=0.
MLAT=65.
R=1.
MLTm12=(MLT-12.)*15*!pi/180.
SMX=R*cos(MLAT*!pi/180.)*cos(MLTm12)
SMY=R*cos(MLAT*!pi/180.)*sin(MLTm12)
SMZ=R*sin(MLAT*!pi/180.)
NOM_REV=1
SMGSM,SMX,SMY,SMZ,GSMX,GSMY,GSMZ,NOM_REV
NOM_REV=-1
GEOGSM,GEOX,GEOY,GEOZ,GSMX,GSMY,GSMZ,NOM_REV

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NOM_REV=1
GEOMAG,GEOX,GEOY,GEOZ,MAGX,MAGY,MAGZ,NOM_REV
NOM_REV=-1
sphcar,R     ,COLAT ,XLON  ,MAGX,MAGY,MAGZ,NOM_REV
MLAT=90.-COLAT/.01745329
MLON=XLON/.01745329

if(MLON lt 0)   then MLON=MLON+360
if(MLON ge 360) then MLON=MLON-360

mlon_midnight=MLON

end