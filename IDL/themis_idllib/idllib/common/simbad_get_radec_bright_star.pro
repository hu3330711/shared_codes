pro simbad_get_radec_bright_star,noload=noload,glat=glat,glon=glon,alt=alt,ra=ra,dec=dec,elev=elev,azimuth=azimuth,vmag=vmag,name=name,hd=hd

get_timespan, t
t_init=t
ts=time_struct(t[0])
Syear=''
Smonth=''
Sday=''
Shour=''
Smin=''
Ssec=''
cdum=''
print,time_string(t[0])
reads,time_string(t[0]),Syear,cdum,Smonth,cdum,Sday,cdum,Shour,cdum,Smin,cdum,Ssec,format='(a4,a1,a2,a1,a2,a1,a2,a1,a2,a1,a2)'

if not keyword_set(noload) then begin
  datafile='lib/common/simbad_bright_star35.txt'
  ;datafile='lib/common/simbad_bright_star5.txt'
  nlines=File_Lines(datafile)
  result=strarr(nlines)
  OpenR,lun,datafile,/get_lun
  ReadF,lun,result
  close,/all

  print,'loading simbad'
  hd=strarr(n_elements(result)-2)
  name=strarr(n_elements(result)-2)
  ra=fltarr(n_elements(result)-2)
  dec=fltarr(n_elements(result)-2)
  vmag=fltarr(n_elements(result)-2)
  for loop=2L,n_elements(result)-1 do begin
    temp=strsplit(result[loop],string(9B),/extract,/preserve)
    hd[loop-2]=strtrim(temp[1],2)
    name[loop-2]=strtrim(temp[2],2)
    temp2=strsplit(temp[4],' ',/extract,/preserve)
    ra[loop-2]=temp2[0]
    dec[loop-2]=temp2[1]
    vmag[loop-2]=temp[5]
  endfor

name_planet=['Mercury','Venus','Mars','Jupiter','Saturn']
vmag_planet=[-1.,-4.,0.,-2.,0.]
PLANET_COORDS,[long(Syear),long(Smonth),long(Sday),long(Shour),long(Smin),long(Ssec)],RA_planet,DEC_planet,planet=name_planet
append_array,hd,name_planet
append_array,name,name_planet
append_array,ra,ra_planet
append_array,dec,dec_planet
append_array,vmag,vmag_planet

;alt in m
print,'eq2hor'
eq2hor, ra, dec, JULDAY(long(Smonth), long(Sday), long(Syear), long(Shour), long(Smin), long(Ssec)), elev, azimuth, lat=glat, lon=glon, altitude=alt

save,hd,name,ra,dec,vmag,elev,azimuth,file='save/simbad_bright_star.sav'
endif

;if keyword_set(noload) then restore,file='save/simbad_bright_star2015062413.sav'
if keyword_set(noload) then restore,file='save/simbad_bright_star.sav'

end