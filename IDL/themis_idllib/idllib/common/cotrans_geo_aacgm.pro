pro cotrans_geo_aacgm,glat=glat,glon=glon,r=r,aacgmmlat=aacgmmlat,aacgmmlon=aacgmmlon

get_timespan,t

if n_elements(glat) eq 1 then begin
temp=fltarr(1)
temp(0)=glat
glat=temp
temp(0)=glon
glon=temp
temp(0)=r
r=temp
endif

temp=where(strtrim(string(glat),1) eq 'NaN',count)
if count ge 1 then glat(temp)=0.
temp=where(strtrim(string(glon),1) eq 'NaN',count)
if count ge 1 then glon(temp)=0.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
time=dblarr(n_elements(glat)+1)
time(0)=t[1]
GEO=fltarr(n_elements(glat)+1,3)
GEO(0,0)=1.
GEO(0,1)=1.
GEO(0,2)=0.

time(1:n_elements(glat))=t[0]
GEO(1:n_elements(glat),0)=r*cos(glat*!pi/180)*cos(glon*!pi/180)
GEO(1:n_elements(glat),1)=r*cos(glat*!pi/180)*sin(glon*!pi/180)
GEO(1:n_elements(glat),2)=r*sin(glat*!pi/180)
store_data,'geoinput',data={x:time,y:GEO}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ttrace2equator,'geoinput',external_model='none',internal_model='igrf',par=par,in_coord='geo',out_coord='geo',trace_var_name='trace_eq',/NOBOUNDARY,RLIM=1000.
if median(glat) ge 0 then ttrace2iono,'geoinput_foot',external_model='none',internal_model='dip',par=par,in_coord='geo',out_coord='geo',trace_var_name='trace_io',/NOBOUNDARY,RLIM=1000.
if median(glat) lt 0 then ttrace2iono,'geoinput_foot',external_model='none',internal_model='dip',par=par,in_coord='geo',out_coord='geo',trace_var_name='trace_io',/NOBOUNDARY,RLIM=1000.,/south

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
copy_data,'geoinput_foot_foot','geoinput_aacgm_geo'
outname='geoinput_aacgm'
cotrans,outname+'_geo',outname+'_gei',/geo2gei,/ignore_dlimits
cotrans,outname+'_gei',outname+'_gse',/gei2gse,/ignore_dlimits
cotrans,outname+'_gse',outname+'_gsm',/gse2gsm,/ignore_dlimits
cotrans,outname+'_gsm',outname+'_sm',/gsm2sm,/ignore_dlimits

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
xyz_to_polar,'geoinput_aacgm_geo'
xyz_to_polar,'geoinput_aacgm_sm'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,outname+'_geo_mag',data=geo_r
get_data,outname+'_geo_th',data=geo_glat
get_data,outname+'_geo_phi',data=geo_glon
convert_geo_mag_dipole,geo_r.y,geo_glat.y,geo_glon.y,mag_mlat,mag_mlon

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
temp=where(strtrim(string(glat),1) eq 'NaN',count)
if count ge 1 then mag_mlat(temp)='NaN'
temp=where(strtrim(string(glon),1) eq 'NaN',count)
if count ge 1 then mag_mlon(temp)='NaN'


store_data,outname+'_mag_mlat',data={x:geo_r.x,y:mag_mlat}
store_data,outname+'_mag_mlon',data={x:geo_r.x,y:mag_mlon}

aacgmmlat=mag_mlat(1:n_elements(glat))
aacgmmlon=mag_mlon(1:n_elements(glat))

end