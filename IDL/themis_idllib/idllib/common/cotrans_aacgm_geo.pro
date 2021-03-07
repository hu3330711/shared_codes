pro cotrans_aacgm_geo,glat=glat,glon=glon,r=r,aacgmmlat=aacgmmlat,aacgmmlon=aacgmmlon

get_timespan,t
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
convert_mag_geo_dipole,r,aacgmmlat,aacgmmlon,glat,glon
;if n_elements(r) eq 1 then convert_mag_geo_dipole,r,aacgmmlat,aacgmmlon,glat,glon
;if n_elements(r) ge 2 then begin
;  glat=fltarr(n_elements(r))
;  glon=fltarr(n_elements(r))
;  for loop=0,n_elements(r)-1,1 do begin
;    convert_mag_geo_dipole,r(loop),aacgmmlat(loop),aacgmmlon(loop),glat_temp,glon_temp
;    glat(loop)=glat_temp
;    glon(loop)=glon_temp
;  endfor
;endif

time=dblarr(n_elements(glat)+1)
time(0)=t[1]
GEO=fltarr(n_elements(glat)+1,3)
GEO(0,0)='NaN'
GEO(0,1)='NaN'
GEO(0,2)='NaN'

time(1:n_elements(glat))=t[0]
GEO(1:n_elements(glat),0)=r*cos(glat*!pi/180)*cos(glon*!pi/180)
GEO(1:n_elements(glat),1)=r*cos(glat*!pi/180)*sin(glon*!pi/180)
GEO(1:n_elements(glat),2)=r*sin(glat*!pi/180)
store_data,'aacgm_geoinput',data={x:time,y:GEO}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ttrace2equator,'aacgm_geoinput',external_model='none',internal_model='dip',in_coord='geo',out_coord='geo',trace_var_name='trace_eq',/NOBOUNDARY,RLIM=1000.
if median(glat ge 0) then ttrace2iono,'aacgm_geoinput_foot',external_model='none',internal_model='igrf',in_coord='geo',out_coord='geo',trace_var_name='trace_io',/NOBOUNDARY,RLIM=1000.
if median(glat lt 0) then ttrace2iono,'aacgm_geoinput_foot',external_model='none',internal_model='igrf',in_coord='geo',out_coord='geo',trace_var_name='trace_io',/NOBOUNDARY,RLIM=1000.,/south

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
copy_data,'aacgm_geoinput_foot_foot','aacgm_geoinput_origgeo_geo'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
xyz_to_polar,'aacgm_geoinput_origgeo_geo'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,'aacgm_geoinput_origgeo_geo_mag',data=geo_r
get_data,'aacgm_geoinput_origgeo_geo_th',data=geo_glat
get_data,'aacgm_geoinput_origgeo_geo_phi',data=geo_glon

glat=geo_glat.y(1:n_elements(glat))
glon=geo_glon.y(1:n_elements(glat))

end