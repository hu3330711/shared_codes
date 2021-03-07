pro risrn_load_sri_beam2,mode=mode,timeres=timeres,nomed=nomed,number=number,nocal=nocal,quantity=quantity,nomlat=nomlat,plot_time=plot_time,verror=verror
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

dir=getenv('BIG_DIR')+'/big/GROUND/risrn/sri/beam/'  ;;; where data files are  ;'

if not keyword_set(quantity) then quantity='density'
quantity_char=quantity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if not keyword_set(mode) then mode='lp'
if not keyword_set(timeres) then timeres='1min'
if not keyword_set(number) then number='*'

print,dir+Syear+Smonth+Sday+'.'+number+'_'+mode+'_'+timeres+'.h5'
result=file_search(dir,Syear+Smonth+Sday+'.'+number+'_'+mode+'_'+timeres+'.h5',count=cnt)

if(cnt eq 0) then begin
    print,dir+Syear+Smonth+Sday+'.'+number+'_'+mode+'_'+timeres+'-*cal.h5'
    print,'No files found cal! '
    
    result=file_search(dir,Syear+Smonth+Sday+'.'+number+'_'+mode+'_'+timeres+'*.h5',count=cnt)

    if(cnt eq 0) then begin
        print,dir+Syear+Smonth+Sday+'.'+number+'_'+mode+'_'+timeres+'*.h5'	
        print,'No files found noncal!'
        goto,stopplot
    endif else begin
        file=result[0]
        file_id=h5f_open(file)
    endelse
endif else begin
    file=result[0]
    file_id=h5f_open(file)
endelse
print,result

filename_cut=Syear+Smonth+Sday+'.'+number+'_'+mode+'_'+timeres

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;see the whole structure
  h5dir='/'
  print,h5dir
  object1=strarr(h5g_get_nmembers(file_id,h5dir))
  type1=strarr(h5g_get_nmembers(file_id,h5dir))
  for loop=0,h5g_get_nmembers(file_id,h5dir)-1,1 do begin
    object1(loop)=h5g_get_member_name(file_id,h5dir,loop)
    temp=h5g_get_objinfo(file_id,h5dir+object1(loop))
    type1(loop)=temp.type
    print,loop,' ',object1(loop),'   ',type1(loop)
  endfor

print,';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;'

  for loop=0,h5g_get_nmembers(file_id,h5dir)-1,1 do begin
    if(type1(loop) eq 'DATASET') then continue
    print,h5dir+object1(loop)
    if h5g_get_nmembers(file_id,h5dir+object1(loop)) eq 0 then continue
    object2=strarr(h5g_get_nmembers(file_id,h5dir+object1(loop)))
    type2=strarr(h5g_get_nmembers(file_id,h5dir+object1(loop)))
    for loop2=0,h5g_get_nmembers(file_id,h5dir+object1(loop))-1,1 do begin
      object2(loop2)=h5g_get_member_name(file_id,h5dir+object1(loop),loop2)
      temp=h5g_get_objinfo(file_id,h5dir+object1(loop)+h5dir+object2(loop2))
      type2(loop2)=temp.type
      print,loop2,' ',object2(loop2),'   ',type2(loop2)
    endfor
  endfor

print,';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;'

  for loop=0,h5g_get_nmembers(file_id,h5dir)-1,1 do begin
    if(type1(loop) eq 'DATASET') then continue
    print,h5dir+object1(loop)
    if h5g_get_nmembers(file_id,h5dir+object1(loop)) eq 0 then continue
    object2=strarr(h5g_get_nmembers(file_id,h5dir+object1(loop)))
    type2=strarr(h5g_get_nmembers(file_id,h5dir+object1(loop)))
    for loop2=0,h5g_get_nmembers(file_id,h5dir+object1(loop))-1,1 do begin
      object2(loop2)=h5g_get_member_name(file_id,h5dir+object1(loop),loop2)
      temp=h5g_get_objinfo(file_id,h5dir+object1(loop)+h5dir+object2(loop2))
      type2(loop2)=temp.type
      if(type2(loop2) eq 'DATASET') then continue
      print,h5dir+object1(loop)+h5dir+object2(loop2)
      object3=strarr(h5g_get_nmembers(file_id,h5dir+object1(loop)+h5dir+object2(loop2)))
      type3=strarr(h5g_get_nmembers(file_id,h5dir+object1(loop)+h5dir+object2(loop2)))
      for loop3=0,h5g_get_nmembers(file_id,h5dir+object1(loop)+h5dir+object2(loop2))-1,1 do begin
        object3(loop3)=h5g_get_member_name(file_id,h5dir+object1(loop)+h5dir+object2(loop2),loop3)
        temp=h5g_get_objinfo(file_id,h5dir+object1(loop)+h5dir+object2(loop2)+h5dir+object3(loop3))
        type3(loop3)=temp.type
        print,loop3,' ',object3(loop3),'   ',type3(loop3)
      endfor
    endfor
  endfor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
dataset_id0=H5d_open(file_id,'/Time/UnixTime')
time=H5d_read(dataset_id0)
dataset_id1=H5d_open(file_id,'/Time/dtime')
dtime=H5d_read(dataset_id1)

if mode ne 'bc' then begin
dataset_id2=H5d_open(file_id,'/FittedParams/Altitude')
Altitude=H5d_read(dataset_id2)
dataset_id3=H5d_open(file_id,'/FittedParams/Errors')
Errors=H5d_read(dataset_id3)
dataset_id4=H5d_open(file_id,'/FittedParams/Fits')
Fits=H5d_read(dataset_id4)
dataset_id5=H5d_open(file_id,'/Geomag/Latitude')              ; when geog coord system chosen
glat=H5d_read(dataset_id5)
dataset_id5_2=H5d_open(file_id,'/Geomag/MagneticLatitude')    ; when aacgm coord system chosen
mlat=H5d_read(dataset_id5_2)
dataset_id6=H5d_open(file_id,'/Geomag/Longitude')         ; when geog coord system chosen
glong=H5d_read(dataset_id6)
dataset_id6_2=H5d_open(file_id,'/Geomag/MagneticLongitude')        ; when aacgm coord system chosen
mlong=H5d_read(dataset_id6_2)
dataset_id6_3=H5d_open(file_id,'/Geomag/Dip')        ; when aacgm coord system chosen
dip=H5d_read(dataset_id6_3)
dataset_id7=H5d_open(file_id,'/FittedParams/Ne')
density=H5d_read(dataset_id7)
dataset_id8=H5d_open(file_id,'/FittedParams/dNe')
dNe=H5d_read(dataset_id8)
endif

if keyword_set(nocal) then begin
  dataset_id2=H5d_open(file_id,'/NeFromPower/Altitude')
  Altitude=H5d_read(dataset_id2)
  dataset_id9=H5d_open(file_id,'/NeFromPower/Ne_NoTr')
  density=H5d_read(dataset_id9)
endif

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  time_med=double(reform(time(0,*)))

  ;Oplus_Ion_Fraction=transpose(reform(Fits(1,0,*,*,*)),[2,0,1])

  Ti=transpose(reform(Fits(1,0,*,*,*)),[2,0,1])
  Te=transpose(reform(Fits(1,1,*,*,*)),[2,0,1])
  if n_elements(fits[0,*,0,0,0]) eq 6 then Te=transpose(reform(Fits(1,5,*,*,*)),[2,0,1])

  Ion_Neutral_Collision_Frequency=transpose(reform(Fits(2,0,*,*,*)),[2,0,1])

  Vlos=-reform(Fits(3,0,*,*,*))
  Vlos_error=reform(Errors(3,0,*,*,*))
  density_transposed=transpose(density,[2,0,1])
  Vlos_transposed=transpose(Vlos,[2,0,1])
  Vlos_error_transposed=transpose(Vlos_error,[2,0,1])
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  store_data,'risrn_beam_density',data={x:time_med,y:density_transposed,mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude,dip:dip},dlim={spec:1,ysubtitle:['[/m^3]']}
  store_data,'risrn_beam_velocity',data={x:time_med,y:Vlos_transposed,mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude,dip:dip},dlim={spec:1,ysubtitle:['[m/s]']}
  store_data,'risrn_beam_velocity_error',data={x:time_med,y:Vlos_error_transposed,mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude,dip:dip},dlim={spec:1,ysubtitle:['[m/s]']}
  ;store_data,'risrn_beam_Oplus_Ion_Fraction',data={x:time_med,y:Oplus_Ion_Fraction,mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude},dlim={spec:1,ysubtitle:['[]']}
  ;store_data,'risrn_beam_Ion_Neutral_Collision_Frequency',data={x:time_med,y:Ion_Neutral_Collision_Frequency,mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude},dlim={spec:1,ysubtitle:['[Hz]']}
  store_data,'risrn_beam_Ti',data={x:time_med,y:Ti,mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude,dip:dip},dlim={spec:1,ysubtitle:['[K]']}
  store_data,'risrn_beam_Te',data={x:time_med,y:Te,mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude,dip:dip},dlim={spec:1,ysubtitle:['[K]']}
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;0-O+ Ion Fraction and ???, 1-Ti and Te, 2-Ion-Neutral Collision Frequency 3-los speed
  ;for i=0,n_elements(Fits(*,0,0,0,0))-1 do begin
  ;  for j=0,n_elements(Fits(0,*,0,0,0))-1 do begin
  ;    store_data,'risrn_beam_'+strtrim(string(i),1)+'_'+strtrim(string(j),1),data={x:time_med,y:transpose(reform(Fits(i,j,*,12,*))),v:reform(altitude(*,12)),mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude},dlim={spec:1,ysubtitle:['[m/s]']}
  ;  endfor
  ;endfor

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  if keyword_set(plot_time) then begin
  time_clip,'risrn_beam_density',plot_time[0],plot_time[1],newname='risrn_beam_density_clip'
  time_clip,'risrn_beam_velocity',plot_time[0],plot_time[1],newname='risrn_beam_velocity_clip'
  time_clip,'risrn_beam_Ti',plot_time[0],plot_time[1],newname='risrn_beam_Ti_clip'
  time_clip,'risrn_beam_Te',plot_time[0],plot_time[1],newname='risrn_beam_Te_clip'
  get_data,'risrn_beam_density',data=data
  get_data,'risrn_beam_density_clip',data=clip
  str_element,data,'x',clip.x,/add_replace
  str_element,data,'y',clip.y,/add_replace
  store_data,'risrn_beam_density',data=data
  get_data,'risrn_beam_velocity',data=data
  get_data,'risrn_beam_velocity_clip',data=clip
  str_element,data,'x',clip.x,/add_replace
  str_element,data,'y',clip.y,/add_replace
  store_data,'risrn_beam_velocity',data=data
  get_data,'risrn_beam_Ti',data=data
  get_data,'risrn_beam_Ti_clip',data=clip
  str_element,data,'x',clip.x,/add_replace
  str_element,data,'y',clip.y,/add_replace
  store_data,'risrn_beam_Ti',data=data
  get_data,'risrn_beam_Te',data=data
  get_data,'risrn_beam_Te_clip',data=clip
  str_element,data,'x',clip.x,/add_replace
  str_element,data,'y',clip.y,/add_replace
  store_data,'risrn_beam_Te',data=data
  del_data,['risrn_beam_density_clip','risrn_beam_velocity_clip','risrn_beam_T?_clip']
  endif
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  get_data,'risrn_beam_'+quantity_char,data=data
  time_med=data.x
  quantity=data.y
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  for beam=0,n_elements(glat(0,*))-1,1 do begin
    beam_char=strtrim(string(beam+1),1)
    if beam+1 le 9 then beam_char='0'+beam_char

  mlat_temp=mlat(*,beam)
  index=where(strtrim(string(mlat_temp),1) eq '-NaN',count)
  if(count ge 1) then mlat_temp(index)=max(mlat_temp)+0.01

  alt_temp=altitude(*,beam)/1000
  index=where(strtrim(string(alt_temp),1) eq '-NaN',count)
  if(count ge 1) then alt_temp(index)=max(alt_temp)+0.01

  if quantity_char eq 'density' then ysubtitle='[/m^3]'
  if quantity_char eq 'velocity' then ysubtitle='[m/s]'
  if quantity_char eq 'Te' then ysubtitle='[K]'
  if quantity_char eq 'Ti' then ysubtitle='[K]'
    if not keyword_set(nomlat) then begin
      store_data,'risrn_beam_'+quantity_char+'_mlat_'+mode+'_beam'+beam_char,data={x:time_med,y:reform(quantity(*,*,beam)),v:mlat_temp,mlong:mlong(*,beam),glat:glat(*,beam),glong:glong(*,beam),alt:altitude(*,beam)/1000},dlim={spec:1,ysubtitle:ysubtitle,zlog:1}
      if quantity_char eq 'velocity' and keyword_set(verror) then store_data,'risrn_beam_velocity_error_mlat_'+mode+'_beam'+beam_char,data={x:time_med,y:reform(Vlos_error_transposed(*,*,beam)),v:mlat_temp,mlong:mlong(*,beam),glat:glat(*,beam),glong:glong(*,beam),alt:altitude(*,beam)/1000},dlim={spec:1,ysubtitle:['[m/s]'],zlog:0}
    endif

    store_data,'risrn_beam_'+quantity_char+'_alt_'+mode+'_beam'+beam_char,data={x:time_med,y:reform(quantity(*,*,beam)),mlat:mlat_temp,mlong:mlong(*,beam),glat:glat(*,beam),glong:glong(*,beam),v:alt_temp},dlim={spec:1,ysubtitle:[ysubtitle],zlog:1}
    if quantity_char eq 'velocity' and keyword_set(verror) then store_data,'risrn_beam_velocity_error_alt_'+mode+'_beam'+beam_char,data={x:time_med,y:reform(Vlos_error_transposed(*,*,beam)),mlat:mlat_temp,mlong:mlong(*,beam),glat:glat(*,beam),glong:glong(*,beam),v:alt_temp},dlim={spec:1,ysubtitle:['[m/s]'],zlog:0}
  endfor

if keyword_set(nocal) then stop

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;median
  if not keyword_set(nomed) then begin
    for beam=0,n_elements(glat(0,*))-1,1 do begin
      beam_char=strtrim(string(beam+1),1)
      if beam+1 le 9 then beam_char='0'+beam_char

      get_data,'risrn_beam_'+quantity_char+'_alt_'+mode+'_beam'+beam_char,data=data,dlim=dlim
      value=data.y
      for gate=2L,n_elements(data.y(0,*))-3,1 do begin
        value(*,gate)=median(reform(data.y(*,gate-2:gate+2)),dimension=2)
      endfor
      data.y=value
      store_data,'risrn_beam_'+quantity_char+'_alt_'+mode+'_med_beam'+beam_char,data=data,dlim=dlim
      
      if not keyword_set(nomlat) then begin
        get_data,'risrn_beam_'+quantity_char+'_mlat_'+mode+'_beam'+beam_char,data=data,dlim=dlim
        value=data.y
        for gate=2L,n_elements(data.y(0,*))-3,1 do begin
          value(*,gate)=median(reform(data.y(*,gate-2:gate+2)),dimension=2)
        endfor
        data.y=value
        store_data,'risrn_beam_'+quantity_char+'_mlat_'+mode+'_med_beam'+beam_char,data=data,dlim=dlim
      endif
    endfor
  endif
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;ac lp combined
  if mode eq 'ac' then begin
    for beam=0,n_elements(glat(0,*))-1,1 do begin
      beam_char=strtrim(string(beam+1),1)
      if beam+1 le 9 then beam_char='0'+beam_char
      store_data,'risrn_beam_'+quantity_char+'_alt_beam'+beam_char,data='risrn_beam_'+quantity_char+'_alt_lp_beam'+beam_char+' '+'risrn_beam_'+quantity_char+'_alt_ac_beam'+beam_char
      store_data,'risrn_beam_'+quantity_char+'_alt_med_beam'+beam_char,data='risrn_beam_'+quantity_char+'_alt_lp_med_beam'+beam_char+' '+'risrn_beam_'+quantity_char+'_alt_ac_med_beam'+beam_char
      store_data,'risrn_beam_'+quantity_char+'_mlat_beam'+beam_char,data='risrn_beam_'+quantity_char+'_mlat_lp_beam'+beam_char+' '+'risrn_beam_'+quantity_char+'_mlat_ac_beam'+beam_char
      store_data,'risrn_beam_'+quantity_char+'_mlat_med_beam'+beam_char,data='risrn_beam_'+quantity_char+'_mlat_lp_med_beam'+beam_char+' '+'risrn_beam_'+quantity_char+'_mlat_ac_med_beam'+beam_char
    endfor
  endif
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  loadct,43,file=getenv('BIG_DIR')+'/big/SATELLITE/themis/software/tdas/current/idl/themis/common/thm_ctables/thm_colors.tbl'
  ylim,['risrn_beam_'+quantity_char+'_mlat*_beam??'],75,90
  ylim,['risrn_beam_'+quantity_char+'_alt*_beam??'],100,400
  zlim,['risrn_beam_velocity_*_beam??'],-500,500
  zlim,['risrn_beam_density_*_beam??'],1e9,1e11
  zlim,['risrn_beam_T?_*_beam??'],0,3000

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

H5D_CLOSE, dataset_id1
H5D_CLOSE, dataset_id2
H5D_CLOSE, dataset_id3
H5D_CLOSE, dataset_id4
H5D_CLOSE, dataset_id5
H5D_CLOSE, dataset_id6
H5D_CLOSE, dataset_id7
H5D_CLOSE, dataset_id8
H5F_CLOSE, file_id

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
risrn_aacgm,mode=mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;store data for scan

;data array
tplot_names,'risrn_beam_'+quantity_char+'_alt_'+mode+'_beam*',names=names
nbeams=n_elements(names)
get_data,'risrn_beam_'+quantity_char+'_alt_'+mode+'_beam01',data=data
time=data.x
scan_quantity=fltarr(n_elements(data.x),nbeams,n_elements(data.y(0,*)))
scan_vel_error=fltarr(n_elements(data.x),nbeams,n_elements(data.y(0,*)))
fov_loc_center=fltarr(2,nbeams,n_elements(data.y(0,*)))
fov_loc_full_rel=fltarr(2,4,nbeams,n_elements(data.y(0,*)))
beam_lat=fltarr(nbeams,n_elements(data.y(0,*)))
beam_lon=fltarr(nbeams,n_elements(data.y(0,*)))
beam_alt=fltarr(nbeams,n_elements(data.y(0,*)))
beam_mlat=fltarr(nbeams,n_elements(data.y(0,*)))
beam_mlon=fltarr(nbeams,n_elements(data.y(0,*)))

;get_data
for beam=0,nbeams-1,1 do begin
  cbeamnum=strmid(names(beam),strlen(names(beam))-2,2)
  get_data,'risrn_beam_'+quantity_char+'_alt_'+mode+'_beam'+cbeamnum,data=quantity
  get_data,'risrn_beam_velocity_error_alt_'+mode+'_beam'+cbeamnum,data=vel_error
  for loop=0L,n_elements(time)-1,1 do begin
    scan_quantity(loop,beam,0:n_elements(quantity.y(loop,*))-1)=quantity.y(loop,*)
    if quantity_char eq 'velocity' and keyword_set(verror) then scan_vel_error(loop,beam,0:n_elements(quantity.y(loop,*))-1)=vel_error.y(loop,*)
    beam_lat(beam,0:n_elements(quantity.aacgmglat)-1)=quantity.aacgmglat
    beam_lon(beam,0:n_elements(quantity.aacgmglon)-1)=quantity.aacgmglon
    beam_alt(beam,0:n_elements(quantity.v)-1)=quantity.v
    beam_mlat(beam,0:n_elements(quantity.aacgmmlat)-1)=quantity.aacgmmlat
    beam_mlon(beam,0:n_elements(quantity.aacgmmlon)-1)=quantity.aacgmmlon
    fov_loc_center(0,beam,0:n_elements(quantity.aacgmglat)-1)=quantity.aacgmglat
    fov_loc_center(1,beam,0:n_elements(quantity.aacgmglon)-1)=quantity.aacgmglon
    lat_lon_factor=cos(min(quantity.glat,/nan)*!pi/180.)
    atan_slope=atan((quantity.aacgmglat-min(quantity.glat,/nan))*lat_lon_factor/(quantity.glong-median(quantity.glong)))
    index=where(beam_lon(beam,*)-median(quantity.glong) lt 0,count)
    if(count ge 1) then atan_slope(index)=atan_slope(index)+!pi
    lat_lon_factor=1
    fov_loc_full_rel(0,0,beam,0:n_elements(atan_slope)-1)=-sin(atan_slope)+cos(atan_slope)*lat_lon_factor
    fov_loc_full_rel(0,1,beam,0:n_elements(atan_slope)-1)=-sin(atan_slope)-cos(atan_slope)*lat_lon_factor
    fov_loc_full_rel(0,2,beam,0:n_elements(atan_slope)-1)= sin(atan_slope)-cos(atan_slope)*lat_lon_factor
    fov_loc_full_rel(0,3,beam,0:n_elements(atan_slope)-1)= sin(atan_slope)+cos(atan_slope)*lat_lon_factor
    fov_loc_full_rel(1,0,beam,0:n_elements(atan_slope)-1)=-cos(atan_slope)-sin(atan_slope)*lat_lon_factor
    fov_loc_full_rel(1,1,beam,0:n_elements(atan_slope)-1)=-cos(atan_slope)+sin(atan_slope)*lat_lon_factor
    fov_loc_full_rel(1,2,beam,0:n_elements(atan_slope)-1)= cos(atan_slope)+sin(atan_slope)*lat_lon_factor
    fov_loc_full_rel(1,3,beam,0:n_elements(atan_slope)-1)= cos(atan_slope)-sin(atan_slope)*lat_lon_factor
  endfor
endfor

;store data
store_data,'risrn_scan_'+quantity_char+'_'+mode,data={x:time,y:scan_quantity,fov_loc_center:fov_loc_center,fov_loc_full_rel:fov_loc_full_rel,glat:beam_lat,glon:beam_lon,alt:beam_alt,aacgmmlat:beam_mlat,aacgmmlon:beam_mlon},dlim={ysubtitle:'geog'}
if quantity_char eq 'velocity' and keyword_set(verror) then store_data,'risrn_scan_velocity_error_'+mode,data={x:time,y:scan_vel_error,fov_loc_center:fov_loc_center,fov_loc_full_rel:fov_loc_full_rel,glat:beam_lat,glon:beam_lon,alt:beam_alt,aacgmmlat:beam_mlat,aacgmmlon:beam_mlon},dlim={ysubtitle:'geog'}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if not keyword_set(nomed) then begin
  ;get_data
for beam=0,nbeams-1,1 do begin
  cbeamnum=strmid(names(beam),strlen(names(beam))-2,2)
  get_data,'risrn_beam_'+quantity_char+'_alt_'+mode+'_med_beam'+cbeamnum,data=quantity
  get_data,'risrn_beam_velocity_error_alt_'+mode+'_beam'+cbeamnum,data=vel_error
  for loop=0L,n_elements(time)-1,1 do begin
    scan_quantity(loop,beam,*)=quantity.y(loop,*)
    if quantity_char eq 'velocity' and keyword_set(verror) then scan_vel_error(loop,beam,*)=vel_error.y(loop,*)
    beam_lat(beam,*)=quantity.aacgmglat
    beam_lon(beam,*)=quantity.aacgmglon
    beam_alt(beam,*)=quantity.v
    beam_mlat(beam,*)=quantity.aacgmmlat
    beam_mlon(beam,*)=quantity.aacgmmlon
    fov_loc_center(0,beam,*)=quantity.aacgmglat
    fov_loc_center(1,beam,*)=quantity.aacgmglon
    lat_lon_factor=cos(min(quantity.glat,/nan)*!pi/180.)
    atan_slope=atan((quantity.aacgmglat-min(quantity.glat,/nan))*lat_lon_factor/(quantity.glong-median(quantity.glong)))
    index=where(beam_lon(beam,*)-median(quantity.glong) lt 0,count)
    if(count ge 1) then atan_slope(index)=atan_slope(index)+!pi
    lat_lon_factor=1
    fov_loc_full_rel(0,0,beam,*)=-sin(atan_slope)+cos(atan_slope)*lat_lon_factor
    fov_loc_full_rel(0,1,beam,*)=-sin(atan_slope)-cos(atan_slope)*lat_lon_factor
    fov_loc_full_rel(0,2,beam,*)= sin(atan_slope)-cos(atan_slope)*lat_lon_factor
    fov_loc_full_rel(0,3,beam,*)= sin(atan_slope)+cos(atan_slope)*lat_lon_factor
    fov_loc_full_rel(1,0,beam,*)=-cos(atan_slope)-sin(atan_slope)*lat_lon_factor
    fov_loc_full_rel(1,1,beam,*)=-cos(atan_slope)+sin(atan_slope)*lat_lon_factor
    fov_loc_full_rel(1,2,beam,*)= cos(atan_slope)+sin(atan_slope)*lat_lon_factor
    fov_loc_full_rel(1,3,beam,*)= cos(atan_slope)-sin(atan_slope)*lat_lon_factor
  endfor
endfor

;store data
store_data,'risrn_scan_'+quantity_char+'_'+mode+'_med',data={x:time,y:scan_quantity,fov_loc_center:fov_loc_center,fov_loc_full_rel:fov_loc_full_rel,glat:beam_lat,glon:beam_lon,alt:beam_alt,aacgmmlat:beam_mlat,aacgmmlon:beam_mlon},dlim={ysubtitle:'geog'}
if quantity_char eq 'velocity' and keyword_set(verror) then store_data,'risrn_scan_velocity_error_'+mode+'_med',data={x:time,y:scan_vel_error,fov_loc_center:fov_loc_center,fov_loc_full_rel:fov_loc_full_rel,glat:beam_lat,glon:beam_lon,alt:beam_alt,aacgmmlat:beam_mlat,aacgmmlon:beam_mlon},dlim={ysubtitle:'geog'}


endif

thm_median,'risrn_beam_'+quantity_char+'_alt_'+mode+'_med_beam??',84.*3
del_data,'risrn_beam_'+quantity_char+'_alt_'+mode+'_med_beam??_md'

stopplot:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;plot

Wyear=''
Wmonth=''
Wday=''
Whour=''
Wmin=''
Wsec=''
cdum=''
if not keyword_set(plot_time) then plot_time=t
reads,time_string(plot_time[0]),Wyear,cdum,Wmonth,cdum,Wday,cdum,Whour,cdum,Wmin,cdum,Ssec,format='(a4,a1,a2,a1,a2,a1,a2,a1,a2,a1,a2)'

tlimit,plot_time
window,xs=600,ys=600
ylim,'*risrn*',150,400
zlim,'*risrn*den*',5e10,5e11,1
zlim,'*risrn*vel*',-600,600,0

midfix=''
suffix=''
if not keyword_set(nomed) then midfix='_med'
if not keyword_set(nomed) then suffix='_m'

if quantity_char eq 'density' then begin
tplot,'risrn_beam_density_alt_lp'+midfix+'_beam'+['07','14','21','28','35','42']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_den1'
tplot,'risrn_beam_density_alt_lp'+midfix+'_beam'+['08','15','22','29','36','43']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_den2'
tplot,'risrn_beam_density_alt_lp'+midfix+'_beam'+['01','04','11','18','25','32','39','46','49']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_den3'
tplot,'risrn_beam_density_alt_lp'+midfix+'_beam'+['02','05','12','19','26','33','40','47','50']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_den4'
tplot,'risrn_beam_density_alt_lp'+midfix+'_beam'+['03','06','13','20','27','34','34','41','48','51']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_den5'
tplot,'risrn_beam_density_alt_lp'+midfix+'_beam'+['09','16','23','30','37','44']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_den6'
tplot,'risrn_beam_density_alt_lp'+midfix+'_beam'+['10','17','24','31','38','45']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_den7'
endif

if quantity_char eq 'velocity' then begin
tplot,'risrn_beam_velocity_alt_lp'+midfix+'_beam'+['07','14','21','28','35','42']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_vel1'
tplot,'risrn_beam_velocity_alt_lp'+midfix+'_beam'+['08','15','22','29','36','43']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_vel2'
tplot,'risrn_beam_velocity_alt_lp'+midfix+'_beam'+['01','04','11','18','25','32','39','46','49']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_vel3'
tplot,'risrn_beam_velocity_alt_lp'+midfix+'_beam'+['02','05','12','19','26','33','40','47','50']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_vel4'
tplot,'risrn_beam_velocity_alt_lp'+midfix+'_beam'+['03','06','13','20','27','34','34','41','48','51']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_vel5'
tplot,'risrn_beam_velocity_alt_lp'+midfix+'_beam'+['09','16','23','30','37','44']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_vel6'
tplot,'risrn_beam_velocity_alt_lp'+midfix+'_beam'+['10','17','24','31','38','45']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_vel7'
endif

ylim,'*risrn*T*',150,400
zlim,'*risrn*Te*',0,3000,0
zlim,'*risrn*Ti*',0,2000,0
if quantity_char eq 'Ti' then begin
tplot,'risrn_beam_Ti_alt_lp'+midfix+'_beam'+['08','15','22','29','36','43']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_ti2'
tplot,'risrn_beam_Ti_alt_lp'+midfix+'_beam'+['02','05','12','19','26','33','40','47','50']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_ti4'
tplot,'risrn_beam_Ti_alt_lp'+midfix+'_beam'+['09','16','23','30','37','44']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_ti6'
endif
if quantity_char eq 'Te' then begin
tplot,'risrn_beam_Te_alt_lp'+midfix+'_beam'+['08','15','22','29','36','43']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_te2'
tplot,'risrn_beam_Te_alt_lp'+midfix+'_beam'+['02','05','12','19','26','33','40','47','50']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_te4'
tplot,'risrn_beam_Te_alt_lp'+midfix+'_beam'+['09','16','23','30','37','44']+suffix
makepng,'png/ql_risrn/risrn_'+Wyear+Wmonth+Wday+Whour+Wmin+'_'+filename_cut+'_te6'
endif
end
