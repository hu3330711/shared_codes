;********main**************
;'
pro pfisr_load_sri_beam,mode=mode,timeres=timeres,nomed=nomed,number=number,nocal=nocal,quantity=quantity,plot_time=plot_time
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

dir=getenv('BIG_DIR')+'/big/GROUND/PFISR/sri/beam/'  ;;; where data files are  ;'

if not keyword_set(quantity) then quantity='density'
quantity_char=quantity


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if not keyword_set(mode) then mode='lp'
if not keyword_set(timeres) then timeres='1min'
if not keyword_set(number) then number='*'

result=file_search(dir,Syear+Smonth+Sday+'.'+number+'_'+mode+'_'+timeres+'-cal.h5',count=cnt)
print,dir+Syear+Smonth+Sday+'.'+number+'_'+mode+'_'+timeres+'-cal.h5'

if(cnt eq 0) then begin
    print,'No files found above. Searching next.'
    result=file_search(dir,Syear+Smonth+Sday+'.'+number+'_'+mode+'_'+timeres+'-fitcal.h5',count=cnt)
    print,dir+Syear+Smonth+Sday+'.'+number+'_'+mode+'_'+timeres+'-fitcal.h5'
    if(cnt eq 0) then begin
      print,'No files found above'
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
dataset_id7=H5d_open(file_id,'/FittedParams/Ne')
density=H5d_read(dataset_id7)
dataset_id8=H5d_open(file_id,'/FittedParams/dNe')
dNe=H5d_read(dataset_id8)
dataset_id4_2=H5d_open(file_id,'/FittedParams/IonMass')
ionmass=H5d_read(dataset_id4_2)
endif

if keyword_set(nocal) then begin
  dataset_id2=H5d_open(file_id,'/NeFromPower/Altitude')
  Altitude_nocal=H5d_read(dataset_id2)
  dataset_id9=H5d_open(file_id,'/NeFromPower/Ne_NoTr')
  density=H5d_read(dataset_id9)
endif

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;FITS            DOUBLE    = Array[4, 6, 22, 4, 639]
;parameter. ion fraction, temperature, collision frequency, LOS velocity
;species specified in ionmass. O+, O2+, NO+, N2+, N+, e-
;range
;beam
;time

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;common
  time_med=double(reform(time(0,*)))
  Vlos=-reform(Fits(3,0,*,*,*))
  Vlos_error=reform(Errors(3,0,*,*,*))
  density_transposed=transpose(density,[2,0,1])
  Vlos_transposed=transpose(Vlos,[2,0,1])
  Vlos_error_transposed=transpose(Vlos_error,[2,0,1])
  Oplus_Ion_Fraction=transpose(reform(Fits(1,0,*,*,*)),[2,0,1])

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;species
  ion_fraction=(transpose(reform(Fits(0,*,*,*,*)),[3,1,2,0]))[*,*,*,0:4]
  Ti=transpose(reform(Fits(1,0,*,*,*)),[2,0,1]);o+
  Te=transpose(reform(Fits(1,5,*,*,*)),[2,0,1])
  collision_frequency=(transpose(reform(Fits(2,*,*,*,*)),[3,1,2,0]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  if keyword_set(plot_time) then begin
    index=where(time_med ge time_double(plot_time[0]) and time_med le time_double(plot_time[1]))
    time_med=time_med[index]
    Vlos=Vlos[index,*,*]
    Vlos_error=Vlos_error[index,*,*]
    density_transposed=density_transposed[index,*,*]
    Vlos_transposed=Vlos_transposed[index,*,*]
    Vlos_error_transposed=Vlos_error_transposed[index,*,*]
    Oplus_Ion_Fraction=Oplus_Ion_Fraction[index,*,*]
    ion_fraction=ion_fraction[index,*,*,*]
    Ti=Ti[index,*,*]
    Te=Te[index,*,*]
    collision_frequency=collision_frequency[index,*,*,*]
  endif

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  store_data,'pfisr_beam_density',data={x:time_med,y:density_transposed,mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude},dlim={spec:1,ysubtitle:['[/m^3]']}
  if keyword_set(nocal) then   store_data,'pfisr_beam_density',data={x:time_med,y:density_transposed,mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude_nocal},dlim={spec:1,ysubtitle:['[/m^3]']}
  store_data,'pfisr_beam_velocity',data={x:time_med,y:Vlos_transposed,mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude},dlim={spec:1,ysubtitle:['[m/s]']}
  store_data,'pfisr_beam_velocity_error',data={x:time_med,y:Vlos_error_transposed,mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude},dlim={spec:1,ysubtitle:['[m/s]']}
  store_data,'pfisr_beam_ionfraction',data={x:time_med,y:ion_fraction,mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude},dlim={spec:1,ysubtitle:['[]']}
  store_data,'pfisr_beam_collisionfrequency',data={x:time_med,y:collision_frequency,mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude},dlim={spec:1,ysubtitle:['[Hz]']}
  store_data,'pfisr_beam_Ti',data={x:time_med,y:Ti,mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude},dlim={spec:1,ysubtitle:['[K]']}
  store_data,'pfisr_beam_Te',data={x:time_med,y:Te,mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude},dlim={spec:1,ysubtitle:['[K]']}

  ;get_data,'pfisr_beam_ionfraction',data=data
  ;store_data,'r_o',data={x:data.x,y:reform(data.y[*,*,1,0]),v:reform(data.alt[*,1])/1000},dlim={spec:1}
  ;store_data,'r_no',data={x:data.x,y:reform(data.y[*,*,1,2]),v:reform(data.alt[*,1])/1000},dlim={spec:1}
  get_data,'pfisr_beam_collisionfrequency',data=data
  store_data,'nu_o',data={x:data.x,y:reform(data.y[*,*,1,0]),v:reform(data.alt[*,1])/1000},dlim={spec:1}
  store_data,'nu_o2',data={x:data.x,y:reform(data.y[*,*,1,1]),v:reform(data.alt[*,1])/1000},dlim={spec:1}
  store_data,'nu_no',data={x:data.x,y:reform(data.y[*,*,1,2]),v:reform(data.alt[*,1])/1000},dlim={spec:1}
  store_data,'nu_n2',data={x:data.x,y:reform(data.y[*,*,1,3]),v:reform(data.alt[*,1])/1000},dlim={spec:1}
  store_data,'nu_n',data={x:data.x,y:reform(data.y[*,*,1,4]),v:reform(data.alt[*,1])/1000},dlim={spec:1}
  ylim,'nu_*',50,300
  zlim,'nu_*',1e0,1e5,1
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;0-O+ Ion Fraction and ???, 1-Ti and Te, 2-Ion-Neutral Collision Frequency 3-los speed
  ;for i=0,n_elements(Fits(*,0,0,0,0))-1 do begin
  ;  for j=0,n_elements(Fits(0,*,0,0,0))-1 do begin
  ;    store_data,'pfisr_beam_'+strtrim(string(i),1)+'_'+strtrim(string(j),1),data={x:time_med,y:transpose(reform(Fits(i,j,*,12,*))),v:reform(altitude(*,12)),mlat:mlat,mlong:mlong,glat:glat,glong:glong,alt:altitude},dlim={spec:1,ysubtitle:['[m/s]']}
  ;  endfor
  ;endfor

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

  if keyword_set(nocal) then begin
    alt_nocal_temp=altitude_nocal(*,beam)/1000
    index=where(strtrim(string(alt_nocal_temp),1) eq '-NaN',count)
    if(count ge 1) then alt_nocal_temp(index)=max(alt_nocal_temp)+0.01
  endif

  if quantity eq 'density' then begin
    store_data,'pfisr_beam_density_mlat_'+mode+'_beam'+beam_char,data={x:time_med,y:reform(density_transposed(*,*,beam)),v:mlat_temp,mlong:mlong(*,beam),glat:glat(*,beam),glong:glong(*,beam),alt:altitude(*,beam)/1000},dlim={spec:1,ysubtitle:['[/m^3]'],zlog:1}
    if keyword_set(nocal) then store_data,'pfisr_beam_density_mlat_'+mode+'_beam'+beam_char,data={x:time_med,y:reform(density_transposed(*,*,beam)),v:mlat_temp,mlong:mlong(*,beam),glat:glat(*,beam),glong:glong(*,beam),alt:altitude_nocal(*,beam)/1000},dlim={spec:1,ysubtitle:['[/m^3]'],zlog:1}
    store_data,'pfisr_beam_density_alt_'+mode+'_beam'+beam_char,data={x:time_med,y:reform(density_transposed(*,*,beam)),mlat:mlat_temp,mlong:mlong(*,beam),glat:glat(*,beam),glong:glong(*,beam),v:alt_temp},dlim={spec:1,ysubtitle:['[/m^3]'],zlog:1}
    if keyword_set(nocal) then store_data,'pfisr_beam_density_alt_'+mode+'_beam'+beam_char,data={x:time_med,y:reform(density_transposed(*,*,beam)),mlat:mlat_temp,mlong:mlong(*,beam),glat:glat(*,beam),glong:glong(*,beam),v:alt_nocal_temp},dlim={spec:1,ysubtitle:['[/m^3]'],zlog:1}
  endif
  if quantity eq 'velocity' then begin
    store_data,'pfisr_beam_velocity_mlat_'+mode+'_beam'+beam_char,data={x:time_med,y:reform(Vlos_transposed(*,*,beam)),v:mlat_temp,mlong:mlong(*,beam),glat:glat(*,beam),glong:glong(*,beam),alt:altitude(*,beam)/1000},dlim={spec:1,ysubtitle:['[m/s]'],zlog:0}
    store_data,'pfisr_beam_velocity_error_mlat_'+mode+'_beam'+beam_char,data={x:time_med,y:reform(Vlos_error_transposed(*,*,beam)),v:mlat_temp,mlong:mlong(*,beam),glat:glat(*,beam),glong:glong(*,beam),alt:altitude(*,beam)/1000},dlim={spec:1,ysubtitle:['[m/s]'],zlog:0}
    store_data,'pfisr_beam_velocity_alt_'+mode+'_beam'+beam_char,data={x:time_med,y:reform(Vlos_transposed(*,*,beam)),mlat:mlat_temp,mlong:mlong(*,beam),glat:glat(*,beam),glong:glong(*,beam),v:alt_temp},dlim={spec:1,ysubtitle:['[m/s]'],zlog:0}
    store_data,'pfisr_beam_velocity_error_alt_'+mode+'_beam'+beam_char,data={x:time_med,y:reform(Vlos_error_transposed(*,*,beam)),mlat:mlat_temp,mlong:mlong(*,beam),glat:glat(*,beam),glong:glong(*,beam),v:alt_temp},dlim={spec:1,ysubtitle:['[m/s]'],zlog:0}
  endif
  if quantity eq 'Ti' then begin
    store_data,'pfisr_beam_Ti_mlat_'+mode+'_beam'+beam_char,data={x:time_med,y:reform(Ti(*,*,beam)),v:mlat_temp,mlong:mlong(*,beam),glat:glat(*,beam),glong:glong(*,beam),alt:altitude(*,beam)/1000},dlim={spec:1,ysubtitle:['[K]'],zlog:1}
    store_data,'pfisr_beam_Ti_alt_'+mode+'_beam'+beam_char,data={x:time_med,y:reform(Ti(*,*,beam)),mlat:mlat_temp,mlong:mlong(*,beam),glat:glat(*,beam),glong:glong(*,beam),v:alt_temp},dlim={spec:1,ysubtitle:['[K]'],zlog:0}
  endif
  if quantity eq 'Te' then begin
    store_data,'pfisr_beam_Te_mlat_'+mode+'_beam'+beam_char,data={x:time_med,y:reform(Te(*,*,beam)),v:mlat_temp,mlong:mlong(*,beam),glat:glat(*,beam),glong:glong(*,beam),alt:altitude(*,beam)/1000},dlim={spec:1,ysubtitle:['[K]'],zlog:1}
    store_data,'pfisr_beam_Te_alt_'+mode+'_beam'+beam_char,data={x:time_med,y:reform(Te(*,*,beam)),mlat:mlat_temp,mlong:mlong(*,beam),glat:glat(*,beam),glong:glong(*,beam),v:alt_temp},dlim={spec:1,ysubtitle:['[K]'],zlog:0}
  endif

  endfor

;if keyword_set(nocal) then stop

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;median
  if not keyword_set(nomed) then begin
    for beam=0,n_elements(glat(0,*))-1,1 do begin
      beam_char=strtrim(string(beam+1),1)
      if beam+1 le 9 then beam_char='0'+beam_char
      ;get_data,'pfisr_beam_density_alt_'+mode+'_beam'+beam_char,data=data,dlim=dlim
      ;data.y=median(data.y,2)
      ;store_data,'pfisr_beam_density_alt_'+mode+'_med_beam'+beam_char,data=data,dlim=dlim
      ;get_data,'pfisr_beam_vlos_alt_'+mode+'_beam'+beam_char,data=data,dlim=dlim
      ;data.y=median(data.y,2)
      ;store_data,'pfisr_beam_vlos_alt_'+mode+'_med_beam'+beam_char,data=data,dlim=dlim

  if quantity eq 'density' then begin
      get_data,'pfisr_beam_density_alt_'+mode+'_beam'+beam_char,data=data,dlim=dlim
      value=data.y
      for gate=2L,n_elements(data.y(0,*))-3,1 do begin
        value(*,gate)=median(reform(data.y(*,gate-2:gate+2)),dimension=2)
      endfor
      data.y=value
      store_data,'pfisr_beam_density_alt_'+mode+'_med_beam'+beam_char,data=data,dlim=dlim
  endif
  if quantity eq 'velocity' then begin
      get_data,'pfisr_beam_velocity_alt_'+mode+'_beam'+beam_char,data=data,dlim=dlim
      value=data.y
      for gate=2L,n_elements(data.y(0,*))-3,1 do begin
        value(*,gate)=median(reform(data.y(*,gate-2:gate+2)),dimension=2)
      endfor
      data.y=value
      store_data,'pfisr_beam_velocity_alt_'+mode+'_med_beam'+beam_char,data=data,dlim=dlim
  endif
  if quantity eq 'Ti' then begin
      get_data,'pfisr_beam_Ti_alt_'+mode+'_beam'+beam_char,data=data,dlim=dlim
      value=data.y
      for gate=2L,n_elements(data.y(0,*))-3,1 do begin
        value(*,gate)=median(reform(data.y(*,gate-2:gate+2)),dimension=2)
      endfor
      data.y=value
      store_data,'pfisr_beam_Ti_alt_'+mode+'_med_beam'+beam_char,data=data,dlim=dlim
  endif
  if quantity eq 'Te' then begin
      get_data,'pfisr_beam_Te_alt_'+mode+'_beam'+beam_char,data=data,dlim=dlim
      value=data.y
      for gate=2L,n_elements(data.y(0,*))-3,1 do begin
        value(*,gate)=median(reform(data.y(*,gate-2:gate+2)),dimension=2)
      endfor
      data.y=value
      store_data,'pfisr_beam_Te_alt_'+mode+'_med_beam'+beam_char,data=data,dlim=dlim
  endif
      
  if quantity eq 'density' then begin
      get_data,'pfisr_beam_density_mlat_'+mode+'_beam'+beam_char,data=data,dlim=dlim
      value=data.y
      for gate=2L,n_elements(data.y(0,*))-3,1 do begin
        value(*,gate)=median(reform(data.y(*,gate-2:gate+2)),dimension=2)
      endfor
      data.y=value
      store_data,'pfisr_beam_density_mlat_'+mode+'_med_beam'+beam_char,data=data,dlim=dlim
  endif
  if quantity eq 'velocity' then begin
      get_data,'pfisr_beam_velocity_mlat_'+mode+'_beam'+beam_char,data=data,dlim=dlim
      value=data.y
      for gate=2L,n_elements(data.y(0,*))-3,1 do begin
        value(*,gate)=median(reform(data.y(*,gate-2:gate+2)),dimension=2)
      endfor
      data.y=value
      store_data,'pfisr_beam_velocity_mlat_'+mode+'_med_beam'+beam_char,data=data,dlim=dlim
  endif
  if quantity eq 'Ti' then begin
      get_data,'pfisr_beam_Ti_mlat_'+mode+'_beam'+beam_char,data=data,dlim=dlim
      value=data.y
      for gate=2L,n_elements(data.y(0,*))-3,1 do begin
        value(*,gate)=median(reform(data.y(*,gate-2:gate+2)),dimension=2)
      endfor
      data.y=value
      store_data,'pfisr_beam_Ti_mlat_'+mode+'_med_beam'+beam_char,data=data,dlim=dlim
  endif
  if quantity eq 'Te' then begin
      get_data,'pfisr_beam_Te_mlat_'+mode+'_beam'+beam_char,data=data,dlim=dlim
      value=data.y
      for gate=2L,n_elements(data.y(0,*))-3,1 do begin
        value(*,gate)=median(reform(data.y(*,gate-2:gate+2)),dimension=2)
      endfor
      data.y=value
      store_data,'pfisr_beam_Te_mlat_'+mode+'_med_beam'+beam_char,data=data,dlim=dlim
  endif

  ;stop
    endfor
  endif
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;ac lp combined
;  if mode eq 'ac' then begin
;    for beam=0,n_elements(glat(0,*))-1,1 do begin
;      beam_char=strtrim(string(beam+1),1)
;      if beam+1 le 9 then beam_char='0'+beam_char
;      store_data,'pfisr_beam_density_alt_beam'+beam_char,data='pfisr_beam_density_alt_lp_beam'+beam_char+' '+'pfisr_beam_density_alt_ac_beam'+beam_char
;      store_data,'pfisr_beam_velocity_alt_beam'+beam_char,data='pfisr_beam_vlos_alt_lp_beam'+beam_char+' '+'pfisr_beam_vlos_alt_ac_beam'+beam_char
;      store_data,'pfisr_beam_density_alt_med_beam'+beam_char,data='pfisr_beam_density_alt_lp_med_beam'+beam_char+' '+'pfisr_beam_density_alt_ac_med_beam'+beam_char
;      store_data,'pfisr_beam_velocity_alt_med_beam'+beam_char,data='pfisr_beam_vlos_alt_lp_med_beam'+beam_char+' '+'pfisr_beam_vlos_alt_ac_med_beam'+beam_char
;      store_data,'pfisr_beam_density_mlat_beam'+beam_char,data='pfisr_beam_density_mlat_lp_beam'+beam_char+' '+'pfisr_beam_density_mlat_ac_beam'+beam_char
;      store_data,'pfisr_beam_velocity_mlat_beam'+beam_char,data='pfisr_beam_vlos_mlat_lp_beam'+beam_char+' '+'pfisr_beam_vlos_mlat_ac_beam'+beam_char
;      store_data,'pfisr_beam_density_mlat_med_beam'+beam_char,data='pfisr_beam_density_mlat_lp_med_beam'+beam_char+' '+'pfisr_beam_density_mlat_ac_med_beam'+beam_char
;      store_data,'pfisr_beam_velocity_mlat_med_beam'+beam_char,data='pfisr_beam_vlos_mlat_lp_med_beam'+beam_char+' '+'pfisr_beam_vlos_mlat_ac_med_beam'+beam_char
;    endfor
;  endif
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  loadct,43,file=getenv('BIG_DIR')+'/big/SATELLITE/themis/software/tdas/current/idl/themis/common/thm_ctables/thm_colors.tbl'
  ylim,['pfisr_beam_density_mlat*_beam??','pfisr_beam_vlos_mlat*_beam??'],65,69
  ylim,['pfisr_beam_density_alt*_beam??','pfisr_beam_vlos_alt*_beam??'],100,400
  zlim,['pfisr_beam_velocity_*_beam??'],-500,500
  zlim,['pfisr_beam_density_*_beam??'],1e10,1e12
  zlim,['pfisr_beam_T?_*_beam??'],0,3000
  ;tplot,['pfisr_beam_density_alt*_beam??']

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
pfisr_aacgm,mode=mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;store data for scan

if keyword_set(nocal) then stop

;data array
tplot_names,'pfisr_beam_'+quantity_char+'_alt_'+mode+'_beam*',names=names
nbeams=n_elements(names)
get_data,'pfisr_beam_'+quantity_char+'_alt_'+mode+'_beam01',data=data
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
  get_data,'pfisr_beam_'+quantity_char+'_alt_'+mode+'_beam'+cbeamnum,data=quantity
  get_data,'pfisr_beam_velocity_error_alt_'+mode+'_beam'+cbeamnum,data=vel_error
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
    lat_lon_factor=cos(max(quantity.glat,/nan)*!pi/180.)
    atan_slope=atan((quantity.aacgmglat-max(quantity.glat,/nan))*lat_lon_factor/(quantity.glong-median(quantity.glong)))
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
store_data,'pfisr_scan_'+quantity_char+'_'+mode,data={x:time,y:scan_quantity,fov_loc_center:fov_loc_center,fov_loc_full_rel:fov_loc_full_rel,glat:beam_lat,glon:beam_lon,alt:beam_alt,aacgmmlat:beam_mlat,aacgmmlon:beam_mlon},dlim={ysubtitle:'geog'}
if quantity_char eq 'velocity' and keyword_set(verror) then store_data,'pfisr_scan_velocity_error_'+mode,data={x:time,y:scan_vel_error,fov_loc_center:fov_loc_center,fov_loc_full_rel:fov_loc_full_rel,glat:beam_lat,glon:beam_lon,alt:beam_alt,aacgmmlat:beam_mlat,aacgmmlon:beam_mlon},dlim={ysubtitle:'geog'}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if not keyword_set(nomed) then begin
  ;get_data
for beam=0,nbeams-1,1 do begin
  cbeamnum=strmid(names(beam),strlen(names(beam))-2,2)
  get_data,'pfisr_beam_'+quantity_char+'_alt_'+mode+'_med_beam'+cbeamnum,data=quantity
  get_data,'pfisr_beam_velocity_error_alt_'+mode+'_beam'+cbeamnum,data=vel_error
  for loop=0L,n_elements(time)-1,1 do begin
    scan_quantity(loop,beam,0:n_elements(quantity.v)-1)=quantity.y(loop,*)
    if quantity_char eq 'velocity' and keyword_set(verror) then scan_vel_error(loop,beam,*)=vel_error.y(loop,*)
    beam_lat(beam,0:n_elements(quantity.v)-1)=quantity.aacgmglat
    beam_lon(beam,0:n_elements(quantity.v)-1)=quantity.aacgmglon
    beam_alt(beam,0:n_elements(quantity.v)-1)=quantity.v
    beam_mlat(beam,0:n_elements(quantity.v)-1)=quantity.aacgmmlat
    beam_mlon(beam,0:n_elements(quantity.v)-1)=quantity.aacgmmlon
    fov_loc_center(0,beam,0:n_elements(quantity.v)-1)=quantity.aacgmglat
    fov_loc_center(1,beam,0:n_elements(quantity.v)-1)=quantity.aacgmglon
    lat_lon_factor=cos(max(quantity.glat,/nan)*!pi/180.)
    atan_slope=atan((quantity.aacgmglat-max(quantity.glat,/nan))*lat_lon_factor/(quantity.glong-median(quantity.glong)))
    index=where(beam_lon(beam,0:n_elements(quantity.v)-1)-median(quantity.glong) lt 0,count)
    if(count ge 1) then atan_slope(index)=atan_slope(index)+!pi
    lat_lon_factor=1
    fov_loc_full_rel(0,0,beam,0:n_elements(quantity.v)-1)=-sin(atan_slope)+cos(atan_slope)*lat_lon_factor
    fov_loc_full_rel(0,1,beam,0:n_elements(quantity.v)-1)=-sin(atan_slope)-cos(atan_slope)*lat_lon_factor
    fov_loc_full_rel(0,2,beam,0:n_elements(quantity.v)-1)= sin(atan_slope)-cos(atan_slope)*lat_lon_factor
    fov_loc_full_rel(0,3,beam,0:n_elements(quantity.v)-1)= sin(atan_slope)+cos(atan_slope)*lat_lon_factor
    fov_loc_full_rel(1,0,beam,0:n_elements(quantity.v)-1)=-cos(atan_slope)-sin(atan_slope)*lat_lon_factor
    fov_loc_full_rel(1,1,beam,0:n_elements(quantity.v)-1)=-cos(atan_slope)+sin(atan_slope)*lat_lon_factor
    fov_loc_full_rel(1,2,beam,0:n_elements(quantity.v)-1)= cos(atan_slope)+sin(atan_slope)*lat_lon_factor
    fov_loc_full_rel(1,3,beam,0:n_elements(quantity.v)-1)= cos(atan_slope)-sin(atan_slope)*lat_lon_factor
  endfor
endfor

;store data
store_data,'pfisr_scan_'+quantity_char+'_'+mode+'_med',data={x:time,y:scan_quantity,fov_loc_center:fov_loc_center,fov_loc_full_rel:fov_loc_full_rel,glat:beam_lat,glon:beam_lon,alt:beam_alt,aacgmmlat:beam_mlat,aacgmmlon:beam_mlon},dlim={ysubtitle:'geog'}
if quantity_char eq 'velocity' and keyword_set(verror) then store_data,'pfisr_scan_velocity_error_'+mode+'_med',data={x:time,y:scan_vel_error,fov_loc_center:fov_loc_center,fov_loc_full_rel:fov_loc_full_rel,glat:beam_lat,glon:beam_lon,alt:beam_alt,aacgmmlat:beam_mlat,aacgmmlon:beam_mlon},dlim={ysubtitle:'geog'}
endif

thm_median,'pfisr_beam_'+quantity_char+'_alt_'+mode+'_med_beam??',84.*3
del_data,'pfisr_beam_'+quantity_char+'_alt_'+mode+'_med_beam??_md'

stopplot:
end
