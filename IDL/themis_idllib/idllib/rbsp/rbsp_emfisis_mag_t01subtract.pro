
pro rbsp_emfisis_mag_t01subtract,probe=probe,dynamic=dynamic,Csec=Csec,Ccoord=Ccoord,load=load

get_timespan,t

if not keyword_set(Csec) then Csec='1sec'
if not keyword_set(Ccoord) then Ccoord='sm'

;load mag
if keyword_set(load) then begin
cdf_leap_second_init
	source = file_retrieve(/struct)
    if not keyword_set(nocdaweb) then begin
    	source.local_data_dir = getenv('BIG_DIR')+'/big/SATELLITE/rbsp/'
    	source.remote_data_dir = 'http://'
    	datfileformat = 'cdaweb.gsfc.nasa.gov/pub/data/rbsp/rbsp'+probe+'/l3/emfisis/magnetometer/1sec/sm/YYYY/rbsp-'+probe+'_magnetometer_1sec-sm_emfisis-l3_YYYYMMDD_v*.cdf'
    endif else if keyword_set(nocdaweb) then begin
    	source.local_data_dir = getenv('BIG_DIR')+'/big/SATELLITE/rbsp/'
    	source.remote_data_dir = 'http://aurora.atmos.ucla.edu/'
    	datfileformat = 'emfisis.physics.uiowa.edu/Flight/RBSP-'+strupcase(probe)+'/L3/YYYY/MM/DD/rbsp-'+probe+'_magnetometer_1sec-sm_emfisis-L3_YYYYMMDD_v*.cdf'
    endif
	relfnames = file_dailynames(file_format=datfileformat,trange=t,/unique,times=times)
	PRINT,'relfnames= ',relfnames
	source.min_age_limit = 900 ; allow 15 mins between updates
	source.use_wget=1;1
	source.nowait=0
	;file1 = file_retrieve(relfnames,_extra=source)
	file1=spd_download(remote_path='https://',remote_file=relfnames,/last_ver,local_path=getenv('BIG_DIR')+'/big/SATELLITE/rbsp/')
file_found=1
if n_elements(file1) eq 1 then if(file1 eq '') then file_found=0
file2=file1(n_elements(file1)-1)
if file_found eq 0 then print,'******** No '+file1
if file_found eq 0 then return

cdf2tplot,file=file2,prefix='rbsp'+probe+'_emfisis_'
endif

get_data,'rbsp'+probe+'_emfisis_Mag',data=data
store_data,'rbsp'+probe+'_emfisis_mag_sm',data=data,dlim={colors:[2,4,6],labels:['Bx','By','Bz'],ysubtitle:'[nT]',labflag:-1,constant:0}


copy_data,'rbsp'+probe+'_emfisis_coordinates','rbsp'+probe+'_emfisis_coordinates_sm'
cotrans,'rbsp'+probe+'_emfisis_coordinates_sm','rbsp'+probe+'_emfisis_coordinates_gsm',/SM2GSM,/ignore
cotrans,'rbsp'+probe+'_emfisis_coordinates_gsm','rbsp'+probe+'_emfisis_coordinates_gse',/GSM2GSE,/ignore
cotrans,'rbsp'+probe+'_emfisis_coordinates_gse','rbsp'+probe+'_emfisis_coordinates_gei',/GSE2GEI,/ignore
cotrans,'rbsp'+probe+'_emfisis_coordinates_gei','rbsp'+probe+'_emfisis_coordinates_geo',/GEI2GEO,/ignore
get_data,'rbsp'+probe+'_emfisis_coordinates_gsm',data=data
data_att={coord_sys:'gsm'}
dlim={data_att:data_att,ysubtitle:'km'}
if n_elements(data.x) ge n_elements(data.y[*,0]) then str_element,data,'x',data.x[0:n_elements(data.y[*,0])-1],/add
store_data,'rbsp'+probe+'_XYZ_GSM_km',data=data,dlim=dlim

;T01--constinput
tt01, 'rbsp'+probe+'_XYZ_GSM_km',pdyn=1.0D,dsti=0.0D,yimf=0.0D,zimf=1.0D,g1=1.0D,g2=1.0D
copy_data,'rbsp'+probe+'_XYZ_GSM_km_bt01','rbsp'+probe+'_XYZ_GSM_bt01_constsw'
get_data,'rbsp'+probe+'_XYZ_GSM_bt01_constsw',data=T01data
store_data,'rbsp'+probe+'_XYZ_GSM_bt01_constsw',data={x:T01data.x, y:T01data.y},dlim={colors:[2,4,6],labels:['Bx','By','Bz'],ysubtitle:'[nT]',labflag:1,constant:0}

;T01--dynamic
if keyword_set(dynamic) then begin
  model='t01'
  omni_load,median=1,/noplot,/init
  tdegap,'Psw',/overwrite
  tdeflag,'Psw','linear',/overwrite
  tdegap,'SYM_H',/overwrite
  tdeflag,'SYM_H','linear',/overwrite
  tdegap,'BY_GSM',/overwrite
  tdeflag,'BY_GSM','linear',/overwrite
  tdegap,'BZ_GSM',/overwrite
  tdeflag,'BZ_GSM','linear',/overwrite
  tdegap,'proton_density',/overwrite
  tdeflag,'proton_density','linear',/overwrite
  tdegap,'flow_speed',/overwrite
  tdeflag,'flow_speed','linear',/overwrite
  store_data,'omni_imf',data=['BY_GSM','BZ_GSM']
  if(model eq 't96' or model eq 't01' or model eq 't04s') then begin
    get_tsy_params,'SYM_H','omni_imf','proton_density','flow_speed',model,/speed,/imf_yz,trange=[t[0]-3900,t[1]]
  endif
  par = model + '_par'
  tt01, 'rbsp'+probe+'_XYZ_GSM_km',parmod=par
  get_data,'rbsp'+probe+'_XYZ_GSM_km_bt01',data=T01data
  store_data,'rbsp'+probe+'_XYZ_GSM_bt01',data={x:T01data.x, y:T01data.y},dlim={colors:[2,4,6],labels:['Bx','By','Bz'],ysubtitle:'[nT]',labflag:1,constant:0}
endif

;interpolation
tinterpol_mxn, strjoin('rbsp'+probe+'_XYZ_GSM_bt01_constsw'), strjoin('rbsp'+probe+'_emfisis_mag_sm'), newname=strjoin('rbsp'+probe+'_XYZ_GSM_bt01_constsw_int'),/spline
if keyword_set(dynamic) then tinterpol_mxn, strjoin('rbsp'+probe+'_XYZ_GSM_bt01'), strjoin('rbsp'+probe+'_emfisis_mag_sm'), newname=strjoin('rbsp'+probe+'_XYZ_GSM_bt01_int'),/spline
cotrans,'rbsp'+probe+'_XYZ_GSM_bt01_constsw_int','rbsp'+probe+'_XYZ_SM_bt01_constsw_int',/GSM2SM,/ignore_dlimit
if keyword_set(dynamic) then cotrans,'rbsp'+probe+'_XYZ_GSM_bt01_int','rbsp'+probe+'_XYZ_SM_bt01_int',/GSM2SM,/ignore_dlimit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;sm
;subtract
get_data,'rbsp'+probe+'_XYZ_SM_bt01_constsw_int',data=T01data
get_data,'rbsp'+probe+'_emfisis_mag_sm',data=FGMdata
dFGM=FGMdata.y-T01data.y
store_data,'rbsp'+probe+'_emfisis_mag_sm-t01const',data={x:FGMdata.x, y:dFGM},dlim={colors:[2,4,6],labels:['dBx','dBy','dBz'],ysubtitle:'[nT]',labflag:-1,constant:0}

if keyword_set(dynamic) then begin
get_data,'rbsp'+probe+'_XYZ_SM_bt01_int',data=T01data
get_data,'rbsp'+probe+'_emfisis_mag_sm',data=FGMdata
dFGM=FGMdata.y-T01data.y
store_data,'rbsp'+probe+'_emfisis_mag_sm-t01',data={x:FGMdata.x, y:dFGM},dlim={colors:[2,4,6],labels:['dBx','dBy','dBz'],ysubtitle:'[nT]',labflag:-1,constant:0}
endif

inclination_mag=atan(FGMdata.y(*,2)/sqrt(FGMdata.y(*,0)^2+FGMdata.y(*,1)^2))*180/!pi
inclination_t01=atan(T01data.y(*,2)/sqrt(T01data.y(*,0)^2+T01data.y(*,1)^2))*180/!pi
store_data,'rbsp'+probe+'_emfisis_mag_sm_inclination',data={x:FGMdata.x, y:inclination_mag},dlim={colors:[0],labels:[''],ysubtitle:'[deg]',labflag:-1,constant:0}
store_data,'rbsp'+probe+'_XYZ_SM_bt01_int_inclination',data={x:FGMdata.x, y:inclination_t01},dlim={colors:[0],labels:[''],ysubtitle:'[deg]',labflag:-1,constant:0}
store_data,'rbsp'+probe+'_emfisis_mag_sm-t01_inclination',data={x:FGMdata.x, y:inclination_mag-inclination_t01},dlim={colors:[0],labels:[''],ysubtitle:'[deg]',labflag:-1,constant:0}

end
