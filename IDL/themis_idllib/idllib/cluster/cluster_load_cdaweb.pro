;.run caa_cef_read.pro cluster_load_caa4.pro
;cluster_load_caa4,sc='3'
;all from CAA

pro cluster_load_cdaweb,sc=sc,noplot=noplot

;del_data, '*'

;set time
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

sc_up=strupcase(sc)

cdf_leap_second_init

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;aux
	source = file_retrieve(/struct)
	source.local_data_dir = getenv('BIG_DIR')+'/big/SATELLITE/'
	source.remote_data_dir = 'https://'
	datfileformat = 'cdaweb.gsfc.nasa.gov/pub/data/cluster/cl/sp/aux/YYYY/cl_sp_aux_YYYYMMDD_v*.cdf'
	relfnames = file_dailynames(file_format=datfileformat,trange=t,/unique,times=times)
	PRINT,'relfnames= ',relfnames
	source.min_age_limit = 900 ; allow 15 mins between updates
	source.use_wget=1;1
	source.nowait=0
	datfiles=source.local_data_dir+relfnames
	relfnames = file_dailynames(file_format=datfileformat,trange=t,/unique,times=times)
;	file1 = file_retrieve(relfnames,_extra=source)
;	file1=file_search(datfiles)
    file1 = spd_download(remote_file=relfnames, remote_path=source.remote_data_dir, local_path = source.local_data_dir, ssl_verify_peer=0, ssl_verify_host=0)
file_found=1
if n_elements(file1) eq 1 then if(file1 eq '') then file_found=0
file2=file1(n_elements(file1)-1)
if file_found eq 0 then print,'******** No '+file1
if file_found eq 1 then cdf2tplot,file=file2,prefix='c'+sc+'_aux_'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;fgm
	source = file_retrieve(/struct)
	source.min_age_limit = 900 ; allow 15 mins between updates
	source.local_data_dir = getenv('BIG_DIR')+'/big/SATELLITE/'
	source.remote_data_dir = 'http://'
	datfileformat = 'cdaweb.gsfc.nasa.gov/pub/data/cluster/c'+sc+'/cp/YYYY/c'+sc+'_cp_fgm_spin_YYYYMMDD_v*.cdf'
	relfnames = file_dailynames(file_format=datfileformat,trange=t,/unique,times=times)
	PRINT,'relfnames= ',relfnames
	source.use_wget=1;1
	source.nowait=0
	datfiles=source.local_data_dir+relfnames
;	file1 = file_retrieve(relfnames,_extra=source)
;	file1=file_search(datfiles)
    file1 = spd_download(remote_file=relfnames, remote_path=source.remote_data_dir, local_path = source.local_data_dir, ssl_verify_peer=0, ssl_verify_host=0)
file_found=1
if n_elements(file1) eq 1 then if(file1 eq '') then file_found=0
file2=file1(n_elements(file1)-1)
if file_found eq 0 then print,'******** No '+file1
if file_found eq 1 then add_att_to_fast_cdf,file2
if file_found eq 1 then cdf2tplot,file=file2,prefix='c'+sc+'_fgm_'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;asp
	source = file_retrieve(/struct)
	source.min_age_limit = 900 ; allow 15 mins between updates
	source.local_data_dir = getenv('BIG_DIR')+'/big/SATELLITE/'
	source.remote_data_dir = 'http://'
	datfileformat = 'cdaweb.gsfc.nasa.gov/pub/data/cluster/c'+sc+'/pp/asp/YYYY/c'+sc+'_pp_asp_YYYYMMDD_v*.cdf'
	relfnames = file_dailynames(file_format=datfileformat,trange=t,/unique,times=times)
	PRINT,'relfnames= ',relfnames
	source.use_wget=1;1
	source.nowait=0
	datfiles=source.local_data_dir+relfnames
;	file1 = file_retrieve(relfnames,_extra=source)
;	file1=file_search(datfiles)
    file1 = spd_download(remote_file=relfnames, remote_path=source.remote_data_dir, local_path = source.local_data_dir, ssl_verify_peer=0, ssl_verify_host=0)
file_found=1
if n_elements(file1) eq 1 then if(file1 eq '') then file_found=0
file2=file1(n_elements(file1)-1)
if file_found eq 0 then print,'******** No '+file1
if file_found eq 1 then cdf2tplot,file=file2,prefix='c'+sc+'_asp_'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;cis
	source = file_retrieve(/struct)
	source.min_age_limit = 900 ; allow 15 mins between updates
	source.local_data_dir = getenv('BIG_DIR')+'/big/SATELLITE/'
	source.remote_data_dir = 'http://'
	datfileformat = 'cdaweb.gsfc.nasa.gov/pub/data/cluster/c'+sc+'/pp/cis/YYYY/c'+sc+'_pp_cis_YYYYMMDD_v*.cdf'
	relfnames = file_dailynames(file_format=datfileformat,trange=t,/unique,times=times)
	PRINT,'relfnames= ',relfnames
	source.use_wget=1;1
	source.nowait=0
	datfiles=source.local_data_dir+relfnames
;	file1 = file_retrieve(relfnames,_extra=source)
;	file1=file_search(datfiles)
    file1 = spd_download(remote_file=relfnames, remote_path=source.remote_data_dir, local_path = source.local_data_dir, ssl_verify_peer=0, ssl_verify_host=0)
file_found=1
if n_elements(file1) eq 1 then if(file1 eq '') then file_found=0
file2=file1(n_elements(file1)-1)
if file_found eq 0 then print,'******** No '+file1
if file_found eq 1 then cdf2tplot,file=file2,prefix='c'+sc+'_cis_'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;edi
	source = file_retrieve(/struct)
	source.min_age_limit = 900 ; allow 15 mins between updates
	source.local_data_dir = getenv('BIG_DIR')+'/big/SATELLITE/'
	source.remote_data_dir = 'http://'
	datfileformat = 'cdaweb.gsfc.nasa.gov/pub/data/cluster/c'+sc+'/pp/edi/YYYY/c'+sc+'_pp_edi_YYYYMMDD_v*.cdf'
	relfnames = file_dailynames(file_format=datfileformat,trange=t,/unique,times=times)
	PRINT,'relfnames= ',relfnames
	source.use_wget=1;1
	source.nowait=0
	datfiles=source.local_data_dir+relfnames
;	file1 = file_retrieve(relfnames,_extra=source)
;	file1=file_search(datfiles)
    file1 = spd_download(remote_file=relfnames, remote_path=source.remote_data_dir, local_path = source.local_data_dir, ssl_verify_peer=0, ssl_verify_host=0)
file_found=1
if n_elements(file1) eq 1 then if(file1 eq '') then file_found=0
file2=file1(n_elements(file1)-1)
if file_found eq 0 then print,'******** No '+file1
if file_found eq 1 then cdf2tplot,file=file2,prefix='c'+sc+'_edi_'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;efw
	source = file_retrieve(/struct)
	source.min_age_limit = 900 ; allow 15 mins between updates
	source.local_data_dir = getenv('BIG_DIR')+'/big/SATELLITE/'
	source.remote_data_dir = 'http://'
	datfileformat = 'cdaweb.gsfc.nasa.gov/pub/data/cluster/c'+sc+'/pp/efw/YYYY/c'+sc+'_pp_efw_YYYYMMDD_v*.cdf'
	relfnames = file_dailynames(file_format=datfileformat,trange=t,/unique,times=times)
	PRINT,'relfnames= ',relfnames
	source.use_wget=1;1
	source.nowait=0
	datfiles=source.local_data_dir+relfnames
;	file1 = file_retrieve(relfnames,_extra=source)
;	file1=file_search(datfiles)
    file1 = spd_download(remote_file=relfnames, remote_path=source.remote_data_dir, local_path = source.local_data_dir, ssl_verify_peer=0, ssl_verify_host=0)
file_found=1
if n_elements(file1) eq 1 then if(file1 eq '') then file_found=0
file2=file1(n_elements(file1)-1)
if file_found eq 0 then print,'******** No '+file1
if file_found eq 1 then cdf2tplot,file=file2,prefix='c'+sc+'_efw_'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;pea
	source = file_retrieve(/struct)
	source.min_age_limit = 900 ; allow 15 mins between updates
	source.local_data_dir = getenv('BIG_DIR')+'/big/SATELLITE/'
	source.remote_data_dir = 'http://'
	datfileformat = 'cdaweb.gsfc.nasa.gov/pub/data/cluster/c'+sc+'/pp/pea/YYYY/c'+sc+'_pp_pea_YYYYMMDD_v*.cdf'
	relfnames = file_dailynames(file_format=datfileformat,trange=t,/unique,times=times)
	PRINT,'relfnames= ',relfnames
	source.use_wget=1;1
	source.nowait=0
	datfiles=source.local_data_dir+relfnames
;	file1 = file_retrieve(relfnames,_extra=source)
;	file1=file_search(datfiles)
    file1 = spd_download(remote_file=relfnames, remote_path=source.remote_data_dir, local_path = source.local_data_dir, ssl_verify_peer=0, ssl_verify_host=0)
file_found=1
if n_elements(file1) eq 1 then if(file1 eq '') then file_found=0
file2=file1(n_elements(file1)-1)
if file_found eq 0 then print,'******** No '+file1
if file_found eq 1 then cdf2tplot,file=file2,prefix='c'+sc+'_pea_'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;AUX
get_data,'c'+sc+'_aux_sc_r_xyz_gse__CL_SP_AUX',data=Rdata,index=index,dlim=dlim,lim=lim
get_data,'c'+sc+'_aux_sc_dr'+sc+'_xyz_gse__CL_SP_AUX',data=dR,index=index
dR.y+=Rdata.y
store_data,'c'+sc+'_aux_sc_r_xyz_gse__C'+sc+'_CP_AUX_POSGSE_1M',data=dR,dlim=dlim,lim=lim

get_data,'c'+sc+'_aux_sc_r_xyz_gse__C'+sc+'_CP_AUX_POSGSE_1M',data=Rdata,index=index
if(index ne 0) then begin
store_data,'c'+sc+'_aux_sc_r_xyz_gse__C'+sc+'_SP_AUX',data=Rdata,dlim={colors:[2,4,6],labels:['X','Y','Z'],ysubtitle:'[km]',labflag:-1}

;GLAT,GLON
cotrans,'c'+sc+'_aux_sc_r_xyz_gse__C'+sc+'_CP_AUX_POSGSE_1M','c'+sc+'_aux_sc_r_xyz_gei__C'+sc+'_CP_AUX_POSGSE_1M',/GSE2GEI
cotrans,'c'+sc+'_aux_sc_r_xyz_gei__C'+sc+'_CP_AUX_POSGSE_1M','c'+sc+'_aux_sc_r_xyz_geo__C'+sc+'_CP_AUX_POSGSE_1M',/GEI2GEO
get_data,'c'+sc+'_aux_sc_r_xyz_geo__C'+sc+'_CP_AUX_POSGSE_1M',data=data
J=-1
sphcar_vector,R,THETA,PHI,data.y(*,0),data.y(*,1),data.y(*,2),J
GLAT=90-THETA*180/!pi
GLON=PHI*180/!pi
store_data,'C'+sc+'_GLAT',data={x:data.x,y:GLAT}
store_data,'C'+sc+'_GLON',data={x:data.x,y:GLON}

;GSM,SM
cotrans,'c'+sc+'_aux_sc_r_xyz_gse__C'+sc+'_SP_AUX','c'+sc+'_aux_sc_r_xyz_gsm__C'+sc+'_SP_AUX',/GSE2GSM
cotrans,'c'+sc+'_aux_sc_r_xyz_gsm__C'+sc+'_SP_AUX','c'+sc+'_aux_sc_r_xyz_sm__C'+sc+'_SP_AUX',/GSM2SM
get_data,'c'+sc+'_aux_sc_r_xyz_sm__C'+sc+'_SP_AUX',data=tmp

;R
store_data, 'C'+sc+'_R', data={x:tmp.x, y:sqrt(tmp.y[*,0]^2+tmp.y[*,1]^2+tmp.y[*,2]^2)/6370.},dlim={colors:[0],labels:['R'],ysubtitle:'[RE]',labflag:-1,constant:0,ytitle:'C'+sc+' R'}

;MLT
MLT=atan(tmp.y[*,1]/tmp.y[*,0])*180/!pi/15.+12
if(n_elements(where(tmp.y[*,0] lt 0)) ne 1) then MLT(where(tmp.y[*,0] lt 0))=(atan(tmp.y[where(tmp.y[*,0] lt 0),1]/tmp.y[where(tmp.y[*,0] lt 0),0])+3.14159)*180/!pi/15.+12
if(n_elements(where(MLT[*] ge 24)) ne 1) then MLT(where(MLT[*] ge 24))=MLT(where(MLT[*] ge 24))-24
store_data, 'C'+sc+'_MLT', data={x:tmp.x, y:MLT},dlim={colors:[0],labels:['MLT'],ysubtitle:'[h]',labflag:-1,constant:0,ytitle:'C'+sc+' MLT'}
;MLAT
MLAT=atan(tmp.y[*,2]/sqrt(tmp.y[*,0]^2+tmp.y[*,1]^2))*180/!pi
store_data, 'C'+sc+'_MLAT', data={x:tmp.x, y:MLAT},dlim={colors:[0],labels:['MLAT'],ysubtitle:'[deg]',labflag:-1,constant:0,ytitle:'C'+sc+' MLAT'}
endif
;L
get_data,'C'+sc+'_R', data=R
get_data,'C'+sc+'_MLAT', data=MLAT
Lshell=R.y/(cos(MLAT.y*!pi/180)^2)
ILAT=acos(sqrt(1/Lshell))*180/!pi
where_temp=where(MLAT.y lt 0,count)
if(count ge 1) then ILAT(where_temp)=-ILAT(where_temp)
store_data, 'C'+sc+'_L', data={x:tmp.x, y:Lshell},dlim={colors:[0],labels:['L'],ysubtitle:'[]',labflag:-1,constant:0,ytitle:'C'+sc+' L'}
store_data, 'C'+sc+'_ILAT', data={x:tmp.x, y:ILAT},dlim={colors:[0],labels:['ILAT'],ysubtitle:'[]',labflag:-1,constant:0,ytitle:'C'+sc+' ILAT'}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;FGM
get_data,'c'+sc+'_fgm_B_vec_xyz_gse__C'+sc+'_CP_FGM_SPIN',data=data,index=index
if index eq 0 then begin
  print,'NO FGM DATA'
  return
endif

if(index ne 0) then begin
store_data,'c'+sc+'_fgm_B_xyz_gse__C'+sc+'_PP_FGM',data=data

;cotrans
cotrans,'c'+sc+'_fgm_B_xyz_gse__C'+sc+'_PP_FGM','c'+sc+'_fgm_B_xyz_gsm__C'+sc+'_PP_FGM',/GSE2GSM
cotrans,'c'+sc+'_fgm_B_xyz_gsm__C'+sc+'_PP_FGM','c'+sc+'_fgm_B_xyz_sm__C'+sc+'_PP_FGM',/GSM2SM


;T01
tt01,'c'+sc+'_aux_sc_r_xyz_gsm__C'+sc+'_SP_AUX',pdyn=1.0D,dsti=0.0D,yimf=0.0D,zimf=1.0D,g1=1.0D,g2=1.0D
;interpolation
tinterpol_mxn, 'c'+sc+'_aux_sc_r_xyz_gsm__C'+sc+'_SP_AUX_bt01', 'c'+sc+'_fgm_B_xyz_gsm__C'+sc+'_PP_FGM', newname='c'+sc+'_aux_sc_r_xyz_gsm__C'+sc+'_SP_AUX_bt01_int';,/spline
tinterpol_mxn, 'c'+sc+'_aux_sc_r_xyz_gsm__C'+sc+'_SP_AUX', 'c'+sc+'_fgm_B_xyz_gsm__C'+sc+'_PP_FGM', newname='c'+sc+'_aux_sc_r_xyz_gsm__C'+sc+'_SP_AUX_int';,/spline
cotrans,'c'+sc+'_aux_sc_r_xyz_gsm__C'+sc+'_SP_AUX_bt01_int','c'+sc+'_aux_sc_r_xyz_sm__C'+sc+'_SP_AUX_bt01_int',/GSM2SM
tinterpol_mxn, 'c'+sc+'_aux_sc_r_xyz_sm__C'+sc+'_SP_AUX', 'c'+sc+'_fgm_B_xyz_sm__C'+sc+'_PP_FGM', newname='c'+sc+'_aux_sc_r_xyz_sm__C'+sc+'_SP_AUX_int'
cotrans,'c'+sc+'_aux_sc_r_xyz_gsm__C'+sc+'_SP_AUX_bt01_int','c'+sc+'_aux_sc_r_xyz_gse__C'+sc+'_SP_AUX_bt01_int',/GSM2GSE
tinterpol_mxn, 'c'+sc+'_aux_sc_r_xyz_gse__C'+sc+'_SP_AUX', 'c'+sc+'_fgm_B_xyz_gse__C'+sc+'_PP_FGM', newname='c'+sc+'_aux_sc_r_xyz_gse__C'+sc+'_SP_AUX_int'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Bgsm
;subtract
get_data,'c'+sc+'_aux_sc_r_xyz_gsm__C'+sc+'_SP_AUX_bt01_int',data=T01data
get_data,'c'+sc+'_fgm_B_xyz_gsm__C'+sc+'_PP_FGM',data=Bdata

;Btot
Btotdata=sqrt(Bdata.y(*,0)^2+Bdata.y(*,1)^2+Bdata.y(*,2)^2)
T01totdata=sqrt(T01data.y(*,0)^2+T01data.y(*,1)^2+T01data.y(*,2)^2)

;remove error
dB=Bdata.y-T01data.y
dBtot=Btotdata-T01totdata
if(n_elements(where(sqrt(dB(*)^2) gt 500)) ne 1) then dB(where(sqrt(dB(*)^2) gt 500))='NaN'
if(n_elements(where(sqrt(dBtot(*)^2) gt 500)) ne 1) then dBtot(where(sqrt(dBtot(*)^2) gt 500))='NaN'

;XYZ-VDH (GSM)
get_data,'c'+sc+'_aux_sc_r_xyz_gsm__C'+sc+'_SP_AUX_int',data=tmp
MLTm12=atan(tmp.y[*,1]/tmp.y[*,0])
if(n_elements(where(tmp.y[*,0] lt 0)) ne 1) then MLTm12(where(tmp.y[*,0] lt 0))=(atan(tmp.y[where(tmp.y[*,0] lt 0),1]/tmp.y[where(tmp.y[*,0] lt 0),0])+!pi)
B_VDH=Bdata.y
B_VDH(*,0)=-(Bdata.y(*,0)*cos(MLTm12(*))+Bdata.y(*,1)*sin(MLTm12(*)))
B_VDH(*,1)= -Bdata.y(*,0)*sin(MLTm12(*))+Bdata.y(*,1)*cos(MLTm12(*))
dB_VDH=dB
dB_VDH(*,0)=-(dB(*,0)*cos(MLTm12(*))+dB(*,1)*sin(MLTm12(*)))
dB_VDH(*,1)= -dB(*,0)*sin(MLTm12(*))+dB(*,1)*cos(MLTm12(*))

;store data
store_data,'CL'+sc+'_Bgsm',data={x:Bdata.x, y:[[Bdata.y(*,0)],[Bdata.y(*,1)],[Bdata.y(*,2)],[Btotdata(*)]]},dlim={colors:[2,4,6,0],labels:['Bx','By','Bz','|B|'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'CL'+sc+'_Bgsm-T01',data={x:Bdata.x, y:dB},dlim={colors:[2,4,6],labels:['dBx','dBy','dBz'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'CL'+sc+'_Bgsm_pol',data={x:Bdata.x, y:[[B_VDH(*,0)],[B_VDH(*,1)],[B_VDH(*,2)],[Btotdata(*)]]},dlim={colors:[2,4,6,0],labels:['B-r','Be','Bz','|B|'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'CL'+sc+'_Bgsm-T01_pol',data={x:Bdata.x, y:[[dB_VDH(*,0)],[dB_VDH(*,1)],[dB_VDH(*,2)],[dBtot(*)]]},dlim={colors:[2,4,6,0],labels:['dB-r','dBe','dBz','d|B|'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'CL'+sc+'_Bgsm-T01_pol_3comp',data={x:Bdata.x, y:[[-dB_VDH(*,0)],[dB_VDH(*,1)],[dB_VDH(*,2)]]},dlim={colors:[2,4,6],labels:['dBr','dBe','dBz'],ysubtitle:'[nT]',labflag:-1,constant:0}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Bsm
;subtract
get_data,'c'+sc+'_aux_sc_r_xyz_sm__C'+sc+'_SP_AUX_bt01_int',data=T01data
get_data,'c'+sc+'_fgm_B_xyz_sm__C'+sc+'_PP_FGM',data=Bdata

;Btot
Btotdata=sqrt(Bdata.y(*,0)^2+Bdata.y(*,1)^2+Bdata.y(*,2)^2)
T01totdata=sqrt(T01data.y(*,0)^2+T01data.y(*,1)^2+T01data.y(*,2)^2)

;remove error
dB=Bdata.y-T01data.y
dBtot=Btotdata-T01totdata
if(n_elements(where(sqrt(dB(*)^2) gt 500)) ne 1) then dB(where(sqrt(dB(*)^2) gt 500))='NaN'
if(n_elements(where(sqrt(dBtot(*)^2) gt 500)) ne 1) then dBtot(where(sqrt(dBtot(*)^2) gt 500))='NaN'

;XYZ-VDH (sm)
get_data,'c'+sc+'_aux_sc_r_xyz_sm__C'+sc+'_SP_AUX_int',data=tmp
MLTm12=atan(tmp.y[*,1]/tmp.y[*,0])
if(n_elements(where(tmp.y[*,0] lt 0)) ne 1) then MLTm12(where(tmp.y[*,0] lt 0))=(atan(tmp.y[where(tmp.y[*,0] lt 0),1]/tmp.y[where(tmp.y[*,0] lt 0),0])+!pi)
B_VDH=Bdata.y
B_VDH(*,0)=-(Bdata.y(*,0)*cos(MLTm12(*))+Bdata.y(*,1)*sin(MLTm12(*)))
B_VDH(*,1)= -Bdata.y(*,0)*sin(MLTm12(*))+Bdata.y(*,1)*cos(MLTm12(*))
dB_VDH=dB
dB_VDH(*,0)=-(dB(*,0)*cos(MLTm12(*))+dB(*,1)*sin(MLTm12(*)))
dB_VDH(*,1)= -dB(*,0)*sin(MLTm12(*))+dB(*,1)*cos(MLTm12(*))

;store data
store_data,'CL'+sc+'_Bsm',data={x:Bdata.x, y:[[Bdata.y(*,0)],[Bdata.y(*,1)],[Bdata.y(*,2)],[Btotdata(*)]]},dlim={colors:[2,4,6,0],labels:['Bx','By','Bz','|B|'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'CL'+sc+'_Bsm-T01',data={x:Bdata.x, y:dB},dlim={colors:[2,4,6],labels:['dBx','dBy','dBz'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'CL'+sc+'_Bsm_pol',data={x:Bdata.x, y:[[B_VDH(*,0)],[B_VDH(*,1)],[B_VDH(*,2)],[Btotdata(*)]]},dlim={colors:[2,4,6,0],labels:['B-r','Be','Bz','|B|'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'CL'+sc+'_Bsm-T01_pol',data={x:Bdata.x, y:[[dB_VDH(*,0)],[dB_VDH(*,1)],[dB_VDH(*,2)],[dBtot(*)]]},dlim={colors:[2,4,6,0],labels:['dB-r','dBe','dBz','d|B|'],ysubtitle:'[nT]',labflag:-1,constant:0}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Bgse
get_data,'c'+sc+'_aux_sc_r_xyz_gse__C'+sc+'_SP_AUX_bt01_int',data=T01data
get_data,'c'+sc+'_fgm_B_xyz_gse__C'+sc+'_PP_FGM',data=Bdata

;remove error
dB=Bdata.y-T01data.y
dBtot=Btotdata-T01totdata
if(n_elements(where(sqrt(dB(*)^2) gt 500)) ne 1) then dB(where(sqrt(dB(*)^2) gt 500))='NaN'
if(n_elements(where(sqrt(dBtot(*)^2) gt 500)) ne 1) then dBtot(where(sqrt(dBtot(*)^2) gt 500))='NaN'

;XYZ-VDH (gse)
get_data,'c'+sc+'_aux_sc_r_xyz_gse__C'+sc+'_SP_AUX_int',data=tmp
MLTm12=atan(tmp.y[*,1]/tmp.y[*,0])
if(n_elements(where(tmp.y[*,0] lt 0)) ne 1) then MLTm12(where(tmp.y[*,0] lt 0))=(atan(tmp.y[where(tmp.y[*,0] lt 0),1]/tmp.y[where(tmp.y[*,0] lt 0),0])+!pi)
B_VDH=Bdata.y
B_VDH(*,0)=-(Bdata.y(*,0)*cos(MLTm12(*))+Bdata.y(*,1)*sin(MLTm12(*)))
B_VDH(*,1)= -Bdata.y(*,0)*sin(MLTm12(*))+Bdata.y(*,1)*cos(MLTm12(*))
dB_VDH=dB
dB_VDH(*,0)=-(dB(*,0)*cos(MLTm12(*))+dB(*,1)*sin(MLTm12(*)))
dB_VDH(*,1)= -dB(*,0)*sin(MLTm12(*))+dB(*,1)*cos(MLTm12(*))

store_data,'CL'+sc+'_Bgse',data={x:Bdata.x, y:[[Bdata.y(*,0)],[Bdata.y(*,1)],[Bdata.y(*,2)],[Btotdata(*)]]},dlim={colors:[2,4,6,0],labels:['Bx','By','Bz','|B|'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'CL'+sc+'_Bgse-T01',data={x:Bdata.x, y:dB},dlim={colors:[2,4,6],labels:['dBx','dBy','dBz'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'CL'+sc+'_Bgse_pol',data={x:Bdata.x, y:[[B_VDH(*,0)],[B_VDH(*,1)],[B_VDH(*,2)],[Btotdata(*)]]},dlim={colors:[2,4,6,0],labels:['B-r','Be','Bz','|B|'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'CL'+sc+'_Bgse-T01_pol',data={x:Bdata.x, y:[[dB_VDH(*,0)],[dB_VDH(*,1)],[dB_VDH(*,2)],[dBtot(*)]]},dlim={colors:[2,4,6,0],labels:['dB-r','dBe','dBz','d|B|'],ysubtitle:'[nT]',labflag:-1,constant:0}

endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;EDI

;Exyz
get_data,'c'+sc+'_edi_E_xyz_gse__C'+sc+'_PP_EDI',data=data,index=index
if(index ne 0) then begin
store_data,'CL'+sc+'_EDIGSE',data={x:data.x, y:data.y},dlim={colors:[2,4,6],labels:['Ex','Ey','Ez'],ysubtitle:'[mV/m]',constant:0,labflag:-1}
ylim,'CL'+sc+'_EDI',-5,5

;EGSE
cotrans,'CL'+sc+'_EDIGSE','CL'+sc+'_EDI',/GSE2GSM
cotrans,'CL'+sc+'_EDI','CL'+sc+'_EDISM',/GSM2SM
endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;EFW

get_data,'c'+sc+'_efw_E_dusk__C'+sc+'_PP_EFW',data=data,index=index
if(index ne 0) then begin
if(n_elements(where(abs(data.y[*]) gt 1000)) ne 1) then data.y[where(abs(data.y[*]) gt 1000)]='NaN'
store_data,'CL'+sc+'_EFW',data={x:data.x, y:data.y},dlim={colors:[0],labels:['Ey'],ysubtitle:'[mV/m]',labflag:-1,constant:0}
ylim,'CL'+sc+'_EFW',-5,5

get_data,'c'+sc+'_efw_U_probe_sc__C'+sc+'_PP_EFW',data=data,index=index
if(index ne 0) then $
store_data,'CL'+sc+'_Vsc',data={x:data.x,y:data.y},dlim={colors:[0],labels:['Vsc'],ysubtitle:'[V]',labflag:-1,constant:0}
ylim,'CL'+sc+'_Vsc',-5,5
endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CIS

get_data,'c'+sc+'_cis_N_HIA__C'+sc+'_PP_CIS',index=index1
get_data,'c'+sc+'_cis_N_p__C'+sc+'_PP_CIS',index=index2
if(index1 ne 0 or index2 ne 0) then begin

;Ni
if(sc ne '4') then begin
get_data,'c'+sc+'_cis_N_HIA__C'+sc+'_PP_CIS',data=Ndata
endif else begin
get_data,'c'+sc+'_cis_N_p__C'+sc+'_PP_CIS',  data=Ndata
endelse
store_data,'CL'+sc+'_Ni',data={x:Ndata.x, y:Ndata.y},dlim={colors:[0],labels:['Ni'],ysubtitle:'[/cm3]',labflag:-1,ylog:1}

;Vi
if(sc ne '4') then begin
cotrans,'c'+sc+'_cis_V_HIA_xyz_gse__C'+sc+'_PP_CIS','c'+sc+'_cis_V_HIA_xyz_gsm__C'+sc+'_PP_CIS',/GSE2GSM
get_data,'c'+sc+'_cis_V_HIA_xyz_gsm__C'+sc+'_PP_CIS',data=data
endif else begin
cotrans,'c'+sc+'_cis_V_p_xyz_gse__C'+sc+'_PP_CIS','c'+sc+'_cis_V_p_xyz_gsm__C'+sc+'_PP_CIS',/GSE2GSM
get_data,'c'+sc+'_cis_V_p_xyz_gsm__C'+sc+'_PP_CIS',data=data
endelse
if(n_elements(where(abs(data.y[*,0]) gt 10000)) ne 1) then data.y[where(abs(data.y[*,0]) gt 10000),0]='NaN'
if(n_elements(where(abs(data.y[*,1]) gt 10000)) ne 1) then data.y[where(abs(data.y[*,1]) gt 10000),1]='NaN'
if(n_elements(where(abs(data.y[*,2]) gt 10000)) ne 1) then data.y[where(abs(data.y[*,2]) gt 10000),2]='NaN'
store_data,'CL'+sc+'_Vigsm',data={x:data.x, y:data.y},dlim={colors:[2,4,6],labels:['Vx','Vy','Vz'],ysubtitle:'[km/s]',labflag:-1,constant:0}

;Vi_Bperp,para
tinterpol_mxn,'CL'+sc+'_Bgsm','CL'+sc+'_Vigsm', newname='CL'+sc+'_Bgsm_int'
get_data,'CL'+sc+'_Vigsm',data=Vdata,index=index
get_data,'CL'+sc+'_Bgsm_int',data=Bdata,index=index2
if index ne 0 and index2 ne 0 then begin
  Bdata2=Bdata.y(*,0:2)
  Btotdata=Bdata.y(*,3)
  Bunit=Bdata2/Btotdata(*)
  tmp1=crossp2(Vdata.y,Bunit)
  Vperp=crossp2(Bunit,tmp1)

  get_data,'CL'+sc+'_Bgsm_pol',data=Bgsm_pol
  Vpara=(Vdata.y(*,0)*Bdata2(*,0)+Vdata.y(*,1)*Bdata2(*,1)+Vdata.y(*,2)*Bdata2(*,2))/Btotdata(*)
  if(n_elements(where(Bgsm_pol.y(*,0) gt 0)) gt 1) then Vpara(where(Bgsm_pol.y(*,0) gt 0))=-Vpara(where(Bgsm_pol.y(*,0) gt 0))
  store_data,'CL'+sc+'_Vigsm2',data={x:Vdata.x, y:[[Vperp(*,0)],[Vperp(*,1)],[Vperp(*,2)],[Vpara(*)]]},dlim={colors:[2,4,6,0],labels:['Vx','Vy','Vz','V||'],ysubtitle:'[km/s]',labflag:-1,constant:0}
  store_data,'CL'+sc+'_Vigsm3',data={x:Vdata.x, y:[[Vperp(*,0)],[Vperp(*,1)],[Vperp(*,2)]]},dlim={colors:[2,4,6],labels:['Vx','Vy','Vz'],ysubtitle:'[km/s]',labflag:-1,constant:0}
  store_data,'CL'+sc+'_Vipara',data={x:Vdata.x, y:[[Vpara(*)]]},dlim={colors:[0],labels:['V||'],ysubtitle:'[km/s]',labflag:-1,constant:0}
endif

;Ti
if(sc ne '4') then begin
get_data,'c'+sc+'_cis_T_HIA_perp__C'+sc+'_PP_CIS',data=Tperp,index=index
get_data,'c'+sc+'_cis_T_HIA_par__C'+sc+'_PP_CIS',data=Tpara
endif else begin
get_data,'c'+sc+'_cis_T_p_perp__C'+sc+'_PP_CIS',data=Tperp
get_data,'c'+sc+'_cis_T_p_par__C'+sc+'_PP_CIS',data=Tpara
endelse
if index ne 0 then store_data,'CL'+sc+'_Ei',data={x:Tperp.x, y:[[Tpara.y(*)/11600*1e6],[Tperp.y(*)/11600*1e6]]},dlim={colors:[2,6],labels:['Eipara','Eiperp'],ysubtitle:'[eV]',labflag:-1,ylog:1}

;Pi
if index ne 0 then store_data,'CL'+sc+'_Pi',data={x:Ndata.x,y:Ndata.y*Tperp.y(*)/11600*1e6*1.6e-19*1e9*1e6},dlim={colors:[0],labels:['Pi'],ysubtitle:['[nPa]'],labflag:-1,constant:[0],ylog:1}

;Evxb
get_data,'CL'+sc+'_Bgsm_int',data=Bdata
Bdata2=fltarr(n_elements(Bdata.x),3)
Bdata2(*,0:2)=Bdata.y(*,0:2)
get_data,'CL'+sc+'_Vigsm3',data=Vdata,index=index
if index ne 0 then begin
  Evxb=crossp2(Bdata2,Vdata.y)*1e-3
  store_data,'CL'+sc+'_Evxbgsm',data={x:Bdata.x,y:Evxb},dlim={colors:[2,4,6],labels:['Ex','Ey','Ez'],ysubtitle:'[mV/m]',labflag:-1,constant:[0],ylog:0}
endif

;V_XYZ-VDH
if(sc ne '4') then begin
tinterpol_mxn, 'c'+sc+'_aux_sc_r_xyz_gsm__C'+sc+'_SP_AUX','c'+sc+'_cis_V_HIA_xyz_gse__C'+sc+'_PP_CIS',newname='c'+sc+'_aux_sc_r_xyz_gsm__C'+sc+'_SP_AUX_int2'
endif else begin
tinterpol_mxn, 'c'+sc+'_aux_sc_r_xyz_gsm__C'+sc+'_SP_AUX','c'+sc+'_cis_V_p_xyz_gse__C'+sc+'_PP_CIS',newname='c'+sc+'_aux_sc_r_xyz_gsm__C'+sc+'_SP_AUX_int2'
endelse
get_data,'c'+sc+'_aux_sc_r_xyz_gsm__C'+sc+'_SP_AUX_int2',data=tmp
MLTm12=atan(tmp.y[*,1]/tmp.y[*,0])
if(n_elements(where(tmp.y[*,0] lt 0)) ne 1) then MLTm12(where(tmp.y[*,0] lt 0))=(atan(tmp.y[where(tmp.y[*,0] lt 0),1]/tmp.y[where(tmp.y[*,0] lt 0),0])+!pi)
V_VDH=Vperp
V_VDH(*,0)=-(Vperp(*,0)*cos(MLTm12(*))+Vperp(*,1)*sin(MLTm12(*)))
V_VDH(*,1)= -Vperp(*,0)*sin(MLTm12(*))+Vperp(*,1)*cos(MLTm12(*))

;store data
store_data,'CL'+sc+'_Vigsm_pol',data={x:Vdata.x, y:[[V_VDH(*,0)],[V_VDH(*,1)],[V_VDH(*,2)]]},dlim={colors:[2,4,6],labels:['V-r','Ve','Vz'],ysubtitle:'[km/s]',labflag:-1,constant:0}

endif;index

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ASP

;ion current
get_data,'c'+sc+'_asp_I_ion__C'+sc+'_PP_ASP',data=tmp,index=index
if(index ne 0) then begin
tmp.y=-tmp.y
if(n_elements(where(abs(tmp.y) lt 1e-5)) ne 1) then tmp.y[where(abs(tmp.y) lt 1e-5)]='NaN'
store_data,'CL'+sc+'_ASP',data={x:tmp.x, y:tmp.y},dlim={colors:[6],labels:['-ASP'],ysubtitle:'[uA]',constant:0}
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PEA

get_data,'c'+sc+'_pea_N_e_den__C'+sc+'_PP_PEA',data=Ndata,index=index
get_data,'c'+sc+'_pea_T_e_par__C'+sc+'_PP_PEA',data=Tpara,index=index
get_data,'c'+sc+'_pea_T_e_perp__C'+sc+'_PP_PEA',data=Tperp,index=index
if(index ne 0) then begin
store_data,'CL'+sc+'_Ne',data={x:Ndata.x, y:Ndata.y},dlim={colors:[0],labels:['Ne'],ysubtitle:'[/cm3]',constant:0,ylog:1}
store_data,'CL'+sc+'_Ee',data={x:Tperp.x, y:[[Tpara.y(*)/11600*1e6],[Tperp.y(*)/11600*1e6]]},dlim={colors:[2,6],labels:['Eepara','Eeperp'],ysubtitle:'[eV]',labflag:-1,ylog:1}
store_data,'CL'+sc+'_Pe',data={x:Ndata.x,y:Ndata.y*Tperp.y(*)/11600*1e6*1.6e-19*1e9*1e6},dlim={colors:[0],labels:['Pe'],ysubtitle:['[nPa]'],labflag:-1,constant:[0],ylog:1}
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;pressure
get_data,'CL'+sc+'_Pi',data=Pthdata,dlim=dlim,index=index
get_data,'CL'+sc+'_Bgsm',data=Bdata
get_data,'CL'+sc+'_Vigsm',data=Vdata
get_data,'CL'+sc+'_Ni',data=Ndata

if(index ne 0) then begin

Pmagdata=(Bdata.y(*,3)^2/(2*4*!pi*1e-7))*1e-9
Pdyndata=Ndata.y(*)
Pdyndata=1.670e-27*Ndata.y(*)*(Vdata.y(*,0)^2+Vdata.y(*,1)^2+Vdata.y(*,2)^2)*1e21

Pmagdata_interp=interp(Pmagdata(*),Bdata.x,Pthdata.x)

Pdata=fltarr(n_elements(Pthdata.x),4)
Pdata(*,0)=Pthdata.y(*)
Pdata(*,1)=Pmagdata_interp(*)
Pdata(*,2)=Pdyndata(*)
Pdata(*,3)=Pthdata.y(*)+Pmagdata_interp(*)+Pdyndata(*)
store_data,'CL'+sc+'_Pall',data={x:Pthdata.x,y:[[Pdata(*,3)],[Pdata(*,2)],[Pdata(*,1)],[Pdata(*,0)]]},dlim={colors:[0,6,4,2],labels:['Pall','Pdyn','Pmag','Pth'],ysubtitle:'[nPa]',labflag:-1,constant:[0],ylog:1}

endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;number flux
get_data,'CL'+sc+'_Ni',data=Ni,index=index
get_data,'CL'+sc+'_Vigsm2',data=Vigsm

if(index ne 0) then begin
Nflux=fltarr(n_elements(Ni.x),4)
Nflux(*,0)=Vigsm.y(*,0)*Ni.y(*)
Nflux(*,1)=Vigsm.y(*,1)*Ni.y(*)
Nflux(*,2)=Vigsm.y(*,2)*Ni.y(*)
Nflux(*,3)=Vigsm.y(*,3)*Ni.y(*)
store_data,'CL'+sc+'_Nflux',data={x:Ni.x,y:Nflux},dlim={colors:[2,4,6,0],labels:['Fx','Fy','Fz','F||'],ysubtitle:'[km/s/cc]',labflag:-1,constant:0}
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;median

;;;;;
;FGM
thm_detrend4,'CL'+sc+'_Bgsm',90,3
thm_detrend4,'CL'+sc+'_Bgsm_pol',90,3
thm_detrend4,'CL'+sc+'_Bgsm-T01_pol',90,3
;thm_detrend4,'CL'+sc+'_Bgse',90,3
;thm_detrend4,'CL'+sc+'_Bgse_pol',90,3
;thm_detrend4,'CL'+sc+'_Bgse-T01_pol',90,3
thm_detrend4,'CL'+sc+'_Bsm-T01_pol',90,3
thm_detrend4,'CL'+sc+'_Bsm-T01',30,3; for FAC calc
thm_detrend4,'CL'+sc+'_Bsm',90,3
thm_median,'CL'+sc+'_Bgsm',90
thm_median,'CL'+sc+'_Bgsm_pol',90
thm_median,'CL'+sc+'_Bgsm-T01_pol',90
;thm_median,'CL'+sc+'_Bgse',90
;thm_median,'CL'+sc+'_Bgse_pol',90
;thm_median,'CL'+sc+'_Bgse-T01_pol',90
thm_median,'CL'+sc+'_Bsm-T01_pol',90
thm_median,'CL'+sc+'_Bsm-T01',30; for FAC calc
thm_median,'CL'+sc+'_Bsm',90

;EDI
thm_detrend4,'CL'+sc+'_EDI',90,3
thm_detrend4,'CL'+sc+'_EDISM',90,3
get_data,'CL'+sc+'_EDI',index=index,data=data
if (index ne 0) then begin
if (n_elements(data.x) ge 30) then begin
thm_median,'CL'+sc+'_EDI',90
thm_median,'CL'+sc+'_EDISM',90
endif
endif

;Evxb
;thm_detrend4,'CL'+sc+'_Evxbgsm',90,3
;thm_median,'CL'+sc+'_Evxbgsm',90

;;;;;
thm_detrend4,'CL'+sc+'_EFW',90,3
thm_median,'CL'+sc+'_EFW',90

;;;;;
thm_detrend4,'CL'+sc+'_Vsc',90,3
thm_median,'CL'+sc+'_Vsc',90

;;;;;
thm_detrend4,'CL'+sc+'_Ni',90,3
thm_median,'CL'+sc+'_Ni',90

;;;;;
thm_detrend4,'CL'+sc+'_Pall',90,3
thm_median,'CL'+sc+'_Pall',90

;;;;;
;thm_detrend4,'CL'+sc+'_Ei',90,3
;thm_median,'CL'+sc+'_Ei',90

;;;;;
thm_detrend4,'CL'+sc+'_Vigsm',90,3
thm_median,'CL'+sc+'_Vigsm',90

;;;;;
;thm_detrend4,'CL'+sc+'_Vigsm3',90,3
;thm_median,'CL'+sc+'_Vigsm3',90

;;;;;
;thm_detrend4,'CL'+sc+'_Nflux',90,3
;thm_median,'CL'+sc+'_Nflux',90


;;;;;
;thm_detrend4,'CL'+sc+'_Vigsm_pol',90,3
;thm_median,'CL'+sc+'_Vigsm_pol',90

;;;;;
thm_detrend4,'CL'+sc+'_Vipara',90,3
thm_median,'CL'+sc+'_Vipara',90

;;;;;
thm_detrend4,'CL'+sc+'_Vigsm_pol',90,3
thm_median,'CL'+sc+'_Vigsm_pol',90

thm_detrend4,'CL'+sc+'_Ne',90,3
thm_median,'CL'+sc+'_Ne',90
;thm_detrend4,'CL'+sc+'_Te',90,3
;thm_median,'CL'+sc+'_Te',90
thm_detrend4,'CL'+sc+'_Pe',90,3
thm_median,'CL'+sc+'_Pe',90

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
store_data, 'CL'+sc+'_Vsc+ASP', data='CL'+sc+'_Vsc_s '+'CL'+sc+'_ASP', dlimit={panel_size:1.0,ysubtitle:'[V] [uA]'}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,'CL'+sc+'_Bgsm-T01_pol_s',data=Bdata
get_data,'CL'+sc+'_Evxbgsm_s',data=Edata,index=index
get_data,'CL'+sc+'_Vigsm_pol_s',data=Vdata

if(index ne 0) then begin
store_data,'CL'+sc+'_Bigsme_s',data={x:Bdata.x,y:[Bdata.y(*,1)]},dlim={colors:[4],labels:['Be'],ysubtitle:['[nT]'],labflag:-1,constant:[0],ylog:0}
store_data,'CL'+sc+'_Evxbgsmez_s',data={x:Edata.x,y:[[Edata.y(*,0)],[Edata.y(*,1)]]},dlim={colors:[6,4],labels:['Ez','Ee'],ysubtitle:['[mV/m]'],labflag:-1,constant:[0],ylog:0}
store_data,'CL'+sc+'_Vigsmre_s',data={x:Vdata.x,y:[[Vdata.y(*,2)],[Vdata.y(*,1)]]},dlim={colors:[2,4],labels:['V-r','Ve'],ysubtitle:['[km/s]'],labflag:-1,constant:[0],ylog:0}
;tplot,['CL'+sc+'_Bgsm','CL'+sc+'_Bigsme_s','CL'+sc+'_Evxbgsmez_s','CL'+sc+'_Vigsmre_s','CL'+sc+'_Pall_s']
endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ylim,'CL'+sc+'_Bgsm-T01_pol_s',-50,50
ylim,'CL'+sc+'_Bsm-T01_pol_s',-50,50
ylim,'CL'+sc+'_Bgsm-T01_pol_m',-50,50
ylim,'CL'+sc+'_Bsm-T01_pol_m',-50,50

ylim,'CL'+sc+'_Ni_m',0.01,3,1
ylim,'CL'+sc+'_Ni_s',0.02,30
ylim,'CL'+sc+'_Vigsm3_s',-100,100
ylim,'CL'+sc+'_Vigsm_pol_s',-100,100
ylim,'CL'+sc+'_EFW_s',-4,4
ylim,'CL'+sc+'_EDI_s',-4,4
ylim,'CL'+sc+'_EDI_m',-4,4
ylim,'CL'+sc+'_Evxbgsm_s',-4,4
ylim,'CL'+sc+'_Vsc_s',-40,0
ylim,'CL'+sc+'_Pall_s',4e-4,1e1
ylim,'CL'+sc+'_Pall_m',4e-4,1e1
ylim,'CL'+sc+'_Vipara_s',-400,1000
ylim,'CL'+sc+'_Ei_s',100,10000
ylim,'CL'+sc+'_Ne_m',0.1,1e2,1
ylim,'CL'+sc+'_Te_m',10,3e3,1
ylim,'CL'+sc+'_Pe_m',1e-3,1,1

options,'CL'+sc+'_Bgsm-T01_pol_s',yticks=4,yminor=3
options,'CL'+sc+'_Bsm-T01_pol_s',yticks=4,yminor=3
options,'CL'+sc+'_Bgsm-T01_pol_m',yticks=4,yminor=3
options,'CL'+sc+'_Bsm-T01_pol_m',yticks=4,yminor=3
options,'CL'+sc+'_Vsc_s',yticks=2,yminor=2,panel_size=0.5
options,'CL'+sc+'_Vigsm3_s',yticks=2,yminor=4
options,'CL'+sc+'_Ni_s',panel_size=0.7
options,'CL'+sc+'_EFW_s',panel_size=0.5
options,'CL'+sc+'_Vipara_s',yticks=2,yminor=2,panel_size=0.5

ylim,'CL'+sc+'_Vsc+ASP',-40,0
options,'CL'+sc+'_Vsc+ASP',yticks=2,yminor=2,panel_size=0.5
options,'CL'+sc+'_Vigsm3_s',panel_size=0.7

options,'CL'+sc+'_Pall_s',ytickformat='logticks_exp'
options,'CL'+sc+'_Pall_m',ytickformat='logticks_exp'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;tplot
if keyword_set(noplot) then begin
options,'*',yticklen=0.02
options,'*',xticklen=0.06
time_stamp,/off
endif else begin
options,'*',yticklen=0.02
options,'*',xticklen=0.06
time_stamp,/off
var_label = ['C'+sc+'_MLAT','C'+sc+'_MLT','C'+sc+'_R']
tplot_options,title='CLUSTER CL'+sc+' overview '+Syear+Smonth+Sday
;tplot,['CL'+sc+'_Bsm-T01_pol_m','CL'+sc+'_Pall_m','CL'+sc+'_Vigsm_pol_m','CL'+sc+'_Vipara_m','CL'+sc+'_Evxbgsm_m','CL'+sc+'_EDI_m','CL'+sc+'_EFW_m','CL'+sc+'_Vsc+ASP'],var_label = ['C'+sc+'_MLAT','C'+sc+'_MLT','C'+sc+'_R']
tplot,['CL'+sc+'_Bsm_m','CL'+sc+'_Bsm-T01_m','CL'+sc+'_Pall_m','CL'+sc+'_Pe_m','CL'+sc+'_Ni_m','CL'+sc+'_Vigsm_m','CL'+sc+'_Vipara_m','CL'+sc+'_EDI_m','CL'+sc+'_EFW_m','CL'+sc+'_Vsc+ASP'],var_label = ['C'+sc+'_MLAT','C'+sc+'_MLT','C'+sc+'_R']
;makepng,strjoin('./png/overview/CL'+sc+'_'+Syear+Smonth+Sday)
endelse

end
