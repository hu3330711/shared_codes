pro cluster_load_cdaweb_orbit,sc=sc,noplot=noplot

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


end
