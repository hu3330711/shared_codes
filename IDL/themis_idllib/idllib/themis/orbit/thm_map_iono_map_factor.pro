;Purpose
;Get S/C L-shell in a magnetic field model
;
;Usage
;timespan,'2008-1-1'
;thm_map_iono3,sc='a'
;
;
pro thm_map_iono_map_factor,sc=sc,show_example=show_example,swfix=swfix,model=model

;set time
get_timespan, t

;set Bmodel
if not keyword_set(model) then model = 't01' ;t96,t01,t04s

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;solar wind data
omni_load,median=1,/noplot,/init
tdegap,'Psw',/overwrite
tdeflag,'Psw','linear',/overwrite
tdegap,'SYM',/overwrite
tdeflag,'SYM','linear',/overwrite
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
if(model eq 't89') then begin
par2=fltarr(1,1)
par2(0,*)=1
parx=dblarr(1)
parx(0)=t[0]
store_data,'t89_par',data={x:parx,y:par2}
endif
if(model eq 'none') then begin
par2=fltarr(1,1)
par2(0,*)=1
parx=dblarr(1)
parx(0)=t[0]
store_data,'none_par',data={x:parx,y:par2}
endif

if keyword_set(swfix) then begin
  get_data,par,data=data
  temp=min(abs(data.x-t[0]),index)
  store_data,par,data={x:data.x(index),y:data.y(index,*)}
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;position
thm_load_state,probe=sc,coord='sm'
time_clip,'th'+sc+'_state_pos',time_double(t[0]),time_double(t[1]),/replace

get_data,'th'+sc+'_state_pos',data=tmp,dlim=dlim
tmp.y=tmp.y/6372.

R=sqrt(tmp.y[*,0]^2+tmp.y[*,1]^2+tmp.y[*,2]^2)
store_data, 'th'+sc+'_R', data={x:tmp.x, y:R},dlim={colors:[0],labels:['R'],ysubtitle:'[RE]',labflag:1,constant:0,ytitle:'th'+sc+'_R'}
MLT=atan(tmp.y[*,1]/tmp.y[*,0])*180/!pi/15.+12
if(n_elements(where(tmp.y[*,0] lt 0)) gt 1) then MLT(where(tmp.y[*,0] lt 0))=(atan(tmp.y[where(tmp.y[*,0] lt 0),1]/tmp.y[where(tmp.y[*,0] lt 0),0])+!pi)*180/!pi/15.+12
if(n_elements(where(MLT[*] gt 24)) gt 1) then MLT(where(MLT[*] ge 24))=MLT(where(MLT[*] ge 24))-24
store_data, 'th'+sc+'_MLT', data={x:tmp.x, y:MLT},dlim={colors:[0],labels:[''],ysubtitle:'[h]',labflag:1,constant:0,ytitle:'th'+sc+'_MLT'}
MLAT=atan(tmp.y[*,2]/sqrt(tmp.y[*,0]^2+tmp.y[*,1]^2))*180/!pi
store_data, 'th'+sc+'_MLAT', data={x:tmp.x, y:MLAT},dlim={colors:[0],labels:['MLAT'],ysubtitle:'[deg]',labflag:1,constant:0,ytitle:'th'+sc+'_MLAT'}


r0=R
mlat0=MLAT
mlt0=MLT
dr=0.3
dmlt=0.1

r=r0
mlat=mlat0
mlt=mlt0
mltm12=(mlt-12.)*15.
pos=fltarr(n_elements(tmp.x),3)
pos(*,0)=r*cos(mlat*!pi/180)*cos(mltm12*!pi/180)
pos(*,1)=r*cos(mlat*!pi/180)*sin(mltm12*!pi/180)
pos(*,2)=r*sin(mlat*!pi/180)
time=dblarr(1)
time(0)=t[0]
store_data,'th'+sc+'_state_pos_re_sm',data={x:tmp.x,y:pos};sm
outname='th'+sc+'_state_pos_re'
cotrans,outname+'_sm', outname+'_gsm',/sm2gsm,/ignore_dlimits
cotrans,outname+'_gsm',outname+'_gse',/gsm2gse,/ignore_dlimits
cotrans,outname+'_gse',outname+'_gei',/gse2gei,/ignore_dlimits
cotrans,outname+'_gei',outname+'_geo',/gei2geo,/ignore_dlimits
copy_data,outname+'_geo',outname

r=r0-dr
mlat=mlat0
mlt=mlt0
mltm12=(mlt-12.)*15.
pos=fltarr(n_elements(tmp.x),3)
pos(*,0)=r*cos(mlat*!pi/180)*cos(mltm12*!pi/180)
pos(*,1)=r*cos(mlat*!pi/180)*sin(mltm12*!pi/180)
pos(*,2)=r*sin(mlat*!pi/180)
time=dblarr(1)
time(0)=t[0]
store_data,'th'+sc+'_state_pos2_re_sm',data={x:tmp.x,y:pos};sm
outname='th'+sc+'_state_pos2_re'
cotrans,outname+'_sm', outname+'_gsm',/sm2gsm,/ignore_dlimits
cotrans,outname+'_gsm',outname+'_gse',/gsm2gse,/ignore_dlimits
cotrans,outname+'_gse',outname+'_gei',/gse2gei,/ignore_dlimits
cotrans,outname+'_gei',outname+'_geo',/gei2geo,/ignore_dlimits
copy_data,outname+'_geo',outname

r=r0
mlat=mlat0
mlt=mlt0+dmlt
mltm12=(mlt-12.)*15.
pos=fltarr(n_elements(tmp.x),3)
pos(*,0)=r*cos(mlat*!pi/180)*cos(mltm12*!pi/180)
pos(*,1)=r*cos(mlat*!pi/180)*sin(mltm12*!pi/180)
pos(*,2)=r*sin(mlat*!pi/180)
time=dblarr(1)
time(0)=t[0]
store_data,'th'+sc+'_state_pos3_re_sm',data={x:tmp.x,y:pos};sm
outname='th'+sc+'_state_pos3_re'
cotrans,outname+'_sm', outname+'_gsm',/sm2gsm,/ignore_dlimits
cotrans,outname+'_gsm',outname+'_gse',/gsm2gse,/ignore_dlimits
cotrans,outname+'_gse',outname+'_gei',/gse2gei,/ignore_dlimits
cotrans,outname+'_gei',outname+'_geo',/gei2geo,/ignore_dlimits
copy_data,outname+'_geo',outname

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ttrace
outname = 'th'+sc+'_iono'
print,outname

;tKm2Re,'th'+sc+'_state_pos'
print,'Performing ttrace2iono'
ttrace2iono,'th'+sc+'_state_pos_re',internal_model='igrf',external_model=model,par=par,in_coord='geo',out_coord='sm',newname=outname,r0=1.,/NOBOUNDARY,rlim=500.*6372,trace_var_name=outname+'_trace'

xyz_to_polar,outname
copy_data,outname+'_mag',outname+'_L_'+model

cotrans,outname,outname+'_gsm',/sm2gsm,/ignore_dlimits
cotrans,outname+'_gsm',outname+'_gse',/gsm2gse,/ignore_dlimits
cotrans,outname+'_gse',outname+'_gei',/gse2gei,/ignore_dlimits
cotrans,outname+'_gei',outname+'_geo',/gei2geo,/ignore_dlimits

xyz_to_polar,outname+'_geo'
copy_data,outname+'_geo_th',outname+'_geo_glat'
copy_data,outname+'_geo_phi',outname+'_geo_glon'

get_data,outname+'_geo_glat',data=glat
get_data,outname+'_geo_glon',data=glon
get_data,outname+'_mag',data=r
alt=(r.y-1)*6372.

aacgmidl
alt=glat.y*0.+110.
;aacgmconvcoord, glat.y,glon.y,alt, aacgmmlat,aacgmmlon,err, /TO_AACGM
aacgmmlat=glat.y*'NaN'
aacgmmlon=glat.y*'NaN'
for loop=0L,n_elements(glat.y)-1 do begin
  cnv_aacgm,glat.y[loop],glon.y[loop],110.,mlat,mlon,r,err
  aacgmmlat[loop]=mlat
  aacgmmlon[loop]=mlon
endfor
store_data,outname+'_mlat',data={x:glat.x,y:aacgmmlat}
store_data,outname+'_mlon',data={x:glat.x,y:aacgmmlon}


;;;;;;;;;;;;;;
outname = 'th'+sc+'_iono2'
print,outname

;tKm2Re,'th'+sc+'_state_pos'
print,'Performing ttrace2iono'
ttrace2iono,'th'+sc+'_state_pos2_re',internal_model='igrf',external_model=model,par=par,in_coord='geo',out_coord='sm',newname=outname,r0=1.,/NOBOUNDARY,rlim=500.*6372,trace_var_name=outname+'_trace'

xyz_to_polar,outname
copy_data,outname+'_mag',outname+'_L_'+model

cotrans,outname,outname+'_gsm',/sm2gsm,/ignore_dlimits
cotrans,outname+'_gsm',outname+'_gse',/gsm2gse,/ignore_dlimits
cotrans,outname+'_gse',outname+'_gei',/gse2gei,/ignore_dlimits
cotrans,outname+'_gei',outname+'_geo',/gei2geo,/ignore_dlimits

xyz_to_polar,outname+'_geo'
copy_data,outname+'_geo_th',outname+'_geo_glat'
copy_data,outname+'_geo_phi',outname+'_geo_glon'

get_data,outname+'_geo_glat',data=glat
get_data,outname+'_geo_glon',data=glon
get_data,outname+'_mag',data=r
alt=(r.y-1)*6372.

;aacgmconvcoord, glat.y,glon.y,alt, aacgmmlat,aacgmmlon,err, /TO_AACGM
aacgmmlat=glat.y*'NaN'
aacgmmlon=glat.y*'NaN'
for loop=0L,n_elements(glat.y)-1 do begin
  cnv_aacgm,glat.y[loop],glon.y[loop],110.,mlat,mlon,r,err
  aacgmmlat[loop]=mlat
  aacgmmlon[loop]=mlon
endfor
store_data,outname+'_mlat',data={x:glat.x,y:aacgmmlat}
store_data,outname+'_mlon',data={x:glat.x,y:aacgmmlon}

;;;;;;;;;;;;;;
outname = 'th'+sc+'_iono3'
print,outname

;tKm2Re,'th'+sc+'_state_pos'
print,'Performing ttrace2iono'
ttrace2iono,'th'+sc+'_state_pos3_re',internal_model='igrf',external_model=model,par=par,in_coord='geo',out_coord='sm',newname=outname,r0=1.,/NOBOUNDARY,rlim=500.*6372,trace_var_name=outname+'_trace'

xyz_to_polar,outname
copy_data,outname+'_mag',outname+'_L_'+model

cotrans,outname,outname+'_gsm',/sm2gsm,/ignore_dlimits
cotrans,outname+'_gsm',outname+'_gse',/gsm2gse,/ignore_dlimits
cotrans,outname+'_gse',outname+'_gei',/gse2gei,/ignore_dlimits
cotrans,outname+'_gei',outname+'_geo',/gei2geo,/ignore_dlimits

xyz_to_polar,outname+'_geo'
copy_data,outname+'_geo_th',outname+'_geo_glat'
copy_data,outname+'_geo_phi',outname+'_geo_glon'

get_data,outname+'_geo_glat',data=glat
get_data,outname+'_geo_glon',data=glon
get_data,outname+'_mag',data=r
alt=(r.y-1)*6372.

;aacgmconvcoord, glat.y,glon.y,alt, aacgmmlat,aacgmmlon,err, /TO_AACGM
aacgmmlat=glat.y*'NaN'
aacgmmlon=glat.y*'NaN'
for loop=0L,n_elements(glat.y)-1 do begin
  cnv_aacgm,glat.y[loop],glon.y[loop],110.,mlat,mlon,r,err
  aacgmmlat[loop]=mlat
  aacgmmlon[loop]=mlon
endfor
store_data,outname+'_mlat',data={x:glat.x,y:aacgmmlat}
store_data,outname+'_mlon',data={x:glat.x,y:aacgmmlon}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;mapping factor
outname = 'th'+sc+'_iono'
get_data,outname+'_mlat',data=mlat
get_data,outname+'_mlon',data=mlon

outname = 'th'+sc+'_iono2'
get_data,outname+'_mlat',data=mlat2
get_data,outname+'_mlon',data=mlon2

outname = 'th'+sc+'_iono3'
get_data,outname+'_mlat',data=mlat3
get_data,outname+'_mlon',data=mlon3

dmlat=mlat2.y-mlat.y
dmlon=mlon3.y-mlon.y

dmlt2=r0*dmlt*15*!pi/180
dmlon2=(alt/6372.+1)*cos(mlat.y*!pi/180)*dmlon*!pi/180
dmlat2=(alt/6372.+1)*dmlat*!pi/180

dl_r=abs(dr/dmlat2)
dl_phi=abs(dmlt2/dmlon2)

store_data,'th'+sc+'_mapfactor_r',data={x:tmp.x,y:dl_r}
store_data,'th'+sc+'_mapfactor_phi',data={x:tmp.x,y:dl_phi}

end