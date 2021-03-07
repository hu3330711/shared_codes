;Purpose
;Get S/C L-shell in a magnetic field model
;
;Usage
;timespan,'2008-1-1'
;thm_map_iono3,sc='a'
;
;
pro thm_map_iono3,sc=sc,show_example=show_example,swfix=swfix,model=model

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
get_tsy_params,'SYM_H','omni_imf','proton_density','flow_speed',model,/speed,/imf_yz,trange=[t[0]-3900,t[1]]
par = model + '_par'

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;ttrace
outname = 'th'+sc+'_iono'

tKm2Re,'th'+sc+'_state_pos'
print,'Performing ttrace2iono'
ttrace2iono,'th'+sc+'_state_pos_re',internal_model='igrf',external_model=model,par=par,in_coord='sm',out_coord='sm',newname=outname,r0=120.,/NOBOUNDARY,rlim=100.,trace_var_name=outname+'_trace'

xyz_to_polar,outname
copy_data,outname+'_mag',outname+'_L_'+model

cotrans,outname,outname+'_gsm',/sm2gsm,/ignore_dlimits
cotrans,outname+'_gsm',outname+'_gse',/gsm2gse,/ignore_dlimits
cotrans,outname+'_gse',outname+'_gei',/gse2gei,/ignore_dlimits
cotrans,outname+'_gei',outname+'_geo',/gei2geo,/ignore_dlimits

xyz_to_polar,outname+'_geo'
copy_data,outname+'_geo_th',outname+'_geo_glat'
copy_data,outname+'_geo_phi',outname+'_geo_glon'

get_data,outname,data=tmp
R=sqrt(tmp.y[*,0]^2+tmp.y[*,1]^2+tmp.y[*,2]^2)
store_data, 'th'+sc+'_R_foot', data={x:tmp.x, y:R},dlim={colors:[0],labels:['R'],ysubtitle:'[RE]',labflag:1,constant:0,ytitle:'th'+sc+'_R'}
MLT=atan(tmp.y[*,1]/tmp.y[*,0])*180/!pi/15.+12
if(n_elements(where(tmp.y[*,0] lt 0)) gt 1) then MLT(where(tmp.y[*,0] lt 0))=(atan(tmp.y[where(tmp.y[*,0] lt 0),1]/tmp.y[where(tmp.y[*,0] lt 0),0])+!pi)*180/!pi/15.+12
if(n_elements(where(MLT[*] gt 24)) gt 1) then MLT(where(MLT[*] ge 24))=MLT(where(MLT[*] ge 24))-24
store_data, 'th'+sc+'_MLT_foot', data={x:tmp.x, y:MLT},dlim={colors:[0],labels:[''],ysubtitle:'[h]',labflag:1,constant:0,ytitle:'th'+sc+'_MLT'}
MLAT=atan(tmp.y[*,2]/sqrt(tmp.y[*,0]^2+tmp.y[*,1]^2))*180/!pi
store_data, 'th'+sc+'_MLAT_foot', data={x:tmp.x, y:MLAT},dlim={colors:[0],labels:['MLAT'],ysubtitle:'[deg]',labflag:1,constant:0,ytitle:'th'+sc+'_MLAT'}
Ldip=R/cos(MLAT*!pi/180)^2
store_data, 'th'+sc+'_Ldip', data={x:tmp.x, y:Ldip},dlim={colors:[0],labels:['L'],ysubtitle:'[deg]',labflag:1,constant:0,ytitle:'th'+sc+'_Ldip'}

get_data,outname+'_geo_glat',data=glat
get_data,outname+'_geo_glon',data=glon
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
store_data,outname+'_aacgm_mlat',data={x:glat.x,y:aacgmmlat}
store_data,outname+'_aacgm_mlon',data={x:glat.x,y:aacgmmlon}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if keyword_set(show_example) then begin
;time_clip,'th'+sc+'_state_pos_re',t[0],t[0]+359,newname='initpos_for_trace'
time_clip,'th'+sc+'_state_pos_re',t[0],t[1]-1,newname='initpos_for_trace'

ttrace2iono,'initpos_for_trace',external_model=model,par=par,in_coord='sm',out_coord='sm',trace_var_name='trace_n'
ttrace2iono,'initpos_for_trace',external_model=model,par=par,in_coord='sm',out_coord='sm',trace_var_name='trace',r0=3.,/noboundary

ttrace2iono,'initpos_for_trace',external_model=model,par=par,in_coord='sm',out_coord='sm',trace_var_name='trace_s',r0=1.,/NOBOUNDARY,/south
;ttrace2iono,'initpos_for_trace',external_model=model,par=par,in_coord='sm',out_coord='sm',trace_var_name='trace_s',/south

xrange = [-20,10] ;x range of the xz plot
yrange = [-10,10] ;y range of the xz plot
zrange = [-10,10] ;z range of the xz plot
window,xs=600,ys=600

tplotxy,'trace_n',versus='yrz',xrange=xrange,yrange=zrange,charsize=1,title="Y-Z",pstart=1,symsize=0.8,xthick=1,ythick=1,thick=1,charthick=1,ymargin=[.15,.1],multi='2 2'
tplotxy,'trace_s',versus='yrz',xrange=xrange,yrange=zrange,charsize=1,title="Y-Z",pstart=1,symsize=0.8,xthick=1,ythick=1,thick=1,charthick=1,ymargin=[.15,.1],/over
tplotxy,'trace',versus='yrz',xrange=xrange,yrange=zrange,charsize=1,title="Y-Z",pstart=1,symsize=0.8,xthick=1,ythick=1,thick=1,charthick=1,color=6,ymargin=[.15,.1],/over

tplotxy,'trace_n',versus='yx',xrange=yrange,yrange=xrange,charsize=1,title="X-Y",pstart=1,symsize=0.8,xthick=1,ythick=1,thick=1,charthick=1,ymargin=[.15,.1],/add
tplotxy,'trace_s',versus='yx',xrange=yrange,yrange=xrange,charsize=1,title="X-Y",pstart=1,symsize=0.8,xthick=1,ythick=1,thick=1,charthick=1,ymargin=[.15,.1],/over
tplotxy,'trace',versus='yx',xrange=yrange,yrange=xrange,charsize=1,title="X-Y",pstart=1,symsize=0.8,xthick=1,ythick=1,thick=1,charthick=1,color=6,ymargin=[.15,.1],/over

tplotxy,'trace_n',versus='xrz',xrange=xrange,yrange=zrange,charsize=1,title="X-Z",pstart=1,symsize=0.8,xthick=1,ythick=1,thick=1,charthick=1,ymargin=[.15,.1],/add
tplotxy,'trace_s',versus='xrz',xrange=xrange,yrange=zrange,charsize=1,title="X-Z",pstart=1,symsize=0.8,xthick=1,ythick=1,thick=1,charthick=1,ymargin=[.15,.1],/over
tplotxy,'trace',versus='xrz',xrange=xrange,yrange=zrange,charsize=1,title="X-Z",pstart=1,symsize=0.8,xthick=1,ythick=1,thick=1,charthick=1,color=6,ymargin=[.15,.1],/over
endif

end
