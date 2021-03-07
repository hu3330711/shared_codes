pro mad_load_dmsp_hdf,sc=sc,ssiesonly=ssiesonly,nofac=nofac,map_from_south=map_from_south

;url = ['http://cedar.openmadrigal.org' ]
;result = madGetAllInstruments(url)
;site_info=result[where(strmatch(result.MNEMONIC,'dms'))]

filetype=['s1','s4','e']
if keyword_set(ssiesonly) then filetype=['s1','s4']

get_timespan,t


for loop_filetype=0L,n_elements(filetype)-1 do begin
  for loop=0,(t[1]-t[0])/86400-1 do begin
    time=t[0]+86400.*(loop-0)
    time2=time_struct(time)
    Cyear=strtrim(string(time2.year),1)
    Cyear2=strmid(strtrim(string(time2.year),1),2,2)
    Cmonth=strtrim(string(time2.month),1)
    if time2.month le 9 then Cmonth='0'+Cmonth
    Cday=strtrim(string(time2.date),1)
    if time2.date le 9 then Cday='0'+Cday
    result=file_search(getenv('BIG_DIR')+'/big/GROUND/madrigal/dms_'+Cyear+Cmonth+Cday+'_'+sc+filetype[loop_filetype]+'.*.hdf5')

    file_flag=0
    if n_elements(result) eq 1 then begin
      if result eq '' then file_flag=1
    endif
    filename=result

    if file_flag eq 1 then begin
      print,'NO '+getenv('BIG_DIR')+'/big/GROUND/madrigal/dms_'+Cyear+Cmonth+Cday+'_'+sc+filetype[loop_filetype]+'.*.hdf5'
      goto,endloop
    endif

    print,'Loading '+filename
    file_id=h5f_open(filename)
    id=h5d_open(file_id,'/Data/Table Layout')
    data1=h5d_read(id)

    if keyword_set(data2) then data2=[data2,data1]
    if not keyword_set(data2) then data2=data1
  endfor

  ;store_data,'dmsp'+sc+'_'+filetype[loop_filetype],data=data2,dlim=site_info
  time=time_double(strtrim(string(data2.year),1)+'-'+strtrim(string(data2.month),1)+'-'+strtrim(string(data2.day),1)+'/'+strtrim(string(data2.hour),1)+':'+strtrim(string(data2.min),1)+':'+strtrim(string(data2.sec),1))
  if filetype[loop_filetype] eq 's1' then begin
    store_data,'dmsp'+sc+'_ssies_dm_glat',data={x:time,y:reform(data2.gdlat)}
    store_data,'dmsp'+sc+'_ssies_dm_glon',data={x:time,y:reform(data2.glon)}
    store_data,'dmsp'+sc+'_ssies_dm_alt',data={x:time,y:reform(data2.gdalt)}
    store_data,'dmsp'+sc+'_ssies_dm_mlt',data={x:time,y:reform(data2.mlt)},dlim={ytitle:'F'+sc+'_mlt'}
    store_data,'dmsp'+sc+'_ssies_dm_mlat',data={x:time,y:reform(data2.mlat)},dlim={ytitle:'F'+sc+'_mlat'}
    store_data,'dmsp'+sc+'_ssies_dm_mlon',data={x:time,y:reform(data2.mlong)}
    store_data,'dmsp'+sc+'_ssies_sm_density',data={x:time,y:reform(data2._ne)*1e-6},dlim={colors:[0],labels:[''],ytitle:'dmsp'+sc+'!CN',ysubtitle:'[/cm3]',constant:0,labflag:-1}
    store_data,'dmsp'+sc+'_ssies_dm_horizontal',data={x:time,y:reform(data2.hor_ion_v)}
    store_data,'dmsp'+sc+'_ssies_dm_vertical',data={x:time,y:reform(data2.vert_ion_v)}
    store_data,'dmsp'+sc+'_ssies_dm_vel',data={x:time,y:[[data2.hor_ion_v],[data2.vert_ion_v]]},dlim={colors:[2,6],labels:['cross','vert'],ytitle:'dmsp'+sc+'!CV',ysubtitle:'[m/s]',constant:0,labflag:-1}
    store_data,'dmsp'+sc+'_ssm_b_d',data={x:time,y:reform(data2.bd)}
    store_data,'dmsp'+sc+'_ssm_b_forward',data={x:time,y:reform(data2.b_forward)}
    store_data,'dmsp'+sc+'_ssm_b_perp',data={x:time,y:reform(data2.b_perp)}
    store_data,'dmsp'+sc+'_ssm_db_d',data={x:time,y:reform(data2.diff_bd)*1e9}
    store_data,'dmsp'+sc+'_ssm_db_forward',data={x:time,y:reform(data2.diff_b_for)*1e9}
    store_data,'dmsp'+sc+'_ssm_db_perp',data={x:time,y:reform(data2.diff_b_perp)*1e9}
    store_data,'dmsp'+sc+'_ssm_db',data={x:time,y:[[data2.diff_bd],[data2.diff_b_for],[data2.diff_b_perp]]*1e9},dlim={colors:[2,4,6],labels:['Bdown','Bvel','Bazim'],ytitle:'dmsp'+sc+'!Cdelta B',ysubtitle:'[nT]',constant:0,labflag:-1,ylog:0}
    thm_detrend4,'dmsp'+sc+'_ssies_sm_density',1000,10
  endif

  if filetype[loop_filetype] eq 's4' and not keyword_set(ssiesonly) then begin
    ;store_data,'dmsp'+sc+'_glat',data={x:time,y:reform(data2.gdlat)}
    ;store_data,'dmsp'+sc+'_glon',data={x:time,y:reform(data2.glon)}
    ;store_data,'dmsp'+sc+'_alt',data={x:time,y:reform(data2.gdalt)}
    ;store_data,'dmsp'+sc+'_mlt',data={x:time,y:reform(data2.mlt)}
    ;store_data,'dmsp'+sc+'_mlat',data={x:time,y:reform(data2.mlat)}
    ;store_data,'dmsp'+sc+'_mlon',data={x:time,y:reform(data2.mlong2)}
    store_data,'dmsp'+sc+'_ti',data={x:time,y:reform(data2.ti)},dlim={psym:1}
    store_data,'dmsp'+sc+'_te',data={x:time,y:reform(data2.te)},dlim={psym:1}
    store_data,'dmsp'+sc+'_t',data={x:time,y:[[data2.te],[data2.ti]]},dlim={psym:1,colors:[2,6],labels:['Te','Ti'],ytitle:'dmsp'+sc+'!CT',ysubtitle:'[K]',constant:0,labflag:-1}
    store_data,'dmsp'+sc+'_po+',data={x:time,y:reform(data2.po_)}
    tinterpol_mxn,'dmsp'+sc+'_po+','dmsp'+sc+'_ssies_sm_density',newname='dmsp'+sc+'_po+_interp'
    get_data,'dmsp'+sc+'_po+_interp',data=data00
    get_data,'dmsp'+sc+'_ssies_sm_density',data=data01
    data00.y=data00.y*data01.y
    store_data,'dmsp'+sc+'_ssies_sm_density',data={x:data01.x,y:[[data01.y],[data00.y]]},dlim={colors:[0,4],labels:['ion','O+'],ytitle:'dmsp'+sc+'!CN',ysubtitle:'[/cm3]',constant:0,labflag:-1}
    store_data,'dmsp'+sc+'_elepot',data={x:time,y:reform(data2.elepot)}
  endif

  if filetype[loop_filetype] eq 'e' and not keyword_set(ssiesonly) then begin
    ;store_data,'dmsp'+sc+'_glat',data={x:time,y:reform(data2.gdlat)}
    ;store_data,'dmsp'+sc+'_glon',data={x:time,y:reform(data2.glon)}
    ;store_data,'dmsp'+sc+'_alt',data={x:time,y:reform(data2.gdalt)}
    ;store_data,'dmsp'+sc+'_mlt',data={x:time,y:reform(data2.mlt)}
    ;store_data,'dmsp'+sc+'_mlat',data={x:time,y:reform(data2.mlat)}
    ;store_data,'dmsp'+sc+'_mlon',data={x:time,y:reform(data2.mlong2)}
    print,'1'
    energy=data2.ch_energy
    energy=energy[UNIQ(energy, SORT(energy))]
    d_energy=energy*0.
    for eloop=n_elements(energy)-1,1,-1 do d_energy[eloop]=energy[eloop]-energy[eloop-1]
    d_energy[0]=d_energy[1]
    time2=time[uniq(time)]
    eeflux=fltarr(n_elements(time2),n_elements(energy))
    ieflux=fltarr(n_elements(time2),n_elements(energy))
    enflux=fltarr(n_elements(time2),n_elements(energy))
    influx=fltarr(n_elements(time2),n_elements(energy))
    teeflux=fltarr(n_elements(time2))
    tieflux=fltarr(n_elements(time2))
    tenflux=fltarr(n_elements(time2))
    tinflux=fltarr(n_elements(time2))
    for loop_element=0L,n_elements(time)-1 do begin
      if loop_element mod 100000 eq 0 then print,loop_element,n_elements(time)-1
      ;temp=min(abs(time[loop_element]-time2),index1)
      ;temp=min(abs(energy-data2[loop_element].ch_energy),index2)
      index1=long(loop_element/n_elements(energy))
      index2=n_elements(energy)-1 - (loop_element mod n_elements(energy))
      eeflux[index1,index2]=data2[loop_element].el_d_ener
      ieflux[index1,index2]=data2[loop_element].ion_d_ener
      enflux[index1,index2]=data2[loop_element].el_d_ener/data2[loop_element].ch_energy
      influx[index1,index2]=data2[loop_element].ion_d_ener/data2[loop_element].ch_energy
      ;teeflux[index1]=data2[loop_element].el_i_ener
      ;tieflux[index1]=data2[loop_element].ion_i_ener
    endfor
    for loop_element=0L,n_elements(time2)-1 do begin
      teeflux[loop_element]=total(reform(eeflux[loop_element,*])*d_energy,/nan)*2*!pi
      tieflux[loop_element]=total(reform(ieflux[loop_element,*])*d_energy,/nan)*2*!pi
      tenflux[loop_element]=total(reform(enflux[loop_element,*])*d_energy,/nan)*2*!pi
      tinflux[loop_element]=total(reform(influx[loop_element,*])*d_energy,/nan)*2*!pi
    endfor
    ave_e_energy=teeflux/tenflux
    ave_i_energy=tieflux/tinflux
    temp=where(tenflux le 1e4,count)
    if count ge 1 then ave_e_energy[temp]='NaN'
    temp=where(tinflux le 3e3,count)
    if count ge 1 then ave_i_energy[temp]='NaN'
    sigmap  = SQRT(teeflux/6.242e11) * 40.0 * ave_e_energy*1e-3 / (16.0 + (ave_e_energy*1e-3)^2)
    sigmah  = 0.45 * sigmap * (ave_e_energy*1e-3)^(0.85)

    store_data,'dmsp'+sc+'_ssj_eeflux',data={x:time2,y:eeflux,v:energy},dlim={spec:1,ytitle:'dmsp'+sc+'!Cele eflux',ysubtitle:'[eV]',ztitle:'[eV/cm2 s sr eV]'}
    store_data,'dmsp'+sc+'_ssj_ieflux',data={x:time2,y:ieflux,v:energy},dlim={spec:1,ytitle:'dmsp'+sc+'!Cion eflux',ysubtitle:'[eV]',ztitle:'[eV/cm2 s sr eV]'}
    store_data,'dmsp'+sc+'_ssj_teeflux',data={x:time2,y:teeflux}
    store_data,'dmsp'+sc+'_ssj_tieflux',data={x:time2,y:tieflux}
    store_data,'dmsp'+sc+'_ssj_tenflux',data={x:time2,y:tenflux}
    store_data,'dmsp'+sc+'_ssj_tinflux',data={x:time2,y:tinflux}
    store_data,'dmsp'+sc+'_ssj_teeflux_erg',data={x:time2,y:teeflux/6.242e11},dlim={ysubtitle:'[erg/cm2 sec]',spec:0,ylog:1,zlog:1}
    store_data,'dmsp'+sc+'_ssj_tieflux_erg',data={x:time2,y:tieflux/6.242e11},dlim={ysubtitle:'[erg/cm2 sec]',spec:0,ylog:1,zlog:1}
    store_data,'dmsp'+sc+'_ssj_aveee',data={x:time2,y:ave_e_energy},dlim={ysubtitle:'[eV]',spec:0,ylog:1,zlog:1}
    store_data,'dmsp'+sc+'_ssj_aveie',data={x:time2,y:ave_i_energy},dlim={ysubtitle:'[eV]',spec:0,ylog:1,zlog:1}
    store_data,'dmsp'+sc+'_ssj_sigmap',data={x:time2,y:sigmap},dlim={ysubtitle:'[S]',spec:0,ylog:1,zlog:1}
    store_data,'dmsp'+sc+'_ssj_sigmah',data={x:time2,y:sigmah},dlim={ysubtitle:'[S]',spec:0,ylog:1,zlog:1}
  endif

  endloop:
  delvarx,data1,data2
endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if keyword_set(map_from_south) then begin
	;Get the s/c position in AACGM
	get_data,'dmsp'+sc+'_ssies_dm_glat',data=glat
	get_data,'dmsp'+sc+'_ssies_dm_glat',data=glon,dlim=dlim
	index=where(glon.y lt 0,count)
	if(count ge 1) then glon.y(index)=glon.y(index)+360
	get_data,'dmsp'+sc+'_ssies_dm_alt',data=alt
	maglat=glat.y & maglon=glat.y & mlt=glat.y & alt_foot=glat.y & glat_foot=glat.y & glon_foot=glat.y ;Make arrays of the same size
	ts=time_struct(glat.x)
	;if not keyword_set(noaacgm) then begin
	alt=alt.y
		FOR j=0l,N_ELEMENTS(glat.x)-1 DO BEGIN
			if (j mod 20000 eq 0) then print,j,N_ELEMENTS(glat.x)-1
			cnv_aacgm,glat.y[j],glon.y[j],alt[j],lat,lon,r,error
			maglat[j]=lat & maglon[j]=lon
			yrsec=cnvtime(ts[j].year,ts[j].month,ts[j].date,ts[j].hour,ts[j].min,ts[j].sec)
			mlt[j]=calc_mlt(ts[j].year,yrsec,maglon[j])
			alt_foot[j]=350.
			cnv_aacgm, maglat[j], maglon[j], alt_foot[j], glat_foot2,glon_foot2,r,error,/geo
			glat_foot[j]=glat_foot2
			glon_foot[j]=glon_foot2
		ENDFOR
		store_data,'dmsp'+sc+'_aacgmmlat',data={x:glat.x,y:maglat},dl={ytitle:'dmsp'+sc+'_Mlat'}
		store_data,'dmsp'+sc+'_aacgmmlt',data={x:glat.x,y:mlt},dl={ytitle:'dmsp'+sc+'_MLT'}
		store_data,'dmsp'+sc+'_aacgmglat',data={x:glat.x,y:glat_foot},dl={ytitle:'dmsp'+sc+'_GLAT'}
		store_data,'dmsp'+sc+'_aacgmglon',data={x:glat.x,y:glon_foot},dl={ytitle:'dmsp'+sc+'_GLON'}
	;endif
	glt=glon.y/15.+(glat.x mod 86400)/3600. mod 24
	store_data,'dmsp'+sc+'_GEO_GLT',data={x:glat.x,y:glt}, $
		dl={ytitle:'dmsp'+sc+'_R'}
endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if not keyword_set(ssiesonly) then begin
  if not keyword_set(nofac) then mad_dmsp_ssm_fac,scnum=sc

    tinterpol_mxn,'dmsp'+sc+'_pos_igrf_gsm','dmsp'+sc+'_ssies_dm_vel',newname='dmsp'+sc+'_pos_igrf_gsm_interp'
    tinterpol_mxn,'dmsp'+sc+'_ssj_sigmap','dmsp'+sc+'_ssies_dm_vel',newname='dmsp'+sc+'_ssj_sigmap_interp'
    get_data,'dmsp'+sc+'_ssies_dm_vel',data=vel
    get_data,'dmsp'+sc+'_pos_igrf_gsm_interp',data=data
    get_data,'dmsp'+sc+'_ssj_sigmap_interp',data=sigmap
    btot=sqrt(data.y[*,0]^2+data.y[*,1]^2+data.y[*,2]^2)
    efield=vel.y[*,0]*btot*1e-9*1e3;mV/m

    joule=sigmap.y*(efield*1e-3)^2

    store_data,'dmsp'+sc+'_ssies_dm_efield',data={x:data.x,y:efield},dlim={label:'equatorward',labflag:-1,ysubtitle:'[mV/m]',constant:0}
    store_data,'dmsp'+sc+'_ssies_dm_efield_eregion',data={x:data.x,y:efield*1.2},dlim={label:'equatorward',labflag:-1,ysubtitle:'[mV/m]',constant:0}
    store_data,'dmsp'+sc+'_ssies_dm_Joule',data={x:data.x,y:joule*1e3},dlim={ysubtitle:'[mW/m2]'}



  ylim,'dmsp'+sc+'_ssies_sm_density',0,0,1
  ylim,['dmsp'+sc+'_ssj_eeflux'],30,30000,1
  ylim,['dmsp'+sc+'_ssj_ieflux'],30,30000,1
  zlim,['dmsp'+sc+'_ssj_eeflux'],1e5,1e9,1
  zlim,['dmsp'+sc+'_ssj_ieflux'],1e4,1e7,1
  options,['*density','dmsp'+sc+'_ssj_?eflux'],ytickformat='logticks_exp',ztickformat='logticks_exp'
  options,['dmsp'+sc+'_ssm_FACupdown_m'],ytitle='dmsp'+sc+'!CFAC',ysubtitle='[uA/m2]'
  options,['dmsp'+sc+'_ssies_dm_vel','dmsp'+sc+'_t'],yminor=2
  options,'dmsp'+sc+'_ssies_dm_mlt',ytitle='MLT'
  options,'dmsp'+sc+'_ssies_dm_mlat',ytitle='MLAT'

  tplot,['dmsp'+sc+'_ssies_sm_density','dmsp'+sc+'_ssies_dm_vel','dmsp'+sc+'_t','dmsp'+sc+'_ssm_FACupdown_m','dmsp'+sc+'_ssj_eeflux','dmsp'+sc+'_ssj_ieflux','dmsp'+sc+'_ssies_dm_mlat','dmsp'+sc+'_ssies_dm_mlt'],var_label=['dmsp'+sc+'_ssies_dm_mlt','dmsp'+sc+'_ssies_dm_mlat']
endif

end
