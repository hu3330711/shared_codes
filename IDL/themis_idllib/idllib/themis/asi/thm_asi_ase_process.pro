;ast->ewogram

;min                med                 max
;    north midnorth  center   midsouth south    ;wide and narrow
;           centnorth   centsouth             ;narrow only
;|----|----|----|----|----|----|----|----|

pro thm_asi_ase_process,stationname=stationname,full=full,subtract_previous=subtract_previous,aseselect=aseselect,maximum=maximum,southpole_wavelength=southpole_wavelength,skip_pixels=skip_pixels,skip_time=skip_time,mlat_offset=mlat_offset,glat_slice=glat_slice,glon_slice=glon_slice

get_timespan, t
Syear=''
Smonth=''
Sday=''
Shour=''
Smin=''
Ssec=''
cdum=''
;print,time_string(t[0])
reads,time_string(t[0]),Syear,cdum,Smonth,cdum,Sday,cdum,Shour,cdum,Smin,cdum,Ssec,format='(a4,a1,a2,a1,a2,a1,a2,a1,a2,a1,a2)'
iSyear=0
iSmonth=0
iSday=0
iShour=0
iSmin=0
iSsec=0
reads,time_string(t[0]),iSyear,cdum,iSmonth,cdum,iSday,cdum,iShour,cdum,iSmin,cdum,iSsec,format='(a4,a1,i2,a1,i2,a1,i2,a1,i2,a1,i2)'
;print,iSyear,iSmonth,iSday,iShour,iSmin,iSsec

if not keyword_set(skip_pixels) then skip_pixels=1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;read coordinates
;southpole_cotrans_geo_aacgm,/noload


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,'thg_asf_'+stationname,data=data
get_data,'thg_asf_'+stationname+'_mlat',data=mlat
get_data,'thg_asf_'+stationname+'_mlon',data=mlon
mlat=mlat.y
mlon=mlon.y

get_data,'thg_asf_'+stationname+'_glat_center',data=glat
get_data,'thg_asf_'+stationname+'_glon_center',data=glon

;if median(mlon ge 270) ge 1 then begin
;  temp=where(mlon lt 180,count)
;  if count ge 1 then mlon(temp)+=360.
;endif

;star removal
if keyword_set(maximum) then begin
  for loop=0,n_elements(data.x)-1,1 do begin
    temp=reform(data.y(loop,*,*))
    data.y(loop,*,*)=median(temp,3)
  endfor
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;cut - central
if not (keyword_set(full)) then begin
med_mlat=median(mlat(*))+mlat_offset
max_mlat=max(mlat(*),/nan)+mlat_offset
min_mlat=min(mlat(*),/nan)+mlat_offset
endif else begin
med_mlat=median(mlat(*))+mlat_offset
max_mlat=max(mlat(*),/nan)+mlat_offset
min_mlat=min(mlat(*),/nan)+mlat_offset
endelse

if not(keyword_set(full)) then begin
dMLON=1.0;default=0.25 for continuous coverage
endif
if (keyword_set(full)) then begin
dMLON=0.5
endif
inv_dMLON=1/dMLON

num=long(540/dMLON)
yaxis=fltarr(num)
if keyword_set(maximum) then ewogram_suffix='_max'
if not keyword_set(maximum) then ewogram_suffix=''
step=(max_mlat-min_mlat)/10


if not keyword_set(aseselect) then begin
  ewogram_names=['mlon_farnorth','mlon_north','mlon_middlenorth','mlon_centernorth','mlon_center','mlon_centersouth','mlon_middlesouth','mlon_south','mlon_farsouth','mlon_farnorth_narrow','mlon_north_narrow','mlon_middlenorth_narrow','mlon_centernorth_narrow','mlon_center_narrow','mlon_centersouth_narrow','mlon_middlesouth_narrow','mlon_south_narrow','mlon_farsouth_narrow','mlon_north_wide','mlon_center_wide','mlon_south_wide','mlon_all']
  ewogram_mlat=[med_mlat+step*4,med_mlat+step*3,med_mlat+step*2,med_mlat+step*1,med_mlat,med_mlat-step*1,med_mlat-step*2,med_mlat-step*3,med_mlat-step*4,med_mlat+step*4,med_mlat+step*3,med_mlat+step*2,med_mlat+step*1,med_mlat,med_mlat-step*1,med_mlat-step*2,med_mlat-step*3,med_mlat-step*4,med_mlat+step*2,med_mlat,med_mlat-step*2,med_mlat]
  dmlat=[step/2,step/2,step/2,step/2,step/2,step/2,step/2,step/2,step/2,step/4,step/4,step/4,step/4,step/4,step/4,step/4,step/4,step/4,step*10/3,step*10/3,step*10/3,step*4]
endif
if keyword_set(aseselect) then begin
  if aseselect eq 1 then begin
  ewogram_names=['mlon_farnorth','mlon_north','mlon_middlenorth','mlon_centernorth','mlon_center','mlon_centersouth','mlon_middlesouth','mlon_south','mlon_farsouth']
  ewogram_mlat=[med_mlat+step*4,med_mlat+step*3,med_mlat+step*2,med_mlat+step*1,med_mlat,med_mlat-step*1,med_mlat-step*2,med_mlat-step*3,med_mlat-step*4]
  dmlat=[step/2,step/2,step/2,step/2,step/2,step/2,step/2,step/2,step/2]
  endif
  if aseselect eq 2 then begin
  ewogram_names=['mlon_farnorth_narrow','mlon_north_narrow','mlon_middlenorth_narrow','mlon_centernorth_narrow','mlon_center_narrow','mlon_centersouth_narrow','mlon_middlesouth_narrow','mlon_south_narrow','mlon_farsouth_narrow']
  ewogram_mlat=[med_mlat+step*4,med_mlat+step*3,med_mlat+step*2,med_mlat+step*1,med_mlat,med_mlat-step*1,med_mlat-step*2,med_mlat-step*3,med_mlat-step*4]
  dmlat=[step/4,step/4,step/4,step/4,step/4,step/4,step/4,step/4,step/4]
  endif
  if aseselect eq 3 then begin
  ewogram_names=['mlon_north_wide','mlon_center_wide','mlon_south_wide','mlon_all']
  ewogram_mlat=[med_mlat+step*2,med_mlat,med_mlat-step*2,med_mlat]
  ;dmlat=[step*10/3,step*10/3,step*10/3,step*4]
  dmlat=[step*1.5,step*1.5,step*1.5,step*3.5]
  endif
  if aseselect eq 4 then begin
  ewogram_names=['mlon_middlenorth2_narrow','mlon_middlenorth1_narrow','mlon_centernorth2_narrow','mlon_centernorth1_narrow','mlon_center_narrow','mlon_centersouth1_narrow','mlon_centersouth2_narrow','mlon_middlesouth1_narrow','mlon_middlesouth2_narrow']
  ewogram_mlat=[med_mlat+step*2,med_mlat+step*1.5,med_mlat+step*1,med_mlat+step*0.5,med_mlat,med_mlat-step*0.5,med_mlat-step*1,med_mlat-step*1.5,med_mlat-step*2]
  dmlat=[step/4,step/4,step/4,step/4,step/4,step/4,step/4,step/4,step/4]
  endif
  if aseselect eq 5 then begin
  ewogram_names=['mlon_farnorth_double','mlon_north_double','mlon_middlenorth_double','mlon_centernorth_double','mlon_center_double','mlon_centersouth_double','mlon_middlesouth_double','mlon_south_double','mlon_farsouth_double']
  ewogram_mlat=[med_mlat-step*4,med_mlat-step*3,med_mlat-step*2,med_mlat-step*1,med_mlat,med_mlat+step*1,med_mlat+step*2,med_mlat+step*3,med_mlat-step*4]
  dmlat=[step*1.0,step*1.0,step*1.0,step*1.0,step*1.0,step*1.0,step*1.0,step*1.0,step*1.0]
  endif
  if aseselect eq 6 then begin
  ewogram_names=['mlon_north6_verynarrow','mlon_north5_verynarrow','mlon_north4_verynarrow','mlon_north3_verynarrow','mlon_north2_verynarrow','mlon_north1_verynarrow','mlon_center_verynarrow','mlon_south1_verynarrow','mlon_south2_verynarrow','mlon_south3_verynarrow','mlon_south4_verynarrow','mlon_south5_verynarrow','mlon_south6_verynarrow']
  ewogram_mlat=[med_mlat+step*1.5,med_mlat+step*1.25,med_mlat+step*1,med_mlat+step*0.75,med_mlat+step*0.5,med_mlat+step*0.25,med_mlat,med_mlat-step*0.25,med_mlat-step*0.5,med_mlat-step*0.75,med_mlat-step*1,med_mlat-step*1.25,med_mlat-step*1.5]
  dmlat=[step/8,step/8,step/8,step/8,step/8,step/8,step/8,step/8,step/8,step/8,step/8,step/8,step/8]
  endif
  if aseselect eq 7 then begin
  ewogram_names=['mlon_north6_verynarrow2','mlon_north5_verynarrow2','mlon_north4_verynarrow2','mlon_north3_verynarrow2','mlon_north2_verynarrow2','mlon_north1_verynarrow2','mlon_center_verynarrow2','mlon_south1_verynarrow2','mlon_south2_verynarrow2','mlon_south3_verynarrow2','mlon_south4_verynarrow2','mlon_south5_verynarrow2','mlon_south6_verynarrow2']
  ewogram_mlat=[med_mlat+step*0.75,med_mlat+step*0.625,med_mlat+step*0.5,med_mlat+step*0.375,med_mlat+step*0.25,med_mlat+step*0.125,med_mlat,med_mlat-step*0.125,med_mlat-step*0.25,med_mlat-step*0.375,med_mlat-step*0.5,med_mlat-step*0.625,med_mlat-step*0.75]
  dmlat=[step/16,step/16,step/16,step/16,step/16,step/16,step/16,step/16,step/16,step/16,step/16,step/16,step/16]
  endif
endif

glat_slice=ptrarr(n_elements(ewogram_names),/allocate_heap)
glon_slice=ptrarr(n_elements(ewogram_names),/allocate_heap)

ewogram_time=data.x(0:n_elements(data.x)-1:skip_time)
for ii=0,n_elements(ewogram_names)-1,1 do begin
  ewogramtime_offset=0
  if n_elements(data.x) mod skip_time ne 0 then ewogramtime_offset=1
  ewogram=fltarr(n_elements(data.x)/skip_time+ewogramtime_offset,num)
  for timeloop=0,n_elements(data.x)-1,skip_time do begin
    arraynum=where(abs(mlat(*)-ewogram_mlat[ii]) lt dmlat[ii] and mlat(*) ge -90 and mlat(*) le 90,count_temp)
    data_temp2=reform(data.y(timeloop,*,*))
    data_temp=data_temp2(*)
    (*glat_slice(ii))=glat.y(arraynum)
    (*glon_slice(ii))=glon.y(arraynum)
    
if timeloop eq 0 then print,ewogram_names(ii),ewogram_mlat(ii),dmlat(ii),count_temp
    ;average
    if not keyword_set(maximum) then begin
      count=intarr(num)
      for loop=0,n_elements(arraynum)-1,1 do begin
        BINnum=long((mlon(arraynum(loop))+180)*inv_dMLON)
        if(data_temp(arraynum(loop)) le 50000) then begin
          ewogram(timeloop/skip_time,BINnum)+=data_temp(arraynum(loop))
          count(BINnum)+=1
        endif
      endfor
      for BINnum=0,num-1,1 do begin
        if count(BINnum) ge 1 then ewogram(timeloop/skip_time,BINnum)=ewogram(timeloop/skip_time,BINnum)/count(BINnum)
      endfor
    endif

    ;max
    if keyword_set(maximum) then begin
      loop=0
      loop=long64(loop)
      while(loop le n_elements(arraynum)-1) do begin
        BINnum=long((mlon(arraynum(loop))+180)*inv_dMLON)
        if(ewogram(timeloop/skip_time,BINnum) le data_temp(arraynum(loop)) and data_temp(arraynum(loop)) le 50000) then ewogram(timeloop/skip_time,BINnum)=data_temp(arraynum(loop))
        loop=loop+1
      endwhile
    endif
  endfor;timeloop

  BINnum=0
  BINnum=long64(BINnum)
  while(BINnum le n_elements(ewogram(0,*))-1) do begin
  yaxis(BINnum)=BINnum*dMLON-180
  BINnum=BINnum+1
  endwhile

  temp=where(min(abs(ewogram),dimension=1) ge 1e-30,count)
  store_data,'thg_ase_'+stationname+'_'+ewogram_names[ii]+ewogram_suffix,data={x:ewogram_time,y:ewogram(*,temp),v:yaxis(temp),mlat:ewogram_mlat[ii],dmlat:dmlat[ii],mlat_offset:mlat_offset},dlim={spec:1}
  ;thm_median_timevalue,'southpole_ase_'+ewogram_names[ii]+ewogram_suffix,3
endfor

;stop
end
