;ast->keogram

;min                med                 max
;    west midwest  center   mideast east    ;wide and narrow
;           centwest   centeast             ;narrow only
;|----|----|----|----|----|----|----|----|

pro thm_asi_ask_process,stationname=stationname,full=full,subtract_previous=subtract_previous,askselect=askselect,maximum=maximum,southpole_wavelength=southpole_wavelength,skip_pixels=skip_pixels,skip_time=skip_time,mlon_offset=mlon_offset,glat_slice=glat_slice,glon_slice=glon_slice

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

if not keyword_set(skip_pixels) and askselect eq 3 then skip_pixels=2
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
med_mlon=median(mlon(*))+mlon_offset
max_mlon=max(mlon(*),/nan)-5+mlon_offset
min_mlon=min(mlon(*),/nan)+5+mlon_offset
endif else begin
med_mlon=median(mlon(*))+mlon_offset
max_mlon=max(mlon(*),/nan)-5+mlon_offset
min_mlon=min(mlon(*),/nan)+5+mlon_offset
endelse

if not(keyword_set(full)) then begin
dMLAT=0.25;default=0.25 for continuous coverage
endif
if (keyword_set(full)) then begin
dMLAT=0.1
endif
inv_dMLAT=1/dMLAT

num=long(180/dMLAT)
yaxis=fltarr(num)
if keyword_set(maximum) then keogram_suffix='_max'
if not keyword_set(maximum) then keogram_suffix=''
step=(max_mlon-min_mlon)/10


if not keyword_set(askselect) then begin
  keogram_names=['mlat_farwest','mlat_west','mlat_middlewest','mlat_centerwest','mlat_center','mlat_centereast','mlat_middleeast','mlat_east','mlat_fareast','mlat_farwest_narrow','mlat_west_narrow','mlat_middlewest_narrow','mlat_centerwest_narrow','mlat_center_narrow','mlat_centereast_narrow','mlat_middleeast_narrow','mlat_east_narrow','mlat_fareast_narrow','mlat_west_wide','mlat_center_wide','mlat_east_wide','mlat_all']
  keogram_mlon=[med_mlon-step*4,med_mlon-step*3,med_mlon-step*2,med_mlon-step*1,med_mlon,med_mlon+step*1,med_mlon+step*2,med_mlon+step*3,med_mlon+step*4,med_mlon-step*4,med_mlon-step*3,med_mlon-step*2,med_mlon-step*1,med_mlon,med_mlon+step*1,med_mlon+step*2,med_mlon+step*3,med_mlon+step*4,med_mlon-step*2,med_mlon,med_mlon+step*2,med_mlon]
  dmlon=[step/2,step/2,step/2,step/2,step/2,step/2,step/2,step/2,step/2,step/4,step/4,step/4,step/4,step/4,step/4,step/4,step/4,step/4,step*10/3,step*10/3,step*10/3,step*4]
endif
if keyword_set(askselect) then begin
  if askselect eq 1 then begin
  keogram_names=['mlat_farwest','mlat_west','mlat_middlewest','mlat_centerwest','mlat_center','mlat_centereast','mlat_middleeast','mlat_east','mlat_fareast']
  keogram_mlon=[med_mlon-step*4,med_mlon-step*3,med_mlon-step*2,med_mlon-step*1,med_mlon,med_mlon+step*1,med_mlon+step*2,med_mlon+step*3,med_mlon-step*4]
  dmlon=[step/2,step/2,step/2,step/2,step/2,step/2,step/2,step/2,step/2]
  endif
  if askselect eq 2 then begin
  keogram_names=['mlat_farwest_narrow','mlat_west_narrow','mlat_middlewest_narrow','mlat_centerwest_narrow','mlat_center_narrow','mlat_centereast_narrow','mlat_middleeast_narrow','mlat_east_narrow','mlat_fareast_narrow']
  keogram_mlon=[med_mlon-step*4,med_mlon-step*3,med_mlon-step*2,med_mlon-step*1,med_mlon,med_mlon+step*1,med_mlon+step*2,med_mlon+step*3,med_mlon-step*4]
  dmlon=[step/4,step/4,step/4,step/4,step/4,step/4,step/4,step/4,step/4]
  endif
  if askselect eq 3 then begin
  keogram_names=['mlat_center_wide','mlat_all','mlat_west_wide','mlat_east_wide']
  keogram_mlon=[med_mlon,med_mlon,med_mlon-7.5,med_mlon+7.5]
  ;dmlon=[step*10/3,step*10/3,step*10/3,step*4]
  ;dmlon=[step*1.5,step*1.5,step*1.5,step*3.5]
  dmlon=[7.5,15,7.5,7.5]
  endif
  if askselect eq 4 then begin
  keogram_names=['mlat_middlewest2_narrow','mlat_middlewest1_narrow','mlat_centerwest2_narrow','mlat_centerwest1_narrow','mlat_center_narrow','mlat_centereast1_narrow','mlat_centereast2_narrow','mlat_middleeast1_narrow','mlat_middleeast2_narrow']
  keogram_mlon=[med_mlon-step*2,med_mlon-step*1.5,med_mlon-step*1,med_mlon-step*0.5,med_mlon,med_mlon+step*0.5,med_mlon+step*1,med_mlon+step*1.5,med_mlon+step*2]
  dmlon=[step/4,step/4,step/4,step/4,step/4,step/4,step/4,step/4,step/4]
  endif
  if askselect eq 5 then begin
  keogram_names=['mlat_farwest_double','mlat_west_double','mlat_middlewest_double','mlat_centerwest_double','mlat_center_double','mlat_centereast_double','mlat_middleeast_double','mlat_east_double','mlat_fareast_double']
  keogram_mlon=[med_mlon-step*4,med_mlon-step*3,med_mlon-step*2,med_mlon-step*1,med_mlon,med_mlon+step*1,med_mlon+step*2,med_mlon+step*3,med_mlon-step*4]
  dmlon=[step*1.5,step*1.5,step*1.5,step*1.5,step*1.5,step*1.5,step*1.5,step*1.5,step*1.5]
  endif
  if askselect eq 6 then begin
  keogram_names=['mlat_west6_verynarrow','mlat_west5_verynarrow','mlat_west4_verynarrow','mlat_west3_verynarrow','mlat_west2_verynarrow','mlat_west1_verynarrow','mlat_center_verynarrow','mlat_east1_verynarrow','mlat_east2_verynarrow','mlat_east3_verynarrow','mlat_east4_verynarrow','mlat_east5_verynarrow','mlat_east6_verynarrow']
  keogram_mlon=[med_mlon-step*1.5,med_mlon-step*1.25,med_mlon-step*1,med_mlon-step*0.75,med_mlon-step*0.5,med_mlon-step*0.25,med_mlon,med_mlon+step*0.25,med_mlon+step*0.5,med_mlon+step*0.75,med_mlon+step*1,med_mlon+step*1.25,med_mlon+step*1.5]
  dmlon=[step/20,step/20,step/20,step/20,step/20,step/20,step/20,step/20,step/20,step/20,step/20,step/20,step/20]
  endif
endif

glat_slice=ptrarr(n_elements(keogram_names),/allocate_heap)
glon_slice=ptrarr(n_elements(keogram_names),/allocate_heap)

keogram_time=data.x(0:n_elements(data.x)-1:skip_time)
for ii=0,n_elements(keogram_names)-1,1 do begin
  keogramtime_offset=0
  if n_elements(data.x) mod skip_time ne 0 then keogramtime_offset=1
  keogram=fltarr(n_elements(data.x)/skip_time+keogramtime_offset,num)
  for timeloop=0,n_elements(data.x)-1,skip_time do begin
    arraynum=where(abs(mlon(*)-keogram_mlon[ii]) lt dmlon[ii] and mlat(*) ge -90 and mlat(*) le 90,count_temp)
    data_temp2=reform(data.y(timeloop,*,*))
    data_temp=data_temp2(*)
    (*glat_slice(ii))=glat.y(arraynum)
    (*glon_slice(ii))=glon.y(arraynum)
    
if timeloop eq 0 then print,keogram_names(ii),keogram_mlon(ii),dmlon(ii),count_temp
    ;average
    if not keyword_set(maximum) then begin
      count=intarr(num)
      for loop=0,n_elements(arraynum)-1,1 do begin
        BINnum=long((mlat(arraynum(loop))+90)*inv_dMLAT)
        if(data_temp(arraynum(loop)) le 50000) then begin
          keogram(timeloop/skip_time,BINnum)+=data_temp(arraynum(loop))
          count(BINnum)+=1
        endif
      endfor
      for BINnum=0,num-1,1 do begin
        if count(BINnum) ge 1 then keogram(timeloop/skip_time,BINnum)=keogram(timeloop/skip_time,BINnum)/count(BINnum)
      endfor
    endif

    ;max
    if keyword_set(maximum) then begin
      loop=0
      loop=long64(loop)
      while(loop le n_elements(arraynum)-1) do begin
        BINnum=long((mlat(arraynum(loop))+90)*inv_dMLAT)
        if(keogram(timeloop/skip_time,BINnum) le data_temp(arraynum(loop)) and data_temp(arraynum(loop)) le 50000) then keogram(timeloop/skip_time,BINnum)=data_temp(arraynum(loop))
        loop=loop+1
      endwhile
    endif
  endfor;timeloop

  BINnum=0
  BINnum=long64(BINnum)
  while(BINnum le n_elements(keogram(0,*))-1) do begin
  yaxis(BINnum)=BINnum*dMLAT-90
  BINnum=BINnum+1
  endwhile

  temp=where(min(abs(keogram),dimension=1) ge 1e-30,count)
  store_data,'thg_ask_'+stationname+'_'+keogram_names[ii]+keogram_suffix,data={x:keogram_time,y:keogram(*,temp),v:yaxis(temp),mlon:keogram_mlon[ii],dmlon:dmlon[ii],mlon_offset:mlon_offset},dlim={spec:1}
  ;thm_median_timevalue,'southpole_ask_'+keogram_names[ii]+keogram_suffix,3
endfor

;stop
end
