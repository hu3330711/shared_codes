pro thm_esa_en_pad_split,sc=sc,datatype=datatype,noload=noload,suffix=suffix,regrid=regrid,emin=emin,emax=emax,normal_threshold=normal_threshold

if not keyword_set(suffix) then suffix=''
if not keyword_set(regrid) then suffix=''
if not keyword_set(emin) then emin=0.0
if not keyword_set(emax) then emax=1000000.0
if not keyword_set(normal_threshold) then normal_threshold=1e7
get_data,'th'+sc+'_'+datatype+'_an_eflux_pa_append'+suffix,data=data
str_element,data,'x',success=success
if success eq 0 then return
print,'th'+sc+'_'+datatype+'_an_eflux_pa_append'+suffix

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pa_list=['000','020','060','090','120','160','180']
pa_v=lonarr(n_elements(pa_list))
for loop_pa=0L,n_elements(pa_list)-1 do begin
  temp=min(abs(data.v[0,*]-pa_list[loop_pa]),index)
  pa_v[loop_pa]=index
endfor
for loop_pa=0L,n_elements(pa_list)-1 do store_data,'th'+sc+'_'+datatype+'_en_eflux_pa'+pa_list[loop_pa]+suffix,data={x:data.x,y:reform(data.y(*,*,pa_v[loop_pa])),v:data.v2},dlim={spec:1}
store_data,'th'+sc+'_'+datatype+'_en_eflux'+suffix,data={x:data.x,y:reform(total(data.y(*,*,*),3,/nan))/n_elements(data.v(*)),v:data.v2}

for loop_pa=0L,n_elements(pa_list)-1 do begin
get_data,'th'+sc+'_'+datatype+'_en_eflux_pa'+pa_list[loop_pa]+suffix,data=data,dlim=dlim,lim=lim
data.y=(data.y>normal_threshold)
data2=data.y
for loop=0,n_elements(data.y(0,*))-1 do begin
  data2(*,loop)=(data.y(*,loop)-min(data.y(*,loop),/nan))/(max(data.y(*,loop),/nan)-min(data.y(*,loop),/nan))*100
endfor
if ndimen(data.v) eq 3 then str_element,data,'v',reform(data.v(*,*,0)),/add_replace
store_data,'th'+sc+'_'+datatype+'_en_eflux_pa'+pa_list[loop_pa]+'_normal'+suffix,data={x:data.x,y:data2,v:data.v},dlim=dlim,lim=lim
endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,'th'+sc+'_'+datatype+'_en_eflux_pa000'+suffix,data=data,dlim=dlim,lim=lim
if ndimen(data.v) eq 3 then str_element,data,'v',reform(data.v(*,*,0)),/add_replace
for loop=0,n_elements(data.y(0,*))-1 do begin
  if(strtrim(string(data.v(loop)),1) eq 'NaN' or strtrim(string(data.v(loop)),1) eq '-NaN')  then continue
  if ndimen(data.v) eq 1 then data.y(*,loop)*=data.v(loop)^1
  if ndimen(data.v) eq 2 then data.y(*,loop)*=data.v(*,loop)^1
endfor
store_data,'th'+sc+'_'+datatype+'_en_eeflux_pa000'+suffix,data=data,dlim=dlim,lim=lim

get_data,'th'+sc+'_'+datatype+'_en_eflux_pa090'+suffix,data=data,dlim=dlim,lim=lim
if ndimen(data.v) eq 3 then str_element,data,'v',reform(data.v(*,*,0)),/add_replace
for loop=0,n_elements(data.y(0,*))-1 do begin
  if(strtrim(string(data.v(loop)),1) eq 'NaN' or strtrim(string(data.v(loop)),1) eq '-NaN')  then continue
  if ndimen(data.v) eq 1 then data.y(*,loop)*=data.v(loop)^1
  if ndimen(data.v) eq 2 then data.y(*,loop)*=data.v(*,loop)^1
endfor
store_data,'th'+sc+'_'+datatype+'_en_eeflux_pa090'+suffix,data=data,dlim=dlim,lim=lim

get_data,'th'+sc+'_'+datatype+'_en_eflux_pa180'+suffix,data=data,dlim=dlim,lim=lim
if ndimen(data.v) eq 3 then str_element,data,'v',reform(data.v(*,*,0)),/add_replace
for loop=0,n_elements(data.y(0,*))-1 do begin
  if(strtrim(string(data.v(loop)),1) eq 'NaN' or strtrim(string(data.v(loop)),1) eq '-NaN')  then continue
  if ndimen(data.v) eq 1 then data.y(*,loop)*=data.v(loop)^1
  if ndimen(data.v) eq 2 then data.y(*,loop)*=data.v(*,loop)^1
endfor
store_data,'th'+sc+'_'+datatype+'_en_eeflux_pa180'+suffix,data=data,dlim=dlim,lim=lim

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,'th'+sc+'_'+datatype+'_an_eflux_pa_append',data=data0
get_data,'th'+sc+'_'+datatype+'_an_eflux_pa_append'+suffix,data=data
for i=0,n_elements(data.v2[0,*])-1 do begin
  estring=strsplit(strtrim(string(data0.v2[0,i]),1),'.',/extract)
  if estring[0] lt emin or estring[0] gt emax then continue
  store_data,'th'+sc+'_'+datatype+'_an_eflux_'+estring(0)+suffix,data={x:data.x,y:reform(data.y(*,i,*)),v:data.v},dlim={spec:1}
  min1=min(data.y(*,i,n_elements(data.v(0,*))/2-2:n_elements(data.v(0,*))/2+2),di=3,/nan)
  for loop=0,n_elements(data.y[0,0,*])-1 do data.y[*,i,loop]-=min1
  zlim,'th'+sc+'_'+datatype+'_an_eflux_'+estring(0)+'*'+suffix,min(reform(data.y(*,i,*)),/nan)>7e5,max(reform(data.y(*,i,*)),/nan),1
endfor
zlim,'th'+sc+'_'+datatype+'_an_eflux_?????',0,0,1

get_data,'th'+sc+'_'+datatype+'_an_eflux_pa_append'+suffix,data=data
temp=where(data.v2[0,*] ge 0 and data.v2[0,*] lt 100,count)
store_data,'th'+sc+'_'+datatype+'_an_eflux_0_100eV'+suffix,data={x:data.x,y:reform(median(data.y(*,temp,*),di=2)),v:data.v},dlim={spec:1}
if datatype eq 'peeb' then zlim,'th'+sc+'_'+datatype+'_an_eflux_0_100eV'+suffix,min(median(data.y(*,temp,*),di=2),/nan)>7e5,max(median(data.y(*,temp,*),di=2),/nan),1
if datatype eq 'peib' then zlim,'th'+sc+'_'+datatype+'_an_eflux_0_100eV'+suffix,min(median(data.y(*,temp,*),di=2),/nan)>1e4,max(median(data.y(*,temp,*),di=2),/nan),1
temp=where(data.v2[0,*] ge 100 and data.v2[0,*] lt 1000,count)
store_data,'th'+sc+'_'+datatype+'_an_eflux_100_1000eV'+suffix,data={x:data.x,y:reform(median(data.y(*,temp,*),di=2)),v:data.v},dlim={spec:1}
if datatype eq 'peeb' then zlim,'th'+sc+'_'+datatype+'_an_eflux_100_1000eV'+suffix,min(median(data.y(*,temp,*),di=2),/nan)>7e5,max(median(data.y(*,temp,*),di=2),/nan),1
if datatype eq 'peib' then zlim,'th'+sc+'_'+datatype+'_an_eflux_100_1000eV'+suffix,min(median(data.y(*,temp,*),di=2),/nan)>1e4,max(median(data.y(*,temp,*),di=2),/nan),1
temp=where(data.v2[0,*] ge 1000 and data.v2[0,*] lt 40000,count)
store_data,'th'+sc+'_'+datatype+'_an_eflux_1000_40000eV'+suffix,data={x:data.x,y:reform(median(data.y(*,temp,*),di=2)),v:data.v},dlim={spec:1}
if datatype eq 'peeb' then zlim,'th'+sc+'_'+datatype+'_an_eflux_1000_40000eV'+suffix,min(median(data.y(*,temp,*),di=2),/nan)>7e5,max(median(data.y(*,temp,*),di=2),/nan),1
if datatype eq 'peib' then zlim,'th'+sc+'_'+datatype+'_an_eflux_1000_40000eV'+suffix,min(median(data.y(*,temp,*),di=2),/nan)>1e4,max(median(data.y(*,temp,*),di=2),/nan),1
;timespan,'2013-5-12/10:44',10,/min
temp=where(data.v2[0,*] ge 0 and data.v2[0,*] lt 300,count)
store_data,'th'+sc+'_'+datatype+'_an_eflux_0_300eV'+suffix,data={x:data.x,y:reform(max(data.y(*,temp,*),di=2,/nan)),v:data.v},dlim={spec:1}
if datatype eq 'peeb' then zlim,'th'+sc+'_'+datatype+'_an_eflux_0_300eV'+suffix,min(median(data.y(*,temp,*),di=2),/nan)>7e5,max(median(data.y(*,temp,*),di=2),/nan),1
if datatype eq 'peib' then zlim,'th'+sc+'_'+datatype+'_an_eflux_0_300eV'+suffix,min(median(data.y(*,temp,*),di=2),/nan)>1e4,max(median(data.y(*,temp,*),di=2),/nan),1

;20130512
temp=where(data.v2[0,*] ge 30 and data.v2[0,*] lt 300,count)
store_data,'th'+sc+'_'+datatype+'_an_eflux_30_300eV'+suffix,data={x:data.x,y:reform(max(data.y(*,temp,*),di=2)),v:data.v},dlim={spec:1}
if datatype eq 'peib' then zlim,'th'+sc+'_'+datatype+'_an_eflux_30_300eV'+suffix,min(max(data.y(*,temp,*),di=2),/nan)>1e4,max(max(data.y(*,temp,*),di=2),/nan),1
temp=where(data.v2[0,*] ge 0 and data.v2[0,*] lt 300,count)
store_data,'th'+sc+'_'+datatype+'_an_eflux_0_300eV'+suffix,data={x:data.x,y:reform(max(data.y(*,temp,*),di=2)),v:data.v},dlim={spec:1}
if datatype eq 'peib' then zlim,'th'+sc+'_'+datatype+'_an_eflux_0_300eV'+suffix,min(max(data.y(*,temp,*),di=2),/nan)>1e4,max(max(data.y(*,temp,*),di=2),/nan),1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if strmid(datatype,1,1) eq 'e' then begin
  options,['th'+sc+'_'+datatype+'_en_eflux_pa[0-9]*'],spec=1
  ylim,['th'+sc+'_'+datatype+'_en_eflux_pa[0-9]*'],7,300,1
  zlim,['th'+sc+'_'+datatype+'_en_eflux_pa[0-9]*'],3e5,3e8,1
  options,['th'+sc+'_'+datatype+'_en_eflux'+suffix],spec=1
  ylim,['th'+sc+'_'+datatype+'_en_eflux'+suffix],7,300,1
  zlim,['th'+sc+'_'+datatype+'_en_eflux'+suffix],3e5,3e8,1
  ylim,['th'+sc+'_'+datatype+'_en_eeflux_pa*'],7,300,1
  zlim,'th'+sc+'_'+datatype+'_en_eeflux_pa*',1e7,1e9,1
  options,'th'+sc+'_'+datatype+'_en_eeflux_pa*',spec=1
endif

zlim,'th'+sc+'_'+datatype+'_en_eflux_pa[0-9]*_normal*',20,100,1
ylim,['th'+sc+'_'+datatype+'_an_eflux_*'],0,180,0


get_data,'th'+sc+'_'+datatype+'_an_eflux_7'+suffix,data=data,index=index
if index ne 0 then begin
store_data,'th'+sc+'_'+datatype+'_an_eflux_7_para'+suffix,data={x:data.x,y:reform(total(data.y(*,0:2),2)/3)}
store_data,'th'+sc+'_'+datatype+'_an_eflux_7_anti'+suffix,data={x:data.x,y:reform(total(data.y(*,13:15),2)/3)}
store_data,'th'+sc+'_'+datatype+'_an_eflux_7_both'+suffix,data={x:data.x,y:reform((total(data.y(*,0:2),2)+total(data.y(*,13:15),2))/6)}
get_data,'th'+sc+'_'+datatype+'_an_eflux_9'+suffix,data=data,index=index
endif
if index ne 0 then begin
store_data,'th'+sc+'_'+datatype+'_an_eflux_9_para'+suffix,data={x:data.x,y:reform(total(data.y(*,0:2),2)/3)}
store_data,'th'+sc+'_'+datatype+'_an_eflux_9_anti'+suffix,data={x:data.x,y:reform(total(data.y(*,13:15),2)/3)}
store_data,'th'+sc+'_'+datatype+'_an_eflux_9_both'+suffix,data={x:data.x,y:reform((total(data.y(*,0:2),2)+total(data.y(*,13:15),2))/6)}
get_data,'th'+sc+'_'+datatype+'_an_eflux_12'+suffix,data=data,index=index
endif
if index ne 0 then begin
store_data,'th'+sc+'_'+datatype+'_an_eflux_12_para'+suffix,data={x:data.x,y:reform(total(data.y(*,0:2),2)/3)}
store_data,'th'+sc+'_'+datatype+'_an_eflux_12_anti'+suffix,data={x:data.x,y:reform(total(data.y(*,13:15),2)/3)}
store_data,'th'+sc+'_'+datatype+'_an_eflux_12_both'+suffix,data={x:data.x,y:reform((total(data.y(*,0:2),2)+total(data.y(*,13:15),2))/6)}
endif
get_data,'th'+sc+'_'+datatype+'_an_eflux_15'+suffix,data=data,index=index
if(index eq 0) then get_data,'th'+sc+'_'+datatype+'_an_eflux_16'+suffix,data=data,index=index
if index ne 0 then begin
store_data,'th'+sc+'_'+datatype+'_an_eflux_15_para'+suffix,data={x:data.x,y:reform(total(data.y(*,0:2),2)/3)}
store_data,'th'+sc+'_'+datatype+'_an_eflux_15_anti'+suffix,data={x:data.x,y:reform(total(data.y(*,13:15),2)/3)}
store_data,'th'+sc+'_'+datatype+'_an_eflux_15_both'+suffix,data={x:data.x,y:reform((total(data.y(*,0:2),2)+total(data.y(*,13:15),2))/6)}
endif
get_data,'th'+sc+'_'+datatype+'_an_eflux_20'+suffix,data=data,index=index
if(index eq 0) then get_data,'th'+sc+'_'+datatype+'_an_eflux_21'+suffix,data=data,index=index
if index ne 0 then begin
store_data,'th'+sc+'_'+datatype+'_an_eflux_20_para'+suffix,data={x:data.x,y:reform(total(data.y(*,0:2),2)/3)}
store_data,'th'+sc+'_'+datatype+'_an_eflux_20_anti'+suffix,data={x:data.x,y:reform(total(data.y(*,13:15),2)/3)}
store_data,'th'+sc+'_'+datatype+'_an_eflux_20_both'+suffix,data={x:data.x,y:reform((total(data.y(*,0:2),2)+total(data.y(*,13:15),2))/6)}
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if(0) then begin
get_data,'th'+sc+'_'+datatype+'_an_eflux_7'+suffix,data=data,index=index
if index ne 0 then begin
  store_data,'th'+sc+'_'+datatype+'_an_eflux_7_para_perpsubt'+suffix,data={x:data.x,y:reform(total(data.y(*,0:2),2)/3-min(data.y(*,4:11),di=2,/nan))}
  store_data,'th'+sc+'_'+datatype+'_an_eflux_7_anti_perpsubt'+suffix,data={x:data.x,y:reform(total(data.y(*,13:15),2)/3-min(data.y(*,4:11),di=2,/nan))}
  store_data,'th'+sc+'_'+datatype+'_an_eflux_7_both_perpsubt'+suffix,data={x:data.x,y:reform((total(data.y(*,0:2),2)+total(data.y(*,13:15),2))/6-min(data.y(*,4:11),di=2,/nan))}
endif
get_data,'th'+sc+'_'+datatype+'_an_eflux_9'+suffix,data=data,index=index
if index ne 0 then begin
store_data,'th'+sc+'_'+datatype+'_an_eflux_9_para_perpsubt'+suffix,data={x:data.x,y:reform(total(data.y(*,0:2),2)/3-min(data.y(*,4:11),di=2,/nan))}
store_data,'th'+sc+'_'+datatype+'_an_eflux_9_anti_perpsubt'+suffix,data={x:data.x,y:reform(total(data.y(*,13:15),2)/3-min(data.y(*,4:11),di=2,/nan))}
store_data,'th'+sc+'_'+datatype+'_an_eflux_9_both_perpsubt'+suffix,data={x:data.x,y:reform((total(data.y(*,0:2),2)+total(data.y(*,13:15),2))/6-min(data.y(*,4:11),di=2,/nan))}
endif
get_data,'th'+sc+'_'+datatype+'_an_eflux_12'+suffix,data=data,index=index
if index ne 0 then begin
store_data,'th'+sc+'_'+datatype+'_an_eflux_12_para_perpsubt'+suffix,data={x:data.x,y:reform(total(data.y(*,0:2),2)/3-min(data.y(*,4:11),di=2,/nan))}
store_data,'th'+sc+'_'+datatype+'_an_eflux_12_anti_perpsubt'+suffix,data={x:data.x,y:reform(total(data.y(*,13:15),2)/3-min(data.y(*,4:11),di=2,/nan))}
store_data,'th'+sc+'_'+datatype+'_an_eflux_12_both_perpsubt'+suffix,data={x:data.x,y:reform((total(data.y(*,0:2),2)+total(data.y(*,13:15),2))/6-min(data.y(*,4:11),di=2,/nan))}
endif
get_data,'th'+sc+'_'+datatype+'_an_eflux_15'+suffix,data=data,index=index
if(index eq 0) then get_data,'th'+sc+'_'+datatype+'_an_eflux_16'+suffix,data=data
if index ne 0 then begin
store_data,'th'+sc+'_'+datatype+'_an_eflux_15_para_perpsubt'+suffix,data={x:data.x,y:reform(total(data.y(*,0:2),2)/3-min(data.y(*,4:11),di=2,/nan))}
store_data,'th'+sc+'_'+datatype+'_an_eflux_15_anti_perpsubt'+suffix,data={x:data.x,y:reform(total(data.y(*,13:15),2)/3-min(data.y(*,4:11),di=2,/nan))}
store_data,'th'+sc+'_'+datatype+'_an_eflux_15_both_perpsubt'+suffix,data={x:data.x,y:reform((total(data.y(*,0:2),2)+total(data.y(*,13:15),2))/6-min(data.y(*,4:11),di=2,/nan))}
endif
get_data,'th'+sc+'_'+datatype+'_an_eflux_20'+suffix,data=data,index=index
if(index eq 0) then get_data,'th'+sc+'_'+datatype+'_an_eflux_21'+suffix,data=data
if index ne 0 then begin
store_data,'th'+sc+'_'+datatype+'_an_eflux_20_para_perpsubt'+suffix,data={x:data.x,y:reform(total(data.y(*,0:2),2)/3-min(data.y(*,4:11),di=2,/nan))}
store_data,'th'+sc+'_'+datatype+'_an_eflux_20_anti_perpsubt'+suffix,data={x:data.x,y:reform(total(data.y(*,13:15),2)/3-min(data.y(*,4:11),di=2,/nan))}
store_data,'th'+sc+'_'+datatype+'_an_eflux_20_both_perpsubt'+suffix,data={x:data.x,y:reform((total(data.y(*,0:2),2)+total(data.y(*,13:15),2))/6-min(data.y(*,4:11),di=2,/nan))}
endif
endif

ylim,['th'+sc+'_'+datatype+'_an_eflux_*para*','th'+sc+'_'+datatype+'_an_eflux_*anti*','th'+sc+'_'+datatype+'_an_eflux_*both*'],0,0,0

end
