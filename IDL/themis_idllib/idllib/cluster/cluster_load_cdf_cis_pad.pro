pro cluster_load_cdf_cis_pad,sc=sc

;set time
get_timespan, t
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
directory=getenv('BIG_DIR')+'/big/SATELLITE/cluster/cdf/'

result=file_search(directory+'C'+sc+'_CP_CIS-HIA_PAD_HS_MAG_IONS_PF/C'+sc+'_*__'+Syear+Smonth+Sday+'_*.cdf')
print,directory+'C'+sc+'_CP_CIS-HIA_PAD_HS_MAG_IONS_PF/C'+sc+'_*__'+Syear+Smonth+Sday+'_*.cdf'
if(n_elements(result) ge 2) then result=result(n_elements(result)-1)
if(result eq '') then return

add_att_to_fast_cdf,result
cdf2tplot,result

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,'Differential_Particle_Flux__C'+sc+'_CP_CIS-HIA_PAD_HS_MAG_IONS_PF',data=data
;nflux -> eflux
for loop=0,n_elements(data.v1)-1 do begin
  for loop2=0,n_elements(data.v2)-1 do begin
    data.y[*,loop2,loop]=data.y[*,loop2,loop]*data.v2[loop2]
  endfor
endfor

store_data,'CL'+sc+'_CIS_HIA_en_eflux_para',data={x:data.x,y:reform(data.y[*,*,0]),v:data.v2},dlim={spec:1}
store_data,'CL'+sc+'_CIS_HIA_en_eflux_perp',data={x:data.x,y:reform(data.y[*,*,7]),v:data.v2},dlim={spec:1}
store_data,'CL'+sc+'_CIS_HIA_en_eflux_anti',data={x:data.x,y:reform(data.y[*,*,15]),v:data.v2},dlim={spec:1}
store_data,'CL'+sc+'_CIS_HIA_en_eflux_omni',data={x:data.x,y:reform(median(data.y[*,*,*],di=3)),v:data.v2},dlim={spec:1}
ylim,'CL'+sc+'_CIS_HIA_en_eflux_*',6,3e4,1
zlim,'CL'+sc+'_CIS_HIA_en_eflux_*',3e6,3e9,1
options,['CL'+sc+'_CIS_HIA_en_eflux_????'],ytickformat='logticks_exp'

erange=where(data.v2 ge 1000 and data.v2 le 10000)
store_data,'CL'+sc+'_CIS_HIA_pad_1000-10000eV',data={x:data.x,y:median(data.y[*,erange,*],di=2),v:data.v1},dlim={spec:1}
erange=where(data.v2 ge 100 and data.v2 le 1000)
store_data,'CL'+sc+'_CIS_HIA_pad_100-1000eV',data={x:data.x,y:median(data.y[*,erange,*],di=2),v:data.v1},dlim={spec:1}
erange=where(data.v2 ge 10 and data.v2 le 100)
store_data,'CL'+sc+'_CIS_HIA_pad_10-100eV',data={x:data.x,y:median(data.y[*,erange,*],di=2),v:data.v1},dlim={spec:1}
erange=where(data.v2 ge 10 and data.v2 le 300)
store_data,'CL'+sc+'_CIS_HIA_pad_10-300eV',data={x:data.x,y:median(data.y[*,erange,*],di=2),v:data.v1},dlim={spec:1}
ylim,'CL'+sc+'_CIS_HIA_pad_*',0,180,0
zlim,'CL'+sc+'_CIS_HIA_pad_*',3e6,3e9,1
options,'CL'+sc+'_CIS_HIA_pad_*',yticks=4,yminor=3
end