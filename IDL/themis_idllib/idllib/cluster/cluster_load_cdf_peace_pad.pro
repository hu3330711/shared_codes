pro cluster_load_cdf_peace_pad,sc=sc

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

result=file_search(directory+'C'+sc+'_CP_PEA_PITCH_SPIN_DPFlux/C'+sc+'_*__'+Syear+Smonth+Sday+'_*.cdf')
print,directory+'C'+sc+'_CP_PEA_PITCH_SPIN_DPFlux/C'+sc+'_*__'+Syear+Smonth+Sday+'_*.cdf'
if(n_elements(result) ge 2) then result=result(n_elements(result)-1)
if(result eq '') then return

add_att_to_fast_cdf,result
cdf2tplot,result

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,'Data__C'+sc+'_CP_PEA_PITCH_SPIN_DPFlux',data=data,index=index
;nflux -> eflux
for loop=0,n_elements(data.v1)-1 do begin
  data.y[*,*,loop]=data.y[*,*,loop]*data.v2*1e-3
endfor


PA2=data.v1
if(index ne 0) then begin
  where_temp=where(data.y le -0.1,count)
  if (count ge 1) then data.y(where_temp)='NaN'

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;0
  where_temp=min(abs(PA2-0.),index)
  en_eflux=data.y(*,*,index)
  ;en_eflux=transpose(reform(en_eflux))
  store_data,'CL'+sc+'_peace_en_eflux_para',data={x:data.x,y:en_eflux,v:data.v2},dlim={spec:1,ysubtitle:'[eV]',ylog:1,zlog:1}

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;90
  where_temp=min(abs(PA2-90.),index)
  en_eflux=data.y(*,*,index)
  ;en_eflux=transpose(reform(en_eflux))
  store_data,'CL'+sc+'_peace_en_eflux_perp',data={x:data.x,y:en_eflux,v:data.v2},dlim={spec:1,ysubtitle:'[eV]',ylog:1,zlog:1}

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;180
  where_temp=min(abs(PA2-180.),index)
  en_eflux=data.y(*,*,index)
  ;en_eflux=transpose(reform(en_eflux))
  store_data,'CL'+sc+'_peace_en_eflux_anti',data={x:data.x,y:en_eflux,v:data.v2},dlim={spec:1,ysubtitle:'[eV]',ylog:1,zlog:1}

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;omni
  en_eflux=median(data.y(*,*,*),di=3)
  ;en_eflux=transpose(reform(en_eflux))
  store_data,'CL'+sc+'_peace_en_eflux_omni',data={x:data.x,y:en_eflux,v:data.v2},dlim={spec:1,ysubtitle:'[eV]',ylog:1,zlog:1}

  ylim,'CL'+sc+'_peace_en_eflux_????',5,3e4
  zlim,'CL'+sc+'_peace_en_eflux_????',1e9,3e11
  options,['CL'+sc+'_peace_en_eflux_????'],ytickformat='logticks_exp'


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
erange=where(data.v2[0,*] ge 1000 and data.v2[0,*] le 10000)
store_data,'CL'+sc+'_peace_pad_1000-10000eV',data={x:data.x,y:median(data.y[*,erange,*],di=2),v:data.v1},dlim={spec:1}
erange=where(data.v2[0,*] ge 100 and data.v2[0,*] le 1000)
store_data,'CL'+sc+'_peace_pad_100-1000eV',data={x:data.x,y:median(data.y[*,erange,*],di=2),v:data.v1},dlim={spec:1}
erange=where(data.v2[0,*] ge 10 and data.v2[0,*] le 100)
store_data,'CL'+sc+'_peace_pad_10-100eV',data={x:data.x,y:median(data.y[*,erange,*],di=2),v:data.v1},dlim={spec:1}
ylim,'CL'+sc+'_peace_pad_*',0,180,0
zlim,'CL'+sc+'_peace_pad_*',1e9,3e11,1
options,'CL'+sc+'_peace_pad_*',yticks=4,yminor=3,ztitle='[eV/eV cm2 s sr]'

endif

end