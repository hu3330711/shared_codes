; +
; Joint ESA + SST moments calculations
; Version 01
; The code is partly written by Pat Cruce.
; Level 1 SST and ESA data are used
; ==================================================================

pro joint_mom_sst_mom9,sc=sc,mode_esa=mode_esa,mode_sst=mode_sst

get_timespan,t
t_temp=t
t_temp[0]=t[0]-(t[1]-t[0])
t_temp[1]=t[1]+(t[1]-t[0])

;...load all data over the requested interval

thm_load_state,probe=sc,coord='gsm',/get_support,trange=[t[0]-180.,t[1]+180.]

mode_esa='m'
mode_sst='m'

;
; SST now
;sun_bins = dblarr(64)+1
;sun_bins[[30,39,40,46,55,56,61,62]] = 0  ;if used, removes and fills bins 30,39,40,46,55,56,61,62
;thm_load_sst2,probe=sc, trange=[t[0]-180.,t[1]+180.]
;thm_part_getspec, probe=sc, $
;                  theta=[-45,45], phi=[0,450], $
;                  data_type=['psi'+mode_sst,'pse'+mode_sst], angle='phi',suffix='',$
;                  /energy, trange=[t[0]-180.,t[1]+180.],method_clean='automatic'
;thm_part_load,probe=sc,datatype='pse'+mode_sst
;thm_part_products,probe=sc,datatype='pse'+mode_sst,trange=[t[0]-180.,t[1]+180.],outputs='moments',energy=[1e4,1e5]
;thm_part_load,probe=sc,datatype='psi'+mode_sst
;thm_part_products,probe=sc,datatype='psi'+mode_sst,trange=[t[0]-180.,t[1]+180.],outputs='moments',energy=[1e4,1e5]
;thm_part_getspec, probe=sc, $
;                  theta=[-45,45], phi=[0,450], $
;                  data_type=['psi'+mode_sst,'pse'+mode_sst], angle='phi',suffix='_new',$
;                  /energy,method_clean='automatic'


; work in gsm
thm_cotrans,'th'+sc+'_ps?'+mode_sst+'_velocity',in_coord='dsl',out_coord='gsm',out_suffix='_gsm'
get_data,'th'+sc+'_psi'+mode_sst+'_velocity',data=data
store_data,'th'+sc+'_psi'+mode_sst+'_velocity',data=data,dlimit={colors:[2,4,6],labels:['Vx','Vy','Vz'],ysubtitle:'[km/s]',labflag:-1,constant:[0]}
;
; ESA now
thm_load_esa,probe=sc,level = 2, datatype = 'p??'+mode_esa+'_en_eflux', trange=[t[0]-180.,t[1]+180.]
thm_load_mom, probe = sc, level = 1, trange=[t[0]-180.,t[1]+180.]
thm_cotrans,'th'+sc+'_pe?m_velocity',in_coord='dsl',out_coord='gsm',out_suffix='_gsm'
get_data,'th'+sc+'_pxxm_pot',data=data,dlim=dlim
data.y=-data.y
store_data,'th'+sc+'_pxxm_-scpot',data=data,dlim=dlim


  ;... degap all data
  ;this puts nans in the right spots so that it is easier to interpolate
  ;otherwise interpolation might generate data inside gaps
  ;density
;   tdegap,'th'+sc+'_pe?'+mode_esa+'_density',/overwrite,margin=1.5
;   tdegap,'th'+sc+'_ps?'+mode_sst+'_density',/overwrite,margin=150
  ;temperature
;   tdegap,'th'+sc+'_pe?'+mode_esa+'_t3',/overwrite,margin=1.5
;   tdegap,'th'+sc+'_ps?'+mode_sst+'_t3',/overwrite,margin=150
  ;velocity
;   tdegap,'th'+sc+'_pe?'+mode_esa+'_velocity',/overwrite,margin=1.5
;   tdegap,'th'+sc+'_ps?'+mode_sst+'_velocity',/overwrite,margin=150


; ...now interpolate
; ...matching to esa cadence

get_data,'th'+sc+'_psi'+mode_sst+'_density',data=data
str_element,data,'x',success=success
;density
 tinterpol_mxn,'th'+sc+'_pe?m_density','th'+sc+'_peim_density',/overwrite,/nan_extrapolate
if success eq 1 then tinterpol_mxn,'th'+sc+'_ps?'+mode_sst+'_density','th'+sc+'_peim_density',/overwrite,/nan_extrapolate
;velocity
 tinterpol_mxn,'th'+sc+'_pe?m_velocity_gsm','th'+sc+'_peim_density',/overwrite,/nan_extrapolate
if success eq 1 then tinterpol_mxn,'th'+sc+'_ps?'+mode_sst+'_velocity','th'+sc+'_peim_density',/overwrite,/nan_extrapolate
;
; --------------------------------------------------------------------------------
; ... Joint moments calculation
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data, 'th'+sc+'_psi'+mode_sst+'_density', data=sst_i_n ; sst ion density
get_data, 'th'+sc+'_psi'+mode_sst+'_velocity', data=sst_i_v ; ion ion velocity (GSM)

get_data, 'th'+sc+'_pse'+mode_sst+'_density', data=sst_e_n ; sst electron density
get_data, 'th'+sc+'_pse'+mode_sst+'_velocity', data=sst_e_v ; sst electron velocity (GSM)

get_data, 'th'+sc+'_peim_density', data=esa_i_n ; esa ion density
get_data, 'th'+sc+'_peim_velocity_gsm', data=esa_i_V ; ion velocity (GSM)

get_data, 'th'+sc+'_peem_density', data=esa_e_n ; esa electron density
get_data, 'th'+sc+'_peem_velocity_gsm', data=esa_e_V ; electron velocity (GSM)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ...sst ion Flux
sstFi = sst_i_v.y*0.
sstFi[*,0] = sst_i_n.y*sst_i_v.y[*,0]
sstFi[*,1] = sst_i_n.y*sst_i_v.y[*,1]
sstFi[*,2] = sst_i_n.y*sst_i_v.y[*,2]

; ...esa ion Flux
esaFi = esa_i_v.y*0.
esaFi[*,0] = esa_i_n.y*esa_i_v.y[*,0]
esaFi[*,1] = esa_i_n.y*esa_i_v.y[*,1]
esaFi[*,2] = esa_i_n.y*esa_i_v.y[*,2]

; ...total ion density
totNi = sst_i_n.y + esa_i_n.y

store_data, 'th'+sc+'_pxi'+mode_esa+'_density', data={x:esa_i_n.x, y:totNi},dlimit={colors:[0],labels:['Ni'],ysubtitle:'[/cm3]',labflag:-1,constant:[0]}
ylim, 'th'+sc+'_pxi'+mode_esa+'_density', 0.01, 1., 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ...sst electron Flux
str_element,sst_e_v,'y',success=success_sst_e_v
if(success_sst_e_v ge 1) then begin
sstFe = sst_e_v.y*0.
sstFe[*,0] = sst_e_n.y*sst_e_v.y[*,0]
sstFe[*,1] = sst_e_n.y*sst_e_v.y[*,1]
sstFe[*,2] = sst_e_n.y*sst_e_v.y[*,2]

; ...esa electron Flux
esaFe = esa_e_v.y*0.
esaFe[*,0] = esa_e_n.y*esa_e_v.y[*,0]
esaFe[*,1] = esa_e_n.y*esa_e_v.y[*,1]
esaFe[*,2] = esa_e_n.y*esa_e_v.y[*,2]

; ...total electron density
totNe = sst_e_n.y + esa_e_n.y

store_data, 'th'+sc+'_pxe'+mode_esa+'_density', data={x:esa_e_n.x, y:totNe},dlimit={colors:[0],labels:['Ne'],ysubtitle:'[/cm3]',labflag:-1,constant:[0]}
ylim, 'th'+sc+'_pxe'+mode_esa+'_density', 0.01, 1., 1
endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ...total ion velocity (GSM)
totVi = esa_i_v.y*0.
totVi[*,0] = (sstFi[*,0]+esaFi[*,0])/totNi
totVi[*,1] = (sstFi[*,1]+esaFi[*,1])/totNi
totVi[*,2] = (sstFi[*,2]+esaFi[*,2])/totNi

store_data, 'th'+sc+'_pxi'+mode_esa+'_velocity', data={x:esa_i_n.x, y:totVi},dlimit={colors:[2,4,6],labels:['Vx','Vy','Vz'],ysubtitle:'[km/s]',labflag:-1,constant:[0]}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ...total electron velocity (GSM)
if(success_sst_e_v ge 1) then begin
totVe = esa_e_v.y*0.
totVe[*,0] = (sstFe[*,0]+esaFe[*,0])/totNe
totVe[*,1] = (sstFe[*,1]+esaFe[*,1])/totNe
totVe[*,2] = (sstFe[*,2]+esaFe[*,2])/totNe

store_data, 'th'+sc+'_pxe'+mode_esa+'_velocity', data={x:esa_e_n.x, y:totVe},dlimit={colors:[2,4,6],labels:['Vx','Vy','Vz'],ysubtitle:'[km/s]',labflag:-1,constant:[0]}
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;read V and B-peir
thm_load_fgm,lev=2,probe=[sc],datatype='fgs',coord='gsm'
tinterpol_mxn,'th'+sc+'_fgs_gsm','th'+sc+'_peim_velocity_gsm',newname='th'+sc+'_fgs_gsm_int'
get_data,strjoin('th'+sc+'_peim_velocity_gsm'),data=Vdata,index=index
get_data,strjoin('th'+sc+'_fgs_gsm_int'),data=Bdata

;Vperp
Btotdata=sqrt(Bdata.y(*,0)^2+Bdata.y(*,1)^2+Bdata.y(*,2)^2)
Bunit=Bdata.y
for i=0,2 do Bunit(*,i)=Bdata.y(*,i)/Btotdata(*)
tmp1=crossp2(Vdata.y,Bunit)
Vperp=crossp2(Bunit,tmp1)
Vpara=dotp2(Bunit,Vdata.y)

store_data,strjoin('th'+sc+'_peim_vperp_gsm'),data={x:Vdata.x,y:Vperp},dlim={colors:[2,4,6],labels:['Vx','Vy','Vz'],ysubtitle:'[km/s]'}
store_data,strjoin('th'+sc+'_peim_vpara_gsm'),data={x:Vdata.x,y:Vpara},dlim={colors:[0],labels:['V||'],ysubtitle:'[km/s]',constant:[0]}

;E=-VxB
Edata=crossp2(Bdata.y,Vperp)*1e-3
store_data,strjoin('th'+sc+'_peim_Evxb'),data={x:Vdata.x,y:Edata},dlim={colors:[2,4,6],labels:['Ex','Ey','Ez'],ysubtitle:'[mV/m]'}
print,'th'+sc+'_peim_vperp_gsm'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;read V and B-peer
;thm_load_fgm,lev=2,probe=[sc],datatype='fgs',coord='gsm'
get_data,strjoin('th'+sc+'_peem_velocity_gsm'),data=Vdata,index=index
get_data,strjoin('th'+sc+'_fgs_gsm_int'),data=Bdata

;Vperp
Btotdata=sqrt(Bdata.y(*,0)^2+Bdata.y(*,1)^2+Bdata.y(*,2)^2)
Bunit=Bdata.y
for i=0,2 do Bunit(*,i)=Bdata.y(*,i)/Btotdata(*)
tmp1=crossp2(Vdata.y,Bunit)
Vperp=crossp2(Bunit,tmp1)
Vpara=dotp2(Bunit,Vdata.y)

store_data,strjoin('th'+sc+'_peem_vperp_gsm'),data={x:Vdata.x,y:Vperp},dlim={colors:[2,4,6],labels:['Vx','Vy','Vz'],ysubtitle:'[km/s]'}
store_data,strjoin('th'+sc+'_peem_vpara_gsm'),data={x:Vdata.x,y:Vpara},dlim={colors:[0],labels:['V||'],ysubtitle:'[km/s]',constant:[0]}

;E=-VxB
Edata=crossp2(Bdata.y,Vperp)*1e-3
store_data,strjoin('th'+sc+'_peem_Evxb'),data={x:Vdata.x,y:Edata},dlim={colors:[2,4,6],labels:['Ex','Ey','Ez'],ysubtitle:'[mV/m]'}
print,'th'+sc+'_peem_vperp_gsm'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;read V and B-peir
thm_load_fgm,lev=2,probe=[sc],datatype='fgs',coord='gsm'
tinterpol_mxn,'th'+sc+'_fgs_gsm','th'+sc+'_pxi'+mode_esa+'_velocity',newname='th'+sc+'_fgs_gsm_int'
get_data,strjoin('th'+sc+'_pxi'+mode_esa+'_velocity'),data=Vdata,index=index
get_data,strjoin('th'+sc+'_fgs_gsm_int'),data=Bdata

;Vperp
Btotdata=sqrt(Bdata.y(*,0)^2+Bdata.y(*,1)^2+Bdata.y(*,2)^2)
Bunit=Bdata.y
for i=0,2 do Bunit(*,i)=Bdata.y(*,i)/Btotdata(*)
tmp1=crossp2(Vdata.y,Bunit)
Vperp=crossp2(Bunit,tmp1)
Vpara=dotp2(Bunit,Vdata.y)

store_data,strjoin('th'+sc+'_pxi'+mode_esa+'_vperp_gsm'),data={x:Vdata.x,y:Vperp},dlim={colors:[2,4,6],labels:['Vx','Vy','Vz'],ysubtitle:'[km/s]',constant:[0]}
store_data,strjoin('th'+sc+'_pxi'+mode_esa+'_vpara_gsm'),data={x:Vdata.x,y:Vpara},dlim={colors:[0],labels:['V||'],ysubtitle:'[km/s]',constant:[0]}

;E=-VxB
Edata=crossp2(Bdata.y,Vperp)*1e-3
store_data,strjoin('th'+sc+'_pxi'+mode_esa+'_Evxb'),data={x:Vdata.x,y:Edata},dlim={colors:[2,4,6],labels:['Ex','Ey','Ez'],ysubtitle:'[mV/m]',constant:[0]}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;read V and B-peer
;thm_load_fgm,lev=2,probe=[sc],datatype='fgs',coord='gsm'
if(success_sst_e_v ge 1) then begin
get_data,strjoin('th'+sc+'_pxe'+mode_esa+'_velocity'),data=Vdata,index=index
get_data,strjoin('th'+sc+'_fgs_gsm_int'),data=Bdata

;Vperp
Btotdata=sqrt(Bdata.y(*,0)^2+Bdata.y(*,1)^2+Bdata.y(*,2)^2)
Bunit=Bdata.y
for i=0,2 do Bunit(*,i)=Bdata.y(*,i)/Btotdata(*)
tmp1=crossp2(Vdata.y,Bunit)
Vperp=crossp2(Bunit,tmp1)
Vpara=dotp2(Bunit,Vdata.y)

store_data,strjoin('th'+sc+'_pxe'+mode_esa+'_vperp_gsm'),data={x:Vdata.x,y:Vperp},dlim={colors:[2,4,6],labels:['Vx','Vy','Vz'],ysubtitle:'[km/s]',constant:[0]}
store_data,strjoin('th'+sc+'_pxe'+mode_esa+'_vpara_gsm'),data={x:Vdata.x,y:Vpara},dlim={colors:[0],labels:['V||'],ysubtitle:'[km/s]',constant:[0]}

;E=-VxB
Edata=crossp2(Bdata.y,Vperp)*1e-3
store_data,strjoin('th'+sc+'_pxe'+mode_esa+'_Evxb'),data={x:Vdata.x,y:Edata},dlim={colors:[2,4,6],labels:['Ex','Ey','Ez'],ysubtitle:'[mV/m]',constant:[0]}
endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Pall
get_data,strjoin('th'+sc+'_peim_ptens'),data=Pidata,dlimit=dl_str
Pidata.y=Pidata.y*1.6e-19*1e6*1e9
store_data,strjoin('th'+sc+'_pei'+mode_esa+'_p'),data={x:Pidata.x,y:[[Pidata.y(*,0)]]},dlim={colors:[6],labels:[ 'Pi'],ysubtitle:'[nPa]',labflag:2}

get_data,'th'+sc+'_peim_velocity_gsm',data=Vdata
get_data,'th'+sc+'_fgs_gsm',data=Bdata,index=index
get_data,'th'+sc+'_peim_density',data=Ndata

if(index ne 0) then begin
Pthdata_x=Pidata.x
Pthdata=Pidata.y(*,0)
Btotdata=sqrt(Bdata.y(*,0)^2+Bdata.y(*,1)^2+Bdata.y(*,2)^2)
Pmagdata=(Btotdata^2/(2*4*!pi*1e-7))*1e-9
Pdyndata=Ndata.y(*)
Pdyndata=1.670e-27*Ndata.y(*)*(Vdata.y(*,0)^2+Vdata.y(*,1)^2+Vdata.y(*,2)^2)*1e21

Pmagdata_interp=interp(Pmagdata(*),Bdata.x,Pthdata_x)

Pdata=fltarr(n_elements(Pthdata_x),3)
Pdata(*,0)=Pthdata(*)
Pdata(*,1)=Pmagdata_interp(*)
Pdata(*,2)=Pdyndata(*)
store_data,'th'+sc+'_Pall',data={x:Pthdata_x,y:Pdata},dlim={colors:[2,4,6],labels:['Pth','Pmag','Pdyn'],ysubtitle:'[nPa]',labflag:-1,constant:[0],ylog:1}
store_data,'th'+sc+'_Ti',data={x:Pthdata_x,y:Pdata*1e-9/(Ndata.y*1e6)/1.60217662e-19},dlim={colors:[2,4,6],labels:['Pth','Pmag','Pdyn'],ysubtitle:'[eV]',labflag:-1,constant:[0],ylog:1}
endif

;Pall
get_data,'th'+sc+'_Pall',data=data,index=index
tinterpol_mxn,'th'+sc+'_pti'+mode_esa+'_ptot','th'+sc+'_Pall',newname='th'+sc+'_pti'+mode_esa+'_ptot_int'
get_data,'th'+sc+'_pti'+mode_esa+'_ptot_int',data=psif
psif.y=psif.y*1.6e-19*1e6*1e9
if(index ne 0) then begin
Pdata=fltarr(n_elements(data.x),4)
Pdata(*,0)=psif.y(*)
Pdata(*,1)=data.y(*,1)
Pdata(*,2)=data.y(*,2)
Pdata(*,3)=Pdata(*,0)+Pdata(*,1)+Pdata(*,2)
store_data,'th'+sc+'_Pall_withsst',data={x:data.x,y:Pdata},dlim={colors:[2,4,6,0],labels:['Pth','Pmag','Pdyn','Ptot'],ysubtitle:'[nPa]',labflag:1,constant:[0],ylog:1}
ylim,'th'+sc+'_Pall_withsst',0.01,10,1
endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Pall
get_data,strjoin('th'+sc+'_peem_ptens'),data=Pidata,dlimit=dl_str
Pidata.y=Pidata.y*1.6e-19*1e6*1e9
store_data,strjoin('th'+sc+'_pei'+mode_esa+'_p'),data={x:Pidata.x,y:[[Pidata.y(*,0)]]},dlim={colors:[6],labels:[ 'Pi'],ysubtitle:'[nPa]',labflag:2}

get_data,'th'+sc+'_peem_velocity_gsm',data=Vdata
get_data,'th'+sc+'_fgs_gsm',data=Bdata,index=index
get_data,'th'+sc+'_peem_density',data=Ndata

if(index ne 0) then begin
Pthdata_x=Pidata.x
Pthdata=Pidata.y(*,0)
Btotdata=sqrt(Bdata.y(*,0)^2+Bdata.y(*,1)^2+Bdata.y(*,2)^2)
Pmagdata=(Btotdata^2/(2*4*!pi*1e-7))*1e-9
Pdyndata=Ndata.y(*)
Pdyndata=1.670e-27*Ndata.y(*)*(Vdata.y(*,0)^2+Vdata.y(*,1)^2+Vdata.y(*,2)^2)*1e21

Pmagdata_interp=interp(Pmagdata(*),Bdata.x,Pthdata_x)

store_data,'th'+sc+'_Pe',data={x:Pthdata_x,y:Pthdata},dlim={colors:[0],labels:['Pe'],ysubtitle:'[nPa]',labflag:-1,constant:[0],ylog:1}
store_data,'th'+sc+'_Te',data={x:Pthdata_x,y:Pdata*1e-9/(Ndata.y*1e6)/1.60217662e-19},dlim={colors:[2,4,6],labels:['Pth','Pmag','Pdyn'],ysubtitle:'[eV]',labflag:-1,constant:[0],ylog:1}
endif

end


