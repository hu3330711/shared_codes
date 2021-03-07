pro cluster_fgm_fac,sc=sc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;physical constants
common share_constants,qe,qp,me,mp,c,pi,k_B,R_E,mu0,eps0,OMEGA_E,G,M_earth,GM,e_me
constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;set time
get_timespan, t
Syear=''
Smonth=''
Sday=''
Shour=''
Smin=''
Ssec=''
cdum=''
print,time_string(t[0])
reads,time_string(t[0]),Syear,cdum,Smonth,cdum,Sday,cdum,Shour,cdum,Smin,cdum,Ssec,format='(a4,a1,a2,a1,a2,a1,a2,a1,a2,a1,a2)'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
tinterpol_mxn,'C'+sc+'_R','CL'+sc+'_Bsm',newname='C'+sc+'_R_int'
tinterpol_mxn,'C'+sc+'_MLAT','CL'+sc+'_Bsm',newname='C'+sc+'_MLAT_int'
tinterpol_mxn,'C'+sc+'_MLT','CL'+sc+'_Bsm',newname='C'+sc+'_MLT_int'
get_data,'C'+sc+'_R_int',data=R

get_data,'C'+sc+'_MLT_int',data=MLT

get_data,'C'+sc+'_MLAT_int',data=MLAT
MLTm12=(MLT.y-12)*15*!pi/180

SMPOS1=fltarr(n_elements(R.x),3)
SMPOS1(*,0)=R.y*cos(MLAT.y*!pi/180)*cos(MLTm12)
SMPOS1(*,1)=R.y*cos(MLAT.y*!pi/180)*sin(MLTm12)
SMPOS1(*,2)=R.y*sin(MLAT.y*!pi/180)
store_data,'CL'+sc+'_SMPOS1',data={x:R.x,y:SMPOS1},dlim={colors:[2,4,6],labels:['X','Y','Z'],ysubtitle:'[RE]',labflag:-1,constant:0}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,'CL'+sc+'_Bsm-T01_s',data=data
store_data,'CL'+sc+'_Bsm-T01_3comp_s',data={x:data.x,y:[[data.y(*,0)],[data.y(*,1)],[data.y(*,2)]]},dlim={colors:[2,4,6],labels:['Bx','By','Bz'],ysubtitle:'[nT]',labflag:-1,constant:0}

get_data,'CL'+sc+'_Bsm-T01_3comp_s',data=BSM
BSMtot=sqrt(BSM.y(*,0)^2+BSM.y(*,1)^2+BSM.y(*,2)^2)

tinterpol_mxn,'CL'+sc+'_SMPOS1','CL'+sc+'_Bsm',/overwrite
get_data,'CL'+sc+'_SMPOS1',data=SMPOS1

;LINDSAY EDIT
;get_data,'c'+sc+'_aux_sc_r_xyz_sm__C1_SP_AUX_bt01_int',data=Bmodel
get_data,'c'+sc+'_aux_sc_r_xyz_sm__C'+sc+'_SP_AUX_bt01_int',data=Bmodel

get_data,'CL'+sc+'_Bsm_pol',data=BSMpol

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Jpara=fltarr(n_elements(BSM.x))
ii=1
ii=long64(ii)
while (ii le n_elements(BSM.x)-1-1) do begin

  if(BSMtot(ii) lt 1e-10 or BSMtot(ii) gt 1000) then continue

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  dB=fltarr(3)
  ds=fltarr(3)
  unit_Bmodel=fltarr(3)
  dB(*)=(BSM.y(ii+1,*)-BSM.y(ii-1,*))/2
  ds(*)=SMPOS1.y(ii+1,*)-SMPOS1.y(ii-1,*)
  Bmodeltot=sqrt(Bmodel.y(ii,0)^2+Bmodel.y(ii,1)^2+Bmodel.y(ii,2)^2)
  unit_Bmodel(*)=Bmodel.y(ii,*)/Bmodeltot

  dBtot=sqrt(dB[0]^2+dB[1]^2+dB[2]^2)
  dstot=sqrt(ds[0]^2+ds[1]^2+ds[2]^2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;db=dBperp
  dBpara=fltarr(3)
  dBpara(*)=unit_Bmodel(*)/Bmodeltot*dotp2(Bmodel.y(ii,*),dB)
  db=dB-dBpara

  dBparatot=sqrt(dBpara(0)^2+dBpara(1)^2+dBpara(2)^2)
  dbtot=sqrt(db(0)^2+db(1)^2+db(2)^2)
  if(dbtot lt 1e-10) then continue

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;de
  unit_e=crossp2(db,Bmodel.y(ii,*))
  unit_etot=sqrt(unit_e(0)^2+unit_e(1)^2+unit_e(2)^2)
  unit_t=db/dbtot
  unit_e=unit_e/unit_etot
  unit_s=ds/dstot
  de=dotp2(ds,unit_e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;alpha
  alpha=acos(dotp2(unit_t,unit_s))*180/!pi
  beta=acos(dotp2(dB,dBpara)/dBtot/dBparatot)*180/!pi
  gamma=acos(dotp2(unit_e,unit_s))*180/!pi

;Jpara
  if(abs(gamma-90) gt 5) then begin
    Jpara(ii)=dbtot/de/mu0/R_E*1e-3;uA/m2
    Jpara(ii)=Jpara(ii)/Bmodeltot*(30000./((1000*1e3+R_E)/R_E)^3*sqrt(1+3*sin(MLAT.y(ii)*!pi/180)^2));map to 1000km
  ;northern hemispshere
  ;if(MLAT.y(ii) gt 0) then Jpara(ii)=-Jpara(ii)
  if(BSMpol.y(ii,0) ge 0) then Jpara(ii)=-Jpara(ii)
  endif
if(BSM.x(ii) gt time_double('2003-03-31/17:05:0') and BSM.x(ii) lt time_double('2003-03-31/17:08:0')) then print,time_string(BSM.x(ii)),dbtot,BSM.y(ii,1),Jpara(ii)
  ii=ii+1
endwhile

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
store_data,'CL'+sc+'_FAC',data={x:BSM.x,y:Jpara},dlim={colors:[0],labels:['FAC'],ysubtitle:'[uA/m2]',labflag:-1,constant:0}
Jpara_up  =fltarr(n_elements(BSM.x))
Jpara_down=fltarr(n_elements(BSM.x))
zero=fltarr(n_elements(BSM.x))
Jpara_up  (where(Jpara gt -1e-10))=Jpara(where(Jpara gt -1e-10))
Jpara_down(where(Jpara lt  1e-10))=Jpara(where(Jpara lt  1e-10))
store_data,'CL'+sc+'_FACup',data={x:BSM.x,y:Jpara_up}  ,dlim={colors:[6],labels:['up'],ysubtitle:'[uA/m2]',labflag:-1,constant:0}
store_data,'CL'+sc+'_FACdown',data={x:BSM.x,y:Jpara_down},dlim={colors:[2],labels:['down'],ysubtitle:'[uA/m2]',labflag:-1,constant:0}
store_data,'0',data={x:BSM.x,y:zero},dlim={colors:[0],labels:[''],ysubtitle:'',labflag:-1,constant:0}
store_data,'CL'+sc+'_FACupdown',data='CL'+sc+'_FACup'+' '+'CL'+sc+'_FACdown'+' '+'0'
thm_median,'CL'+sc+'_FACup',120
thm_median,'CL'+sc+'_FACdown',120
store_data,'CL'+sc+'_FACupdown_m',data='CL'+sc+'_FACup_m'+' '+'CL'+sc+'_FACdown_m'+' '+'0'
ylim,'CL'+sc+'_FACupdown_m',-5,5

end