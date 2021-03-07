pro mad_dmsp_ssm_fac,scnum=scnum

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
copy_data,'dmsp'+scnum+'_ssies_dm_glat','dmsp'+scnum+'_ssm_glat'
copy_data,'dmsp'+scnum+'_ssies_dm_glon','dmsp'+scnum+'_ssm_glon'
copy_data,'dmsp'+scnum+'_ssies_dm_alt','dmsp'+scnum+'_ssm_alt'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,'dmsp'+scnum+'_ssm_alt',data=data
store_data,'dmsp'+scnum+'_ssm_R',data={x:data.x,y:data.y/6372.+1},dlim={colors:[0],labels:['R'],ysubtitle:'[RE]',constant:0,labflag:-1,ylog:0}
get_data,'dmsp'+scnum+'_ssm_glon',data=data
glt=data.y/15.+(data.x mod 86400)/3600. mod 24
store_data,'dmsp'+scnum+'_ssm_glt',data={x:data.x,y:glt},dlim={colors:[0],labels:['GLT'],ysubtitle:'[h]',constant:0,labflag:-1,ylog:0}

get_data,'dmsp'+scnum+'_ssm_R',data=R
get_data,'dmsp'+scnum+'_ssm_glat',data=glat
get_data,'dmsp'+scnum+'_ssm_glon',data=glon
get_data,'dmsp'+scnum+'_ssm_glt',data=glt

t_time=R.x
gltm12=(glt.y-12)*15*!pi/180
GEOPOS1=fltarr(n_elements(R.x),3)
GEOPOS1(*,0)=R.y*cos(glat.y*!pi/180)*cos(glon.y*!pi/180)
GEOPOS1(*,1)=R.y*cos(glat.y*!pi/180)*sin(glon.y*!pi/180)
GEOPOS1(*,2)=R.y*sin(glat.y*!pi/180)
for i=0,2 do GEOPOS1(*,i)=median(GEOPOS1(*,i),180)
store_data,'dmsp'+scnum+'_ssm_pos_geo',data={x:R.x,y:GEOPOS1},dlim={colors:[2,4,6],labels:['X','Y','Z'],ysubtitle:'[RE]',labflag:-1,constant:0}
cotrans,'dmsp'+scnum+'_ssm_pos_geo','dmsp'+scnum+'_ssm_pos_gei',/geo2gei,/ignore_dlimits
cotrans,'dmsp'+scnum+'_ssm_pos_gei','dmsp'+scnum+'_ssm_pos_gse',/gei2gse,/ignore_dlimits
cotrans,'dmsp'+scnum+'_ssm_pos_gse','dmsp'+scnum+'_ssm_pos_gsm',/gse2gsm,/ignore_dlimits

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;dmsp radial
get_data,'dmsp'+scnum+'_ssm_pos_geo',data=geo
geopos1_abs=sqrt(geo.y(*,0)^2+geo.y(*,1)^2+geo.y(*,2)^2)
for loop=0,2 do geo.y(*,loop)=-geo.y(*,loop)/geopos1_abs(*)
store_data,'dmsp'+scnum+'_ssm_pos_unitvec_down_geo',data=geo,dlim={colors:[2,4,6],labels:['X','Y','Z'],ysubtitle:'[]',labflag:-1,constant:0}

;dmsp velocity
dmsp_vel=fltarr(n_elements(t_time),3)
for loop=1,n_elements(t_time)-2,1 do begin
  dmsp_vel(loop,0)=(geo.y(loop+1,0)-geo.y(loop-1,0))/(t_time(loop+1)-t_time(loop-1))/2
  dmsp_vel(loop,1)=(geo.y(loop+1,1)-geo.y(loop-1,1))/(t_time(loop+1)-t_time(loop-1))/2
  dmsp_vel(loop,2)=(geo.y(loop+1,2)-geo.y(loop-1,2))/(t_time(loop+1)-t_time(loop-1))/2
endfor
loop=0
dmsp_vel(loop,0)=dmsp_vel(loop+1,0)
dmsp_vel(loop,1)=dmsp_vel(loop+1,1)
dmsp_vel(loop,2)=dmsp_vel(loop+1,2)
loop=n_elements(t_time)-1
dmsp_vel(loop,0)=dmsp_vel(loop-1,0)
dmsp_vel(loop,1)=dmsp_vel(loop-1,1)
dmsp_vel(loop,2)=dmsp_vel(loop-1,2)
for i=0,2 do dmsp_vel(*,i)=smooth(dmsp_vel(*,i),180,/nan)
;for i=0,2 do dmsp_vel(*,i)=median(dmsp_vel(*,i),240)
store_data,'dmsp'+scnum+'_ssm_vel_geo',data={x:R.x,y:dmsp_vel}
;thm_median,'dmsp'+scnum+'_ssm_vel_geo',10
get_data,'dmsp'+scnum+'_ssm_vel_geo',data=dmsp_vel
dmsp_vel_abs=sqrt(dmsp_vel.y(*,0)^2+dmsp_vel.y(*,1)^2+dmsp_vel.y(*,2)^2)
for loop=0,2 do dmsp_vel.y(*,loop)=-dmsp_vel.y(*,loop)/dmsp_vel_abs(*)
store_data,'dmsp'+scnum+'_ssm_pos_unitvec_vel_geo',data={x:R.x,y:dmsp_vel.y},dlim={colors:[2,4,6],labels:['X','Y','Z'],ysubtitle:'[]',labflag:-1,constant:0}
dmsp_azim=geopos1*0
for loop=0,n_elements(t_time)-1 do dmsp_azim(loop,*)=crossp(dmsp_vel.y(loop,*),geopos1(loop,*))
for i=0,2 do dmsp_azim(*,i)=smooth(dmsp_azim(*,i),180,/nan)
store_data,'dmsp'+scnum+'_ssm_pos_unitvec_azim_geo',data={x:R.x,y:dmsp_azim},dlim={colors:[2,4,6],labels:['X','Y','Z'],ysubtitle:'[]',labflag:-1,constant:0}

;ssm_db -> Bgeo
get_data,'dmsp'+scnum+'_ssm_db',data=Bsat
get_data,'dmsp'+scnum+'_ssm_pos_unitvec_down_geo',data=unitvec_down
get_data,'dmsp'+scnum+'_ssm_pos_unitvec_vel_geo',data=unitvec_vel
get_data,'dmsp'+scnum+'_ssm_pos_unitvec_azim_geo',data=unitvec_azim
Bgeox=Bsat.y(*,0)*unitvec_down.y(*,0)+Bsat.y(*,1)*unitvec_vel.y(*,0)+Bsat.y(*,2)*unitvec_azim.y(*,0)
Bgeoy=Bsat.y(*,0)*unitvec_down.y(*,1)+Bsat.y(*,1)*unitvec_vel.y(*,1)+Bsat.y(*,2)*unitvec_azim.y(*,1)
Bgeoz=Bsat.y(*,0)*unitvec_down.y(*,2)+Bsat.y(*,1)*unitvec_vel.y(*,2)+Bsat.y(*,2)*unitvec_azim.y(*,2)
store_data,'dmsp'+scnum+'_ssm_Bgeo',data={x:Bsat.x,y:[[Bgeox],[Bgeoy],[Bgeoz]]},dlim={colors:[2,4,6],labels:['X','Y','Z'],ysubtitle:'[nT]',labflag:-1,constant:0}

cotrans,'dmsp'+scnum+'_ssm_Bgeo','dmsp'+scnum+'_ssm_Bgei',/geo2gei,/ignore_dlimits
cotrans,'dmsp'+scnum+'_ssm_Bgei','dmsp'+scnum+'_ssm_Bgse',/gei2gse,/ignore_dlimits

;Bgeo->Bnec
get_data,'dmsp'+scnum+'_ssm_Bgeo',data=Bgeo
BGEOX=bgeo.y(*,0)
BGEOY=bgeo.y(*,1)
BGEOZ=bgeo.y(*,2)
BX2= BGEOX*cos(GLON.y*!pi/180)+BGEOY*sin(GLON.y*!pi/180);
BY2=-BGEOX*sin(GLON.y*!pi/180)+BGEOY*cos(GLON.y*!pi/180);
BZ2= BGEOZ
BNORTH =-BX2*sin(GLAT.y*!pi/180)+BZ2*cos(GLAT.y*!pi/180);
BRADIAL= BX2*cos(GLAT.y*!pi/180)+BZ2*sin(GLAT.y*!pi/180);
BAZIMUT= BY2
store_data,'dmsp'+scnum+'_ssm_Bnec',data={x:bgeo.x,y:[[BNORTH],[BAZIMUT],[-BRADIAL]]},dlim={colors:[2,4,6],labels:['Bnorth','Beast','Bcenter'],ysubtitle:'[nT]',labflag:1,constant:0}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
tinterpol_mxn,'dmsp'+scnum+'_ssm_R','dmsp'+scnum+'_ssm_Bnec',newname='dmsp'+scnum+'_ssm_R_int'
tinterpol_mxn,'dmsp'+scnum+'_ssm_glat','dmsp'+scnum+'_ssm_Bnec',newname='dmsp'+scnum+'_ssm_glat_int'
tinterpol_mxn,'dmsp'+scnum+'_ssm_glon','dmsp'+scnum+'_ssm_Bnec',newname='dmsp'+scnum+'_ssm_glon_int'
tinterpol_mxn,'dmsp'+scnum+'_ssm_glt','dmsp'+scnum+'_ssm_Bnec',newname='dmsp'+scnum+'_ssm_glt_int'
get_data,'dmsp'+scnum+'_ssm_R_int',data=R

get_data,'dmsp'+scnum+'_ssm_glt_int',data=glt

get_data,'dmsp'+scnum+'_ssm_glat_int',data=glat
get_data,'dmsp'+scnum+'_ssm_glon_int',data=glon
get_data,'dmsp'+scnum+'_ssm_pos_gse',data=GSEPOS1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;igrf
get_data,'dmsp'+scnum+'_ssm_pos_gsm',data=gsm
igrf_bx=dblarr(n_elements(gsm.x))
igrf_by=dblarr(n_elements(gsm.x))
igrf_bz=dblarr(n_elements(gsm.x))

;GEOPACK INIT
;yymmdd_doy_conv,yymmdd,Sdoy
for loop=0l,n_elements(gsm.x)-1,1 do begin
if (loop mod 10000 eq 0) then print,loop,N_ELEMENTS(glat.x)-1
reads,time_string(gsm.x(loop)),Syear,cdum,Smonth,cdum,Sday,cdum,Shour,cdum,Smin,cdum,Ssec,format='(a4,a1,a2,a1,a2,a1,a2,a1,a2,a1,a2)'
if(long(syear) lt 1965) then continue
reads,Syear,year,format='(i)'
reads,Smonth,month,format='(i)'
reads,Sday,day,format='(i)'
  Sdoy=get_doy(day,month,year)
  IYEAR=year
  IDAY=Sdoy
  IHOUR=long(Shour)
  IMIN=long(Smin)
  ISEC=long(Ssec)
  geopack_recalc,IYEAR,IDAY,IHOUR,IMIN,ISEC
  recalc,IYEAR,IDAY,IHOUR,IMIN,ISEC
  geopack_igrf_gsm,gsm.y(loop,0), gsm.y(loop,1), gsm.y(loop,2),bx,by,bz
  igrf_bx(loop)=bx
  igrf_by(loop)=by
  igrf_bz(loop)=bz
endfor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
store_data,'dmsp'+scnum+'_pos_igrf_gsm',data={x:gsm.x, y:[[igrf_bx],[igrf_by],[igrf_bz]]},dlim={colors:[2,4,6],labels:['Bx','By','Bz'],ysubtitle:'[nT]',labflag:1,constant:0}
store_data,'dmsp'+scnum+'_pos_igrf_babs',data={x:gsm.x,y:sqrt(igrf_bx^2+igrf_by^2+igrf_bz^2)}

cotrans,'dmsp'+scnum+'_pos_igrf_gsm','dmsp'+scnum+'_pos_igrf_gse',/gsm2gse,/ignore_dlimits
;thm_cotrans,'champ_pos_igrf_gsm',in_coord='gsm',out_coord='geo',out_suffix='_geo'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,'dmsp'+scnum+'_ssm_Bgse',data=Bgse,dlim=dlim
get_data,'dmsp'+scnum+'_pos_igrf_gse',data=Bmodel
;Bgse.y=Bgse.y-Bmodel.y
store_data,'dmsp'+scnum+'_ssm_db-igrf',data=Bgse,dlim=dlim

thm_median,'dmsp'+scnum+'_ssm_db-igrf',3
get_data,'dmsp'+scnum+'_ssm_db-igrf_m',data=Bgse,dlim=dlim
BGSEtot=sqrt(Bgse.y(*,0)^2+Bgse.y(*,1)^2+Bgse.y(*,2)^2)

cotrans,'dmsp'+scnum+'_pos_igrf_gse','dmsp'+scnum+'_pos_igrf_gei',/gse2gei,/ignore_dlimits
cotrans,'dmsp'+scnum+'_pos_igrf_gei','dmsp'+scnum+'_pos_igrf_geo',/gei2geo,/ignore_dlimits
get_data,'dmsp'+scnum+'_pos_igrf_geo',data=bgeo

BGEOX=bgeo.y(*,0)
BGEOY=bgeo.y(*,1)
BGEOZ=bgeo.y(*,2)
BX2= BGEOX*cos(GLON.y*!pi/180)+BGEOY*sin(GLON.y*!pi/180);
BY2=-BGEOX*sin(GLON.y*!pi/180)+BGEOY*cos(GLON.y*!pi/180);
BZ2= BGEOZ
BNORTH =-BX2*sin(GLAT.y*!pi/180)+BZ2*cos(GLAT.y*!pi/180);
BRADIAL= BX2*cos(GLAT.y*!pi/180)+BZ2*sin(GLAT.y*!pi/180);
BAZIMUT= BY2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Jpara=fltarr(n_elements(BGSE.x))
for ii=1L,n_elements(BGSE.x)-1-1 do begin

if (ii mod 10000 eq 0) then print,ii,N_ELEMENTS(BGSE.x)-1

  if(BGSEtot(ii) lt 1e-10 or BGSEtot(ii) gt 100000) then continue

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  dB=fltarr(3)
  ds=fltarr(3)
  unit_Bmodel=fltarr(3)
  dB(*)=(BGSE.y(ii+1,*)-BGSE.y(ii-1,*))/2
  ds(*)=GSEPOS1.y(ii+1,*)-GSEPOS1.y(ii-1,*)
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
    ;Jpara(ii)=Jpara(ii)/Bmodeltot*(30000./((1000*1e3+R_E)/R_E)^3*sqrt(1+3*sin(glat.y(ii)*!pi/180)^2));map to 1000km
  ;northern hemispshere
  ;if(glat.y(ii) gt 0) then Jpara(ii)=-Jpara(ii)
  if(Bradial(ii) le 0) then Jpara(ii)=-Jpara(ii)
  endif
;if(BGSE.x(ii) gt time_double('2003-03-31/17:05:0') and BGSE.x(ii) lt time_double('2003-03-31/17:08:0')) then print,time_string(BGSE.x(ii)),dbtot,BGSE.y(ii,1),Jpara(ii)
endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
store_data,'dmsp'+scnum+'_ssm_FAC',data={x:BGSE.x,y:Jpara},dlim={colors:[0],labels:['FAC'],ysubtitle:'[uA/m2]',labflag:-1,constant:0}
Jpara_up  =fltarr(n_elements(BGSE.x))
Jpara_down=fltarr(n_elements(BGSE.x))
zero=fltarr(n_elements(BGSE.x))
Jpara_up  (where(Jpara gt -1e-20))=Jpara(where(Jpara gt -1e-20))
Jpara_down(where(Jpara lt  1e-20))=Jpara(where(Jpara lt  1e-20))
store_data,'dmsp'+scnum+'_ssm_FACup',data={x:BGSE.x,y:Jpara_up}  ,dlim={colors:[6],labels:['up'],ysubtitle:'[uA/m2]',labflag:-1,constant:0}
store_data,'dmsp'+scnum+'_ssm_FACdown',data={x:BGSE.x,y:Jpara_down},dlim={colors:[2],labels:['down'],ysubtitle:'[uA/m2]',labflag:-1,constant:0}
store_data,'0',data={x:BGSE.x,y:zero},dlim={colors:[0],labels:[''],ysubtitle:'',labflag:-1,constant:0}
store_data,'dmsp'+scnum+'_ssm_FACupdown',data='dmsp'+scnum+'_ssm_FACup'+' '+'dmsp'+scnum+'_ssm_FACdown'+' '+'0'
thm_median,'dmsp'+scnum+'_ssm_FACup',5
thm_median,'dmsp'+scnum+'_ssm_FACdown',5
thm_median,'dmsp'+scnum+'_ssm_FAC',5
store_data,'dmsp'+scnum+'_ssm_FACupdown_m',data='dmsp'+scnum+'_ssm_FACup_m'+' '+'dmsp'+scnum+'_ssm_FACdown_m'+' '+'0'
;ylim,'dmsp'+scnum+'_FACupdown_m',-5,5

end
