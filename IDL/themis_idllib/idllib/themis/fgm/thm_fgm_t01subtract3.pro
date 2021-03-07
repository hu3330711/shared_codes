
pro thm_fgm_t01subtract3,sc=sc,use_eclipse_correction=use_eclipse_correction

get_timespan,t
;load
;thm_load_state,probe=sc,/get_support_data
;thm_load_fgm,lev=2,probe=[sc],datatype='fgs',coord='gsm'
;thm_load_fgm,lev=2,probe=[sc],datatype='fgs',coord='dsl',trange=t;gsm data are not load for removing errors
thm_load_fit,lev=1,probe=[sc],datatype='fgs',coord='dsl',trange=t,type='calibrated',use_eclipse_correction=use_eclipse_correction;gsm data are not load for removing errors
copy_data,'th'+sc+'_fgs','th'+sc+'_fgs_dsl'
thm_load_state, probe = sc, coord = 'gsm',trange=t

;T01
tt01, 'th'+sc+'_state_pos',pdyn=1.0D,dsti=0.0D,yimf=0.0D,zimf=1.0D,g1=1.0D,g2=1.0D
get_data,'th'+sc+'_state_pos_bt01',data=T01data
store_data,'th'+sc+'_state_pos_bt01',data={x:T01data.x, y:T01data.y},dlim={colors:[2,4,6],labels:['Bx','By','Bz'],ysubtitle:'[nT]',labflag:1,constant:0}
copy_data,'th'+sc+'_state_pos_bt01','th'+sc+'_bt01'

;interpolation
;tinterpol_mxn, strjoin('th'+sc+'_state_pos_bt01'), strjoin('th'+sc+'_fgs_dsl'), newname=strjoin('th'+sc+'_state_pos_bt01_int'),/spline
;tinterpol_mxn, strjoin('th'+sc+'_state_pos'), strjoin('th'+sc+'_fgs_dsl'), newname=strjoin('th'+sc+'_state_pos_int'),/spline
tinterpol_mxn,'th'+sc+'_state_pos_bt01','th'+sc+'_fgs_dsl',newname='th'+sc+'_state_pos_bt01_int',/spline
tinterpol_mxn,'th'+sc+'_state_pos','th'+sc+'_fgs_dsl',newname='th'+sc+'_state_pos_int',/spline
print,''

;remove fgs error
;thm_fgs_remove_error2,sc=sc

;coordinate transformation from DSL
dsl2gse,'th'+sc+'_fgs_dsl',strjoin('th'+sc+'_state_spinras'),strjoin('th'+sc+'_state_spindec'),'th'+sc+'_fgs_gse'
cotrans,'th'+sc+'_fgs_gse','th'+sc+'_fgs_gsm',/GSE2GSM
cotrans,'th'+sc+'_fgs_gsm','th'+sc+'_fgs_sm',/GSM2SM
get_data,'th'+sc+'_fgs_gsm',data=data
store_data,'th'+sc+'_fgs_gsm',data=data,dlim={colors:[2,4,6],labels:['Bx','By','Bz'],ysubtitle:'[nT]',labflag:1,constant:0}
cotrans,'th'+sc+'_state_pos_bt01_int','th'+sc+'_state_pos_bt01_int_gse',/GSM2GSE
cotrans,'th'+sc+'_state_pos_bt01_int','th'+sc+'_state_pos_bt01_int_sm',/GSM2SM
cotrans,'th'+sc+'_state_pos_int','th'+sc+'_state_pos_int_gse',/GSM2GSE
cotrans,'th'+sc+'_state_pos_int','th'+sc+'_state_pos_int_sm',/GSM2SM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;gsm
;subtract
get_data,'th'+sc+'_state_pos_bt01_int',data=T01data
get_data,'th'+sc+'_fgs_gsm',data=FGMdata

;remove error
dFGM=FGMdata.y-T01data.y
if(n_elements(where(sqrt(dFGM(*)^2) gt 500)) gt 1) then dFGM(where(sqrt(dFGM(*)^2) gt 500))='NaN'

;XYZ-VDH
get_data,strjoin('th'+sc+'_state_pos_int'),data=tmp
MLTm12=atan(tmp.y[*,1]/tmp.y[*,0])
if(n_elements(where(tmp.y[*,0] lt 0)) gt 1) then MLTm12(where(tmp.y[*,0] lt 0))=(atan(tmp.y[where(tmp.y[*,0] lt 0),1]/tmp.y[where(tmp.y[*,0] lt 0),0])+3.14159)
FGM_VDH=FGMdata.y
FGM_VDH(*,0)= FGMdata.y(*,0)*cos(MLTm12(*))+FGMdata.y(*,1)*sin(MLTm12(*))
FGM_VDH(*,1)=-FGMdata.y(*,0)*sin(MLTm12(*))+FGMdata.y(*,1)*cos(MLTm12(*))
dFGM_VDH=dFGM
dFGM_VDH(*,0)= dFGM(*,0)*cos(MLTm12(*))+dFGM(*,1)*sin(MLTm12(*))
dFGM_VDH(*,1)=-dFGM(*,0)*sin(MLTm12(*))+dFGM(*,1)*cos(MLTm12(*))

;store data
store_data,'th'+sc+'_fgs_gsm_pol',data={x:FGMdata.x, y:FGM_VDH},dlim={colors:[2,4,6],labels:['Br','Be','Bz'],ysubtitle:'[nT]',labflag:1,constant:0}
store_data,'th'+sc+'_fgs-T01_gsm_pol',data={x:FGMdata.x, y:dFGM_VDH},dlim={colors:[2,4,6],labels:['dBr','dBe','dBz'],ysubtitle:'[nT]',labflag:1,constant:0}
store_data,'th'+sc+'_fgs-T01_gsm',data={x:FGMdata.x, y:dFGM},dlim={colors:[2,4,6],labels:['dBx','dBy','dBz'],ysubtitle:'[nT]',labflag:1,constant:0}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;gse
;subtract
get_data,'th'+sc+'_state_pos_bt01_int_gse',data=T01data
get_data,'th'+sc+'_fgs_gse',data=FGMdata

;remove error
dFGM=FGMdata.y-T01data.y
if(n_elements(where(sqrt(dFGM(*)^2) gt 500)) gt 1) then dFGM(where(sqrt(dFGM(*)^2) gt 500))='NaN'

;XYZ-VDH
get_data,strjoin('th'+sc+'_state_pos_int_gse'),data=tmp
GLTm12=atan(tmp.y[*,1]/tmp.y[*,0])
if(n_elements(where(tmp.y[*,0] lt 0)) gt 1) then GLTm12(where(tmp.y[*,0] lt 0))=(atan(tmp.y[where(tmp.y[*,0] lt 0),1]/tmp.y[where(tmp.y[*,0] lt 0),0])+3.14159)
FGM_VDH=FGMdata.y
FGM_VDH(*,0)= FGMdata.y(*,0)*cos(GLTm12(*))+FGMdata.y(*,1)*sin(GLTm12(*))
FGM_VDH(*,1)=-FGMdata.y(*,0)*sin(GLTm12(*))+FGMdata.y(*,1)*cos(GLTm12(*))
dFGM_VDH=dFGM
dFGM_VDH(*,0)= dFGM(*,0)*cos(GLTm12(*))+dFGM(*,1)*sin(GLTm12(*))
dFGM_VDH(*,1)=-dFGM(*,0)*sin(GLTm12(*))+dFGM(*,1)*cos(GLTm12(*))

;store data
store_data,'th'+sc+'_fgs_gse_pol',data={x:FGMdata.x, y:FGM_VDH},dlim={colors:[2,4,6],labels:['Br','Be','Bz'],ysubtitle:'[nT]',labflag:1,constant:0}
store_data,'th'+sc+'_fgs-T01_gse_pol',data={x:FGMdata.x, y:dFGM_VDH},dlim={colors:[2,4,6],labels:['dBr','dBe','dBz'],ysubtitle:'[nT]',labflag:1,constant:0}
store_data,'th'+sc+'_fgs-T01_gse',data={x:FGMdata.x, y:dFGM},dlim={colors:[2,4,6],labels:['dBx','dBy','dBz'],ysubtitle:'[nT]',labflag:1,constant:0}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;sm
;subtract
get_data,'th'+sc+'_state_pos_bt01_int_sm',data=T01data
get_data,'th'+sc+'_fgs_sm',data=FGMdata

;remove error
dFGM=FGMdata.y-T01data.y
if(n_elements(where(sqrt(dFGM(*)^2) gt 500)) gt 1) then dFGM(where(sqrt(dFGM(*)^2) gt 500))='NaN'

;XYZ-VDH
get_data,strjoin('th'+sc+'_state_pos_int_sm'),data=tmp
MLTm12=atan(tmp.y[*,1]/tmp.y[*,0])
if(n_elements(where(tmp.y[*,0] lt 0)) gt 1) then MLTm12(where(tmp.y[*,0] lt 0))=(atan(tmp.y[where(tmp.y[*,0] lt 0),1]/tmp.y[where(tmp.y[*,0] lt 0),0])+3.14159)
FGM_VDH=FGMdata.y
FGM_VDH(*,0)= FGMdata.y(*,0)*cos(MLTm12(*))+FGMdata.y(*,1)*sin(MLTm12(*))
FGM_VDH(*,1)=-FGMdata.y(*,0)*sin(MLTm12(*))+FGMdata.y(*,1)*cos(MLTm12(*))
dFGM_VDH=dFGM
dFGM_VDH(*,0)= dFGM(*,0)*cos(MLTm12(*))+dFGM(*,1)*sin(MLTm12(*))
dFGM_VDH(*,1)=-dFGM(*,0)*sin(MLTm12(*))+dFGM(*,1)*cos(MLTm12(*))

T01_VDH=T01data.y
T01_VDH(*,0)= T01data.y(*,0)*cos(MLTm12(*))+T01data.y(*,1)*sin(MLTm12(*))
T01_VDH(*,1)=-T01data.y(*,0)*sin(MLTm12(*))+T01data.y(*,1)*cos(MLTm12(*))

;inclination
inclination_FGMdata=atan(FGMdata.y(*,2)/sqrt(FGMdata.y(*,0)^2+FGMdata.y(*,1)^2))*180/!pi
inclination_model=atan(T01data.y(*,2)/sqrt(T01data.y(*,0)^2+T01data.y(*,1)^2))*180/!pi
;inclination_FGMdata=atan(FGM_VDH(*,2)/abs(FGM_VDH(*,0)))*180/!pi
;inclination_model=atan(T01_VDH(*,2)/abs(T01_VDH(*,0)))*180/!pi

;store data
store_data,'th'+sc+'_fgs_sm_pol',data={x:FGMdata.x, y:FGM_VDH},dlim={colors:[2,4,6],labels:['Br','Be','Bz'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'th'+sc+'_fgs-T01_sm_pol',data={x:FGMdata.x, y:dFGM_VDH},dlim={colors:[2,4,6],labels:['dBr','dBe','dBz'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'th'+sc+'_fgs-T01_sm',data={x:FGMdata.x, y:dFGM},dlim={colors:[2,4,6],labels:['dBx','dBy','dBz'],ysubtitle:'[nT]',labflag:-1,constant:0}
store_data,'th'+sc+'_fgs_sm_incl',data={x:FGMdata.x, y:[[inclination_FGMdata],[inclination_model]]},dlim={colors:[2,0],labels:['FGM','T01'],ysubtitle:'[deg]',labflag:-1,constant:0}
store_data,'th'+sc+'_state_pos_bt01_int_sm_pol',data={x:FGMdata.x, y:T01_VDH},dlim={colors:[2,4,6],labels:['Br','Be','Bz'],ysubtitle:'[nT]',labflag:-1,constant:0}


;;;;;

thm_detrend4,'th'+sc+'_fgs_gsm',30,3
thm_detrend4,'th'+sc+'_fgs-T01_gsm_pol',30,3
thm_detrend4,'th'+sc+'_fgs-T01_gsm',30,3
thm_median,'th'+sc+'_fgs_gsm',30
thm_median,'th'+sc+'_fgs-T01_gsm_pol',30
thm_median,'th'+sc+'_fgs-T01_gsm',30

;tplot
ylim,strjoin('th'+sc+'_fgs-T01_gsm'),-100,100
;tplot,['th'+sc+'_fgs_gsm','th'+sc+'_state_pos_bt01','th'+sc+'_fgs-T01_gsm']

end
