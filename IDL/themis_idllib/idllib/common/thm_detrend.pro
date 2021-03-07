pro thm_detrend,sc=sc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;fgs
get_data,'th'+sc+'_fgs-T01_gsm',data=Bdata
Bdata_s=Bdata.y
Bdata_d=Bdata.y
Bdata_b=Bdata.y
wdw=ceil(120.0/find_datarate(Bdata.x))
for xx=0,2 do Bdata_s[*,xx]=smooth(Bdata.y[*,xx],wdw,/NaN)
for xx=0,2 do Bdata_d[*,xx]=Bdata.y(*,xx)-Bdata_s(*,xx)
wdw=ceil(30.0/find_datarate(Bdata.x))
for xx=0,2 do Bdata_b[*,xx]=smooth(Bdata_d[*,xx],wdw,/NaN)
store_data,'th'+sc+'_fgs-T01_gsm_s',data={x:Bdata.x,y:[[Bdata_s(*,0)],[Bdata_s(*,1)],[Bdata_s(*,2)]]},dlim={colors:[2,4,6],labels:['Bx','By','Bz'],ysubtitle:'[nT]',labflag:1,constant:0}
store_data,'th'+sc+'_fgs-T01_gsm_b',data={x:Bdata.x,y:[[Bdata_b(*,0)],[Bdata_b(*,1)],[Bdata_b(*,2)]]},dlim={colors:[2,4,6],labels:['Bx','By','Bz'],ysubtitle:'[nT]',labflag:1,constant:0}
store_data,'th'+sc+'_fgs-T01_gsm_d',data={x:Bdata.x,y:[[Bdata_d(*,0)],[Bdata_d(*,1)],[Bdata_d(*,2)]]},dlim={colors:[2,4,6],labels:['Bx','By','Bz'],ysubtitle:'[nT]',labflag:1,constant:0}

;XYZ-VDH
;get_data
get_data,strjoin('th'+sc+'_state_pos_int'),data=tmp
get_data,'th'+sc+'_fgs-T01_gsm_s',data=dFGM
;convert
MLTm12=atan(tmp.y[*,1]/tmp.y[*,0])
if(n_elements(where(tmp.y[*,0] lt 0)) gt 1) then MLTm12(where(tmp.y[*,0] lt 0))=(atan(tmp.y[where(tmp.y[*,0] lt 0),1]/tmp.y[where(tmp.y[*,0] lt 0),0])+!pi)
dFGM_VDH=dFGM
dFGM_VDH.y(*,0)= dFGM.y(*,0)*cos(MLTm12(*))+dFGM.y(*,1)*sin(MLTm12(*))
dFGM_VDH.y(*,1)=-dFGM.y(*,0)*sin(MLTm12(*))+dFGM.y(*,1)*cos(MLTm12(*))
;store data
store_data,'th'+sc+'_fgs-T01_gsm_s_pol',data={x:dFGM.x, y:dFGM_VDH.y},dlim={colors:[2,4,6],labels:['dBr','dBe','dBz'],ysubtitle:'[nT]',labflag:1,constant:0}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;efs
get_data,'th'+sc+'_efs_0-vxb-Ecor_gse',data=Edata
Edata_s=Edata.y
Edata_d=Edata.y
Edata_b=Edata.y
wdw=ceil(300.0/find_datarate(Edata.x))
for xx=0,2 do Edata_s[*,xx]=smooth(Edata.y[*,xx],wdw,/NaN)
for xx=0,2 do Edata_d[*,xx]=Edata.y(*,xx)-Edata_s(*,xx)
store_data,'th'+sc+'_efs_0-vxb-Ecor_gse2_s',data={x:Edata.x,y:[[Edata_s(*,0)],[Edata_s(*,1)]]},dlim={colors:[2,4],labels:['Ex','Ey'],ysubtitle:'[nT]',labflag:1,constant:0}
store_data,'th'+sc+'_efs_0-vxb-Ecor_gse2_b',data={x:Edata.x,y:[[Edata_b(*,0)],[Edata_b(*,1)]]},dlim={colors:[2,4],labels:['Ex','Ey'],ysubtitle:'[nT]',labflag:1,constant:0}
store_data,'th'+sc+'_efs_0-vxb-Ecor_gse2_d',data={x:Edata.x,y:[[Edata_d(*,0)],[Edata_d(*,1)]]},dlim={colors:[2,4],labels:['Ex','Ey'],ysubtitle:'[nT]',labflag:1,constant:0}
store_data,'th'+sc+'_efs_0-vxb-Ecor_gse1_s',data={x:Edata.x,y:[[Edata_s(*,1)]]},dlim={colors:[0],labels:['Ey'],ysubtitle:'[nT]',labflag:1,constant:0}
ylim,'th'+sc+'_efs_0-vxb-Ecor_gse2_s',-5,5

end