;remove fgs error
pro thm_fgs_remove_error2,sc=sc

;load
thm_load_state,probe=sc,/get_support_data
thm_load_state, probe = sc, coord = 'gsm'
thm_load_fgm,lev=2,probe=[sc],datatype='fgs',coord='dsl';gsm data are not load for removing errors

;T01
tt01, 'th'+sc+'_state_pos',pdyn=1.0D,dsti=0.0D,yimf=0.0D,zimf=1.0D,g1=1.0D,g2=1.0D
get_data,'th'+sc+'_state_pos_bt01',data=T01data
store_data,'th'+sc+'_state_pos_bt01',data={x:T01data.x, y:T01data.y},dlim={colors:[2,4,6],labels:['Bx','By','Bz'],ysubtitle:'[nT]',labflag:1,constant:0}

;interpolation
tinterpol_mxn, strjoin('th'+sc+'_state_pos_bt01'), strjoin('th'+sc+'_fgs_dsl'), newname=strjoin('th'+sc+'_state_pos_bt01_int')

get_data, strjoin('th'+sc+'_fgs_dsl'), data=Bdata,dlim=dlim
store_data, strjoin('th'+sc+'_fgs_dsl_orig'), data=Bdata,dlim=dlim

get_data,'th'+sc+'_state_pos_bt01_int', data=Bmodel
;tinterpol_mxn,'th'+sc+'_R','th'+sc+'_fgs_dsl','th'+sc+'_R_int'


;jump=where(abs(Bdata.y(*,2)) ge 1000 and abs(Bmodel.y(*,2)) le 500)
jump=where(abs(Bdata.y(*,0)) ge 1000 and abs(Bmodel.y(*,0)) le 500,count)

if(count ge 1) then begin
if(n_elements(jump) ge 3 and jump(0) ne 0) then begin
if jump(n_elements(jump)-1) eq n_elements(Bdata.y(*,0))-1 then jump=jump(0:n_elements(jump)-2);;;added by WL
factor1=fltarr(n_elements(Bdata.y(0,*)))
factor1(*)=Bdata.y(jump(2),*)/Bdata.y(jump(0)-1,*)
factor2=fltarr(n_elements(Bdata.y(0,*)))
factor2(*)=Bdata.y(jump(n_elements(jump)-1),*)/Bdata.y(jump(n_elements(jump)-1)+1,*)
factor3=fltarr(n_elements(Bdata.y(0,*)))
for ii=0,n_elements(Bdata.y(0,*))-1,1 do factor3(ii)=(factor1(ii) ge factor2(ii)?factor1(ii):factor2(ii))
Bdata.y(jump,0)=Bdata.y(jump,0)/factor3(0)
Bdata.y(jump,1)=Bdata.y(jump,1)/factor3(1)
Bdata.y(jump,2)=Bdata.y(jump,2)/factor3(2)
endif
endif

store_data,'th'+sc+'_fgs_dsl',data=Bdata,dlim=dlim

end

;sc='a'
; .r thm_fgs_remove_error2
;thm_load_fgm,lev=2,probe=[sc],datatype='fgs'
;thm_fgs_remove_error2,sc=sc
;split_vec,'th'+sc+'_fgs_dsl'
;thm_median,'th'+sc+'_fgs_dsl_z',10
;