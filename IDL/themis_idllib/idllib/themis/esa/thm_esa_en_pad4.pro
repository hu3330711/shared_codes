;esa only
;use revised thm_part_products_store
pro thm_esa_en_pad4,sc=sc,datatype=datatype,estep=estep,emin=emin,emax=emax,noload=noload,perpsubt=perpsubt,regrid=regrid

get_timespan,t

if not keyword_set(datatype) then datatype='peef'
data_type=datatype
probe=sc
print,sc,data_type
if not keyword_set(regrid) then regrid=[16,16]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
thm_load_fit,probe=sc,trange=trange
if not keyword_set(noload) and strmid(datatype,1,1) eq 'e' then thm_part_load,probe=sc,datatype=datatype,trange=[t(0)-60,t(1)+60]
if not keyword_set(noload) and strmid(datatype,1,1) eq 's' then thm_part_load,probe=sc,datatype=datatype,trange=[t(0)-60,t(1)+60],/sst_cal
thm_part_products_store,probe=sc,outputs='pa',pitch=[0,180],datatype=datatype,/energy,trange=[t(0)-60,t(1)+60],regrid=regrid;erange not needed to speedup;,/bgnd_remove,bgnd_type='anode',bgnd_npoints=5,bgnd_scale=1.5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,'th'+sc+'_'+datatype+'_tclean_data_data_grid',data=data
get_data,'th'+sc+'_'+datatype+'_tclean_data_theta_fac_grid',data=theta
str_element,data,'x',success=success
if success eq 0 then return

if keyword_set(perpsubt) then begin
  for loop=0,data.regrid[0]-1 do begin
    min1=min(data.data[*,*,(4*data.regrid[0]+loop):(n_elements(data.data[0,0,*])-1-4*data.regrid[0]):data.regrid[0]],di=3,/nan)>0.0
    for loop2=0,data.regrid[1]-1 do data.data[*,*,loop+loop2*data.regrid[0]]=(data.data[*,*,loop+loop2*data.regrid[0]]-min1)>0.0
  endfor
endif
store_data,'th'+sc+'_'+datatype+'_tclean_data_data_grid',data=data

data_phiave=fltarr(n_elements(data.x),n_elements(data.data[0,*,0]),data.regrid[1])
theta_phiave=fltarr(n_elements(data.x),n_elements(data.data[0,*,0]),data.regrid[1])
for loop=0,data.regrid[1]-1 do begin
  data_phiave[*,*,loop]=total(data.data[*,*,loop*data.regrid[0]:loop*data.regrid[0]+data.regrid[0]-1],3)/data.regrid[0]
  theta_phiave[*,*,loop]=total(theta.y[*,*,loop*data.regrid[0]:loop*data.regrid[0]+data.regrid[0]-1],3)/data.regrid[0]
endfor

;append
store_data,'th'+sc+'_'+datatype+'_an_eflux_pa_append',data={x:data.x,y:reverse(data_phiave,3),v:reverse(reform(theta_phiave[*,0,*]),2),v2:reform(data.energy[*,*,0])}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
restore,filename='th'+sc+'_'+datatype+'_tclean_data_grid_str.sav'
;spawn,'rm th'+sc+'_'+datatype+'_tclean_data_grid_str.sav'
get_data,'th'+probe+'_'+datatype+'_tclean_data_phi_fac_grid',data=phi
get_data,'th'+probe+'_'+datatype+'_tclean_data_theta_fac_grid',data=theta

thm_load_esa,probe=probe,datatype='peer_sc_pot',trange=t
tinterpol_mxn,'th'+probe+'_peer_sc_pot','th'+probe+'_'+datatype+'_tclean_data_phi_fac_grid',newname='temp'
get_data,'temp',data=scpot
energy_scpot_correct=tclean_data_grid_str.energy
for i=0,n_elements(scpot.x)-1 do energy_scpot_correct[i,*,*]+=(tclean_data_grid_str.charge*(scpot.y[i]-median(scpot.y))/abs(tclean_data_grid_str.charge))

data_df=conv_units(tclean_data_grid_str,'df')
store_data,'th'+sc+'_'+datatype+'_tclean_data_data_grid_df',data=data_df

data_df.energy=energy_scpot_correct
tclean_data_grid_str_scpot_correct=conv_units(data_df,'eflux')

store_data,'th'+sc+'_'+datatype+'_tclean_data_data_grid_df_scpot_correct',data=data_df
store_data,'th'+sc+'_'+datatype+'_tclean_data_data_grid_scpot_correct',data=tclean_data_grid_str_scpot_correct

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,'th'+sc+'_'+datatype+'_tclean_data_data_grid_scpot_correct',data=data
get_data,'th'+sc+'_'+datatype+'_tclean_data_theta_fac_grid',data=theta

data_phiave=fltarr(n_elements(data.x),n_elements(data.data[0,*,0]),data.regrid[1])
theta_phiave=fltarr(n_elements(data.x),n_elements(data.data[0,*,0]),data.regrid[1])
for loop=0,data.regrid[1]-1 do begin
  data_phiave[*,*,loop]=total(data.data[*,*,loop*data.regrid[0]:loop*data.regrid[0]+data.regrid[0]-1],3)/data.regrid[0]
  theta_phiave[*,*,loop]=total(theta.y[*,*,loop*data.regrid[0]:loop*data.regrid[0]+data.regrid[0]-1],3)/data.regrid[0]
endfor

;append
store_data,'th'+sc+'_'+datatype+'_an_eflux_pa_append_scpot_correct',data={x:data.x,y:reverse(data_phiave,3),v:reverse(reform(theta_phiave[*,0,*]),2),v2:reform(data.energy[*,*,0])}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,'th'+sc+'_'+datatype+'_tclean_data_data_grid_df',data=data
get_data,'th'+sc+'_'+datatype+'_tclean_data_theta_fac_grid',data=theta

data_phiave=fltarr(n_elements(data.x),n_elements(data.data[0,*,0]),data.regrid[1])
theta_phiave=fltarr(n_elements(data.x),n_elements(data.data[0,*,0]),data.regrid[1])
for loop=0,data.regrid[1]-1 do begin
  data_phiave[*,*,loop]=total(data.data[*,*,loop*data.regrid[0]:loop*data.regrid[0]+data.regrid[0]-1],3)/data.regrid[0]
  theta_phiave[*,*,loop]=total(theta.y[*,*,loop*data.regrid[0]:loop*data.regrid[0]+data.regrid[0]-1],3)/data.regrid[0]
endfor

;append
store_data,'th'+sc+'_'+datatype+'_an_df_pa_append',data={x:data.x,y:reverse(data_phiave,3),v:reverse(reform(theta_phiave[*,0,*]),2),v2:reform(data.energy[*,*,0])}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_data,'th'+sc+'_'+datatype+'_tclean_data_data_grid_df_scpot_correct',data=data
get_data,'th'+sc+'_'+datatype+'_tclean_data_theta_fac_grid',data=theta

data_phiave=fltarr(n_elements(data.x),n_elements(data.data[0,*,0]),data.regrid[1])
theta_phiave=fltarr(n_elements(data.x),n_elements(data.data[0,*,0]),data.regrid[1])
for loop=0,data.regrid[1]-1 do begin
  data_phiave[*,*,loop]=total(data.data[*,*,loop*data.regrid[0]:loop*data.regrid[0]+data.regrid[0]-1],3)/data.regrid[0]
  theta_phiave[*,*,loop]=total(theta.y[*,*,loop*data.regrid[0]:loop*data.regrid[0]+data.regrid[0]-1],3)/data.regrid[0]
endfor

;append
store_data,'th'+sc+'_'+datatype+'_an_df_pa_append_scpot_correct',data={x:data.x,y:reverse(data_phiave,3),v:reverse(reform(theta_phiave[*,0,*]),2),v2:reform(data.energy[*,*,0])}

end
