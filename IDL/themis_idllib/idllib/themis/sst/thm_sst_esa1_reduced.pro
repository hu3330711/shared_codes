pro thm_sst_esa1_reduced,sc=sc

sc_init=sc

for scloop=0,n_elements(sc_init)-1 do begin

sc=sc_init[scloop]

thm_load_sst2, probe=sc, level='l2',datatype=['ps?r_en_eflux','ps?f_en_eflux']


ylim,'th'+sc+'_pse?_en_eflux',2.8e4,1e6
ylim,'th'+sc+'_psi?_en_eflux',2.8e4,1e6
zlim,strjoin('th'+sc+'_psi?_en_eflux'),1e3,1e8
zlim,strjoin('th'+sc+'_pse?_en_eflux'),1e3,1e8

options,strjoin('th'+sc+'_psi?_en_eflux'),ysubtitle='',ytickformat='logticks_exp'
options,strjoin('th'+sc+'_pse?_en_eflux'),ysubtitle='',ytickformat='logticks_exp'

;ESA+SST
store_data, 'th'+sc+'_pxir_en_eflux', data='th'+sc+'_peir_en_eflux th'+sc+'_psif_en_eflux', dlimit={panel_size:1.0,ysubtitle:''};[/cm2/s/sr]
store_data, 'th'+sc+'_pxer_en_eflux', data='th'+sc+'_peer_en_eflux th'+sc+'_psef_en_eflux', dlimit={panel_size:1.0,ysubtitle:''};[/cm2/s/sr]
ylim, 'th?_px?r_en_eflux', 5, 1e+6, 1
zlim, 'th?_px?r_en_eflux', 1e4, 1e+8, 1
options,strjoin('th'+sc+'_pxir_en_eflux'),ysubtitle='',ytickformat='logticks_exp'
options,strjoin('th'+sc+'_pxer_en_eflux'),ysubtitle='',ytickformat='logticks_exp'
;tplot, ['th'+sc+'_pxir_en_eflux','th'+sc+'_pxer_en_eflux']

options,'th?_p*r_en_eflux','y_no_interp',1,/default
options,'th?_p*r_en_eflux','x_no_interp',1,/default
   
;tplot,[strjoin('th'+sc+'_psir_en_eflux'),strjoin('th'+sc+'_peir_en_eflux'),'th'+sc+'_pser_en_eflux',strjoin('th'+sc+'_peer_en_eflux')]


endfor



end
