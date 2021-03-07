
pro ex1_1

del_data, '*'

;; plot Bxyz for THC & THD
;; select time
timespan, '08-02-12/03:00', 2, /hours
probe=['c','d']
;; data load
thm_load_state, probe=probe, /get_support_data
thm_load_fgm, probe=probe, datatype='fgs'
;; coordinate
thm_cotrans, 'th?_fgs_', in_suffix='dsl', out_suffix='gsm'
;; THC Bxyz, THD Bxyz
window, 0, xsize=700, ysize=850
tplot, ['th?_fgs_gsm']
makepng, 'plot1'


;; plot Bx of THC & THD
;; to three components
split_vec, 'th?_fgs_gsm'
;; set tplot variable
store_data, 'thx_fgs_gsm_x', data=['thc_fgs_gsm_x','thd_fgs_gsm_x']
;; color and linestyle
options, 'thd_fgs_gsm_x', color=2;, linestyle=2
tplot, 'thx_fgs_gsm_x'
makepng, 'plot2'


;; plot Bx of THC & THD with THD-10
get_data, 'thd_fgs_gsm_x', data=dat_d
store_data, 'thd_fgs_gsm_x10', data={x:dat_d.x, y:dat_d.y-10.}
store_data, 'thx_fgs_gsm_x', data=['thc_fgs_gsm_x','thd_fgs_gsm_x','thd_fgs_gsm_x10']
options, 'thd_fgs_gsm_x10', color=6, linestyle=1
tplot, 'thx_fgs_gsm_x'
makepng, 'plot3'


;; plot Bx of THC & THD with s/c location of X in GSM
;; coordinate transform
thm_cotrans, 'th?_state_pos', in_coord='gei', out_suffix='_gsm'
re=6374.
get_data, 'thc_state_pos_gsm', data=dat
store_data, 'thc_state_pos_gsm_re', data={x:dat.x, y:dat.y/re}
get_data, 'thd_state_pos_gsm', data=dat
store_data, 'thd_state_pos_gsm_re', data={x:dat.x, y:dat.y/re}
split_vec, 'th?_state_pos_gsm_re'
tplot, 'thx_fgs_gsm_x', var_label=['thd_state_pos_gsm_re_x', 'thc_state_pos_gsm_re_x']
makepng, 'plot4'


;; ESA&SST
thm_load_esa_pkt, probe=probe, datatype=['peir']
thm_load_sst, probe=probe, datatype=['psif']
tplot, ['thc_psif_en','thc_peir_en_counts','thd_psif_en','thd_peir_en_counts'], var_label=''
makepng, 'plot5'


;; EFLUX
thm_part_moments, probe=probe, instr='peir', moments=''
thm_part_moments, probe=probe, instr='psif', moments=''
store_data, 'thc_pxix_en_eflux', data='thc_peir_en_eflux thc_psif_en_eflux', dlimit={panel_size:1.5}
store_data, 'thd_pxix_en_eflux', data='thd_peir_en_eflux thd_psif_en_eflux', dlimit={panel_size:1.5}
;tplot, ['thc_psif_en_eflux','thc_peir_en_eflux','thd_psif_en_eflux','thd_peir_en_eflux']
ylim, 'th?_pxix_en_eflux', 5, 1e+6, 1
zlim, 'th?_pxix_en_eflux', 10, 1e+6, 1
tplot, ['thc_pxix_en_eflux','thd_pxix_en_eflux']
makepng, 'plot6'


;; wave power spectra
tdpwrspc, 'thc_fgs_gsm_x';,  _extra='_dpw'
wav_data, 'thd_fgs_gsm_x'
ylim,'*dpwrspc*', 1e-3, 0.2, 1
ylim,'*wv_pow*', 1e-3, 0.2, 1
zlim,'*dpwrspc', 10, 1e+4, 1
zlim,'*wv_pow', 10, 1e+4, 1
tplot, ['thc_fgs_gsm_x', 'thc_fgs_gsm_x_dpwrspc','thd_fgs_gsm_x','thd_fgs_gsm_x_wv_pow']
makepng, 'plot7'


;; with proton cycltron freq.
xyz_to_polar, 'th?_fgs_gsm'
get_data, 'thc_fgs_gsm_mag', data=dat
store_data, 'thc_wi', data={x:dat.x, y:dat.y*1.5e-2}
get_data, 'thd_fgs_gsm_mag', data=dat
store_data, 'thd_wi', data={x:dat.x, y:dat.y*1.5e-2}
options, 'th?_wi', color=255;1
store_data, 'thc_fgs_gsm_x_dpwrspc_wi', data='thc_fgs_gsm_x_dpwrspc thc_wi'
store_data, 'thd_fgs_gsm_x_wv_pow_wi', data='thd_fgs_gsm_x_wv_pow thd_wi'
ylim,'*dpwrspc*', 1e-3, 0.2, 1
ylim,'*wv_pow*', 1e-3, 0.2, 1
tplot, ['thc_fgs_gsm', 'thc_fgs_gsm_x_dpwrspc_wi','thd_fgs_gsm','thd_fgs_gsm_x_wv_pow_wi']
makepng, 'plot8'


;; MVA
minvar_matrix_make,'thc_fgs_gsm',tstart='2008-02-12/03:45:00',tstop='2008-02-12/04:00:00'
;minvar_matrix_make,'thc_fgs_gsm',twindow=60,tslide=30 ;; 60s interval every 30s
tvector_rotate,'thc_fgs_gsm_mva_mat','thc_fgs_gsm',newname='thc_b_mva'
options, 'th?_b_mva', 'labels', ['Bmax','Bint','Bmin']
options, 'thc_b_mva', 'ytitle', 'THC B!dMVA!n'
tplot, ['thc_fgs_gsm', 'thc_b_mva']
timebar, ['2008-02-12/03:45:00','2008-02-12/04:00:00']
makepng, 'plot9'


;; AU/AL
load_position='gmag'
thm_load_gmag,/subtract_median
split_vec,'thg_mag_????'
;superpo_histo,'thg_mag_????_x', dif='thg_pseudoAE', res=3.;60.0
superpo_histo,'thg_mag_????_x', min='thg_pseudoAL', res=3.;30.0
superpo_histo,'thg_mag_????_x', max='thg_pseudoAU', res=3.;30.0
;tdespike_AE,-2000.0,1500.0
;clean_spikes, 'thg_pseudoAE_despike', new_name = 'thg_pseudoAE', thresh = 5
;options,'thg_pseudoAE',ytitle='THEMIS!CAE Index'
store_data, 'thg_pseudoAUAL', data=['thg_pseudoAL','thg_pseudoAU']
options,'thg_pseudoAUAL',ytitle='THEMIS!CAU/AL Index'
options,'thg_pseudoAUAL',constant=0

tplot, ['thg_pseudoAUAL', 'thc_fgs_gsm', 'thc_fgs_gsm_x_dpwrspc_wi','thc_pxix_en_eflux',$
'thd_fgs_gsm','thd_fgs_gsm_x_wv_pow_wi','thd_pxix_en_eflux'], var_label=['thd_state_pos_gsm_re_x', 'thc_state_pos_gsm_re_x']
makepng, 'plot10'


;; T96
tt96, 'thc_state_pos_gsm',pdyn=2.0D,dsti=-30.0D,yimf=0.0D,zimf=0.D
tt96, 'thd_state_pos_gsm',pdyn=2.0D,dsti=-30.0D,yimf=0.0D,zimf=0.D
options, 'th?_state_pos_gsm_bt96', linestyle=2
options, 'th?_state_pos_gsm_bt96', colors=[2,4,6];, labels=['bx','by','bz'],
store_data, 'thc_fgs_gsm_t96', data='thc_fgs_gsm thc_state_pos_gsm_bt96'
store_data, 'thd_fgs_gsm_t96', data='thd_fgs_gsm thd_state_pos_gsm_bt96'
;tplot, ['thc_fgs_gsm_t96', 'thd_fgs_gsm_t96']

tplot, ['thg_pseudoAUAL', 'thc_fgs_gsm_t96', 'thc_fgs_gsm_x_dpwrspc_wi','thc_pxix_en_eflux',$
'thd_fgs_gsm_t96','thd_fgs_gsm_x_wv_pow_wi','thd_pxix_en_eflux'], var_label=['thd_state_pos_gsm_re_x', 'thc_state_pos_gsm_re_x']
makepng, 'plot11'

;tlimit, '08-02-12/03:00','08-02-12/04:30'
stop
end