
pro thm_satellite_ql,sc=sc,noplot=noplot,median_time=median_time,noload=noload,nolimit=nolimit,tail=tail,sw=sw,use_eclipse_correction=use_eclipse_correction

if not (keyword_set(median_time)) then median_time=60

;timespan, '2008-01-14/22:00:00',3,/hours

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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if not (keyword_set(noload)) then begin
thm_fgm_t01subtract3,sc=sc,use_eclipse_correction=use_eclipse_correction
del_data,'th?_state_*'
;thm_fit_subtract2,sc=sc,use_eclipse_correction=use_eclipse_correction
del_data,'th?_state_*'
thm_xaxis_sm,sc=sc
del_data,'th?_state_*'
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
if not (keyword_set(noload)) then begin
joint_mom_sst_mom9,sc=sc
thm_load_esa,probe=sc,datatype='*en_eflux'
thm_sst_esa1_reduced,sc=sc
del_data,'th?_state_*'
split_vec,'th'+sc+'_Pall_withsst'
ylim,'th'+sc+'_Pall_withsst_?',0,0,0
endif


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;zoom
thm_median,'th'+sc+'_efs_0-vxb-Ecor_gse2',median_time
thm_median,'th'+sc+'_fgs_gsm',median_time
thm_median,'th'+sc+'_fgs-T01_gsm_pol',median_time
thm_median,'th'+sc+'_peim_vperp_gsm',median_time
thm_median,'th'+sc+'_peem_vperp_gsm',median_time
thm_median,'th'+sc+'_pxir_vperp_gsm',median_time
thm_median,'th'+sc+'_pxer_vperp_gsm',median_time
thm_median,'th'+sc+'_pxxm_-scpot',median_time
ylim,'th'+sc+'_fgs-T01_gsm_pol_m',-50,50
ylim,'th'+sc+'_efs_0-vxb-Ecor_gse2_m',-4,4
ylim,'th'+sc+'_Pall_withsst',0.01,5
;ylim,'th'+sc+'_Pall_withsst',0.1,5
get_data,'thmAL',data=data,index=index
if(index ne 0) then begin
minAL=min(data.y)
if(minAL lt -1000) then begin
thm_median,'thmAL',300
get_data,'thmAL_m',data=data
minAL=min(data.y)*1.3
endif
if(minAL lt -2000) then minAL=-2000
ylim,'AL_thmAL',minAL,max(data.y)
endif
zlim,'th'+sc+'_peir_en_eflux_corrected',1e5,1e7

split_vec,'th'+sc+'_fgs_gsm_m'

datatype_esa='peer'
options,'th'+sc+'_'+datatype_esa+'_-scpot_min0',panel_size=0.6
options,'th'+sc+'_pxir_en_eflux',panel_size=0.8
options,'th'+sc+'_pxer_en_eflux',panel_size=0.8
zlim,['th'+sc+'_pxir_en_eflux','th'+sc+'_pxir_en_eflux_corrected'],1e4,1e7
zlim,['th'+sc+'_pxer_en_eflux','th'+sc+'_pxer_en_eflux_corrected'],1e5,1e8
options,['AL_thmAL','th'+sc+'_fgs-T01_gsm_pol_m','th'+sc+'_efs_0-vxb-Ecor_gse2_m','th'+sc+'_pxxm_-scpot_m','th'+sc+'_pxir_vperp_gsm_m','th'+sc+'_pxer_vperp_gsm_m'],yminor=2,labflag=-1


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
tplot_options,title='th'+sc+'  '+Syear+Smonth+Sday+Shour
tplot,['AL_thmAL'],var_label = ['th'+sc+'_MLAT','th'+sc+'_MLT','th'+sc+'_R']

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
options,'*',yticklen=0.02
options,'*',xticklen=0.06
time_stamp,/off

get_data,'th'+sc+'_pxxm_-scpot',data=data,index=index
if(index ne 0) then begin
  if(min(data.y,/nan) lt -20) then ylim,'th'+sc+'_pxxm_-scpot',-20,0
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_timespan,t
get_data,'th'+sc+'_fgs-T01_gsm_pol_m',lim=limits1
get_ylimits,'th'+sc+'_fgs-T01_gsm_pol_m',limits2,t
if(limits1.yrange(1)-limits1.yrange(0) le limits2.yrange(1)-limits2.yrange(0)) then $
ylim,'th'+sc+'_fgs-T01_gsm_pol_m',limits1.yrange(0),limits1.yrange(1)
if(limits1.yrange(1)-limits1.yrange(0) gt limits2.yrange(1)-limits2.yrange(0)) then $
ylim,'th'+sc+'_fgs-T01_gsm_pol_m',limits2.yrange(0),limits2.yrange(1)


if not(keyword_set(noplot)) then begin
if not (keyword_set(skip_esa_correct)) then begin
suffix_esa=''
endif else begin
suffix_esa='_corrected'
endelse
if(index ne 0) then $
tplot,['AL_thmAL','th'+sc+'_fgs-T01_gsm_m','th'+sc+'_Pall_withsst','th'+sc+'_efs_0-vxb-Ecor_gse2_m','th'+sc+'_pxxm_-scpot_m','th'+sc+'_pxir_vperp_gsm_m','th'+sc+'_peim_vperp_gsm_m','th'+sc+'_pxir_en_eflux'+suffix_esa,'th'+sc+'_pxer_en_eflux'+suffix_esa],var_label = ['th'+sc+'_MLAT','th'+sc+'_MLT','th'+sc+'_R']
if(index eq 0) then $
tplot,['AL_thmAL','th'+sc+'_fgs-T01_gsm_m','th'+sc+'_Pall_withsst','th'+sc+'_efs_0-vxb-Ecor_gse2_m','th'+sc+'_pxxm_-scpot_m','th'+sc+'_pxir_vperp_gsm_m','th'+sc+'_peim_vperp_gsm_m','th'+sc+'_pxir_en_eflux'+suffix_esa+'_combbackground','th'+sc+'_pxer_en_eflux'+suffix_esa+'_combbackground'],var_label = ['th'+sc+'_MLAT','th'+sc+'_MLT','th'+sc+'_R']
if keyword_set(tail) then $
tplot,['AL_thmAL','th'+sc+'_fgs_gsm_m_x','th'+sc+'_fgs_gsm_m_y','th'+sc+'_fgs_gsm_m_z','th'+sc+'_Pall_withsst_0','th'+sc+'_Pall_withsst_1','th'+sc+'_efs_0-vxb-Ecor_gse2_m','th'+sc+'_peim_vperp_gsm_m','th'+sc+'_peem_vperp_gsm_m','th'+sc+'_pxir_en_eflux'+suffix_esa,'th'+sc+'_pxer_en_eflux'+suffix_esa],var_label = ['th'+sc+'_MLAT','th'+sc+'_MLT','th'+sc+'_R']
if keyword_set(sw) then $
tplot,['AL_thmAL','th'+sc+'_fgs_gsm_m','th'+sc+'_peim_density','th'+sc+'_peim_velocity','th'+sc+'_Pall_withsst','th'+sc+'_pxir_en_eflux'+suffix_esa,'th'+sc+'_pxer_en_eflux'+suffix_esa],var_label = ['th'+sc+'_MLAT','th'+sc+'_MLT','th'+sc+'_R']
endif



end