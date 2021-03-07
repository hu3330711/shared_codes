;;How to run
;timespan,'2008-02-09/10:16:00',1,/min
;thm_ast_ask,stationname='fykn',/full
;
;timespan,'2008-02-09/10:00:00',1,/hour
;thm_ast_ask,stationname='fykn',/full,skip=3
;
;;Created variables
;Look at keogram_mlt and dMLT in "thm_ast_ask_1time.pro" for center MLT and MLT width for each variable
;'thg_ask_'+stationname+'_mlat_center'
;'thg_ask_'+stationname+'_mlat_west'
;'thg_ask_'+stationname+'_mlat_east'
;'thg_ask_'+stationname+'_mlat_center_narrow'
;'thg_ask_'+stationname+'_mlat_west_narrow'
;'thg_ask_'+stationname+'_mlat_centerwest_narrow'
;'thg_ask_'+stationname+'_mlat_centereast_narrow'
;'thg_ask_'+stationname+'_mlat_east_narrow'
;'thg_ask_'+stationname+'_mlat_max'
;
;Longitude order
;min                med                 max
;    west midwest  center   mideast east    ;wide and narrow
;           centwest   centeast             ;narrow only
;|----|----|----|----|----|----|----|----|
;
;'*_narrow' has a narrower MLT width to make the keograms.
;MLT width for '*_narrow' is 1/20 of the total imager size.
;MLT width for the other keograms is 1/8 of the total imager size.
;
;
;skip
;skip a specified number of samples.
;It helps doing completing the calculation faster.
;

pro thm_asi_ask,stationname=stationname,full=full,skip_time=skip_time,subtract_previous=subtract_previous,askselect=askselect,maximum=maximum,noload=noload,median_pixels=median_pixels,skip_pixels=skip_pixels,mlon_offset=mlon_offset,flat_cal=flat_cal,glat_slice=glat_slice,glon_slice=glon_slice,pgm=pgm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

tplot_init
;loadct,43,file='/big/SATELLITE/themis/software/tdas/current/idl/themis/common/thm_ctables/thm_colors.tbl'

if not keyword_set(stationname) then stationname='xxxx'
if not keyword_set(full) then thumb=1

if not keyword_set(filter_time) then filter_time=120.


get_timespan, t

if not keyword_set(askselect) then keogram_names=['mlat_farwest','mlat_west','mlat_middlewest','mlat_centerwest','mlat_center','mlat_centereast','mlat_middleeast','mlat_east','mlat_fareast','mlat_farwest_narrow','mlat_west_narrow','mlat_middlewest_narrow','mlat_centerwest_narrow','mlat_center_narrow','mlat_centereast_narrow','mlat_middleeast_narrow','mlat_east_narrow','mlat_fareast_narrow','mlat_west_wide','mlat_center_wide','mlat_east_wide','mlat_all']
if keyword_set(askselect) then begin
  if askselect eq 1 then keogram_names=['mlat_farwest','mlat_west','mlat_middlewest','mlat_centerwest','mlat_center','mlat_centereast','mlat_middleeast','mlat_east','mlat_fareast']
  if askselect eq 2 then keogram_names=['mlat_farwest_narrow','mlat_west_narrow','mlat_middlewest_narrow','mlat_centerwest_narrow','mlat_center_narrow','mlat_centereast_narrow','mlat_middleeast_narrow','mlat_east_narrow','mlat_fareast_narrow']
  if askselect eq 3 then keogram_names=['mlat_west_wide','mlat_center_wide','mlat_east_wide','mlat_all']
  if askselect eq 4 then keogram_names=['mlat_middlewest2_narrow','mlat_middlewest1_narrow','mlat_centerwest2_narrow','mlat_centerwest1_narrow','mlat_center_narrow','mlat_centereast1_narrow','mlat_centereast2_narrow','mlat_middleeast1_narrow','mlat_middleeast2_narrow']
  if askselect eq 5 then keogram_names=['mlat_farwest_double','mlat_west_double','mlat_middlewest_double','mlat_centerwest_double','mlat_center_double','mlat_centereast_double','mlat_middleeast_double','mlat_east_double','mlat_fareast_double']
  if askselect eq 6 then keogram_names=['mlat_west6_verynarrow','mlat_west5_verynarrow','mlat_west4_verynarrow','mlat_west3_verynarrow','mlat_west2_verynarrow','mlat_west1_verynarrow','mlat_center_verynarrow','mlat_east1_verynarrow','mlat_east2_verynarrow','mlat_east3_verynarrow','mlat_east4_verynarrow','mlat_east5_verynarrow','mlat_east6_verynarrow']
endif

if keyword_set(maximum) then keogram_suffix='_max'
if not keyword_set(maximum) then keogram_suffix=''

if not keyword_set(skip_time) then skip_time=1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;del_data for thm_mosaic_array_all
del_data,'thm_asi_asf*'

thm_asi_minimum_elevation=5
thm_asi_factor_intensity=1
if not keyword_set(skip_time) then skip_time=1
if not keyword_set(skip_pixels) then skip_pixels=1
if not keyword_set(mlon_offset) then mlon_offset=0.

if not keyword_set(noload) then begin
  if not keyword_set(pgm) then thm_mosaic_array_all_readonly,stationname,thumb=thumb,flat_cal=flat_cal
  if keyword_set(pgm) then thm_mosaic_array_all_readonly_pgm,stationname,thumb=thumb,flat_cal=flat_cal

  ;if keyword_set(thumb) then begin
  ;  copy_data,'thg_ast_'+stationname+'_glat','thg_asf_'+stationname+'_glat'
  ;  copy_data,'thg_ast_'+stationname+'_glon','thg_asf_'+stationname+'_glon'
  ;  copy_data,'thg_ast_'+stationname+'_glat_center','thg_asf_'+stationname+'_glat_center'
  ;  copy_data,'thg_ast_'+stationname+'_glon_center','thg_asf_'+stationname+'_glon_center'
  ;endif
  if keyword_set(median_pixels) then begin
    get_data,'thg_asf_'+stationname,data=data
    for loop=0,n_elements(data.x)-1,1 do data.y(loop,*,*)=median(reform(data.y(loop,*,*)),median_pixels)
    store_data,'thg_asf_'+stationname,data=data
  endif

  get_data,'thg_asf_'+stationname+'_glat_center',data=glat
  store_data,'thg_asf_'+stationname+'_time',data={x:glat.x,y:glat.x}
  r=fltarr(n_elements(glat.y))
  r(*)=1.02
  tplot_restore,filename='/projectnb/burbsp/big/SATELLITE/themis/software/idllib/themis/asi/save/thg_asf_'+stationname+'_aacgm.tplot'
  get_data,'thg_asf_'+stationname+'_aacgm',data=grid
  aacgmmlon=grid.mlon
  aacgmmlat=grid.mlat
  mlon={x:'',y:aacgmmlon}
  mlat={x:'',y:aacgmmlat}
  store_data,'thg_asf_'+stationname+'_mlat',data=mlat
  store_data,'thg_asf_'+stationname+'_mlon',data=mlon
  if(min(mlon.y,/nan) le 5 and max(mlon.y,/nan) ge 355) then begin
  endif
  if keyword_set(median_pixels) then begin
    get_data,'thg_asf_'+stationname,data=data
    for loop=0,n_elements(data.x)-1,1 do data.y(loop,*,*)=median(reform(data.y(loop,*,*)),median_pixels)
    store_data,'thg_asf_'+stationname,data=data
  endif

  get_data,'thg_asf_'+stationname+'_glat_center',data=glat
  store_data,'thg_asf_'+stationname+'_time',data={x:glat.x,y:glat.x}
  r=fltarr(n_elements(glat.y))
  r(*)=1.02
  tplot_restore,filename='./save/thg_asf_'+stationname+'_aacgm.tplot'
  get_data,'thg_asf_'+stationname+'_aacgm',data=grid
  aacgmmlon=grid.mlon
  aacgmmlat=grid.mlat
  mlon={x:'',y:aacgmmlon}
  mlat={x:'',y:aacgmmlat}
  store_data,'thg_asf_'+stationname+'_mlat',data=mlat
  store_data,'thg_asf_'+stationname+'_mlon',data=mlon
  if(min(mlon.y,/nan) le 5 and max(mlon.y,/nan) ge 355) then begin
    where_temp=where(mlon.y ge 180,count)
    if count ge 1 then mlon.y(where_temp)-=360.
    store_data,'thg_asf_'+stationname+'_mlon',data=mlon
  endif
  delvarx,data
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
thm_asi_ask_process,stationname=stationname,full=full,subtract_previous=subtract_previous,askselect=askselect,maximum=maximum,skip_pixels=skip_pixels,skip_time=skip_time,mlon_offset=mlon_offset,glat_slice=glat_slice,glon_slice=glon_slice

;loadct2,0

if not keyword_set(askselect) then keogram_names2=['mlat_farwest_narrow','mlat_west_narrow','mlat_middlewest_narrow','mlat_centerwest_narrow','mlat_center_narrow','mlat_centereast_narrow','mlat_middleeast_narrow','mlat_east_narrow','mlat_fareast_narrow']
if keyword_set(askselect) then begin
  if askselect eq 1 then keogram_names2=['mlat_farwest','mlat_west','mlat_middlewest','mlat_centerwest','mlat_center','mlat_centereast','mlat_middleeast','mlat_east','mlat_fareast']
  if askselect eq 2 then keogram_names2=['mlat_farwest_narrow','mlat_west_narrow','mlat_middlewest_narrow','mlat_centerwest_narrow','mlat_center_narrow','mlat_centereast_narrow','mlat_middleeast_narrow','mlat_east_narrow','mlat_fareast_narrow']
  if askselect eq 3 then keogram_names2=['mlat_west_wide','mlat_center_wide','mlat_east_wide','mlat_all']
  if askselect eq 4 then keogram_names2=['mlat_middlewest2_narrow','mlat_middlewest1_narrow','mlat_centerwest2_narrow','mlat_centerwest1_narrow','mlat_center_narrow','mlat_centereast1_narrow','mlat_centereast2_narrow','mlat_middleeast1_narrow','mlat_middleeast2_narrow']
  if askselect eq 5 then keogram_names2=['mlat_farwest_double','mlat_west_double','mlat_middlewest_double','mlat_centerwest_double','mlat_center_double','mlat_centereast_double','mlat_middleeast_double','mlat_east_double','mlat_fareast_double']
  if askselect eq 6 then keogram_names2=['mlat_west6_verynarrow','mlat_west5_verynarrow','mlat_west4_verynarrow','mlat_west3_verynarrow','mlat_west2_verynarrow','mlat_west1_verynarrow','mlat_center_verynarrow','mlat_east1_verynarrow','mlat_east2_verynarrow','mlat_east3_verynarrow','mlat_east4_verynarrow','mlat_east5_verynarrow','mlat_east6_verynarrow']
endif
;ylim,'thg_ask_'+stationname+'_'+keogram_names(*)+keogram_suffix,60,80
tplot,'thg_ask_'+stationname+'_'+keogram_names2(*)+keogram_suffix

stop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
thm_median,['thg_ask_'+stationname+'_'+keogram_names2(*)+keogram_suffix],300
thm_ask_subtmin_mlat,names=['thg_ask_'+stationname+'_'+keogram_names2(*)+keogram_suffix]
thm_median_timevalue,['thg_ask_'+stationname+'_'+keogram_names2(*)+keogram_suffix+'_md','thg_ask_'+stationname+'_'+keogram_names2(*)+keogram_suffix+'_subtmin'],2

zlim,'thg_ask_'+stationname+'_*_md_m',-500,500,0
zlim,'thg_ask_'+stationname+'_*_subtmin',0,500,0

tplot,['thg_ask_'+stationname+'_'+keogram_names2(*)+keogram_suffix+'_md_m']
stop
tplot,['thg_ask_'+stationname+'_'+keogram_names2(*)+keogram_suffix+'_subtmin']

;loadct2,43

end
