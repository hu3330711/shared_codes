;;How to run
;timespan,'2008-02-09/10:16:00',1,/min
;thm_ast_ase,stationname='fykn',/full
;
;timespan,'2008-02-09/10:00:00',1,/hour
;thm_ast_ase,stationname='fykn',/full,skip=3
;
;;Created variables
;Look at ewogram_mlt and dMLT in "thm_ast_ase_1time.pro" for center MLT and MLT width for each variable
;'thg_ase_'+stationname+'_mlat_center'
;'thg_ase_'+stationname+'_mlat_north'
;'thg_ase_'+stationname+'_mlat_south'
;'thg_ase_'+stationname+'_mlat_center_narrow'
;'thg_ase_'+stationname+'_mlat_north_narrow'
;'thg_ase_'+stationname+'_mlat_centernorth_narrow'
;'thg_ase_'+stationname+'_mlat_centersouth_narrow'
;'thg_ase_'+stationname+'_mlat_south_narrow'
;'thg_ase_'+stationname+'_mlat_max'
;
;Longitude order
;min                med                 max
;    north midnorth  center   midsouth south    ;wide and narrow
;           centnorth   centsouth             ;narrow only
;|----|----|----|----|----|----|----|----|
;
;'*_narrow' has a narrower MLT width to make the ewograms.
;MLT width for '*_narrow' is 1/20 of the total imager size.
;MLT width for the other ewograms is 1/8 of the total imager size.
;
;
;skip
;skip a specified number of samples.
;It helps doing completing the calculation faster.
;

pro thm_asi_ase,stationname=stationname,full=full,skip_time=skip_time,subtract_previous=subtract_previous,aseselect=aseselect,maximum=maximum,noload=noload,median_pixels=median_pixels,skip_pixels=skip_pixels,mlat_offset=mlat_offset,flat_cal=flat_cal,tplot_footprint_glat=tplot_footprint_glat,tplot_footprint_glon=tplot_footprint_glon,tplot_footprint_alt=tplot_footprint_alt,subtract_min=subtract_min,glat_slice=glat_slice,glon_slice=glon_slice,pgm=pgm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

tplot_init
;loadct,43,file='/big/SATELLITE/themis/software/tdas/current/idl/themis/common/thm_ctables/thm_colors.tbl'

if not keyword_set(stationname) then stationname='xxxx'

if not keyword_set(filter_time) then filter_time=120.


get_timespan, t

if not keyword_set(aseselect) then ewogram_names=['mlon_farnorth','mlon_north','mlon_middlenorth','mlon_centernorth','mlon_center','mlon_centersouth','mlon_middlesouth','mlon_south','mlon_farsouth','mlon_farnorth_narrow','mlon_north_narrow','mlon_middlenorth_narrow','mlon_centernorth_narrow','mlon_center_narrow','mlon_centersouth_narrow','mlon_middlesouth_narrow','mlon_south_narrow','mlon_farsouth_narrow','mlon_north_wide','mlon_center_wide','mlon_south_wide','mlon_all']
if keyword_set(aseselect) then begin
  if aseselect eq 1 then ewogram_names=['mlon_farnorth','mlon_north','mlon_middlenorth','mlon_centernorth','mlon_center','mlon_centersouth','mlon_middlesouth','mlon_south','mlon_farsouth']
  if aseselect eq 2 then ewogram_names=['mlon_farnorth_narrow','mlon_north_narrow','mlon_middlenorth_narrow','mlon_centernorth_narrow','mlon_center_narrow','mlon_centersouth_narrow','mlon_middlesouth_narrow','mlon_south_narrow','mlon_farsouth_narrow']
  if aseselect eq 3 then ewogram_names=['mlon_north_wide','mlon_center_wide','mlon_south_wide','mlon_all']
  if aseselect eq 4 then ewogram_names=['mlon_middlenorth2_narrow','mlon_middlenorth1_narrow','mlon_centernorth2_narrow','mlon_centernorth1_narrow','mlon_center_narrow','mlon_centersouth1_narrow','mlon_centersouth2_narrow','mlon_middlesouth1_narrow','mlon_middlesouth2_narrow']
  if aseselect eq 5 then ewogram_names=['mlon_farwest_double','mlon_west_double','mlon_middlewest_double','mlon_centerwest_double','mlon_center_double','mlon_centereast_double','mlon_middleeast_double','mlon_east_double','mlon_fareast_double']
  if aseselect eq 6 then ewogram_names=['mlon_north6_verynarrow','mlon_north5_verynarrow','mlon_north4_verynarrow','mlon_north3_verynarrow','mlon_north2_verynarrow','mlon_north1_verynarrow','mlon_center_verynarrow','mlon_south1_verynarrow','mlon_south2_verynarrow','mlon_south3_verynarrow','mlon_south4_verynarrow','mlon_south5_verynarrow','mlon_south6_verynarrow']
  if aseselect eq 7 then ewogram_names=['mlon_north6_verynarrow2','mlon_north5_verynarrow2','mlon_north4_verynarrow2','mlon_north3_verynarrow2','mlon_north2_verynarrow2','mlon_north1_verynarrow2','mlon_center_verynarrow2','mlon_south1_verynarrow2','mlon_south2_verynarrow2','mlon_south3_verynarrow2','mlon_south4_verynarrow2','mlon_south5_verynarrow2','mlon_south6_verynarrow2']
endif

if keyword_set(maximum) then ewogram_suffix='_max'
if not keyword_set(maximum) then ewogram_suffix=''

if not keyword_set(skip_time) then skip_time=1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;del_data for thm_mosaic_array_all
del_data,'thm_asi_asf*'

thm_asi_minimum_elevation=5
thm_asi_factor_intensity=1
if not keyword_set(skip_time) then skip_time=1
if not keyword_set(skip_pixels) then skip_pixels=1
if not keyword_set(mlat_offset) then mlat_offset=0.
if not keyword_set(flat_cal) then flat_cal=250.

if not keyword_set(noload) then begin
  if not keyword_set(pgm) then thm_mosaic_array_all_readonly,stationname,thumb=thumb,flat_cal=flat_cal
  if keyword_set(pgm) then thm_mosaic_array_all_readonly_pgm,stationname,thumb=thumb,flat_cal=flat_cal

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
    where_temp=where(mlon.y ge 180,count)
    if count ge 1 then mlon.y(where_temp)-=360.
    store_data,'thg_asf_'+stationname+'_mlon',data=mlon
  endif
  delvarx,data
endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
thm_asi_ase_process,stationname=stationname,full=full,subtract_previous=subtract_previous,aseselect=aseselect,maximum=maximum,skip_pixels=skip_pixels,skip_time=skip_time,mlat_offset=mlat_offset,glat_slice=glat_slice,glon_slice=glon_slice;,footprint_mlat=footprint_mlat_temp,footprint_mlt=footprint_mlt_temp



if not keyword_set(aseselect) then ewogram_names2=['mlon_farnorth_narrow','mlon_north_narrow','mlon_middlenorth_narrow','mlon_centernorth_narrow','mlon_center_narrow','mlon_centersouth_narrow','mlon_middlesouth_narrow','mlon_south_narrow','mlon_farsouth_narrow']
if keyword_set(aseselect) then begin
  if aseselect eq 1 then ewogram_names2=['mlon_farnorth','mlon_north','mlon_middlenorth','mlon_centernorth','mlon_center','mlon_centersouth','mlon_middlesouth','mlon_south','mlon_farsouth']
  if aseselect eq 2 then ewogram_names2=['mlon_farnorth_narrow','mlon_north_narrow','mlon_middlenorth_narrow','mlon_centernorth_narrow','mlon_center_narrow','mlon_centersouth_narrow','mlon_middlesouth_narrow','mlon_south_narrow','mlon_farsouth_narrow']
  if aseselect eq 3 then ewogram_names2=['mlon_north_wide','mlon_center_wide','mlon_south_wide','mlon_all']
  if aseselect eq 4 then ewogram_names2=['mlon_middlenorth2_narrow','mlon_middlenorth1_narrow','mlon_centernorth2_narrow','mlon_centernorth1_narrow','mlon_center_narrow','mlon_centersouth1_narrow','mlon_centersouth2_narrow','mlon_middlesouth1_narrow','mlon_middlesouth2_narrow']
  if aseselect eq 5 then ewogram_names2=['mlon_north4_verynarrow','mlon_north3_verynarrow','mlon_north2_verynarrow','mlon_north1_verynarrow','mlon_center_verynarrow','mlon_south1_verynarrow','mlon_south2_verynarrow','mlon_south3_verynarrow','mlon_south4_verynarrow']
  if aseselect eq 6 then ewogram_names2=['mlon_north6_verynarrow','mlon_north5_verynarrow','mlon_north4_verynarrow','mlon_north3_verynarrow','mlon_north2_verynarrow','mlon_north1_verynarrow','mlon_center_verynarrow','mlon_south1_verynarrow','mlon_south2_verynarrow','mlon_south3_verynarrow','mlon_south4_verynarrow','mlon_south5_verynarrow','mlon_south6_verynarrow']
  if aseselect eq 7 then ewogram_names2=['mlon_north6_verynarrow2','mlon_north5_verynarrow2','mlon_north4_verynarrow2','mlon_north3_verynarrow2','mlon_north2_verynarrow2','mlon_north1_verynarrow2','mlon_center_verynarrow2','mlon_south1_verynarrow2','mlon_south2_verynarrow2','mlon_south3_verynarrow2','mlon_south4_verynarrow2','mlon_south5_verynarrow2','mlon_south6_verynarrow2']
endif

get_data,'thg_ase_'+stationname+'_'+ewogram_names2[n_elements(ewogram_names2)/2]+ewogram_suffix,data=data
ylim,'thg_ase_'+stationname+'_'+ewogram_names(*)+ewogram_suffix,median(data.v)-15,median(data.v)+15
zlim,'thg_ase_'+stationname+'_'+ewogram_names(*)+ewogram_suffix,3000,10000,1
tplot,'thg_ase_'+stationname+'_'+ewogram_names2(*)+ewogram_suffix

loadct2,43

end
