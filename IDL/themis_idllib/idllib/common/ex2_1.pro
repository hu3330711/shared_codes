pro ex2_1, ps=ps, filename=filename, original=original
if not keyword_set(filename) then filename='test'
del_data, '*'

probe = ['c','e'];['a','c','d','e']
dtyp = ['esa/peif_en_eflux/l2','esa/peif_density/l2','esa/peif_avgtemp/l2','esa/peif_velocity_gsm/l2','sst/psif_en_eflux/l2','fgm/fgs/l2']
start_time = '2007-11-12/12:00:00'
end_time = '2007-11-12/24:00:00'
varnames = thm_ui_load_data_fn(start_time, end_time,dtype=dtyp, station=station, astation=asi_station, probe=probe)

if keyword_set(original) then begin
   tplot, ['the_psif_en_eflux','the_peif_en_eflux','the_peif_density','the_peif_avgtemp','thc_psif_en_eflux','thc_peif_en_eflux','thc_peif_density','thc_peif_avgtemp','thc_peif_velocity_gsm'], /time_stamp
   tlimit, start_time, end_time
   if keyword_set(ps) then makeps, filename+'_org' else makepng, filename+'_org'
stop
endif

options, 'th?_peif_density', constant=1 ;;horizontal line;;
options, 'th?_peif_avgtemp', constant=2

;; Definition of variables
;; THE
get_data, 'the_peif_avgtemp', data=dat
store_data, 'the_peif_avgtemp', data={x:dat.x,y:dat.y/1000.}, dlimit={ytitle:'',color:6,linestyle:2}
store_data, 'the_peif_nt', data=['the_peif_density', 'the_peif_avgtemp'], dlimit={ytitle:'THE!cN!di!n T!di!n'}
store_data, 'the_pxif_en_eflux', data='the_peif_en_eflux the_psif_en_eflux',$
dlimit={ytitle:'THE!cion',zrange:[1e+2,1e+8],panel_size:1.5, ztitle:'Eflux'}

;; THC
get_data, 'thc_peif_avgtemp', data=dat
store_data, 'thc_peif_avgtemp', data={x:dat.x,y:dat.y/1000.}, dlimit={ytitle:'',color:6,linestyle:2}
store_data, 'thc_peif_nt', data=['thc_peif_density','thc_peif_avgtemp'], dlimit={ytitle:'THC!cN!di!n T!di!n'}
store_data, 'thc_pxif_en_eflux', data='thc_peif_en_eflux thc_psif_en_eflux',$
dlimit={ytitle:'THC!cion',zrange:[1e+2,1e+8],panel_size:1.5, ztitle:'Eflux'}

;; beta
get_data, 'thc_peif_density', data=ni
get_data, 'thc_peif_avgtemp', data=ti
xyz_to_polar, 'thc_fgs_dsl'
tinterpol_mxn, 'thc_fgs_dsl_mag', 'thc_peif_density', newname='thc_fgs_dsl_mag_int' ;;interpolation;;
get_data, 'thc_fgs_dsl_mag_int', data=bt
get_data, 'thc_peif_avgtemp', data=ti
store_data, 'thc_beta', data={x:ni.x, y:3.47e-5*ni.y*ti.y(*)*11605.e+3/bt.y^2.}, lim={ylog:1}

;; options
options, 'th*' , xticklen=0.1 ;;tick length;;
options, 'th*' , yticklen=0.01
options, 'th?_p?if_en_eflux' , zticks=3 ;;# of ticks;;
options, 'thd_peif_density' , ytickname=['o.l','l','lo'] ;;ticks array;;
options, 'th*' , xminor=8 ;;minor ticks;;
options, 'thd_peif_nt' , yminor=2
options, 'th?_p?if_en_eflux', 'spec',  1 ;;line to contour plots;;
options, 'th?_peif_velocity_gsm', labels=['Vx','Vy','Vz'], colors=[2,4,6], ytitle='Vi [km/s]', labflag=1 ;;label form;;

;options, 'th?_peif_nt' , constant=1
options, '*', ysubtitle='' ;;(sub-title;;
options, 'thd_pxif_en_elux', ytitle='THD!cion'
options, 'the_pxif_en_elux', ytitle='THE!cion'
options, 'tha_pxif_en_elux', ytitle='THA!cion'
options, 'thc_pxif_en_elux', ytitle='THC!cion'
options, 'thd_peif_density', ytitle='THD!cN!di!n'
options, 'the_peif_density', ytitle='THE!cN!di!n'
options, 'tha_peif_density', ytitle='THA!cN!di!n'
options, 'thc_peif_density', ytitle='THC!cN!di!n'
;;options, 'th?_pxif_en_elux', ztitle='Eflux'

;; limit
ylim, 'th?_peif_nt', 0.1 , 10 , 1
ylim, 'th?_p?if_en_eflux', 10, 1e+6, 1
ylim, 'th?_peif_density', 0.1 , 10 , 1
ylim, 'th?_peif_avgtemp', 0 , 4, 0
zlim, 'th?_peif_en_eflux', 1e+2, 1e+8 , 1

;; PS
if not keyword_set(ps) then begin
   window, 0, xsize = 700, ysize = 850
endif else begin
   mydevice = !D.NAME
   myplot = !p
   SET_PLOT, 'PS'
   DEVICE, FILENAME=filename+'.ps',/color,bits_per_pixel=8, $
   xsize=17.6, ysize=24.7, xoffset=2., yoffset=2.8 ;;xsize=21.0, ysize=29.7
endelse

tplot, ['the_pxif_en_eflux','the_peif_nt','thc_pxif_en_eflux','thc_peif_nt','thc_beta','thc_peif_velocity_gsm']
tlimit, start_time, end_time
tr='2007-11-12/16:30:00'
timebar,tr

;; output
if not keyword_set(ps) then begin
   makepng, filename
endif else begin
   DEVICE, /CLOSE
   SET_PLOT, mydevice
   !p = myplot
endelse
end