;+
;PROCEDURE:	gen_fa_k0_ies_gifps.pro
;INPUT:	none
;
;PURPOSE:
;	Generates gif and ps FAST ion key parameter data.
;
;	Plot 1: Ion Differential Energy Flux vs Energy, 0-30    deg pitch angle 
;	Plot 2: Ion Differential Energy Flux vs Energy, 40-140  deg pitch angle 
;	Plot 3: Ion Differential Energy Flux vs Energy, 150-180 deg pitch angle 
;	Plot 4: Ion Differential Energy Flux vs Pitch Angle, .05-1. keV  
;	Plot 5: Ion Differential Energy Flux vs Pitch Angle, 1.-25. keV  
;	Plot 6: Ion Energy Flux - mapped to 100 km, positive earthward  
;	Plot 7: Ion Flux - mapped to 100 km, positive earthward  
;
;KEYWORDS
;	bw 		If set, grayscale postscript plots generated
;	k0		If set, output files names are 'fa_k0_ies_XXXXX.ext'
;				where ext = ps,gif and XXXXX = padded orbit
;				files sent to /disks/juneau/www/WWWSUM/yyyy_mm_dd_tttttt/
;			If not set, output files names are 'fa_ies_XXXXX.ext'
;				files sent to local directory
;
;NOTES:	
;	Run load_fa_k0_ies.pro first to get the k0 data
;
;CREATED BY:	J.McFadden		96-9-24
;VERSION:	1
;LAST MODIFICATION:  97/03/04
;MOD HISTORY:	
;		96/10/08	k0 keyword added
;		97/03/04	color=4 used for positive (earthward) Ji,JEi; color=6 used for negative Ji,JEi
;				upgrade for Ji,JEi definition changes - mapped to 100 km now
;-
pro gen_fa_k0_ies_gifps,bw=bw,k0=k0

g_old_device = !d.name

; Generate postscript and gif files for 20 minute intervals prior and post the
;	highest invariant latitudes

	get_data,'ion_0',data=tmp
	tmin=min(tmp.x)
	tmax=max(tmp.x)
	get_data,'ORBIT',data=tmp
	orb=tmp.y(0)
	orbit = STRMID( STRCOMPRESS( orb + 1000000, /RE), 2, 5)
	orbit_num=strcompress(string(tmp.y(0)),/remove_all)
	get_data,'ILAT',data=ilat
	maxilat = max(ilat.y,max_sub)
	minilat = min(ilat.y,min_sub)

	web_path='/disks/juneau/www/WWWSUM'
 	if maxilat gt 60 then begin

		if max_sub eq 0 or max_sub eq dimen1(ilat.y)-1 then begin
			delta_t = 0
		endif else begin
			f0 = ilat.y(max_sub)
			fp = ilat.y(max_sub+1)
			fm = ilat.y(max_sub-1)
			dt = ilat.x(max_sub) - ilat.x(max_sub-1)
			delta_t = (dt/2)*(fm-fp)/(fp+fm-2.*f0)
		endelse
		t1 = ilat.x(max_sub) + delta_t 
		t2 = t1 - 1200

; make ps
		if t2 lt tmin then get_fa_orbit,t2,tmax,orbit_file=orbit_file
		timespan,t2,20,/minutes
		if keyword_set(bw) then ct=42 else ct=39
		if keyword_set(k0) then begin
			output=fa_web_path(web_path,t2,'k0','ies',orb,'in','')
		endif else output='fa_ies_'+orbit+'_in'
		popen,output,ct=ct,/port,/color
		tplot,['ion_0','ion_90','ion_180','ion_low','ion_high','JEi','Ji'] $
		,var_label=['MLT','ALT','ILAT'],title='FAST Ions  Orbit '+orbit_num
		if keyword_set(bw) then begin
			tplot_panel,var='JEi',deltat=dt
			get_data,'JEi',data=dat
			oplot,dat.x-dt,dat.y,thick=2
			oplot,dat.x-dt,-dat.y,thick=1
		endif
		if keyword_set(bw) then begin
			tplot_panel,var='Ji',deltat=dt
			get_data,'Ji',data=dat
			oplot,dat.x-dt,dat.y,thick=2
			oplot,dat.x-dt,-dat.y,thick=1
		endif
		pclose

; make gif
		set_plot,'Z'
		device,set_resolution=[512,768]
		device,set_character_size=[6,10]
		loadct2,39
		tplot
		tvlct,r,g,b,/get
		if keyword_set(k0) then begin
			output=fa_web_path(web_path,t2,'k0','ies',orb,'in','gif')
		endif else output='fa_ies_'+orbit+'_in.gif'
		write_gif,output,tvrd(),r,g,b

; make ps
		if t1+1200 gt tmax then get_fa_orbit,tmin,t1+1200,orbit_file=orbit_file
		timespan,t1,20,/minutes
		if keyword_set(bw) then ct=42 else ct=39
		if keyword_set(k0) then begin
			output=fa_web_path(web_path,t1,'k0','ies',orb,'on','')
		endif else output='fa_ies_'+orbit+'_on'
		popen,output,ct=ct,/port,/color
		tplot,['ion_0','ion_90','ion_180','ion_low','ion_high','JEi','Ji'] $
		,var_label=['MLT','ALT','ILAT'],title='FAST Ions  Orbit '+orbit_num
		if keyword_set(bw) then begin
			tplot_panel,var='JEi',deltat=dt
			get_data,'JEi',data=dat
			oplot,dat.x-dt,dat.y,thick=2
			oplot,dat.x-dt,-dat.y,thick=1
		endif
		if keyword_set(bw) then begin
			tplot_panel,var='Ji',deltat=dt
			get_data,'Ji',data=dat
			oplot,dat.x-dt,dat.y,thick=2
			oplot,dat.x-dt,-dat.y,thick=1
		endif
		pclose

; make gif
		set_plot,'Z'
		device,set_resolution=[512,768]
		device,set_character_size=[6,10]
		loadct2,39
		tplot
		tvlct,r,g,b,/get
		if keyword_set(k0) then begin
			output=fa_web_path(web_path,t1,'k0','ies',orb,'on','gif')
		endif else output='fa_ies_'+orbit+'_on.gif'
		write_gif,output,tvrd(),r,g,b

	endif

	if minilat lt -60 then begin

		if min_sub eq 0 or min_sub eq dimen1(ilat.y)-1 then begin
			delta_t = 0
		endif else begin
			f0 = ilat.y(min_sub)
			fp = ilat.y(min_sub+1)
			fm = ilat.y(min_sub-1)
			dt = ilat.x(min_sub) - ilat.x(min_sub-1)
			delta_t = (dt/2)*(fm-fp)/(fp+fm-2.*f0)
		endelse
		t1 = ilat.x(min_sub) + delta_t 
		t2 = t1 - 1200

; make ps
		if t2 lt tmin then get_fa_orbit,t2,tmax,orbit_file=orbit_file
		timespan,t2,20,/minutes
		if keyword_set(bw) then ct=42 else ct=39
		if keyword_set(k0) then begin
			output=fa_web_path(web_path,t2,'k0','ies',orb,'is','')
		endif else output='fa_ies_'+orbit+'_is'
		popen,output,ct=ct,/port,/color
		tplot,['ion_0','ion_90','ion_180','ion_low','ion_high','JEi','Ji'] $
		,var_label=['MLT','ALT','ILAT'],title='FAST Ions  Orbit '+orbit_num
		if keyword_set(bw) then begin
			tplot_panel,var='JEi',deltat=dt
			get_data,'JEi',data=dat
			oplot,dat.x-dt,dat.y,thick=2
			oplot,dat.x-dt,-dat.y,thick=1
		endif
		if keyword_set(bw) then begin
			tplot_panel,var='Ji',deltat=dt
			get_data,'Ji',data=dat
			oplot,dat.x-dt,dat.y,thick=2
			oplot,dat.x-dt,-dat.y,thick=1
		endif
		pclose

; make gif
		set_plot,'Z'
		device,set_resolution=[512,768]
		device,set_character_size=[6,10]
		loadct2,39
		tplot
		tvlct,r,g,b,/get
		if keyword_set(k0) then begin
			output=fa_web_path(web_path,t2,'k0','ies',orb,'is','gif')
		endif else output='fa_ies_'+orbit+'_is.gif'
		write_gif,output,tvrd(),r,g,b

; make ps
		if t1+1200 gt tmax then get_fa_orbit,tmin,t1+1200,orbit_file=orbit_file
		timespan,t1,20,/minutes
		if keyword_set(bw) then ct=42 else ct=39
		if keyword_set(k0) then begin
			output=fa_web_path(web_path,t1,'k0','ies',orb,'os','')
		endif else output='fa_ies_'+orbit+'_os'
		popen,output,ct=ct,/port,/color
		tplot,['ion_0','ion_90','ion_180','ion_low','ion_high','JEi','Ji'] $
		,var_label=['MLT','ALT','ILAT'],title='FAST Ions  Orbit '+orbit_num
		if keyword_set(bw) then begin
			tplot_panel,var='JEi',deltat=dt
			get_data,'JEi',data=dat
			oplot,dat.x-dt,dat.y,thick=2
			oplot,dat.x-dt,-dat.y,thick=1
		endif
		if keyword_set(bw) then begin
			tplot_panel,var='Ji',deltat=dt
			get_data,'Ji',data=dat
			oplot,dat.x-dt,dat.y,thick=2
			oplot,dat.x-dt,-dat.y,thick=1
		endif
		pclose

; make gif
		set_plot,'Z'
		device,set_resolution=[512,768]
		device,set_character_size=[6,10]
		loadct2,39
		tplot
		tvlct,r,g,b,/get
		if keyword_set(k0) then begin
			output=fa_web_path(web_path,t1,'k0','ies',orb,'os','gif')
		endif else output='fa_ies_'+orbit+'_os.gif'
		write_gif,output,tvrd(),r,g,b


	endif

set_plot,g_old_device

return
end
