;+
;PROCEDURE:	get_the_pees
;PURPOSE:	
;	Returns either peer or combined peef-peer data structure at a single time from common generated by thm_load_esa_pkt.pro, for spin resolution combined data
;INPUT:		
;	time:		dbl		time of data to be returned
;
;KEYWORDS:
;	start:		0,1		if set, gets first time in common block
;	en:		0,1		if set, gets last time in common block
;	advance		0,1		if set, gets next time in common block
;	retreat		0,1		if set, gets previous time in common block
;	index		long		gets data at the index value "ind" in common block
;	calib:		0,1		not working yet, allows alternate calibration
;	times		0,1		returns an array of times for all the data, returns 0 if no data
;
;
;CREATED BY:	J. McFadden
;VERSION:	1
;LAST MODIFICATION:  10/04/07
;MOD HISTORY:
;
;NOTES:	  
;	Data structures can be used as inputs to functions such as n_3d.pro, v_3d.pro
;	Or used in conjunction with iterative programs such as get_3dt.pro, get_en_spec.pro
;-
FUNCTION get_the_pees,time,START=st,EN=en,ADVANCE=adv,RETREAT=ret,index=ind,calib=calib,times=times

dat2=get_the_peer(time,START=st,EN=en,ADVANCE=adv,RETREAT=ret,index=ind,calib=calib,times=times)

if keyword_set(times) then return,dat2

dat=get_the_peef(dat2.time)
abs_dt = abs(dat.time-dat2.time)

if abs_dt gt 0.5 then begin
	if abs_dt lt 2. then begin
		dprint, 'get_the_pees: significant spin period time error  ',abs_dt,'  ',time_string(dat.time,/msec),'  ',time_string(dat2.time,/msec)
	endif else begin
;		print,'get_the_pees: missing peer data',time_string(dat.time)
		return,dat2
	endelse
endif
if dat.nenergy eq 32 then return,dat

if dat.nenergy eq 15 and dat2.nbins eq 1 then begin

	dat1 = 	{project_name:		dat.project_name,					$
		spacecraft:		dat.spacecraft, 					$
		data_name:		dat.data_name, 						$
		apid:			dat.apid,						$
		units_name: 		dat.units_name, 					$
		units_procedure: 	dat.units_procedure, 					$
		valid: 			dat.valid, 						$

		time: 			dat.time, 						$
		end_time: 		dat.end_time, 						$
		delta_t: 		dat.delta_t,						$
		integ_t: 		dat.integ_t,						$
		dt_arr: 		replicate(.5,dat2.nenergy)#reform(dat.dt_arr[0,*]),	$

		config1:		dat.config1,						$
		config2:		dat.config2,						$
		an_ind:			dat.an_ind,						$
		en_ind:			dat.en_ind,						$
		mode:			dat.mode,						$						

		nenergy: 		dat2.nenergy, 						$
		energy: 		dat2.energy#replicate(1.,dat.nbins), 			$
		denergy: 		dat2.denergy#replicate(1.,dat.nbins),     		$
		eff: 			fltarr(dat2.nenergy,dat.nbins),	 			$
		bins: 			dat2.bins#replicate(1.,dat.nbins), 			$

		nbins: 			dat.nbins,	 					$
		theta: 			replicate(1.,dat2.nenergy)#reform(dat.theta[0,*]),  	$
		dtheta: 		replicate(1.,dat2.nenergy)#reform(dat.dtheta[0,*]),  	$
		phi: 			replicate(1.,dat2.nenergy)#reform(dat.phi[0,*]) + (11.25*findgen(32)/32.)#replicate(1.,dat.nbins),  				$
		dphi: 			replicate(1.,dat2.nenergy)#reform(dat.dphi[0,*]),	$
;		domega: 		domega,  						$
		gf: 			replicate(0.5,dat2.nenergy)#reform(dat.gf[0,*]),		$

		geom_factor: 		dat.geom_factor, 					$
		dead: 			dat.dead,						$
		mass: 			dat.mass, 						$
		charge: 		dat.charge, 						$
		sc_pot: 		dat.sc_pot, 						$

    eclipse_dphi:    dat.eclipse_dphi,  $

		magf:	 		dat.magf,						$

		bkg:	 		fltarr(dat2.nenergy,dat.nbins),		 		$

		data: 			fltarr(dat2.nenergy,dat.nbins)}

	ind0=indgen(15)
	ind1=2*ind0+1
	ind2=ind1+1

	fr0 = 2.*dat2.eff[0]/(dat2.eff[1]+dat2.eff[2])
	fr1 = 2.*dat2.eff[ind1]/(dat2.eff[ind1]+dat2.eff[ind2])
	fr2 = 2.*dat2.eff[ind2]/(dat2.eff[ind1]+dat2.eff[ind2])
	fr3 = 2.*dat2.eff[31]/(dat2.eff[30]+dat2.eff[29])
	dat1.eff[0,*]    = dat.eff[0,*]   *(fr0#replicate(1.,dat.nbins))
	dat1.eff[ind1,*] = dat.eff[ind0,*]*(fr1#replicate(1.,dat.nbins))
	dat1.eff[ind2,*] = dat.eff[ind0,*]*(fr2#replicate(1.,dat.nbins))
	dat1.eff[31,*]   = dat.eff[14,*]  *(fr3#replicate(1.,dat.nbins))

	fr0 = dat2.bkg[0]/(dat2.bkg[1]+dat2.bkg[2])
	fr1 = dat2.bkg[ind1]/(dat2.bkg[ind1]+dat2.bkg[ind2])
	fr2 = dat2.bkg[ind2]/(dat2.bkg[ind1]+dat2.bkg[ind2])
	fr3 = dat2.bkg[31]/(dat2.bkg[30]+dat2.bkg[29])
	dat1.bkg[0,*]    = dat.bkg[0,*]   *(fr0#replicate(1.,dat.nbins))
	dat1.bkg[ind1,*] = dat.bkg[ind0,*]*(fr1#replicate(1.,dat.nbins))
	dat1.bkg[ind2,*] = dat.bkg[ind0,*]*(fr2#replicate(1.,dat.nbins))
	dat1.bkg[31,*]   = dat.bkg[14,*]  *(fr3#replicate(1.,dat.nbins))


	fr0 = dat2.data[0]/(dat2.data[1]+dat2.data[2])
	fr1 = dat2.data[ind1]/(dat2.data[ind1]+dat2.data[ind2])
	fr2 = dat2.data[ind2]/(dat2.data[ind1]+dat2.data[ind2])
	fr3 = dat2.data[31]/(dat2.data[30]+dat2.data[29])
	dat1.data[0,*]    = dat.data[0,*]   *(fr0#replicate(1.,dat.nbins))
	dat1.data[ind1,*] = dat.data[ind0,*]*(fr1#replicate(1.,dat.nbins))
	dat1.data[ind2,*] = dat.data[ind0,*]*(fr2#replicate(1.,dat.nbins))
	dat1.data[31,*]   = dat.data[14,*]  *(fr3#replicate(1.,dat.nbins))

return,dat1

endif else return,dat2

end
