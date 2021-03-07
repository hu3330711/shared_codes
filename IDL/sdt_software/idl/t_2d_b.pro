;+
;FUNCTION:	t_2d_b(dat,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins)
;INPUT:	
;	dat:	structure,	2d data structure filled by get_eesa_surv, get_eesa_burst, etc.
;KEYWORDS
;	ENERGY:	fltarr(2),	optional, min,max energy range for integration
;	ERANGE:	fltarr(2),	optional, min,max energy bin numbers for integration
;	EBINS:	bytarr(na),	optional, energy bins array for integration
;					0,1=exclude,include,  
;					na = dat.nenergy
;	ANGLE:	fltarr(2),	optional, min,max pitch angle range for integration
;	ARANGE:	fltarr(2),	optional, min,max angle bin numbers for integration
;	BINS:	bytarr(nb),	optional, angle bins array for integration
;					0,1=exclude,include,  
;					nb = dat.ntheta
;	BINS:	bytarr(na,nb),	optional, energy/angle bins array for integration
;					0,1=exclude,include
;PURPOSE:
;	Returns the temperature, [Tx,Ty,Tz,Tavg], eV, assumes a narrow (< 5 deg) field aligned beam
;NOTES:	
;	Function calls p_2d_b.pro and n_2d_b.pro
;	Function normally called by "get_2dt.pro" to generate 
;	time series data for "tplot.pro".
;
;CREATED BY:
;	J.McFadden	97-8-20		Created from t_2d.pro
;					Treats narrow beams correctly, no do loops
;LAST MODIFICATION:
;	97-8-20		J.McFadden
;-
function t_2d_b,dat2,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins

Tx = 0.
Ty = 0.
Tz = 0.
Tavg = 0.

if dat2.valid eq 0 then begin
	print,'Invalid Data'
	return, [Tx,Ty,Tz,Tavg]
endif

press = p_2d_b(dat2,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins)
density = n_2d_b(dat2,ENERGY=en,ERANGE=er,EBINS=ebins,ANGLE=an,ARANGE=ar,BINS=bins)
Tavg = (press(0)+press(1)+press(2))/(density*3.)
Tx = press(0)/(density)
Ty = press(1)/(density)
Tz = press(2)/(density)

return, [Tx,Ty,Tz,Tavg]

; temperature in eV

end

