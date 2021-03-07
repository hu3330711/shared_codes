;+
; FUNCTION:
; 	 GET_FA_SEB6
;
; DESCRIPTION:
;
;	function to load FAST S-esa6 burst data from the SDT program shared
;	memory buffers.
;
;	A structure of the following format is returned:
;
;	   DATA_NAME     STRING    'Sesa6 Burst'        ; Data Quantity name
;	   VALID         INT       1                   ; Data valid flag
; 	   PROJECT_NAME  STRING    'FAST'              ; project name
; 	   UNITS_NAME    STRING    'Counts'            ; Units of this data
; 	   UNITS_PROCEDURE  STRING 'proc'              ; Units conversion proc
;	   TIME          DOUBLE    8.0118726e+08       ; Start Time of sample
;	   END_TIME      DOUBLE    7.9850884e+08       ; End time of sample
;	   INTEG_T       DOUBLE    3.0000000           ; Integration time
;	   NBINS         INT       nbins               ; Number of angle bins
;	   NENERGY       INT       nnrgs               ; Number of energy bins
;	   DATA          FLOAT     Array(nnrgs, nbins) ; Data qauantities
;	   ENERGY        FLOAT     Array(nnrgs, nbins) ; Energy steps
;	   THETA         FLOAT     Array(nnrgs, nbins) ; Angle for bins
;	   GEOM          FLOAT     Array(nbins)        ; Geometry factor
;	   DENERGY       FLOAT     Array(nnrgs, nbins) ; Energies for bins
;	   DTHETA        FLOAT     Array(nbins)        ; Delta Theta
;	   EFF           FLOAT     Array(nnrgs)        ; Efficiency (GF)
;	   MASS          DOUBLE    5.68566e-6          ; Particle Mass
;	   GEOMFACTOR    DOUBLE    0.000625            ; Bin GF
;	   HEADER_BYTES  BYTE      Array(25)	       ; Header bytes
;	   INDEX         LONG      idx		       ; Index into sdt data
;	
; CALLING SEQUENCE:
;
; 	data = get_fa_seb6 (time, [START=start | EN=en | ADVANCE=advance |
;				RETREAT=retreat])
;
; ARGUMENTS:
;
;	time 			This argument gives a time handle from which
;				to take data from.  It may be either a string
;				with the following possible formats:
;					'YY-MM-DD/HH:MM:SS.MSC'  or
;					'HH:MM:SS'     (use reference date)
;				or a number, which will represent seconds
;				since 1970 (must be a double > 94608000.D), or
;				a hours from a reference time, if set.
;
;				time will always be returned as a double
;				representing the actual data time found in
;				seconds since 1970.
;
; KEYWORDS:
;
;	START			If non-zero, get data from the start time
;				of the data instance in the SDT buffers
;
;	EN			If non-zero, get data at the end time
;				of the data instance in the SDT buffers
;
;	ADVANCE			If non-zero, advance to the next data point
;				following the time input
;
;	RETREAT			If non-zero, retreat (reverse) to the previous
;				data point before the time input
;
;	CALIB			If non-zero, caclulate geometry
;				factors for each bin instead of using 1.'s
;
;       INDEX                   Index into sdt data
;
; RETURN VALUE:
;
;	Upon success, the above structure is returned, with the valid tag
;	set to 1.  Upon failure, the valid tag will be 0.
;
; REVISION HISTORY:
;
;	@(#)get_fa_seb6.pro	1.17 07/23/97
; 	Originally written by Jonathan M. Loran,  University of 
; 	California at Berkeley, Space Sciences Lab.   June '95
;-

FUNCTION Get_fa_seb6, inputTime, START=start, EN=en, ADVANCE=advance,  $
                         RETREAT=retreat, CALIB=calib, INDEX=idx

      ; Get samples while dimensions are wrong

      first = 1

      REPEAT BEGIN
          IF NOT first THEN       $
            print, '@(#)get_fa_seb6.pro	1.17: Badly formed data.  Getting next sample'
          first = 0

          dat = get_md_from_sdt ('Sesa 6 Burst', 2001, TIME=inputTime,    $
                                 START = start, EN = en, $
                                 ADVANCE = advance, RETREAT=retreat, INDEX=idx)

          IF NOT dat.valid THEN       RETURN, {data_name: 'Null', valid: 0}

          IF ( keyword_set (en) OR keyword_set (retreat) ) THEN BEGIN
              retreat = 1
              en = 0
          ENDIF ELSE BEGIN
              advance = 1
              start = 0
          ENDELSE

          inputTime = dat.time 

      ENDREP UNTIL md_dims_ok(dat)

      ; get data dat.values into correct dimensions here

      data_name = 'Sesa 6 Burst'
      units_name = 'Counts'
      units_procedure = 'convert_esa_units2'
      IF (where(dat.max2-dat.min2 lt 0))(0) NE -1 THEN      $
        dat.max2(where(dat.max2-dat.min2 lt 0))=dat.max2(where(dat.max2-dat.min2 lt 0))+360
      theta = FLOAT (REPLICATE (1., dat.dimsizes(0)) # ((dat.max2+dat.min2)/2.) mod 360.)
      data = FLOAT (REFORM (dat.values, dat.dimsizes(0), dat.dimsizes(1)))

      IF N_ELEMENTS(dat.max1) EQ 1 THEN BEGIN
          energy = FLOAT (REBIN([(dat.max1+dat.min1)/2.], dat.dimsizes(0), dat.dimsizes(1)))
          denergy = FLOAT (REBIN ([dat.max1 - dat.min1], dat.dimsizes(0), dat.dimsizes(1)))
	ENDIF ELSE BEGIN
            energy = FLOAT (REBIN((dat.max1+dat.min1)/2., dat.dimsizes(0), dat.dimsizes(1)))
            denergy = FLOAT (REBIN (dat.max1 - dat.min1, dat.dimsizes(0), dat.dimsizes(1)))
        ENDELSE

      dtheta = FLOAT (dat.max2 - dat.min2)
      eff = REPLICATE (1., dat.dimsizes(0))
      mass = 5.68566e-6
      geomfactor = .000625

      ; get the header bytes for this time

      hdr_time = inputTime
      
      hdr_dat = get_fa_seb6_hdr (hdr_time, INDEX=idx)
;      hdr_dat = get_fa_seb6_hdr (hdr_time)

      IF hdr_dat.valid EQ 0 THEN BEGIN
         print, 'Error getting Header bytes for this packet.  Bytes will be nil.'
         header_bytes = BYTARR(130)
         got_header_bytes = 0
      ENDIF ELSE BEGIN
         header_bytes = hdr_dat.bytes
         got_header_bytes = 1
      ENDELSE

      ; get geometry factors

      IF NOT keyword_set(calib) THEN  $
          calib = getenv ('FAST_ESA_CALIBRATION')
calib=0
      IF keyword_set(calib) AND got_header_bytes THEN BEGIN
          geom = calc_fa_esa_geom({data_name:		data_name, $
                                   time:		inputTime, $
                                   header_bytes:	header_bytes})
          IF geom(0) GE 0 AND (n_elements(geom) EQ dat.dimsizes(0) * dat.dimsizes(1)) THEN BEGIN
              geom = reform(geom, dat.dimsizes(0), dat.dimsizes(1)) 
          ENDIF ELSE  BEGIN
              PRINT, 'Error getting geom factors for this packet.  Values will be 1.'
              geom = FLOAT(REPLICATE (1., dat.dimsizes(0), dat.dimsizes(1)))
          ENDELSE
      ENDIF ELSE BEGIN
;          geom = FLOAT(REPLICATE (1., dat.dimsizes(0), dat.dimsizes(1)))
          geom = FLOAT(REPLICATE (1., dat.dimsizes(1)))
      ENDELSE

      ; load up the data into IDL data structs

      idx = dat.index

      RETURN,  {data_name:	data_name, 				      $
                 valid: 	1, 					      $
                 project_name:	'FAST',					      $
                 units_name: 	units_name, 				      $
                 units_procedure: units_procedure,			      $
                 time: 		inputTime,				      $
                 end_time: 	dat.endTime,				      $
                 integ_t: 	(dat.endTime - dat.time)/dat.dimsizes(0),     $
                 nbins: 	dat.dimsizes(1), 			      $
                 nenergy: 	dat.dimsizes(0), 			      $
                 data: 		data,					      $
                 energy: 	energy, 				      $
                 theta: 	theta,                                        $
                 geom: 		geom, 	       				      $
                 denergy: 	denergy,       				      $
                 dtheta: 	dtheta, 				      $
                 eff: 		eff,					      $
                 mass: 		mass,					      $
                 geomfactor: 	geomfactor,				      $
                 header_bytes: 	header_bytes,				      $
                 index: 	dat.index}

END 
