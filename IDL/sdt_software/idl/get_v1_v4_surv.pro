;+
; FUNCTION:
; 	 GET_V1_V4_SURV
;
; DESCRIPTION:
;
;	function to load FAST V1-V4 Survey fields data from the SDT program
; 	shared memory buffers.
;
;	At structure of the following format is returned:
;
;	   DATA_NAME     STRING    'V1-V4 S'           ; Data Quantity name
;	   VALID         INT       1                   ; Data valid flag
; 	   PROJECT_NAME  STRING    'FAST'              ; project name
; 	   UNITS_NAME    STRING    'Counts'            ; Units of this data
; 	   UNITS_PROCEDURE  STRING 'proc'              ; Units conversion proc
;	   START_TIME    DOUBLE    8.0118726e+08       ; Start Time of sample
;	   END_TIME      DOUBLE    7.9850884e+08       ; End time of sample
;	   NPTS          INT       npts                ; Number of time samples
;	   NCOMP         INT       1                   ; Number of components
;	   DEPTH         INT       1                   ; depth of component
;          TIME          DOUBLE    Array(npts)         ; timetags
;	   DATA          FLOAT     Array(npts)         ; Data qauantities
;	   HEADER_BYTES  BYTE      Array(1)	       ; Header bytes (not implemented)
;	   
;	
; CALLING SEQUENCE:
;
; 	data = get_v1_v4_SURV (time1, time2, [NPTS=npts], [START=st | EN=en |
;				PANF=panf | PANB = panb])
;
; ARGUMENTS:
;
;	time1 			This argument gives the start time from
;				which to take data, or, if START or EN keywords
;				are non-zero, the length of time to take data.
;				It may be either a string with the following
;				possible formats:
;					'YY-MM-DD/HH:MM:SS.MSC'  or
;					'HH:MM:SS'     (use reference date)
;				or a number, which will represent seconds
;				since 1970 (must be a double > 94608000.D), or
;				a hours from a reference time, if set.
;
;				Time will always be returned as a double
;				representing the actual data start time found 
;				in seconds since 1970.
;
;	time2			The same as time1, except it represents the
;				end time.
;
;				If the NPTS, START, EN, PANF or PANB keywords 
;				are non-zero, THEN time2 will be ignored as an
;				input paramter.
; KEYWORDS:
;
;	Data time selection is determined from the keywords as given in the 
;	following truth table (NZ == non-zero):
;
; |ALL |NPTS |START| EN  |PANF |PANB |selection                  |use time1|use time2|
; |----|-----|-----|-----|-----|-----|---------------------------|---------|---------|
; | NZ |  0  |  0  |  0  |  0  |  0  | start -> end              |  X      |  X      |
; | 0  |  0  |  0  |  0  |  0  |  0  | time1 -> time2            |  X      |  X      |
; | 0  |  0  |  NZ |  0  |  0  |  0  | start -> time1 secs       |  X      |         |
; | 0  |  0  |  0  |  NZ |  0  |  0  | end-time1 secs -> end     |  X      |         |
; | 0  |  0  |  0  |  0  |  NZ |  0  | pan fwd from time1->time2 |  X      |  X      |
; | 0  |  0  |  0  |  0  |  0  |  NZ | pan back from time1->time2|  X      |  X      |
; | 0  |  NZ |  0  |  0  |  0  |  0  | time1 -> time1+npts       |  X      |         |
; | 0  |  NZ |  NZ |  0  |  0  |  0  | start -> start+npts       |         |         |
; | 0  |  NZ |  0  |  NZ |  0  |  0  | end-npts -> end           |         |         |
;
;	Any other combination of keywords is not allowed.
;
; RETURN VALUE:
;
;	Upon success, the above structure is returned, with the valid tag
;	set to 1.  Upon failure, the valid tag will be 0.
;
; REVISION HISTORY:
;
;	@(#)get_v1_v4_surv.pro	1.4 06/19/96
; 	Originally written by	 Jonathan M. Loran,  University of 
; 	California at Berkeley, Space Sciences Lab.   Jan '96
;-

FUNCTION Get_v1_v4_SURV, time1, time2, NPTS=npts, START=st, EN=en,           $
                      PANF=pf, PANB=pb, ALL=all

   dat = get_ts_from_sdt ('V1-V4_S', 2001, t1=time1, t2=time2, NPTS=npts,    $
                          START=st, EN=en, PANF=pf, PANB=pb, ALL=all)

   IF NOT dat.valid THEN          RETURN, {data_name: 'Null', valid: 0}
   
   ; load up the data into IDL data structs

   ret = 								  $
     {data_name:	dat.data_name, 					  $
       valid: 		dat.valid,					  $
       project_name:	'FAST', 					  $
       units_name: 	'Raw',	 					  $
       values_procedure: 'proc',	 				  $
       start_time:	dat.start_time,					  $
       end_time:	dat.end_time, 					  $
       npts:		dat.npts, 					  $
       ncomp:		dat.ncomp, 					  $
       depth:		dat.depth(0), 					  $
       time:		dat.time,					  $
       data:		dat.comp1,					  $
       header_bytes:	BYTARR(1)}

   RETURN,  ret
END 
