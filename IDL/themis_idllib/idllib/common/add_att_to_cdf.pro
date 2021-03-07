;+
; ADD_ATT_TO_FAST_CDF 
; Add "VAR_TYPE" attribute to the variables in a FAST CDF file.
; This may enable cdf2tplot.pro to read them and load the variables 
; as tpot variables. 
;
; This routine has been tested with only FAST high resolution CDF 
; files of the orbit and DC field data. 
;
; Usage: 
;        add_att_to_fast, 'fa_hr_dcmag_04500_19971011114715_7986.cdf
;        cdf2tplot, file='fa_hr_dcmag_04500_19971011114715_7986.cdf'
;
; written by T. Hori on July 1, 2009
;
;-

PRO add_att_to_cdf, fn

npar=N_PARAMS()
IF npar LT 1 THEN RETURN

FOR i=0L,N_ELEMENTS(fn)-1 DO BEGIN

	IF FILE_TEST(fn) NE 1 THEN CONTINUE

	id=cdf_open(fn[i])
	info= cdf_inquire(id)

	;Does VAR_TYPE exist?
	flg=0
	FOR j=0,info.natts-1 DO BEGIN
		cdf_attinq, id, j, name, scope, maxent
		IF STRMID(scope,0,1) EQ 'V' AND STRUPCASE(name) EQ 'VAR_TYPE' THEN flg=1
	ENDFOR
	IF flg EQ 0 THEN attid=cdf_attcreate(id, 'VAR_TYPE' ,/var)
	
	FOR j=0,info.nvars-1 DO BEGIN
		vi= cdf_varinq(id, j)
		cdf_attput, id, 'VAR_TYPE', j, 'data', zvar=vi.is_zvar
	ENDFOR

	FOR j=0,info.nzvars-1 DO BEGIN
		vi= cdf_varinq(id, j, /z)
		cdf_attput, id, 'VAR_TYPE', j, 'data', zvar=vi.is_zvar
	ENDFOR

	cdf_close, id

ENDFOR


RETURN
END

