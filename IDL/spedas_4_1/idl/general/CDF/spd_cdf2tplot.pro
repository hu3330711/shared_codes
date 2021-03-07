;+
;spd_cdf2tplot ,files,varformat=varformat
;
;Keywords:
; 
; VARFORMAT = PATTERN  ; PATTERN should be a string (wildcards accepted) that will match the CDF variable that should be made into tplot variables
; PREFIX = STRING      ; String that will be pre-pended to all tplot variable names. 
; SUFFIX = STRING      ; String appended to end of each tplot variable created.
; VARNAMES = named variable ; CDF variable names are returned in this variable
; /GET_SUPPORT_DATA    ; Often required to get support data if the CDF file does not have all the needed depend attributes
; /TT2000              ; Keep TT200 time in unsigned long format
; 
; record=record if only one record and not full cdf-file is requested
;
;load_labels=load_labels ;copy labels from labl_ptr_1 in attributes into dlimits
;         resolve labels implemented as keyword to preserve backwards compatibility 
;
; NOTE:
;   The used function spd_cdf_info_to_tplot rely on the order of the variables 
;   in the cdf file. According to ISTP/IACG guidelines "Epoch" should be the 
;   first variable in each CDF data set.
;   https://spdf.gsfc.nasa.gov/istp_guide/variables.html#Epoch    
;   
;
; Author: Davin Larson -  20th century
;   Forked from MMS, 04/09/2018, adrozdov
;
; $LastChangedBy: jwl $
; $LastChangedDate: 2019-04-26 16:18:53 -0700 (Fri, 26 Apr 2019) $
; $LastChangedRevision: 27108 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_4_1/general/CDF/spd_cdf2tplot.pro $
;-

pro spd_cdf2tplot,files,files=files2,prefix=prefix,midfix=midfix,midpos=midpos,suffix=suffix ,newname=newname $
   ,varformat=varformat ,varnames=varnames2 $
   ,all=all,verbose=verbose, get_support_data=get_support_data, convert_int1_to_int2=convert_int1_to_int2 $
   ,record=record, tplotnames=tplotnames,load_labels=load_labels, loaded_versions=loaded_versions $
   ,min_version=min_version,version=version,latest_version=latest_version $
   ,number_records=number_records, center_measurement=center_measurement, major_version=major_version, $
   tt2000=tt2000, disable_cdfcheck=disable_cdfcheck


dprint,dlevel=4,verbose=verbose,'Id:'
vb = keyword_set(verbose) ? verbose : 0

if keyword_set(files2) then files=files2    ; added for backward compatibility  and to make it match the documentation

; Load data from file(s)
dprint,dlevel=4,verbose=verbose,'Starting CDF file load'

if not keyword_set(varformat) then var_type = 'data'
if keyword_set(get_support_data) then var_type = ['data','support_data']

;;;; the following is a filter for CDF file versions (specific to MMS)
files = spd_unh_mms_file_filter(files, min_version=min_version, version=version, $
  latest_version=latest_version, loaded_versions = loaded_versions, major_version=major_version, /no_time)

if ~keyword_set(disable_cdfcheck) then begin
  ; Rename cdf files that cannot be opened (adding the file extension .todelete).
  cant_open = spd_cdf_check_delete(files, /iscdf)
  if n_elements(cant_open) gt 0 then begin
    dprint, dlevel=2, 'spd_cdf2tplot: Error! Some files cannot be opened and were renamed:  ' + cant_open
  endif
endif

cdfi = spd_mms_cdf_load_vars(files,varformat=varformat,var_type=var_type,/spdf_depend, $
     varnames=varnames2,verbose=verbose,record=record, convert_int1_to_int2=convert_int1_to_int2, $
     number_records=number_records)

dprint,dlevel=4,verbose=verbose,'Starting load into tplot'
;  Insert into tplot format
spd_cdf_info_to_tplot,cdfi,varnames2,all=all,prefix=prefix,midfix=midfix,midpos=midpos,suffix=suffix,newname=newname, $  ;bpif keyword_set(all) eq 0
       verbose=verbose,  tplotnames=tplotnames,load_labels=load_labels, center_measurement=center_measurement, tt2000=tt2000

dprint,dlevel=4,verbose=verbose,'Starting Clean up' ;bpif keyword_set(all) eq 0
tplot_ptrs = ptr_extract(tnames(/dataquant))
unused_ptrs = ptr_extract(cdfi,except=tplot_ptrs)
ptr_free,unused_ptrs

end



