;+
; PROCEDURE:
;       mex_marsis_load
; PURPOSE:
;       Loads MEX/MARSIS data
; CALLING SEQUENCE:
;       > timespan,'2015-01-01'
;       > mex_marsis_load
; KEYWORDS:
;       types: specifies data types from 'geometry','eledens_bmag','ionogram' (Def. all)
; CREATED BY:
;       Yuki Harada on 2017-05-03
;
; $LastChangedBy: haraday $
; $LastChangedDate: 2018-04-06 01:38:33 -0700 (Fri, 06 Apr 2018) $
; $LastChangedRevision: 25009 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_4_1/projects/mex/marsis/mex_marsis_load.pro $
;-

pro mex_marsis_load, trange=trange, marsis_user_pass=marsis_up, vvvv_eledens_bmag=vvvv_eledens_bmag, types=types, notclearcom=notclearcom, notplot=notplot, testplot=testplot, no_server=no_server, uiowa=uiowa, public=public,_extra=_ex


if ~keyword_set(trange) then tr = timerange() else tr = time_double(trange)
if keyword_set(vvvv_eledens_bmag) then VVVV = vvvv_eledens_bmag else VVVV = 'V1.0'
if ~keyword_set(types) then types = ['geometry','eledens_bmag','ionogram'] else types = strlowcase(types)
if size(uiowa,/type) eq 0 and strmatch(getenv('HOSTNAME'),'*uiowa.edu*') then uiowa = 1


;;; get user_pass
@mex_marsis_com
marsis_user_pass = getenv('MARSIS_USER_PASS')
if keyword_set(marsis_up) then marsis_user_pass = marsis_up
if ~total(strlen(marsis_user_pass)) and ~size(public,/type) then public = 1



;;; set up file source
s = file_retrieve(/struc)
str_element,s,/add,'remote_data_dir','http://www-pw.physics.uiowa.edu/plasma-wave/marsx/'
str_element,s,/add,'local_data_dir',root_data_dir() + 'mex/'
str_element,s,/add,'user_pass',marsis_user_pass
str_element,s,/add,'last_version',1
str_element,s,/add,'valid_only',1
if keyword_set(no_server) then no_server0 = no_server else no_server0 = 0
str_element,s,/add,'no_server',no_server0
if keyword_set(public) then begin
   s.remote_data_dir = 'ftp://psa.esac.esa.int/pub/mirror/MARS-EXPRESS/MARSIS/'
   s.local_data_dir += 'marsis/'
endif
extract_tags,s,_ex



;;; set up delay times
if size(marsis_delay_times,/type) eq 0 then marsis_delay_times = 167.443 + 91.4286 * indgen(80) ;- cf. http://www-pw.physics.uiowa.edu/plasma-wave/marsx/restricted/MEX-M-MARSIS-3-RDR-AIS-V1.0/CATALOG/AISDS.CAT


;;; get orbit numbers
mex_orbit_num,time=orb_times ;- [orbnum,startapotime,stopapotime,peritime] x N
store_data,'mex_orbnum',data={x:reform(orb_times[3,*]), $
                              y:reform(orb_times[0,*])}
w = where( orb_times[2,*] gt tr[0] and orb_times[1,*] lt tr[1] , nw )
if nw eq 0 then begin
   dprint,'No orbits in the specified time range'
   return
endif
orbnumr = long(minmax( orb_times[0,w] ))



;;;;;; geometry
if total(strmatch(types,'geometry')) gt 0 then begin
if ~keyword_set(notclearcom) then undefine,marsis_geometry
if ~keyword_set(public) then begin
files = ''
for orbnum=orbnumr[0],orbnumr[1] do begin
   ;;; retrieve files
   orbnumx = string(orbnum,f='(i0)')
   strput,orbnumx,'X',strlen(orbnumx)-1
   pf = 'restricted/super/GEOMETRY/ACTIVE_IONOSPHERIC_SOUNDER/RDR'+orbnumx+'/FRM_AIS_RDR_'+string(orbnum,f='(i0)')+'.TAB'
   if keyword_set(uiowa) then begin
      s.local_data_dir = '/opt/project/marsx/'
      pf = 'super/GEOMETRY/ACTIVE_IONOSPHERIC_SOUNDER/RDR'+orbnumx+'/FRM_AIS_RDR_'+string(orbnum,f='(i0)')+'.TAB'
      s.no_server = 1
   endif
   f = file_retrieve(pf,_extra=s)
   s.no_server = no_server0
   if total(strlen(f)) eq 0 then continue
   files = [ files, f ]

   ;;; read in files
   dprint,'reading in '+f
   d = read_csv(f,n_table_header=0)
   ndat = n_elements(d.(0))
   dat = replicate( {orbnum:long(orbnum), $
                     time:!values.d_nan, $
                     sol_lon:!values.f_nan, $
                     subsol_lat:!values.f_nan, $
                     subsol_lon:!values.f_nan, $
                     scsun_dist:!values.f_nan, $
                     alt:!values.f_nan, $
                     lat:!values.f_nan, $
                     lon:!values.f_nan, $
                     loctime:!values.f_nan} , ndat )

   dat.time = time_double(d.(9),tf='YYYY-MM-DDThh:mm:ss.fff')
   dat.sol_lon = d.(11)
   dat.subsol_lat = d.(12)
   dat.subsol_lon = d.(13)      ;- west longitude
   dat.scsun_dist = d.(14)
   dat.alt = d.(27)
   dat.lat = d.(28)
   dat.lon = d.(29)
   dat.loctime = d.(31)

   ;;; store data
   if size(marsis_geometry,/type) eq 0 then marsis_geometry = dat $
   else marsis_geometry = [marsis_geometry,dat]
endfor                          ;- orbnum
if total(strlen(files)) eq 0 then dprint,'geometry files not found'
endif else begin ;- public, from spice
   mex_spice_load,trange=tr
   res = 4d
   ndat = long((tr[1]-tr[0])/res) + 1
   times = tr[0] + res*dindgen(ndat)
   geo = spice_body_pos('MEX', 'MARS', frame='IAU_MARS', utc=times)
   ;;; get areoid altitude
   alt = mvn_get_altitude(reform(geo[0,*]),reform(geo[1,*]),reform(geo[2,*]))
   dat = replicate( {orbnum:0l, $ ;- dummy, not used
                     time:!values.d_nan, $
                     sol_lon:!values.f_nan, $
                     subsol_lat:!values.f_nan, $
                     subsol_lon:!values.f_nan, $
                     scsun_dist:!values.f_nan, $
                     alt:!values.f_nan, $
                     lat:!values.f_nan, $
                     lon:!values.f_nan, $
                     loctime:!values.f_nan} , ndat )
   dat.time = times
   dat.alt = alt                ;- used for radargram
   if size(marsis_geometry,/type) eq 0 then marsis_geometry = dat $
   else marsis_geometry = [marsis_geometry,dat]
endelse                         ;- public
endif
;;;;;; geometry



;;;;;; eledens_bmag
if total(strmatch(types,'eledens_bmag')) gt 0 then begin
if ~keyword_set(notclearcom) then undefine,marsis_eledens_bmag
;;; retrieve files
files = ''
for orbnum=orbnumr[0],orbnumr[1] do begin
   orbnumx = string(orbnum,f='(i5.5)')
   strput,orbnumx,'X',strlen(orbnumx)-1
   exmission = mex_marsis_ex_mission(orbnum) ;- get ith extended mission
   if exmission eq 0 then $           ;- prime mission
      pf = 'restricted/MEX-M-MARSIS-5-DDR-ELEDENS_BMAG-'+VVVV+'/DATA/MARSIS_ELEDENS_BMAG/DDR'+orbnumx+'/ELEDENS_BMAG_DDR_'+string(orbnum,f='(i5.5)')+'.CSV' $
   else pf = 'restricted/MEX-M-MARSIS-5-DDR-ELEDENS_BMAG-EXT'+string(exmission,f='(i0)')+'-'+VVVV+'/DATA/MARSIS_ELEDENS_BMAG/DDR'+orbnumx+'/ELEDENS_BMAG_DDR_'+string(orbnum,f='(i5.5)')+'.CSV'
   if keyword_set(uiowa) then begin
      s.local_data_dir = '/opt/project/marsx/'
      if exmission eq 0 then $  ;- prime mission
         pf = 'volume/MEX-M-MARSIS-5-DDR-ELEDENS_BMAG-'+VVVV+'/DATA/MARSIS_ELEDENS_BMAG/DDR'+orbnumx+'/ELEDENS_BMAG_DDR_'+string(orbnum,f='(i5.5)')+'.CSV' $
      else pf = 'volume/MEX-M-MARSIS-5-DDR-ELEDENS_BMAG-EXT'+string(exmission,f='(i0)')+'-'+VVVV+'/DATA/MARSIS_ELEDENS_BMAG/DDR'+orbnumx+'/ELEDENS_BMAG_DDR_'+string(orbnum,f='(i5.5)')+'.CSV'
      s.no_server = 1
   endif                             ;- uiowa
   if keyword_set(public) then begin ;- psa
      if exmission eq 0 then $       ;- prime mission
         pf = 'MEX-M-MARSIS-5-DDR-ELEDENS-BMAG-'+VVVV+'/DATA/MARSIS_ELEDENS_BMAG/DDR'+orbnumx+'/ELEDENS_BMAG_DDR_'+string(orbnum,f='(i5.5)')+'.CSV' $
      else pf = 'MEX-M-MARSIS-5-DDR-ELEDENS-BMAG-EXT'+string(exmission,f='(i0)')+VVVV+'/DATA/MARSIS_ELEDENS_BMAG/DDR'+orbnumx+'/ELEDENS_BMAG_DDR_'+string(orbnum,f='(i5.5)')+'.CSV'
      ftmp = file_retrieve(pf,_extra=s,no_server=1)
      if total(strlen(ftmp)) eq 0 then $ ;- if no local files
         fff = spd_download(remote_file=pf,ftp_connection_mode=0,_extra=s)
      s.no_server = 1
   endif                        ;- public
   f = file_retrieve(pf,_extra=s)
   s.no_server = no_server0
   if total(strlen(f)) eq 0 then continue
   ;;; error message html?
   openr,unit,f,/get_lun
   flag_html = 0 & line = ''
   for iline=0,2 do begin
      readf,unit,line
      if strmatch(line,'*<html>*') then flag_html = 1
   endfor
   free_lun,unit
   if flag_html then begin
      file_delete,f             ;- clean up
      continue
   endif
   ;;; error message html?
   files = [ files, f ]

   ;;; read in files
   dprint,'reading in '+f
   d = read_csv(f,header=dh,missing=!values.f_nan)
   dat = replicate( {orbnum:long(orbnum), $
                     time:!values.d_nan, $
                     alt:!values.f_nan, $
                     lat:!values.f_nan, $
                     wlon:!values.f_nan, $
                     sza:!values.f_nan, $
                     fpe:!values.f_nan, $
                     fpe_quality:0, $
                     fpe_verification:0, $
                     eledens:!values.f_nan, $
                     tce:!values.f_nan, $
                     fce_quality:0, $
                     bmag:!values.f_nan} ,n_elements(d.(0)) )
   dat.time = time_double(d.(1),tf='YYYY-MM-DDThh:mm:ss.fff')
   dat.alt = d.(3)
   dat.lat = d.(4)
   dat.wlon = d.(7)
   dat.sza = d.(10)
   dat.fpe = d.(11)
   dat.fpe_quality = d.(12)
   dat.fpe_verification = d.(13)
   dat.eledens = d.(14)
   dat.tce = d.(15)
   dat.fce_quality = d.(16)
   dat.bmag = d.(17)

   if size(marsis_eledens_bmag,/type) eq 0 then marsis_eledens_bmag = dat $
   else marsis_eledens_bmag = [ marsis_eledens_bmag, dat ]
endfor                          ;- orbnum
if total(strlen(files)) eq 0 then dprint,'eledens_bmag files not found'
endif
;;;;;; eledens_bmag




;;;;;; ionogram
if total(strmatch(types,'ionogram')) gt 0 then begin
if ~keyword_set(notclearcom) then undefine,marsis_ionograms
files = ''
lfiles = ''
for orbnum=orbnumr[0],orbnumr[1] do begin
   ;;; retrieve files
   orbnumx = string(orbnum,f='(i0)')
   strput,orbnumx,'X',strlen(orbnumx)-1
   pf = 'restricted/super/DATA/ACTIVE_IONOSPHERIC_SOUNDER/RDR'+orbnumx+'/FRM_AIS_RDR_'+string(orbnum,f='(i0)')+'.DAT'
   if keyword_set(uiowa) then begin
      s.local_data_dir = '/opt/project/marsx/'
      pf = 'super/DATA/ACTIVE_IONOSPHERIC_SOUNDER/RDR'+orbnumx+'/FRM_AIS_RDR_'+string(orbnum,f='(i0)')+'.DAT'
      s.no_server = 1
   endif
   if keyword_set(public) then begin                 ;- psa
      exmission = mex_marsis_ex_mission(orbnum,/rdr) ;- get ith extended mission
      if exmission eq 0 then $  ;- prime mission
         pf = 'MEX-M-MARSIS-3-RDR-AIS-'+VVVV+'/DATA/ACTIVE_IONOSPHERIC_SOUNDER/RDR'+orbnumx+'/FRM_AIS_RDR_'+string(orbnum,f='(i0)')+'.DAT' $
      else pf = 'MEX-M-MARSIS-3-RDR-AIS-EXT'+string(exmission,f='(i0)')+'-'+VVVV+'/DATA/ACTIVE_IONOSPHERIC_SOUNDER/RDR'+orbnumx+'/FRM_AIS_RDR_'+string(orbnum,f='(i0)')+'.DAT'
      ftmp = file_retrieve(pf,_extra=s,no_server=1)
      if total(strlen(ftmp)) eq 0 then $ ;- if no local files
         fff = spd_download(remote_file=pf,ftp_connection_mode=0,_extra=s)
      s.no_server = 1
   endif                        ;- public
   f = file_retrieve(pf,_extra=s)
   s.no_server = no_server0

   lpf = 'restricted/super/DATA/ACTIVE_IONOSPHERIC_SOUNDER/RDR'+orbnumx+'/FRM_AIS_RDR_'+string(orbnum,f='(i0)')+'.LBL'
   if keyword_set(uiowa) then begin
      s.local_data_dir = '/opt/project/marsx/'
      lpf = 'super/DATA/ACTIVE_IONOSPHERIC_SOUNDER/RDR'+orbnumx+'/FRM_AIS_RDR_'+string(orbnum,f='(i0)')+'.LBL'
      s.no_server = 1
   endif
   if keyword_set(public) then begin
      exmission = mex_marsis_ex_mission(orbnum,/rdr) ;- get ith extended mission
      if exmission eq 0 then $  ;- prime mission
         lpf = 'MEX-M-MARSIS-3-RDR-AIS-'+VVVV+'/DATA/ACTIVE_IONOSPHERIC_SOUNDER/RDR'+orbnumx+'/FRM_AIS_RDR_'+string(orbnum,f='(i0)')+'.LBL' $
      else lpf = 'MEX-M-MARSIS-3-RDR-AIS-EXT'+string(exmission,f='(i0)')+'-'+VVVV+'/DATA/ACTIVE_IONOSPHERIC_SOUNDER/RDR'+orbnumx+'/FRM_AIS_RDR_'+string(orbnum,f='(i0)')+'.LBL'
      ftmp = file_retrieve(lpf,_extra=s,no_server=1)
      if total(strlen(ftmp)) eq 0 then $ ;- if no local files
         fff = spd_download(remote_file=lpf,ftp_connection_mode=0,_extra=s)
      s.no_server = 1
   endif                        ;- public
   lf = file_retrieve(lpf,_extra=s)
   s.no_server = no_server0

   if total(strlen(f)) eq 0 or total(strlen(lf)) eq 0 then continue
   ;;; error message html?
   openr,unit,f,/get_lun
   flag_html = 0 & line = ''
   for iline=0,2 do begin
      readf,unit,line
      if strmatch(line,'*<html>*') then flag_html = 1
   endfor
   free_lun,unit
   if flag_html then begin
      file_delete,f             ;- clean up
      file_delete,lf            ;- clean up
      continue
   endif
   ;;; error message html?

   files = [ files, f ]
   lfiles = [ lfiles, lf ]

   ;;; read in files
   dlbl = read_ascii(lf)
   nrow = long(dlbl.(0)[2,3])  ;- get the number of rows from the label file
   ntime = long(nrow/160)

   dat = replicate( {orbnum:long(orbnum), $
                     time:!values.d_nan, $
                     freq:make_array(value=!values.f_nan,160), $
                     spec:make_array(value=!values.f_nan,160,80)} , ntime )

   dprint,'reading in '+f
   openr,unit,f,/get_lun
   for irow=0l,nrow-1 do begin
      bytdum = bytarr(400)
      readu,unit,bytdum
      ;;; get useful info (cf: http://www-pw.physics.uiowa.edu/plasma-wave/marsx/restricted/super/LABEL/AIS_FORMAT.FMT)
      SCET_STRING = string(bytdum[24:48])
      TRANSMIT_POWER = bytdum[59]
      FREQUENCY_TABLE_NUMBER = bytdum[60]
      FREQUENCY_NUMBER = bytdum[61]
      BAND_NUMBER = bytdum[62]
      RECEIVER_ATTENUATION = bytdum[63]
      FREQUENCY = float(reverse(bytdum[76:79]),0)
      SPECTRAL_DENSITY = reverse(float(reverse(bytdum[80:399]),0,80))

      itime = long(irow/160)
      ifreq = long(irow mod 160)

      dat[itime].time = time_double(SCET_STRING,tf='YYYY-DOYThh:mm:ss.fff')
      dat[itime].freq[ifreq] = FREQUENCY
      dat[itime].spec[ifreq,*]= SPECTRAL_DENSITY
   endfor
   free_lun,unit

   ;;; store data
   if size(marsis_ionograms,/type) eq 0 then marsis_ionograms = dat $
   else marsis_ionograms = [marsis_ionograms,dat]
endfor                          ;- orbnum
if total(strlen(files)) eq 0 then dprint,'ionogram files not found'
endif
;;;;;; ionogram



;;; generate tplot variables
if ~keyword_set(notplot) then mex_marsis_tplot
if keyword_set(notclearcom) then tplot_sort,'mex_marsis_*'

;;; test plot
if keyword_set(testplot) then $
   tplot,['mex_marsis_freq_sdens','mex_marsis_aalt_sdens', $
          'mex_marsis_eledens','mex_marsis_bmag', $
          'mex_marsis_alt','mex_eph_sza'],var='mex_orbnum'


end
