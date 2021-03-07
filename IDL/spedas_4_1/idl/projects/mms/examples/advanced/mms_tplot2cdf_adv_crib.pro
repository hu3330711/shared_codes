;+
;
; This crib sheet shows how to modify the global and variable attributes in CDFs saved with tplot2cdf
;
; See also: mms/examples/advanced/mms_tplot2cdf_crib.pro
;
; $LastChangedBy: egrimes $
; $LastChangedDate: 2019-01-30 13:25:23 -0800 (Wed, 30 Jan 2019) $
; $LastChangedRevision: 26518 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_4_1/projects/mms/examples/advanced/mms_tplot2cdf_adv_crib.pro $
;-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CDF global attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the following example shows how to create a custom global attributes structure for your CDF file
; note: by default, tplot2cdf uses the global attributes structure from the first tplot variable 
;       in the list of tvars
trange=['2015-12-15', '2015-12-16']

mms_load_feeps, trange=trange, /tt2000
mms_feeps_pad

; define the tplot variables to save
tvars = ['mms1_epd_feeps_srvy_l2_electron_intensity_omni', $
  'mms1_epd_feeps_srvy_l2_electron_intensity_omni_spin', $
  'mms1_epd_feeps_srvy_l2_electron_intensity_70-600keV_pad', $
  'mms1_epd_feeps_srvy_l2_electron_intensity_70-600keV_pad_spin']

; define some global attributes; example taken from current FEEPS data
global_attrs = {DATA_TYPE: 'Survey_Level 2', $
  DATA_VERSION: '6.0', $
  DESCRIPTOR: 'FEEPS', $
  DISCIPLINE: 'Space Physics>Magnetospheric Science', $
  GENERATION_DATE: time_string(systime(/seconds), tformat='YYYYMMDD'), $
  INSTRUMENT_TYPE: 'Particles (space)', $
  LOGICAL_FILE_ID: 'mms1_survey_level 2_feeps_20151215_v6.0.1', $
  LOGICAL_SOURCE: 'mms1_feeps_srvy_l1B', $
  LOGICAL_SOURCE_DESCRIPTION: 'Level 2 Flys Eye Energetic Particle Sensor Survey Data', $
  MISSION_GROUP: 'MMS', $
  PI_AFFILIATION: 'LASP', $
  PI_NAME: 'STP', $
  SOURCE_NAME: 'mms1', $
  TEXT: 'http://www.lasp.colorado.edu', $
  LINK_TEXT: 'FEEPS data', $
  LINK_TITLE: 'http://www.lasp.colorado.edu', $
  MODS: 'Generated by SPEDAS'}


; define the tplot variables to save
tvars = ['mms1_epd_feeps_srvy_l2_electron_intensity_omni', $
         'mms1_epd_feeps_srvy_l2_electron_intensity_omni_spin', $
         'mms1_epd_feeps_srvy_l2_electron_intensity_70-600keV_pad', $
         'mms1_epd_feeps_srvy_l2_electron_intensity_70-600keV_pad_spin']

; use the g_attributes keyword to set your custom global attributes
tplot2cdf, tvars=tvars, filename='mms1_survey_level2_feeps_20151215_v6.0.1', g_attributes=global_attrs, /tt2000
stop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CDF variable attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the following example shows how to modify the variable attributes for the vars in your CDF file;
; the CDF variable attributes come from the tplot metadata structure (dlimits.cdf.vatt), e.g.,
get_data, 'mms1_epd_feeps_srvy_l2_electron_intensity_omni_spin', data=d, dlimits=dl
help, dl.cdf.vatt
stop

; modify an attribute directly
dl.cdf.vatt.catdesc = 'MMS1 FEEPS spin averaged omni-directional electron intensity'
help, dl.cdf.vatt
stop

; add a new attribute to the metadata
str_element, dl, 'cdf.vatt.data_rate', 'SRVY', /add 
help, dl.cdf.vatt
stop

; save the updated metadata structure
store_data, 'mms1_epd_feeps_srvy_l2_electron_intensity_omni_spin', data=d, dlimits=dl
stop

; save the CDF with the updated tplot variable
tplot2cdf, tvars=tvars, filename='mms1_survey_level2_feeps_20151215_v6.0.1', /tt2000
stop
end