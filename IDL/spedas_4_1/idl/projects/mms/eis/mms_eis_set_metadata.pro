;+
;
; PROCEDURE:
;     mms_eis_set_metadata
;
; PURPOSE:
;     This procedure sets some metadata for EIS data products
;
;
; HISTORY:
;     2020 Feb 24 -  Ian Cohen at APL: changed defualt yrange of electronenergy data to scale automatically to accommodate changes to electron energy
;                     channels
;
; $LastChangedBy: egrimes $
; $LastChangedDate: 2020-02-25 10:57:51 -0800 (Tue, 25 Feb 2020) $
; $LastChangedRevision: 28340 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_4_1/projects/mms/eis/mms_eis_set_metadata.pro $
;-
;
pro mms_eis_set_metadata, tplotnames, probe = probe, level=level, data_rate = data_rate, suffix = suffix, datatype = datatype, no_interp=no_interp
    if undefined(probe) then probe = '1'
    if undefined(level) then level = ''
    if undefined(data_rate) then data_rate = 'srvy'
    if undefined(suffix) then suffix = ''
    if undefined(datatype) then datatype = 'extof'
    
    case datatype of 
        'extof': begin
            ylim,'*_extof_proton_flux_omni*', 55, 1000, 1
            ylim,'*_extof_alpha_flux_omni*', 80, 650, 1
            ylim,'*_extof_oxygen_flux_omni*', 145, 950, 1
            zlim, '*_extof_proton_flux_omni*', 0, 0, 1
            zlim, '*_extof_alpha_flux_omni*', 0, 0, 1
            zlim, '*_extof_oxygen_flux_omni*', 0, 0, 1
            options, '*_extof_*_flux_omni*', ystyle=1, yticks=2
            if undefined(no_interp) && data_rate eq 'srvy' then options, '*extof_*_flux_omni*', no_interp=0, y_no_interp=0
            if keyword_set(no_interp) then options, '*extof_*_flux_omni*', no_interp=1, y_no_interp=1 
        end
        'phxtof': begin
            ; note: PHxTOF proton energy depends on version of files
            ; so yrange is set by the p# in the variable name
            ; in the file mms_eis_omni (so it shouldn't be set here)
            
            zlim, '*phxtof_*_flux_omni*', 0, 0, 1
            options, '*_phxtof_*_flux_omni*', ystyle=1
            if undefined(no_interp) && data_rate eq 'srvy' then options, '*phxtof_*_flux_omni*', no_interp=0, y_no_interp=0
            if keyword_set(no_interp) then options, '*phxtof_*_flux_omni*', no_interp=1, y_no_interp=1
        end
        'electronenergy': begin
            ylim,'*_electronenergy_electron_flux_omni*', 0, 0, 1
            zlim, '*_electronenergy_electron_flux_omni*', 0, 0, 1
            options, '*_electronenergy_electron_flux_omni*', ystyle=1, yticks=2
            if undefined(no_interp) && data_rate eq 'srvy' then options, '*_electronenergy_electron_flux_omni*', no_interp=0, y_no_interp=0
            if keyword_set(no_interp) then options, '*_electronenergy_electron_flux_omni*', no_interp=1, y_no_interp=1
        end  
    endcase

end