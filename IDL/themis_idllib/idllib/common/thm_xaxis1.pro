
pro thm_xaxis1,sc=sc

;load
thm_load_state, probe = sc, coord = 'gsm',datatype='pos'

;position
get_data,strjoin('th'+sc+'_state_pos'),data=tmp
store_data,strjoin('th'+sc+'_state_pos_gsm_x'),data={x:tmp.x,y:tmp.y[*,0]/6371.}
options,strjoin('th'+sc+'_state_pos_gsm_x'),'ytitle','th'+sc+'_XGSM'
store_data,strjoin('th'+sc+'_state_pos_gsm_y'),data={x:tmp.x,y:tmp.y[*,1]/6371.}
options,strjoin('th'+sc+'_state_pos_gsm_y'),'ytitle','th'+sc+'_YGSM'
store_data,strjoin('th'+sc+'_state_pos_gsm_z'),data={x:tmp.x,y:tmp.y[*,2]/6371.}
options,strjoin('th'+sc+'_state_pos_gsm_z'),'ytitle','th'+sc+'_ZGSM'

;R,MLT,MLAT
thm_load_state, probe = sc, coord = 'sm',datatype='pos'
get_data,strjoin('th'+sc+'_state_pos'),data=tmp
store_data, 'th'+sc+'_R', data={x:tmp.x, y:sqrt(tmp.y[*,0]^2+tmp.y[*,1]^2+tmp.y[*,2]^2)/6371.},dlim={colors:[0],labels:['R'],ysubtitle:'[km]',labflag:1,constant:0,ytitle:'th'+sc+'_R'}
MLT=atan(tmp.y[*,1]/tmp.y[*,0])*180/!pi/15.+12
if(n_elements(where(tmp.y[*,0] lt 0)) gt 1) then MLT(where(tmp.y[*,0] lt 0))=(atan(tmp.y[where(tmp.y[*,0] lt 0),1]/tmp.y[where(tmp.y[*,0] lt 0),0])+!pi)*180/!pi/15.+12
if(n_elements(where(MLT[*] gt 24)) gt 1) then MLT(where(MLT[*] ge 24))=MLT(where(MLT[*] ge 24))-24
store_data, 'th'+sc+'_MLT', data={x:tmp.x, y:MLT},dlim={colors:[0],labels:['R'],ysubtitle:'[km]',labflag:1,constant:0,ytitle:'th'+sc+'_MLT'}
MLAT=atan(tmp.y[*,2]/sqrt(tmp.y[*,0]^2+tmp.y[*,1]^2))*180/!pi
store_data, 'th'+sc+'_MLAT', data={x:tmp.x, y:MLAT},dlim={colors:[0],labels:['MLAT'],ysubtitle:'[deg]',labflag:1,constant:0,ytitle:'th'+sc+'_MLAT'}

;tplot,1,var_label = ['th'+sc+'_state_pos_gsm_z', 'th'+sc+'_state_pos_gsm_y', 'th'+sc+'_state_pos_gsm_x','th'+sc+'_MLT','th'+sc+'_R']
;tplot,1,var_label = ['th'+sc+'_MLT','th'+sc+'_R']

end