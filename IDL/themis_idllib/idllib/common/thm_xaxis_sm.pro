
pro thm_xaxis_sm,sc=sc,trange=trange

if not keyword_set(trange) then get_timespan,trange

;load
thm_load_state, probe = sc, coord = 'sm',trange=trange
time_clip,'th'+sc+'_state_pos',trange[0],trange[1],newname='th'+sc+'_state_pos'
;position
get_data,strjoin('th'+sc+'_state_pos'),data=tmp
store_data,strjoin('th'+sc+'_state_pos_sm_x'),data={x:tmp.x,y:tmp.y[*,0]/6371.}
options,strjoin('th'+sc+'_state_pos_sm_x'),'ytitle','th'+sc+'_Xsm'
store_data,strjoin('th'+sc+'_state_pos_sm_y'),data={x:tmp.x,y:tmp.y[*,1]/6371.}
options,strjoin('th'+sc+'_state_pos_sm_y'),'ytitle','th'+sc+'_Ysm'
store_data,strjoin('th'+sc+'_state_pos_sm_z'),data={x:tmp.x,y:tmp.y[*,2]/6371.}
options,strjoin('th'+sc+'_state_pos_sm_z'),'ytitle','th'+sc+'_Zsm'

;R,MLT,MLAT
;thm_load_state, probe = sc, coord = 'sm',trange=trange
get_data,strjoin('th'+sc+'_state_pos'),data=tmp
store_data, 'th'+sc+'_R', data={x:tmp.x, y:sqrt(tmp.y[*,0]^2+tmp.y[*,1]^2+tmp.y[*,2]^2)/6371.},dlim={colors:[0],labels:['R'],ysubtitle:'[km]',labflag:1,constant:0,ytitle:'th'+sc+'_R'}
MLT=atan(tmp.y[*,1]/tmp.y[*,0])*180/!pi/15.+12
if(n_elements(where(tmp.y[*,0] lt 0)) gt 1) then MLT[where(tmp.y[*,0] lt 0)]=(atan(tmp.y[where(tmp.y[*,0] lt 0),1]/tmp.y[where(tmp.y[*,0] lt 0),0])+!pi)*180/!pi/15.+12
if(n_elements(where(MLT[*] gt 24)) gt 1) then MLT[where(MLT[*] ge 24)]=MLT[where(MLT[*] ge 24)]-24
store_data, 'th'+sc+'_MLT', data={x:tmp.x, y:MLT},dlim={colors:[0],labels:['MLT'],ysubtitle:'[hour]',labflag:1,constant:0,ytitle:'th'+sc+'_MLT'}
MLAT=atan(tmp.y[*,2]/sqrt(tmp.y[*,0]^2+tmp.y[*,1]^2))*180/!pi
store_data, 'th'+sc+'_MLAT', data={x:tmp.x, y:MLAT},dlim={colors:[0],labels:['MLAT'],ysubtitle:'[deg]',labflag:1,constant:0,ytitle:'th'+sc+'_MLAT'}

;tplot,1,var_label = ['th'+sc+'_state_pos_sm_z', 'th'+sc+'_state_pos_sm_y', 'th'+sc+'_state_pos_sm_x','th'+sc+'_MLT','th'+sc+'_R']
;tplot,1,var_label = ['th'+sc+'_MLT','th'+sc+'_R']

end