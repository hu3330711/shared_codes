pro omni_load_hourly

;del_data, '*'

;set time
get_timespan, t
ts=time_struct(t[0])
Syear=''
Smonth=''
Sday=''
Shour=''
Smin=''
Ssec=''
cdum=''
print,time_string(t[0])
reads,time_string(t[0]),Syear,cdum,Smonth,cdum,Sday,cdum,Shour,cdum,Smin,cdum,Ssec,format='(a4,a1,a2,a1,a2,a1,a2,a1,a2,a1,a2)'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

filename=getenv('BIG_DIR')+'/big/SATELLITE/cdaweb.gsfc.nasa.gov/pub/data/omni/low_res_omni/omni2_'+Syear+'.dat'
data_ascii=read_ascii(filename)

str_element,data_ascii,'field1',success=success
if success ge 1 then data_ascii2=data_ascii.field1
str_element,data_ascii,'field01',success=success
if success ge 1 then data_ascii2=data_ascii.field01
str_element,data_ascii,'field001',success=success
if success ge 1 then data_ascii2=data_ascii.field001

Syear=strtrim(string(long(data_ascii2[0,*])),1)
Sdoy=strtrim(string(long(data_ascii2[1,*])),1)
doy_yymmdd_conv2,Syear,Sdoy,Smonth,Sday
Smonth=strtrim(string(Smonth))
Sday=strtrim(string(Sday))
Shour=strtrim(string(long(data_ascii2[2,*])),1)

time=reform(time_double(Syear+'-'+Smonth+'-'+Sday+'/'+Shour))

store_data,'omni_hourly_bx',data={x:time,y:reform(data_ascii2[12,*])},dlim={ysubtitle:'[nT]'}
store_data,'omni_hourly_by',data={x:time,y:reform(data_ascii2[15,*])},dlim={ysubtitle:'[nT]'}
store_data,'omni_hourly_bz',data={x:time,y:reform(data_ascii2[16,*])},dlim={ysubtitle:'[nT]'}
store_data,'omni_hourly_ti',data={x:time,y:reform(data_ascii2[22,*])},dlim={ysubtitle:'[K]'}
store_data,'omni_hourly_ni',data={x:time,y:reform(data_ascii2[23,*])},dlim={ysubtitle:'[cm-3]'}
store_data,'omni_hourly_vi',data={x:time,y:reform(data_ascii2[24,*])},dlim={ysubtitle:'[km/s]'}
store_data,'omni_hourly_pdyn',data={x:time,y:reform(data_ascii2[28,*])},dlim={ysubtitle:'[nPa]'}
store_data,'omni_hourly_e',data={x:time,y:reform(data_ascii2[35,*])},dlim={ysubtitle:'[mV/m]'}
store_data,'omni_hourly_beta',data={x:time,y:reform(data_ascii2[36,*])},dlim={ysubtitle:'[]'}
store_data,'omni_hourly_ma',data={x:time,y:reform(data_ascii2[37,*])},dlim={ysubtitle:'[]'}
store_data,'omni_hourly_kp',data={x:time,y:reform(data_ascii2[38,*])},dlim={ysubtitle:'[]'}
store_data,'omni_hourly_sunspot',data={x:time,y:reform(data_ascii2[39,*])},dlim={ysubtitle:'[]'}
store_data,'omni_hourly_ap',data={x:time,y:reform(data_ascii2[49,*])},dlim={ysubtitle:'[nT]'}
store_data,'omni_hourly_f10.7',data={x:time,y:reform(data_ascii2[50,*])},dlim={ysubtitle:'[]'}
store_data,'omni_hourly_pcn',data={x:time,y:reform(data_ascii2[51,*])},dlim={ysubtitle:'[]'}
end
