;timespan,'2007-8-1',9.*366
;timespan,['2008-2-1','2016-5-1']
pro supermag_index2tplot

;timespan, '2008-01-14/22:00:00',3,/hours
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

if Syear le 2016 then spawn,'tail --lines=+90 /projectnb/burbsp/big/GROUND/supermag/index/supermag_index_'+Syear+'.txt > test.dat'
if Syear ge 2017 then spawn,'tail --lines=+105 /projectnb/burbsp/big/GROUND/supermag/index/supermag_index_'+Syear+'.txt > test.dat'
print,'reading data file. wait for several seconds...'
  data1=read_ascii('test.dat')
  str_element,data1,'field1',success=success
  if success eq 1 then data1=data1.field1
  if success eq 0 then begin
    str_element,data1,'field01',success=success
    if success eq 1 then data1=data1.field01
  endif
  if success eq 0 then begin
    str_element,data1,'field001',success=success
    if success eq 1 then data1=data1.field001
  endif

Wyear=strtrim(string(reform(long(data1[0,*]))),1)
Wmonth=strtrim(string(reform(long(data1[1,*]))),1)
Wday=strtrim(string(reform(long(data1[2,*]))),1)
Whour=strtrim(string(reform(long(data1[3,*]))),1)
Wmin=strtrim(string(reform(long(data1[4,*]))),1)
Wsec=strtrim(string(reform(long(data1[5,*]))),1)
time=time_double(Wyear+'-'+Wmonth+'-'+Wday+'/'+Whour+':'+Wmin+':'+Wsec)

  SME=reform(data1[6,*])
  SML=reform(data1[7,*])
  SML_MLAT=reform(data1[8,*])
  SML_MLT=reform(data1[9,*])
  SMU=reform(data1[10,*])
  SMU_MLAT=reform(data1[11,*])
  SMU_MLT=reform(data1[12,*])
  SME_station=reform(data1[13,*])
  SMR=reform(data1[14,*])
  SMR00=reform(data1[15,*])
  SMR06=reform(data1[16,*])
  SMR12=reform(data1[17,*])
  SMR18=reform(data1[18,*])
  SMR_station=reform(data1[19,*])

store_data,'supermag_index_SME',data={x:time,y:SME}
store_data,'supermag_index_SMU',data={x:time,y:SMU}
store_data,'supermag_index_SML',data={x:time,y:SML}
store_data,'supermag_index_SMU-SML',data={x:time,y:[[SMU],[SML]]},dlim={constant:0,colors:[6,2]}
store_data,'supermag_index_SMR',data={x:time,y:SMR},dlim={constants:0}
store_data,'supermag_index_SMR_MLT',data={x:time,y:[[SMR00],[SMR06],[SMR12],[SMR18]]},dlim={colors:[0,2,4,6],constants:0}
store_data,'supermag_index_SML_MLAT',data={x:time,y:SML_MLAT}
store_data,'supermag_index_SML_MLT',data={x:time,y:SML_MLT}
store_data,'supermag_index_SMU_MLAT',data={x:time,y:SMU_MLAT}
store_data,'supermag_index_SMU_MLT',data={x:time,y:SMU_MLT}
store_data,'supermag_index_SME_station',data={x:time,y:SME_station}
store_data,'supermag_index_SMR_station',data={x:time,y:SMR_station}

end
