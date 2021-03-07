;download from supermag.jhuapl.edu
;select stations
;download ascii including everything
;this code opens a daily file with 1 min resolution
;
;all sites in the file
;supermag2tplot_ascii,file='20010101-supermag.txt'
;
;selected sites
;supermag2tplot_ascii,file='20010101-supermag.txt',site=['TRO','THY']
;
pro supermag2tplot_ascii,filename=filename,site=site

;stationlist
stationfile=getenv('BIG_DIR')+'/big/GROUND/supermag/20140916-17-01-supermag-stations.txt'
spawn,'tail -n  447 '+stationfile,line
stationcode=strarr(n_elements(line))
glon=fltarr(n_elements(line))
glat=fltarr(n_elements(line))
mlon=fltarr(n_elements(line))
mlat=fltarr(n_elements(line))
stationname=strarr(n_elements(line))
for loop2=0L,n_elements(line)-1 do begin
  result2=strsplit(line[loop2],' ',/extract)
  stationcode[loop2]=result2[0]
  glon[loop2]=result2[1]
  glat[loop2]=result2[2]
  mlon[loop2]=result2[3]
  mlat[loop2]=result2[4]
  stationname[loop2]=result2[5]
endfor
store_data,'supermag_stationlist',data={stationcode:stationcode,glon:glon,glat:glat,mlon:mlon,mlat:mlat,stationname:stationname}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
datafile=filename
nlines=File_Lines(datafile)
OpenR,lun,datafile,/get_lun
temp=""
readF,lun,Format='(a1000)',temp
Close,lun
ncol=N_Elements(StrSplit(temp," ",/Extract))
data3=strarr(nlines)
OpenR,lun,datafile,/get_lun
ReadF,lun,data3
close,/all

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;time 1 min resolution
time=dblarr(1440)
ii=long64(0)
datanum=long64(-1)
while (ii le nlines-1) do begin
  temp=strsplit(data3[ii],string(9B),/extract)
  ;time
  if(strmatch(strmid(data3(ii),0,1),['[0-9]'])) then begin
    datanum=datanum+1
    time[datanum]=time_double(temp[0]+'-'+temp[1]+'-'+temp[2]+'/'+temp[3]+':'+temp[4]+':'+temp[5])
  endif
  ii=ii+1
endwhile

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;station code in the data file
stationcode3=strmid(data3,0,3)
stationcode4=stationcode3[uniq(stationcode3,sort(stationcode3))]
stationcode5=stationcode4[where(strmatch(strmid(stationcode4,0,3),['[A-Z][A-Z,0-9][A-Z,0-9]']))]

if keyword_set(site) then stationcode5=strupcase(site)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;make data array
data_grid_ptr=ptrarr(n_elements(stationcode5),/allocate_heap)
for loop=0L,n_elements(stationcode5)-1 do begin
  *data_grid_ptr[loop]=fltarr(1440,7)*'NaN'
endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;put data into array one by one
datanum=-1
for loop=0L,n_elements(data3)-1 do begin
  if(strmatch(strmid(data3[loop],0,4),['[0-9][0-9][0-9][0-9]'])) then datanum+=1
  
  index=where(strmid(data3[loop],0,3) eq stationcode5,count)
  if count eq 0 then continue
  temp=strsplit(data3[loop],string(9B),/extract)
  (*data_grid_ptr[index[0]])[datanum,*]=temp[1:7]
endfor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;store data
for loop=0L,n_elements(stationcode5)-1 do begin
  index=where(strmatch(stationcode,stationcode5[loop]) eq 1)
  str_element,data_att,'SITE_LATITUDE',glat[index],/add_replace
  str_element,data_att,'SITE_LONGITUDE',glon[index],/add_replace
  store_data,'supermag_'+stationcode5[loop],data={x:time,y:reform((*data_grid_ptr[loop])[*,0:2])},dlim={colors:[2,4,6],data_att:data_att}
endfor
stop
end
