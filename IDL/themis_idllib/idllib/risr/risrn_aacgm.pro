
pro risrn_aacgm,mode=mode

get_timespan,t
Syear=''
Smonth=''
Sday=''
Shour=''
Smin=''
Ssec=''
cdum=''
print,time_string(t[0])
reads,time_string(t[0]),Syear,cdum,Smonth,cdum,Sday,cdum,Shour,cdum,Smin,cdum,Ssec,format='(a4,a1,a2,a1,a2,a1,a2,a1,a2,a1,a2)'
Syear2=strmid(Syear,2,2)
Smin2=strmid(Smin,0,1)+'*'

;Load AACGM libs
aacgmidl


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Get the s/c position in AACGM
tplot_names,'risrn_beam_*_alt_'+mode+'*_beam??',names=names
for beam=0,n_elements(names)-1,1 do begin
  get_data,names(beam),data=data,index=index
	index=where(data.glat lt 0,count)
	maglat=data.glat & maglon=data.glat & mlt=data.glat & alt_foot=data.glat & glat_foot=data.glat & glon_foot=data.glat ;Make arrays of the same size
	ts=time_struct(data.x)
	if not keyword_set(noaacgm) then begin
		FOR j=0l,N_ELEMENTS(data.glat)-1 DO BEGIN
			if (j mod 1000 eq 0) then print,j,N_ELEMENTS(data.x)-1
			cnv_aacgm,data.glat[j],data.glong[j],data.v[j],lat,lon,r,error
			maglat[j]=lat & maglon[j]=(lon lt 0? lon+360 : lon)
			yrsec=cnvtime(ts[0].year,ts[0].month,ts[0].date,ts[0].hour,ts[0].min,ts[0].sec)
			mlt[j]=calc_mlt(ts[0].year,yrsec,maglon[j])
			alt_foot[j]=110.
			cnv_aacgm, maglat[j], maglon[j], alt_foot[j], glat_foot2,glon_foot2,r,error,/geo
			glat_foot[j]=glat_foot2
			glon_foot[j]=glon_foot2
		ENDFOR
        str_element,data,'aacgmmlat',maglat,/add_replace
        str_element,data,'aacgmmlon',maglon,/add_replace
        str_element,data,'aacgmglat',glat_foot,/add_replace
        str_element,data,'aacgmglon',glon_foot,/add_replace
        store_data,names(beam),data=data
	endif
endfor

tplot_names,'risrn_beam_*_mlat_'+mode+'*_beam??',names=names
if n_elements(names) ge 2 then begin
  for beam=0,n_elements(names)-1,1 do begin
    get_data,names(beam),data=data,index=index
  	index=where(data.glat lt 0,count)
  	maglat=data.glat & maglon=data.glat & mlt=data.glat & alt_foot=data.glat & glat_foot=data.glat & glon_foot=data.glat ;Make arrays of the same size
  	ts=time_struct(data.x)
  	if not keyword_set(noaacgm) then begin
  		FOR j=0l,N_ELEMENTS(data.glat)-1 DO BEGIN
  			if (j mod 1000 eq 0) then print,j,N_ELEMENTS(data.x)-1
  			cnv_aacgm,data.glat[j],data.glong[j],data.alt[j],lat,lon,r,error
  			maglat[j]=lat & maglon[j]=(lon lt 0? lon+360 : lon)
  			yrsec=cnvtime(ts[0].year,ts[0].month,ts[0].date,ts[0].hour,ts[0].min,ts[0].sec)
  			mlt[j]=calc_mlt(ts[j].year,yrsec,maglon[j])
  			alt_foot[j]=110.
  			cnv_aacgm, maglat[j], maglon[j], alt_foot[j], glat_foot2,glon_foot2,r,error,/geo
  			glat_foot[j]=glat_foot2
  			glon_foot[j]=glon_foot2
  		ENDFOR
          str_element,data,'aacgmmlat',maglat,/add_replace
          str_element,data,'aacgmmlon',maglon,/add_replace
          str_element,data,'aacgmglat',glat_foot,/add_replace
          str_element,data,'aacgmglon',glon_foot,/add_replace
          store_data,names(beam),data=data
  	endif
  endfor
endif

end
