pro thm_mosaic_array_all_readonly,station_string,thumb=thumb,flat_cal=flat_cal

get_timespan,t

if not keyword_set(flat_cal) then flat_cal=0.
flat_cal=float(flat_cal)

       if keyword_set(thumb) then begin
         datatype='ast'
         thm_load_asi_all,site=station_string,trange=t,datatype='ast'
       endif else begin
         datatype='asf'
         thm_load_asi_all,site=station_string,trange=t,datatype='asf'
       endelse

       thm_load_asi_cal,station_string,cal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         if keyword_set(thumb) then begin
            lon_name='thg_ast_'+station_string+'_glon' 
            field_index=where(cal.vars.name eq lon_name)
            x=*cal.vars[field_index[0]].dataptr
            lat_name='thg_ast_'+station_string+'_glat'
            field_index=where(cal.vars.name eq lat_name)
            y=*cal.vars[field_index[0]].dataptr
            ele_name='thg_ast_'+station_string+'_elev'
            field_index=where(cal.vars.name eq ele_name)
            elev=*cal.vars[field_index[0]].dataptr
         endif ELSE begin 
            lon_name='thg_asf_'+station_string+'_glon'
            field_index=where(cal.vars.name eq lon_name)
            x=*cal.vars[field_index[0]].dataptr
            x=reform(x[1,*,*])
            lat_name='thg_asf_'+station_string+'_glat'
            field_index=where(cal.vars.name eq lat_name)
            y=*cal.vars[field_index[0]].dataptr
            y=reform(y[1,*,*])
            ele_name='thg_asf_'+station_string+'_elev'
            field_index=where(cal.vars.name eq ele_name)
            elev=*cal.vars[field_index[0]].dataptr
            endelse
store_data,lon_name,data={x:t[0],y:x}
store_data,lat_name,data={x:t[0],y:y}
store_data,ele_name,data={x:t[0],y:elev}


           max_number=255
           ;corx=fltarr(max_number+1,max_number+1)
           ;cory=fltarr(max_number+1,max_number+1)
           medx=fltarr(max_number+1,max_number+1)
           medy=fltarr(max_number+1,max_number+1)
           for j1=0,max_number do for i1=0,max_number do begin
; (x,y)=(lon,lat)
; 1(50,50) 2(51,51)
; 0(50,50) 3(51,50)
;
             medx(i1,j1)=median(x[[i1,i1,i1+1,i1+1],[j1,j1+1,j1+1,j1]])
             medy(i1,j1)=median(y[[i1,i1,i1+1,i1+1],[j1,j1+1,j1+1,j1]])
           endfor


           n1=256l*256l
           cor=fltarr(max_number+1,max_number+1,4,2)
           k1=0l
           for j1=0,max_number do for i1=0,max_number do begin
             corx=x[[i1,i1,i1+1,i1+1],[j1,j1+1,j1+1,j1]]
             cory=y[[i1,i1,i1+1,i1+1],[j1,j1+1,j1+1,j1]]
             cor(i1,j1,*,0)=corx
             cor(i1,j1,*,1)=cory
             k1=k1+1l
             endfor

store_data,lon_name+'_center',data={x:t[0],y:medx}
store_data,lat_name+'_center',data={x:t[0],y:medy}
store_data,'thg_'+datatype+'_'+station_string+'_corners',data={x:t[0],y:cor}

;flat cal
if keyword_set(flat_cal) then begin
  print,'flat_cal'
  get_data,'thg_'+datatype+'_'+station_string,data=data
  get_data,'thg_'+datatype+'_'+station_string+'_elev',data=elev
  temp=1/90.*flat_cal
  for loop=0,n_elements(data.x)-1,1 do begin
    temp2=reform(data.y(loop,*,*))
    temp2+=abs(elev.y(*,*))*temp
    data.y(loop,*,*)=temp2
  endfor
  store_data,'thg_'+datatype+'_'+station_string,data=data
endif

end