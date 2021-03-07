;thg_map_aacgm_lat_contour(2,*,*)--(0glat, 1glon), (180 latitude bins), (num_lon_grid points along the isolat contour)
;thg_map_aacgm_lon_contour(2,*,*)--(0glat, 1glon), (360 longitude bins), (num_lat_grid points along the isolon contour)

pro aacgm_grids2

get_timespan,t

;Load AACGM libs
aacgmidl

dmlon=1.
dmlat=1.
num_lon_grid=long(360./dmlon)+1
num_lat_grid=long(180./dmlat)+1
r=1.02
alt=110.

;FCHU 
glat=58.76
glon=265.91
cnv_aacgm,glat,glon,alt,aacgmmlat,aacgmmlon
fchu_aacgmmlon=aacgmmlon


thg_map_aacgm_lon_contour=fltarr(2,num_lon_grid,num_lat_grid)
thg_map_aacgm_lon_contour(*)='NaN'

;aacgm_lon_contour--NH
for loop=0,num_lon_grid-1,1 do begin
  aacgmmlon=loop*dmlon+fchu_aacgmmlon
  if loop ge 360 then aacgmmlon=(loop mod 360)*dmlon+fchu_aacgmmlon
print,loop,num_lon_grid,aacgmmlon
  aacgmmlon_temp=fltarr(num_lat_grid/2)
  aacgmmlon_temp(*)=aacgmmlon
  aacgmmlon_temp=aacgmmlon_temp mod 360
  aacgmmlat=(findgen(num_lat_grid/2)+num_lat_grid/2+1)*dmlat-90.
  r_temp=fltarr(num_lat_grid/2)
  r_temp(*)=r
  glat=fltarr(num_lat_grid/2)
  glon=fltarr(num_lat_grid/2)
  alt_temp=fltarr(num_lat_grid/2)
  alt_temp(*)=alt
  ;for loop2=0,num_lat_grid/2-1,1 do cnv_aacgm,aacgmmlat(loop2),aacgmmlon_temp(loop2),alt_temp(loop2),glat(loop2),glon(loop2),r_temp(loop2),error;,/geo
aacgm_conv_coord, aacgmmlat,aacgmmlon_temp,alt_temp,glat,glon,error,/to_geo
  thg_map_aacgm_lon_contour(1,loop,num_lat_grid/2+1:num_lat_grid-1)=glat
  thg_map_aacgm_lon_contour(0,loop,num_lat_grid/2+1:num_lat_grid-1)=glon
  where_temp=where(error ne 0,count)
  if count ge 1 then begin
temp=where(aacgmmlat(where_temp) ge 10,count1)
if (count1 ge 1) then print,aacgmmlon(where_temp),aacgmmlat(where_temp)
    thg_map_aacgm_lon_contour(1,loop,num_lat_grid/2+1+where_temp)='NaN'
    thg_map_aacgm_lon_contour(0,loop,num_lat_grid/2+1+where_temp)='NaN'
  endif
endfor

;aacgm_lon_contour--SH
for loop=0,num_lon_grid-1,1 do begin
  aacgmmlon=loop*dmlon+fchu_aacgmmlon
print,loop,num_lon_grid,aacgmmlon
  aacgmmlon_temp=fltarr(num_lat_grid/2+1)
  aacgmmlon_temp(*)=aacgmmlon
  aacgmmlon_temp=aacgmmlon_temp mod 360
  aacgmmlat=findgen(num_lat_grid/2+1)*dmlat-90.
  r_temp=fltarr(num_lat_grid/2+1)
  r_temp(*)=r
  glat=fltarr(num_lat_grid/2+1)
  glon=fltarr(num_lat_grid/2+1)
  alt_temp =fltarr(num_lat_grid/2+1)
  alt_temp(*)=alt
  ;for loop2=0,num_lat_grid/2-1,1 do cnv_aacgm,aacgmmlat(loop2),aacgmmlon_temp(loop2),alt(loop2),glat(loop2),glon(loop2),r_temp(loop2);,/geo
  aacgm_conv_coord, aacgmmlat,aacgmmlon_temp,alt_temp,glat,glon,error,/to_geo
  thg_map_aacgm_lon_contour(1,loop,0:num_lat_grid/2)=glat
  thg_map_aacgm_lon_contour(0,loop,0:num_lat_grid/2)=glon
  where_temp=where(error ne 0,count)
  if count ge 1 then begin
    thg_map_aacgm_lon_contour(1,loop,where_temp)='NaN'
    thg_map_aacgm_lon_contour(0,loop,where_temp)='NaN'
  endif
endfor

;aacgm_lat_contour
thg_map_aacgm_lat_contour=transpose(thg_map_aacgm_lon_contour,[0,2,1])


;aacgm_lat_contour
thg_map_aacgm_lat_contour=fltarr(2,num_lat_grid,num_lon_grid)
thg_map_aacgm_lat_contour(*)='NaN'

for loop=0,num_lat_grid-1,1 do begin
  aacgmmlat=loop*dmlat-90.
print,loop,num_lat_grid,aacgmmlat
  aacgmmlat_temp=fltarr(num_lon_grid)
  aacgmmlat_temp(*)=aacgmmlat
  aacgmmlon=findgen(num_lon_grid)*dmlon+fchu_aacgmmlon
  aacgmmlon=aacgmmlon mod 360
  r_temp=fltarr(num_lon_grid)
  r_temp(*)=r
  glat=fltarr(num_lon_grid)
  glon=fltarr(num_lon_grid)
  alt_temp =fltarr(num_lon_grid)
  alt_temp(*)=alt
  ;for loop2=0,num_lon_grid-1,1 do cnv_aacgm,aacgmmlat_temp(loop2),aacgmmlon(loop2),alt(loop2),glat(loop2),glon(loop2),r_temp(loop2);,/geo
  aacgm_conv_coord, aacgmmlat_temp,aacgmmlon,alt_temp,glat,glon,error,/to_geo
  thg_map_aacgm_lat_contour(1,loop,*)=glat
  thg_map_aacgm_lat_contour(0,loop,*)=glon
  where_temp=where(error ne 0,count)
  if count ge 1 then begin
    thg_map_aacgm_lat_contour(1,loop,where_temp)='NaN'
    thg_map_aacgm_lat_contour(0,loop,where_temp)='NaN'
  endif
endfor

;dip equator
ascii=read_ascii('~/research/others/Tsyganenko/DIPEQ_120.dat')
thg_map_aacgm_lat_contour(1,90,*)=ascii.field1(1,0:3600:10)
thg_map_aacgm_lat_contour(0,90,*)=ascii.field1(2,0:3600:10)
stop

;save,thg_map_aacgm_lat_contour,thg_map_aacgm_lon_contour,filename='thm_map_add2008_2.sav'
end