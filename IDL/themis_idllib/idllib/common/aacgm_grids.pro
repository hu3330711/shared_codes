;thg_map_aacgm_lat_contour(2,*,*)--(0glat, 1glon), (180 latitude bins), (num_lon_grid points along the isolat contour)
;thg_map_aacgm_lon_contour(2,*,*)--(0glat, 1glon), (360 longitude bins), (num_lat_grid points along the isolon contour)

pro aacgm_grids

get_timespan,t

dmlon=1.
dmlat=1.
num_lon_grid=long(360./dmlon)+1
num_lat_grid=long(180./dmlat)+1
r=1.02

;FCHU 
cotrans_geo_aacgm,glat=58.76,glon=265.91,r=r,aacgmmlat=aacgmmlat,aacgmmlon=aacgmmlon
fchu_aacgmmlon=aacgmmlon

thg_map_aacgm_lon_contour=fltarr(2,num_lon_grid,num_lat_grid)

;aacgm_lon_contour--NH
for loop=0,num_lon_grid-1,1 do begin
  aacgmmlon=loop*dmlon+fchu_aacgmmlon
print,loop,num_lon_grid,aacgmmlon
  aacgmmlon_temp=fltarr(num_lat_grid/2)
  aacgmmlon_temp(*)=aacgmmlon
  aacgmmlon_temp=aacgmmlon_temp mod 360
  aacgmmlat=(findgen(num_lat_grid/2)+num_lat_grid/2+1)*dmlat-90.
  r_temp=fltarr(num_lat_grid/2+1)
  r_temp(*)=r
  cotrans_aacgm_geo,r=r_temp,aacgmmlat=aacgmmlat,aacgmmlon=aacgmmlon_temp,glat=glat,glon=glon
  thg_map_aacgm_lon_contour(1,loop,num_lat_grid/2+1:num_lat_grid-1)=glat
  thg_map_aacgm_lon_contour(0,loop,num_lat_grid/2+1:num_lat_grid-1)=glon
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
  cotrans_aacgm_geo,r=r_temp,aacgmmlat=aacgmmlat,aacgmmlon=aacgmmlon_temp,glat=glat,glon=glon
  thg_map_aacgm_lon_contour(1,loop,0:num_lat_grid/2)=glat
  thg_map_aacgm_lon_contour(0,loop,0:num_lat_grid/2)=glon
endfor

;aacgm_lat_contour
thg_map_aacgm_lat_contour=transpose(thg_map_aacgm_lon_contour,[0,2,1])
stop

;aacgm_lat_contour
thg_map_aacgm_lat_contour=fltarr(2,num_lat_grid,num_lon_grid)
for loop=0,num_lat_grid-1,1 do begin
  aacgmmlat=loop*dmlat-90.
print,loop,num_lat_grid,aacgmmlat
  aacgmmlat_temp=fltarr(num_lon_grid)
  aacgmmlat_temp(*)=aacgmmlat
  aacgmmlon=findgen(num_lon_grid)*dmlon+fchu_aacgmmlon
  aacgmmlon=aacgmmlon mod 360
  r_temp=fltarr(num_lon_grid)
  r_temp(*)=r
  cotrans_aacgm_geo,r=r_temp,aacgmmlat=aacgmmlat_temp,aacgmmlon=aacgmmlon,glat=glat,glon=glon
  thg_map_aacgm_lat_contour(1,loop,*)=glat
  thg_map_aacgm_lat_contour(0,loop,*)=glon
endfor

;dip equator
data_ascii=read_ascii(datafile)
thg_map_aacgm_lat_contour(1,90,*)=ascii.field1(1,0:3600:10)
thg_map_aacgm_lat_contour(0,90,*)=ascii.field1(2,0:3600:10)

stop

end