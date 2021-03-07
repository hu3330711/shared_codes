pro plot_map_color_grid,data,corners,crange,log=log


system_check_sav

if ndimen(data) ne 2 then begin
  print,'data dimension must be 2.'
  return
endif
if ndimen(corners) ne 4 then begin
  print,'corners dimension must be 4.'
  return
endif
if n_elements(data[*,0]) ne n_elements(corners[*,0]) or n_elements(data[0,*]) ne n_elements(corners[0,*]) then begin
  print,'data and corners elements do not agree.'
  return
endif
if n_elements(corners[0,0,*,0]) ne 4 or n_elements(corners[0,0,0,*]) ne 2 then begin
  print,'corners must be [i,j,4,2].'
  return
endif
if n_elements(crange) ne 2 then begin
  print,'crange elements must be 2.'
  return
endif


color=(((reform(data[*,*])-crange(0))/(crange(1)-crange(0))*254)<254)>0
if keyword_set(log) then color=(((reform(alog10(data[*,*]))-alog10(crange(0)))/(alog10(crange(1))-alog10(crange(0)))*254)<254)>0


for iii=0L,n_elements(data[*,0])-1 do begin
  for jjj=0L,n_elements(data[0,*])-1 do begin
if(finite(data[iii,jjj]) ge 1) then POLYFILL, corners[iii,jjj,[0,1,2,3,0],1],corners[iii,jjj,[0,1,2,3,0],0], COL=color[iii,jjj]
  endfor
endfor

end