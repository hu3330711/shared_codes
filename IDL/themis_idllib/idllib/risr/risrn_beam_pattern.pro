pro risrn_beam_pattern

tplot_names,'risrn_scan_density_*',names=names
get_data,names[0],data=data
plot,data.glon,data.glat,psym=1,yrange=[63,88],charsize=2,title=names[0],xtitle='glon',ytitle='glat',xrange=[-110,-80],/xstyle,/ystyle
for i=0,n_elements(data.glon[*,0])-1 do oplot,data.glon[i,*],data.glat[i,*],psym=0
for i=0,n_elements(data.glon[*,0])-1 do xyouts,data.glon[i,10],data.glat[i,10],strtrim(string(i+1),1),charsize=2

print,'press .c to see aacgm beam pattern.'
stop

tplot_names,'risrn_scan_density_*',names=names
get_data,names[0],data=data
plot,data.aacgmmlon,data.aacgmmlat,psym=1,yrange=[75,90],charsize=2,title=names[0],xtitle='aacgmmlon',ytitle='aacgmmlat',xrange=[290,350],/xstyle,/ystyle
for i=0,n_elements(data.aacgmmlon[*,0])-1 do oplot,data.aacgmmlon[i,*],data.aacgmmlat[i,*],psym=0
for i=0,n_elements(data.aacgmmlon[*,0])-1 do xyouts,data.aacgmmlon[i,10],data.aacgmmlat[i,10],strtrim(string(i+1),1),charsize=2

end
