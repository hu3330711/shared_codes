;+
;PROCEDURE:    plot_orbit2, data_name
;PURPOSE:   To do an xyplot of orbital or other data
;INPUT:      String associated with spacecraft position data quantity. 
;      Default is 'GSE_pos_def'.  (see "get_orbit") 
;KEYWORDS:   
;   ZPLOT:    will produce an x-z plot, rather than x-y
;   BOW:      structure that determines bow shock parameters.
;   FREQ:     if set will draw a star every freq days
;   INIT_FREQ:    first date to label. (date string: yy-mm-dd)
;   DATE_LAB: if set will label a date every date_lab days
;   INIT_DATE_LAB:  first date to start labeling. (date string: yy-mm-dd)
;   LIMITS:   A limit structure that defines plot limits. 
;          (see "OPTIONS", "XLIM" or "YLIM")
;   TRANGE:   Two element vector of time values giving the time range. 
;          (see "GETTIME")
;   DAT3D:   A data structure that contains some of the following:
;        magf:   3 components of magnetic field.
;        sc_pos: 3 components of spacecraft position.
;        vsw:    3 components of solar wind velocity.
;   NOCOLOR:  Plot everything in black and white.   
;   
;
;Written by: Davin Larson    95-9-12
;Modified from xyplot.pro by Jasper Halekas.
;Modified by J. McFadden to work better with FAST
;Modified 00-08-14 by J. McFadden replacing plot_pos.pro with plot_positions.pro
;FILE: plot_orbit2.pro
;VERSION:  1.13
;LAST MODIFICATION: 96/04/17
;-

pro plot_orbit2,data_name, $
   xzplot = xzplot, $   
   yzplot = yzplot, $   
   bow = bow, $   
   freq = freq, $
   init_freq = init_freq, $
   date_lab = date_lab, $
   init_date_lab = init_date_lab, $
   trange = t_range, $
   dat3d = dat3d, $
   rho = rho,$
   tplot = tplot,$
   limits = limits, $
   nocolor = nocolor

@tplot_com.pro

if n_elements(data_name) eq 0 then data_name = 'GSE_pos_def'

get_data, data_name, data=data

; mcfadden modification
if keyword_set(rho) then begin
	xvar=0
	yvar=1
endif else if keyword_set(xzplot) then begin
	xvar=0
	yvar=2
endif else if keyword_set(yzplot) then begin
	xvar=1
	yvar=2
endif else begin
	xvar=0
	yvar=1
endelse

label = ['X','Y','Z']
		
t = data.x
if n_elements(t_range) eq 0 then t_range = minmax_range(t)
t_range = gettime(t_range)

ind = where(t ge t_range(0) and t le t_range(1),count)

if count eq 0 then begin
  message,'No Orbit data in time range.'
  return
endif

; mcfadden modification
;color = get_colors(nocolor=nocolor)
color = get_colors()
default = {Title:'Spacecraft Position',ytitle:label(yvar),xtitle:label(xvar),  $
  xrange:[-2.,2.],yrange:[-2.,2.],xstyle:1,ystyle:1}
extract_tags,default,limits

t  = data.x(ind)
px = data.y(ind,xvar)
if keyword_set(rho) then py = sqrt(data.y(ind,1)^2+data.y(ind,2)^2) $
else   py = data.y(ind,yvar)

extract_tags,plotstuff,default,/plot
x = default.xrange
y = default.yrange
aspect = abs((y(1)-y(0))/(x(1)-x(0)))
;pos = plot_pos(aspect)			;old code
pos = plot_positions(aspect=aspect)
plot, px, py, _extra=plotstuff,position=pos

if keyword_set(tplot) then begin
   thick = 1.
   str_element,default,'thick',value=thick
   ind = where(t ge trange(0) and t le trange(1),count)
   if count ne 0 then oplot,px(ind),py(ind),thick=thick*5.,color=color.red
endif


extract_tags,oplotstuff,default,/oplot
extract_tags,plotsstuff,default,/oplot

;plot the Earth:

ang = findgen(50)/49.*2.*!pi
x = cos(ang)
y = sin(ang)
plots,x, y, color=color.blue

str_element,dat3d,'sc_pos',val=sc_pos
str_element,dat3d,'magf',val=magf
str_element,dat3d,'vsw',val=vsw



if 1  then begin
   if data_type(bow) ne 8 then $
          bow={standoff:23.3,eccentricity:1.16,x_offset:3.0}
   theta = (findgen(271)-135.) / !radeg        ;generate values -135 to 135

   r = bow.standoff/(1 + bow.eccentricity * cos(theta))      
   x = r*cos(theta) + bow.x_offset      ;just a curve_fitting exercise
   y = r*sin(theta)         ;see Slavin in JGR V. 86, #A13
   oplot, x, y,color=color.red, _extra=oplotstuff
end

if keyword_set(rho) and keyword_set(sc_pos) then begin
   sx = sc_pos(0)
   sy = sqrt(sc_pos(1)^2+sc_pos(2)^2) 
   oplot, [sx],[sy],  psym=5, _extra=oplotstuff
   if keyword_set(magf) then begin 
      b = magf/sqrt(total(magf*magf))
      re = sqrt(total(sc_pos*sc_pos))
      pts = (findgen(51)/25.-1.)
      delta = pts # b *re
      p1 = (replicate(1.,51) # sc_pos) + delta
;help,delta,p1
      x = p1(*,0)
      y = sqrt(p1(*,1)^2+p1(*,2)^2)
;th = atan(y(1)-y(0),x(1)-x(0))
;sym = [[-6,-2],[0,0],[-6,2]]/3.
;rot = [[cos(th),sin(th)],[-sin(th),cos(th)]]
;usersym , rot # sym
      
      oplot,  x , y  , psym=-3,color=color.green, _extra=oplotstuff
   endif

   sc_pos = 0
endif

if keyword_set(sc_pos) then begin
   oplot, [sc_pos(0)],[sc_pos(n)],  psym=4, _extra=oplotstuff
   if keyword_set(magf) then begin
      b = magf/sqrt(total(magf*magf))
      re = sqrt(total(sc_pos*sc_pos))
      delta = b*re
      p1 = sc_pos-delta
      p2 = sc_pos+delta
      x = [p1(0),p2(0)]
      y = [p1(n),p2(n)]
      th = atan(-(y(1)-y(0)),-(x(1)-x(0)))
;print,th*!radeg
      sym = [[-6,-2],[0,0],[-6,2]]/3.
      rot = [[cos(th),sin(th)],[-sin(th),cos(th)]]
      usersym , rot # sym
      oplot,  x , y  ,  color = color.green,psym=-8, _extra=oplotstuff
   endif
endif


if keyword_set(freq) then begin
   sfreq = freq*3600.*24.d
   nlabs = fix((t_range(1)-t_range(0))/sfreq)+1

   if keyword_set(init_freq) then starttime = str_to_time(init_freq) $
   else starttime = floor(t_range(0)/sfreq)*sfreq 
   time = dindgen(nlabs)*sfreq + starttime
   npx = interpol(px,t,time)
   npy = interpol(py,t,time)
   oplot,npx, npy, psym = 1, color=color.red, _extra=oplotstuff
endif

if keyword_set(date_lab) then begin
   sfreq = date_lab*3600.*24.d
   nlabs = fix((t_range(1)-t_range(0))/sfreq)+1

   if keyword_set(init_date_lab) then starttime = str_to_time(init_date_lab) $
   else starttime = floor(t_range(0)/sfreq)*sfreq 
   time = dindgen(nlabs)*sfreq + starttime
   npx = interpol(px,t,time)
   npy = interpol(py,t,time)

   oplot,npx, npy, psym = 2, color=color.red, _extra=oplotstuff

   labs = strmid( time_to_str(time),5,5)
   extract_tags,xyoutstuff,default,/xyout
; xyouts will not clip....
; convert_data,npx,npy,/data,/to_normal
   xyouts, npx, npy, labs, alignment = 0., color=color.red
end


return
end
