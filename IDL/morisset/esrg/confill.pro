pro confill,z,x,y,xrange=xrange,yrange=yrange,charsize=charsize,$
            title=title,stitle=stitle,xtitle=xtitle,ytitle=ytitle,$
            aspect=aspect,colors=colors,range=range,nlevels=nlevels,$
            rmarg=rmarg,pcharsize=pcharsize,lcharsize=lcharsize,$
            c_thick=c_thick,c_label=c_label,c_charsize=c_charsize,$
            c_orientation=c_orientation,c_spacing=c_spacing,noscale=noscale,$
            downhill=downhill,xstyle=xstyle,ystyle=ystyle,xtype=xtype, $
            ytype=ytype,barwidth=barwidth,c_color=c_color,labels=labels, $
            position=position,levels=levels,rgb_nodata=rgb_nodata, $
            overplot=overplot
;+
; ROUTINE:  confill
;
; PURPOSE:  display a contour plot with filled contours 
;
; USEAGE:   confill,z,x,y,xrange=xrange,yrange=yrange,charsize=charsize,$
;                   title=title,stitle=stitle,xtitle=xtitle,ytitle=ytitle,$
;                   aspect=aspect,colors=colors,range=range,nlevels=nlevels,$
;                   rmarg=rmarg,pcharsize=pcharsize,lcharsize=lcharsize,$
;                   c_thick=c_thick,c_label=c_label,c_charsize=c_charsize,$
;                   c_orientation=c_orientation,c_spacing=c_spacing,$
;                   downhill=downhill,xtype=xtype,ytype=ytype,$
;                   c_color=c_color,labels=labels,position=position,$
;                   levels=levels,rgb_data=rgb_data,noscale=noscale
;
; INPUT:    
;  z
;    array of field values
;
;  x
;    one or two dimensional array of x axis points
;
;  y
;    one or two dimensional array of y axis points
;
;    IF z,x, and y are all one dimensional vectors and all the same
;    size, it is assumed that irregularly grided data is being
;    supplied.  In this case the routine TRIANGULATE is called to get
;    the Delaunay triangulation of the data.  The triangulation info
;    is then passed on to CONTOUR
;
;
; KEYWORD INPUT:
;
;  title
;    plot title
;
;  xtitle
;    x axis title
; 
;  ytitle
;    y axis title
;
;  stitle
;    color key title (drawn to the right of color scale)
;
;  noscale 
;    if set, no color key will be drawn
;
;  rmarg 
;    right margin expansion factor to provide more room for extra wide
;    color scale annotation (default=1)
;
;  xrange
;    x axis range. (default = [0,x_dimension-1])
;
;  yrange
;    y axis range. (default = [0,y_dimension-1])
;
;  range
;    two element vector indicating physical range over which
;    to distribute contour levels. 
;
;  nlevels
;    maximum number of contour levels.  If LEVELS is not set the
;    actual number of contour levels is determined automatically by
;    AUTORANGE.  NLEVELS sets the maximum allowed number of levels,
;    the actual number of levels may be less.
;
;  levels
;    contour level values (vector).  The default contour levels are
;    set by AUTORANGE. The automatically selected levels chosen by
;    AUTORANGE can be retrieved by setting LEVELS to a named variable
;    which is initially undefined or set to zero.
;
;  labels
;     a vector of strings used to label the color key levels.  If not
;     set the default color key number labels are used.  If the number
;     of elements in LABELS is the same as the number of elements in
;     COLORS then the labels will appear in the middle of a given
;     color band instead of at the boundary between colors.  If COLORS
;     is not set, the number of elements in LABELS should be at least
;     as large as the number of color key segments plus one.
;
;  colors
;     an array of color indicies used to fill contours.  The number of
;     color values should be one less than the number of levels. The
;     default colors are chosen to be uniformally distributed over the
;     full color table.  The automatically selected colors chosen by
;     CONFILL are:  colors=fix((!d.n_colors-2)*(1+findgen(nclr))/nclr),
;     where nclr=n_elements(levels)-1
;
;     NOTE: The default color indicies generated by CONFILL never
;     include color index 0 or !d.n_colors-1.  These indicies are
;     reserved for the overall plot background and foreground colors.
;     For best results, color index 0 and !d.n_colors-1 should
;     correspond to pure black and white respectively, which is
;     usually the case in the predefined color tables supplied by RSI.
;     A predefined color table that doesn't follow this rule can be
;     redefined using TVLCT to put black and white in these slots,
;     without affecting any of the filled contours used in the CONFILL
;     plot:   tvlct,0,0,0,0 & tvlct,255,255,255,!d.n_colors-1
;     
;  c_label 
;     integer array indicating which contour lines to label.  For example,
;     c_label=[0,0,0,1,0,1] indicates that the contour lines which correspond
;     to levels(3) and levels(5) are to be labeled.  The number of C_LABELs
;     should be the same as the number of LEVELS. By default no contour
;     lines are labeled.
;
;  c_thick
;     thickness of contour lines (may be a vector of values), if c_thick
;     is set to 0 then no contour lines are drawn.  Default is 0.
;
;  c_color
;     color of contour lines (may be a vector of values).  A negative
;     value can be used to indicate a dividing line between contours
;     drawn in color 0 and levels drawn with !d.n_colors-1. In this
;     case the magnitude of C_COLORS indicates the last contour level
;     that should be colored with color value !d.n_colors-1 (usually
;     white), where the lowest contour level is numbered one.  The
;     rest of the color levels will be colored with color 0 (usually
;     black).  For example C_COLORS=-3 indicates that that the first
;     three contours should by colored with !d.ncolors-1 and the
;     remaining levels colored with color 0.  Default is !p.color
;
;     NOTE: The number of contour levels is determined automatically
;     if LEVELS is not set.  Hence, it may not be possible to guess
;     how many values of LABELS, COLORS, C_LABEL, C_THICK or C_COLOR
;     should be specified. It is probably safer not to include these
;     parameters unless LEVELS is also specified.
;        
;  c_orientation
;    If the FILL keyword is set, this keyword can be set to the angle,
;    in degrees counterclockwise from the horizontal, of the lines
;    used to fill contours.  If neither C_ORIENTATION or C_SPACING are
;    specified, the contours are solid filled.
;        
;  c_spacing
;    If the FILL keyword is set, this keyword can be used to control
;    the distance, in centimeters, between the lines used to fill the
;    contours.
;
;  xtype
;    Set this keyword to specify a logarithmic X axis.
;        
;  ytype
;    Set this keyword to specify a logarithmic Y axis.
;        
;  downhill
;    Set this keyword to label each contour with short, perpendicular
;    tick marks that point in the "downhill" direction, making the
;    direction of the grade readily apparent. If this keyword is set,
;    the contour following method is used in drawing the contours.
;
;    NOTE: Due to a bug in IDL_3.6,  the concurrent use of C_LABEL and
;          DOWNHILL causes an IDL session crash.  For this reason the
;          DOWNHILL option is disabled on IDL_3.6.
;        
;  aspect
;    the x over y aspect ratio of the output image. If not set, the 
;    aspect ratio is adjusted to fill the available plotting area.
;
;  position
;    specifies the lower left and upper right CONFILL frame position in
;    normal coordinates.  When set POSITION overides the setting of
;    ASPECT.
;
;  pcharsize
;    character size of numbers along x and y axis and the title
;    (default is !p.charsize if non-zero, 1 otherwise)
; 
;    NOTE: The character size of the x and y-axis number scale
;    annotation may be independently controlled by the !x.charsize or
;    !y.charsize system variables.  For example, the x and y axis
;    number scale characters may be made invisible by setting these
;    variables to a very small non-zero number before calling CONFILL.
;    Remember to restore normal character sizes by setting !x.charsize
;    and !y.charsize to 1 or zero after the call to CONFILL.
;
;  lcharsize
;    character size of color key number or labels (default is
;    pcharsize if set, otherwise !p.charsize if non-zero, 1 otherwise)
;
;  barwidth
;     width of color key which appears to right of contour plot (default=1).
;
;  rgb_nodata
;    if set, regions with field values less then the minimum of LEVELS
;    or RANGE will be colored with the RGB values specified in
;    RGB_NODATA (a scalor or three element vector).  Color index 1 is
;    used for the nodata color.  When RGB_NODATA is set don't use
;    color index 1 in the COLORS input vector.  
;
;    NOTE: RGB_NODATA has no effect unless either RANGE or LEVELS is
;    set.  This is because the default contour levels are set so that
;    the minimum level is less than the minimum field value.
;
;  overplot if set, current field is super imposed over a previous
;    confill plot this option only works when z,x, and y are all one
;    dimensional vectors and all the same size, in which case it is
;    assumed that irregularly grided data is being supplied.  In this
;    case the routine TRIANGULATE is called to get the Delaunay
;    triangulation of the data.  The triangulation info is then passed
;    on to CONTOUR.
;
; DEPENDENCIES: COLOR_KEY, AUTORANGE
;
; SIDE EFFECTS: if RGB_NODATA is set the rgb value of color index 1
;               is modified
;
; EXAMPLES:
;
; loadct,5
; n=128
; zz=randata(n,n,s=4)
; xx=findrng([-120,-100],n)
; yy=findrng([20,50],n)
; confill,zz,xx,yy,title='Plot title',xtitle='x-axis',ytitle='y-axis',$
;         stitle='contour levels',c_color=-4
;
; confill,zz,xx,yy,title='Plot title',xtitle='x-axis',ytitle='y-axis',$
;         stitle='contour levels',c_color=-4,rgb_nodata=!p.color/2
;
;; labeled regions
;
; levels=-3+findgen(6)*2
; labels=[' ocean','  rain!c forrest',' sand',$
;         ' old!c snow',' real!c new!c snow']
; confill,zz,xx,yy,title='Plot title',xtitle='x-axis',ytitle='y-axis',$
;         stitle='surface types',labels=labels,levels=levels,barwidth=.5
;
;; contour irregularly grided data
;
; loadct,5
; w8x11
; n=128
; z=randata(n,n,s=4)
; x=findgen(n)
; y=findgen(n)
; !p.multi=[0,1,2]
; confill,z,x,y,title='Regularly Grided',c_color=-5
;
; ii=randomu(iseed,200)*(n*n-1)         ; random sample points
; zz=z(ii)                              ; random samples
; xx=ii mod n                           ;   (zz,xx,yy are vectors)
; yy=ii / n
; confill,zz,xx,yy,title='Randomly Sampled',c_color=-5
;
; oplot,xx,yy,psym=4,color=1,symsize=.4 ; show locations of sample points
;
;; overlay two irregular regions in one confill plot
;
; loadct,5
; w8x11
; n=128
; z=randata(n,n,s=4)
; ii=randomu(iseed,200)*(n*n-1)         ; random sample points
; zz=z(ii)                              ; random samples
; xx=ii mod n                           ;   (zz,xx,yy are vectors)
; yy=ii / n
; confill,zz,xx,yy,xrange=[0,300]
; confill,zz^2,xx+150,yy/2,/overplot
; confill,sqrt(zz>0),xx+150,70+yy/2,/over
;
;
; AUTHOR:   Paul Ricchiazzi                        24 May 95
;           Institute for Computational Earth System Science
;           University of California, Santa Barbara
;-
;
if n_params() eq 0 then begin
  xhelp,'confill'
  return
endif

if not keyword_set(range) then range=[min(z,max=mx),mx]
if not keyword_set(title) then title=''
if not keyword_set(xtitle) then xtitle=''
if not keyword_set(ytitle) then ytitle=''
if not keyword_set(c_orientation) then c_orientation=0
if not keyword_set(c_spacing) then c_spacing=0
if not keyword_set(downhill) then downhill=0
if downhill eq 1 and !version.release eq '3.6' then begin
  downhill=0
  print,'WARNING: CONFILL -- DOWNHILL disabled on IDL_3.6'
endif
if not keyword_set(xtype) then xtype=0
if not keyword_set(ytype) then ytype=0
if keyword_set(pcharsize) eq 0 then begin
  if !p.charsize eq 0 then pcharsize=1 else pcharsize=!p.charsize
endif
if not keyword_set(lcharsize) then begin
  if !p.multi(1)>!p.multi(2) gt 2 then lcharsize=.5*pcharsize $
                                  else lcharsize=pcharsize
endif
if not keyword_set(nlevels) then nlevels=11

if not keyword_set(levels) then begin
  autorange,range,ntick,levels,ntickmax=nlevels
  nlevels=ntick
endif else begin

  if not keyword_set(labels) then begin
    digits=(fix(alog10(max(abs(levels(where(levels ne 0))))))+2)
    incc=min((levels-shift(levels,+1))(1:*))
    nzs=fix(alog10(incc*1.01))
    nsig=digits-nzs-1

    case 1 of
      nzs lt 0 and nzs gt -5: frmt='(f30.'+string(form='(i1)',-nzs+1)+')'
      abs(digits) gt 5:       frmt='(g30.'+string(form='(i1)',nsig)+')' 
      nzs eq 0:               frmt='(f30.'+string(form='(i1)',1)+')'
      nzs ge 1 and nzs le 3:  frmt='(f30.'+string(form='(i1)',0)+')'
      else:                   frmt='(g30.'+string(form='(i1)',nsig)+')'
    endcase
    labels=strcompress(string(f=frmt,levels),/remove_all)
  endif
  nlevels=n_elements(levels)
endelse
  
if not keyword_set(colors) then  $
   colors=fix((!d.n_colors-2)*(1+findgen(nlevels-1))/(nlevels-1))
if not keyword_set(barwidth) then barwidth=1

if n_elements(xstyle) eq 0 then xstyle=1
if n_elements(ystyle) eq 0 then ystyle=1

sz=size(z)
nx=sz(1)
ny=sz(2)

if sz(0) eq 1 then begin
  nx=fix(sqrt(sz(1)))
  ny=nx
  if n_elements(x) ne n_elements(z) or n_elements(y) ne n_elements(z) then $
    message,'the z, x and y array sizes are not equal'
  triangulate,x,y,tri
endif

if keyword_set(position) then begin
  pos=position
  noerase=0
endif else begin
  noerase=1
  if not keyword_set(overplot) then begin
    psave=!p.multi
    plot, [0,1],[0,1],/nodata,xstyle=4,ystyle=4,charsize=pcharsize
    px=!x.window*!d.x_vsize
    py=!y.window*!d.y_vsize
    xsize=px(1)-px(0)
    ysize=py(1)-py(0)
    if not keyword_set(rmarg) then rmarg=1.
    xmarg=!d.x_ch_size*(4+4*lcharsize)
    if keyword_set(stitle) then xmarg=xmarg+2*!d.y_ch_size*lcharsize
    if keyword_set(noscale) then rmarg=0
    xsize=xsize-rmarg*xmarg
    oxs=xsize
    oys=ysize
    if not keyword_set(aspect) then aspect=xsize/ysize
    if xsize gt ysize*aspect then xsize=ysize*aspect else ysize=xsize/aspect 
    if xsize lt oxs then px(0)=px(0)+.5*(oxs-xsize)
    if ysize lt oys then py(0)=py(0)+.5*(oys-ysize)
    px(1)=px(0)+xsize
    py(1)=py(0)+ysize
    pos=fltarr(4)
    pos([0,2])=float(px)/!d.x_vsize
    pos([1,3])=float(py)/!d.y_vsize
  endif else begin
    pos=[!d.x_vsize*!x.window,!d.y_vsize*!y.window]
    pos=pos([0,2,1,3])
  endelse
endelse

if not keyword_set(stitle) then stitle=''
if not keyword_set(c_label) then c_label=replicate(0,nlevels)
if n_elements(c_color) eq 0 then c_color=!d.n_colors-1
if c_color lt 0 then begin
  ii=-c_color-1
  c_color=intarr(nlevels)
  c_color(0:ii)=!d.n_colors-1
endif
if not keyword_set(x) then x=findgen(nx)
if not keyword_set(y) then y=findgen(ny)

drawlines=keyword_set(c_thick)

if not keyword_set(xrange) then xrange=[min(x,max=mx),mx]
if not keyword_set(yrange) then yrange=[min(y,max=mx),mx]

zmn=levels(0)
zmx=levels(nlevels-1)
zmx=zmx-.0001*(zmx-zmn)
clrs=colors
levs=levels
minz=min(z)

if n_elements(rgb_nodata) ne 0 and minz lt levels(0) then begin
  clrs=[1,colors]
  zmn=minz
  levs=[zmn,levels]
  if n_elements(rgb_nodata) eq 1 $
    then tvlct,rgb_nodata,rgb_nodata,rgb_nodata,1 $
    else tvlct,rgb_nodata(0),rgb_nodata(1),rgb_nodata(2),1
endif

if n_elements(tri) eq 0 then begin

  contour,z>zmn<zmx,x,y,levels=levs,/follow,c_colors=clrs,/fill,$
          position=pos,xtitle=xtitle,ytitle=ytitle,title=title,$
          xstyle=xstyle,ystyle=ystyle,charsize=pcharsize,noerase=noerase,$
          c_orientation=c_orientation,c_spacing=c_spacing,$
          xtype=xtype,ytype=ytype,xrange=xrange,yrange=yrange

  if drawlines then contour,z>zmn<zmx,x,y,levels=levels,/follow,/overplot,$
          c_label=c_label,c_thick=c_thick,c_color=c_color,downhill=downhill

endif else begin

  if not keyword_set(overplot) then begin
    !p.multi=psave
    plot,x,y,/nodata,/xstyle,/ystyle,pos=pos,charsize=pcharsize,$
       xtype=xtype,ytype=ytype,xtitle=xtitle,ytitle=ytitle,title=title,$
       xrange=xrange,yrange=yrange
  endif else begin
    xrange=!x.crange
    yrange=!y.crange
  endelse

  contour,z>zmn<zmx,x,y,levels=levs,/follow,c_colors=clrs,/fill,$
          position=pos,xstyle=xstyle,ystyle=ystyle,/overplot,$
          c_orientation=c_orientation,c_spacing=c_spacing,$
          xtype=xtype,ytype=ytype,triangulation=tri,xrange=xrange,yrange=yrange

  if drawlines then contour,z>zmn<zmx,x,y,levels=levels,/follow,/overplot,$
          c_label=c_label,c_thick=c_thick,c_color=c_color,$
          triangulation=tri,downhill=downhill

  axis,xaxis=1,xrange=xrange,/xstyle,xtype=xtype,xtickname=replicate(' ',30)
  axis,xaxis=0,xrange=xrange,/xstyle,xtype=xtype
  axis,yaxis=1,yrange=yrange,/ystyle,ytype=ytype
  axis,yaxis=0,yrange=yrange,/ystyle,ytype=ytype,ytickname=replicate(' ',30)
endelse

if keyword_set(noscale) then return

if keyword_set(labels) then begin
  color_key,range=levels,color=colors,labels=labels,$
          charsize=lcharsize,title=stitle,barwidth=barwidth
endif else begin
  color_key,range=levels,color=colors,charsize=lcharsize,$
          title=stitle,barwidth=barwidth
endelse  

end

