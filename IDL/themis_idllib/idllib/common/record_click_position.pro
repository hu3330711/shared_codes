pro record_click_position,xarray=xarray,yarray=yarray

xarray=fltarr(1000)
yarray=fltarr(1000)

   ; Get the initial point in normalized coordinates:  
   CURSOR, X, Y, /nowait,/data
   ; Repeat until right button is pressed. Get the second point.  
   ; Draw the line. Make the current second point be the new first.  
   loop=0L
   print,'Left click for selecting positions. Use right click for finishing.'
   WHILE (!MOUSE.button NE 4 or loop ge 999) DO BEGIN  
      CURSOR, X1, Y1,/down,/data
      PLOTS,X1,Y1,psym=6,symsize=1.5,color=5,thick=2
      print,loop,x1,y1
      xarray[loop] = X1
      yarray[loop] = Y1  
      loop+=1
   ENDWHILE

if loop ge 2 then begin
  xarray=xarray[0:loop-2]
  yarray=yarray[0:loop-2]
endif
if loop le 1 then begin
  xarray=xarray[0]
  yarray=yarray[0]
endif

end