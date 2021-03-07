;tplot_init
;TVLCT, [255,0], [255,255], [0,0], 1
;POLYFILL, CIRCLE2(65, 65, 20), Color=1, /Device


FUNCTION HALF_CIRCLE2, xcenter, ycenter, radius,rotate
points = (2 * !PI / 99.0) * FINDGEN(50)
x = xcenter + radius * COS(points + rotate*!PI/180)
y = ycenter + radius * SIN(points + rotate*!PI/180)
RETURN, TRANSPOSE([[x],[y]])
END

