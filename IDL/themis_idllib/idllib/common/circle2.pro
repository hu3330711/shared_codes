;tplot_init
;TVLCT, [255,0], [255,255], [0,0], 1
;POLYFILL, CIRCLE2(65, 65, 20), Color=1, /Device


FUNCTION CIRCLE2, xcenter, ycenter, radius
points = (2 * !PI / 99.0) * FINDGEN(100)
x = xcenter + radius * COS(points )
y = ycenter + radius * SIN(points )
RETURN, TRANSPOSE([[x],[y]])
END

