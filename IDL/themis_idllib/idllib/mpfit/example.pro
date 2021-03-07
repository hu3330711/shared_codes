FUNCTION MYGAUSS, X, P
  RETURN, P[0] + GAUSS1(X, P[1:3])
END
;the three parameters to GAUSS1 are, in order: mean, sigma, and area under curve

restore,'fakesave.sav'
start = [950.D, 2.5, 1., 1000.]
result = MPFITFUN('MYGAUSS', t, r, rerr, start)


;general
start = [p[0],p[1],p[2]...]
result = MPFITFUN('fitfunc', x, y, error,start)

