function mpfitfunc_sin,x,p
return,p[0]*sin(x+p[1])+p[2]
end
