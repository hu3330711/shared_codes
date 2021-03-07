C GSFCTEST.FOR ----------------------------------------- Nov 1989
c
1	type *,'MODEL: 1 FOR GSFC80, 2 FOR GSFC83, 3 FOR GSFC87'
	read(5,*) model
        type *,'enter  0  FOR SURFACE DATA, I.E. DATA REFERENCED TO THE' 
        type *,'            GEODETIC ELLIPSOID.' 
        type *,'       1  FOR GEOCENTRIC DATA (USUALLY SPACE DATA), IN'
        type *,'            SPHERICAL COORDINATES.' 
        type *,'      -1  exit'
	read(5,*) jj
	if(jj.gt.0) then
           type *,'geodetic latitude, longitude, geocent. rad./km, year'
	else if (jj.eq.0) then
           type *,'geodetic latitude, longitude, altitude/km, year'
	else
	   goto 2
        endif
	read(5,*) dlat,dlon,alt,tm
	type *, model,jj,dlat,dlon,alt,tm

	call  fidd(model,jj,dlat,dlon,alt,tm,x,y,z,f)

	type *,'X (NORTH) COMPONENT IN NT:',x
        type *,'Y (EAST) COMPONENT:',y
        type *,'VERTICALLY DOWN COMPONENT:',z
        type *,'SCALAR MAGNITUDE:',f
	goto 1
2	stop
	end
