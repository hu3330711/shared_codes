
      pro SPHCAR_vector,R,THETA,PHI,X,Y,Z,J
;
;   CONVERTS SPHERICAL COORDS INTO CARTESIAN ONES AND VICA VERSA
;    (THETA AND PHI IN RADIANS).
;
;                  J>0            J<0
;-----INPUT:   J,R,THETA,PHI     J,X,Y,Z
;----OUTPUT:      X,Y,Z        R,THETA,PHI
;
;  NOTE: AT THE POLES (X=0 AND Y=0) WE ASSUME PHI=0 (WHEN CONVERTING
;        FROM CARTESIAN TO SPHERICAL COORDS, I.E., FOR J<0)
;
;   LAST MOFIFICATION:  APRIL 1, 2003 (ONLY SOME NOTATION CHANGES AND MORE
;                         COMMENTS ADDED)
;
;   AUTHOR:  N. A. TSYGANENKO
;

      IF(J GT 0) then GOTO, JUMP3
      SQ=X^2+Y^2
      R=SQRT(SQ+Z^2)

      ele_sq=where(sq ne 0.,count)
      IF (count NE 0) then begin
        SQ=SQRT(SQ)
        PHI=fltarr(n_elements(SQ))
        THETA=fltarr(n_elements(SQ))
        PHI(ele_sq)=ATAN(Y(ele_sq)/X(ele_sq))
        ele_x=where(x lt 0.,count)
        IF (count NE 0) then phi(ele_x)=phi(ele_x)+!pi
        ele_z=where(z ne 0.,count)
        if(count ne 0) then THETA(ele_z)=ATAN(SQ(ele_z)/Z(ele_z))
        ele_z=where(z lt 0.,count)
        if(count ne 0) then theta(ele_z)=theta(ele_z)+!pi
        ele_phi=where(phi lt 0.,count)
        IF (count ne 0.) then PHI(ele_phi)=PHI(ele_phi)+2*!pi
        RETURN
      endif

      PHI=0.

      ele_z=where(z lt 0.,count)
      IF (count NE 0) then begin
        THETA(ele_z)=!pi
        return
      endif

      THETA=0.
      RETURN

JUMP3:   SQ=R*SIN(THETA)
      X=SQ*COS(PHI)
      Y=SQ*SIN(PHI)
      Z=R*COS(THETA)
      RETURN
END
