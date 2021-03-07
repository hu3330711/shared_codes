
      pro SPHCAR,R,THETA,PHI,X,Y,Z,J
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
      IF (SQ NE 0.) then GOTO, JUMP2
      PHI=0.
      IF (Z LT 0.) then GOTO, JUMP1
      THETA=0.
      RETURN
JUMP1:   THETA=!pi
      RETURN
JUMP2:   SQ=SQRT(SQ)
      PHI=ATAN(Y/X)
      if(x lt 0) then phi=phi+!pi
      THETA=ATAN(SQ/Z)
      if(z lt 0) then theta=theta+!pi
      IF (PHI LT 0.) then PHI=PHI+2*!pi
      RETURN
JUMP3:   SQ=R*SIN(THETA)
      X=SQ*COS(PHI)
      Y=SQ*SIN(PHI)
      Z=R*COS(THETA)
      RETURN
END
