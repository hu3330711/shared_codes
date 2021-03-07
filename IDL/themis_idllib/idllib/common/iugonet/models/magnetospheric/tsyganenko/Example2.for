C
      PROGRAM EXAMPLE2
C
C  THIS IS ANOTHER EXAMPLE OF USING THE GEOPACK SUBROUTINE "TRACE". UNLIKE IN THE EXAMPLE1,
C  HERE WE ASSUME A PURELY DIPOLAR APPROXIMATION FOR THE EARTH'S INTERNAL FIELD.
C  IN THIS CASE WE ALSO EXPLICITLY SPECIFY THE TILT ANGLE OF THE GEODIPOLE,
C  INSTEAD OF CALCULATING IT FROM THE DATE/TIME.

C
C  Unlike in the EXAMPLE1, here we "manually" specify the tilt angle and its sine/cosine.
c  To forward them to the coordinate transformation subroutines, we need to explicitly
c  include the common block /GEOPACK1/:

C
      COMMON /GEOPACK1/ AA(10),SPS,CPS,BB(3),PS,CC(19)
      DIMENSION XX(500),YY(500),ZZ(500), PARMOD(10)
c
c be sure to include an EXTERNAL statement with the names of (i) a magnetospheric
c external field model and (ii) Earth's internal field model.
c
      EXTERNAL T96_01, DIP
C
C   First, call RECALC, to define the main field coefficients and, hence, the magnetic
C      moment of the geodipole for IYEAR=1997 and IDAY=350.
C   The universal time does not matter in this example, because here we explicitly
C   specify the tilt angle (hence, the orientation of the dipole in the GSM coordinates),
C   and we arbitrarily set IHOUR=MIN=ISEC=0  (any other values would be OK):
c
      CALL RECALC (1997,350,0,0,0)
c
c   Enter T96 model input parameters:
c
      PRINT *, '   ENTER SOLAR WIND RAM PRESSURE IN NANOPASCALS'
      READ *, PARMOD(1)
C
      PRINT *, '   ENTER DST '
      READ *, PARMOD(2)
C
      PRINT *, '   ENTER IMF BY AND BZ'
      READ *, PARMOD(3),PARMOD(4)
C

c  Define the latitude (XLAT) and longitude (XLON) of the field line footpoint
c   in the GSM coordinate system:
c
      XLAT=75.
      XLON=180.
C
C  Specify the dipole tilt angle PS, its sine SPS and cosine CPS, entering
c    in the common block /GEOPACK1/:
C
       PS=0.
       SPS=SIN(PS)
       CPS=COS(PS)
c
c   Calculate Cartesian coordinates of the starting footpoint:
c
      T=(90.-XLAT)*.01745329
      XL=XLON*.01745329
      XGSM=SIN(T)*COS(XL)
      YGSM=SIN(T)*SIN(XL)
      ZGSM=COS(T)
C
c   SPECIFY TRACING PARAMETERS:
C
      DIR=1.
C            (TRACE THE LINE WITH A FOOTPOINT IN THE NORTHERN HEMISPHERE, THAT IS,
C             ANTIPARALLEL TO THE MAGNETIC FIELD)

      RLIM=60.
C            (LIMIT THE TRACING REGION WITHIN R=60 Re)
C
      R0=1.
C            (LANDING POINT WILL BE CALCULATED ON THE SPHERE R=1,
C                   I.E. ON THE EARTH'S SURFACE)
c
      IOPT=0
C           (IN THIS EXAMPLE IOPT IS JUST A DUMMY PARAMETER,
C                 WHOSE VALUE DOES NOT MATTER)
c
c  Trace the field line:
c
      CALL TRACE (XGSM,YGSM,ZGSM,DIR,RLIM,R0,IOPT,PARMOD,T96_01,DIP,
     *  XF,YF,ZF,XX,YY,ZZ,M)
C
C   Write the result in the output file 'LINTEST2.DAT':
C
       OPEN(UNIT=1, FILE='LINTEST2.DAT',STATUS='NEW')
  1   WRITE (1,20) (XX(L),YY(L),ZZ(L),L=1,M)
 20   FORMAT((2X,3F6.2))

      CLOSE(UNIT=1)
      END
