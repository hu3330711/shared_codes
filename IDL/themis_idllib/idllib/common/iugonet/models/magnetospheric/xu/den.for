C     **************************************************************
C     *                     PROGRAM DEN.FOR                        *
C     **************************************************************
C This program is to find the DEN(Displaced Equatorial Neutral) sheet inside 
C  the magnetopause in different tine and positions 

C Input and Output:
C Input: DATE, TIME (In the following 2 different format) XSM, YSM, ZSM 
C    (Position in GSM Coordinate System)
C (1) When Date is Year+DOY(Day Of Year) and Time is Second of Day. For 
C    Example: Year=1995, DOY=34, SD=35000, then Date is 95034 and SD is 35000 
C (2) When Date is Year/Month/Day, and Time is Hour/Minute/Second. For example:
C    February 3, 1995, 2 Hour 5 minute and 10 second, then Date is 950203, and 
C    Time is 020520
C Output: ZDEN: Position of the neutral sheet along ZSM axis, at XSM and YSM
C         IE: Parameter to show whether the observed point is in or outside 
C             the magnetopause.

C Authors:
C Ronglan XU and Lei LI, Center for Space Sci. and Applied Res.,
C Chinese Academy of Sciences, PO Box 8701, Beijing 100080, China
C E-mail: XURL@SUN.IHEP.AC.CN, XURL@SUN20.CSSAR.AC.CN

C References:
C (1) DEN(Displaced Equatorial Neutral):
C Xu, R.-L., A Displaced Equatorial Neutral Sheet Surface Observed on ISEE-2
C Satellite,  J. Atmospheric and Terrestrial Phys., 58, 1085, 1991

C (2) Magnetopause model:
C  Sibeck, D. G., R. E. Lopez, and R. C. Roelof, Solar wind control of the
C  magnetopause shape, location, and motion, J. Grophys. Res., 96, 5489, 1991

C Subroutines
C (1) SDOY(DATE,TIME,YR,DOY,HR,TMI,SC)
C   Find Year, Day of year, Month, Day of month, Hour, Minute, Second from 
C   Ddate(year/month/day) and Hour/Minute/Second 
C (2) STIME(YD,SD,YR,DOY,TMO,DAY,HR,TMI,SC)
C   Find Year,Day of Year, Month), Day of month, Hour, Minute and Second from 
C   Year and Day of Year and Second of Day
C (3) STIL(DOY,HR,TMI,TIL)
C   Find the tilt angle of the geomagnetic axis for different Date and Time
C (4) SDEN(TILA,XSM,YSM,ZDEN,RMP,IE)
C   Find the position of DEN sheet along ZSM axis (ZAEN) in the Magnetopause 
C   for different XSM,YSM and TILA(Tilt Angle) 
C (5) SD1(TIL,H,H1,XSM,D)
C   Find the displaced parameter D of the DEN model, so that it provides 
C   approximately equal cross-sectional areas above and below the neutral sheet
C (6) SFA4(AA,BB,CC,DD,X)
C   Find the root of equation: X**4+AA*X**3+BB*X**2+CC*X+DD=0 
C (7) SMPF(XSM,RMP,IM)
C    Find the radius of the cross section of the magnetopause

C MAIN PROGRAM:
        WRITE(*,*)'   '
        WRITE(*,*)'This program is to find the Displaced Equatorial Neut
     *ral Sheet in the Magnetopause in different date, time and position
     *s in GSM Coordinate system'
        WRITE(*,*)'   '
        WRITE(*,*)'When Date is Year+DOY(Day Of Year) and Time is Second
     * of Day, then AA=1 '
        WRITE(*,*)'Example: Year=1995, DOY=34, SD=35000, then Date is 95
     *034 and SD is 35000 '
        WRITE(*,*)'   '
        WRITE(*,*)'When Date is Year/Month/Day, and Time is Hour/Minute/
     *Second, then AA=2 '       
        WRITE(*,*)'February 3, 1995, 2 Hour 5 minute and 10 second, then
     * Date is 950203, and Time is 020520'
        WRITE(*,*)'   '
 100    WRITE(*,*)'Type AA = '
        READ(*,*) AA
        IF(AA.EQ.1) THEN
          WRITE(*,*)'Type YD(Year+DOY), SD(Second of Day)'
          READ(*,*) YD,SD
          CALL STIME(YD,SD,YR,DOY,TMO,DAY,HR,TMI,SC)
          CALL STIL(DOY,HR,TMI,TILA)
          GOTO 200
        ENDIF
        IF(AA.EQ.2) THEN
          WRITE(*,*)'Type DATE and TIME'
          READ(*,*) DATE,TIME
          CALL SDOY(DATE,TIME,YR,DOY,HR,TMI,SC)
        ENDIF

        CALL STIL(DOY,HR,TMI,TILA)
 200    WRITE(*,*)'XSM,YSM'
        READ(*,*) XSM,YSM       
        CALL SDEN(TILA,XSM,YSM,ZDEN,RMP,IE)
        WRITE(*,*)ZDEN,IE
        GOTO 100
        STOP
        END

        SUBROUTINE SDOY(DATE,TIME,YR,DOY,HR,TMI,SC)
C FIND YR(YEAR),DOY(DAY OF YEAR),TMO(MONTH),DAY(DAY OF MONTH),
C  HR(HOUR),TMI(MINUTE),SC(SECOND) FROM DATE(YEAR/MONTH/DAY)
c  AND TIME(HOUR/MINUTE/SECOND)
C        INPUT:DATE,TIME
C        OUTPUT:YR,DOY,HR,TMI,SC
         DIMENSION DM(13)

C FIND YR, DOY, FROM DATE
         DATA DM/0,31,28,31,30,31,30,31,31,30,31,30,31/
         YR0=FLOAT(IFIX(DATE/10000.))
         YR=YR0+1900
         DY=YR/4.-IFIX(YR/4.)
         IF(DY.EQ.0) DM(3)=29
         TMODA=DATE-YR0*10000
         TMO=FLOAT(IFIX(TMODA/100))
         DAY=TMODA-TMO*100
         DOYM=0
         DO 100 I=1,TMO
 100     DOYM=DOYM+DM(I)
         DOY=DOYM+DAY
 
C FIND HR,TMI,SC FROM TIME
         HR=FLOAT(IFIX(TIME/10000.))
         TMI0=TIME-HR*10000
         TMI=FLOAT(IFIX(TMI0/100.))         
         SC=TMI0-TMI*100         
         RETURN
         END
       
         SUBROUTINE STIME(YD,SD,YR,DOY,TMO,DAY,HR,TMI,SC)
C FIND YR(YEAR),DOY(DAY OF YEAR),TMO(MONTH),DAY(DAY OF MONTH),
C  HR(HOUR),TMI(MINUTE),SC(SECOND) FROM YD(YEAR AND DAY OF YEAR)
C  AND SD(SECOND OF THE DAY)
C        INPUT:YD,SD
C        OUTPUT:YR,DOY,DAY,TMO,HR,TMI,SC
         DIMENSION DM(13)

C FIND YR, DOY, IMO AND DAY FROM YD
         DATA DM/0,31,28,31,30,31,30,31,31,30,31,30,31/
         YR=FLOAT(IFIX(YD/1000.))
         DOY=YD-YR*1000.
         DY=YR/4.-IFIX(YR/4.)
         IF(DY.EQ.0) DM(3)=29
         IM=0
         DAY=DOY
 1       IM=IM+1
         DAY=DAY-DM(IM)
         TMO=FLOAT(IM)
         A=DM(IM+1)
         IF(DAY.GT.A) GOTO 1

C FIND HR,TMI AND SC FROM SD
         THR=SD/3600
         HR=FLOAT(IFIX(THR))
         SH=SD-3600*HR
         TTMI=SH/60
         TMI=FLOAT(IFIX(TTMI))
         SC=SD-60*TMI-3600*HR
         RETURN
         END

        SUBROUTINE STIL(DOY,HR,TMI,TILA)
C THIS SUBROUTINE IS TO FIND THE TILT ANGLE OF THE GEOMAGNETIC AXIS IN 
C  DIFFERENT TIME: DOY(Day Of Year), HR(hour) and TMI(Minute)
C INPUT: DOY,HR,TMI
C OUTPUT: TILA(Degree)
        PI=3.14159
        RAD=PI/180.
        TD=DOY+(HR+TMI/60.)/24.-80.6
        TH=HR+TMI/60.-4.6
        FD=360./365.
        FH=360./24.
        TILA=23.5*SIN(RAD*FD*TD)-11.7*COS(RAD*FH*TH)
        RETURN
        END

         SUBROUTINE SDEN(TILA,XSM,YSM,ZDEN,RMP,IE)
C FIND THE POSITION OF DEN(Displaced Equatorial-Neutral) SHEET ALONG ZSM AXIS
C  (ZAEN), THE RADIUS OF THE CROSS SECTION OF THE MAGNETOPAUSE (RMP) FOR 
C  DIFFERENT XSM,YSM AND TILA(Tilt Angle) AND SHOW WHETHER AEN IS INSIDE THE
C  MAGNETOPAUSE OR NOT. WHERE: 
C  IE=1: IS INSIDE THE MAGNETOPAUSE, IE=2: IS OUTSIDE THE MAGNETOPAUSE   
C THE RADIUS OF THE CROSS SECTION OF THE MAGNETOPAUSE IS DETERMINED BY THE 
C  MAGNETOPAUSE MODEL OF SIBECK ET AL IN:
C  Sibeck, D. G., R. E. Lopez, and R. C. Roelof, Solar wind control of the
C  magnetopause shape, location, and motion, J. Grophys. Res., 96, 5489, 1991
C INPUT XSM,YSM,TILA(Degree)         
C OUTPUT ZDEN,RMP,IE

* INITIAL PARAMETERS
         IE=1
         H=10.5
         H1=10.05
         TIL=TILA*3.1416/180
         ATIL=ABS(TIL)
         IF(XSM.GE.0) GOTO 101
         CALL SD1(TIL,H,H1,XSM,D)
         YM21=((H1*(H+D))**2)*(1-(XSM/(H*COS(TIL)))**2)
         YM22=(H+D)**2-(D-XSM/COS(TIL))**2
         YM2=YM21/YM22
         IF(YM2.LT.0) THEN
          IE=2
          RETURN
         ENDIF 
         YM=SQRT(YM2)
         XD2=((H*COS(TIL))**2)*(1-(YSM/H1)**2)
         IF(ABS(YSM).GE.H1) XD2=0
* Find the Equatorial Region     
         XD=SQRT(XD2)
         RD=SQRT(XD**2+YSM**2)
         RSM=SQRT(XSM**2+YSM**2)
* Find DEN from 3 different equations in 3 different regions
 101     IF(XSM.GE.0.OR.RSM.LE.RD) THEN
          ZDEN=-XSM*SIN(TIL)/COS(TIL)
          GOTO 100
         ENDIF
         IF(ABS(YSM).GT.YM) THEN
          ZDEN=-D*SIN(TIL)
          GOTO 100
         ENDIF
         ZDEN=((H+D)*SQRT(1-(YSM**2)/YM2)-D)*SIN(TIL)
* Find whether the DEN surface is in the Magnetopause or not
 100     RP=SQRT(YSM**2+ZDEN**2)
         CALL SMPF(XSM,RMP,IM)
         IF(RP.GT.RMP.OR.IM.EQ.9999.99) IE=2
         RETURN
         END

         SUBROUTINE SD1(TIL,H,H1,XSM,D)
C FIND THE DISPLACED PARAMETER D OF THE DISPLACED NEUTRAL SHEET MODEL, SO THAT
C  IT PROVIDES APPROXIMATELY EQUAL CROSS-SECTIONAL AREAS ABOVE AND BELOW THE
C  NEUTRAL SHEET
C INPUT=TIL,H,H1,XSM    OUTPUT=D
         PAI=3.1416
         CT=COS(TIL)
         XX=XSM
         XH=-H*CT
         IF(XSM.GE.XH) XX=XH
* Radius of the cross-section of Olson's Magnetopause Model
         IF(XX.LE.-5.) RM=9*(10-3*XX)/(10-XX)+3 
         IF(XX.GT.-5.) RM=SQRT(18**2-(XX+5)**2) 
         RM2=RM**2
* If the cross-section areas above & below the Neutral Sheet are approxi-
* mately equal, then D satified: D**4+AA*D**3+BB*D**2+CC*D+DD=0. Where
         AA=4*H-(32*RM2*H**2)/(PAI**2*H1**2*(H-XX/CT)) 
         BB=2*H**2*(3-8*RM2/(PAI**2*H1**2)) 
         CC=4*(H**3)    
         DD=H**4     
         CALL SFA4(AA,BB,CC,DD,X)
         IF(XSM.GE.XH) THEN
          FK=-X/SQRT(-XH)
          D=-FK*SQRT(-XSM)
          RETURN
         ENDIF
         D=X
         RETURN
         END

         SUBROUTINE SFA4(AA,BB,CC,DD,X)
C FIND THE ROOT OF X**4+AA*X**3+BB*X**2+CC*X+DD=0
C INTITIAL PARAMETER
         NDX=0
         XMIN=0
         XMAX=50
         NDXMAX=3
         DX=1
         X=XMIN
         YY=X**4+AA*X**3+BB*X**2+CC*X+DD
 100     X=X+DX
         IF(NDX.GT.NDXMAX.OR.X.GE.XMAX) THEN
          NDX=0
          RETURN
         ENDIF
         Y=X**4+AA*X**3+BB*X**2+CC*X+DD
         RY=Y/YY
         IF(RY.LT.0) THEN
C THIS MEANS THAT Y.LE.0 AND YY.GT.0 OR Y.GT.0.AND
C       YY.LE.0, IN THIS CASE DX CHANGE TO ITS HIGHER ORDER
          X=X-DX
          DX=DX/10.
          NDX=NDX+1
          GOTO 100
         ENDIF
         YY=Y
         GOTO 100
         RETURN
         END     

         SUBROUTINE SMPF(XSM,RMP,IM)
C FIND THE RADIUS OF THE CROSS SECTION OF THE MAGNETOPAUSE RMP AND PARAMETER IE
C IE=1: INSIDE THE MAGNETOPAUSE, IE=2: OUTSIDE THE MAGNETOPAUSE  
C RMPIS DETERMINED BY THE 
C  MAGNETOPAUSE MODEL OF SIBECK ET AL IN:
C  Sibeck, D. G., R. E. Lopez, and R. C. Roelof, Solar wind control of the
C  magnetopause shape, location, and motion, J. Grophys. Res., 96, 5489, 1991 
         IM=0
         RMP2=-0.14*XSM**2-18.2*XSM+217.2
         IF(RMP2.LT.0) THEN
           IM=9999.99
           GOTO 50
         ENDIF
         RMP=SQRT(RMP2)
         IF(XSM.LE.-65) RMP=28.5
 50      RETURN
         END
