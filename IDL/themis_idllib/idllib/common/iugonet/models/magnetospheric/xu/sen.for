C     **************************************************************
C     *                     PROGRAM SEN.FOR                        *
C     **************************************************************
C This program is to find the SEN(Standard Equatorial Neutral) sheet inside 
C  the magnetopause in different time and position

C Input and Output:
C Input: DATE, TIME (In the following 2 different format) XSM, YSM, ZSM 
C    (Position in GSM Coordinate System)
C (1) When Date is Year+DOY(Day Of Year) and Time is Second of Day. For 
C    Example: Year=1995, DOY=34, SD=35000, then Date is 95034 and SD is 35000 
C (2) When Date is Year/Month/Day, and Time is Hour/Minute/Second. For example:
C    February 3, 1995, 2 Hour 5 minute and 10 second, then Date is 950203, and 
C    Time is 020520
C Output: ZSEN: Position of the neutral sheet along Zsm axis, at XSM and YSM
C         IE: Parameter to show whether the observed point in in or outside
C             the magnetopause

C Authors:
C Ronglan XU and Lei LI, Center for Space Sci. and Applied Res.,
C Chinese Academy of Sciences, PO Box 8701, Beijing 100080, China
C E-mail: XURL@SUN.IHEP.AC.CN, XURL@SUN20.CSSAR.AC.CN

C References:
C (1) SEN(Standard Equatorial Neutral):
C Xu, R.-L., Dynamics of the Neutral Sheet in the Magnetotail during Substorm,
C Advances in solar-terrestrial science of China,  ed. by W.-R. Hu et al., China
C Science Press, 1992. 
C (2) The magnetopause model:
C  Sibeck, D. G., R. E. Lopez, and R. C. Roelof, Solar wind control of the
C  magnetopause shape, location, and motion, J. Grophys. Res., 96, 5489, 1991

C Subroutines
C (1) SDOY(DATE,TIME,YR,DOY,HR,TMI,SC)
C   Find Year, Day of year, Month, Day of month, Hour, Minute, Second from 
C   Date(year/month/day) and Hour/Minute/Second 
C (2) STIME(YD,SD,YR,DOY,TMO,DAY,HR,TMI,SC)
C   Find Year,Day of Year, Month, Day of month, Hour, Minute and Second from 
C   Year and Day of Year and Second of Day
C (3) STIL(DOY,HR,TMI,TIL)
C   Find the tilt angle of the geomagnetic axis for different Date and Time
C (4) SSEN(TILA,XSM,YSM,ZSEN,RMP,IE)
C   Find the position of SEN sheet along ZSM axis for different Rate and Time
C (5) SMPF(XSM,RMP,IM)
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
        CALL SSEN(TILA,XSM,YSM,ZSEN,RMP,IE)
        WRITE(*,*)ZSEN,IE
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
C  DIFFERENT TIME: DOY(Day Of Year), HR(Hour) and TMI(Minute)
C INPUT: DOY,HR,TMI
C OUTPUT: TIL(Degree)
        PI=3.14159
        RAD=PI/180.
        TD=DOY+(HR+TMI/60.)/24.-80.6
        TH=HR+TMI/60.-4.6
        FD=360./365.
        FH=360./24.
        TILA=23.5*SIN(RAD*FD*TD)-11.7*COS(RAD*FH*TH)
        RETURN
        END

         SUBROUTINE SSEN(TILA,XSM,YSM,ZSEN,RMP,IE)
C FIND THE POSITION OF SEN(Standard Equatorial-Neutral) SHEET ALONG ZSM AXIS
C  (ZAEN), THE RADIUS OF THE CROSS SECTION OF THE MAGNETOPAUSE (RMP) FOR 
C  DIFFERENT XSM,YSM AND TILA(Tilt Angle) AND SHOW WHETHER AEN IS INSIDE THE
C  MAGNETOPAUSE OR NOT. WHERE: 
C  IE=1: IS INSIDE THE MAGNETOPAUSE, IE=2: IS OUTSIDE THE MAGNETOPAUSE   
C THE RADIUS OF THE CROSS SECTION OF THE MAGNETOPAUSE IS DETERMINED BY THE 
C  MAGNETOPAUSE MODEL OF SIBECK ET AL IN:
C  Sibeck, D. G., R. E. Lopez, and R. C. Roelof, Solar wind control of the
C  magnetopause shape, location, and motion, J. Grophys. Res., 96, 5489, 1991
C INPUT XSM,YSM,TILA(Degree)         
C OUTPUT ZSEN,RMP,IE

* INITIAL PARAMETERS
         IE=1
         H=10.5
         TIL=TILA*3.1416/180
         ATIL=ABS(TIL)
         IF(XSM.GE.0) GOTO 101
         YM2=H*(H-XSM/COS(TIL))
         IF(YM2.LT.0) THEN
          IE=2
          RETURN
         ENDIF
         YM=SQRT(YM2)
* Determinte the Equatorial Region       
         XD2=((H*COS(TIL))**2)*(1-(YSM/H)**2)
         IF(ABS(YSM).GE.H) XD2=0
         RD=SQRT(XD2+YSM**2)
         RSM=SQRT(XSM**2+YSM**2)
* Find EN using 3 diffrent Equations in 3 different Region       
 101     IF(XSM.GE.0.OR.RSM.LE.RD) THEN
          ZSEN=-XSM*SIN(TIL)/COS(TIL)
          GOTO 100
         ENDIF
         IF(ABS(YSM).GT.YM) THEN
          ZSEN=0
          GOTO 100
         ENDIF
         ZSEN=(H*(1-(YSM**2)/YM2))*SIN(TIL)
* Find whether the SEN surface is in the Magnetopause or not
 100     RP=SQRT(YSM**2+ZSEN**2)
         CALL SMPF(XSM,RMP,IM)
         IF(RP.GT.RMP.OR.IM.EQ.9999.99) IE=2
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