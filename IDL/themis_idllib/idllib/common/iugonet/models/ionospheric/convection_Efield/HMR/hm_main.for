      PROGRAM RUNMOD5
C **********************************************************************
C                                                                      *
C PROGRAM TO USE A MODEL ELECTRIC POTENTIAL AND MODEL CONDUCTIVITY     *
C   TO CALCULATE FIELD-ALIGNED CURRENTS AND JOULE HEATING              *
C                                                                      *
C **********************************************************************
C *
C *DISCLAIMER:                                                         *
C *           THIS SOFTWARE IS PROVIDED TO THE USER WITHOUT ANY        *
C *        EXPRESSED OR IMPLIED GUARANTEE. THIS SOFTWARE IS INTENDED   *
C *        FOR USE IN RESEARCH BY COMPUTER PROGRAMMERS EXPERIENCED IN  *
C *        USE OF THE FORTRAN 77 LANGUAGE.  THIS SOFTWARE WAS DEVELOPED*
C *        BY EMPLOYEES OF THE UNITED STATES FEDERAL GOVERNMENT AND IS *
C *        NOT SUBJECT TO U.S. OR INTERNATIONAL COPYRIGHTS.            *
C **********************************************************************
C INPUTS - INTERACTIVE                                                 *
C   PK     - KP INDEX (FLOATING POINT FROM 0.0 TO 9.0)                 *
C   SUBLAT - GEOMAGNETIC LATITUDE OF SUB-SOLAR POINT                   *
C   F107   - FLUX OF 10.7 CM FLUX                                      *
C   MODLE  - INDEX FOR SPECIFYING POTENTIAL MODEL                      *
C            1 = HEPPNER-MAYNARD MODEL A                               *
C            2 = HEPPNER-MAYNARD MODEL BC                              *
C            3 = HEPPNER-MAYNARD MODEL DE                              *
C            4 = HEPPNER-MAYNARD MODEL BCP                             *
C            5 = HEPPNER-MAYNARD MODEL BCPP                            *
C            6 = HEPPNER-MAYNARD MODEL DEP                             *
C            7 = HEPPNER-MAYNARD MODEL DEPP                            *
C            8 = HEELIS MODEL                                          *
C            9 = MODEL OBTAINED FROM INPUT FILE (TAPE98)               *
C                                                                      *
C INPUTS - FILE                                                        *
C   TAPE99 - COEFFICENTS FOR HEPNER-MAYNARD MODEL (SEE OPEN STATEMENT) *
C   TAPE98 - TABLE OF ELECTRIC POTENTIALS (MODLE=9)                    *
C                                                                      *
C OUTPUTS - FILE                                                       *
C   TAPE1  - CONDUCTIVITY, ELECTRIC POTENTIAL, ETC IN TABULAR FORMAT   *
C            FOR INPUT TO PROGRAM WHICH CREATES PLOTS USING USER       *
C            SUPPLIED GRAPHICS SUBROUTINES (SEE OPEN STATEMENT)        *
C                                                                      *
C SUBROUTINES USED                                                     *
C   CONDUCT - CALCULATES HEIGHT-INTEGRATED CONDUCTIVITIES FOR          *
C             PRECIPITATING ELECTRONS USING HARDY'S STATISTICAL MODEL  *
C   CONDSUN - CALCULATES HEIGHT-INTEGRATED CONDUCTIVITIES FOR SOLAR UV *
C             INPUT                                                    *
C   EPOT    - CALCULATES ELECTRIC POTENTIALS FROM SPHERICAL HARMONIC   *
C             FIT TO HEPPNER-MAYNARD MODELS                            *
C   PMODEL  - CALCULATES ELECTRIC POTENTIALS FROM HEELIS MODEL         *
C   PBROT   - ROTATE COORDINATE SYSTEM, USED TO OFFSET HEELIS MODEL    *
C             FROM POLE TOWARD MIDNIGHT                                *
C   SETHEL  - INPUTS NON-DEFAULT VALUES FOR USE IN HEELIS MODEL WITH   *
C             INTERACTIVE INTERFACE                                    *
C                                                                      *
C COMMENTS:                                                            *
C   THIS PROGRAM SHOULD BE COMPILED WITH A FORTRAN 77 COMPILER.  IT    *
C   IS INTENDED TO BE RUN INTERACTIVELY WITH THE USER ANSWERING        *
C   QUESTIONS ABOUT MODEL PARAMETERS. THE PROGRAM PRESENTLY WORKS      *
C   AT THE AIR FORCE GEOPHYSICS LABORATORY, HANSCOM AIR FORCE BASE,    *
C   BEDFORD, MASSACHUSETTS 01731, USA, ON A CONTROL DATA CYBER 180     *
C   USING NOS AND ON AN IBM PERSONAL COMPUTER USING RYAN-MCFARLAND     *
C   FORTRAN AND MS-DOS.  IT IS UNKNOWN WHETHER THIS PROGRAM WILL WORK  *
C   ON OTHER COMPUTERS. IT IS KNOW THAT THE OPEN STATEMENTS ARE        *
C   DEPENDENT ON THE MACHINE BEING USED.                               *
C                                                                      *
C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
      DIMENSION Y(41), EPHI(41,25), EY(41,25), EZ(41,25)
      DIMENSION SH(41,25),SP(41,25),CZ(41,25), CY(41,25)
      DIMENSION SIGMA(41,25,2)
      DIMENSION C11(41,25)
      CHARACTER*1 ANS
      CHARACTER*10 LABEL(7)
      CHARACTER*10 MLBL(3),MNAME(2)
      CHARACTER*14 MODELE(9)
      EQUIVALENCE (SIGMA(1,1,1),SH(1,1)), (SIGMA(1,1,2),SP(1,1))
      COMMON/RUNCON/ PII,RAD,RE
      DATA MLBL/'PORH','CTIVITY MO','DEL/KP ='  /
      DATA MNAME/'HALL CONDU' ,'PED  CONDU' /
      DATA MODELE /      'H-M MODEL A   ', 'H-M MODEL BC  ',
     X 'H-M MODEL DE  ', 'H-M MODEL BCP ', 'H-M MODEL BCPP',
     X 'H-M MODEL DEP ', 'H-M MODEL DEPP', 'HEELIS MODEL  ',
     X 'TABLE (TAPE98)'/
      DATA EPHI /1025*0./, EY/1025*0./, EZ/1025*0./
      DATA SH /1025*0./, SP/1025*0./, CZ/1025*0./, CY/1025*0./
      DATA CUTL/0.01/,CUTH/.5/
      DATA POLE2M, POLE2D /5.,0./
c

      open(1,file='tape1',status='new')
      open(99,file='hmcoef.dat',status='old')
      PII = 4 * ATAN(1.)
      RAD = 180. / PII
      NMLT = 24
      JMLT = NMLT+1
      DLMLT = 2.*PII/FLOAT(NMLT)
      DLILAT = FLOAT(1)/RAD
      RE = 6.49E6
      DLY2D = 2.*DLMLT * RE
      DLZ2M = 2.*DLILAT * RE
      REWIND 1
      REWIND 99
C
C  ENTER PARAMETERS
C
C        GET VALUE OF KP INDEX
C
      write(*,*)'ENTER DESIRED KP FOR CONDUCTIVITIES: '
      READ (*,*,END=10000)PK
10000 KP=PK
      DKP = PK - FLOAT(KP)
C
C        GET INPUT FOR SOLAR CONDUCTIVITY
C
      write(*,*) ' ENTER GEOMAG LAT OF SUB-SOLAR POINT: (DEG) '
      SUBLAT = 0.0
      READ (*,*,END=10001)SUBLAT
10001 WRITE(*,*)' ENTER 10.7 CM FLUX: '
      F107 = 80.
      READ (*,*,END=10002) F107
10002 COLATS = (90-SUBLAT)/RAD
C
C         CALCULATE HALL AND PEDERSEN CONDUCTIVITIES
C
      WRITE(*,991)
  991 FORMAT(' DO YOU WANT TO OUTPUT CONDUCTIVITIES? (Y/N)')
      ANS = 'N'
      READ(*,103,END=10004) ANS
  103 FORMAT(A1)
10004 CONTINUE
	type *,'ans:',ans
      DO 10 MODEL=1,2
      WRITE(*,992) MODEL,MNAME(MODEL)
 992  FORMAT(1X,I2,1X,A9 )
      MLBL(1) = MNAME(MODEL)
C
C        WRITE OUT NAME OF MODEL (74 CHARACTERS)
C
  993 FORMAT(3A10,F3.1, '/SUN AT', F5.1,' DEG/F(10.7)=',
     X  F5.0,'      90., 50.,-12.,+12.')
      XLEVEL = 2.
      ITYPE = 1
      LABLEN = 64
      IF( ANS.EQ.'Y' ) THEN
        WRITE(1,990) ITYPE,LABLEN,XLEVEL
 990    FORMAT(2I2,F10.1)
        WRITE(1,993) MLBL,PK,SUBLAT,F107
      ENDIF
C
      DO 10 IMLT=1,JMLT
      TLOC=IMLT-13
      IF(TLOC.LT.0.) TLOC=TLOC+24.
      DO 20 ILAT=1,41
      ALAT=91-ILAT
      CALL CONDUCT(ALAT,TLOC,KP,DKP,CUTL,CUTH,MODEL,CONJ)
      GMLT = (TLOC*15.)/RAD
      COLAT = (90.-ALAT)/RAD
      CALL CONDSUN(COLAT,GMLT,COLATS,PII,F107,MODEL,CONSUN)
      Y(ILAT) = SQRT( CONJ**2 + CONSUN**2 )
      SIGMA(ILAT,IMLT,MODEL) = Y(ILAT)
   20 CONTINUE
C
      IF( ANS.EQ.'Y' )  WRITE(1,41) (Y(JJ),JJ=1,41)
   41 FORMAT(1X,15F7.1)
   10 CONTINUE
C
C         GET AN ELECTRIC POTENTIAL MODEL
C
 12   WRITE(*,395) (I,MODELE(I),I=1,9)
 395  FORMAT(' WHICH MODEL TO USE? ',/,
     X (1X,I3,'= ',A14, 5X, I3, '= ', A14)  )
      MODLE = 0
      READ (*,*,END=10003) MODLE
10003 CONTINUE
      IF( MODLE.LT.1 .OR. MODLE.GT.9 ) GO TO 12
C
C        WRITE HEADER TO OUTPUT FILE:
C  PLOT TYPE = CONTOUR PLOT.
C  FOR H-M MODEL A, BC AND DE AND FOR HEELIS MODEL, PLOT POTENTIAL
C  CONTOURS AT 4 KV LEVELS.
C  FOR H-M MODEL BCP, BCPP, DEP AND DEPP, PLOT AT 1.5 KV LEVELS.
C
      XLEVEL = 4.
      IF( MODLE.GT.3 .AND. MODLE.LT.8 ) XLEVEL = 1.5
      ITYPE = 1
      LABLEN = 46
      IF( MODLE.LE.7 ) THEN
C
C  ONE OF THE HEPPNER-MAYNARD MODELS HAS BEEN CHOSEN
       ANS = 'N'
       ICHGHM = 0
       WRITE(*,*)'DO YOU WISH TO CHANGE THE HEPPNER-MAYNARD PATTERN? ( Y
     X/N ) '
       READ(*,103,END=11003) ANS
11003  IF( ANS .EQ. 'Y') THEN
        IF( MODLE .LE. 3 ) THEN
         ICHGHM=1
         CALL SETHM(MODLE,ACHG,BCHG,DXCHG,DYCHG,AHM,BHM,DXHM,DYHM,CHGV)
        ELSE
        WRITE(*,*)'CANNOT CHANGE ANY OF THE H-M PATTERNS FOR IMF BZ > 0'
        ENDIF
       ENDIF
C
C                      WRITE PLOT TITLE FOR HEPPNER-MAYNARD MODEL
       WRITE(1,990) ITYPE,LABLEN,XLEVEL
       WRITE(1,995) MODELE(MODLE)
 995    FORMAT(        A14,' ELECTRIC POTENTIAL (KV)                 ---
     X----------  ')
      ELSE
C
C                   GET INPUTS FOR HEELIS MODEL AND WRITE TITLE FOR PLOT
       IF( MODLE.EQ.8 ) THEN
       WRITE(1,990) ITYPE,LABLEN,XLEVEL
         WRITE(*,994)
  994    FORMAT(' CHANGE ROTATION OF HEELIS PATTERN? (Y/N)',/,
     X          ' (DEFAULT = 5.0 DEG AND 0.0 HRS)')
         ANS = 'N'
         READ(*,103,END=10005) ANS
10005    IF( ANS.EQ.'Y' ) THEN
           WRITE(*,984)
 984       FORMAT(' ROTATION ANGLE (DEG) FROM POLE TOWARD MIDNIGHT=')
           POLE2M = 0.
           READ(*,985,END=10006) POLE2M
 985       FORMAT(G15.6)
10006      WRITE(*,986)
 986       FORMAT(' ROTATION ANGLE (HR) AROUND POLE TOWARD DUSK=')
           POLE2D = 0.
           READ(*,985,END=10007) POLE2D
10007      CONTINUE
           WRITE(*,*) POLE2M, POLE2D
         ENDIF
         CALL SETHEL
       ENDIF
      ENDIF
      IF( MODLE .NE. 9 ) THEN
C
C            CALCULATE ELECTRIC POTENTIAL FROM APPROPRIATE ALGORITHM
       DO 100 IMLT=1,JMLT
       TLOC = IMLT - 13
       DO 120 ILAT=3,40
       GLONG = TLOC*15.
       GMLAT = FLOAT(91-ILAT)
       IF( MODLE .LT.8 ) THEN
        CALL EPOT(GMLAT,GLONG,PHI,99,MODLE,16,ACHG,BCHG,DXCHG,DYCHG,
     X            AHM,BHM,DXHM,DYHM,ICHGHM)
        IF( ICHGHM .EQ. 1 ) PHI=PHI*CHGV
       ELSE
        CALL PBROT(GMLAT,TLOC,POLE2M,POLE2D,GMLAT2,TLOC2)
        CALL PMODEL(GMLAT2,TLOC2,PHI,DPDLAT,DPDLT)
       ENDIF
       EPHI(ILAT,IMLT) = PHI*1.E3
 120   CONTINUE
       EPHI(41,IMLT) = EPHI(40,IMLT)
 141   FORMAT(1X,15F7.2)
 100   CONTINUE
C
C  FIT OF H-M MODEL DOES NOT WORK WELL ABOVE 88 DEGREES, SO
C  WE WILL FILL IN THAT AREA WITH A SIMPLE EXTRAPOLATION
       YA = 0.
       DO 130 JJ = 1,24
 130   YA = YA + EPHI(3,JJ)
       YA = YA / 24.
       DO 132 JJ = 1,25
       EPHI(1,JJ) = YA
       EPHI(2,JJ) = (EPHI(3,JJ) + YA) / 2.
 132   CONTINUE
C
C  ADJUST POTENTIAL SO THAT LOW LATITUDE POTENTIAL IS ZERO
       YA = 0.
       DO 145 JJ=1,24
 145   YA = YA + EPHI(41,JJ)
       YA = YA /24.
       DO 148 JJ=1,25
       DO 148 II=1,41
       EPHI(II,JJ) = EPHI(II,JJ) - YA
 148   CONTINUE
C
C   WRITE CALCULATED ELECTRIC POTENTIAL TO OUTPUT FILE FOR PLOTTING
       DO 149 JJ=1,25
       DO 147 II=1,41
 147   Y(II) = EPHI(II,JJ)/1.E3
       WRITE(1, 41) Y
 149   CONTINUE
      ELSE
C             DONE WITH CALCULATING A POTENTIAL
C                            - OR -
C       USE A ELECTRIC POTENTIAL FROM A TABLE / OUTPUT TO PLOT FILE
       open(98,file='tape98',status='old')
       READ(98, 990) ITYPE,LABLEN,XLEVEL
       WRITE(1, 990) ITYPE,LABLEN,XLEVEL
       READ(98, 980) LABEL
       WRITE(1, 980) LABEL
 980   FORMAT(6A10,A4)
       DO 150 JJ = 1,25
       READ(98, 41) (EPHI(II,JJ),II=1,41)
       WRITE(1, 41) (EPHI(II,JJ),II=1,41)
       DO 150 II=1,41
 150   EPHI(II,JJ) = EPHI(II,JJ) * 1.E3
      ENDIF
C
C      CALCULATE: ELECTRIC FIELD (MV/M) AND JOULE HEATING RATE
C                                                 (MILLI-WATT/M**2)
      DO 210 I=2,40
       COLAT = FLOAT(I-1)/RAD
       DLY2M = DLY2D*SIN(COLAT)
       IP = I+1
       IM = I-1
       DO 210 J=1,JMLT
        JP = J+1
        JM = J-1
        IF( J.EQ.1 ) JM = JMLT-1
        IF( J.EQ.JMLT ) JP = 2
        EZ(I,J) = 1.E3 * (EPHI(IM,J)-EPHI(IP,J))/DLZ2M
        EY(I,J) = 1.E3 * (EPHI(I,JM)-EPHI(I,JP))/DLY2M
  210 CONTINUE
      DO 212 J=1,JMLT
       EZ(1,J) = EZ(2,J)
       EY(1,J) = EY(2,J)
       EZ(41,J) = EZ(40,J)
       EY(41,J) = EY(40,J)
  212 CONTINUE
C
C       WRITE JOULE HEATING VALUES TO A OUTPUT FILE FOR PLOTTING
      XLEVEL = 1.0
      IF( MODLE.GT.3 .AND. MODLE.LT.8 ) XLEVEL = 0.3
      ITYPE = 1
      LABLEN = 27
      WRITE(1,990) ITYPE,LABLEN,XLEVEL
      WRITE(1,975)
  975 FORMAT('JOULE HEATING RATE MW/M**2                   -------------
     X----------  ' )
      DO 211 IMLT=1,JMLT
      DO 209 ILAT=1,41
      Y(ILAT)= SIGMA(ILAT,IMLT,2)*(EY(ILAT,IMLT)**2+EZ(ILAT,IMLT)**2)
     X                           *1.E-3
 209  C11(ILAT,IMLT) = Y(ILAT)
      WRITE(1,41)  (Y(JJ),JJ=1,41)
 211  CONTINUE
C
C       WRITE NET JOULE HEAT TO TERMINAL
      CALL CFACNET(C11,41,JMLT,TOTJOUL,TOTNEG)
      TOTJOUL = TOTJOUL * 1.E-3
      WRITE(*,411) TOTJOUL
 411  FORMAT(' TOTAL JOULE HEAT =', 1PG15.6,' WATTS')
C
C       WRITE ELECTRIC FIELD VALUES TO OUTPUT FILE FOR PLOTTING
C  VECTOR PLOTS: MAXIMUM VECTOR LENGTH = 100 MV/M
      XLEVEL =  100.
      IF( MODLE.GT.3 .AND. MODLE.LT.8 ) XLEVEL = 40.
      ITYPE = 2
      LABLEN = 22
      WRITE(1,990) ITYPE,LABLEN,XLEVEL
      WRITE(1,996)
  996 FORMAT('ELECTRIC FIELD (MV/M)                       --------------
     X----------  ' )
      DO 215 J=1,JMLT
 215  WRITE(1,41)  (EY(JJ,J),JJ=1,41)
      DO 216 J=1,JMLT
 216  WRITE(1,41)  (EZ(JJ,J),JJ=1,41)
C
C                 IONOSPHERIC CURRENT (AMPS/KM)
C
      DO 220 I = 3,41
      COLAT = (I-1)/RAD
      CTHE = COS(COLAT)
      SKAI = 2.*CTHE/SQRT(1. + 3.*CTHE*CTHE)
      DO 219 J = 1,JMLT
      CZ(I,J) = SP(I,J)/SKAI*EZ(I,J) + SH(I,J)*EY(I,J)
      CY(I,J) = -SH(I,J)*EZ(I,J) + SP(I,J)*SKAI*EY(I,J)
  219 CONTINUE
  220 CONTINUE
C
C  QUICK FIX  FOR POLE
      DO 224 JJ = 1,JMLT
      CZ(1,JJ) = 0.
      CY(1,JJ) = 0.
      CZ(2,JJ) = CZ(3,JJ) / 2.
      CY(2,JJ) = CY(3,JJ) / 2.
 224  CONTINUE
C  OUTPUT IONOSPHERIC CURRENT
      XLEVEL =  400.
      IF( MODLE.GT.3 .AND. MODLE.LT.8 ) XLEVEL = 200.
      ITYPE = 2
      LABLEN = 31
      WRITE(1,990) ITYPE,LABLEN,XLEVEL
      WRITE(1,997)
  997 FORMAT('IONOSPHERIC CURRENT (AMPS/KM)  ---------------------------
     X----------  ')
      DO 226 J=1,JMLT
      WRITE(1,41) ( CY(JJ,J),JJ=1,41)
  226 CONTINUE
      DO 228 J=1,JMLT
      WRITE(1,41) ( CZ(JJ,J),JJ=1,41)
  228 CONTINUE
C
C                           FIELD-ALIGNED CURRENTS (MICRO-AMPS/M**2)
      DO 300 I=2,40
      COLAT = FLOAT(I-1)/RAD
      DLY2M = DLY2D*SIN(COLAT)
      IP = I+1
      IM = I-1
      DO 300 J=1,JMLT
      JP = J+1
      JM = J-1
      IF( J.EQ.1 ) JM = JMLT-1
      IF( J.EQ.JMLT ) JP = 2
      C11(I,J) = 1.E3 * (  (CZ(IP,J)-CZ(IM,J))/DLZ2M
     X                   + (CY(I,JP)-CY(I,JM))/DLY2M )
  300 CONTINUE
      DO 311 J=1,JMLT
      C11(41,J) = C11(40,J)
  311 CONTINUE
C
C  MAKE FIELD-ALIGNED CURRENT AT THE POLE INDEPENDENT OF MLT (LONGITUDE)
       YA = 0.
      DO 320 JJ = 1, 24
 320  YA = YA + C11(2,JJ)
      YA = YA / 24.
      DO 322 JJ = 1,25
 322  C11(1,JJ) = YA
C
C  CALCULATE TOTAL CURRENT INTO/OUTOF IONOSPHERE (AMPS)
      CALL CFACNET(C11,41,JMLT,FACPOS,FACNEG)
C CONVERT FROM MICRO-AMPS TO AMPS
      FACPOS = FACPOS * 1.E-6
      FACNEG = FACNEG * 1.E-6
      WRITE(*,414) FACPOS, FACNEG
 414  FORMAT(' TOTAL POSITIVE CURRENT INTO HEMISPHERE =',1P,G15.6,' AMPS
     X ',/,' TOTAL NEGATIVE CURRENT INTO HEMISPHERE =',G15.6,'AMPS')
C
C  NOW OUTPUT THE FIELD ALIGNED CURRENT
      XLEVEL = .1
      ITYPE = 0
      LABLEN = 38
      WRITE(1,990) ITYPE,LABLEN,XLEVEL
      WRITE(1,988)
  988 FORMAT('FIELD-ALIGNED CURRENT (MICRO-AMP/M**2)   -----------------
     X----------  ' )
      DO 315 IMLT=1,JMLT
  315 WRITE(1,41) (C11(JJ,IMLT),JJ=1,41)
C
      STOP
      END

