C******************************************************************
C************************* SOLPRO *********************************
C******************************************************************
C
C****************** TEST PROGRAM **********************************
C
	DIMENSION	F(10)
C
	WRITE(6,200)
	DO 2 ITAU=1,60,3
		TAU=ITAU
		CALL SOLPRO(TAU,82,F,INALE)
		WRITE(6,100) ITAU,F,INALE
2		CONTINUE
100	FORMAT(1X,I2,2X,10(1X,1PE9.3),I3)
200	FORMAT(5X,'INTERPLANETARY INTEGRAL SOLAR PROTON FLUX [PROTON/',
     &    '(CM*CM)]  IQ=82'//' NR OF',30X,'ENERGY THRESHOLD/MeV'/
     &	  ' MONTH',3X,'10',8X,'20',8X,'30',8X,'40',8X,'50',8X,'60',
     &    8X,'70',8X,'80',8X,'90',7X,'100','  AL-EVENTS')
	STOP
	END
C
C
      SUBROUTINE SOLPRO(TAU,IQ,F,INALE)                                 SOLPR010
C *** MODIFIED 9/77 TO RETURN NUMBER OF AL EVENTS (INALE). 		SOLPR020
C *** INTERPLANETARY SOLAR PROTON FLUX AT 1 AU (FROM E=10 TO E=100 MEV) SOLPR030
C *** SINGLE PRECISION DECK IN STANDARD FORTRAN IV FOR IBM 360 MACHINES SOLPR040
C *** (EBCDIC, 029 PUNCH) OR OTHER COMPATIBLE SYSTEMS.                  SOLPR050
C *** PROGRAM DESIGNED AND TESTED BY E.G. STASSINOPOULOS, CODE 601,     SOLPR060
C *** NASA GODDARD SPACE FLIGHT CENTER, GREENBELT, MARYLAND 20771 .     SOLPR070
C ********************************************************************* SOLPR080
C ****  INPUT: TAU     MISSION DURATION IN MONTHS (REAL*4)              SOLPR090
C ****         IQ      CONFIDENCE LEVEL THAT CALCULATED FLUENCE F(N)    SOLPR100
C ****                 WILL NOT BE EXCEEDED (INTEGER*4)                 SOLPR110
C **** OUTPUT: F(N)    SPECTRUM OF INTEGRAL SOLAR PROTON FLUENCE FOR    SOLPR120
C ****                 ENERGIES E=10*N (1=<N=<10) MEV                   SOLPR130
C ****         INALE   % OF AL EVENTS FOR GIVEN TAU AND Q               SOLPR140
      REAL NALE,NALECF(7,20)/-.1571,.2707,-.1269E-1,.4428E-3,-.8185E-5, SOLPR150
     $.7754E-7,-.2939E-9,-.1870,.1951,-.6559E-2,.1990E-3,-.3618E-5,     SOLPR160
     $.3740E-7,-.1599E-9,-.2007,.1497,-.3179E-2,.5730E-4,-.4664E-6,     SOLPR170
     $.1764E-8,0.,-.1882,.1228,-.1936E-2,.2660E-4,-.1022E-6,2*0.,       SOLPR180
     $-.2214,.1149,-.1871E-2,.2695E-4,-.1116E-6,2*0.,-.2470,.1062,      SOLPR190
     $-.1658E-2,.2367E-4,-.9465E-7,2*0.,-.2509,.8710E-1,-.8300E-3,      SOLPR200
     $.8438E-5,3*0.,-.2923,.8932E-1,-.1023E-2,.1029E-4,3*0.,-.3222,     SOLPR210
     $.8648E-1,-.9992E-3,.9935E-5,3*0.,-.3518,.8417E-1,-.1000E-2,       SOLPR220
     $.9956E-5,3*0.,-.3698,.7951E-1,-.8983E-3,.8940E-5,3*0.,-.2771,     SOLPR230
     $.5473E-1,-.1543E-4,4*0., -.2818,.5072E-1,.2511E-4,4*0.,-.2845,    SOLPR240
     $.4717E-1,.5664E-4,4*0.,-.2947,.4405E-1,.8507E-4,4*0.,-.2923,      SOLPR250
     $.4111E-1,.1106E-3,4*0.,-.2981,.3853E-1,.1312E-3,4*0.,-.3002,      SOLPR260
     $.3585E-1,.1529E-3,4*0.,-.3001,.3312E-1,.1781E-3,4*0.,-.3141,      SOLPR270
     $.3248E-1,.1654E-3,4*0./,F(10),G(10)                               SOLPR280
      REAL ORFLXC(5,9)/.154047E3,-.522258E4,.714275E5,-.432747E6,.955315SOLPR290
     $E6,.198004E3,-.448788E4,.438148E5,-.196046E6,.32552E6,.529120E3,  SOLPR300
     $-.122227E5,.112869E6,-.465084E6,.710572E6,.121141E4,-.266412E5,   SOLPR310
     $.226778E6,-.85728E6,.120444E7,.452062E4,-.103248E6,.896085E6,     SOLPR320
     $-.346028E7,.499852E7,.272028E4,-.499088E5,.35305E6,-.111929E7,    SOLPR330
     $.133386E7,.275597E4,-.469718E5,.314729E6,-.960383E6,.11165E7,     SOLPR340
     $.570997E4,-.799689E5,.381074E6,-.610714E6,0.,.101E3,4*0./         SOLPR350
      INTEGER INDEX(20)/2*7,6,3*5,5*4,9*3/                              SOLPR360
    1 FORMAT(' TAU=',F4.0,' IQ=',I3,3X,'PARAMETER(S) EXCEED PROGRAM LIMISOLPR370
     $TS')                                                              SOLPR380
    2 FORMAT(2X,'FOR THE COMBINATION OF TAU AND IQ GIVEN, NO SIGNIFICANTSOLPR390
     $ SOLAR PROTON FLUXES ARE TO BE EXPECTED. TAU=',F6.2,' IQ=',I2)    SOLPR400
      IF(TAU.GT.72..OR.IQ.LT.80)GO TO 500                               SOLPR410
      IP=100-IQ                                                         SOLPR420
      M=INDEX(IP)                                                       SOLPR430
      NALE=0.                                                           SOLPR440
      DO 300 J=1,M                                                      SOLPR450
  300 NALE=NALE+NALECF(J,IP)*TAU**(J-1)                                 SOLPR460
      INALE=NALE+1.0001                                                 SOLPR470
      IF(INALE.GT.0) GO TO 400                                          SOLPR480
C *** CALCULATIONS FOR OR-EVENT CONDITIONS                              SOLPR490
      IT=TAU                                                            SOLPR500
      IF(IT.EQ.1.AND.IP.GT.16) GO TO 700                                SOLPR510
      P=FLOAT(IP)/100.                                                  SOLPR520
      OF=0.                                                             SOLPR530
      DO 100 J=1,5                                                      SOLPR540
  100 OF=OF+ORFLXC(J,IT)* P**(J-1)*1.E7                                 SOLPR550
      E=10.                                                             SOLPR560
      DO 200 N=1,10                                                     SOLPR570
      G(N)=EXP(.0158*(30.-E))                                           SOLPR580
      F(N)=OF*G(N)                                                      SOLPR590
  200 E=E+10.                                                           SOLPR600
      GO TO 800                                                         SOLPR610
C *** CALCULATIONS FOR AL-EVENT CONDITIONS                              SOLPR620
  400 E=10.                                                             SOLPR630
      DO 600 N=1,10                                                     SOLPR640
      F(N)=7.9E9*EXP((30.-E)/26.5)*INALE                                SOLPR650
  600 E=E+10.                                                           SOLPR660
      GO TO 800                                                         SOLPR670
  700 WRITE(6,2) TAU,IQ                                                 SOLPR680
      GO TO 800                                                         SOLPR690
  500 WRITE (6,1) TAU,IQ                                                SOLPR700
  800 RETURN                                                            SOLPR710
      END                                                               SOLPR720
