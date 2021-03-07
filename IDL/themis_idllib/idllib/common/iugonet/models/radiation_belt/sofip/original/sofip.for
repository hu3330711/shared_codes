C **********************************************************************SOFIP001
C ************    SHORT ORBITAL FLUX INTEGRATION PROGRAM     ***********SOFIP002
C *******    FOR USE WITH NSSDC'S STANDARD ENVIRONMENT MODELS    *******SOFIP003
C **********************************************************************SOFIP004
C ** DESIGNED AND TESTED BY STASSINOPOULOS, HEBERT, BUTLER, & BARTH   **SOFIP005
C ** CODE 601, NASA/GODDARD SPACE FLIGHT CENTER; GREENBELT, MD. 20771 **SOFIP006
C ** SINGLE PRECISION DECK FOR FORTRAN IV   (EBCDIC,029 PUNCH)        **SOFIP007
C ** TRAJECTORY INPUT FROM UNFORMATTED BINARY OR BCD FORMATTED TAPE   **SOFIP008
C **********************************************************************SOFIP009
C ***                                                                   SOFIP010
C ***  INPUT PARAMETERS:                                                SOFIP011
C ***  * NAME  : 12-CHARACTER MISSION (OR PROJECT) NAME                 SOFIP012
C ***  * INCL  : APPROXIMATE INCLINATION OF ORBIT PLANE IN DEGREES (I*4)SOFIP013
C ***  * IPRG  : APPROXIMATE PERIGEE ALTITUDE IN KILOMETERS        (I*4)SOFIP014
C ***  * IAPG  : APPROXIMATE APOGEE ALTITUDE IN KILOMETERS         (I*4)SOFIP015
C ***  * MODEL : NUMBER OF FIELD-MODEL USED IN B/L CALCULATION     (R*4)SOFIP016
C ***  * PERIOD: MATHEMATICAL PERIOD OF ORBIT IN HOURS             (R*4)SOFIP017
C ***  * BLTIME: EPOCH OF FIELD-MODEL USED IN B/L CALCULATION      (R*4)SOFIP018
C ***  * NRGYLV: THRESHOLD-ENERGY SELECTOR FOR RUNNING PRINTOUT    (I*4)SOFIP019
C ***  * ITAPE : B/L ORBIT TAPE IDENTIFIER, < 10000                (I*4)SOFIP020
C ***  * NTABLS: # OF OUTPUT-TABLE SETS PER TRAJECTORY             (I*4)SOFIP021
C ***  * CUTOFF: ORBIT DURATION IN DECIMAL HOURS                   (R*4)SOFIP022
C ***  * ISKIP : POSITION SKIPPING CONTROL                         (I*4)SOFIP023
C ***  * KPRINT: RUNNING PRINTOUT CONTROL                          (I*4)SOFIP024
C ***                                                                   SOFIP025
C ***  INPUT VARIABLES:                                                 SOFIP026
C ***  * PSNTIM:  POSITIONAL TIME (DECIMAL HOURS)                       SOFIP027
C ***  * PSNLON:      "      LONGITUDE (DEGREES)                        SOFIP028
C ***  * PSNLAT:      "      LATITUDE (DEGREES)                         SOFIP029
C ***  * PSNALT:      "      ALTITUDE (KILOMETERS)                      SOFIP030
C ***  * PSNB  :      "      FIELD MAGNITUDE (GAUSS)                    SOFIP031
C ***  * PSNL  :      "      SHELL PARAMETER (EARTH RADII)              SOFIP032
C **********************************************************************SOFIP033
C *** TO READ BCD FORMATTED ORBIT TAPES, UNCOMMENT LINES 132,137,& 143. SOFIP034
C *** COMMENT OUT LINES 133-134,138-139,& 144.                          SOFIP035
C *** TO READ UNFORMATTED BINARY ORBIT TAPES, UNCOMMENT LINES 133-134,  SOFIP036
C *** 138-139,& 144.  COMMENT OUT LINES 132,137,& 143.                  SOFIP037
C *** ********************  BLOCK 0: INITIALIZATION  *******************SOFIP038
C     COMMON /AP8MAC/DESCR(8),LIST(1)                                   SOFIP039
C     COMMON /AE6MAX/DESCR(8),LIST(1)                                   SOFIP040
C     COMMON /AEI7HI/DESCR7(8),LIST7(1)                                 SOFIP041
C     COMMON /AEI7LO/DESCR7(8),LIST7(1)                                 SOFIP042
C     COMMON /AE5MIN/DESCR(8),LIST(1)                                   SOFIP043
C     COMMON /AP8MIC/DESCR(8),LIST(1)                                   SOFIP044
      REAL MODLAB*8(4,7)/'HENDRICK','S&CAIN 9','9-TERM G','SFC 9/65',' CSOFIP045
     $AIN ET','.AL. 120','-TERM GS','FC 12/66',' CAIN&LA','NGEL 143','-TSOFIP046
     $ERM PO','GO 10/68',' CAIN&SW','EENEY 12','0-TERM P','OGO 8/69','  SOFIP047
     $  IGRF',' 1965.0 ','80-TERM ','10/68   ',' LEATON ','MALIN EV','ANSOFIP048
     $S 80-T','ERM 1965','   HURWI','TZ US C&','GS 168-T','ERM 1970'/,  SOFIP049
     $MODLBL*8(4),AP8/' AP8'/,MAX/'MAX '/,MIN/'MIN '/,LOW/'S LO'/,      SOFIP050
     $MAC/'MAC'/,DESCR7(8),BINDMY*8(5),ADUMMY(6),MOD7/'LO-7'/           SOFIP051
      REAL ENERGY(31,2)/2.,3.,4.,5.,6.,8.,10.,15.,20.,25.,30.,35.,40.,  SOFIP052
     $45.,50.,55.,60.,70.,80.,90.,100.,125.,150.,175.,200.,250.,300.,   SOFIP053
     $350.,400.,500.,0.,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.,1.25,1.5,1.75,2., SOFIP054
     $2.25,2.5,2.75,3.,3.25,3.5,3.75,4.,4.25,4.5,4.75,5.,5.5,6.,6.5,7., SOFIP055
     $0.0/,SPNRG(20)/10.,20.,30.,40.,50.,60.,70.,80.,90.,100.,110.,120.,SOFIP056
     $130.,140.,150.,160.,170.,180.,190.,200./                          SOFIP057
      INTEGER NRGRNG(10,2)/1,3,5,7,12,20,22,26,30,31,1,5,8,10,12,13,14, SOFIP058
     $22,30,31/,IZONE(120)/10*1,17*2,93*3/,NRBITO/1/                    SOFIP059
      DIMENSION FLUXES(30),ALGFLX(30),ALNFLX(30),DIFSPC(30),EXPFLX(10), SOFIP060
     $PKVALU(50,8),AIFLXS(30),ENRNGS(11),EXPTIM(10),IYMD(3),LCOUNT(4),  SOFIP061
     $DIFFLX(30),NAME(3),PKFLX(50),PKTIM(50),PTIME(4),                  SOFIP062
     $PKLON(50),PKLAT(50),PKALT(50),PKB(50),PKL(50),TAUFLX(50),F(20)    SOFIP063
      EQUIVALENCE(PKVALU(1,1),PKFLX(1)),(PKVALU(1,2),PKLON(1)),(PKVALU(1SOFIP064
     $,3),PKLAT(1)),(PKVALU(1,4),PKALT(1)),(PKVALU(1,5),PKTIM(1)),(PKVALSOFIP065
     $U(1,6),PKB(1)),(PKVALU(1,7),PKL(1)),(PKVALU(1,8),TAUFLX(1))       SOFIP066
      REAL TYPLBL(3,2)/'  PR','OTON','S   ','ELEC','TRON','S HI'/,      SOFIP067
     $FIRNGS(11)/'0.E0','1.E0','1.E1','1.E2','1.E3','1.E4','1.E5',      SOFIP068
     $'1.E6','1.E7','OVER',' '/,XLABEL*8(3,2)/'PRNRGY','PRINTG','PRDIFF'SOFIP069
     $,'ELNRGY','ELINTG','ELDIFF'/,PROTLB*8(2)/'SPNRGY','SPFLUX'/       SOFIP070
    1 FORMAT('1NAME  = ',3A4/' INCL  = ',I3/' IPRG  = ', I6/' IAPG = ', SOFIP071
     $I6/' ITAPE = ',I4/' MODEL = ',I2/' PERIOD= ',F9.6/                SOFIP072
     $' BLTIME= ',F7.2/' NRGLEV= ',I2/' NTABLS= ',I2/' CUTOFF= ',F6.2/  SOFIP073
     $' ISKIP= ',I2/' KPRINT= ',I2//)                                   SOFIP074
    2 FORMAT('1',131('*')/' * SOFIP : SHORT ORBITAL FLUX INTEGR. PROGRAMSOFIP075
     $ FOR STANDARD NSSDC PROTON AND ELECTRON ENVIR. MODELS (SPECIES CONSOFIP076
     $SIDERED SEPARATELY) *'/' * MAGNETIC PARAMETERS B AND L COMPUTED WISOFIP077
     $TH GEOMAGN. FIELD MODEL',I3,': ',4A8,' * COEFF. UPDATED TO:',F7.1,SOFIP078
     $' *'/' * PROJECT :',3A4,' * INCLIN=',I3,'DEG * PERIG=',I5,'KM * APSOFIP079
     $OG=',I6,'KM * B/L TAPE=TD',I4,' * PERIOD=',F7.3,'HRS * SOLAR ',A3,SOFIP080
     $'IMUM     *'/' * FOR INFORMATION OR EXPLANATION CONTACT E.G. STASSSOFIP081
     $INOPOULOS AT NASA-GSFC,CODE 601, GREENBELT, MARYLAND 20771, TEL.(3SOFIP082
     $01)-344-8067 *'/1X,131('*')//)                                    SOFIP083
    3 FORMAT(2('1'/12('0'/)/53X,28('*')/53X,'**',6X,3A4,6X,'**'/53X,'**'SOFIP084
     $,I3,'DEG/',I5,'KM/',I6,'KM **'/53X,28('*')/))                     SOFIP085
    4 FORMAT (3A4,7X,I3,7X,I6,4X,I6,4X,I2,8X,F9.6,1X,F7.2/I2,7X,I4,     SOFIP086
     $6X,I2,8X,F6.2,4X,I2,8X,I2)                                        SOFIP087
    5 FORMAT(6E18.8)                                                    SOFIP088
C *** ********************  BLOCK 1: INITIALIZATION  *******************SOFIP089
   10 READ(5,4,END=999) NAME,INCL,IPRG,IAPG,MODEL,PERIOD,BLTIME,NRGYLV, SOFIP090
     $ITAPE,NTABLS,CUTOFF,ISKIP,KPRINT                                  SOFIP091
      ITYPE=1                                                           SOFIP092
      WRITE(6,3) (NAME,INCL,IPRG,IAPG,I=1,2)                            SOFIP093
      NORBIT=1                                                          SOFIP094
      IPASS=1                                                           SOFIP095
      IPRINT=KPRINT                                                     SOFIP096
      ASSIGN 110 TO NGO2                                                SOFIP097
      L=0                                                               SOFIP098
      LSUM=0                                                            SOFIP099
      EXPFCT=0.0                                                        SOFIP100
      XAMNIM=MAX                                                        SOFIP101
      ISWTCH=1                                                          SOFIP102
      IF(DESCR(1).EQ.AP8) GO TO 15                                      SOFIP103
      ITYPE = 2                                                         SOFIP104
      ASSIGN 120 TO NGO2                                                SOFIP105
      IF(DESCR(2).NE.MAX) XAMNIM=MIN                                    SOFIP106
      IF(DESCR7(2).EQ.MOD7) TYPLBL(3,2)=LOW                             SOFIP107
      GO TO 17                                                          SOFIP108
   15 IF(DESCR(2).NE.MAC)  XAMNIM=MIN                                   SOFIP109
   17 DO 20 I=1,4                                                       SOFIP110
      LCOUNT(I)=0                                                       SOFIP111
   20 MODLBL(I) = MODLAB(I,MODEL)                                       SOFIP112
      TAU = PERIOD                                                      SOFIP113
      FLXSUM = 0.0                                                      SOFIP114
      OFLXSM = 0.0                                                      SOFIP115
      PEAK = -1.0                                                       SOFIP116
      DO 30 NRNG=1,10                                                   SOFIP117
      ENRNGS(NRNG) = ENERGY(NRGRNG(NRNG,ITYPE),ITYPE)                   SOFIP118
      EXPTIM(NRNG) = 0.0                                                SOFIP119
   30 EXPFLX(NRNG) = 0.0                                                SOFIP120
      DO 35 NRGSP=1,20                                                  SOFIP121
   35 F(NRGSP)=0.0                                                      SOFIP122
      DO 40 NRG=1,30                                                    SOFIP123
      AIFLXS(NRG)=0.0                                                   SOFIP124
      ALNFLX(NRG) = 0.0                                                 SOFIP125
      DIFSPC(NRG) = 0.0                                                 SOFIP126
   40 FLUXES(NRG) = 0.0                                                 SOFIP127
C *** WRITE OUT INPUT PARAMETERS                                        SOFIP128
      WRITE(6,1)NAME,INCL,IPRG,IAPG,ITAPE,MODEL,PERIOD,BLTIME,NRGYLV,   SOFIP129
     $NTABLS,CUTOFF,ISKIP,KPRINT                                        SOFIP130
C *** ************************  BLOCK 2: INPUT  ************************SOFIP131
C     READ(9,5,END=400,ERR=10)PSNTM1,PSNLN1,PSNLT1,PSNAL1,PSNB1,PSNL1   SOFIP132
      READ(9,END=400,ERR=10)PSNTM1,PSNLN1,DUMMY,PSNLT1,DUMMY,PSNAL1,    SOFIP133
     $DUMMY,PSNB1,DUMMY,PSNL1,DUMMY                                     SOFIP134
      TMLAST = PSNTM1                                                   SOFIP135
   50 DO 60 ISKP=1,ISKIP                                                SOFIP136
C     READ(9,5,END=400,ERR=10) PSNTIM,PSNLON,PSNLAT,PSNALT,PSNB,PSNL    SOFIP137
      READ(9,END=400,ERR=10)PSNTIM,PSNLON,DUMMY,PSNLAT,DUMMY,PSNALT,    SOFIP138
     $DUMMY,PSNB,DUMMY,PSNL,DUMMY                                       SOFIP139
   60 CONTINUE                                                          SOFIP140
      IF(PSNTIM.LE.CUTOFF) GO TO 65                                     SOFIP141
C *** DUMMY READ LOOP TO READ TO END OF FILE                            SOFIP142
C  66 READ(9,5,END=400,ERR=10) ADUMMY                                   SOFIP143
   66 READ(9,END=400,ERR=10) BINTIM,BINDMY                              SOFIP144
      GO TO 66                                                          SOFIP145
   65 CONTINUE                                                          SOFIP146
C *** *********************  BLOCK 3: CALCULATIONS  ********************SOFIP147
C *** CALCULATE KPSTEP (NUMBER OF MINUTES BETWEEN POINTS ON B/L TAPE)   SOFIP148
      GO TO (70,80), IPASS                                              SOFIP149
   70 KPSTEP = INT((PSNTIM-TMLAST)/.0166667+0.1)                        SOFIP150
   80 TMLAST = PSNTIM                                                   SOFIP151
C *** TEST L-VALUE  & BYPASS FLUX CALCULATIONS IF WARRANTED             SOFIP152
      IF(PSNL.GT.0.0.AND.PSNL.LT.12.0) GO TO NGO2,(110,120)             SOFIP153
      DO 100 NRG=1,30                                                   SOFIP154
  100 FLUXES(NRG) = 0.0                                                 SOFIP155
      GO TO 170                                                         SOFIP156
C *** OBTAIN COMMON LOGARITHM OF POSITIONAL FLUXES (ALGFLX)             SOFIP157
C *** PROTONS                                                           SOFIP158
  110 CALL TRARA1(DESCR,LIST,PSNL,PSNB,ENERGY(1,1),ALGFLX(1),30)        SOFIP159
      GO TO 140                                                         SOFIP160
C *** ELECTRONS                                                         SOFIP161
  120 IF(INT(100.0*PSNL+0.2).LE.280) GO TO 130                          SOFIP162
      CALL TRARA1(DESCR7,LIST7,PSNL,PSNB,ENERGY(1,2),ALGFLX(1),30)      SOFIP163
      GO TO 140                                                         SOFIP164
  130 CALL TRARA1(DESCR,LIST,  PSNL,PSNB,ENERGY(1,2),ALGFLX(1),30)      SOFIP165
C *** CONVERT LOG-FLUX TO FLUX                                          SOFIP166
  140 DO 150 NRG=1,30                                                   SOFIP167
      FLUXES(NRG) = 10.0**ALGFLX(NRG)                                   SOFIP168
  150 IF(FLUXES(NRG).LT.1.001) FLUXES(NRG) = 0.0                        SOFIP169
C *** SUM FLUXES FOR (A) RUNNING PRINTOUT, (B) TABULAR OUTPUT           SOFIP170
      FLXSUM = FLXSUM+FLUXES(NRGYLV)*FLOAT(KPSTEP)*60.                  SOFIP171
      DO 160 NRG=1,30                                                   SOFIP172
  160 AIFLXS(NRG) = AIFLXS(NRG)+FLUXES(NRG)                             SOFIP173
  170 CONTINUE                                                          SOFIP174
C *** ********************  RUNNING PRINTOUT MODULE  *******************SOFIP175
      GO TO (200,210),IPASS                                             SOFIP176
  200 WRITE (6,2) MODEL,MODLBL,BLTIME,NAME,INCL,IPRG,IAPG,ITAPE,PERIOD, SOFIP177
     $XAMNIM                                                            SOFIP178
      WRITE (6,201)(TYPLBL(I,ITYPE),I=1,3), ENERGY(NRGYLV,ITYPE)        SOFIP179
  201 FORMAT('0',21X,'***** ',3A4,'(E>',G9.3,'MEV)   *****'//    '  LONGSOFIP180
     $.    LAT.    ALT.   FIELD    LINE   ORBIT    POSITIONAL  TIME-INTESOFIP181
     $G    ORBITAL'/' ',T28,'-B-',T37,'-L-    TIME       FLUX     PSTNL SOFIP182
     $FLUX   FLUX(SUM)'/'  (DEG)    (DEG)   (KM)  (GAUSS)  (E.R.)  (HRS)SOFIP183
     $   #/CM**2/SEC')                                                  SOFIP184
      WRITE(6,202)PSNTM1,PSNLN1,PSNLT1,PSNAL1,PSNB1,PSNL1               SOFIP185
  202 FORMAT(' ',T41,F9.5,T2,F7.2,1X,F6.2,1X,F8.1,1X,F8.5,1X,F5.2,T50,  SOFIP186
     $7(2X,1PE10.3))                                                    SOFIP187
  210 IF(MOD(IPRINT,KPRINT).NE.0) GO TO 220                             SOFIP188
      TIFLUX = FLUXES(NRGYLV)*FLOAT(KPSTEP)*60.                         SOFIP189
      WRITE(6,202)PSNTIM,PSNLON,PSNLAT,PSNALT,PSNB,PSNL,                SOFIP190
     $FLUXES(NRGYLV),TIFLUX,FLXSUM                                      SOFIP191
  220 IPRINT=IPRINT+1                                                   SOFIP192
C *** *****************  ORBIT L-ZONE BREAKDOWN MODULE  ****************SOFIP193
C *** *******  THIS MODULE MUST BE USED WITH PERCENT TIME MODULE  ******SOFIP194
C *** STORE TIME IN INNER & OUTER ZONE, EXTERNAL                        SOFIP195
      IF(PSNL.LT.0.0.OR.PSNL.GT.11.0) GO TO 250                         SOFIP196
      IZ = IZONE(INT(PSNL/.1))                                          SOFIP197
      LCOUNT(IZ) = LCOUNT(IZ) + 1                                       SOFIP198
      GO TO 260                                                         SOFIP199
  250 LCOUNT(4) = LCOUNT(4)+1                                           SOFIP200
  260 CONTINUE                                                          SOFIP201
C *** *********************  EXPOSURE INDEX MODULE  ********************SOFIP202
C *** STORE FLUXES AND TIMES IN INTENSITY RANGES                        SOFIP203
      GO TO(270,280),IPASS                                              SOFIP204
  270 ISWTCH=ISWTCH+1                                                   SOFIP205
  280 INTRNG = (8-INT(1.0-SIGN(0.5,ALGFLX(NRGYLV)-7.0)) *               SOFIP206
     $(7-INT(ALGFLX(NRGYLV)))) * INT(1.0+SIGN(0.5,                      SOFIP207
     $FLUXES(NRGYLV)-1.0009))+1                                         SOFIP208
      EXPFLX(INTRNG)=EXPFLX(INTRNG)+FLUXES(NRGYLV)*60.0*FLOAT(          SOFIP209
     $KPSTEP)                                                           SOFIP210
      EXPFLX(10)=EXPFLX(10)+FLUXES(NRGYLV)*60.0*FLOAT(KPSTEP)           SOFIP211
      EXPTIM(INTRNG) = EXPTIM(INTRNG) + FLOAT(KPSTEP) * .0166667        SOFIP212
      EXPTIM(10) = EXPTIM(10) + FLOAT(KPSTEP) * .0166667                SOFIP213
C *** ***************  PEAK AND TOTALS PER ORBIT MODULE  ***************SOFIP214
C *** DETERMINE ORBIT NUMBER AND TOTAL FLUXES PER ORBIT                 SOFIP215
      IF(PSNTIM.LT.TAU) GO TO 300                                       SOFIP216
      PEAK = -1.0                                                       SOFIP217
      TAUFLX(NORBIT) = FLXSUM-OFLXSM                                    SOFIP218
      OFLXSM = FLXSUM                                                   SOFIP219
      NRBITO=NORBIT                                                     SOFIP220
      NORBIT = NORBIT+1                                                 SOFIP221
      TAU = NORBIT * PERIOD                                             SOFIP222
      IF(NORBIT.LE.50) GO TO 300                                        SOFIP223
      WRITE(6,301)                                                      SOFIP224
  301 FORMAT('0ERROR: NORBIT EXCEEDS LIMIT OF 50.  ************')       SOFIP225
      STOP                                                              SOFIP226
C *** DETERMINE FLUX PEAKS AND POSITIONS PER ORBIT                      SOFIP227
  300 IF(FLUXES(NRGYLV).LE.PEAK) GO TO 310                              SOFIP228
      PKFLX(NORBIT) = FLUXES(NRGYLV)                                    SOFIP229
      PKTIM(NORBIT) = PSNTIM                                            SOFIP230
      PKLON(NORBIT) = PSNLON                                            SOFIP231
      PKLAT(NORBIT) = PSNLAT                                            SOFIP232
      PKALT(NORBIT) = PSNALT                                            SOFIP233
      PKB(NORBIT) = PSNB                                                SOFIP234
      PKL(NORBIT) = PSNL                                                SOFIP235
      PEAK = FLUXES(NRGYLV)                                             SOFIP236
  310 CONTINUE                                                          SOFIP237
C *** *****************  GEOMAGNETIC SHIELDING MODULE  *****************SOFIP238
C *** *******  THIS MODULE MUST BE USED WITH SOLAR PROTON MODULE  ******SOFIP239
      IF(INT(PSNL).GE.5.OR.PSNL.LE.0.0) L=L+1                           SOFIP240
C *** ************  BLOCK 4: LOOPING  (READ-LOOP ENDS HERE)  ***********SOFIP241
      IPASS=2                                                           SOFIP242
      GO TO 50                                                          SOFIP243
C *** ******************  BLOCK 5: OUTPUT PREPARATION  *****************SOFIP244
C *** COMPOSITE ORBIT SPECTRUM                                          SOFIP245
  400 AFCTRS = (KPSTEP*1440.0) / (PSNTIM*86400.0)                       SOFIP246
      DO 410 NRG=1,30                                                   SOFIP247
      AIFLXS(NRG) = AIFLXS(NRG)*AFCTRS                                  SOFIP248
      IF(AIFLXS(NRG).LE.0.0) GO TO 440                                  SOFIP249
      ALNFLX(NRG) = ALOG(AIFLXS(NRG))                                   SOFIP250
  410 CONTINUE                                                          SOFIP251
  440 DO 450 NRG=1,29                                                   SOFIP252
  450 DIFFLX(NRG) = AIFLXS(NRG)-AIFLXS(NRG+1)                           SOFIP253
      DIFFLX(30) = AIFLXS(30)                                           SOFIP254
C *** **********************  PERCENT TIME MODULE  *********************SOFIP255
C *** **  THIS MODULE MUST BE USED WITH ORBIT L-ZONE BREAKDOWN MODULE  *SOFIP256
C *** CALCULATE AND PRINT PERCENT TIME TABLE                            SOFIP257
      LSUM=LCOUNT(1) + LCOUNT(2) + LCOUNT(3) + LCOUNT(4)                SOFIP258
      IF(LSUM.EQ.0) GO TO 470                                           SOFIP259
      DO 460 IL=1,4                                                     SOFIP260
  460 PTIME(IL)=FLOAT(LCOUNT(IL)*KPSTEP)*1.66667/TMLAST                 SOFIP261
      PTIZ=PTIME(1)+PTIME(2)                                            SOFIP262
      WRITE(6,401)PTIZ,(PTIME(II),II=1,4),PSNTIM                        SOFIP263
  470 CONTINUE                                                          SOFIP264
  401 FORMAT('0***** PERCENT OF TOTAL LIFETIME SPENT INSIDE AND OUTSIDE SOFIP265
     $TRAPPED PARTICLE RADIATION BELT ****'//6X,'INNER ZONE (1.0 <= L < SOFIP266
     $2.8)  : ',F6.2,' %'/18X,'OUTSIDE TRAPPING REGION (1.0 <= L < 1.1) SOFIP267
     $: ',F6.2,' %'/18X,'INSIDE TRAPPING REGION  (1.1 <= L < 2.8) : ',  SOFIP268
     $F6.2,' %'/6X,'OUTER ZONE (2.8 <= L <= 11.0) : ',F6.2,' %'/6X,'EXTESOFIP269
     $RNAL   (L > 11.0)         : ',F6.2,' %'//' TOTAL ORBIT TIME IS :',SOFIP270
     $F8.2,' HOURS')                                                    SOFIP271
C *** *****************  DIFFERENTIAL SPECTRUM MODULE  *****************SOFIP272
      CALL DSPCTR(ALNFLX(1),ENERGY(1,ITYPE),DIFSPC(1))                  SOFIP273
C *** **********************  SOLAR PROTON MODULE  *********************SOFIP274
C *** ****  THIS MODULE MUST BE USED WITH GEOMAG. SHIELDING MODULE  ****SOFIP275
      T=12.                                                             SOFIP276
      IT=T                                                              SOFIP277
      IQ=90                                                             SOFIP278
      ISWTCH=ISWTCH+2                                                   SOFIP279
      IF(L.LE.0) GO TO 510                                              SOFIP280
      CALL SOLPRO(T,IQ,F,INALE)                                         SOFIP281
      EXPOTM=FLOAT(L*KPSTEP)*.0166667                                   SOFIP282
      EXPFCT=(EXPOTM/PSNTIM)                                            SOFIP283
      DO 500 J=1,20                                                     SOFIP284
      F(J)=F(J)*EXPFCT                                                  SOFIP285
  500 CONTINUE                                                          SOFIP286
  510 CONTINUE                                                          SOFIP287
C *** **********************  OUTPUT PUNCH MODULE  *********************SOFIP288
C *** PUNCHES ENERGY, INTEG AND DIFF FLUX, SOLAR PROTONS IF PRESENT     SOFIP289
      WRITE(7,605) NAME,INCL,IPRG,IAPG,MODEL,BLTIME                     SOFIP290
      WRITE(7,602)((ENERGY((II-1)*6+JJ,ITYPE),JJ=1,6),XLABEL(1,ITYPE),IISOFIP291
     $,II=1,5)                                                          SOFIP292
      WRITE(7,602)((AIFLXS((II-1)*6+JJ),JJ=1,6),XLABEL(2,ITYPE),II,II=1,SOFIP293
     $5)                                                                SOFIP294
      WRITE(7,602)((DIFSPC((II-1)*6+JJ),JJ=1,6),XLABEL(3,ITYPE),II,II=1,SOFIP295
     $5)                                                                SOFIP296
      IF(L.LE.0) GO TO 600                                              SOFIP297
      WRITE(7,603) IQ,T,INALE,EXPFCT                                    SOFIP298
      WRITE(7,604)((SPNRG((II-1)*5+JJ),JJ=1,5),PROTLB(1),II,II=1,4)     SOFIP299
      WRITE(7,604)((F((II-1)*5+JJ),JJ=1,5),PROTLB(2),II,II=1,4)         SOFIP300
  600 CONTINUE                                                          SOFIP301
  602 FORMAT(1P6E12.4,A6,I2)                                            SOFIP302
  603 FORMAT('SOLAR PROTONS  #ENERGIES=20   Q=',I2,'  TAU=',F4.1,       SOFIP303
     $'  NALE=',I1,'  EXPFCTR=',F5.2)                                   SOFIP304
  604 FORMAT(1P5E12.4,12X,A6,I2)                                        SOFIP305
  605 FORMAT(3A4,1X,I2,'/',I5,'-',I6,1X,'I(#/CM**2-SEC) D(#/CM**2-SEC-KESOFIP306
     $V) MOD/TM=',I1,'/',F6.1)                                          SOFIP307
C *** ********************  OUTPUT TABLES MODULE 1  ********************SOFIP308
      DO 900 NTBL=1,NTABLS                                              SOFIP309
      WRITE(6,2) MODEL,MODLBL,BLTIME,NAME,INCL,IPRG,IAPG,ITAPE,PERIOD,  SOFIP310
     $XAMNIM                                                            SOFIP311
      GO TO (710,700,730,720),ISWTCH                                    SOFIP312
C *** COMPOSITE ORBIT SPECTRUM AND EXPOSURE INDEX                       SOFIP313
  700 WRITE (6,701) (TYPLBL(K,ITYPE),K=1,3),ENERGY(NRGYLV,ITYPE),       SOFIP314
     $(ENERGY(N,ITYPE),AIFLXS(N),DIFFLX(N),DIFSPC(N),FIRNGS(N),         SOFIP315
     $FIRNGS(N+1),EXPTIM(N),EXPFLX(N),N=1,10),(ENERGY(N,ITYPE),AIFLXS(N)SOFIP316
     $,DIFFLX(N),DIFSPC(N),N=11,30)                                     SOFIP317
  701 FORMAT ('+',41X,16('*'),3X,3A4,2X,16('*')/' ',41X,49('*')////'0', SOFIP318
     $T18,15('*'),' COMPOSITE ORBIT SPECTRUM ',15('*'),T80,'** EXPOSURE SOFIP319
     $INDEX:ENERGY>',G9.2,T112,'MEV **'/'0',T18,'ENERGY     AVERAGED    SOFIP320
     $   DIFFERENCE     AVERAGED DIFFE-       INTENSITY    EXPOSURE   TOSOFIP321
     $TAL # OF'/' ',T18,'LEVELS   INTEGRAL FLUX   INTEGRAL FLUX    RENTISOFIP322
     $AL FLUX',10X,'RANGES      DURATION   ACCUMULATED'/' ',T18,'>(MEV) SOFIP323
     $  #/CM**2/SEC     #/CM**2/SEC/DE  #/CM**2/SEC/KEV      #/CM**2/SECSOFIP324
     $   (HOURS)     PARTICLES'/'0',T18,0PG9.4,T23,'    ',1PE9.3,7X,    SOFIP325
     $1PE9.3,8X,1PE9.3,T81,2A4,T81,'ZERO FLUX',1X,0PF10.3,1X,1PE13.3/8('SOFIP326
     $ ',T18,0PG9.4,1PE9.3,7X,1PE9.3,8X,1PE9.3,T81,A4,'-',A4,1X,0PF10.3,SOFIP327
     $1X,1PE13.3/),' ',T18,0PG9.4,1PE9.3,7X,1PE9.3,8X,1PE9.3/' ',T81,2A4SOFIP328
     $,T81,'    TOTAL',1X,0PF10.3,1X,1PE13.3,20(T18,0PG9.4,1PE9.3,7X,   SOFIP329
     $1PE9.3,8X,1PE9.3/' '))                                            SOFIP330
      GO TO 750                                                         SOFIP331
C *** COMPOSITE ORBIT SPECTRUM ONLY                                     SOFIP332
  710 WRITE (6,702) (TYPLBL(K,ITYPE),K=1,3),(ENERGY(N,ITYPE),AIFLXS(N), SOFIP333
     $DIFFLX(N),DIFSPC(N),N=1,30)                                       SOFIP334
  702 FORMAT ('+',41X,16('*'),3X,3A4,2X,16('*')/' ',41X,49('*')////'0', SOFIP335
     $T40,15('*'),' COMPOSITE ORBIT SPECTRUM ',15('*')/'0',T40,'ENERGY  SOFIP336
     $   AVERAGED     DIFFERENCE       AVERAGED DIFFE-'/' ',T40,'LEVELS SOFIP337
     $  INTEGRAL FLUX  INTEGRAL FLUX     RENTIAL FLUX'/' ',T40,'>(MEV)  SOFIP338
     $  #/CM**2/SEC  #/CM**2/SEC/DE    #/CM**2/SEC/KEV'//30(' ',T41,    SOFIP339
     $0PG9.4,T46,'    ',1PE9.3,6X,1PE9.3,8X,1PE9.3/))                   SOFIP340
      GO TO 750                                                         SOFIP341
C *** COMPOSITE ORBIT SPECTRUM WITH SOLAR PROTONS AND EXPOSURE INDEX    SOFIP342
  720 WRITE(6,703)(TYPLBL(K,ITYPE),K=1,3),IT,IQ,INALE,ENERGY(NRGYLV,    SOFIP343
     $ITYPE),EXPFCT,(ENERGY(N,ITYPE),AIFLXS(N),DIFFLX(N),DIFSPC(N),SPNRGSOFIP344
     $(N),F(N),FIRNGS(N),FIRNGS(N+1),EXPTIM(N),EXPFLX(N),N=1,10),(ENERGYSOFIP345
     $(N,ITYPE),AIFLXS(N),DIFFLX(N),DIFSPC(N),SPNRG(N),F(N),N=11,20),   SOFIP346
     $(ENERGY(N,ITYPE),AIFLXS(N),DIFFLX(N),DIFSPC(N),N=21,30)           SOFIP347
  703 FORMAT('+',41X,16('*'),3X,3A4,2X,16('*')/' ',41X,49('*')////'0',  SOFIP348
     $62X,'**** SOLAR PROTONS ****'//63X,'FOR TAU=',I2,',Q=',I2,': NALE=SOFIP349
     $',I1/3X,15('*'),' COMPOSITE ORBIT SPECTRUM ',15('*'),5X,'WITH GEOMSOFIP350
     $AG SHIELDING',8X,'** EXPOSURE INDEX: ENERGY>',G9.4,T125,' MEV **'/SOFIP351
     $64X,'(EXPOSR FACTOR=',F4.2,')'//3X,'ENERGY     AVERAGED       DIFFSOFIP352
     $ERENCE     AVERAGED DIFF-       ENERGY       TOTAL           INTENSOFIP353
     $SITY    EXPOSURE   TOTAL # OF'/3X,'LEVELS   INTEGRAL FLUX  INTEGRASOFIP354
     $L FLUX    RENTIAL FLUX         LEVELS      FLUENCE           RANGESOFIP355
     $S      DURATION   ACCUMULATED'/3X,'>(MEV)    #/CM**2/SEC  #/CM**2/SOFIP356
     $SEC/DE   #/CM**2/SEC/KEV       >(MEV)      #/CM**2         #/CM**2SOFIP357
     $/SEC   (HOURS)     PARTICLES'//T4,0PG9.4,T9,'      ',1PE9.3,6X,   SOFIP358
     $1PE9.3,7X,1PE9.3,11X,0PF4.0,7X,1PE9.3,T95,2A4,T95,'ZERO FLUX',    SOFIP359
     $0PF11.3,1PE14.3/8(T4,0PG9.4,T9,'      ',1PE9.3,6X,1PE9.3,7X,1PE9.3SOFIP360
     $,11X,0PF4.0,7X,1PE9.3,9X,A4,'-',A4,0PF11.3,1PE14.3/),T4,0PG9.4,T9,SOFIP361
     $'      ',1PE9.3,6X,1PE9.3,7X,1PE9.3,11X,0PF4.0,7X,1PE9.3,T95,2A4, SOFIP362
     $T95,'    TOTAL',0PF11.3,1PE14.3/10(T4,0PG9.4,T9,'      ',1PE9.3,6XSOFIP363
     $,1PE9.3,7X,1PE9.3,11X,0PF4.0,7X,1PE9.3/),10(T4,0PG9.4,T9,'      ',SOFIP364
     $1PE9.3,6X,1PE9.3,7X,1PE9.3/))                                     SOFIP365
      GO TO 750                                                         SOFIP366
C *** COMPOSITE ORBIT SPECTRUM WITH SOLAR PROTONS                       SOFIP367
  730 WRITE(6,704)(TYPLBL(K,ITYPE),K=1,3),IT,IQ,INALE,EXPFCT,(ENERGY(N, SOFIP368
     $ITYPE),AIFLXS(N),DIFFLX(N),DIFSPC(N),SPNRG(N),F(N),N=1,20),       SOFIP369
     $(ENERGY(N,ITYPE),AIFLXS(N),DIFFLX(N),DIFSPC(N),N=21,30)           SOFIP370
  704 FORMAT('+',41X,16('*'),3X,3A4,2X,16('*')/' ',41X,49('*')////T93,  SOFIP371
     $'**** SOLAR PROTONS ****'//T93,'FOR TAU=',I2,',Q=',I2,': NALE=',  SOFIP372
     $I1/19X,15('*'),' COMPOSITE ORBIT SPECTRUM ',15('*'),18X,'WITH GEOMSOFIP373
     $AG SHIELDING'/T94,'(EXPOSR FACTOR=',F4.2,')'//19X,'ENERGY     AVERSOFIP374
     $AGED       DIFFERENCE     AVERAGED DIFF-',20X,'ENERGY       TOTAL'SOFIP375
     $/19X,'LEVELS   INTEGRAL FLUX  INTEGRAL FLUX    RENTIAL FLUX',22X, SOFIP376
     $'LEVELS      FLUENCE'/19X,'>(MEV)    #/CM**2/SEC  #/CM**2/SEC/DE  SOFIP377
     $ #/CM**2/SEC/KEV',20X,'>(MEV)      #/CM**2'//20(T20,0PG9.4,T25,'  SOFIP378
     $    ',1PE9.3,6X,1PE9.3,7X,1PE9.3,24X,0PF4.0,7X,1PE9.3/),10(T20,   SOFIP379
     $0PG9.4,T25,'      ',1PE9.3,6X,1PE9.3,7X,1PE9.3/))                 SOFIP380
  750 CONTINUE                                                          SOFIP381
C *** ********************  OUTPUT TABLES MODULE 2  ********************SOFIP382
C *** PEAK AND TOTAL FLUXES PER PERIOD                                  SOFIP383
      WRITE(6,2) MODEL,MODLBL,BLTIME,NAME,INCL,IPRG,IAPG,ITAPE,PERIOD,  SOFIP384
     $XAMNIM                                                            SOFIP385
      WRITE(6,801)(TYPLBL(K,ITYPE),K=1,3),ENERGY(NRGYLV,ITYPE),         SOFIP386
     $(N,(PKVALU(N,K),K=1,8),N=1,NRBITO)                                SOFIP387
  801 FORMAT(                '+',T35,24('*'),3X,3A4,2X,27('*')/' ',T35, SOFIP388
     $'** TABLE OF PEAK AND TOTAL FLUXES PER PERIOD : ENERGY >',G9.2,T97SOFIP389
     $,   'MEV **'/' ',T35,68('*')//'0',13X,'PERIOD    PEAK FLUX      POSOFIP390
     $SITION AT WHICH ENCOUNTERED    ORBIT TIME    FIELD(B)    LINE(L)  SOFIP391
     $  TOTAL FLUX'/' ',13X,'NUMBER    ENCOUNTERED    LONGITUDE  LATITUDSOFIP392
     $E  ALTITUDE',41X,'PER ORBIT'/  ' ',23X,'#/CM**2/SEC ',2(5X,'(DEG)'SOFIP393
     $),6X,'(KM)',7X,'(HOURS)',6X,'(GAUSS)     (E.R.)     #/CM**2/ORBIT'SOFIP394
     $//(' ',14X,I4,1PE14.3,0PF13.3,F10.2,F12.2,F13.5,F12.5,F10.2,1PE15.SOFIP395
     $3))                                                               SOFIP396
C *** *****************  BLOCK 6: PROGRAM TERMINATION  *****************SOFIP397
  900 CONTINUE                                                          SOFIP398
      GO TO 10                                                          SOFIP399
  999 STOP                                                              SOFIP400
      END                                                               SOFIP401
C *** ***************  DIFFERENTIAL SPECTRUM SUBROUTINE  ***************DFSPC002
C *** CALCULATES FIRST DERIVATIVES OF INPUT SPECTRUM DEFINED BY FF VS XXDFSPC004
C *** INPUT:   XX - 30 INTEGRAL THRESHOLD ENERGIES, IN MEV         (R*4)DFSPC006
C ***          FF - ALOG OF THE INTEGRAL FLUXES FOR THE 30 ENERGY  (R*4)DFSPC008
C ***               LEVELS, IN PARTICLES/CM**2/SEC                      DFSPC010
C *** OUTPUT:  DD - DIFFERENTIAL FLUXES OBTAINED FROM THE INTEGRAL (R*4)DFSPC012
C ***               FLUXES, IN PARTICLES/CM**2/SEC/KEV                  DFSPC014
C *** ******************************************************************DFSPC016
C *** THIS IS A MODIFIED VERSION OF A PROGRAM (DCS1FU) OBTAINED FROM    DFSPC018
C *** IMSL LIBRARY 1:  AUTHOR/IMPLEMENTOR - C.L.SMITH                   DFSPC020
C *** ******************************************************************DFSPC022
      SUBROUTINE DSPCTR(FF,XX,DD)                                       DFSPC024
      IMPLICIT REAL*8(A-H,O-Z)                                          DFSPC026
      REAL*4 DD,FF,XX                                                   DFSPC028
      DIMENSION F(30),X(30),D(30),H(500),FF(30) ,XX(30) ,DD(30)         DFSPC030
      DATA EPSLN,OMEGA/1.D-6,1.0717968D0/                               DFSPC032
C *** DATA INITIALIZATION                                               DFSPC034
      M=0                                                               DFSPC036
      DO 5 L=1,30                                                       DFSPC038
    5 DD(L)=0.0                                                         DFSPC040
C *** DETERMINE SIZE OF ARRAY: OBTAIN M & K INDICES                     DFSPC042
C ***   M = # OF NONZERO FLUXES - 1;  K = # OF NONZERO FLUXES           DFSPC044
      DO 10 K=1,30                                                      DFSPC046
      IF(FF(K).EQ.0.)      GO TO 15                                     DFSPC048
      M=K-1                                                             DFSPC050
      F(K)=FF(K)+ALOG(1000.)                                            DFSPC052
      X(K)=XX(K)*1000.D0                                                DFSPC054
   10 D(K)=X(K)                                                         DFSPC056
   15 K=M+1                                                             DFSPC058
      IF(K.LT.10) GO TO 170                                             DFSPC060
C *** SMOOTHING INTEGRAL FLUX                                           DFSPC062
      CALL SMOOTH(X,F,M)                                                DFSPC064
C *** CALCULATE SECOND DERIVATIVES USING CENTRAL DIFFERENCES            DFSPC066
      DO 30 I=1,M                                                       DFSPC068
         H(I)=X(I+1)-X(I)                                               DFSPC070
   30    H(K +I)=(F(I+1)-F(I))/H(I)                                     DFSPC072
      DO 40  I=2,M                                                      DFSPC074
         H(2*K+I)=H(I-1)+H(I)                                           DFSPC076
         H(3*K+I)=.5*H(I-1)/H (2*K+I)                                   DFSPC078
         H(4*K+I)=(H(K+I)-H(K+I-1))/H(2*K+I)                            DFSPC080
         H(5*K+I)=H(4*K+I)+H(4*K+I)                                     DFSPC082
   40    H(6*K+I)=H(5*K+I)+H(4*K+I)                                     DFSPC084
      H(5*K+1)=0.                                                       DFSPC086
      H(6*K)=0.                                                         DFSPC088
C *** BEGIN ITERATION ON SECOND DERIVATIVES                             DFSPC090
      KCOUNT=0                                                          DFSPC092
   50 ETA=0.                                                            DFSPC094
      KCOUNT=KCOUNT+1                                                   DFSPC096
      DO 70  I=2,M                                                      DFSPC098
         W=(H(6*K+I)-H(3*K+I)*H(5*K+I-1)-(.5-H(3*K+I))*H(5*K+I+1)-H(5*K+DFSPC100
     $   I)*OMEGA)                                                      DFSPC102
         IF (DABS(W).LE.ETA) GO TO 60                                   DFSPC104
         ETA=DABS(W)                                                    DFSPC106
   60    H(5*K+I)=H(5*K+I)+W                                            DFSPC108
   70 CONTINUE                                                          DFSPC110
      IF(KCOUNT.GT.5*K)GO TO 170                                        DFSPC112
      IF (ETA.GE.EPSLN) GO TO 50                                        DFSPC114
C *** CONVERGENCE OBTAINED                                              DFSPC116
      DO 80 I=1,M                                                       DFSPC118
   80    H(7*K+I)=(H(5*K+1+I)-H(5*K+I))/H(I)                            DFSPC120
      DO 140 J=1,K                                                      DFSPC122
         I=1                                                            DFSPC124
         IF (D(J).EQ.X(1))GO TO 130                                     DFSPC126
         IF (D(J)-X(K )) 100,110,110                                    DFSPC128
   90    IF (D(J)-X(I)) 120,130,100                                     DFSPC130
  100    I=I+1                                                          DFSPC132
         GO TO 90                                                       DFSPC134
  110    I=K                                                            DFSPC136
  120    I=I-1                                                          DFSPC138
C *** COMPUTE D(J)                                                      DFSPC140
  130    HT1=D(J)-X(I)                                                  DFSPC142
         HT2=D(J)-X(I+1)                                                DFSPC144
         PROD=HT1*HT2                                                   DFSPC146
         H(8*K+J)=H(5*K+I)+HT1*H(7*K+I)                                 DFSPC148
         DELSQS=(H(5*K+I)+H(5*K+1+I)+H(8*K+J))/6.                       DFSPC150
  140    D(J)=-(H(K +I)+(HT1+HT2)*DELSQS+PROD*H(7*K+I)*.1666667)        DFSPC152
C *** SMOOTHING DIFFERENTIAL FLUX                                       DFSPC154
      CALL SMOOTH(X,D,M)                                                DFSPC156
      DO 160 I=1,K                                                      DFSPC158
      F(I)=2.718281828D0**(F(I)-ALOG(1000.))                            DFSPC160
  160 DD(I)     =D(I)*F(I)                                              DFSPC162
  170 RETURN                                                            DFSPC164
      END                                                               DFSPC166
C                                                                       DFSPC168
C *** SMOOTH DATA BY 3-POINT AVERAGING OVER EQUAL INTERVALS             DFSPC170
      SUBROUTINE SMOOTH(X,F,M)                                          DFSPC172
      IMPLICIT REAL*8(A-H,O-Z)                                          DFSPC174
      DIMENSION X(30),F(30)                                             DFSPC176
      FINTER(X1,X2,X3,Y1,Y2,Y3,XIN)=Y1*(XIN-X2)*(XIN-X3)/               DFSPC178
     $((X1-X2)*(X1-X3)) + Y2*(XIN-X1)*(XIN-X3)/((X2-X1)*(X2-X3))        DFSPC180
     $                  + Y3*(XIN-X1)*(XIN-X2)/((X3-X1)*(X3-X2))        DFSPC182
C                                                                       DFSPC184
      FI = F(1)                                                         DFSPC186
      DO 20 I=2,M                                                       DFSPC188
        SIZE1 = X(I) - X(I-1)                                           DFSPC190
        SIZE2 = X(I+1) - X(I)                                           DFSPC192
C *** CHECK FOR EQUAL STEPSIZES                                         DFSPC194
        IF(DABS(SIZE1-SIZE2).LT.0.001) GO TO 200                        DFSPC196
        IF(SIZE2.GT.SIZE1) GO TO 210                                    DFSPC198
C *** STEPSIZE DECREASES - FIT CURVE AND INTERPOLATE BACKWARD           DFSPC200
        F2 = F(I+1)                                                     DFSPC202
        XINTER = X(I) - SIZE2                                           DFSPC204
        F1 = FINTER(X(I-1),X(I),X(I+1),FI,F(I),F2,XINTER)               DFSPC206
        GO TO 300                                                       DFSPC208
C *** STEPSIZE INCREASES - FIT CURVE AND INTERPOLATE FORWARD            DFSPC210
  210   F1 = FI                                                         DFSPC212
        XINTER = X(I) + SIZE1                                           DFSPC214
        F2 = FINTER(X(I-1),X(I),X(I+1),F1,F(I),F(I+1),XINTER)           DFSPC216
        GO TO 300                                                       DFSPC218
C *** STEPSIZES ARE EQUAL - AVERAGE OVER EXISTING VALUES                DFSPC220
  200   F1 = FI                                                         DFSPC222
        F2 = F(I+1)                                                     DFSPC224
C                                                                       DFSPC226
C *** PERFORM AVERAGING                                                 DFSPC228
  300   FNEW = (F1+2.0*F(I)+F2)/4.                                      DFSPC230
        FI = F(I)                                                       DFSPC232
        F(I) = FNEW                                                     DFSPC234
   20 CONTINUE                                                          DFSPC236
      RETURN                                                            DFSPC238
      END                                                               DFSPC240
      SUBROUTINE SOLPRO(TAU,IQ,F,INALE)                                 SOLPR010
C *** MODIFIED 9/77 TO RETURN INALE(# OF AL EVENTS) TO CALLING PROGRAM  SOLPR020
C *** INTERPLANETARY SOLAR PROTON FLUX AT 1 AU (FROM E>10 TO E>200 MEV  SOLPR030
C *** FOR ANOMALOUSLY LARGE (AL) EVENTS AND FROM E>10 TO E>100 MEV FOR  SOLPR040
C *** ORDINARY (OR) EVENTS)                                             SOLPR050
C *** SINGLE PRECISION DECK IN STANDARD FORTRAN IV FOR IBM 360 MACHINES SOLPR060
C *** (EBCDIC, 029 PUNCH) OR OTHER COMPATIBLE SYSTEMS.                  SOLPR070
C *** PROGRAM DESIGNED AND TESTED BY E.G. STASSINOPOULOS, CODE 601,     SOLPR080
C *** NASA GODDARD SPACE FLIGHT CENTER, GREENBELT, MARYLAND 20771 .     SOLPR090
C ********************************************************************* SOLPR100
C ****  INPUT: TAU     MISSION DURATION IN MONTHS (REAL*4)              SOLPR110
C ****         IQ      CONFIDENCE LEVEL THAT CALCULATED FLUENCE F(N)    SOLPR120
C ****                 WILL NOT BE EXCEEDED (INTEGER*4)                 SOLPR130
C **** OUTPUT: F(N)    SPECTRUM OF INTEGRAL SOLAR PROTON FLUENCE FOR    SOLPR140
C ****                 ENERGIES E>10*N (1=<N=<20) FOR AL EVENTS         SOLPR150
C ****                 ENERGIES E>10*N (1=<N=<10) FOR OR EVENTS         SOLPR160
C ****         INALE   # OF AL EVENTS FOR GIVEN TAU AND Q               SOLPR170
      REAL NALE,NALECF(7,20)/-.1571,.2707,-.1269E-1,.4428E-3,-.8185E-5, SOLPR180
     $.7754E-7,-.2939E-9,-.1870,.1951,-.6559E-2,.1990E-3,-.3618E-5,     SOLPR190
     $.3740E-7,-.1599E-9,-.2007,.1497,-.3179E-2,.5730E-4,-.4664E-6,     SOLPR200
     $.1764E-8,0.,-.1882,.1228,-.1936E-2,.2660E-4,-.1022E-6,2*0.,       SOLPR210
     $-.2214,.1149,-.1871E-2,.2695E-4,-.1116E-6,2*0.,-.2470,.1062,      SOLPR220
     $-.1658E-2,.2367E-4,-.9465E-7,2*0.,-.2509,.8710E-1,-.8300E-3,      SOLPR230
     $.8438E-5,3*0.,-.2923,.8932E-1,-.1023E-2,.1029E-4,3*0.,-.3222,     SOLPR240
     $.8648E-1,-.9992E-3,.9935E-5,3*0.,-.3518,.8417E-1,-.1000E-2,       SOLPR250
     $.9956E-5,3*0.,-.3698,.7951E-1,-.8983E-3,.8940E-5,3*0.,-.2771,     SOLPR260
     $.5473E-1,-.1543E-4,4*0., -.2818,.5072E-1,.2511E-4,4*0.,-.2845,    SOLPR270
     $.4717E-1,.5664E-4,4*0.,-.2947,.4405E-1,.8507E-4,4*0.,-.2923,      SOLPR280
     $.4111E-1,.1106E-3,4*0.,-.2981,.3853E-1,.1312E-3,4*0.,-.3002,      SOLPR290
     $.3585E-1,.1529E-3,4*0.,-.3001,.3312E-1,.1781E-3,4*0.,-.3141,      SOLPR300
     $.3248E-1,.1654E-3,4*0./,F(20),G(20)                               SOLPR310
      REAL ORFLXC(5,9)/.154047E3,-.522258E4,.714275E5,-.432747E6,.955315SOLPR320
     $E6,.198004E3,-.448788E4,.438148E5,-.196046E6,.32552E6,.529120E3,  SOLPR330
     $-.122227E5,.112869E6,-.465084E6,.710572E6,.121141E4,-.266412E5,   SOLPR340
     $.226778E6,-.85728E6,.120444E7,.452062E4,-.103248E6,.896085E6,     SOLPR350
     $-.346028E7,.499852E7,.272028E4,-.499088E5,.35305E6,-.111929E7,    SOLPR360
     $.133386E7,.275597E4,-.469718E5,.314729E6,-.960383E6,.11165E7,     SOLPR370
     $.570997E4,-.799689E5,.381074E6,-.610714E6,0.,.101E3,4*0./         SOLPR380
      INTEGER INDEX(20)/2*7,6,3*5,5*4,9*3/                              SOLPR390
    1 FORMAT(' TAU=',F4.0,' IQ=',I3,3X,'PARAMETER(S) EXCEED PROGRAM LIMISOLPR400
     $TS')                                                              SOLPR410
    2 FORMAT(2X,'FOR THE COMBINATION OF TAU AND IQ GIVEN, NO SIGNIFICANTSOLPR420
     $ SOLAR PROTON FLUXES ARE TO BE EXPECTED. TAU=',F6.2,' IQ=',I2)    SOLPR430
      IF(TAU.GT.72..OR.IQ.LT.80)GO TO 500                               SOLPR440
      IP=100-IQ                                                         SOLPR450
      M=INDEX(IP)                                                       SOLPR460
      NALE=0.                                                           SOLPR470
      DO 300 J=1,M                                                      SOLPR480
  300 NALE=NALE+NALECF(J,IP)*TAU**(J-1)                                 SOLPR490
      INALE=NALE+1.0001                                                 SOLPR500
      IF(INALE.GT.0) GO TO 400                                          SOLPR510
C *** CALCULATIONS FOR OR-EVENT CONDITIONS                              SOLPR520
      IT=TAU                                                            SOLPR530
      IF(IT.EQ.1.AND.IP.GT.16) GO TO 700                                SOLPR540
      P=FLOAT(IP)/100.                                                  SOLPR550
      OF=0.                                                             SOLPR560
      DO 100 J=1,5                                                      SOLPR570
  100 OF=OF+ORFLXC(J,IT)* P**(J-1)*1.E7                                 SOLPR580
      E=10.                                                             SOLPR590
      DO 200 N=1,10                                                     SOLPR600
      G(N)=EXP(.0158*(30.-E))                                           SOLPR610
      F(N)=OF*G(N)                                                      SOLPR620
  200 E=E+10.                                                           SOLPR630
      GO TO 800                                                         SOLPR640
C *** CALCULATIONS FOR AL-EVENT CONDITIONS                              SOLPR650
  400 E=10.                                                             SOLPR660
      DO 600 N=1,20                                                     SOLPR670
      F(N)=7.9E9*EXP((30.-E)/26.5)*INALE                                SOLPR680
  600 E=E+10.                                                           SOLPR690
      GO TO 800                                                         SOLPR700
  700 WRITE(6,2) TAU,IQ                                                 SOLPR710
      GO TO 800                                                         SOLPR720
  500 WRITE (6,1) TAU,IQ                                                SOLPR730
  800 RETURN                                                            SOLPR740
      END                                                               SOLPR750
      SUBROUTINE TRARA1(DESCR,MAP,FL,BABS,E,F,N)                        TRAR1002
C***********************************************************************TRAR1004
C     B / B0 CASE     JOEL STEIN     9-15-71     X2133     KMS          TRAR1006
C   TRARA1 DOES ENERGY VALUE SEARCH FOR FLUX CALCULATION WHEN GIVEN A   TRAR1008
C    B AND L POINT.                                                     TRAR1010
C***********************************************************************TRAR1012
C  MAP(1) IS THE FIRST WORD OF LIST                                     TRAR1014
      LOGICAL S0,S1,S2                                                  TRAR1016
C   S0,S1,S2 ARE LOGICAL VARIABLES WHICH INDICATE WHETHER THE FLUX FOR ATRAR1018
C     PARTICULAR E,B,L POINT HAS ALREADY BEEN FOUND IN A PREVIOUS CALL  TRAR1020
C     TO TRARA2.                                                        TRAR1022
      DIMENSION E(1),F(1),DESCR(8),MAP(1)                               TRAR1024
      NL=AMIN1(32766.,ABS(FL*DESCR(5)))                                 TRAR1026
      NB =ABS(((BABS*(FL*FL*FL)/0.311653)-1) * DESCR(6))                TRAR1028
C     NB=ABS((BABS-0.311653E0/(FL*FL*FL))*DESCR(6))                     TRAR1030
C   NL IS THE MINIMUM OF THE L VALUE OR 15.999, SCALED TO AN INTEGER BY TRAR1032
C     THE L SCALING FACTOR                                              TRAR1034
C   NB IS THE DIFFERENCE BETWEEN THE INPUT B VALUE AND B EQUATORIAL,    TRAR1036
C     SCALED TO AN INTEGER BY THE B SCALING FACTOR.                     TRAR1038
      I1=0                                                              TRAR1040
      I2=MAP(1)                                                         TRAR1042
      I3=I2+MAP(I2+1)                                                   TRAR1044
      L3=MAP(I3+1)                                                      TRAR1046
      E1=MAP(I1+2)/DESCR(4)                                             TRAR1048
      E2=MAP(I2+2)/DESCR(4)                                             TRAR1050
      S1=.TRUE.                                                         TRAR1052
      S2=.TRUE.                                                         TRAR1054
C                                                                       TRAR1056
C   I2 IS THE NUMBER OF ELEMENTS IN THE FLUX MAP FOR THE FIRST ENERGY.  TRAR1058
C   I3 IS THE INDEX OF THE LAST ELEMENT OF THE SECOND ENERGY MAP.       TRAR1060
C   L3 IS THE LENGTH OF THE MAP FOR THE THIRD ENERGY.                   TRAR1062
C   E1 IS THE ENERGY OF THE FIRST ENERGY MAP (UNSCALED)                 TRAR1064
C   E2 IS THE ENERGY OF THE SECOND ENERGY MAP (UNSCALED)                TRAR1066
C   S1 AND S2 ARE TRUE TO INDICATE THAT NO FLUXES HAVE YET BEEN FOUND.  TRAR1068
C                                                                       TRAR1070
      DO 3 IE=1,N                                                       TRAR1072
C   THE DO STATEMENT LOOPS THROUGH THE ENERGIES FOR WHICH FLUXES ARE    TRAR1074
C     DESIRED AT THE GIVEN B,L POINT (BABS,FL).                         TRAR1076
1     IF(E(IE).LE.E2.OR.L3.EQ.0)GOTO2                                   TRAR1078
C                                                                       TRAR1080
C   THE IF STATEMENT CHECKS TO SEE IF THE INPUT ENERGY IS LESS THAN OR ETRAR1082
C     THE ENERGY OF THE SECOND MAP, OR IF THE LENGTH OF THE THIRD MAP ISTRAR1084
C     (I.E. THERE ARE NO HIGHER ENERGIES IN THE TABLE).  IF TRUE, USE THTRAR1086
C     FOR THOSE TWO ENERGY MAPS TO FIND THE DESIRED FLUX AT THE DESIRED TRAR1088
C     ENERGY.  IF FALSE, THE ZEROTH ENERGY MAP IS DEFINED TO BE TNE FIRSTRAR1090
C     ENERGY MAP, THE FIRST BECOMES THE SECOND, AND THE SECOND BECOMES  TRAR1092
C     THE THIRD.  E0,E1,E2 ARE THE ENERGIES FOR THE ZEROTH,FIRST,AND SECTRAR1094
C     ENERGY MAPS.  F0,F1,F2 ARE THE FLUXES FOR THE ZEROTH, FIRST, AND  TRAR1096
C     SECOND ENERGY MAPS AT THE B,L POINT.                              TRAR1098
C                                                                       TRAR1100
      I0=I1                                                             TRAR1102
      I1=I2                                                             TRAR1104
      I2=I3                                                             TRAR1106
      I3=I3+L3                                                          TRAR1108
      L3=MAP(I3+1)                                                      TRAR1110
      E0=E1                                                             TRAR1112
      E1=E2                                                             TRAR1114
      E2=MAP(I2+2)/DESCR(4)                                             TRAR1116
      S0=S1                                                             TRAR1118
      S1=S2                                                             TRAR1120
      S2=.TRUE.                                                         TRAR1122
      F0=F1                                                             TRAR1124
      F1=F2                                                             TRAR1126
      GOTO1                                                             TRAR1128
2     IF(S1)F1=TRARA2(MAP(I1+3),NL,NB)/DESCR(7)                         TRAR1130
      IF(S2)F2=TRARA2(MAP(I2+3),NL,NB)/DESCR(7)                         TRAR1132
C   THESE TWO LOGICAL IFS CALL TRARA2 FOR THE FLUX FROM THE FIRST AND   TRAR1134
C     SECOND ENERGY MAPS AT THE B,L POINT IF THEY HAVE NOT ALREADY BEEN TRAR1136
      S1=.FALSE.                                                        TRAR1138
      S2=.FALSE.                                                        TRAR1140
C   S1 AND S2 ARE FALSE SINCE F1 AND F2 ARE NOW FOUND.                  TRAR1142
      F(IE)=F1+(F2-F1)*(E(IE)-E1)/(E2-E1)                               TRAR1144
C                                                                       TRAR1146
C   INTERPOLATE FOR THE FLUX F(IE) USING THE FLUXES AND ENERGIES FOR MAPTRAR1148
C     ONE AND TWO.                                                      TRAR1150
C   THE FOLLOWING COMMENTS APPLY TO THE REMAINING PROGRAM STATEMENTS.   TRAR1152
C   IF THE FLUX F2 FOR THE SECOND ENERGY MAP IS GREATER THAN ZERO, OR THTRAR1154
C     ZEROTH ENERGY MAP HAS NOT BEEN DEFINED, THE FINAL FLUX IS THE MAXITRAR1156
C     OF THE INTEROOLATED FLUX OR ZERO.  IF THE FLUX FOR THE SECOND ENERTRAR1158
C     MAP IS EQUAL TO ZERO, AND THE ZEROTH ENERGY MAP HAS BEEN DEFINED, TRAR1160
C     THEN INTERPOLATE FOR THE FLUX USING THE ZEROTH AND FIRST ENERGY MATRAR1162
C     CHOOSE THE MINIMUM OF THE TWO INTERPOLATIONS, AND THEN THE MAXIMUMTRAR1164
C     CHOICE AND ZERO FOR THE FINAL FLUX VALUE.                         TRAR1166
C                                                                       TRAR1168
      IF(F2.GT.0.)GOTO3                                                 TRAR1170
      IF(I1.EQ.0)GOTO3                                                  TRAR1172
      IF(S0)F0=TRARA2(MAP(I0+3),NL,NB)/DESCR(7)                         TRAR1174
      S0=.FALSE.                                                        TRAR1176
      F(IE)=AMIN1(F(IE),F0+(F1-F0)*(E(IE)-E0)/(E1-E0))                  TRAR1178
3     F(IE)=AMAX1(F(IE),0.)                                             TRAR1180
      RETURN                                                            TRAR1182
      END                                                               TRAR1184
      FUNCTION TRARA2(MAP,IL,IB)                                        TRAR2002
      DIMENSION MAP(777)                                                TRAR2004
      DATA FISTEP/256./                                                 TRAR2006
      FNL=IL                                                            TRAR2008
      FNB=IB                                                            TRAR2010
      ITIME=0                                                           TRAR2012
      I2=0                                                              TRAR2014
    1 L2=MAP(I2+1)                                                      TRAR2016
      IF(MAP(I2+2).GT.IL)GO TO 2                                        TRAR2018
      I1=I2                                                             TRAR2020
      L1=L2                                                             TRAR2022
      I2=I2+L2                                                          TRAR2024
      GO TO 1                                                           TRAR2026
    2 CONTINUE                                                          TRAR2028
      IF(L1.LT.4.AND.L2.LT.4)GO TO 50                                   TRAR2030
      IF(MAP(I2+3).GT.MAP(I1+3))GO TO 10                                TRAR2032
    5 KT=I1                                                             TRAR2034
      I1=I2                                                             TRAR2036
      I2=KT                                                             TRAR2038
      KT=L1                                                             TRAR2040
      L1=L2                                                             TRAR2042
      L2=KT                                                             TRAR2044
   10 FLOG1=MAP(I1+3)                                                   TRAR2046
      FLL1=MAP(I1+2)                                                    TRAR2048
      FLOG2=MAP(I2+3)                                                   TRAR2050
      FLL2=MAP(I2+2)                                                    TRAR2052
      DFL=(FNL-FLL1)/(FLL2-FLL1)                                        TRAR2054
      FKB1=0.                                                           TRAR2056
      FKB2=0.                                                           TRAR2058
      IF(L1.LT.4)GO TO 32                                               TRAR2060
      DO 17 J2=4,L2                                                     TRAR2062
      FINCR2=MAP(I2+J2)                                                 TRAR2064
      IF(FKB2+FINCR2.GT.FNB)GO TO 23                                    TRAR2066
      FKB2=FKB2+FINCR2                                                  TRAR2068
   17 FLOG2=FLOG2-FISTEP                                                TRAR2070
      ITIME=ITIME+1                                                     TRAR2072
      IF(ITIME.EQ.1)GO TO 5                                             TRAR2074
      GO TO 50                                                          TRAR2076
   23 IF(ITIME.EQ.1)GO TO 30                                            TRAR2078
      IF(J2.EQ.4)GO TO 28                                               TRAR2080
      SL2=FLOG2/FKB2                                                    TRAR2082
      DO 27 J1=4,L1                                                     TRAR2084
      FINCR1=MAP(I1+J1)                                                 TRAR2086
      FKB1=FKB1+FINCR1                                                  TRAR2088
      FLOG1=FLOG1-FISTEP                                                TRAR2090
      FKBJ1=((FLOG1/FISTEP)*FINCR1+FKB1)/((FINCR1/FISTEP)*SL2+1.)       TRAR2092
      IF(FKBJ1.LE.FKB1)GO TO 31                                         TRAR2094
   27 CONTINUE                                                          TRAR2096
      GO TO 55                                                          TRAR2098
   31 IF(FKBJ1.LE.FKB2)GO TO 29                                         TRAR2100
      FKB1=0.                                                           TRAR2102
   30 FKB2=0.                                                           TRAR2104
   32 J2=4                                                              TRAR2106
      FINCR2=MAP(I2+J2)                                                 TRAR2108
      FLOG2=MAP(I2+3)                                                   TRAR2110
      FLOG1=MAP(I1+3)                                                   TRAR2112
   28 FLOGM=FLOG1+(FLOG2-FLOG1)*DFL                                     TRAR2114
      FKBM=0.                                                           TRAR2116
      FKB2=FKB2+FINCR2                                                  TRAR2118
      FLOG2=FLOG2-FISTEP                                                TRAR2120
      SL2=FLOG2/FKB2                                                    TRAR2122
      IF(L1.LT.4)GO TO 35                                               TRAR2124
      J1=4                                                              TRAR2126
      FINCR1=MAP(I1+J1)                                                 TRAR2128
      FKB1=FKB1+FINCR1                                                  TRAR2130
      FLOG1=FLOG1-FISTEP                                                TRAR2132
      SL1=FLOG1/FKB1                                                    TRAR2134
      GO TO 15                                                          TRAR2136
   29 FKBM=FKBJ1+(FKB2-FKBJ1)*DFL                                       TRAR2138
      FLOGM=FKBM*SL2                                                    TRAR2140
      FLOG2=FLOG2-FISTEP                                                TRAR2142
      FKB2=FKB2+FINCR2                                                  TRAR2144
      SL1=FLOG1/FKB1                                                    TRAR2146
      SL2=FLOG2/FKB2                                                    TRAR2148
   15 IF(SL1.LT.SL2)GO TO 20                                            TRAR2150
      FKBJ2=((FLOG2/FISTEP)*FINCR2+FKB2)/((FINCR2/FISTEP)*SL1+1.)       TRAR2152
      FKB=FKB1+(FKBJ2-FKB1)*DFL                                         TRAR2154
      FLOG=FKB*SL1                                                      TRAR2156
      IF(FKB.GE.FNB)GO TO 60                                            TRAR2158
      FKBM=FKB                                                          TRAR2160
      FLOGM=FLOG                                                        TRAR2162
      IF (J1.GE.L1) GO TO 55                                            TRAR2164
      J1=J1+1                                                           TRAR2166
      FINCR1=MAP(I1+J1)                                                 TRAR2168
      FLOG1=FLOG1-FISTEP                                                TRAR2170
      FKB1=FKB1+FINCR1                                                  TRAR2172
      SL1=FLOG1/FKB1                                                    TRAR2174
      GO TO 15                                                          TRAR2176
   20 FKBJ1=((FLOG1/FISTEP)*FINCR1+FKB1)/((FINCR1/FISTEP)*SL2+1.)       TRAR2178
      FKB=FKBJ1+(FKB2-FKBJ1)*DFL                                        TRAR2180
      FLOG=FKB*SL2                                                      TRAR2182
      IF(FKB.GE.FNB)GO TO 60                                            TRAR2184
      FKBM=FKB                                                          TRAR2186
      FLOGM=FLOG                                                        TRAR2188
      IF(J2.GE.L2) GO TO 55                                             TRAR2190
      J2=J2+1                                                           TRAR2192
      FINCR2=MAP(I2+J2)                                                 TRAR2194
      FLOG2=FLOG2-FISTEP                                                TRAR2196
      FKB2=FKB2+FINCR2                                                  TRAR2198
      SL2=FLOG2/FKB2                                                    TRAR2200
      GO TO 15                                                          TRAR2202
   50 TRARA2=0.                                                         TRAR2204
      RETURN                                                            TRAR2206
   35 FINCR1=0.                                                         TRAR2208
      SL1=-900000.                                                      TRAR2210
      GO TO 20                                                          TRAR2212
   60 IF(FKB.LT.FKBM+1.E-20)GO TO 50                                    TRAR2214
      TRARA2=FLOGM+(FLOG-FLOGM)*((FNB-FKBM)/(FKB-FKBM))                 TRAR2216
      TRARA2=AMAX1(TRARA2,0.)                                           TRAR2218
      RETURN                                                            TRAR2220
 55   TRARA2=0.                                                         TRAR2222
      RETURN                                                            TRAR2224
      END                                                               TRAR2226
