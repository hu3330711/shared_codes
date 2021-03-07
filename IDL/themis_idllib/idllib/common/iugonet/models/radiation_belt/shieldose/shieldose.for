
     SHIELDOSE, 12 FEB 80.                                               
        DEVELOPED BY S. M. SELTZER, NATIONAL BUREAU OF STANDARDS.        A   15ZZZZZZZZZZ
                                                                         A   20ZZZZZZZZZZ
        IDET = 1, AL DETECTOR                                            A   30ZZZZZZZZZZ
               2, H2-O DETECTOR                                          A   40ZZZZZZZZZZ
               3, SI DETECTOR                                            A   50ZZZZZZZZZZ
               4, SI-O2 DETECTOR                                         A   60ZZZZZZZZZZ
        INCIDENT OMNIDIRECTIONAL FLUX IN /ENERGY/CM2/UNIT TIME           A   70ZZZZZZZZZZ
        (SOLAR-FLARE FLUX IN /ENERGY/CM2).                               A   80ZZZZZZZZZZ
        EUNIT IS CONVERSION FACTOR FROM /ENERGY TO /MEV,                 A   90ZZZZZZZZZZ
             E.G., EUNIT = 1000 IF FLUX IS /KEV.                         A  100ZZZZZZZZZZ
        TINTER IS MISSION DURATION IN MULTIPLES OF UNIT TIME.            A  110ZZZZZZZZZZ
                                                                         A  120ZZZZZZZZZZ
     DIMENSION  EP(30),RP(30),DP(30,51),ZP(51),ER(80),RE(80),EE(10),     A  130ZZZZZZZZZZ
    1   DE(10,41,2),ZE(41),ZB(60),EB(10),DB(10,60,2),ZM(50),Z(50),       A  140ZZZZZZZZZZ
    2   TPL(301),TP(301),RINP(301),TEL(101),TE(101),RINE(101),SOL(301),  A  150ZZZZZZZZZZ
    3   EPS(101),S(101),SPG(301),G(301),SEG(101),DOSOL(50,2),            A  160ZZZZZZZZZZ
    4   DOSP(50,2),DIN(60,301),GP(301,50),GE(101,50,2),GB(101,50,2),     A  170ZZZZZZZZZZ
    5   DOSE(50,2,2),DOSB(50,2,2),ZMM(50),ZL(50),GARB(60)                A  180ZZZZZZZZZZ
     CALL ERRSET (208,256,-1,1)                                          A  190ZZZZZZZZZZ
     ZCON=0.001*2.540005*2.70                                            A  200ZZZZZZZZZZ
     ZMCON=10.0/2.70                                                     A  210ZZZZZZZZZZ
     RADCON=1.6021892E-08                                                A  220ZZZZZZZZZZ
     PRINT 10                                                            A  230ZZZZZZZZZZ
  10 FORMAT ('0 IDET   NUT IPRNT')                                       A  240ZZZZZZZZZZ
     READ 30, IDET,NUT,IPRNT                                             A  250ZZZZZZZZZZ
     PRINT 30, IDET,NUT,IPRNT                                            A  260ZZZZZZZZZZ
     IDET1=IDET+1                                                        A  270ZZZZZZZZZZ
     REWIND NUT                                                          A  280ZZZZZZZZZZ
     PRINT 20                                                            A  290ZZZZZZZZZZ
  20 FORMAT ('0MPMAX LPMAX  KMAX MEMAX LEMAX MBMAX LBMAX')               A  300ZZZZZZZZZZ
     READ (NUT,30) MPMAX,LPMAX,KMAX,MEMAX,LEMAX,MBMAX,LBMAX              A  310ZZZZZZZZZZ
     PRINT 30, MPMAX,LPMAX,KMAX,MEMAX,LEMAX,MBMAX,LBMAX                  A  320ZZZZZZZZZZ
  30 FORMAT (12I6)                                                       A  330ZZZZZZZZZZ
     PRINT 40                                                            A  340ZZZZZZZZZZ
  40 FORMAT ('0PROTON DOSE DATA (ENERGIES INCREASING)')                  A  350ZZZZZZZZZZ
     PRINT 50                                                            A  360ZZZZZZZZZZ
  50 FORMAT ('0     E(MEV)    R(G/CM2)')                                 A  370ZZZZZZZZZZ
     DO 110 M=1,MPMAX                                                    A  380ZZZZZZZZZZ
     READ (NUT,60) EP(M),RP(M)                                           A  390ZZZZZZZZZZ
     PRINT 60, EP(M),RP(M)                                               A  400ZZZZZZZZZZ
  60 FORMAT (1P6E12.4)                                                   A  410ZZZZZZZZZZ
     EP(M)=ALOG(EP(M))                                                   A  420ZZZZZZZZZZ
     RP(M)=ALOG(RP(M))                                                   A  430ZZZZZZZZZZ
                                                                             4 ZZZZZZZZZZ
  70 CONTINUE                                                            A  460ZZZZZZZZZZ
     GO TO (80,90), IPRNT                                                A  470ZZZZZZZZZZ
  80 PRINT 60, (DP(M,L),L=1,LPMAX)                                       A  480ZZZZZZZZZZ
  90 IF (IDET1.GT.4) GO TO 110                                           A  490ZZZZZZZZZZ
     DO 100 ID=IDET1,4                                                   A  500ZZZZZZZZZZ
     READ (NUT,60) (GARB(L),L=1,LPMAX)                                   A  510ZZZZZZZZZZ
 100 CONTINUE                                                            A  520ZZZZZZZZZZ
 110 CONTINUE                                                            A  530ZZZZZZZZZZ
     ZPFAC=1.0/FLOAT(LPMAX-1)                                            A  540ZZZZZZZZZZ
     DO 120 L=1,LPMAX                                                    A  550ZZZZZZZZZZ
 120 ZP(L)=FLOAT(L-1)*ZPFAC                                              A  560ZZZZZZZZZZ
     PRINT 130                                                           A  570ZZZZZZZZZZ
 130 FORMAT ('0ELECTRON DOSE DATA')                                      A  580ZZZZZZZZZZ
     PRINT 140                                                           A  590ZZZZZZZZZZ
 140 FORMAT ('0E(MEV)')                                                  A  600ZZZZZZZZZZ
     READ (NUT,150) (ER(K),K=1,KMAX)                                     A  610ZZZZZZZZZZ
     PRINT 150, (ER(K),K=1,KMAX)                                         A  620ZZZZZZZZZZ
 150 FORMAT (6F12.5)                                                     A  630ZZZZZZZZZZ
     PRINT 160                                                           A  640ZZZZZZZZZZ
 160 FORMAT ('0RANGE(G/CM2)')                                            A  650ZZZZZZZZZZ
     READ (NUT,60) (RE(K),K=1,KMAX)                                      A  660ZZZZZZZZZZ
     PRINT 60, (RE(K),K=1,KMAX)                                          A  670ZZZZZZZZZZ
     DO 170 K=1,KMAX                                                     A  680ZZZZZZZZZZ
     ER(K)=ALOG(ER(K))                                                   A  690ZZZZZZZZZZ
 170 RE(K)=ALOG(RE(K))                                                   A  700ZZZZZZZZZZ
     PRINT 180                                                           A  710ZZZZZZZZZZ
 180 FORMAT ('0E(MEV), THEN R0*D/E (ENERGIES INCREASING)')               A  720ZZZZZZZZZZ
     DO 270 M=1,MEMAX                                                    A  730ZZZZZZZZZZ
     READ (NUT,150) EE(M)                                                A  740ZZZZZZZZZZ
     PRINT 150, EE(M)                                                    A  750ZZZZZZZZZZ
     EE(M)=ALOG(EE(M))                                                   A  760ZZZZZZZZZZ
     DO 210 ID=1,IDET                                                    A  770ZZZZZZZZZZ
     DO 200 N=1,2                                                        A  780ZZZZZZZZZZ
     READ (NUT,190) (DE(M,L,N),L=1,LEMAX)                                A  790ZZZZZZZZZZ
 190 FORMAT (1P8E10.3)                                                   A  800ZZZZZZZZZZ
 200 CONTINUE                                                            A  810ZZZZZZZZZZ
 210 CONTINUE                                                            A  820ZZZZZZZZZZ
     GO TO (220,240), IPRNT                                              A  830ZZZZZZZZZZ
 220 DO 230 N=1,2                                                        A  840ZZZZZZZZZZ
     PRINT 190, (DE(M,L,N),L=1,LEMAX)                                    A  850ZZZZZZZZZZ
 230 CONTINUE                                                            A  860ZZZZZZZZZZ
 240 IF (IDET1.GT.4) GO TO 270                                           A  870ZZZZZZZZZZ
     DO 260 ID=IDET1,4                                                   A  880ZZZZZZZZZZ
     DO 250 N=1,2                                                        A  890ZZZZZZZZZZ
     READ (NUT,190) (GARB(L),L=1,LEMAX)                                  A  900ZZZZZZZZZZ
 250 CONTINUE                                                            A  910ZZZZZZZZZZ
 260 CONTINUE                                                            A   20ZZZZZZZZZZ
 270 CONTINUE                                                            A  930ZZZZZZZZZZ
     ZEFAC=1.0/FLOAT(LEMAX-1)                                            A  940ZZZZZZZZZZ
     DO 280 L=1,LEMAX                                                    A  950ZZZZZZZZZZ
 280 ZE(L)=FLOAT(L-1)*ZEFAC                                              A  960ZZZZZZZZZZ
     PRINT 290                                                           A  970ZZZZZZZZZZ
 290 FORMAT ('0BREMSSTRAHLUNG DOSE DATA (ENERGIES INCREASING)')          A  980ZZZZZZZZZZ
     PRINT 300                                                           A  990ZZZZZZZZZZ
 300 FORMAT ('0Z/E (G/CM2 KEV)')                                         A 1000ZZZZZZZZZZ
     READ (NUT,310) (ZB(L),L=1,LBMAX)                                    A 1010ZZZZZZZZZZ
     PRINT 310, (ZB(L),L=1,LBMAX)                                        A 1020ZZZZZZZZZZ
 310 FORMAT (1P8E9.2)                                                    A 1030ZZZZZZZZZZ
     DO 320 L=1,LBMAX                                                    A 1040ZZZZZZZZZZ
 320 ZB(L)=ALOG(1000.0*ZB(L))                                            A 1050ZZZZZZZZZZ
     PRINT 330                                                           A 1060ZZZZZZZZZZ
 330 FORMAT ('0E(MEV), THEN D/E (CM2/G)')                                A 1070ZZZZZZZZZZ
     DO 420 M=1,MBMAX                                                    A 1080ZZZZZZZZZZ
     READ (NUT,150) EB(M),RENORM                                         A 1090ZZZZZZZZZZ
     PRINT 150, EB(M),RENORM                                             A 1100ZZZZZZZZZZ
     EB(M)=ALOG(EB(M))                                                   A 1110ZZZZZZZZZZ
     DO 350 ID=1,IDET                                                    A 1120ZZZZZZZZZZ
     DO 340 N=1,2                                                        A 1130ZZZZZZZZZZ
     READ (NUT,310) (DB(M,L,N),L=1,LBMAX)                                A 1140ZZZZZZZZZZ
 340 CONTINUE                                                            A 1150ZZZZZZZZZZ
 350 CONTINUE                                                            A 1160ZZZZZZZZZZ
     GO TO (360,380), IPRNT                                              A 1170ZZZZZZZZZZ
 360 DO 370 N=1,2                                                        A 1180ZZZZZZZZZZ
     PRINT 310, (DB(M,L,N),L=1,LBMAX)                                    A 1190ZZZZZZZZZZ
 370 CONTINUE                                                            A 1200ZZZZZZZZZZ
 380 DO 390 N=1,2                                                        A 1210ZZZZZZZZZZ
     DO 390 L=1,LBMAX                                                    A 1220ZZZZZZZZZZ
     IF (IDET1.GT.4) GO TO 420                                           A 1240ZZZZZZZZZZ
     DO 410 ID=IDET1,4                                                   A 1250ZZZZZZZZZZ
     DO 400 N=1,2                                                        A 1260ZZZZZZZZZZ
     READ (NUT,310) (GARB(L),L=1,LBMAX)                                  A 1270ZZZZZZZZZZ
 400 CONTINUE                                                            A 1280ZZZZZZZZZZ
 410 CONTINUE                                                            A 1290ZZZZZZZZZZ
 420 CONTINUE                                                            A 1300ZZZZZZZZZZ
     PRINT 430                                                           A 1310ZZZZZZZZZZ
 430 FORMAT ('0 IMAX  IUNT')                                             A 1320ZZZZZZZZZZ
     READ 30, IMAX,IUNT                                                  A 1330ZZZZZZZZZZ
     PRINT 30, IMAX,IUNT                                                 A 1340ZZZZZZZZZZ
     GO TO (440,470,500), IUNT                                           A 1350ZZZZZZZZZZ
 440 PRINT 450                                                           A 1360ZZZZZZZZZZ
 450 FORMAT ('0SHIELD DEPTH (MILS)')                                     A 1370ZZZZZZZZZZ
     READ 150, (ZM(I),I=1,IMAX)                                          A 1380ZZZZZZZZZZ
     PRINT 150, (ZM(I),I=1,IMAX)                                         A 1390ZZZZZZZZZZ
     DO 460 I=1,IMAX                                                     A 1400ZZZZZZZZZZ
     Z(I)=ZCON*ZM(I)                                                     A 1410ZZZZZZZZZZ
 460 ZMM(I)=Z(I)*ZMCON                                                   A 1420ZZZZZZZZZZ
     GO TO 530                                                           A 1430ZZZZZZZZZZ
 470 PRINT 480                                                           A 1440ZZZZZZZZZZ
 480 FORMAT ('0SHIELD DEPTH (G/CM2)')                                    A 1450ZZZZZZZZZZ
     READ 150, (Z(I),I=1,IMAX)                                           A 1460ZZZZZZZZZZ
     PRINT 150, (Z(I),I=1,IMAX)                                          A 1470ZZZZZZZZZZ
     DO 490 I=1,IMAX                                                     A 1480ZZZZZZZZZZ
     ZM(I)=Z(I)/ZCON                                                     A 1490ZZZZZZZZZZ
 490 ZMM(I)=Z(I)*ZMCON                                                   A 1500ZZZZZZZZZZ
     GO TO 530                                                           A 1510ZZZZZZZZZZ
 500 PRINT 510                                                           A 1520ZZZZZZZZZZ
 510 FORMAT ('0SHIELD DEPTH (MM)')                                       A 1530ZZZZZZZZZZ
     READ 150, (ZMM(I),I=1,IMAX)                                         A 1540ZZZZZZZZZZ
     PRINT 150, (ZMM(I),I=1,IMAX)                                        A 1550ZZZZZZZZZZ
     DO 520 I=1,IMAX                                                     A 1560ZZZZZZZZZZ
     Z(I)=ZMM(I)/ZMCON                                                   A 1570ZZZZZZZZZZ
 520 ZM(I)=Z(I)/ZCON                                                     A 1580ZZZZZZZZZZ
 530 DO 540 I=1,IMAX                                                     A 1590ZZZZZZZZZZ
 540 ZL(I)=ALOG(Z(I))                                                    A 1600ZZZZZZZZZZ
     PRINT 550                                                           A 1610ZZZZZZZZZZ
 550 FORMAT ('0    EMINS     EMAXS     EMINP     EMAXP NPTSP     EMINE   A 1620ZZZZZZZZZZ
    1    EMAXE NPTSE')                                                   A 1630ZZZZZZZZZZ
     READ 560, EMINS,EMAXS,EMINP,EMAXP,NPTSP,EMINE,EMAXE,NPTSE           A 1640ZZZZZZZZZZ
     PRINT 560, EMINS,EMAXS,EMINP,EMAXP,NPTSP,EMINE,EMAXE,NPTSE          A 1650ZZZZZZZZZZ
 560 FORMAT (4F10.3,I6,2F10.3,I6)                                        A 1660ZZZZZZZZZZ
     EMINU=AMIN1(EMINP,EMINS)                                            A 1670ZZZZZZZZZZ
     EMAXU=AMAX1(EMAXP,EMAXS)                                            A 1680ZZZZZZZZZZ
     DEP=ALOG(EMAXU/EMINU)/FLOAT(NPTSP-1)                                A 1690ZZZZZZZZZZ
     NFSTS=ALOG(EMINS/EMINU)/DEP+0.5                                     A 1700ZZZZZZZZZZ
     NFSTS=NFSTS+1                                                       A 1710ZZZZZZZZZZ
     NLSTS=ALOG(EMAXS/EMINU)/DEP+0.5                                     A 1720ZZZZZZZZZZ
     NLSTS=NLSTS+1                                                       A 1730ZZZZZZZZZZ
     NLENS=NLSTS-NFSTS+1                                                 A 1740ZZZZZZZZZZ
     NFSTP=ALOG(EMINP/EMINU)/DEP+0.5                                     A 1750ZZZZZZZZZZ
     NFSTP=NFSTP+1                                                       A 1760ZZZZZZZZZZ
     NLSTP=ALOG(EMAXP/EMINU)/DEP+0.5                                     A 1770ZZZZZZZZZZ
     NLSTP=NLSTP+1                                                       A 1780ZZZZZZZZZZ
     NLENP=NLSTP-NFSTP+1                                                 A 1790ZZZZZZZZZZ
     EMINUL=ALOG(EMINU)                                                  A 1800ZZZZZZZZZZ
     DELP=DEP/3.0                                                        A 1810ZZZZZZZZZZ
     ICALL=1                                                             A 1820ZZZZZZZZZZ
     DO 570 NP=1,NPTSP                                                   A 1830ZZZZZZZZZZ
     TPL(NP)=EMINUL+FLOAT(NP-1)*DEP                                      A 1840ZZZZZZZZZZ
     TP(NP)=EXP(TPL(NP))                                                 A 1850ZZZZZZZZZZ
     CALL SPOL (TPL(NP),EP,RP,MPMAX,ICALL,ANS)                           A 1860ZZZZZZZZZZ
 570 RINP(NP)=EXP(ANS)                                                   A 1870ZZZZZZZZZZ
     PRINT 580, TP(NFSTS),TP(NLSTS),TP(NFSTP),TP(NLSTP),NPTSP,EMINE,EMA  A 1880ZZZZZZZZZZ
    1XE,NPTSE                                                            A 1890ZZZZZZZZZZ
 580 FORMAT (4F10.3,I6,2F10.3,I6,'  ADJUSTED VALUES')                    A 1900ZZZZZZZZZZ
     DO 620 L=1,LPMAX                                                    A 1910ZZZZZZZZZZ
     ICALL=1                                                             A 1920ZZZZZZZZZZ
     DO 610 NP=1,NPTSP                                                   A 1930ZZZZZZZZZZ
     IF (TPL(NP).LT.EP(MPMAX)) GO TO 590                                 A 1940ZZZZZZZZZZ
     DIN(L,NP)=DP(MPMAX,L)                                               A 1950ZZZZZZZZZZ
     GO TO 610                                                           A 1960ZZZZZZZZZZ
 590 IF (TPL(NP).GT.EP(1)) GO TO 600                                     A 1970ZZZZZZZZZZ
     DIN(L,NP)=DP(1,L)                                                   A 1980ZZZZZZZZZZ
     GO TO 610                                                           A 1990ZZZZZZZZZZ
 600 CALL SPOL (TPL(NP),EP,DP(1,L),MPMAX,ICALL,DIN(L,NP))                A 2000ZZZZZZZZZZ
 610 CONTINUE                                                            A 2010ZZZZZZZZZZ
 620 CONTINUE                                                            A 2020ZZZZZZZZZZ
     DO 660 NP=1,NPTSP                                                   A 2030ZZZZZZZZZZ
     ICALL=1                                                             A 2040ZZZZZZZZZZ
     DO 650 I=1,IMAX                                                     A 2050ZZZZZZZZZZ
     ZRIN=Z(I)/RINP(NP)                                                  A 2060ZZZZZZZZZZ
     IF (ZRIN.LT.1.0) GO TO 640                                          A 2070ZZZZZZZZZZ
 630 GP(NP,I)=0.0                                                        A 2080ZZZZZZZZZZ
     GO TO 650                                                           A 2090ZZZZZZZZZZ
 640 CALL SPOL (ZRIN,ZP,DIN(1,NP),LPMAX,ICALL,ANS)                       A 2100ZZZZZZZZZZ
     IF (ANS.LT.0.0) GO TO 630                                           A 2110ZZZZZZZZZZ
     GP(NP,I)=TP(NP)*ANS/RINP(NP)                                        A 2120ZZZZZZZZZZ
 650 CONTINUE                                                            A 2130ZZZZZZZZZZ
 660 CONTINUE                                                            A 2140ZZZZZZZZZZ
     EMINEL=ALOG(EMINE)                                                  A 2150ZZZZZZZZZZ
     DEE=(ALOG(EMAXE)-EMINEL)/FLOAT(NPTSE-1)                             A 2160ZZZZZZZZZZ
     DELE=DEE/3.0                                                        A 2170ZZZZZZZZZZ
     ICALL=1                                                             A 2180ZZZZZZZZZZ
     DO 670 NE=1,NPTSE                                                   A 2190ZZZZZZZZZZ
     TEL(NE)=EMINEL+FLOAT(NE-1)*DEE                                      A 2200ZZZZZZZZZZ
     TE(NE)=EXP(TEL(NE))                                                 A 2210ZZZZZZZZZZ
     CALL SPOL (TEL(NE),ER,RE,KMAX,ICALL,ANS)                            A 2220ZZZZZZZZZZ
 670 RINE(NE)=EXP(ANS)                                                   A 2230ZZZZZZZZZZ
     DO 820 N=1,2                                                        A 2240ZZZZZZZZZZ
     DO 710 L=1,LEMAX                                                    A 2250ZZZZZZZZZZ
     ICALL=1                                                             A 2260ZZZZZZZZZZ
     DO 700 NE=1,NPTSE                                                   A 2270ZZZZZZZZZZ
     IF (TEL(NE).LT.EE(MEMAX)) GO TO 680                                 A 2280ZZZZZZZZZZ
     DIN(L,NE)=DE(MEMAX,L,N)                                             A 2290ZZZZZZZZZZ
     GO TO 700                                                           A 2300ZZZZZZZZZZ
 680 IF (TEL(NE).GT.EE(1)) GO TO 690                                     A 2310ZZZZZZZZZZ
     DIN(L,NE)=DE(1,L,N)                                                 A 2320ZZZZZZZZZZ
     GO TO 700                                                           A 2330ZZZZZZZZZZ
 690 CALL SPOL (TEL(NE),EE,DE(1,L,N),MEMAX,ICALL,DIN(L,NE))              A 2340ZZZZZZZZZZ
 700 CONTINUE                                                            A 2350ZZZZZZZZZZ
 710 CONTINUE                                                            A 2360ZZZZZZZZZZ
     DO 750 NE=1,NPTSE                                                   A 2370ZZZZZZZZZZ
     ICALL=1                                                             A 2380ZZZZZZZZZZ
     DO 740 I=1,IMAX                                                     A 2390ZZZZZZZZZZ
     ZRIN=Z(I)/RINE(NE)                                                  A 2400ZZZZZZZZZZ
     IF (ZRIN.LT.1.0) GO TO 730                                          A 2410ZZZZZZZZZZ
 720 GE(NE,I,N)=0.0                                                      A 2420ZZZZZZZZZZ
     GO TO 740                                                           A 2430ZZZZZZZZZZ
 730 CALL SPOL (ZRIN,ZE,DIN(1,NE),LEMAX,ICALL,ANS)                       A 2440ZZZZZZZZZZ
     IF (ANS.LT.0.0) GO TO 720                                           A 2450ZZZZZZZZZZ
     GE(NE,I,N)=TE(NE)*ANS/RINE(NE)                                      A 2460ZZZZZZZZZZ
 740 CONTINUE                                                            A 2470ZZZZZZZZZZ
 750 CONTINUE                                                            A 2480ZZZZZZZZZZ
     DO 790 L=1,LBMAX                                                    A 2490ZZZZZZZZZZ
     ICALL=1                                                             A 2500ZZZZZZZZZZ
     DO 780 NE=1,NPTSE                                                   A 2510ZZZZZZZZZZ
     IF (TEL(NE).LT.EB(MBMAX)) GO TO 760                                 A 2520ZZZZZZZZZZ
     DIN(L,NE)=DB(MBMAX,L,N)                                             A 2530ZZZZZZZZZZ
     GO TO 780                                                           A 2540ZZZZZZZZZZ
 760 IF (TEL(NE).GT.EB(1)) GO TO 770                                     A 2550ZZZZZZZZZZ
     DIN(L,NE)=DB(1,L,N)                                                 A 2560ZZZZZZZZZZ
     GO TO 780                                                           A 2570ZZZZZZZZZZ
 770 CALL SPOL (TEL(NE),EB,DB(1,L,N),MBMAX,ICALL,DIN(L,NE))              A 2580ZZZZZZZZZZ
 780 CONTINUE                                                            A 2590ZZZZZZZZZZ
 790 CONTINUE                                                            A 2600ZZZZZZZZZZ
     DO 810 NE=1,NPTSE                                                   A 2610ZZZZZZZZZZ
     ICALL=1                                                             A 2620ZZZZZZZZZZ
     DO 800 I=1,IMAX                                                     A 2630ZZZZZZZZZZ
     ZBIN=ALOG(Z(I)/TE(NE))                                              A 2640ZZZZZZZZZZ
     CALL SPOL (ZBIN,ZB,DIN(1,NE),LBMAX,ICALL,ANS)                       A 2650ZZZZZZZZZZ
 800 GB(NE,I,N)=TE(NE)*EXP(ANS)                                          A 2660ZZZZZZZZZZ
 810 CONTINUE                                                            A 2670ZZZZZZZZZZ
 820 CONTINUE                                                            A 2680ZZZZZZZZZZ
 830 PRINT 840                                                           A 2690ZZZZZZZZZZ
 840 FORMAT ('1')                                                        A 2700ZZZZZZZZZZ
     PRINT 850                                                           A 2710ZZZZZZZZZZ
 850 FORMAT ('0')                                                        A 2720ZZZZZZZZZZ
     READ (5,860,END=1420)                                               A 2730ZZZZZZZZZZ
     PRINT 860                                                           A 2740ZZZZZZZZZZ
 860 FORMAT (72H                                                         A 2750ZZZZZZZZZZ
    1                 )                                                  A 2760ZZZZZZZZZZ
     PRINT 870                                                           A 2770ZZZZZZZZZZ
 870 FORMAT ('0JSMAX JPMAX JEMAX       EUNIT      TINTER')               A 2780ZZZZZZZZZZ
     READ 880, JSMAX,JPMAX,JEMAX,EUNIT,TINTER                            A 2790ZZZZZZZZZZ
     PRINT 880, JSMAX,JPMAX,JEMAX,EUNIT,TINTER                           A 2800ZZZZZZZZZZ
 880 FORMAT (3I6,1P2E12.5)                                               A 2810ZZZZZZZZZZ
     IF (TINTER.LE.0.0) TINTER=1.0                                       A 2820ZZZZZZZZZZ
     DELTAS=RADCON*DELP/4.0                                              A 2830ZZZZZZZZZZ
     DELTAP=TINTER*RADCON*DELP/4.0                                       A 2840ZZZZZZZZZZ
     DELTAE=TINTER*RADCON*DELE/4.0                                       A 2850ZZZZZZZZZZ
     IF (EUNIT.LE.0.0) EUNIT=1.0                                         A 2860ZZZZZZZZZZ
     ISOL=2                                                              A 2870ZZZZZZZZZZ
     IF (JSMAX.LT.3) GO TO 900                                           A 2880ZZZZZZZZZZ
     ISOL=1                                                              A 2890ZZZZZZZZZZ
     PRINT 140                                                           A 2900ZZZZZZZZZZ
     READ 60, (EPS(J),J=1,JSMAX)                                         A 2910ZZZZZZZZZZ
     PRINT 60, (EPS(J),J=1,JSMAX)                                        A 2920ZZZZZZZZZZ
     PRINT 890                                                           A 2930ZZZZZZZZZZ
 890 FORMAT ('0SOLAR PROTON SPECTRUM (/ENERGY/CM2)')                     A 2940ZZZZZZZZZZ
     READ 60, (S(J),J=1,JSMAX)                                           A 2950ZZZZZZZZZZ
     PRINT 60, (S(J),J=1,JSMAX)                                          A 2960ZZZZZZZZZZ
     CALL SPECTR (JSMAX,EPS,S,EUNIT,DELP,NLENS,TP(NFSTS),TPL(NFSTS),SOL  A 2970ZZZZZZZZZZ
    1(NFSTS))                                                            A 2980ZZZZZZZZZZ
 900 ITRP=2                                                              A 2990ZZZZZZZZZZ
     IF (JPMAX.LT.3) GO TO 920                                           A 3000ZZZZZZZZZZ
     ITRP=1                                                              A 3010ZZZZZZZZZZ
     PRINT 140                                                           A 3020ZZZZZZZZZZ
     READ 60, (EPS(J),J=1,JPMAX)                                         A 3030ZZZZZZZZZZ
     PRINT 60, (EPS(J),J=1,JPMAX)                                        A 3040ZZZZZZZZZZ
     PRINT 910                                                           A 3050ZZZZZZZZZZ
 910 FORMAT ('0TRAPPED PROTON SPECTRUM (/ENERGY/CM2/TIME)')              A 3060ZZZZZZZZZZ
     READ 60, (S(J),J=1,JPMAX)                                           A 3070ZZZZZZZZZZ
     PRINT 60, (S(J),J=1,JPMAX)                                          A 3080ZZZZZZZZZZ
     CALL SPECTR (JPMAX,EPS,S,EUNIT,DELP,NLENP,TP(NFSTP),TPL(NFSTP),SPG  A 3090ZZZZZZZZZZ
    1(NFSTP))                                                            A 3100ZZZZZZZZZZ
 920 ILEC=2                                                              A 3110ZZZZZZZZZZ
     IF (JEMAX.LT.3) GO TO 940                                           A 3120ZZZZZZZZZZ
     ILEC=1                                                              A 3130ZZZZZZZZZZ
     PRINT 140                                                           A 3140ZZZZZZZZZZ
     READ 60, (EPS(J),J=1,JEMAX)                                         A 3150ZZZZZZZZZZ
     PRINT 60, (EPS(J),J=1,JEMAX)                                        A 3160ZZZZZZZZZZ
     PRINT 930                                                           A 3170ZZZZZZZZZZ
 930 FORMAT ('0ELECTRON SPECTRUM (/ENERGY/CM2/TIME)')                    A 3180ZZZZZZZZZZ
     READ 60, (S(J),J=1,JEMAX)                                           A 3190ZZZZZZZZZZ
     PRINT 60, (S(J),J=1,JEMAX)                                          A 3200ZZZZZZZZZZ
     CALL SPECTR (JEMAX,EPS,S,EUNIT,DELE,NPTSE,TE,TEL,SEG)               A 3210ZZZZZZZZZZ
 940 GO TO (980,950), ISOL                                               A 3220ZZZZZZZZZZ
 950 DO 960 NP=NFSTS,NLSTS                                               A 3230ZZZZZZZZZZ
 960 SOL(NP)=0.0                                                         A 3240ZZZZZZZZZZ
     DO 970 J=1,2                                                        A 3250ZZZZZZZZZZ
     DO 970 I=1,IMAX                                                     A 3260ZZZZZZZZZZ
 970 DOSOL(I,J)=0.0                                                      A 3270ZZZZZZZZZZ
     GO TO 1010                                                          A 3280ZZZZZZZZZZ
 980 DO 1000 I=1,IMAX                                                    A 3290ZZZZZZZZZZ
     DO 990 NP=NFSTS,NLSTS                                               A 3300ZZZZZZZZZZ
 990 G(NP)=SOL(NP)*GP(NP,I)                                              A 3310ZZZZZZZZZZ
     CALL INT (DELTAS,G(NFSTS),NLENS,DOSOL(I,1))                         A 3320ZZZZZZZZZZ
1000 CONTINUE                                                            A 3330ZZZZZZZZZZ
     CALL SPHERE (ZL,DOSOL(1,1),IMAX,DOSOL(1,2))                         A 3340ZZZZZZZZZZ
1010 GO TO (1050,1020), ITRP                                             A 3350ZZZZZZZZZZ
1020 DO 1030 NP=NFSTP,NLSTP                                              A 3360ZZZZZZZZZZ
1030 SPG(NP)=0.0                                                         A 3370ZZZZZZZZZZ
     DO 1040 J=1,2                                                       A 3380ZZZZZZZZZZ
     DO 1040 I=1,IMAX                                                    A 3390ZZZZZZZZZZ
1040 DOSP(I,J)=0.0                                                       A 3400ZZZZZZZZZZ
     GO TO 1080                                                          A 3410ZZZZZZZZZZ
1050 DO 1070 I=1,IMAX                                                    A 3420ZZZZZZZZZZ
     DO 1060 NP=NFSTP,NLSTP                                              A 3430ZZZZZZZZZZ
1060 G(NP)=SPG(NP)*GP(NP,I)                                              A 3440ZZZZZZZZZZ
     CALL INT (DELTAP,G(NFSTP),NLENP,DOSP(I,1))                          A 3450ZZZZZZZZZZ
1070 CONTINUE                                                            A 3460ZZZZZZZZZZ
     CALL SPHERE (ZL,DOSP(1,1),IMAX,DOSP(1,2))                           A 3470ZZZZZZZZZZ
1080 GO TO (1110,1090), ILEC                                             A 3480ZZZZZZZZZZ
1090 DO 1100 J=1,2                                                       A 3490ZZZZZZZZZZ
     DO 1100 N=1,2                                                       A 3500ZZZZZZZZZZ
     DO 1100 I=1,IMAX                                                    A 3510ZZZZZZZZZZ
     DOSE(I,N,J)=0.0                                                     A 3520ZZZZZZZZZZ
1100 DOSB(I,N,J)=0.0                                                     A 3530ZZZZZZZZZZ
     GO TO 1160                                                          A 3540ZZZZZZZZZZ
1110 DO 1150 N=1,2                                                       A 3550ZZZZZZZZZZ
     DO 1130 I=1,IMAX                                                    A 3560ZZZZZZZZZZ
     DO 1120 NE=1,NPTSE                                                  A 3570ZZZZZZZZZZ
     G(NE)=SEG(NE)*GE(NE,I,N)                                            A 3580ZZZZZZZZZZ
1120 SPG(NE)=SEG(NE)*GB(NE,I,N)                                          A 3590ZZZZZZZZZZ
     CALL INT (DELTAE,G,NPTSE,DOSE(I,N,1))                               A 3600ZZZZZZZZZZ
     CALL INT (DELTAE,SPG,NPTSE,DOSB(I,N,1))                             A 3610ZZZZZZZZZZ
1130 CONTINUE                                                            A 3620ZZZZZZZZZZ
     GO TO (1150,1140), N                                                A 3630ZZZZZZZZZZ
1140 CALL SPHERE (ZL,DOSE(1,N,1),IMAX,DOSE(1,N,2))                       A 3640ZZZZZZZZZZ
     CALL SPHERE (ZL,DOSB(1,N,1),IMAX,DOSB(1,N,2))                       A 3650ZZZZZZZZZZ
1150 CONTINUE                                                            A 3660ZZZZZZZZZZ
1160 J=1                                                                 A 3670ZZZZZZZZZZ
     DO 1340 N=1,2                                                       A 3680ZZZZZZZZZZ
     GO TO (1170,1190), N                                                A 3690ZZZZZZZZZZ
1170 PRINT 1180                                                          A 3700ZZZZZZZZZZ
1180 FORMAT ('1DOSE AT TRANSMISSION SURFACE OF FINITE ALUMINUM SLAB SHI  A 3710ZZZZZZZZZZ
    1ELDS')                                                              A 3720ZZZZZZZZZZ
     GO TO 1210                                                          A 3730ZZZZZZZZZZ
1190 PRINT 1200                                                          A 3740ZZZZZZZZZZ
1200 FORMAT ('1DOSE IN SEMI-INFINITE ALUMINUM MEDIUM')                   A 3750ZZZZZZZZZZ
1210 GO TO (1220,1240,1260,1280), IDET                                   A 3760ZZZZZZZZZZ
1220 PRINT 1230                                                          A 3770ZZZZZZZZZZ
1230 FORMAT ('0RADS AL')                                                 A 3780ZZZZZZZZZZ
     GO TO 1300                                                          A 3790ZZZZZZZZZZ
1240 PRINT 1250                                                          A 3800ZZZZZZZZZZ
1250 FORMAT ('0RADS H2O')                                                A 3810ZZZZZZZZZZ
     GO TO 1300                                                          A 3820ZZZZZZZZZZ
1260 PRINT 1270                                                          A 3830ZZZZZZZZZZ
1270 FORMAT ('0RADS SI')                                                 A 3840ZZZZZZZZZZ
     GO TO 1300                                                          A 3850ZZZZZZZZZZ
1280 PRINT 1290                                                          A 3860ZZZZZZZZZZ
1290 FORMAT ('0RADS SI-O2')                                              A 3870ZZZZZZZZZZ
1300 PRINT 1310                                                          A 3880ZZZZZZZZZZ
1310 FORMAT ('0   Z(MILS)      Z(MM)   Z(G/CM2)   ELECTRON    BREMS      A 3890ZZZZZZZZZZ
    1  EL+BR     TRP PROT   SOL PROT  EL+BR+TRP    TOTAL')               A 3900ZZZZZZZZZZ
     PRINT 850                                                           A 3910ZZZZZZZZZZ
     DO 1330 I=1,IMAX                                                    A 3920ZZZZZZZZZZ
     DOSEB=DOSE(I,N,J)+DOSB(I,N,J)                                       A 3930ZZZZZZZZZZ
     DOSEBP=DOSEB+DOSP(I,J)                                              A 3940ZZZZZZZZZZ
     DOST=DOSEBP+DOSOL(I,J)                                              A 3950ZZZZZZZZZZ
     PRINT 1320, ZM(I),ZMM(I),Z(I),DOSE(I,N,J),DOSB(I,N,J),DOSEB,DOSP(I  A 3960ZZZZZZZZZZ
    1,J),DOSOL(I,J),DOSEBP,DOST                                          A 3970ZZZZZZZZZZ
1320 FORMAT (0P3F11.3,1P7E11.3)                                          A 3980ZZZZZZZZZZ
1330 CONTINUE                                                            A 3990ZZZZZZZZZZ
1340 CONTINUE                                                            A 4000ZZZZZZZZZZ
     J=2                                                                 A 4010ZZZZZZZZZZ
     N=2                                                                 A 4020ZZZZZZZZZZ
     PRINT 1350                                                          A 4030ZZZZZZZZZZ
1350 FORMAT ('11/2 DOSE AT CENTER OF ALUMINUM SPHERES')                  A 4040ZZZZZZZZZZ
     GO TO (1360,1370,1380,1390), IDET                                   A 4050ZZZZZZZZZZ
1360 PRINT 1230                                                          A 4060ZZZZZZZZZZ
     GO TO 1400                                                          A 4070ZZZZZZZZZZ
1370 PRINT 1250                                                          A 4080ZZZZZZZZZZ
     GO TO 1400                                                          A 4090ZZZZZZZZZZ
1380 PRINT 1270                                                          A 4100ZZZZZZZZZZ
     GO TO 1400                                                          A 4110ZZZZZZZZZZ
1390 PRINT 1290                                                          A 4120ZZZZZZZZZZ
1400 PRINT 1310                                                          A 4130ZZZZZZZZZZ
     PRINT 850                                                           A 4140ZZZZZZZZZZ
     DO 1410 I=1,IMAX                                                    A 4150ZZZZZZZZZZ
     DOSEB=DOSE(I,N,J)+DOSB(I,N,J)                                       A 4160ZZZZZZZZZZ
     DOSEBP=DOSEB+DOSP(I,J)                                              A 4170ZZZZZZZZZZ
     DOST=DOSEBP+DOSOL(I,J)                                              A 4180ZZZZZZZZZZ
     PRINT 1320, ZM(I),ZMM(I),Z(I),DOSE(I,N,J),DOSB(I,N,J),DOSEB,DOSP(I  A 4190ZZZZZZZZZZ
    1,J),DOSOL(I,J),DOSEBP,DOST                                          A 4200ZZZZZZZZZZ
1410 CONTINUE                                                            A 4210ZZZZZZZZZZ
     GO TO 830                                                           A 4220ZZZZZZZZZZ
1420 STOP                                                                A 4230ZZZZZZZZZZ
     END                                                                 A 4240ZZZZZZZZZZ
     SUBROUTINE SPECTR (JMAX,EPS,S,EUNIT,DELTA,NPTS,T,TL,SP), 1 NOV 79.  B   10ZZZZZZZZZZ
     SUBROUTINE SPECTR (JMAX,EPS,S,EUNIT,DELTA,NPTS,T,TL,SP)             B   20ZZZZZZZZZZ
     DIMENSION  EPS(1),S(1),T(1),TL(1),SP(1),G(301)                      B   30ZZZZZZZZZZ
     IF (EPS(1).GT.0.0) GO TO 20                                         B   40ZZZZZZZZZZ
     ALPHA=S(1)                                                          B   50ZZZZZZZZZZ
     BETA=S(2)                                                           B   60ZZZZZZZZZZ
     IF (BETA.LE.0.0) BETA=1.0                                           B   70ZZZZZZZZZZ
     BETA=BETA/ALPHA                                                     B   80ZZZZZZZZZZ
     DO 10 N=1,NPTS                                                      B   90ZZZZZZZZZZ
     SP(N)=T(N)*BETA*EXP(-T(N)/ALPHA)                                    B  100ZZZZZZZZZZ
  10 G(N)=T(N)*SP(N)                                                     B  110ZZZZZZZZZZ
     GO TO 50                                                            B  120ZZZZZZZZZZ
  20 DO 30 J=1,JMAX                                                      B  130ZZZZZZZZZZ
     EPS(J)=ALOG(EPS(J))                                                 B  140ZZZZZZZZZZ
  30 S(J)=ALOG(EUNIT*S(J))                                               B  150ZZZZZZZZZZ
     ICALL=1                                                             B  160ZZZZZZZZZZ
     DO 40 N=1,NPTS                                                      B  170ZZZZZZZZZZ
     CALL SPOL (TL(N),EPS,S,JMAX,ICALL,ANS)                              B  180ZZZZZZZZZZ
     SP(N)=T(N)*EXP(ANS)                                                 B  190ZZZZZZZZZZ
  40 G(N)=T(N)*SP(N)                                                     B  200ZZZZZZZZZZ
  50 CALL INT (DELTA,SP,NPTS,SIN)                                        B  210ZZZZZZZZZZ
     CALL INT (DELTA,G,NPTS,EBAR)                                        B  220ZZZZZZZZZZ
     EBAR=EBAR/SIN                                                       B  230ZZZZZZZZZZ
     PRINT 60                                                            B  240ZZZZZZZZZZ
  60 FORMAT ('0   INT SPEC    EAV(MEV)')                                 B  250ZZZZZZZZZZ
     PRINT 70, SIN,EBAR                                                  B  260ZZZZZZZZZZ
  70 FORMAT (1PE12.4,0PF12.5)                                            B  270ZZZZZZZZZZ
     RETURN                                                              B  280ZZZZZZZZZZ
     END                                                                 B  290ZZZZZZZZZZ
     SUBROUTINE SPOL (S,X,Y,N,IN,T), 15 JAN 71.                          C   10ZZZZZZZZZZ
     SUBROUTINE SPOL (S,X,Y,N,IN,T)                                      C   20ZZZZZZZZZZ
     CUBIC SPLINE INTERPOLATION WITH PARABOLIC RUNOUT.                   C   30ZZZZZZZZZZ
     DIMENSION  X(1),Y(1),E(101),U(101)                                  C   40ZZZZZZZZZZ
     GO TO (10,50), IN                                                   C   50ZZZZZZZZZZ
  10 IN=2                                                                C   60ZZZZZZZZZZ
     N1=N-1                                                              C   70ZZZZZZZZZZ
     E(1)=1.0                                                            C   80ZZZZZZZZZZ
     U(1)=0.0                                                            C   90ZZZZZZZZZZ
     B1=X(2)-X(1)                                                        C  100ZZZZZZZZZZ
     C1=(Y(2)-Y(1))/B1                                                   C  110ZZZZZZZZZZ
     DO 20 J=2,N1                                                        C  120ZZZZZZZZZZ
     B2=X(J+1)-X(J)                                                      C  130ZZZZZZZZZZ
     C2=(Y(J+1)-Y(J))/B2                                                 C  140ZZZZZZZZZZ
     B=X(J+1)-X(J-1)                                                     C  150ZZZZZZZZZZ
     D=(C2-C1)/B                                                         C  160ZZZZZZZZZZ
     C=B1/B                                                              C  170ZZZZZZZZZZ
     B1=B2                                                               C  180ZZZZZZZZZZ
     C1=C2                                                               C  190ZZZZZZZZZZ
     P=C*E(J-1)+2.0                                                      C  200ZZZZZZZZZZ
     E(J)=(C-1.0)/P                                                      C  210ZZZZZZZZZZ
  20 U(J)=(D-C*U(J-1))/P                                                 C  220ZZZZZZZZZZ
     E(N)=U(N1)/(1.0-E(N1))                                              C  230ZZZZZZZZZZ
     DO 30 KK=1,N1                                                       C  240ZZZZZZZZZZ
     K=N-KK                                                              C  250ZZZZZZZZZZ
  30 E(K)=E(K)*E(K+1)+U(K)                                               C  260ZZZZZZZZZZ
     IF (X(1).GT.X(N)) GO TO 40                                          C  270ZZZZZZZZZZ
     IDIR=0                                                              C  280ZZZZZZZZZZ
     MLB=0                                                               C  290ZZZZZZZZZZ
     MUB=N                                                               C  300ZZZZZZZZZZ
     GO TO 50                                                            C  310ZZZZZZZZZZ
  40 IDIR=1                                                              C  320ZZZZZZZZZZ
     MLB=N                                                               C  330ZZZZZZZZZZ
     MUB=0                                                               C  340ZZZZZZZZZZ
  50 IF (S.GE.X(MUB+IDIR)) GO TO 90                                      C  350ZZZZZZZZZZ
     IF (S.LE.X(MLB+1-IDIR)) GO TO 100                                   C  360ZZZZZZZZZZ
     ML=MLB                                                              C  370ZZZZZZZZZZ
     MU=MUB                                                              C  380ZZZZZZZZZZ
     GO TO 70                                                            C  390ZZZZZZZZZZ
  60 IF (IABS(MU-ML).LE.1) GO TO 110                                     C  400ZZZZZZZZZZ
  70 MAV=(ML+MU)/2                                                       C  410ZZZZZZZZZZ
     IF (S.LT.X(MAV)) GO TO 80                                           C  420ZZZZZZZZZZ
     ML=MAV                                                              C  430ZZZZZZZZZZ
     GO TO 60                                                            C  440ZZZZZZZZZZ
  80 MU=MAV                                                              C  450ZZZZZZZZZZ
     GO TO 60                                                            C  460ZZZZZZZZZZ
  90 MU=MUB+2*IDIR                                                       C  470ZZZZZZZZZZ
     GO TO 120                                                           C  480ZZZZZZZZZZ
 100 MU=MLB+2*(1-IDIR)                                                   C  490ZZZZZZZZZZ
     GO TO 120                                                           C  500ZZZZZZZZZZ
 110 MU=MU+IDIR                                                          C  510ZZZZZZZZZZ
 120 T=(E(MU-1)*((X(MU)-S)**3)+E(MU)*((S-X(MU-1))**3)+(Y(MU-1)-E(MU-1)*  C  520ZZZZZZZZZZ
    1((X(MU)-X(MU-1))**2))*(X(MU)-S)+(Y(MU)-E(MU)*((X(MU)-X(MU-1))**2))  C  530ZZZZZZZZZZ
    2*(S-X(MU-1)))/(X(MU)-X(MU-1))                                       C  540ZZZZZZZZZZ
     RETURN                                                              C  550ZZZZZZZZZZ
     END                                                                 C  560ZZZZZZZZZZ
     SUBROUTINE INT(DELTA,G,N,RESULT), (N=1).                            D   10ZZZZZZZZZZ
     SUBROUTINE INT (DELTA,G,N,RESULT)                                   D   20ZZZZZZZZZZ
     DIMENSION G(1)                                                      D   30ZZZZZZZZZZ
     NL1=N-1                                                             D   40ZZZZZZZZZZ
     NL2=N-2                                                             D   50ZZZZZZZZZZ
     IF (FLOAT(N)-2.0*FLOAT(N/2)) 90,90,10                               D   60ZZZZZZZZZZ
  10 IF (N-1) 20,20,30                                                   D   70ZZZZZZZZZZ
  20 SIGMA=0.0                                                           D   80ZZZZZZZZZZ
     GO TO 80                                                            D   90ZZZZZZZZZZ
  30 IF (N-3) 40,40,50                                                   D  100ZZZZZZZZZZ
  40 SIGMA=G(1)+4.0*G(2)+G(3)                                            D  110ZZZZZZZZZZ
     GO TO 80                                                            D  120ZZZZZZZZZZ
  50 SUM4=0.0                                                            D  130ZZZZZZZZZZ
     DO 60 K=2,NL1,2                                                     D  140ZZZZZZZZZZ
  60 SUM4=SUM4+G(K)                                                      D  150ZZZZZZZZZZ
     SUM2=0.0                                                            D  160ZZZZZZZZZZ
     DO 70 K=3,NL2,2                                                     D  170ZZZZZZZZZZ
  70 SUM2=SUM2+G(K)                                                      D  180ZZZZZZZZZZ
     SIGMA=G(1)+4.0*SUM4+2.0*SUM2+G(N)                                   D  190ZZZZZZZZZZ
  80 RESULT=DELTA*SIGMA                                                  D  200ZZZZZZZZZZ
     RETURN                                                              D  210ZZZZZZZZZZ
  90 IF (N-2) 100,100,110                                                D  220ZZZZZZZZZZ
 100 SIGMA=1.5*(G(1)+G(2))                                               D  230ZZZZZZZZZZ
     GO TO 80                                                            D  240ZZZZZZZZZZ
 110 IF (N-4) 120,120,130                                                D  250ZZZZZZZZZZ
 120 SIGMA=1.125*(G(1)+3.0*G(2)+3.0*G(3)+G(4))                           D  260ZZZZZZZZZZ
     GO TO 80                                                            D  270ZZZZZZZZZZ
 130 IF (N-6) 140,140,150                                                D  280ZZZZZZZZZZ
 140 SIGMA=G(1)+3.875*G(2)+2.625*G(3)+2.625*G(4)+3.875*G(5)+G(6)         D  290ZZZZZZZZZZ
     GO TO 80                                                            D  300ZZZZZZZZZZ
 150 IF (N-8) 160,160,170                                                D  310ZZZZZZZZZZ
 160 SIGMA=G(1)+3.875*G(2)+2.625*G(3)+2.625*G(4)+3.875*G(5)+2.0*G(6)+4.  D  320ZZZZZZZZZZ
    10*G(7)+G(8)                                                         D  330ZZZZZZZZZZ
     GO TO 80                                                            D  340ZZZZZZZZZZ
 170 SIG6=G(1)+3.875*G(2)+2.625*G(3)+2.625*G(4)+3.875*G(5)+G(6)          D  350ZZZZZZZZZZ
     SUM4=0.0                                                            D  360ZZZZZZZZZZ
     DO 180 K=7,NL1,2                                                    D  370ZZZZZZZZZZ
 180 SUM4=SUM4+G(K)                                                      D  380ZZZZZZZZZZ
     SUM2=0.0                                                            D  390ZZZZZZZZZZ
     DO 190 K=8,NL2,2                                                    D  400ZZZZZZZZZZ
 190 SUM2=SUM2+G(K)                                                      D  410ZZZZZZZZZZ
     SIGMA=SIG6+G(6)+4.0*SUM4+2.0*SUM2+G(N)                              D  420ZZZZZZZZZZ
     GO TO 80                                                            D  430ZZZZZZZZZZ
     END                                                                 D  440ZZZZZZZZZZ
     SUBROUTINE SPHERE (ZL,DOSE,IMAX,DOSPH), 30 AUG 79                   E   10ZZZZZZZZZZ
     SUBROUTINE SPHERE (ZL,DOSE,IMAX,DOSPH)                              E   20ZZZZZZZZZZ
     DIMENSION  ZL(1),DOSE(1),DOSL(50),DERV(50),DOSPH(1)                 E   30ZZZZZZZZZZ
     DO 10 I=1,IMAX                                                      E   40ZZZZZZZZZZ
     IF (DOSE(I).LE.0.0) GO TO 20                                        E   50ZZZZZZZZZZ
  10 DOSL(I)=ALOG(DOSE(I))                                               E   60ZZZZZZZZZZ
     I=IMAX+1                                                            E   70ZZZZZZZZZZ
  20 IMIX=I-1                                                            E   80ZZZZZZZZZZ
     IF (IMIX.LT.3) GO TO 40                                             E   90ZZZZZZZZZZ
     CALL SPLDRV (ZL,DOSL,DERV,IMIX)                                     E  100ZZZZZZZZZZ
     DO 30 I=1,IMIX                                                      E  110ZZZZZZZZZZ
  30 DOSPH(I)=DOSE(I)*(1.0-DERV(I))                                      E  120ZZZZZZZZZZ
  40 IMIX1=IMIX+1                                                        E  130ZZZZZZZZZZ
     IF (IMIX1.GT.IMAX) RETURN                                           E  140ZZZZZZZZZZ
     DO 50 I=IMIX1,IMAX                                                  E  150ZZZZZZZZZZ
  50 DOSPH(I)=0.0                                                        E  160ZZZZZZZZZZ
     RETURN                                                              E  170ZZZZZZZZZZ
     END                                                                 E  180ZZZZZZZZZZ
     SUBROUTINE SPLDRV (X,Y,U,N), 28 AUG 79.                             F   10ZZZZZZZZZZ
     SUBROUTINE SPLDRV (X,Y,U,N)                                         F   20ZZZZZZZZZZ
     CUBIC SPLINE WITH PARABOLIC RUNOUT.                                 F   30ZZZZZZZZZZ
        U CONTAINS DERIVATIVES AT KNOTS                                  F   40ZZZZZZZZZZ
     DIMENSION  X(1),Y(1),E(101),U(1)                                    F   50ZZZZZZZZZZ
     N1=N-1                                                              F   60ZZZZZZZZZZ
     E(1)=1.0                                                            F   70ZZZZZZZZZZ
     U(1)=0.0                                                            F   80ZZZZZZZZZZ
     B1=X(2)-X(1)                                                        F   90ZZZZZZZZZZ
     C1=(Y(2)-Y(1))/B1                                                   F  100ZZZZZZZZZZ
     DO 10 J=2,N1                                                        F  110ZZZZZZZZZZ
     B2=X(J+1)-X(J)                                                      F  120ZZZZZZZZZZ
     C2=(Y(J+1)-Y(J))/B2                                                 F  130ZZZZZZZZZZ
     B=X(J+1)-X(J-1)                                                     F  140ZZZZZZZZZZ
     D=(C2-C1)/B                                                         F  150ZZZZZZZZZZ
     C=B1/B                                                              F  160ZZZZZZZZZZ
     B1=B2                                                               F  170ZZZZZZZZZZ
     C1=C2                                                               F  180ZZZZZZZZZZ
     P=C*E(J-1)+2.0                                                      F  190ZZZZZZZZZZ
     E(J)=(C-1.0)/P                                                      F  200ZZZZZZZZZZ
  10 U(J)=(D-C*U(J-1))/P                                                 F  210ZZZZZZZZZZ
     E(N)=U(N1)/(1.0-E(N1))                                              F  220ZZZZZZZZZZ
     DO 20 KK=1,N1                                                       F  230ZZZZZZZZZZ
     K=N-KK                                                              F  240ZZZZZZZZZZ
     E(K)=E(K)*E(K+1)+U(K)                                               F  250ZZZZZZZZZZ
     B2=X(K+1)-X(K)                                                      F  260ZZZZZZZZZZ
     U(K)=(Y(K+1)-Y(K))/B2-B2*(2.0*E(K)+E(K+1))                          F  270ZZZZZZZZZZ
  20 CONTINUE                                                            F  280ZZZZZZZZZZ
     U(N)=(Y(N)-Y(N1))/B2+B2*(2.0*E(N)+E(N1))                            F  290ZZZZZZZZZZ
     RETURN                                                              F  300ZZZZZZZZZZ
     END                                                                 F  310ZZZZZZZZZZ