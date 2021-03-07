From:	NCF::BILITZA      "Dieter Bilitza, 301-441-4193"  9-DEC-1994 10:28:04.66
To:	STPMODELS
CC:	BILITZA
Subj:	euvac.for

From:	CSPAR::UAHOAL::RICHARDS     24-OCT-1994 15:48:58.11
To:	CSPARC::NCF::BILITZA
CC:	RICHARDS
Subj:	EUVAC model - let me know if it need more documentation

C............................ EUVAC.FOR ..........................
C--- Test driver for the EUVAC solar EUV flux model
C--- See Richards et al. [1994] J. Geophys. Res. p8981 for details.
       INTEGER I
       REAL F107,F107A,EUVFLX(37)
C----- Input F10.7
       WRITE(6,*) ' Input daily F10.7 (e.g. 74)'
       READ(5,*) F107
       WRITE(6,*) ' Input 81 day average F10.7 (F10.7A) (e.g. 86)'
       READ(5,*) F107A
C
C----- Now get EUV fluxes
       CALL EUVAC(F107,F107A,EUVFLX)
C
       WRITE(6,*) ' The wavelength bins are same as Torr et al. [1979]'
C------ write out the first 36 solar fluxes by 1.0E-9
       WRITE(6,*) ' F10.7=',F107,  '    F10.7A=', F107A
       WRITE(6,*) '   I     FLUX        I     FLUX        I     FLUX'
       DO 20 I=1,12
         WRITE(6,91) I,EUVFLX(I)/1.0E9,I+12,EUVFLX(I+12)/1.0E9
     >      ,I+24,EUVFLX(I+24)/1.0E9
 20    CONTINUE
C------ write out last wavelength bin
       I=37
       WRITE(6,91) I,EUVFLX(I)/1.0E9
C
 91    FORMAT(1X,I4,F10.3,4X,I4,F10.3,4X,I4,F10.3)
       END

C:::::::::::::::::::::::::::::::: EUVAC :::::::::::::::::::::::
C------ This EUV flux model uses the F74113 solar reference spectrum and 
C------ ratios determined from Hinteregger's SERF1 model. It uses the daily
C------ F10.7 flux (F107) and the 81 day mean (F107A) as a proxy for scaling
C------ The fluxes are returned in EUVFLX and correspond to the 37 wavelength
C------ bins of Torr et al. [1979] Geophys. Res. Lett. p771. 
C------ See Richards et al. [1994] J. Geophys. Res. p8981 for details.
C
C...... F107   = input daily 10.7 cm flux index. 
C...... F107A  = input 81 day average of daily F10.7 centered on current day
C...... EUVFLX = output array for EUV flux in units of photons/cm2/sec.
      SUBROUTINE EUVAC(F107,F107A,EUVFLX)
      INTEGER I
      REAL F107,F107A,EUVFLX(37),AFAC(37),F74113(37),FLXFAC
C
C------ F74113 reference spectrum (doubled below 150-250 A, tripled <150)
C------ Will be multiplied by 1.0E9 later
      DATA F74113/1.20,0.450,4.800,3.100,0.460,0.210,1.679,0.8
     > ,6.900,0.965,0.650,0.314,0.383,0.290,0.285,0.452,0.720
     > ,1.270,0.357,0.530,1.590,0.342,0.230,0.360,0.141,0.170
     > ,0.260,0.702,0.758,1.625,3.537,3.000,4.400,1.475,3.500
     > ,2.100,2.467/
C
C--- Scaling factors(Ai) for the EUV flux
      DATA AFAC/1.0017E-02,7.1250E-03,1.3375E-02,1.9450E-02,2.7750E-03
     > ,1.3768E-01,2.6467E-02,2.5000E-02,3.3333E-03,2.2450E-02
     > ,6.5917E-03,3.6542E-02,7.4083E-03,7.4917E-03,2.0225E-02
     > ,8.7583E-03,3.2667E-03,5.1583E-03,3.6583E-03,1.6175E-02
     > ,3.3250E-03,1.1800E-02,4.2667E-03,3.0417E-03,4.7500E-03
     > ,3.8500E-03,1.2808E-02,3.2750E-03,4.7667E-03,4.8167E-03
     > ,5.6750E-03,4.9833E-03,3.9417E-03,4.4167E-03,5.1833E-03
     > ,5.2833E-03,4.3750E-03/
C
C----- loop through the wavelengths calculating the scaling factors and
C----- the resulting solar flux.
C----- The scaling factors are restricted to be greater than 0.8
       DO 50 I=1,37
          FLXFAC=(1.0 + AFAC(I) * (0.5*(F107+F107A) - 80.0))
          IF(FLXFAC.LT.0.8) FLXFAC=0.8
          EUVFLX(I)=F74113(I) * FLXFAC * 1.0E9
 50    CONTINUE
      RETURN
      END
C
C---------------- Sample OUTPUT for solar minimum F74113 spectrum ---------
C
C---- Note that the fluxes in bins 1 and 2 are triple the F74113 fluxes 
C---- and bins 3 and 4 are double the F74113 fluxes. The other fluxes
C---- are the same as F74113.
C 
C  The wavelength bins are same as Torr et al. [1979]
C  F10.7=   74.00000        F10.7A=   86.00000    
C    I     FLUX    I     FLUX    I     FLUX
C    1     1.200  13     0.383  25     0.141 
C    2     0.450  14     0.290  26     0.170
C    3     4.800  15     0.285  27     0.260
C    4     3.100  16     0.452  28     0.702
C    5     0.460  17     0.720  29     0.758
C    6     0.210  18     1.270  30     1.625
C    7     1.679  19     0.357  31     3.537
C    8     0.800  20     0.530  32     3.000
C    9     6.900  21     1.590  33     4.400
C   10     0.965  22     0.342  34     1.475
C   11     0.650  23     0.230  35     3.500
C   12     0.314  24     0.360  36     2.100
C   37     2.467
C------------------ Sample OUTPUT for solar max ----------------------
C  The wavelength bins are same as Torr et al. [1979]
C  F10.7=   200.0000        F10.7A=   200.0000    
C    I     FLUX    I     FLUX    I     FLUX
C    1     2.642  13     0.723  25     0.221
C    2     0.835  14     0.551  26     0.249
C    3    12.504  15     0.977  27     0.660
C    4    10.335  16     0.927  28     0.978
C    5     0.613  17     1.002  29     1.192
C    6     3.680  18     2.056  30     2.564
C    7     7.012  19     0.514  31     5.946
C    8     3.200  20     1.559  32     4.794
C    9     9.660  21     2.224  33     6.481
C   10     3.565  22     0.826  34     2.257
C   11     1.164  23     0.348  35     5.677
C   12     1.691  24     0.491  36     3.431
C   37     3.762
