C *** ***************  DIFFERENTIAL SPECTRUM SUBROUTINE  ***************
C *** CALCULATES FIRST DERIVATIVES OF INPUT SPECTRUM DEFINED BY FF VS XX
C *** INPUT:   XX - 30 INTEGRAL THRESHOLD ENERGIES, IN MEV         (R*4)
C ***          FF - ALOG OF THE INTEGRAL FLUXES FOR THE 30 ENERGY  (R*4)
C ***               LEVELS, IN PARTICLES/CM**2/SEC
C *** OUTPUT:  DD - DIFFERENTIAL FLUXES OBTAINED FROM THE INTEGRAL (R*4)
C ***               FLUXES, IN PARTICLES/CM**2/SEC/KEV
C *** ******************************************************************
C *** THIS IS A MODIFIED VERSION OF A PROGRAM (DCS1FU) OBTAINED FROM
C *** IMSL LIBRARY 1:  AUTHOR/IMPLEMENTOR - C.L.SMITH
C *** ******************************************************************
      SUBROUTINE DSPCTR(FF,XX,DD)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 DD,FF,XX
      DIMENSION F(30),X(30),D(30),H(500),FF(30) ,XX(30) ,DD(30)
      DATA EPSLN,OMEGA/1.D-6,1.0717968D0/
C *** DATA INITIALIZATION
      M=0
      DO 5 L=1,30
    5 DD(L)=0.0
C *** DETERMINE SIZE OF ARRAY: OBTAIN M & K INDICES
C ***   M = # OF NONZERO FLUXES - 1;  K = # OF NONZERO FLUXES
      DO 10 K=1,30
      IF(FF(K).EQ.0.)      GO TO 15
      M=K-1
      F(K)=FF(K)+ALOG(1000.)
      X(K)=XX(K)*1000.D0
   10 D(K)=X(K)
   15 K=M+1
      IF(K.LT.10) GO TO 170
C *** SMOOTHING INTEGRAL FLUX
      CALL SMOOTH(X,F,M)
C *** CALCULATE SECOND DERIVATIVES USING CENTRAL DIFFERENCES
      DO 30 I=1,M
         H(I)=X(I+1)-X(I)
   30    H(K +I)=(F(I+1)-F(I))/H(I)
      DO 40  I=2,M
         H(2*K+I)=H(I-1)+H(I)
         H(3*K+I)=.5*H(I-1)/H (2*K+I)
         H(4*K+I)=(H(K+I)-H(K+I-1))/H(2*K+I)
         H(5*K+I)=H(4*K+I)+H(4*K+I)
   40    H(6*K+I)=H(5*K+I)+H(4*K+I)
      H(5*K+1)=0.
      H(6*K)=0.
C *** BEGIN ITERATION ON SECOND DERIVATIVES
      KCOUNT=0
   50 ETA=0.
      KCOUNT=KCOUNT+1
      DO 70  I=2,M
         W=(H(6*K+I)-H(3*K+I)*H(5*K+I-1)-(.5-H(3*K+I))*H(5*K+I+1)-H(5*K+
     $   I)*OMEGA)
         IF (DABS(W).LE.ETA) GO TO 60
         ETA=DABS(W)
   60    H(5*K+I)=H(5*K+I)+W
   70 CONTINUE
      IF(KCOUNT.GT.5*K)GO TO 170
      IF (ETA.GE.EPSLN) GO TO 50
C *** CONVERGENCE OBTAINED
      DO 80 I=1,M
   80    H(7*K+I)=(H(5*K+1+I)-H(5*K+I))/H(I)
      DO 140 J=1,K
         I=1
         IF (D(J).EQ.X(1))GO TO 130
         IF (D(J)-X(K )) 100,110,110
   90    IF (D(J)-X(I)) 120,130,100
  100    I=I+1
         GO TO 90
  110    I=K
  120    I=I-1
C *** COMPUTE D(J)
  130    HT1=D(J)-X(I)
         HT2=D(J)-X(I+1)
         PROD=HT1*HT2
         H(8*K+J)=H(5*K+I)+HT1*H(7*K+I)
         DELSQS=(H(5*K+I)+H(5*K+1+I)+H(8*K+J))/6.
  140    D(J)=-(H(K +I)+(HT1+HT2)*DELSQS+PROD*H(7*K+I)*.1666667)
C *** SMOOTHING DIFFERENTIAL FLUX
      CALL SMOOTH(X,D,M)
      DO 160 I=1,K
      F(I)=2.718281828D0**(F(I)-ALOG(1000.))
  160 DD(I)     =D(I)*F(I)
  170 RETURN
      END
C
C *** SMOOTH DATA BY 3-POINT AVERAGING OVER EQUAL INTERVALS
      SUBROUTINE SMOOTH(X,F,M)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(30),F(30)
      FINTER(X1,X2,X3,Y1,Y2,Y3,XIN)=Y1*(XIN-X2)*(XIN-X3)/
     $((X1-X2)*(X1-X3)) + Y2*(XIN-X1)*(XIN-X3)/((X2-X1)*(X2-X3))
     $                  + Y3*(XIN-X1)*(XIN-X2)/((X3-X1)*(X3-X2))
C
      FI = F(1)
      DO 20 I=2,M
        SIZE1 = X(I) - X(I-1)
        SIZE2 = X(I+1) - X(I)
C *** CHECK FOR EQUAL STEPSIZES
        IF(DABS(SIZE1-SIZE2).LT.0.001) GO TO 200
        IF(SIZE2.GT.SIZE1) GO TO 210
C *** STEPSIZE DECREASES - FIT CURVE AND INTERPOLATE BACKWARD
        F2 = F(I+1)
        XINTER = X(I) - SIZE2
        F1 = FINTER(X(I-1),X(I),X(I+1),FI,F(I),F2,XINTER)
        GO TO 300
C *** STEPSIZE INCREASES - FIT CURVE AND INTERPOLATE FORWARD
  210   F1 = FI
        XINTER = X(I) + SIZE1
        F2 = FINTER(X(I-1),X(I),X(I+1),F1,F(I),F(I+1),XINTER)
        GO TO 300
C *** STEPSIZES ARE EQUAL - AVERAGE OVER EXISTING VALUES
  200   F1 = FI
        F2 = F(I+1)
C
C *** PERFORM AVERAGING
  300   FNEW = (F1+2.0*F(I)+F2)/4.
        FI = F(I)
        F(I) = FNEW
   20 CONTINUE
      RETURN
      END
