C
C
C
C
C LEVEL 1      FUNCTION ALNINT(A,B,X,N)
      FUNCTION ALNINT(A,B,X,N)
C
C *** ALNINT by JCM 21 May 85 ***
C
CX
CC 9C
CH Performs linear interpolation, suitable for profile backgrounds etc.
C
CA On entry:
CA     A is a real array of dimension N which holds the arguments
CA     B is a real array of dimension N which holds the function values
CA     X is a real which holds the argument for which the function value
CA       is required
CA     N is the integer dimension of A and B
CA
CA ALINT on exit will hold the function value for argument X
C
      DIMENSION A(N),B(N)
C
      DO 1 I=2,N
      IF (A(I) .GT. X) GO TO 2
   1  CONTINUE
      I=N
   2  ALNINT=(B(I-1)*(A(I)-X)-B(I)*(A(I-1)-X))/(A(I)-A(I-1))
      RETURN
      END
C
C
C
C
C LEVEL 3      FUNCTION BJ(N,X)
      FUNCTION BJ(N,X)
C
C *** BJ by JCM 17 Jan 85 ***
C
CX
CC 9C
CH Calculates Bessel functions of order 1 or 2.
CA On entry N is the order required
CA          X is the argument
CA On exit BJ holds the required Bessel function
CD Uses Harwell Library FF01A or FF02A, which have been included as
CD part of the CCSL.
C
C
      IF (N-1) 1,2,99
C
C ORDER 1:
    1 CALL FF01A(BJ,Y,X,0)
      GO TO 100
C
C ORDER 2:
    2 CALL FF02A(BJ,Y,X,0)
      GO TO 100
C
C ANY OTHER ORDER:
  99  CALL ERRIN2(N,0,'Bessel function order',' not calculated by BJ')
 100  RETURN
      END
C
C
C
C
C LEVEL 2      FUNCTION BRILL(T,TN,S)
      FUNCTION BRILL(T,TN,S)
C
C *** BRILL from PJB 14 Jun 88 ***
C
CX
CC 9C
CH Returns the value of the Brillouin function.
CA On entry T is the absolute temperature at which the function is required,
CA          TN the transition temperature,
CA          S is the spin.
CN  Uses the subprogram NB01A from the Harwell library.
C
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      IF (T.LE.0) THEN
        BRILL=1.
        GO TO 100
      ELSE IF (T.GE.TN) THEN
        BRILL=0.
        GO TO 100
      ENDIF
      K=0
      NUM=0
      B=1.
      ERR=.0001
      MAXIT=100
      X=0.002
      GO TO 1
    5 CALL NB01A(K,A,B,ERR,X,Y,MAXIT)
      GO TO (1,2,3,4) K
C
    1 S2=(2*S+1)/(2*S)
      Z=3*X*S*TN/((S+1)*T)
      Y=X-(S2/TANH(S2*Z))+(1./(TANH(Z/(2*S))*2*S))
      IF (K.EQ.1) GO TO 5
      IF (Y.LE.ERR) THEN
        X=2*X
        GO TO 1
      ENDIF
      A=X
      GO TO 5
C
    2 BRILL=X
      GO TO 100
C
    3 K=0
      B=A
      A=A/2.
      NUM=NUM+1
      IF (NUM.LE.10) GO TO 5
      I=0
   6  WRITE (ITO,3000) I,T,TN,S
      WRITE (LPT,3000) I,T,TN,S
3000  FORMAT (' ERROR **',I2,' calculating Brillouin function: ',3F8.2)
      BRILL=-1
      GO TO 100
C
    4 I=1
      GO TO 6
C
  100 RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE C1MSCA(A,B,SCALE,NI,NJ)
      SUBROUTINE C1MSCA(A,B,SCALE,NI,NJ)
C
C *** C1MSCA by PJB Apr 87 ***
C
CX
CC 12C
CH Multiplies every element of the REAL matrix A by the COMPLEX scalar SCALE.
CA On entry A is a REAL matrix of dimension NIxNJ
CA          SCALE is a COMPLEX scalar
CA On exit  B is a scaled COMPLEX matrix being SCALE times A
CN B may be the same as A
CN Does nothing if NI or NJ is zero
C
      COMPLEX B(1),SCALE
      DIMENSION A(1)
      NIJ=NI*NJ
      DO 1 I=1,NIJ
    1 B(I)=SCALE*A(I)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CGAMMA(R,X,E,G)
      SUBROUTINE CGAMMA(R,X,E,G)
C
C *** CGAMMA/CGAMMS updated by PJB/JBF Dec 89 ***
C
CX
CC 9C
CH Solves the quadratic equation for gamma in terms of the flipping ratio.
CH The ENTRY CGAMMS is for when there is spin-independent multiple scattering.
CA On entry R is the flipping ratio,
CA          X is the polarisation,
CA          E the flipping efficiency,
CA    and in CGAMMS, RMS is the ratio of multiple to nuclear scattering.
CA On exit the positive root is in G(1) and the negative one is in G(2).
CN For polarised neutron data analysis.
C
      DOUBLE PRECISION Y,Z,A
      LOGICAL SIMS,SI
C
      DIMENSION G(2)
      SIMS=.FALSE.
      GO TO 1
C
      ENTRY CGAMMS(R,X,E,G,RMS)
      SIMS=.TRUE.
C
   1  Y = DBLE(R)
      Y = (Y-1.)/((Y*E +1.)*X)
      SI=DABS(Y) .GE. .005
      IF (SIMS) SI = SI .OR. (RMS .GE. 0.005)
      IF (SI) THEN
        Y = 1./Y
        IF (DABS(Y) .LE. 1.) THEN
          G(1)=SIGN(1.,SNGL(Y))
          G(2)=G(1)
        ELSE
C
          A=Y*Y - 1.
          IF (SIMS) A=A+RMS
          Z = DSQRT(A)
          G(1) = Y + Z
          G(2) = Y - Z
        ENDIF
      ELSE
C
        IF (Y .LT. 0.) THEN
          G(2)=0.0
          G(1)=0.5*Y
        ELSE
          G(2) = 0.5*Y
** NOTE FROM JUDY - G(2) WILL BE +VE HERE ANYWAY
          G(1) = SIGN(99999.,G(2))
          IF (G(2).GT..00001) G(1)=1/G(2)
        ENDIF
      ENDIF
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CGMADD(A,B,C,NI,NJ)
      SUBROUTINE CGMADD(A,B,C,NI,NJ)
C
C *** CGMADD by JCM 20 Nov 87 ***
C
CX
CC 12C
CH Sets COMPLEX matrix C = COMPLEX matrix A + COMPLEX matrix B.
CA On entry A and B are NI by NJ COMPLEX matrices
CA On exit C is set to the sum of A and B
C
      COMPLEX A(NI,NJ),B(NI,NJ),C(NI,NJ)
      DO 1 I= 1,NI
      DO 1 J=1,NJ
   1  C(I,J) = A(I,J) + B(I,J)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CGMEQ(A,B,N,M)
      SUBROUTINE CGMEQ(A,B,N,M)
C
C *** CGMEQ by JCM ***
C
CX
CC 12C
CH Sets COMPLEX matrix B equal to COMPLEX matrix A.
CA On entry A is a COMPLEX matrix of DIMENSION N,M
CA On exit B, a COMPLEX matrix of the same total size, has been set equal to A
CN Would not give error even if N or M were zero.
      COMPLEX A(1),B(1)
      NE = N*M
      DO 1 I = 1,NE
    1 B(I) = A(I)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CGMPRD(A,B,C,I,J,K)
      SUBROUTINE CGMPRD(A,B,C,I,J,K)
C
C *** CGMPRD by JCM ***
C
CX
CC 12C
CH Multiplies together two COMPLEX matrices.
CA On entry A (of dimensions IxJ) and B (of dimensions JxK) are COMPLEX matrices
CA On exit  C (of dimensions IxK) is the product of A and B
CN C is unchanged if any of I,J or K is zero.
C
      COMPLEX A(1),B(1),C(1)
      DO 2 II = 1,I
      IK = II
      JK = 1
      DO 2 KK = 1,K
      IJ = II
      C(IK) = 0.
      DO 1 JJ = 1,J
      C(IK) = C(IK) + A(IJ)*B(JK)
      IJ = IJ + I
    1 JK = JK + 1
    2 IK = IK + I
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CGMSCA(A,B,SCALE,NI,NJ)
      SUBROUTINE CGMSCA(A,B,SCALE,NI,NJ)
C
C *** CGMSCA by PJB Apr 87 ***
C
CX
CC 12C
CH Multiplies every element of a COMPLEX matrix by a COMPLEX scale.
CA On entry, A holds a COMPLEX NI by NJ matrix
CA           SCALE holds a COMPLEX scalar scaling number
CA On exit,  B holds a COMPLEX NI by NJ matrix being A times SCALE
CN A may be the same as B
CN Note the existence of CMRSCA which works with a REAL scale
C
      COMPLEX A(1),B(1),SCALE
      NIJ=NI*NJ
      DO 1 I=1,NIJ
    1 B(I)=SCALE*A(I)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CGMSUB(A,B,C,NI,NJ)
      SUBROUTINE CGMSUB(A,B,C,NI,NJ)
C
C *** CGMSUB by PJB 20 Feb 91 ***
C
CX
CC 12C
CH Sets COMPLEX matrix C = COMPLEX matrix A - COMPLEX matrix B.
CA On entry A nnd B are NI by NJ COMPLEX matrices
CA On exit C is set to the difference of A and B
C
      COMPLEX A(NI,NJ),B(NI,NJ),C(NI,NJ)
      DO 1 I= 1,NI
      DO 1 J=1,NJ
   1  C(I,J) = A(I,J) - B(I,J)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CGMUNI(A,NI)
      SUBROUTINE CGMUNI(A,NI)
C
C *** CGMUNI modified by PJB c17 17 sept 93 ***
C
CX
CC 12C
CH Clears a COMPLEX square matrix to contain the unit matrix.
CA On exit A is an NIxNI unit matrix
CN NI must be at least 1
C
      COMPLEX A(NI,NI)
      DATA C0,C1/0.,1./
C
      DO 1 I=1,NI
      A(I,I)=CMPLX(C1,C0)
      I1=I+1
      IF (I1 .GT. NI) GO TO 100
      DO 1 J=I1,NI
      A(I,J)=CMPLX(C0,C0)
   1  A(J,I)=A(J,I)
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CGMZER(A,NI,NJ)
      SUBROUTINE CGMZER(A,NI,NJ)
C
C *** CGMZER by JCM 23 Oct 87 ***
C
CX
CC 12C
CH Clears to zero a complex matrix.
CA On entry A is an NI by NJ COMPLEX matrix
CA On exit A is cleared to COMPLEX zero
C
      COMPLEX A(1)
      NIJ=NI*NJ
      DO 1 I=1,NIJ
   1  A(I)=CMPLX(0.,0.)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CMCONJ(A,B,I,J)
      SUBROUTINE CMCONJ(A,B,I,J)
C
C *** CMCONJ by PJB Nov 89 ***
C
CX
CC 12C
CH Gives the conjugate of a COMPLEX matrix.
CA On entry A is a complex I by J matrix
CA On exit the I by J complex matrix B contains conjugate complex of A
C
      COMPLEX A(I,J), B(I,J)
      DO 1 II=1,I
      DO 2 JJ=1,J
      B(II,JJ)=CONJG(A(II,JJ))
    2 CONTINUE
    1 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CMIMAG(A,B,I,J)
      SUBROUTINE CMIMAG(A,B,I,J)
C
C *** CMIMAG by PJB Nov 89 ***
C
CX
CC 12C
CH Gives the imaginary parts of a COMPLEX matrix.
CA On entry A is a complex I by J matrix
CA On exit the I by J real matrix B contains the imaginary parts of A
C
      COMPLEX A(I,J)
      DIMENSION B(I,J)
      DO 1 II=1,I
      DO 2 JJ=1,J
      B(II,JJ)=AIMAG(A(II,JJ))
    2 CONTINUE
    1 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CMREAL(A,B,I,J)
      SUBROUTINE CMREAL(A,B,I,J)
C
C *** CMREAL by PJB Nov 89 ***
C
CX
CC 12C
CH Gives the real parts of a COMPLEX matrix.
CA On entry A is a complex I by J matrix
CA On exit the I by J real matrix B contains the real parts of A
C
      COMPLEX A(I,J)
      DIMENSION B(I,J)
      DO 1 II=1,I
      DO 2 JJ=1,J
      B(II,JJ)=REAL(A(II,JJ))
    2 CONTINUE
    1 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CMRSCA(A,B,SCALE,NI,NJ)
      SUBROUTINE CMRSCA(A,B,SCALE,NI,NJ)
C
C *** CMRSCA by JCM 23 Aug 88 ***
C
CX
CC 12C
CH Multiplies every element of a COMPLEX matrix by a REAL scale.
CA On entry, A holds a COMPLEX NI by NJ matrix
CA           SCALE holds a REAL scalar scaling number
CA On exit,  B holds a COMPLEX NI by NJ matrix being A times SCALE
CN A may be the same as B
CN Note also the existence of CGMSCA which works entirely in COMPLEX.
C
      COMPLEX A(1),B(1)
      NIJ=NI*NJ
      DO 1 I=1,NIJ
    1 B(I)=SCALE*A(I)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE CRMPRD(A,B,C,I,J,K)
      SUBROUTINE CRMPRD(A,B,C,I,J,K)
C
C *** CRMPRD by PJB Nov 89 ***
C
CX
CC 12C
CH Performs COMPLEX by REAL matrix multiplication.
CA On entry A (of dimensions IxJ) is a COMPLEX matrix
CA          B (of dimensions JxK) is a REAL matrix
CA On exit  C (of dimensions IxK and COMPLEX) is the product of A and B.
CN C is unchanged if any of I,J or K is zero.
CN Note also the existence of SUBROUTINE RCMPRD which multiplies them
CN the other way round.
C
      DIMENSION B(1)
      COMPLEX A(1),C(1)
      DO 2 II = 1,I
      IK = II
      JK = 1
      DO 2 KK = 1,K
      IJ = II
      C(IK) = 0.
      DO 1 JJ = 1,J
      C(IK) = C(IK) + A(IJ)*B(JK)
      IJ = IJ + I
    1 JK = JK + 1
    2 IK = IK + I
      RETURN
      END
C
C
C
C
C LEVEL 1      COMPLEX FUNCTION CRSCLP(A,B)
      COMPLEX FUNCTION CRSCLP(A,B)
C
C *** CRSCLP by PJB Nov 89 ***
C
CX
CC 12C
CH Finds the COMPLEX scalar product of a COMPLEX with a REAL vector.
CA On entry A is a complex vector of dimension 3
CA          B is a real vector of dimension 3 in a space orthogonal to
CA            that of A
CD On exit the function value is the scalar product of A and B
C
      COMPLEX A(3),C
      DIMENSION  B(3)
      C=CMPLX(0.,0.)
      DO 1 I=1,3
      C=C+A(I)*B(I)
    1 CONTINUE
      CRSCLP=C
      RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION DEGREE(X)
      FUNCTION DEGREE(X)
C
C *** DEGREE by JCM ***
C
CX
CC 10C
CH Converts from radians to degrees.
CA On entry X is an angle in radians
CA On exit DEGREE is the angle in degrees
C
CN The function RADIAN(X) does the degrees to radians conversion.
CN In routines where time matters, it is quicker to declare the COMMON
CN /CONSTA/ and multiply X by DEG.
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
C
      DEGREE = DEG*X
      RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION DETER3(A)
      FUNCTION DETER3(A)
C
C *** DETER3 by JCM 28 Jun 83 ***
C
CX
CC 12C
CH Forms the determinant of a 3 x 3 matrix.
CA On entry A is a 3x3 real matrix.
CA On exit DETER3 holds its determinant.
C
      DIMENSION A(3,3)
      D=0.
      J=2
      K=3
      DO 1 I=1,3
      D=D+A(1,I)*(A(2,J)*A(3,K)-A(2,K)*A(3,J))
      J=K
   1  K=I
      DETER3=D
      RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION ERFNC(X)
      FUNCTION ERFNC(X)
C
C *** ERFNC by WIFD 22 Aug 85 ***
C
CX
CC 9C
CH Calculates the error function accurate to 3E-7, for + and - X.
CA On entry X is the argument at which the function is required.
CA On exit ERFNC holds the function.
CD See Abramovitz and Stegun p.299
C
      DATA A1,A2,A3/0.0705230784,0.0422820123,0.0092705272/
      DATA A4,A5,A6/0.0001520143,0.0002765672,0.0000430638/
C  THESE COEFFICIENTS WERE TAKEN FROM ABRAMOVITZ AND STEGUN P.299
C  AND GIVE ERFNC ACCURATE TO 3E-07 FOR BOTH +VE AND -VE X.
C
      Z= ABS(X)
      ZZ=Z*Z
      ZZZ=Z*Z*Z
      E= 1.0 +A1*Z +A2*ZZ +A3*ZZZ +A4*ZZ*ZZ +A5*ZZ*ZZZ +A6*ZZZ*ZZZ
      IF(E.LT.10.0) GO TO 1
      E=0.0
      GO TO 2
   1  E=1.0/E**16
      IF (E.LE.0.0) E=0.0
   2  IF (X.LT.0.0) E=2.0-E
C
      ERFNC=E
      RETURN
      END
C
C
C
C
C LEVEL 4      FUNCTION EXPINT(A,P,N,L)
      FUNCTION EXPINT(A,P,N,L)
C
C *** EXPINT by PJB Dec 84 ***
C
CX
CC 9C
CH Calculates an exponential radial integral.
CA On entry A, P N and L are set up for the routine to calculate the integral
CA between 0 and infinity of the Lth order spherical Bessel function of
CA A*X times X**N*exp(-P*X)
C
CD Uses hypergeometric series.
CN Used to calculate form-factors from Slater type wave-functions
      DIMENSION B(4)
      B(1) = FLOAT(N+L+3)
      B(2) = 0.5*FLOAT(L-N-1)
      B(3) = FLOAT(L) + 1.5
      EX = 2.0
      GAM = B(3)-1.0
      L1=L+1
      DO 5 I=1,L1
      EX = EX*GAM
   5  GAM = GAM-1.0
C
      EX = FACT(N+L+2)/EX
      IF (L .NE. 0) THEN
        X = (0.5*A)**L
        EX = EX*X
      ENDIF
      X = A*A + P*P
      B(1) = 0.5*B(1)
      EX = EX/(X**B(1))
      X = A*A/X
C
C     HYPERGEOMETRIC SERIES
      SUM = 1.
      TERM = 1.
      B(4) = 1.
    3 T = TERM
      TERM = TERM*B(1)*B(2)*X/(B(3)*B(4))
      SUM = SUM+ TERM
      E = ABS(TERM/SUM)
      IF (E .GT. .001) THEN
        DO 2 I = 1,4
    2   B(I) = B(I) + 1.0
        GO TO 3
      ENDIF
C
      EXPINT = EX*SUM
      RETURN
      END
C
C
C
C
C LEVEL 3      FUNCTION FACT(K)
      FUNCTION FACT(K)
C
C *** FACT by PJB 19 Jan 85 ***
C
CX
CC 9C
CH Calculates factorial K.
CA On entry K holds an integer which is positive or zero.
CA On exit FACT holds factorial K
C
C
      FAC = 1.
      IF (K) 1,101,2
   1  CALL ERRIN2(K,1,'negative argument for factorial',' ')
C
    2 DO 3 I = 1,K
    3 FAC = FAC*FLOAT(I)
 101  FACT=FAC
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE FF01A(VJS,VYS,XS,N)
      SUBROUTINE FF01A(VJS,VYS,XS,N)
C
C *** FF01A from HARWELL LIBRARY modified by JCM 17 Jan 85 ***
C
CX
CC 9C
CH Modified Harwell routine for zero order Bessel functions.
C
      DOUBLE PRECISION A(18),B(19),C(18),D(18)
      DOUBLE PRECISION XLG
      DOUBLE PRECISION VJ0,VY0,X,Y,Z,Q1,Q2,Q3,FX,X1,X2,X3,X4
      DOUBLE PRECISION ABCD(73)
      EQUIVALENCE (ABCD(1),A(1)),(ABCD(19),B(1)),(ABCD(38),C(1))
      EQUIVALENCE (ABCD(56),D(1))
      DATA A/
     &   -.17D-18                  , .1222D-16             ,
     &   -.75885D-15               , .4125321D-13          ,
     &   -.194383469D-11           , .7848696314D-10       ,
     &   -.267925353056D-8         , .7608163592419D-7     ,
     &   -.176194690776215D-5      , .3246032882100508D-4  ,
     &   -.4606261662062751D-3    , .4819180069467605D-2 ,
     &   -.3489376941140889D-1    , .1580671023320973D0  ,
     &   -.3700949938726498D0     , .2651786132033368D0  ,
     &   -.8723442352852221D-2    , .3154559429497802D0  /
      DATA B/
     &   -.1D-19                   , .39D-18               ,
     &   -.2698D-16                , .164349D-14           ,
     &   -.8747341D-13             , .402633082D-11        ,
     &   -.15837552542D-9          , .524879478733D-8      ,
     &   -.14407233274019D-6       , .32065325376548D-5    ,
     &   -.5632079141056987D-4     , .7531135932577742D-3 ,
     &   -.7287962479552079D-2    , .4719668959576339D-1 ,
     &   -.1773020127811436D0     , .2615673462550466D0  ,
     &    .179034314077183D0     ,-.2744743055297453D0  ,
     &   -.6629222640656988D-1     /
      DATA C/
     &   -.1D-19                   , .2D-19                ,
     &   -.11D-18                  , .55D-18               ,
     &   -.288D-17                 , .1631D-16             ,
     &   -.10012D-15               , .67481D-15            ,
     &   -.506903D-14              , .4326596D-13          ,
     &   -.43045789D-12            , .516826239D-11        ,
     &   -.7864091377D-10          , .163064646352D-8      ,
     &   -.5170594537606D-7        , .307518478751947D-5   ,
     &   -.5365220468132117D-3    , .1998920698695037D1 /
      DATA D/
     &    .1D-19                   ,-.3D-19                ,
     &    .13D-18                  ,-.62D-18               ,
     &    .311D-17                 ,-.1669D-16             ,
     &    .9662D-16                ,-.60999D-15            ,
     &    .425523D-14              ,-.3336328D-13          ,
     &    .30061451D-12            ,-.320674742D-11        ,
     &    .4220121905D-10          ,-.72719159369D-9       ,
     &    .1797245724797D-7        ,-.74144984110606D-6    ,
     &    .683851994261165D-4      ,-.3111170921067402D-1 /
C
CVMS
      DATA XLG/1.7D+38/
C3084      XLG=(1.0D0 - 16.0D0**(-14)) * 16.0D0**(+63)
      X=DBLE(XS)
      Y=DABS(X)
      Z=Y*.125D0
      IF (Z .GT.1D0) GO TO 1
      IF (Z .EQ. 0D0) GO TO  2
      X2=4D0*Z*Z-2D0
      JUMP=1
      N1=1
      N2=18
   3  Q3=0D0
      Q2=0D0
      DO 4  I=N1,N2
      Q1=Q2
      Q2=Q3
   4  Q3=X2*Q2-Q1+ABCD(I)
      FX=(Q3-Q1)*.5D0
      GO TO (10,11,12,13),JUMP
  10  VJ0=FX
      VJS=SNGL(VJ0)
      IF (N .LE. 0) GO TO 100
      JUMP=2
      N1=19
      N2=37
      GO TO 3
C
   2  VJ0=1D0
      VY0=-XLG
      GO TO 101
C
  11  VY0=.6366197723675813D0*DLOG(Y)*VJ0+FX
      VYS=SNGL(VY0)
      GO TO 100
C
   1  Z=1D0/Z
      X2=4D0*Z*Z-2D0
      JUMP=3
      N1=38
      N2=55
      GO TO 3
C
  12  X1=FX
      JUMP=4
      N1=56
      N2=73
      GO TO 3
C
  13  X2=DCOS(Y-.7853981633974483D0)
      X3=DSIN(Y-.7853981633974483D0)
      X4=.7978845608028654D0/DSQRT(Y)
      FX=FX*Z
      VJ0=X4*(X1*X2-FX*X3)
      VY0=X4*(FX*X2+X1*X3)
 101  VJS=SNGL(VJ0)
      VYS=SNGL(VY0)
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE FF02A(VJS,VYS,S,N)
      SUBROUTINE FF02A(VJS,VYS,S,N)
C
C *** FF02A from HARWELL LIBRARY modified by JCM 17 Jan 85 ***
C
CX
CC 9C
CH Modified Harwell routine for first order Bessel functions.
C
      DOUBLE PRECISION A(18),B(18),C(18),D(18),ABCD(72),XLG
      DOUBLE PRECISION VJ1,VY1,X,Y,Z,Q1,Q2,Q3,FX,X1,X2,X3,X4
      EQUIVALENCE (ABCD(1),A(1)),(ABCD(19),B(1)),(ABCD(37),C(1)),
     & (ABCD(55),D(1))
      DATA A/
     &   -.4D-19                   , .295D-17              ,
     &   -.19554D-15               , .1138572D-13          ,
     &   -.57774042D-12            , .2528123664D-10       ,
     &   -.94242129816D-9          , .2949707007278D-7     ,
     &   -.76175878054003D-6       , .1588701923993213D-4  ,
     &   -.2604443893485807D-3    , .3240270182683858D-2 ,
     &   -.2917552480615421D-1    , .1777091172397283D0  ,
     &   -.6614439341345433D0     , .1287994098857678D1  ,
     &   -.1191801160541217D1     , .1296717541210530D1  /
      DATA B/
     &    .9D-19                   ,-.658D-17              ,
     B    .42773D-15               ,-.2440949D-13          ,
     C    .121143321D-11           ,-.5172121473D-10       ,
     D    .187547032473D-8         ,-.5688440039919D-7     ,
     E    .141662436449235D-5      ,-.283046401495148D-4   ,
     F    .4404786298670995D-3    ,-.5131641161061085D-2 ,
     G    .4231918035333690D-1    ,-.2266249915567549D0  ,
     H    .6756157807721877D0     ,-.7672963628866459D0  ,
     I   -.1286973843813500D0     , .4060821177186851D-1 /
      DATA C/
     J    .1D-19                   ,-.2D-19                ,
     K    .12D-18                  ,-.58D-18               ,
     L    .305D-17                 ,-.1731D-16             ,
     M    .10668D-15               ,-.72212D-15            ,
     N    .545267D-14              ,-.4684224D-13          ,
     O    .46991955D-12            ,-.570486364D-11        ,
     P    .881689866D-10           ,-.187189074911D-8      ,
     Q    .6177633960644D-7        ,-.398728430048891D-5   ,
     R    .8989898330859409D-3    , .2001806081720027D1  /
      DATA D/
     S   -.1D-19                   , .3D-19                ,
     T   -.14D-18                  , .65D-18               ,
     U   -.328D-17                 , .1768D-16             ,
     V   -.10269D-15               , .65083D-15            ,
     W   -.456125D-14              , .3596777D-13          ,
     X   -.32643157D-12            , .351521879D-11        ,
     Y   -.4686363688D-10          , .82291933277D-9       ,
     Z   -.2095978138408D-7        , .91386152579555D-6    ,
     &    -.9627723549157079D-4     , .9355557413907065D-1 /
C
CVMS
      DATA XLG/1.7D+38/
C3084      XLG=(1.0D0 - 16.0D0**(-14)) * 16.0D0**(+63)
      X=DBLE(S)
      Y=DABS(X)
      Z=Y*.125D0
      IF (Z.GT.1D0) GO TO 1
      IF (Z .LE. 0D0) GO TO 2
      X2=4D0*Z*Z-2D0
      JUMP=1
      N1=1
      N2=18
   3  Q3=0D0
      Q2=0D0
      DO 4 I=N1,N2
      Q1=Q2
      Q2=Q3
      Q3=X2*Q2-Q1+ABCD(I)
   4  CONTINUE
      FX=(Q3-Q1)*.5D0
      GO TO (10,11,12,13),JUMP
C
  10  VJ1=FX*Z
      VJS=SNGL(VJ1)
      IF(N.LE.0)GO TO 100
      JUMP=2
      N1=19
      N2=36
      GO TO 3
   2  VJ1=0D0
      VY1=-XLG
      GO TO 101
C
  11  VY1=.6366197723675813D0*(DLOG(Y)*VJ1-1D0/Y)+FX*Z
      VYS=SNGL(VY1)
      GO TO 100
C
   1  Z=1D0/Z
      X2=4D0*Z*Z-2D0
      JUMP=3
      N1=37
      N2=54
      GO TO 3
C
  12  X1=FX
      JUMP=4
      N1=55
      N2=72
      GO TO 3
C
  13  X2=DCOS(Y-2.356194490192345D0)
      X3=DSIN(Y-2.356194490192345D0)
      X4=.7978845608028654D0/DSQRT(Y)
      FX=FX*Z
      VJ1=X4*(X1*X2-FX*X3)
      VY1=X4*(FX*X2+X1*X3)
 101  VJS=SNGL(VJ1)
      VYS=SNGL(VY1)
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE FRAC3(VEC)
      SUBROUTINE FRAC3(VEC)
C
C *** FRAC3 by JCM 11 Nov 83 ***
C
CX
CC 1C
CH Makes all 3 elements of a vector fractional
CA On entry VEC is a 1x3 real vector.
CA On exit the elements of VEC have each been put into the range 0 =< X < 1
C
      DIMENSION VEC(3)
C
      DO 1 I=1,3
   1  CALL FRACT(VEC(I),A,J)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE FRACT(X,Y,N)
      SUBROUTINE FRACT(X,Y,N)
C
C *** FRACT by JCM ***
C
CX
CC 9C
CH Forms the fractional part of a real number.
CA On entry X is a real number
CA On exit  X is in the range 0=< X <1
CA          Y is set so that X+Y=original X
CA          N= 0 if X was unchanged
CA           = 1 if X was >= 1
CA           =-1 if X was < 0
C
      Y=0.
      N=1
      IF (X .GE. 1.) GO TO 1
      N=0
      IF (X .GE. 0.) GO TO 100
C OUT IN USUAL CASE OF X ALREADY BEING +VE FRACTION
C
      N=-1
   1  Y=AINT(X)
      IF (N .EQ. -1) Y=Y-1.0
      X=X-Y
 100  RETURN
      END
C
C
C
C
C LEVEL 1    SUBROUTINE FT01A(IT,INV,TR,TI)
      SUBROUTINE FT01A(IT,INV,TR,TI)
C
C *** FT01A updated by JCM FROM HARWELL ROUTINE 9 Sep 91 ***
C
CX
CC 9C
CH Modification of Harwell Fast Fourier Transform.
C
C%
C      DIMENSION TR(%FFT2%),TI(%FFT2%)
      DIMENSION TR(1024),TI(1024)
      EXTERNAL FFTADD
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
      COMMON /FFTDA/KJUMP,UR(15),UI(15)
C
      GO TO (1,2),KJUMP
   1  UM=0.5
      DO 6 I=1,15
      UM=0.5*UM
      TH=TWOPI*UM
      UR(I)=COS(TH)
   6  UI(I)=SIN(TH)
      KJUMP=2
C
C SECOND AND SUBSEQUENT ENTRIES:
   2  UM=1.
      IF (INV .EQ. 1) UM=-1.
      IO=2
      DO 3 I=2,16
      IO=IO+IO
      IF(IO-IT)3,4,99
   3  CONTINUE
C ERROR EXIT - IT NOT A POWER OF 2, OR TOO BIG:
  99  INV=-1
      GO TO 100
C
   4  IO=I
      II=IO
      I1=IT/2
      I3=1
  10  K=0
      I2=I1+I1
  11  WR=1.
      WI=0.
      KK=K
      JO=IO
C
  12  IF (KK .EQ. 0) GO TO 13
  14  JO=JO-1
      KK1=KK
      KK=KK/2
      IF (KK1 .EQ. 2*KK) GO TO 14
      WS=WR*UR(JO)-WI*UI(JO)
      WI=WR*UI(JO)+WI*UR(JO)
      WR=WS
      GO TO 12
C
  13  WI=WI*UM
      J=0
C
   9  L=J*I2+K
      L1=L+I1
      ZR=TR(L+1)+TR(L1+1)
      ZI=TI(L+1)+TI(L1+1)
      Z=WR*(TR(L+1)-TR(L1+1))-WI*(TI(L+1)-TI(L1+1))
      TI(L1+1)=WR*(TI(L+1)-TI(L1+1))+WI*(TR(L+1)-TR(L1+1))
      TR(L+1)=ZR
      TR(L1+1)=Z
      TI(L+1)=ZI
      J=J+1
      IF (J.LT. I3) GO TO 9
      K=K+1
      IF (K .LT. I1) GO TO 11
      I3=I3+I3
      IO=IO-1
      I1=I1/2
      IF (I1 .GT. 0) GO TO 10
      J=1
      UM=1.
      IF (INV .EQ. 1) UM=1./FLOAT(IT)
C
   7  K=0
      J1=J
      DO 8 I=1,II
      J2=J1/2
      K=2*(K-J2)+J1
   8  J1=J2
C
      IF (K .GE. J) THEN
        IF (K .EQ. J) THEN
          TR(J+1)=TR(J+1)*UM
          TI(J+1)=TI(J+1)*UM
        ELSE
          ZR=TR(J+1)
          ZI=TI(J+1)
          TR(J+1)=TR(K+1)*UM
          TI(J+1)=TI(K+1)*UM
          TR(K+1)=ZR*UM
          TI(K+1)=ZI*UM
        ENDIF
      ENDIF
      J=J+1
      IF (J .LT. IT-1) GO TO 7
      TR(1)=TR(1)*UM
      TI(1)=TI(1)*UM
      TR(IT)=TR(IT)*UM
      TI(IT)=TI(IT)*UM
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE GMADD(A,B,C,NI,NJ)
      SUBROUTINE GMADD(A,B,C,NI,NJ)
C
C *** GMADD by JCM ***
C
CX
CC 12C
CH Sets matrix C = matrix A plus matrix B.
CA On entry A and B are real matrices of dimension (NIxNJ)
CA On exit  C is a real matrix which is their sum.
CN NI and NJ must be at least 1
C
      DIMENSION A(NI,NJ),B(NI,NJ),C(NI,NJ)
      DO 1 I= 1,NI
      DO 1 J=1,NJ
   1  C(I,J) = A(I,J) + B(I,J)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE GMEQ(A,B,NI,NJ)
      SUBROUTINE GMEQ(A,B,NI,NJ)
C
C *** GMEQ by PJB/JCM 28 Jun 83 ***
C
CX
CC 12C
CH Sets matrix B = matrix A.
CA On entry A is a real matrix of dimension NIxNJ
CA On exit  B is a real matrix equal to A
CN NI and NJ must be at least 1
C
      DIMENSION A(NI,NJ),B(NI,NJ)
      DO 1 I=1,NI
      DO 1 J=1,NJ
    1 B(I,J)=A(I,J)
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE GMINV(A,B,N)
      SUBROUTINE GMINV(A,B,N)
C
C *** GMINV by JCM from SID 11 Oct 88 ***
C
CX
CC 12C
CH Inverts matrix A into matrix B.
CA On entry A is a square NxN real matrix
CA On exit  B is its inverse
CD Based on SID
C
      DIMENSION II(100),IL(100),IG(100),A(N,N),B(N,N)
C
      CALL GMEQ(A,B,N,N)
      D=1.
      IS=N-1
      DO 10 K=1,N
      IL(K)=0
   10 IG(K)=K
C
      DO 150 K=1,N
      R=0.
      DO 40 I=1,N
      IF (IL(I) .NE. 0) GO TO 40
      W=B(I,K)
      X=ABS(W)
      IF (R .GT. X) GO TO 40
      R=X
      P=W
      KF=I
   40 CONTINUE
      II(K)=KF
      IL(KF)=KF
      D=D*P
      IF (D .EQ. 0.) CALL ERRMES(1,0,'Zero determinant')
C
      DO 80 I=1,N
      IF (I .EQ. KF) THEN
      B(I,K)=1./P
      ELSE
      B(I,K)=-B(I,K)/P
      ENDIF
   80 CONTINUE
C
      DO 140 J=1,N
      IF (J .EQ. K) GO TO 140
      W=B(KF,J)
      IF (W .EQ. 0.) GO TO 140
      DO 130 I=1,N
      IF (I .EQ. KF) THEN
      B(I,J)=W/P
      ELSE
      B(I,J)=B(I,J)+W*B(I,K)
      ENDIF
  130 CONTINUE
  140 CONTINUE
C
  150 CONTINUE
C.....
C
      DO 190 K=1,IS
      KF=II(K)
      KL=IL(KF)
      KG=IG(K)
      IF(KF .EQ. KG) GO TO 190
      DO 170 I=1,N
      R=B(I,KF)
      B(I,KF)=B(I,KG)
  170 B(I,KG)=R
      DO 180 J=1,N
      R=B(K,J)
      B(K,J)=B(KL,J)
  180 B(KL,J)=R
      IL(KF)=K
      IL(KG)=KL
      IG(KL)=IG(K)
      IG(K)=KF
      D=-D
  190 CONTINUE
      RETURN
      END
C
C
C
C
C
C LEVEL 2       SUBROUTINE GMNORM(A,B,II,JJ)
       SUBROUTINE GMNORM(A,B,II,JJ)
C
C *** GMNORM by PJB ***
C
CX
CC 12C
CH Normalises the rows of a matrix.
CA On entry A is an IIxJJ real matrix
CA On exit A has been normalised using B, A(I,J) out = A(I,J) in / B(J)
CA         B holds the normalising coefficients
C
      DIMENSION A(II,JJ),B(JJ)
C
      DO 1 J=1,JJ
      CALL GMPRD(A(1,J),A(1,J),B(J),1,II,1)
      IF (B(J).GT..0001) THEN
        B(J)=SQRT(B(J))
        ANORM=1./B(J)
        CALL GMSCA(A(1,J),A(1,J),ANORM,II,1)
      ENDIF
    1 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE GMPRD(A,B,C,NI,NJ,NK)
      SUBROUTINE GMPRD(A,B,C,NI,NJ,NK)
C
C *** GMPRD by JCM ***
C
CX
CC 12C
CH Sets matrix C = matrix A times matrix B.
CA On entry A is a real NIxNJ matrix
CA          B is a real NJxNK matrix
CA On exit  C is a real NIxNK matrix holding A times B
C
      DIMENSION A(1),B(1),C(1)
      DO 2 I = 1,NI
      IK = I
      JK = 1
      DO 2 K = 1,NK
      IJ = I
      C(IK) = 0.
      DO 1 J = 1,NJ
      C(IK) = C(IK) + A(IJ)*B(JK)
      IJ = IJ + NI
    1 JK = JK + 1
    2 IK = IK + NI
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE GMREV(A,B,NI,NJ)
      SUBROUTINE GMREV(A,B,NI,NJ)
C
C *** GMREV by PJB/JCM 28 Jun 83 ***
C
CX
CC 12C
CH Reverses the signs of the elements of an NI X NJ matrix.
CA On entry A is a real matrix of dimension NIxNJ
CA On exit  B is a real matrix holding -A
CN A and B may be the same matrix
C
      DIMENSION A(NI,NJ),B(NI,NJ)
      DO 1 I=1,NI
      DO 1 J=1,NJ
    1 B(I,J)=-A(I,J)
      RETURN
      END
C
C
C
C
C LEVEL 1      LOGICAL FUNCTION GMSAME(A,B,N,TOLER)
      LOGICAL FUNCTION GMSAME(A,B,N,TOLER)
C
C *** GMSAME by JCM 22 Oct 86 ***
C
CX
CC 11C
CH Tells whether one vector is the same as another, to a given tolerance.
C
CA A on entry is an N-sized array, to be compared with:
CA B, also an N-sized array (A and B may of course be parts of larger arrays)
CA N on entry is the number of elements of A and B to compare
CA TOLER on entry is the number within which all the elements of A and B
CA      must agree
CA
CA GMSAME will be set .TRUE. if all elements of A and B agree within TOLER,
CA      and .FALSE. otherwise
C
      DIMENSION A(N),B(N)
C
      GMSAME=.FALSE.
      DO 1 I=1,N
      IF (ABS(A(I)-B(I)) .GT. TOLER) GO TO 100
   1  CONTINUE
      GMSAME=.TRUE.
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE GMSCA(A,B,SCALE,NI,NJ)
      SUBROUTINE GMSCA(A,B,SCALE,NI,NJ)
C
C *** GMSCA by JCM 22 Nov 84 ***
C
CX
CC 12C
CH Multiplies every element of the matrix A by the scalar SCALE.
CA On entry A is a real matrix of dimension NIxNJ
CA          SCALE is the required multiplying scale
CA On exit  B is a real matrix whose elements are those of A times SCALE.
CN A and B may be the same matrix
C
      DIMENSION A(1),B(1)
      NIJ=NI*NJ
      DO 1 I=1,NIJ
    1 B(I)=SCALE*A(I)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE GMSUB(A,B,C,NI,NJ)
      SUBROUTINE GMSUB(A,B,C,NI,NJ)
C
C *** GMSUB by PJB/JCM 28 Jun 83 ***
C
CX
CC 12C
CH Sets matrix C = matrix A minus matrix B.
CA On entry A and B are real matrices of dimension NIxNJ
CA On exit  C is a real matrix whose elements are those of A-B
C
      DIMENSION A(1),B(1),C(1)
      NIJ=NI*NJ
      DO 1 I= 1,NIJ
   1  C(I) = A(I) - B(I)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE GMTRAN(A,B,JJ,II)
      SUBROUTINE GMTRAN(A,B,JJ,II)
C
C *** GMTRAN by PJB ***
C
CX
CC 12C
CH Transposes a JJxII matrix A into B.
CA On entry A is a real matrix of dimension (JJxII)
CA On exit  B is a real matrix of dimension (IIxJJ) which is the transpose of A
C
      DIMENSION A(JJ,II),B(II,JJ)
C
      DO 1 J=1,JJ
      DO 1 I=1,II
      B(I,J)=A(J,I)
    1 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE GMUNI(A,NI)
      SUBROUTINE GMUNI(A,NI)
C
C *** GMUNI by JCM 7 Jul 83 ***
C
CX
CC 12C
CH Writes a unit matrix into the square matrix A.
CA On entry NI is the dimension of the required matrix
CA On exit  A is a square real matrix of dimension NIxNI, set to a unit matrix
C
      DIMENSION A(NI,NI)
      DO 1 I=1,NI
      A(I,I)=1.
      I1=I+1
      IF (I1 .GT. NI) GO TO 100
      DO 1 J=I1,NI
      A(I,J)=0.
   1  A(J,I)=0.
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE GMZER(A,NI,NJ)
      SUBROUTINE GMZER(A,NI,NJ)
C
C *** GMZER by JCM 7 Jul 83 ***
C
CX
CC 12C
CH Clears to zero the matrix A.
CA On entry NI and NJ specify A to be of dimension NIxNJ
CA On exit  A is a real matrix of dimension NIxNJ all cleared to zero.
C
      DIMENSION A(1)
      NIJ=NI*NJ
      DO 1 I=1,NIJ
   1  A(I)=0.
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE JGMADD(JA,JB,JC,NI,NJ)
      SUBROUTINE JGMADD(JA,JB,JC,NI,NJ)
C
C *** JGMADD by JCM Jun 88 ***
C
CX
CC 12C
CH In integers, sets matrix C = matrix A plus matrix B.
CA On entry JA and JB are integer matrices both of dimension NIxNJ
CA On exit  JC=JA+JB also NIxNJ
C
      DIMENSION JA(NI,NJ),JB(NI,NJ),JC(NI,NJ)
      DO 1 I= 1,NI
      DO 1 J=1,NJ
   1  JC(I,J) = JA(I,J) + JB(I,J)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE JGMEQ(JA,JB,NI,NJ)
      SUBROUTINE JGMEQ(JA,JB,NI,NJ)
C
C *** JGMEQ by JCM  Jun 88 ***
C
CX
CC 12C
CH Equates an integer matrix to a given integer matrix.
CA On entry JA is an integer matrix of dimension NIxNJ
CA On exit  JB = integer matrix being a copy of JA
C
      DIMENSION JA(NI,NJ),JB(NI,NJ)
      DO 1 I=1,NI
      DO 1 J=1,NJ
    1 JB(I,J)=JA(I,J)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE JGMPRD(JA,JB,JC,NI,NJ,NK)
      SUBROUTINE JGMPRD(JA,JB,JC,NI,NJ,NK)
C
C *** JGMPRD by JCM Jun 88 ***
C
CX
CC 12C
CH In integers, sets matrix C = matrix A times matrix B.
CA On entry JA is an integer matrix of dimension NIxNJ
CA          JB is an integer matrix of dimension NJxNK
CA On exit  JC is an integer matrix of dimension NIxNK, being JA times JB
C
      DIMENSION JA(1),JB(1),JC(1)
      DO 2 I = 1,NI
      IK = I
      JK = 1
      DO 2 K = 1,NK
      IJ = I
      JC(IK) = 0.
      DO 1 J = 1,NJ
      JC(IK) = JC(IK) + JA(IJ)*JB(JK)
      IJ = IJ + NI
    1 JK = JK + 1
    2 IK = IK + NI
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE JGMREV(JA,JB,NI,NJ)
      SUBROUTINE JGMREV(JA,JB,NI,NJ)
C
C *** JGMREV by JCM Jun 88 ***
C
CX
CC 12C
CH Reverses the signs of the elements of an integer matrix.
CA On entry JA is an integer matrix of dimension NIxNJ
CA On exit  JB is an integer matrix, being a copy of JA with reversed signs.
CN JB may be the same as JA.
C
      DIMENSION JA(NI,NJ),JB(NI,NJ)
      DO 1 I=1,NI
      DO 1 J=1,NJ
    1 JB(I,J)=-JA(I,J)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE JGMSUB(JA,JB,JC,NI,NJ)
      SUBROUTINE JGMSUB(JA,JB,JC,NI,NJ)
C
C *** JGMSUB by JCM Jun 88 ***
C
CX
CC 12C
CH In integers, sets matrix C = matrix A minus matrix B.
CA On entry JA and JB are integer matrices of dimension NIxNJ
CA On exit  JC is an integer matrix = JA - JB
C
      DIMENSION JA(1),JB(1),JC(1)
      NIJ=NI*NJ
      DO 1 I= 1,NIJ
   1  JC(I) = JA(I) - JB(I)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE JGMZER(JA,NI,NJ)
      SUBROUTINE JGMZER(JA,NI,NJ)
C
C *** JGMZER by JCM Jun 88 ***
C
CX
CC 12C
CH Clears an integer matrix to zero.
CA On entry NI and NJ are the dimensions of the required matrix
CA On exit  the integer matrix JA, of dimensions NIxNJ is cleared to zero.
C
      DIMENSION JA(1)
      NIJ=NI*NJ
      DO 1 I=1,NIJ
   1  JA(I)=0
      RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE JTERMS(G,R,H,S)
      SUBROUTINE JTERMS(G,R,H,S)
C
C *** JTERMS by PJB 17 Jan 85 ***
C
CX
CC 9C
CH Calculates the terms in the 2D averaged spherically symmetric form factor
CH summation which depend on S,K, and R only.
C
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
C
C     TEST FOR SPECIAL CASES:
      IF (H .LT .001) GO TO 1
      IF (S .LT. .001) GO TO 2
      IF (ABS(H-S) .LT. .001) GO TO 3
      G = H*BJ(1,H*R)*BJ(0,S*R) - S*BJ(1,S*R)*BJ(0,H*R)
      G = TWOPI*G*R/(H*H-S*S)
      GO TO 100
    1 IF (S .LT. .001) GO TO 4
      G = TWOPI*R*BJ(1,S*R)/S
      GO TO 100
    2 G = TWOPI*R*BJ(1,H*R)/H
      GO TO 100
    3 G = PI*R*R*(BJ(1,H*R)*BJ(1,S*R) + BJ(0,H*R)*BJ(0,S*R))
      GO TO 100
    4 G = PI*R*R
 100  RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE MB11A(M,N,A,IA,W)
      SUBROUTINE MB11A(M,N,A,IA,W)
C
C *** MB11A from HARWELL 25 May 79 ***
C
CC 12C
CH Inverts a rectangular matrix whose order is the smaller dimension;
CH used by the Harwell refinement routine VA05A.
C
      DIMENSION A(IA,1),W(1)
C     PARTITION THE WORKING SPACE ARRAY W
C     THE FIRST PARTITION HOLDS THE FIRST COMPONENTS OF THE VECTORS OF
C     THE ELEMENTARY TRANSFORMATIONS
      NRW=M
C     THE SECOND PARTITION RECORDS ROW INTERCHANGES
      NCW=M+M
C     THE THIRD PARTITION RECORDS COLUMN INTERCHANGES
C     SET THE INITIAL RECORDS OF ROW AND COLUMN INTERCHANGES
      DO 1 I=1,M
      N1=NRW+I
      W(N1)=0.5+FLOAT(I)
    1 CONTINUE
      DO 2 I=1,N
      N1=NCW+I
      W(N1)=0.5+FLOAT(I)
    2 CONTINUE
C     'KK' COUNTS THE SEPARATE ELEMENTARY TRANSFORMATIONS
      KK=1
C     FIND LARGEST ROW AND MAKE ROW INTERCHANGES
    3 RMAX=0.0
      DO 4 I=KK,M
      SUM=0.
      DO 5 J=KK,N
      SUM=SUM+A(I,J)**2
    5 CONTINUE
      IF (RMAX .LT. SUM) THEN
      RMAX=SUM
      IR=I
      ENDIF
    4 CONTINUE
      IF(RMAX.EQ.0.0) CALL ERRIN2(M-KK,0,'in MB11A:',
     & 'reduced rows found to be zero')
      IF (IR .LE. KK) GO TO 7
      N3=NRW+KK
      SUM=W(N3)
      N4=NRW+IR
      W(N3)=W(N4)
      W(N4)=SUM
      DO 9 J=1,N
      SUM=A(KK,J)
      A(KK,J)=A(IR,J)
      A(IR,J)=SUM
    9 CONTINUE
C     FIND LARGEST ELEMENT OF PIVOTAL ROW, AND MAKE COLUMN INTERCHANGES
    7 RMAX=0.
      SUM=0.
      DO 10 J=KK,N
      SUM=SUM+A(KK,J)**2
      IF (RMAX .GE. ABS(A(KK,J))) GO TO 10
      RMAX=ABS(A(KK,J))
      IR=J
   10 CONTINUE
      IF (IR .LE. KK) GO TO 12
      N5=NCW+KK
      RMAX=W(N5)
      N6=NCW+IR
      W(N5)=W(N6)
      W(N6)=RMAX
      DO 14 I=1,M
      RMAX=A(I,KK)
      A(I,KK)=A(I,IR)
      A(I,IR)=RMAX
   14 CONTINUE
C     REPLACE THE PIVOTAL ROW BY THE VECTOR OF THE TRANSFORMATION
   12 SIGMA=SQRT(SUM)
      BSQ=SQRT(SUM+SIGMA*ABS(A(KK,KK)))
      W(KK)=SIGN(SIGMA+ABS(A(KK,KK)),A(KK,KK))/BSQ
      A(KK,KK)=-SIGN(SIGMA,A(KK,KK))
      KP=KK+1
      IF (KP .GT. N) GO TO 16
      DO 17 J=KP,N
      A(KK,J)=A(KK,J)/BSQ
   17 CONTINUE
C     APPLY THE TRANSFORMATION TO THE REMAINING ROWS OF A
      IF (KP .GT. M) GO TO 16
      DO 19 I=KP,M
      SUM=W(KK)*A(I,KK)
      DO 20 J=KP,N
      SUM=SUM+A(KK,J)*A(I,J)
   20 CONTINUE
      A(I,KK)=A(I,KK)-SUM*W(KK)
      DO 21 J=KP,N
      A(I,J)=A(I,J)-SUM*A(KK,J)
   21 CONTINUE
   19 CONTINUE
      KK=KP
      GO TO 3
C     AT THIS STAGE THE REDUCTION OF A IS COMPLETE
C     NOW WE BUILD UP THE GENERALIZED INVERSE
C     APPLY THE FIRST ELEMENTARY TRANSFORMATION
   16 KK=M
      KP=M+1
      SUM=W(M)/A(M,M)
      IF (N .LE. M) GO TO 33
      DO 35 J=KP,N
      A(M,J)=-SUM*A(M,J)
   35 CONTINUE
   33 A(M,M)=1./A(M,M)-SUM*W(M)
C     NOW APPLY THE OTHER (M-1) TRANSFORMATIONS
   36 KP=KK
      KK=KP-1
      IF (KK .LE. 0) GO TO 37
C     FIRST TRANSFORM THE LAST (M-KK) ROWS
      DO 39 I=KP,M
      SUM=0.
      DO 40 J=KP,N
      SUM=SUM+A(KK,J)*A(I,J)
   40 CONTINUE
      DO 41 J=KP,N
      A(I,J)=A(I,J)-SUM*A(KK,J)
   41 CONTINUE
      W(I)=-SUM*W(KK)
   39 CONTINUE
C     THEN CALCULATE THE NEW ROW IN POSITION KK
      DO 42 J=KP,N
      SUM=-W(KK)*A(KK,J)
      DO 43 I=KP,M
      SUM=SUM-A(I,KK)*A(I,J)
   43 CONTINUE
      A(KK,J)=SUM/A(KK,KK)
   42 CONTINUE
C     AND REVISE THE COLUMN IN POSITION KK
      SUM=1.-W(KK)**2
      DO 44 I=KP,M
      SUM=SUM-A(I,KK)*W(I)
      A(I,KK)=W(I)
   44 CONTINUE
      A(KK,KK)=SUM/A(KK,KK)
      GO TO 36
C     RESTORE THE ROW INTERCHANGES
   37 DO 45 I=1,M
   46 N1=NRW+I
      IR=IFIX(W(N1))
      IF (I .GE. IR) GO TO 45
      SUM=W(N1)
      N2=NRW+IR
      W(N1)=W(N2)
      W(N2)=SUM
      DO 48 J=1,N
      SUM=A(I,J)
      A(I,J)=A(IR,J)
      A(IR,J)=SUM
   48 CONTINUE
      GO TO 46
   45 CONTINUE
C     RESTORE THE COLUMN INTERCHANGES
      DO 49 J=1,N
   50 N1=NCW+J
      IR=IFIX(W(N1))
      IF (J .GE. IR) GO TO 49
      SUM=W(N1)
      N2=NCW+IR
      W(N1)=W(N2)
      W(N2)=SUM
      DO 52 I=1,M
      SUM=A(I,J)
      A(I,J)=A(I,IR)
      A(I,IR)=SUM
   52 CONTINUE
      GO TO 50
   49 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE NB01A(K,AZ,BZ,E2,X,Y,MAXIT)
      SUBROUTINE NB01A(K,AZ,BZ,E2,X,Y,MAXIT)
C
C *** NB01A updated by PJB 15-Nov-1994  ***
C
CX
CC 9C
CH Harwell routine NB01A to find the zero of a function.
CD Finds the value of X in the range AZ <= X <= BZ for which Y(X)=0
CA Called initially with K=0, AZ and BZ set to the minimum and maximum values
CA of X to be searched, E2 to the precision in X required and ABS(MAXIT) to
CA the maximum number of iterations allowed.
CA Subsequent calls depend on the value of K returned
CA     K=0 Initial call as above
CA     K=1 calculate Y for the value of X returned and call again
CA     K=2 The value of X retuned is the required solution
CA     K=3 No zero value of Y was found X=AZ and BZ
CA     K=4 MAXIT calls have been made without finding the zero value within the
CA         required precision
CN the private common /NB01AP has been included (C43) to preserve the values
CN of A,B,IT AND THE Js between calls
      COMMON /NB01AP/A,B,IT,J1,J2,J3
C
      IF(K .GT. 0)GO TO 30
C
C     CALCULATE Y(X) AT X=AZ.
      A = AZ
      B = BZ
      X = A
      J1 = 1
      IT = 1
      M = IABS(MAXIT)
   10 K = J1
      GO TO 100
C
C     PRINT X AND Y(X) WHEN REQUESTED.
   30 IF(MAXIT .LE. 0) WRITE(6,2000)X,Y
2000  FORMAT(2X,'NB01A   X=',E16.7,'  Y(X)=',E16.7)
C
C     TEST WHETHER Y(X) IS SUFFICIENTLY SMALL.
      IF( ABS(Y) .GT. E2)GO TO 50
   45 K = 2
      GO TO 100
C
C     BRANCH DEPENDING ON THE VALUE OF J1.
   50 GO TO (60,70,88,170),J1
C
C     CALCULATE Y(X) AT X=BZ.
   60 YA = Y
      X = B
      J1 = 2
      GO TO 100
C
C     TEST WHETHER THE SIGNS OF Y(AZ) AND Y(BZ) ARE DIFFERENT.
   70 IF(YA*Y .LT. 0.)GO TO 120
C
C     BEGIN THE BINARY SUBDIVISION TO SEARCH FOR A BRACKET.
      X1 = A
      Y1 = YA
      J1 = 3
      H = B-A
      J2 = 1
   80 X2 = A+0.5*H
      J3 = 1
C
C     CHECK WHETHER MAXIT FUNCTION VALUES HAVE BEEN CALCULATED.
   90 IT = IT+1
      IF(IT .GE. M)GO TO 10
      X = X2
      GO TO 100
C
C     TEST WHETHER A BRACKET HAS BEEN FOUND.
   88 IF(YA*Y .LT. 0.)GO TO 120
C
C     CONTINUE THE SEARCH FOR A BRACKET.
      IF(J3 .GE. J2)GO TO 110
      A = X
      YA = Y
      X2 = X+H
      J3 = J3+1
      GO TO 90
  110 A = X1
      YA = Y1
      H = 0.5*H
      J2 = J2+J2
      GO TO 80
C
C     AT THIS POINT THE FIRST BRACKET HAS BEEN FOUND.
  120 B = X
      YB = Y
      J1 = 4
C
C     CALCULATE THE NEXT X BY THE SECANT METHOD BASED ON THE BRACKET.
  130 IF( ABS(YA) .LE. ABS(YB))GO TO 140
      X1 = A
      Y1 = YA
      X = B
      Y = YB
      GO TO 150
  140 X1 = B
      Y1 = YB
      X = A
      Y = YA
C
C     USE THE SECANT METHOD BASED ON THE FUNCTION VALUES Y1 AND Y.
  150 U = Y*(X-X1)/(Y-Y1)
  155 X2 = X-U
      IF(X2.EQ.X)GO TO 195
      X1 = X
      Y1 = Y
      YTEST = 0.5*AMIN1( ABS(YA), ABS(YB))
C
C     CHECK THAT X2 IS INSIDE THE INTERVAL (A,B).
      IF((X2-A)*(X2-B) .LT. 0. )GO TO 90
C
C     CALCULATE THE NEXT VALUE OF X BY BISECTION.
  160 X2 = 0.5*(A+B)
      YTEST = 0.
C
C     CHECK WHETHER THE MAXIMUM ACCURACY HAS BEEN ACHIEVED.
      IF((X2-A)*(X2-B))90,45,45
C
C     REVISE THE BRACKET (A,B).
  170 IF(YA*Y .GE. 0.)GO TO 180
      B = X
      YB = Y
      GO TO 190
  180 A = X
      YA = Y
C
C     USE YTEST TO DECIDE THE METHOD FOR THE NEXT VALUE OF X.
  190 IF(YTEST .LE. 0.)GO TO 130
      IF( ABS(Y) -YTEST)150,150,160
  195 IF(U.EQ.0.)GO TO 45
      U=U+U
      GO TO 155
C
 100  RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION RADIAN(X)
      FUNCTION RADIAN(X)
C
C *** RADIAN by JCM ***
C
CX
CC 10C
CH Converts from degrees to radians.
CA On entry X is the value of an angle in degrees
CD On exit  X has been converted to radians.
CN The function DEGREE(X) does the radians to degrees conversion
CN In routines where time matters, it is quicker to declare the COMMON
CN /CONSTA/ and multiply X by RAD.
C
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
C
      RADIAN = RAD*X
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE RCMPRD(A,B,C,I,J,K)
      SUBROUTINE RCMPRD(A,B,C,I,J,K)
C
C *** RCMPRD by PJB Jun 87 ***
C
CX
CC 12C
CH Performs the multiplication of a COMPLEX by a REAL matrix.
CA On entry A is a real IxJ matrix,
CA          B is a complex JxK matrix,
CA On exit  C is returned as the complex IxK matrix A*B
C
      DIMENSION A(1)
      COMPLEX B(1),C(1)
C
      DO 2 II = 1,I
      IK = II
      JK = 1
      DO 2 KK = 1,K
      IJ = II
      C(IK) = 0.
      DO 1 JJ = 1,J
      C(IK) = C(IK) + A(IJ)*B(JK)
      IJ = IJ + I
    1 JK = JK + 1
    2 IK = IK + I
      RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION RSCALP(A,B)
      FUNCTION RSCALP(A,B)
C
C *** RSCALP by PJB Jun 87 ***
C
CX
CC 12C
CH Forms the scalar product of two COMPLEX vectors.
CA On entry A and B are complex vectors of dimension 3
CD The function value is the real product (A.Conjg(B) + Conjg(A).B)/2
C
      COMPLEX A(3),B(3),P
      P=CMPLX(0.,0.)
      DO 1 I=1,3
      P=P+A(I)*CONJG(B(I))
    1 CONTINUE
      RSCALP=REAL(P)
      RETURN
      END
C
C
C
C
C LEVEL 2      FUNCTION SCALPR(A,B)
      FUNCTION SCALPR(A,B)
C
C *** SCALPR by JCM ***
C
CX
CC 12C
CH Forms the scalar product of two orthogonal vectors (or of one from
CH real space with one from reciprocal space).
CA On entry A and B are vectors of dimension 3
CD The function is set to the scalar product of A and B (not involving
CD  cell parameters) e.g. with one vector from real & one from reciprocal
CD  space
CN The function SCLPRD gives the scalar product using the cell parameters.
C
      DIMENSION A(3),B(3),C(1)
      CALL GMPRD(A,B,C,1,3,1)
      SCALPR = C(1)
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE SID(A,N,ND1,ND2,D)
      SUBROUTINE SID(A,N,ND1,ND2,D)
C
C *** SID from PJB by A. Bartelemy ***
C
CX
CC 12C
CH Solves a set of simultaneous linear equations.
CA  A   is an N by N+1 sized matrix written in a table of dimension ND1
CA      by ND2
CA  On entry A(1:N,1:N) contains the matrix of coefficients
CA           A(1:N,N+1) contains the column of right-hand sides
CA  On exit  D is the value of the determinant
CA           A(1:N,N+1) contains the solution if D is not zero
C
C  SUBROUTINE *** SID ***         A BARTHELEMY  19/2/75        *
C  INVERSION DE MATRICE AVEC CALCUL DE DETERMINANT             *
C  MATRICE A(N,N+1)    DANS TABLEAU (ND1,ND2)                  *
C   EN ENTREE POUR APPEL PAR RAFIN MAIN :                      *
C        A(I=1 A N,J=1 A N)=AMAT                               *
C        A(I=1 A N,J=N+1  )=VVEC                               *
C  N=NOMBRE DE COLONNES DE A                                   *
C
      DIMENSION II(16),IL(16),IG(16),A(ND1,ND2)
C
      M=N+1
      D=1.
      IS=N-1
      DO 10 K=1,N
      IL(K)=0
   10 IG(K)=K
C
C.....
      DO 150 K=1,N
      R=0.
      DO 40 I=1,N
      IF (IL(I) .NE. 0) GO TO 40
      W=A(I,K)
      X=ABS(W)
      IF (X .LT. R) GO TO 40
      R=X
      P=W
      KF=I
   40 CONTINUE
C
      II(K)=KF
      IL(KF)=KF
      D=D*P
C.....ERROR NUL DETERMINANT
      IF (D .EQ. 0.) GO TO 100
C
      DO 80 I=1,N
      IF (I .EQ. KF) THEN
        A(I,K)=1./P
      ELSE
        A(I,K)=-A(I,K)/P
      ENDIF
   80 CONTINUE
C
      DO 140 J=1,M
      IF (J .EQ. K) GO TO 140
      W=A(KF,J)
      IF (W .EQ. 0.) GO TO 140
      DO 130 I=1,N
      IF (I .EQ. KF) THEN
        A(I,J)=W/P
      ELSE
        A(I,J)=A(I,J)+W*A(I,K)
      ENDIF
  130 CONTINUE
  140 CONTINUE
C
  150 CONTINUE
C.....
C
      DO 190 K=1,IS
      KF=II(K)
      KL=IL(KF)
      KG=IG(K)
      IF (KF .EQ. KG) GO TO 190
C
      DO 170 I=1,N
      R=A(I,KF)
      A(I,KF)=A(I,KG)
  170 A(I,KG)=R
C
      DO 180 J=1,M
      R=A(K,J)
      A(K,J)=A(KL,J)
  180 A(KL,J)=R
C
      IL(KF)=K
      IL(KG)=KL
      IG(KL)=IG(K)
      IG(K)=KF
      D=-D
  190 CONTINUE
  100 RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE SINCOS(SN,CS,L1)
      SUBROUTINE SINCOS(SN,CS,L1)
C
C *** SINCOS updated by JCM 1 Jul 86 ***
C
CX
CC 10C
CH Calculates sin from cos or vice-versa.
CA On entry SN is a sine or cosine of an angle
CA On exit  CS is the cosine or sine respectively of the same angle
CA          L1 is a character variable which is used to identify the
CA             calling routine if the abssolute value of SN is greater
CA             than 1.
CN If ABS(SN)>1 the routine writes an error message and STOPS
C
      CHARACTER *(*) L1
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      IF (ABS(SN)-10.E-5 .GT. 1.) THEN
        WRITE (LPT,3000) SN,L1
        WRITE (ITO,3000) SN,L1
3000    FORMAT (/' Sin or Cos value',E12.4,' greater than unity',
     &  ' - called from ',A6)
C>> JCC        STOP
	  CALL BMBOUT
	  RETURN
      ENDIF
C
C>> JCC There can be an error raised here. SN can be passed
C>> As a NaN, due to a corruption in the CELL array. Please check
C>>
C NOW DETECT CASE WHERE SN IS ONLY JUST MORE THAN 1:
      IF (ABS(SN) .GE. 1.0) THEN
        SN = SIGN(1.0,SN)
        CS = 0.
      ELSE
C
C USUAL CASE:
        CS = SQRT(1. - SN*SN)
      ENDIF
 100  RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE SPHARM(Y,T,P,LMAX,NUM)
      SUBROUTINE SPHARM(Y,T,P,LMAX,NUM)
C
C *** SPHARM by PJB Apr 84 ***
C
CX
CC 9C
CH Calculates spherical harmonics.
CA  On entry T and P are the spherical polar angles theta and phi
CA             defining the orientation of the spherical harmonic function
CA             (in radians).
CA           LMAX is the number of different l values to calculate
CA             (LMAX=maximum l +1)
CA           NUM is the total number of spherical harmonic functions
CA              which will be calculated (NUM=LMAX(LMAX+1)/2)
CA On exit  Y(N) is complex and is set to the value of the Nth
CA              spherical harmonic, these being in sets of constant l
CA              arranged in order of increasing l with m running from
CA              0 to l within each l value.
CN Only values for positive m are stored
C
      COMPLEX Y(NUM),EXPHI(20)
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
C
      D = 1./SQRT(TWOPI)
      C = COS(T)
      S = SIN(T)
      Y(1) = CMPLX(SQRT(0.5),0.)
      IF (LMAX .EQ. 1) GO TO 6
      Y(2) = CMPLX(SQRT(1.5),0.)*C
      Y(3) = CMPLX(-SQRT(.75),0.)*S
      IF (LMAX .LT. 3) GO TO 6
      LL = LMAX-2
      N = 2
      DO 1 L = 1,LL
      A = SQRT(FLOAT((2*L+1)*(2*L+3)))
      B = SQRT(FLOAT((2*L+1)*(2*L-1)))
      MAX = L+1
      DO 2 MM = 1,MAX
      M = MM-1
      Y(N+L+1) = C*Y(N)
      IF (L .NE. M) Y(N+L+1) = Y(N+L+1)-SQRT(FLOAT((L-M)*(L+M)))*Y(N-L)
     & /B
      Y(N+L+1) = Y(N+L+1)*A/SQRT(FLOAT((L+1-M)*(L+1+M)))
      N = N+1
    2 CONTINUE
C
      Y(N+L+1) = CMPLX(0.,0.)
      IF (S .LT. .0001) GO TO 1
      Y(N+L+1) = -FLOAT(2*L)*C*Y(N+L)/S - SQRT(FLOAT(4*L+2))*Y(N+L-1)
      Y(N+L+1) = Y(N+L+1)/SQRT(FLOAT(2*L+2))
    1 CONTINUE
C
    6 CALL TRIG(EXPHI,P,LMAX)
      N = 1
C
      DO 4 L = 1,LMAX
      DO 5 M = 1,L
      Y(N) = Y(N)*EXPHI(M)*D
    5 N = N+1
    4 CONTINUE
      RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE SPLINE(N,X,F,D)
      SUBROUTINE SPLINE(N,X,F,D)
C
C *** SPLINE from HARWELL TB04A 21 May 85 ***
C
CX
CC 9A
CH Sets up a cubic spline to fit a curve.
CA On entry N is the number of points on a curve
CA          F(I) are the N function values at the N points X(I)
CA          D(I) is set to the derivative if the spline at the point I
CA              for all N points.
CN Uses working space in COMMON /SCRAT/
CN For further details see the description of the Harwell Library
C
      DIMENSION X(N),F(N),D(N)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
      COMMON /SCRAT/A(2000)
C
C F(I) ARE THE FUNCTION VALUES AT THE POINTS X(I) FOR I=1,N AND
C THE SPLINE DERIVATIVES D(I) ARE FOUND.  THE DIMENSION OF A MUST
C NOT BE LESS THAN 3*N.
C
      DO 5 I=2,N
      IF (X(I)-X(I-1)) 1,1,5
   1  WRITE(LPT,3000) I
3000  FORMAT(/'  ERROR ** in SPLINE - X',I3,' out of order')
      A(1)=1.
      GO TO 100
C
   5  CONTINUE
C
      DO 2 I=1,N
      J=2
      IF (I-1) 6,10,6
   6  J=N-1
      IF (I.EQ.N) GO TO 10
      H1=1./(X(I)-X(I-1))
      H2=1./(X(I+1)-X(I))
      A(3*I-2)=H1
      A(3*I-1)=2.*(H1+H2)
      A(3*I)=H2
      D(I)=3.0*(F(I+1)*H2*H2+F(I)*(H1*H1-H2*H2)-F(I-1)*H1*H1)
      GO TO 2
C
  10  H1=1./(X(J)-X(J-1))
      H2=1./(X(J+1)-X(J))
      A(3*I-2)=H1*H1
      A(3*I-1)=H1*H1-H2*H2
      A(3*I)=-H2*H2
      D(I)=2.*(F(J)*(H2*H2*H2+H1*H1*H1)-F(J+1)*H2*H2*H2-F(J-1)*H1*H1*H1)
   2  CONTINUE
      P=A(4)/A(1)
      A(5)=A(5)-P*A(2)
      A(6)=A(6)-P*A(3)
      D(2)=D(2)-P*D(1)
      DO 4 I=3,N
      K=3*I-4
      P=A(K+2)/A(K)
      A(K+3)=A(K+3)-P*A(K+1)
      D(I)=D(I)-P*D(I-1)
      IF (I.NE.N-1) GO TO 4
      P=A(K+5)/A(K)
      A(K+5)=A(K+6)-P*A(K+1)
      A(K+6)=A(K+7)
      D(N)=D(N)-P*D(N-2)
   4  CONTINUE
C
      D(N)=D(N)/A(3*N-1)
      DO 7 I=3,N
      J=N+2-I
   7  D(J)=(D(J)-A(3*J)*D(J+1))/A(3*J-1)
      D(1)=(D(1)-D(2)*A(2)-D(3)*A(3))/A(1)
      A(1)=0.
 100   RETURN
      END
C
C
C
C
C LEVEL 1      FUNCTION SPLINT(IX,N,U,S,D,X)
      FUNCTION SPLINT(IX,N,U,S,D,X)
C
C *** SPLINT from HARWELL TG01B 21 May 85 ***
C
CX
CC 9C
CH Evaluates a cubic spline given spline values and first derivative
CH values at the given knots.
C
C
CA On entry IX allows the caller to take advantage of spline parameters
CA            set on a previous call in cases when an X point follows
CA            the previous X point.
CA      If IX < 0 the whole range is searched for knot interval;
CA      If IX > 0 it is assumed that X is greater than the X of the
CA      previous call and search started from there.
CA           N=The number of knots.
CA           U holds the knots.
CA           S holds the spline values.
CA           D holds the first derivative values of the spline at the
CA             knots.
CA           X=the point at which the spline value is required.
C
CP A call of SPLINE must have set up the knots in D.
C
CN The spline value is defined as zero outside the knot range, which
CN is extended by a rounding error for the purpose.
C
C     ALLOWABLE ROUNDING ERROR ON POINTS AT EXTREMES OF KNOT RANGE
C     IS 2**IEPS*MAX(\U(1)\,\U(N)\).
      DIMENSION U(N),S(N),D(N)
      DATA IFLG,IEPS/0,-19/
C
C       TEST WETHER POINT IN RANGE.
      IF(X.LT.U(1)) GO TO 3
      IF(X.GT.U(N)) GO TO 4
C
C       JUMP IF KNOT INTERVAL REQUIRES RANDOM SEARCH.
      IF(IX.LT.0.OR.IFLG.EQ.0) GO TO 12
C       JUMP IF KNOT INTERVAL SAME AS LAST TIME.
      IF(X.LE.U(J+1)) GO TO 8
C       LOOP TILL INTERVAL FOUND.
    1 J=J+1
   11 IF(X.GT.U(J+1)) GO TO 1
      GO TO 7
C
C       ESTIMATE KNOT INTERVAL BY ASSUMING EQUALLY SPACED KNOTS.
   12 J=ABS(X-U(1))/(U(N)-U(1))*(N-1)+1
C       ENSURE CASE X=U(N) GIVES J=N-1.
      J=MIN0(J,N-1)
C       INDICATE THAT KNOT INTERVAL INSIDE RANGE HAS BEEN USED.
      IFLG=1
C       SEARCH FOR KNOT INTERVAL CONTAINING X.
      IF(X.GE.U(J)) GO TO 11
    2 J=J-1
      IF(X.LT.U(J)) GO TO 2
C
C       CALCULATE SPLINE PARAMETERS FOR JTH INTERVAL.
    7 H=U(J+1)-U(J)
      Q1=H*D(J)
      Q2=H*D(J+1)
      SS=S(J+1)-S(J)
      B=3.0*SS-2.0*Q1-Q2
      A=Q1+Q2-2.0*SS
C
C       CALCULATE SPLINE VALUE.
    8 Z=(X-U(J))/H
      SPLINT=((A*Z+B)*Z+Q1)*Z+S(J)
      GO TO 100
C
C       TEST IF X WITHIN ROUNDING ERROR OF U(1).
   3  IF(X.LE.U(1)-2.0**IEPS*AMAX1(ABS(U(1)),ABS(U(N)))) GO TO 101
      J=1
      GO TO 7
C       TEST IF X WITHIN ROUNDING ERROR OF U(N).
   4  IF(X.GE.U(N)+2.0**IEPS*AMAX1(ABS(U(1)),ABS(U(N)))) GO TO 101
      J=N-1
      GO TO 7
C
C POINTS OUTSIDE RANGE:
 101  IFLG=0
      SPLINT=0.0
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE STERMS(G,R,H,S)
      SUBROUTINE STERMS(G,R,H,S)
C
C *** STERMS by PJB 17 Jan 85 ***
C
CX
CC 9C
CH Calculates a term in the spherically symmetric form factor summation,
CH for a 3D averaged form-factor involving Sk and r only.
C
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
C
      FOURPI = 4.*PI
C  TEST FOR SPECIAL CASES:
      IF (H .LT .001) GO TO 1
      IF (S .LT. .001) GO TO 2
      IF (ABS(H-S) .LT. .001) GO TO 3
      G = H*COS(H*R)*SIN(S*R) - S*COS(S*R)*SIN(H*R)
      G = FOURPI*G/(H*S*(S*S-H*H))
      GO TO 100
    1 IF (S .LT. .001) GO TO 4
      G = SIN(S*R) - S*R*COS(S*R)
      G = FOURPI*G/S**3
      GO TO 100
    2 G = SIN(H*R) - H*R*COS(H*R)
      G = FOURPI*G/H**3
      GO TO 100
    3 G = -SIN((H+S)*R) + (H+S)*R
      G = TWOPI*G/(H+S)*H*S
      GO TO 100
    4 G = (FOURPI*R**3)/3.
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE SUMVEC(A,I,J,K,SUM)
      SUBROUTINE SUMVEC(A,I,J,K,SUM)
C
C *** SUMVEC by JCM 26 Sep 85 ***
C
CX
CC 12C
CH Calculates the sum of elements of a real array
CA On entry A is a real vector of length at least J
CA          I is the position in the array of the first element in the sum
CA          J is the position beyond which elements are not included
CA          K is the "step" between included elements
CA On exit  SUM is the sum of the chosen elements
C
CD Sets SUM=the sum of elements of array A by a DO LOOP 'I,J,K', dealing
CD correctly with J < I, and therefore usable in both FORTRAN 66 & 77.
C
      DIMENSION A(1)
C
      SUM=0.
      IF (J .LT. I) GO TO 100
C
      DO 1 N=I,J,K
   1  SUM=SUM+A(N)
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE TB02A(A,F,X,Y,N)
      SUBROUTINE TB02A(A,F,X,Y,N)
C
C *** TB02A updated by JCM from HARWELL ***
C
CX
CC 9C
CH Interpolates in non-equal interval table.
CA On entry real arrays A and F hold the arguments and function values
CA          X holds the required argument
CA          N is the dimension of A and F, and so is the number of items
CA On exit  Y holds the interpolated function value
C
      DIMENSION A(N),F(N)
      Y=0.0
      DO 1 L=1,N
      PL=1.0
      DO 3 J=1,N
      IF(L .EQ. J) GO TO 3
      PL=(X-A(J))*PL/(A(L)-A(J))
   3  CONTINUE
   1  Y=Y+PL*F(L)
      RETURN
      END
C
C
C
C
C LEVEL 3      SUBROUTINE TQLI(D,E,N,NP,Z,BOTH)
      SUBROUTINE TQLI(D,E,N,NP,Z,BOTH)
C
C *** TQLI by JCM from "NUMERICAL RECIPES"  Dec 89 ***
C
CX
CC 9C
CH Performs the QL algorithm for eigenvalues and vectors of a real
CH symmetric matrix previously reduced to tridiagonal form.
CA On entry D is a real vector of length NP whose first N elements are the
CA            diagonal elements of the required tridiagonal matrix;
CA          E similarly holds the sub-diagonal matrix;  E(1) is arbitrary.
CA          N is the size of all required matrices and vectors;
CA          NP is their DIMENSION in the routine, and may be larger.
CA          Z is an NPxNP array holding an NxN matrix.  If the routine
CA            TRED2 has been used to reduce the matrix to tridiagonal, Z should
CA            on entry to TQLI contain the matrix on exit from TRED2.
CA            Otherwise it should contain a unit matrix.
CA          BOTH is a logical, set TRUE if both eigenvalues and eigenvectors
CA             are wanted.
CA On exit  D holds the eigenvalues, and the Kth column of Z holds the
CA            normalised eigenvector corresponding to D(K).
C
CN Adapted from Press, Flannery, Teukolsky & Vetterling, Numerical Recipes.
C
      LOGICAL BOTH
      DIMENSION D(NP),E(NP),Z(NP,NP)

C>> JCC Numerical trap
	INTEGER IBMBER
	COMMON / CCSLER / IBMBER 
C
      IF (N .LE. 1) GO TO 100
      DO 11 I=2,N
      E(I-1)=E(I)
  11  CONTINUE
C
      E(N)=0.
      DO 15 L=1,N
      ITER=0
   1  DO 12 M=L,N-1
      DD=ABS(D(M))+ABS(D(M+1))
      IF (ABS(E(M))+DD.EQ.DD) GO TO 2
  12  CONTINUE
C
      M=N
   2  IF (M .NE. L) THEN
        IF (ITER.EQ.30)
     &   CALL ERRMES(1,0,'too many iterations IN TQLI')
        ITER=ITER+1
        G=(D(L+1)-D(L))/(2.*E(L))
        R=SQRT(G**2+1.)
        G=D(M)-D(L)+E(L)/(G+SIGN(R,G))
        S=1.
        C=1.
        P=0.
        DO 14 I=M-1,L,-1
        F=S*E(I)
        B=C*E(I)
C>> JCC It seems that the next line causes numerous crashes in the Pawley refinement.
C>> As such, Ive added in a check on the values of G and F to prevent it happening
C>> This ought to be tested - why is it causing the crash: the input data must be corrupt
C>> somehow.

	  IF (ABS(F).GT.0.0 .OR. ABS(G).GT.0.0) THEN
         IF(ABS(F).GE.ABS(G))THEN
          C=G/F
          R=SQRT(C**2+1.)
          E(I+1)=F*R
          S=1./R
          C=C*S
         ELSE
          S=F/G
          R=SQRT(S**2+1.)
          E(I+1)=G*R
          C=1./R
          S=S*C
         ENDIF
	  ELSE
C>> Ok: An error has occurred in the passed data
	   IBMBER = 1
	   RETURN
	  END IF 
        G=D(I+1)-P
        R=(D(I)-G)*S+2.*C*B
        P=S*R
        D(I+1)=G+P
        G=C*R-B
        IF (BOTH) THEN
          DO 13 K=1,N
          F=Z(K,I+1)
          Z(K,I+1)=S*Z(K,I)+C*F
          Z(K,I)=C*Z(K,I)-S*F
  13      CONTINUE
        ENDIF
C
  14    CONTINUE
        D(L)=D(L)-P
        E(L)=G
        E(M)=0.
        GO TO 1
      ENDIF
  15  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE TRANSC(A,ND)
      SUBROUTINE TRANSC(A,ND)
C
C *** TRANSC by JCM 12 Jul 83 ***
C
CX
CC 12C
CH Replaces a COMPLEX square matrix by its transposed conjugate.
CA On entry A is a COMPLEX NDxND matrix
CA On exit  the elements of A have been replaced by their conjugates, and A
CA          has been transposed.
C
      COMPLEX A(ND,ND),B
C
      DO 2 I=1,ND
   2  A(I,I)=CONJG(A(I,I))
C
      DO 1 I=1,ND-1
      DO 1 J=I+1,ND
      B=CONJG(A(I,J))
      A(I,J)=CONJG(A(J,I))
   1  A(J,I)=B
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE TRANSQ(A,ND)
      SUBROUTINE TRANSQ(A,ND)
C
C *** TRANSQ by JCM 12 Jul 83 ***
C
CX
CC 12C
CH Replaces a real square matrix by its transpose.
CA On entry A is an NDxND real square matrix
CA On exit  A has been replaced by its transpose
C
      DIMENSION A(ND,ND)
C
      DO 1 I=1,ND-1
      DO 1 J=I+1,ND
      B=A(I,J)
      A(I,J)=A(J,I)
   1  A(J,I)=B
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE TRED2(A,N,NP,D,E,BOTH)
      SUBROUTINE TRED2(A,N,NP,D,E,BOTH)
C
C *** TRED2 by JCM from "NUMERICAL RECIPES" 4 Dec 89 ***
C
CX
CC 9C
CH Performs the Householder reduction of a real symmetric matrix to
CH tridiagonal form.
CA On entry A is a real, symmetric NxN matrix held in an NPxNP array
CA          BOTH is a LOGICAL which is TRUE if both eigenvalues and eigen-
CA          vectors will subsequently be wanted.
CA On exit  D is a vector of length NP whose first N elements hold the
CA            diagonal of the tridiagonal matrix,
CA          E similarly holds the sub-diagonal elements, and E(1)=0.
CA          If BOTH, then A is replaced by the orthogonal matrix Q which
CA          effects the transformation;  if .NOT. BOTH, A holds rubbish.
C
CN For eigenvalues and eigenvectors, should be followed by a call to TQLI.
CN Adapted from Press, Flannery, Teukolsky & Vetterling, Numerical Recipes.
C
      LOGICAL BOTH
      DIMENSION A(NP,NP),D(NP),E(NP)
C
      IF (N .LE. 1) GO TO 1
      DO 18 I=N,2,-1
      L=I-1
      H=0.
      SCALE=0.
      IF(L.GT.1)THEN
        DO 11 K=1,L
        SCALE=SCALE+ABS(A(I,K))
  11    CONTINUE
        IF(SCALE.EQ.0.)THEN
          E(I)=A(I,L)
        ELSE
          DO 12 K=1,L
          A(I,K)=A(I,K)/SCALE
          H=H+A(I,K)**2
  12      CONTINUE
          F=A(I,L)
          G=-SIGN(SQRT(H),F)
          E(I)=SCALE*G
          H=H-F*G
          A(I,L)=F-G
          F=0.
          DO 15 J=1,L
          IF (BOTH) A(J,I)=A(I,J)/H
          G=0.
          DO 13 K=1,J
          G=G+A(J,K)*A(I,K)
  13      CONTINUE
C
          IF (L .GT. J) THEN
            DO 14 K=J+1,L
            G=G+A(K,J)*A(I,K)
  14        CONTINUE
          ENDIF
          E(J)=G/H
          F=F+E(J)*A(I,J)
  15      CONTINUE
          HH=F/(H+H)
          DO 17 J=1,L
          F=A(I,J)
          G=E(J)-HH*F
          E(J)=G
          DO 16 K=1,J
          A(J,K)=A(J,K)-F*E(K)-G*A(I,K)
  16      CONTINUE
  17      CONTINUE
        ENDIF
      ELSE
        E(I)=A(I,L)
      ENDIF
      D(I)=H
  18  CONTINUE
C
   1  IF (BOTH) D(1)=0.
        E(1)=0.
        DO 23 I=1,N
      IF (BOTH) THEN
        L=I-1
        IF(D(I).NE.0.)THEN
          DO 21 J=1,L
          G=0.
          DO 19 K=1,L
          G=G+A(I,K)*A(K,J)
  19      CONTINUE
          DO 20 K=1,L
          A(K,J)=A(K,J)-G*A(K,I)
  20      CONTINUE
  21      CONTINUE
        ENDIF
      ENDIF
      D(I)=A(I,I)
C
      IF (BOTH) THEN
        A(I,I)=1.
        IF(L.GE.1)THEN
          DO 22 J=1,L
          A(I,J)=0.
          A(J,I)=0.
  22      CONTINUE
        ENDIF
      ENDIF
  23  CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE TRIAN1(A,B,C,D,I)
      SUBROUTINE TRIAN1(A,B,C,D,I)
C
C *** TRIAN1 by JCM 25 Jan 85 ***
C
CX
CC 10C
CH Applies the cosine formula for the solution of spherical triangles.
CA The entry and exit conditions vary according to the entry value of I:
CA     I=1: sets D=angle A given sides A,B,C
CA     I=2: sets A=side A given sides B,C and D=angle A.
CA     I=3: sets D=side A given angles A,B,C
CA     I=4: sets A=angle A given angles B,C and D=side A.
CD The angles are given and returned as their cosines.
CO Writes an error message is a cosine found >1
C
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
      CS = B*C
      S = SQRT((1-B*B)*(1-C*C))
      GO TO (1,2,2,1),I
    1 CS = -CS
    2 GO TO (3,4,3,4),I
   3  IF(ABS(S) .GT. 1.E-3) GO TO 9
      IF (ABS(A+CS) .GT. 1.E-3) GO TO 10
      D=0.
      GO TO 100
    9 X = (A + CS)/S
      GO TO 5
    4 X = D*S + CS
    5 IF (ABS(X) - 1. .LT. 10.E-3) GO TO 8
  10  WRITE (LPT,3000) A,B,C,D,I,S,CS,X
      WRITE (ITO,3000) A,B,C,D,I,S,CS,X
3000  FORMAT (/' ERROR ** in spherical triangle - cosine greater',
     & ' than 1.0'/' A,B,C,D=',4F10.4,'I=',I2,'S,CS,X=',3F10.4)
C>> JCC      STOP
	CALL BMBOUT
	RETURN
C
    8 IF (ABS(X) .GT. 1.) X = SIGN(1.,X)
      GO TO (6,7,6,7),I
    6 D = X
      GO TO 100
    7 A = X
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE TRIG(A,B,K)
      SUBROUTINE TRIG(A,B,K)
C
C *** TRIG by JCM 18 Apr 84 ***
C
CX
CC 10C
CH Sets up cos(nx) and sin(nx) for a range of n by recursion.
CA On entry B is the reqired argument x
CA          K is the required number of terms
CA On exit  A is the COMPLEX array holding the answers
CD Calculates the COMPLEX trigonometric functions A(i) = CEXP(0,m*B)
CD for i = m+1, m = 0,K-1.
C
      COMPLEX A(K),MULT
      A(1) = CMPLX(1.,0.)
      IF (K .LT. 2) GO TO 100
      MULT = CEXP(CMPLX(0.,B))
      DO 1 I = 2,K
      A(I) = A(I-1)*MULT
    1 CONTINUE
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE TRINV3(A,D)
      SUBROUTINE TRINV3(A,D)
C
C *** TRINV3 by JCM ***
C
CX
CC 12C
CH Replaces a 3x3 matrix by the transpose of its inverse.
CA On entry A holds a 3x3 real matrix
CA On exit  D is the value of the determinant of the matrix.
CA          A has been replaced by its inverse
CD If D is less than 10E-5 no replacement takes place.
C
      DIMENSION A(9),P(9)
      D = 0.
      II = 0
      IO = 0
      JO = 3
      KO = 6
      DO 1 IC = 1,3
      J = 2
      K = 3
      DO 2 I = 1,3
      II = II+1
      P(II) = A(JO+J)*A(KO+K) - A(JO+K)*A(KO+J)
      J = K
    2 K = I
      D = D + P(II)*A(II)
      JO = KO
      KO = IO
    1 IO = IO + 3
      IF (ABS(D) .LT. 10.E-5) GO TO 100
      DO 3 I = 1,9
    3 A(I) = P(I)/D
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE UNIVEC(X,U)
      SUBROUTINE UNIVEC(X,U)
C
C *** UNIVEC 1 May 84 by JCM ***
C
CX
CC 12C
CH Replaces a vector by a parallel unit vector, and gives its length.
CA On entry X is a real 1x3 vector referred to the CCSL orthogonal axes
CA On exit  X is replaced by a parallel unit vector
CA          U is set to the original length of X
CD If the length would be very small, X is not changed.
CN Works in double precision
C
      DOUBLE PRECISION DU
      DIMENSION X(3)
C
      U = 0.
      DU = 0.D0
      DO 1 I = 1,3
    1 DU = DU + X(I)*X(I)
      IF (DU .LT. 10.D-8) GO TO 100
      U = SNGL(DSQRT(DU))
      DO 2 I = 1,3
    2 X(I) = X(I)/U
 100  RETURN
      END
C
C
C
C
C LEVEL 4      SUBROUTINE VA05A(M,N,F,X,DSTEP,DMAX,ACC,MAXFUN,IPRINT,W)
      SUBROUTINE VA05A (M,N,F,X,DSTEP,DMAX,ACC,MAXFUN,IPRINT,W)
C
C *** VA05A updated by JCM from HARWELL ***
C
CX
CC 6C
CH Minimises the sum of squares of given functions without requiring
CH the explicit calculation of derivatives.
CA On entry M,N,X,DSTEP,DMAX,ACC,MAXFUN,IPRINT,W should be set according to the
CA          specification of VA05A
CA On exit  X is set to contain the refined values of the parameters
CA          F is set to the corresponding function values
CP A routine called CALFUN(M,N,F,X) must be provided to set the array F
C
CN IPRINT must be the name of an integer, NOT an explicit integer
C
      DIMENSION F(1),X(1),W(1)
      COMMON /IOUNIT/LPT,ITI,ITO,IPLO,LUNI,IOUT
C
C MAXC COUNTS CALLS OF CALFUN:
      MAXC=0
      MPN=M+N
C     'NT' AND 'NTEST' CAUSE AN ERROR RETURN IF F(X) DOES NOT DECREASE
      NT=N+2
      NTEST=0
C     'DTEST' IS USED IN A TEST TO MAINTAIN LINEAR INDEPENDENCE
      DTEST=FLOAT(N+N)-0.5
C     PARTITION THE WORKING SPACE ARRAY W
C     THE FIRST PARTITION HOLDS THE JACOBIAN APPROXIMATION
      NWI=M*N
C     THE NEXT PARTITION HOLDS THE GENERALIZED INVERSE
      NWX=NWI+MPN*N
C     THE NEXT PARTITION HOLDS THE BEST VECTOR X
      NWF=NWX+N
C     THE NEXT PARTITION HOLDS THE BEST VECTOR F
      NWC=NWF+M
C     THE NEXT PARTITION HOLDS THE COUNTS OF THE INDEPENDENT DIRECTIONS
      NWD=NWC+N
C     THE NEXT PARTITION HOLDS THE INDEPENDENT DIRECTIONS
      NWW=NWD+N*N
C     THE REMAINDER OF W IS USED FOR SCRATCH VECTORS
      NWV=NWW+N
      NWT=NWV+M
      NWU=NWT+N
C     USUALLY 'FMIN' IS THE LEAST CALCULATED VALUE OF F(X)
      FMIN=-1.
C     USUALLY 'DD' IS THE SQUARE OF THE CURRENT STEP LENGTH
      DD=0.
      DSS=DSTEP*DSTEP
      DM=DMAX*DMAX
C     'PARM' IS THE LEAST VALUE OF THE MARQUARDT PARAMETER
      PARM=SQRT(ACC)/DMAX
C     'DPAR' AND 'NTPAR' ARE USED TO REGULATE THE MARQUARDT PARAMETER
      DPAR=10.*DM
C     'IS' CONTROLS A GO TO STATEMENT FOLLOWING A CALL OF CALFUN
      IS=4
      IC=0
C     'TINC' IS USED IN THE CRITERION TO INCREASE THE STEP LENGTH
      TINC=1.
C
C     START A NEW PAGE FOR PRINTING
      IF (IPRINT .NE. 0) THEN
        CALL NEWPAG(LPT)
        CALL MESS(LPT,0,'   THE FOLLOWING OUTPUT IS PROVIDED BY '//
     &  'SUBROUTINE VA05A')
        CALL NEWLIN(LPT)
        CALL NEWLIN(LPT)
        IPC=0
      ENDIF
      GO TO 3
C
C     TEST WHETHER THERE HAVE BEEN MAXFUN CALLS OF CALFUN
    4 IF (MAXFUN .GT. MAXC) GO TO 3
      IF (IPRINT .EQ. 0) THEN
        IPRINT=2
        GO TO 19
      ELSE
        WRITE(LPT,3000) MAXC
        WRITE(ITO,3000) MAXC
3000    FORMAT (///5X,'ERROR RETURN FROM VA05A BECAUSE THERE HAVE BEEN',
     &   I5,' CALLS OF CALFUN')
        GO TO 7
      ENDIF
C
C     CALL THE SUBROUTINE CALFUN
    3 MAXC=MAXC+1
      CALL CALFUN (M,N,F,X)
C     CALCULATE THE SUM OF SQUARES
      FSQ=0.
      DO 8 I=1,M
      FSQ=FSQ+F(I)*F(I)
    8 CONTINUE
C     TEST FOR ERROR RETURN BECAUSE F(X) DOES NOT DECREASE
      GO TO (9,10,9,10),IS
    9 IF (FSQ .LT. FMIN) GO TO 11
      IF (DD .GT. DSS) GO TO 10
      NTEST=NTEST-1
      IF (NTEST .GT. 0) GO TO 10
      IF (IPRINT .NE. 0) GO TO 15
      IPRINT=1
      GO TO 19
   15 WRITE (LPT,3001)
      WRITE (ITO,3001)
3001  FORMAT (///5X,'ERROR RETURN FROM VA05A BECAUSE F(X) NO LONGER',
     & ' DECREASES'//5X,'THIS MAY BE DUE TO THE VALUES OF DSTEP',
     & ' AND ACC, OR TO LOSS OF RANK IN THE JACOBIAN MATRIX')
C     PROVIDE PRINTING OF FINAL SOLUTION IF REQUESTED
    7 IF (IPRINT .EQ. 0) GO TO 19
      WRITE (LPT,2001) MAXC
2001  FORMAT (///5X,'THE FINAL SOLUTION CALCULATED BY VA05A REQUIRED',
     & I5,' CALLS OF CALFUN, AND IS')
      WRITE (LPT,2002) (I,W(NWX+I),I=1,N)
2002  FORMAT (//4X,'I',7X,'X(I)',10X,'I',7X,'X(I)',10X,'I',7X,'X(I)',
     & 10X,'I',7X,'X(I)',10X,'I',7X,'X(I)'//5(I5,E17.8))
      WRITE (LPT,2003) (I,W(NWF+I),I=1,M)
2003  FORMAT (//4X,'I',7X,'F(I)',10X,'I',7X,'F(I)',10X,'I',7X,'F(I)',
     & 10X,'I',7X,'F(I)',10X,'I',7X,'F(I)'//5(I5,E17.8))
      WRITE (LPT,2004) FMIN
2004  FORMAT (/5X,'THE SUM OF SQUARES IS',E17.8)
C     RESTORE THE BEST VALUES OF X AND F
   19 DO 135 I=1,N
      X(I)=W(NWX+I)
  135 CONTINUE
      DO 136 I=1,M
      F(I)=W(NWF+I)
  136 CONTINUE
      GO TO 100
C
   11 NTEST=NT
C     PROVIDE ORDINARY PRINTING IF REQUESTED
   10 IF(IABS(IPRINT)-1) 39,38,40
   38 WRITE (LPT,2005) MAXC
2005  FORMAT (///5X,'AT THE',I5,' TH CALL OF CALFUN WE HAVE')
   42 WRITE (LPT,2002) (I,X(I),I=1,N)
      WRITE (LPT,2004) FSQ
      IF (IPRINT .GT. 0) WRITE (LPT,2003) (I,F(I),I=1,M)
      GO TO 39
C
   40 IPC=IPC-1
      IF (IPC .GT. 0) GO TO 39
      WRITE (LPT,2006) MAXC
2006  FORMAT (///5X,'THE BEST ESTIMATE AFTER',I5,' CALLS OF CALFUN IS')
      IPC=IABS(IPRINT)
      IF (FSQ .LT. FMIN) GO TO 42
      IF (FMIN .LT. 0) GO TO 42
      WRITE (LPT,2002) (I,W(NWX+I),I=1,N)
      WRITE (LPT,2004) FMIN
      IF (IPRINT .GT. 0) WRITE (LPT,2003) (I,W(NWF+I),I=1,M)
C
   39 GO TO (49,47,47,48),IS
C     STORE THE INITIAL VECTORS X AND F
   48 IF (IC .GT. 0) GO TO 51
      DO 52 I=1,N
      W(NWX+I)=X(I)
   52 CONTINUE
      GO TO 54
C
C     CALCULATE THE INITIAL JACOBIAN APPROXIMATION
   51 K=IC
      DO 55 I=1,M
      W(K)=(F(I)-W(NWF+I))/DSTEP
      K=K+N
   55 CONTINUE
C     TEST WHETHER THE MOST RECENT X IS BEST
      IF (FMIN .GT. FSQ) GO TO 57
      X(IC)=W(NWX+IC)
      GO TO 58
C
   57 W(NWX+IC)=X(IC)
   54 DO 53 I=1,M
      W(NWF+I)=F(I)
   53 CONTINUE
      FMIN=FSQ
C
C     SET X FOR THE NEXT CALL OF CALFUN
   58 IC=IC+1
      IF (IC .GT. N) GO TO 60
      X(IC)=W(NWX+IC)+DSTEP
      GO TO 3
C     SET THE DIRECTION MATRIX
   60 K=NWD
      DO 61 I=1,N
      DO 62 J=1,N
      K=K+1
      W(K)=0.
   62 CONTINUE
      W(K+I-N)=1.
      W(NWC+I)=1.+FLOAT(N-I)
   61 CONTINUE
C     SET THE MARQUARDT PARAMETER TO ITS LEAST VALUE
   24 PAR=PARM
C     COPY THE JACOBIAN AND APPEND THE MARQUARDT MATRIX
   25 PPAR=PAR*PAR
      NTPAR=0
   63 KK=0
      K=NWI+NWI
      DO 26 I=1,N
      DO 141 J=1,M
      KK=KK+1
      W(KK+NWI)=W(KK)
  141 CONTINUE
      DO 27 J=1,N
      K=K+1
      W(K)=0.
   27 CONTINUE
      W(K+I-N)=PAR
   26 CONTINUE
C     CALCULATE THE GENERALIZED INVERSE OF J
      CALL MB11A (N,MPN,W(NWI+1),N,W(NWW+1))
C     NOTE THAT THE THIRD AND FIFTH ENTRIES OF THIS ARGUMENT LIST
C     STAND FOR ONE-DIMENSIONAL ARRAYS.
C     START THE ITERATION BY TESTING FMIN
   64 IF (FMIN .LE. ACC) GO TO 7
C     NEXT PREDICT THE DESCENT AND MARQUARDT MINIMA
      DS=0.
      DN=0.
      SP=0.
      DO 66 I=1,N
      X(I)=0.
      F(I)=0.
      K=I
      DO 67 J=1,M
      X(I)=X(I)-W(K)*W(NWF+J)
      F(I)=F(I)-W(NWI+K)*W(NWF+J)
      K=K+N
   67 CONTINUE
      DS=DS+X(I)*X(I)
      DN=DN+F(I)*F(I)
      SP=SP+X(I)*F(I)
   66 CONTINUE
C     PREDICT THE REDUCTION IN F(X) DUE TO THE MARQUARDT STEP
C     AND ALSO PREDICT THE LENGTH OF THE STEEPEST DESCENT STEP
      PRED=SP+SP
      DMULT=0.
      K=0
      DO 68 I=1,M
      AP=0.
      AD=0.
      DO 69 J=1,N
      K=K+1
      AP=AP+W(K)*F(J)
      AD=AD+W(K)*X(J)
   69 CONTINUE
      PRED=PRED-AP*AP
      DMULT=DMULT+AD*AD
   68 CONTINUE
C     TEST FOR CONVERGENCE
      IF (DN .GT.DM) GO TO 29
      AP=SQRT(DN)
      IF (PRED+2.*PPAR*AP*(DMAX-AP)-ACC) 7,7,70
   29 IF (PRED+PPAR*(DM-DN)-ACC) 7,7,70
C     TEST WHETHER TO APPLY THE FULL MARQUARDT CORRECTION
   70 DMULT=DS/DMULT
      DS=DS*DMULT*DMULT
   71 IS=2
      IF (DN .GT. DD) GO TO 73
C     TEST THAT THE MARQUARDT PARAMETER HAS ITS LEAST VALUE
      IF (PAR .GT. PARM) GO TO 24
      DD=AMAX1(DN,DSS)
      DS=0.25*DN
      TINC=1.
      IF (DN .GE. DSS) GO TO 132
      IS=3
      GO TO 103
C
C     TEST WHETHER TO INCREASE THE MARQUARDT PARAMETER
   73 IF (DN .GT. DPAR) GO TO 32
      NTPAR=0
      GO TO 33
   32 IF (NTPAR .GT. 0) GO TO 35
      NTPAR=1
      PTM=DN
      GO TO 33
   35 NTPAR=NTPAR+1
      PTM=AMIN1 (PTM,DN)
      IF (NTPAR .LT. NT) GO TO 33
C     SET THE LARGER VALUE OF THE MARQUARDT PARAMETER
      PAR=PAR*(PTM/DM)**0.25
      IF (6.*DD .GE. DM) GO TO 25
      AP=SQRT(PRED/DN)
      IF (AP .LE. PAR) GO TO 25
      PAR=AMIN1(AP,PAR*(DM/(6.*DD))**0.25)
      GO TO 25
C
C     TEST WHETHER TO USE THE STEEPEST DESCENT DIRECTION
   33 IF (DS .LT. DD) GO TO 75
C     TEST WHETHER THE INITIAL VALUE OF DD HAS BEEN SET
      IF (DD .GT. 0.) GO TO 78
      DD=AMIN1(DM,DS)
      IF (DD .GE. DSS) GO TO 78
      DD=DSS
      GO TO 71
C
C     SET THE MULTIPLIER OF THE STEEPEST DESCENT DIRECTION
   78 ANMULT=0.
      DMULT=DMULT*SQRT(DD/DS)
      GO TO 80
C     INTERPOLATE BETWEEN THE STEEPEST DESCENT AND MARQUARDT DIRECTIONS
   75 SP=SP*DMULT
      ANMULT=(DD-DS)/((SP-DS)+SQRT((SP-DD)**2+(DN-DD)*(DD-DS)))
      DMULT=DMULT*(1.-ANMULT)
C     CALCULATE THE CORRECTION TO X, AND ITS ANGLE WITH THE FIRST
C     DIRECTION
   80 DN=0.
      SP=0.
      DO 81 I=1,N
      F(I)=DMULT*X(I)+ANMULT*F(I)
      DN=DN+F(I)*F(I)
      SP=SP+F(I)*W(NWD+I)
   81 CONTINUE
      DS=0.25*DN
C     TEST WHETHER AN EXTRA STEP IS NEEDED FOR INDEPENDENCE
      IF (W(NWC+1) .LE. DTEST) GO TO 132
      IF (SP*SP .GE. DS) GO TO 132
C
C     TAKE THE EXTRA STEP AND UPDATE THE DIRECTION MATRIX
   83 DO 84 I=1,N
      X(I)=W(NWX+I)+DSTEP*W(NWD+I)
      W(NWC+I)=W(NWC+I+1)+1.
   84 CONTINUE
      W(NWD)=1.
      IF(N.LE.1)GO TO 4
      DO 85 I=1,N
      K=NWD+I
      SP=W(K)
      DO 86 J=2,N
      W(K)=W(K+N)
      K=K+N
   86 CONTINUE
      W(K)=SP
   85 CONTINUE
      GO TO 4
C     EXPRESS THE NEW DIRECTION IN TERMS OF THOSE OF THE DIRECTION
C     MATRIX, AND UPDATE THE COUNTS IN W(NWC+1) ETC.
  132 IF(N.GE.2)GO TO 153
      IS=1
      GO TO 152
  153 SP=0.
      K=NWD
      DO 87 I=1,N
      X(I)=DW
      DW=0.
      DO 88 J=1,N
      K=K+1
      DW=DW+F(J)*W(K)
   88 CONTINUE
      GO TO (89,90),IS
   90 W(NWC+I)=W(NWC+I)+1.
      SP=SP+DW*DW
      IF (SP .LE. DS) GO TO 87
      IS=1
      KK=I
      X(1)=DW
      GO TO 92
   89 X(I)=DW
   92 W(NWC+I)=W(NWC+I+1)+1.
   87 CONTINUE
      W(NWD)=1.
C     REORDER THE DIRECTIONS SO THAT KK IS FIRST
      IF (KK .LE. 1) GO TO 93
      KS=NWC+KK*N
      DO 95 I=1,N
      K=KS+I
      SP=W(K)
      DO 96 J=2,KK
      W(K)=W(K-N)
      K=K-N
   96 CONTINUE
      W(K)=SP
   95 CONTINUE
C     GENERATE THE NEW ORTHOGONAL DIRECTION MATRIX
   93 DO 97 I=1,N
      W(NWW+I)=0.
   97 CONTINUE
      SP=X(1)*X(1)
      K=NWD
      DO 98 I=2,N
      DS=SQRT(SP*(SP+X(I)*X(I)))
      DW=SP/DS
      DS=X(I)/DS
      SP=SP+X(I)*X(I)
      DO 99 J=1,N
      K=K+1
      W(NWW+J)=W(NWW+J)+X(I-1)*W(K)
      W(K)=DW*W(K+N)-DS*W(NWW+J)
   99 CONTINUE
   98 CONTINUE
      SP=1./SQRT(DN)
      DO 199 I=1,N
      K=K+1
  199 W(K)=SP*F(I)
C     PREDICT THE NEW RIGHT HAND SIDES
  152 FNP=0.
      K=0
      DO 101 I=1,M
      W(NWW+I)=W(NWF+I)
      DO 102 J=1,N
      K=K+1
      W(NWW+I)=W(NWW+I)+W(K)*F(J)
  102 CONTINUE
      FNP=FNP+W(NWW+I)**2
  101 CONTINUE
C     CALCULATE THE NEXT VECTOR X, AND THEN CALL CALFUN
  103 DO 104 I=1,N
      X(I)=W(NWX+I)+F(I)
  104 CONTINUE
      GO TO 4
C     UPDATE THE STEP SIZE
   49 DMULT=0.9*FMIN+0.1*FNP-FSQ
      IF (DMULT .GE. 0.) GO TO 108
      DD=AMAX1(DSS,0.25*DD)
      TINC=1.
      IF (FSQ-FMIN) 106,107,107
C     TRY THE TEST TO DECIDE WHETHER TO INCREASE THE STEP LENGTH
  108 SP=0.
      SS=0.
      DO 109 I=1,M
      SP=SP+ABS(F(I)*(F(I)-W(NWW+I)))
      SS=SS+(F(I)-W(NWW+I))**2
  109 CONTINUE
      PJ=1.+DMULT/(SP+SQRT(SP*SP+DMULT*SS))
      SP=AMIN1(4.,TINC,PJ)
      TINC=PJ/SP
      DD=AMIN1(DM,SP*DD)
      GO TO 106
C     IF F(X) IMPROVES STORE THE NEW VALUE OF X
   47 IF (FSQ .GE. FMIN) GO TO 110
  106 FMIN=FSQ
      DO 111 I=1,N
      SP=X(I)
      X(I)=W(NWX+I)
      W(NWX+I)=SP
  111 CONTINUE
      DO 112 I=1,M
      SP=F(I)
      F(I)=W(NWF+I)
      W(NWF+I)=SP
  112 CONTINUE
  110 GO TO (107,107,113),IS
  113 IS=2
      IF (FMIN-ACC) 7,7,83
C     CALCULATE THE CHANGES IN X AND IN F
  107 DS=0.
      DO 114 I=1,N
      X(I)=X(I)-W(NWX+I)
      DS=DS+X(I)*X(I)
  114 CONTINUE
      DO 115 I=1,M
      F(I)=F(I)-W(NWF+I)
  115 CONTINUE
C     CALCULATE THE GENERALIZED INVERSE TIMES THE CHANGE IN X
      K=NWI
      SS=0.
      DO 116 I=1,MPN
      SP=0.
      DO 117 J=1,N
      K=K+1
      SP=SP+W(K)*X(J)
  117 CONTINUE
      W(NWV+I)=SP
      SS=SS+SP*SP
  116 CONTINUE
C     CALCULATE J TIMES THE CHANGE IN F
C     ALSO APPLY PROJECTION TO THE GENERALIZED INVERSE
      DO 118 I=1,N
      ST=0.
      K=NWI+I
      DO 119 J=1,MPN
      ST=ST+W(K)*W(J+NWV)
      K=K+N
  119 CONTINUE
      ST=ST/SS
      K=NWI+I
      DO 120 J=1,MPN
      W(K)=W(K)-ST*W(J+NWV)
      K=K+N
  120 CONTINUE
      ST=PPAR*X(I)
      K=I
      DO 121 J=1,M
      ST=ST+W(K)*F(J)
      K=K+N
  121 CONTINUE
      W(NWW+I)=ST
  118 CONTINUE
C     REVISE J AND CALCULATE ROW VECTOR FOR CORRECTION TO INVERSE
      IC=0
      K=0
      KK=NWI
      SP=0.
      SPP=0.
      DO 122 I=1,M
      SS=F(I)
      ST=F(I)
      DO 123 J=1,N
      IC=IC+1
      KK=KK+1
      SS=SS-W(IC)*X(J)
      ST=ST-W(KK)*W(NWW+J)
  123 CONTINUE
      SS=SS/DS
      W(NWV+I)=ST
      SP=SP+F(I)*ST
      SPP=SPP+ST*ST
      DO 124 J=1,N
      K=K+1
      W(K)=W(K)+SS*X(J)
  124 CONTINUE
  122 CONTINUE
      DO 125 I=1,N
      ST=PAR*X(I)
      DO 126 J=1,N
      KK=KK+1
      ST=ST-W(KK)*W(NWW+J)
  126 CONTINUE
      W(NWT+I)=ST
      SP=SP+PAR*X(I)*ST
      SPP=SPP+ST*ST
  125 CONTINUE
C     TEST THAT THE SCALAR PRODUCT IS SUFFICIENTLY ACCURATE
      IF (0.01*SPP-ABS(SP-SPP)) 63,63,127
C     CALCULATE THE NEW GENERALIZED INVERSE
  127 DO 128 I=1,N
      K=NWI+I
      ST=X(I)
      DO 129 J=1,M
      ST=ST-W(K)*F(J)
      K=K+N
  129 CONTINUE
      SS=0.
      DO 130 J=1,N
      SS=SS+W(K)*X(J)
      K=K+N
  130 CONTINUE
      ST=(ST-PAR*SS)/SP
      K=NWI+I
      DO 131 J=1,MPN
      W(K)=W(K)+ST*W(NWV+J)
      K=K+N
  131 CONTINUE
  128 CONTINUE
      GO TO 64
C
 100  RETURN
      END
C
C
C
C
C LEVEL 4      FUNCTION VECOUP(J1,M1,J2,M2,J,M)
      FUNCTION VECOUP(J1,M1,J2,M2,J,M)
C
C *** VECOUP by PJB 17 Jan 85 ***
C
CX
CC 9C
CH Calculates Clebsch-Gordon vector coupling coefficients.
C
CA On exit VECOUP holds the answer
C
      DIMENSION IFAC(6)
C
      VECOUP = 0.
      IF (J .GT. J1+J2) GO TO 100
      IF (IABS(J1-J2) .GT. J) GO TO 100
      IF (M .NE. M1+M2) GO TO 100
      ANUM = FACT(J1+J2-J)*FACT(J1-J2+J)*FACT(J2+J-J1)
      ANUM = ANUM*FACT(J1+M1)*FACT(J2+M2)*FACT(J+M)
      ANUM = ANUM*FACT(J1-M1)*FACT(J2-M2)*FACT(J-M)
      VECOUP = SQRT(FLOAT(2*J+1)*ANUM/FACT(J1+J2+J+1))
      VEC = VECOUP
      IFAC(1) = J1+J2-J
      IFAC(2) = J1-M1
      IFAC(3) = J2+M2
      IFAC(4) = J-J2+M1
      IFAC(5) = J-J1-M2
      IFAC(6) = 0
      N = 0
      DO 1 I = 4,6
      IF (IFAC(I) .LT. N) N = IFAC(I)
    1 CONTINUE
C
      K = 0
      DO 7 I = 1,3
      IFAC(I) = IFAC(I) + N
      IF (IFAC(I) .EQ. 0) K = 1
    7 CONTINUE
C
      DO 8 I = 4,6
    8 IFAC(I) = IFAC(I) - N
C
      SUM = 1
      DO 2 I = 1,6
    2 SUM = SUM/FACT(IFAC(I))
C
      SUM = SUM*FLOAT(1-2*MOD(IABS(N),2))
      TERM = SUM
      IF (K .EQ. 1) GO TO 101
    5 TERM = -TERM
C
      DO 3 I = 4,6
      IFAC(I) = IFAC(I) + 1
    3 TERM = TERM/FLOAT(IFAC(I))
C
      DO 4 I = 1,3
      TERM = TERM*FLOAT(IFAC(I))
      IFAC(I) = IFAC(I) - 1
      IF (IFAC(I) .EQ. 0) K = 1
    4 CONTINUE
      SUM = SUM + TERM
      IF (K .NE. 1) GO TO 5
C
 101  VECOUP = VECOUP*SUM
 100  RETURN
      END
C
C
C
C
C LEVEL 1      SUBROUTINE VECPRD(VEC1,VEC2,PRD)
      SUBROUTINE VECPRD(VEC1,VEC2,PRD)
C
C *** VECPRD by JCM ***
C
CX
CC 12C
CH Calculates the vector product of two 1x3 vectors.
CA On entry VEC1 and VEC2 hold 1x3 real vectors
CA On exit PRD = the vector product of VEC1 cross VEC2
C
      DIMENSION VEC1(3),VEC2(3),PRD(3)
      J = 2
      K = 3
      DO 1 I = 1,3
      PRD(I) = VEC1(J)*VEC2(K) - VEC1(K)*VEC2(J)
      J = K
   1  K = I
      RETURN
      END
C
C
C
C
C LEVEL 2      SUBROUTINE WTMEAN(X,DX,IFUN,SUMS)
      SUBROUTINE WTMEAN(X,DX,IFUN,SUMS)
C
C *** WTMEAN by JCM 17 Jan 85 ***
C
CX
CC 9C
CH Multiple entry routine for the calculation of weighted averages.
CA On entry IFUN indicates which action is required:
CA          IFUN= 0 initialses an array SUMS for subsequent summing.
CA          IFUN= a positive integer adds an observation X with a  weight
CA                deduced from the value of IFUN:
CA                IFUN=1 gives unit weights
CA                IFUN=2 gives 1/DX squared
CA          IFUN= a negative integer returns the mean in X, the standard
CA                deviation in DX, and the number of observations in
CA                the 4th element of given array SUMS.
CA If an observation has zero DX, it is ignored.  -ve DX is treated as +ve.
C
CA SUMS is a 1x5 array holding values for an individual set of data, so that
CA      several sets may be collected simultaneously.
CA
CA SUMS HOLDS:
CA      Sum of weighted observations
CA      Sum of weighted squares of observations
CA      Sum of 1/DX squared
CA      Number of observations
CA      Denominator for mean
CA             If unit weights, this is the number of observations,
CA             If 1/DX sqrd weights this is the sum of the weights
C
C
      DIMENSION SUMS(5)
      IF (IFUN) 1,2,3
C
C  INITIALISE:
   2  CALL GMZER(SUMS,1,5)
      GO TO 100
C
C  ADD AN OBSERVATION:
   3  SUMS(4)=SUMS(4)+1.
      W=1.
      IF (DX .EQ. 0.) GO TO 100
      D=1./(DX)**2
      IF (IFUN .EQ. 2) W=D
      SUMS(1) = SUMS(1)+X*W
      SUMS(2) = SUMS(2)+X*(X*W)
      SUMS(3) = SUMS(3) + D
      SUMS(5) = SUMS(5) + W
      GO TO 100
C
C  CALCULATE MEAN AND SDEV:
   1  X = 0.
      DX = 0.
      IF (SUMS(4)) 100,100,8
    8 X = SUMS(1)/SUMS(5)
      DEL = 1./SUMS(3)
      IF (SUMS(4) .EQ.1.) GO TO 5
      DDX=SUMS(2)-SUMS(1)*X
      DX = DDX/(SUMS(5)*(SUMS(4)-1.))
    5 IF (DEL .GT. DX) DX = DEL
      DX=SQRT(DX)
 100  RETURN
      END
C
C
C
C
C LEVEL 2      FUNCTION ARCCOS(X)
      FUNCTION ARCCOS(X)
C
C *** ARCCOS FOR IBM by JCM 26 Sep 83 ***
C
CX
CC 10C
CH Calculates an arc cosine.
CA On entry X= a cosine
CA On exit ARCCOS is the arc cosine of X in radians in range 0 to Pi.
CN Written because IBM did not have it at the time.
C
      COMMON /CONSTA/PI,RAD,DEG,TWOPI,FOURPI,PIBY2,ALOG2,SQL2X8,VALMUB
C
      CALL SINCOS(X,Y,'ARCCOS')
      IF (ABS(X) .LT. 0.5) GO TO 1
      ARCCOS=ATAN(Y/X)
      GO TO 2
   1  ARCCOS=PIBY2-ATAN(X/Y)
   2  IF (ARCCOS .LT. 0.) ARCCOS=ARCCOS+PI
      RETURN
      END

      SUBROUTINE CALFUN(M,N,F,X)
      RETURN
      END

