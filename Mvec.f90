!*==ALNINT.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      FUNCTION ALNINT(A,B,X,N)
      FUNCTION ALNINT(A,B,X,N)
!
! *** ALNINT by JCM 21 May 85 ***
!
!X
!C 9C
!H Performs linear interpolation, suitable for profile backgrounds etc.
!
!A On entry:
!A     A is a real array of dimension N which holds the arguments
!A     B is a real array of dimension N which holds the function values
!A     X is a real which holds the argument for which the function value
!A       is required
!A     N is the integer dimension of A and B
!A
!A ALINT on exit will hold the function value for argument X
!
      DIMENSION A(N), B(N)
!
      DO I = 2, N
        IF (A(I).GT.X) GOTO 2
      ENDDO
      I = N
    2 ALNINT = (B(I-1)*(A(I)-X)-B(I)*(A(I-1)-X))/(A(I)-A(I-1))
      RETURN
      END FUNCTION ALNINT
!*==C1MSCA.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE C1MSCA(A,B,SCALE,NI,NJ)
      SUBROUTINE C1MSCA(A,B,SCALE,NI,NJ)
!
! *** C1MSCA by PJB Apr 87 ***
!
!X
!C 12C
!H Multiplies every element of the REAL matrix A by the COMPLEX scalar SCALE.
!A On entry A is a REAL matrix of dimension NIxNJ
!A          SCALE is a COMPLEX scalar
!A On exit  B is a scaled COMPLEX matrix being SCALE times A
!N B may be the same as A
!N Does nothing if NI or NJ is zero
!
      COMPLEX B(1), SCALE
      DIMENSION A(1)
      NIJ = NI*NJ
      DO I = 1, NIJ
        B(I) = SCALE*A(I)
      ENDDO
      RETURN
      END SUBROUTINE C1MSCA
!*==CGAMMA.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE CGAMMA(R,X,E,G)
      SUBROUTINE CGAMMA(R,X,E,G)
!
! *** CGAMMA/CGAMMS updated by PJB/JBF Dec 89 ***
!
!X
!C 9C
!H Solves the quadratic equation for gamma in terms of the flipping ratio.
!H The ENTRY CGAMMS is for when there is spin-independent multiple scattering.
!A On entry R is the flipping ratio,
!A          X is the polarisation,
!A          E the flipping efficiency,
!A    and in CGAMMS, RMS is the ratio of multiple to nuclear scattering.
!A On exit the positive root is in G(1) and the negative one is in G(2).
!N For polarised neutron data analysis.
!
      DOUBLE PRECISION Y, Z, A
      LOGICAL SIMS, SI
!
      DIMENSION G(2)
      SIMS = .FALSE.
      GOTO 1
!
      ENTRY CGAMMS(R,X,E,G,RMS)
      SIMS = .TRUE.
!
    1 Y = DBLE(R)
      Y = (Y-1.)/((Y*E+1.)*X)
      SI = DABS(Y).GE..005
      IF (SIMS) SI = SI .OR. (RMS.GE.0.005)
      IF (SI) THEN
        Y = 1./Y
        IF (DABS(Y).LE.1.) THEN
          G(1) = SIGN(1.,SNGL(Y))
          G(2) = G(1)
        ELSE
!
          A = Y*Y - 1.
          IF (SIMS) A = A + RMS
          Z = DSQRT(A)
          G(1) = Y + Z
          G(2) = Y - Z
        ENDIF
      ELSE
!
        IF (Y.LT.0.) THEN
          G(2) = 0.0
          G(1) = 0.5*Y
        ELSE
          G(2) = 0.5*Y
!* NOTE FROM JUDY - G(2) WILL BE +VE HERE ANYWAY
          G(1) = SIGN(99999.,G(2))
          IF (G(2).GT..00001) G(1) = 1/G(2)
        ENDIF
      ENDIF
      RETURN
      END SUBROUTINE CGAMMA
!*==CGMADD.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE CGMADD(A,B,C,NI,NJ)
      SUBROUTINE CGMADD(A,B,C,NI,NJ)
!
! *** CGMADD by JCM 20 Nov 87 ***
!
!X
!C 12C
!H Sets COMPLEX matrix C = COMPLEX matrix A + COMPLEX matrix B.
!A On entry A and B are NI by NJ COMPLEX matrices
!A On exit C is set to the sum of A and B
!
      COMPLEX A(NI,NJ), B(NI,NJ), C(NI,NJ)
      DO I = 1, NI
        DO J = 1, NJ
          C(I,J) = A(I,J) + B(I,J)
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE CGMADD
!*==CGMEQ.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE CGMEQ(A,B,N,M)
      SUBROUTINE CGMEQ(A,B,N,M)
!
! *** CGMEQ by JCM ***
!
!X
!C 12C
!H Sets COMPLEX matrix B equal to COMPLEX matrix A.
!A On entry A is a COMPLEX matrix of DIMENSION N,M
!A On exit B, a COMPLEX matrix of the same total size, has been set equal to A
!N Would not give error even if N or M were zero.
      COMPLEX A(1), B(1)
      NE = N*M
      DO I = 1, NE
        B(I) = A(I)
      ENDDO
      RETURN
      END SUBROUTINE CGMEQ
!*==CGMSCA.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE CGMSCA(A,B,SCALE,NI,NJ)
      SUBROUTINE CGMSCA(A,B,SCALE,NI,NJ)
!
! *** CGMSCA by PJB Apr 87 ***
!
!X
!C 12C
!H Multiplies every element of a COMPLEX matrix by a COMPLEX scale.
!A On entry, A holds a COMPLEX NI by NJ matrix
!A           SCALE holds a COMPLEX scalar scaling number
!A On exit,  B holds a COMPLEX NI by NJ matrix being A times SCALE
!N A may be the same as B
!N Note the existence of CMRSCA which works with a REAL scale
!
      COMPLEX A(1), B(1), SCALE
      NIJ = NI*NJ
      DO I = 1, NIJ
        B(I) = SCALE*A(I)
      ENDDO
      RETURN
      END SUBROUTINE CGMSCA
!*==CGMZER.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE CGMZER(A,NI,NJ)
      SUBROUTINE CGMZER(A,NI,NJ)
!
! *** CGMZER by JCM 23 Oct 87 ***
!
!X
!C 12C
!H Clears to zero a complex matrix.
!A On entry A is an NI by NJ COMPLEX matrix
!A On exit A is cleared to COMPLEX zero
!
      COMPLEX A(1)
      NIJ = NI*NJ
      DO I = 1, NIJ
        A(I) = CMPLX(0.,0.)
      ENDDO
      RETURN
      END SUBROUTINE CGMZER
!*==CMCONJ.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE CMCONJ(A,B,I,J)
      SUBROUTINE CMCONJ(A,B,I,J)
!
! *** CMCONJ by PJB Nov 89 ***
!
!X
!C 12C
!H Gives the conjugate of a COMPLEX matrix.
!A On entry A is a complex I by J matrix
!A On exit the I by J complex matrix B contains conjugate complex of A
!
      COMPLEX A(I,J), B(I,J)
      DO II = 1, I
        DO JJ = 1, J
          B(II,JJ) = CONJG(A(II,JJ))
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE CMCONJ
!*==CMRSCA.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE CMRSCA(A,B,SCALE,NI,NJ)
      SUBROUTINE CMRSCA(A,B,SCALE,NI,NJ)
!
! *** CMRSCA by JCM 23 Aug 88 ***
!
!X
!C 12C
!H Multiplies every element of a COMPLEX matrix by a REAL scale.
!A On entry, A holds a COMPLEX NI by NJ matrix
!A           SCALE holds a REAL scalar scaling number
!A On exit,  B holds a COMPLEX NI by NJ matrix being A times SCALE
!N A may be the same as B
!N Note also the existence of CGMSCA which works entirely in COMPLEX.
!
      COMPLEX A(1), B(1)
      NIJ = NI*NJ
      DO I = 1, NIJ
        B(I) = SCALE*A(I)
      ENDDO
      RETURN
      END SUBROUTINE CMRSCA
!*==DEGREE.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      FUNCTION DEGREE(X)
      FUNCTION DEGREE(X)
!
! *** DEGREE by JCM ***
!
!X
!C 10C
!H Converts from radians to degrees.
!A On entry X is an angle in radians
!A On exit DEGREE is the angle in degrees
!
!N The function RADIAN(X) does the degrees to radians conversion.
!N In routines where time matters, it is quicker to declare the COMMON
!N /CONSTA/ and multiply X by DEG.
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
!
      DEGREE = DEG*X
      RETURN
      END FUNCTION DEGREE
!*==DETER3.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      FUNCTION DETER3(A)
      FUNCTION DETER3(A)
!
! *** DETER3 by JCM 28 Jun 83 ***
!
!X
!C 12C
!H Forms the determinant of a 3 x 3 matrix.
!A On entry A is a 3x3 real matrix.
!A On exit DETER3 holds its determinant.
!
      DIMENSION A(3,3)
      D = 0.
      J = 2
      K = 3
      DO I = 1, 3
        D = D + A(1,I)*(A(2,J)*A(3,K)-A(2,K)*A(3,J))
        J = K
        K = I
      ENDDO
      DETER3 = D
      RETURN
      END FUNCTION DETER3
!*==ERFNC.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      FUNCTION ERFNC(X)
      FUNCTION ERFNC(X)
!
! *** ERFNC by WIFD 22 Aug 85 ***
!
!X
!C 9C
!H Calculates the error function accurate to 3E-7, for + and - X.
!A On entry X is the argument at which the function is required.
!A On exit ERFNC holds the function.
!D See Abramovitz and Stegun p.299
!
      DATA A1, A2, A3/0.0705230784, 0.0422820123, 0.0092705272/
      DATA A4, A5, A6/0.0001520143, 0.0002765672, 0.0000430638/
!  THESE COEFFICIENTS WERE TAKEN FROM ABRAMOVITZ AND STEGUN P.299
!  AND GIVE ERFNC ACCURATE TO 3E-07 FOR BOTH +VE AND -VE X.
!
      Z = ABS(X)
      ZZ = Z*Z
      ZZZ = Z*Z*Z
      E = 1.0 + A1*Z + A2*ZZ + A3*ZZZ + A4*ZZ*ZZ + A5*ZZ*ZZZ +          &
     &    A6*ZZZ*ZZZ
      IF (E.LT.10.0) GOTO 1
      E = 0.0
      GOTO 2
    1 E = 1.0/E**16
      IF (E.LE.0.0) E = 0.0
    2 IF (X.LT.0.0) E = 2.0 - E
!
      ERFNC = E
      RETURN
      END FUNCTION ERFNC
!*==FF01A.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE FF01A(VJS,VYS,XS,N)
      SUBROUTINE FF01A(VJS,VYS,XS,N)
!
! *** FF01A from HARWELL LIBRARY modified by JCM 17 Jan 85 ***
!
!X
!C 9C
!H Modified Harwell routine for zero order Bessel functions.
!
      DOUBLE PRECISION A(18), B(19), C(18), D(18)
      DOUBLE PRECISION XLG
      DOUBLE PRECISION VJ0, VY0, X, Y, Z, Q1, Q2, Q3, FX, X1, X2, X3, X4
      DOUBLE PRECISION ABCD(73)
      EQUIVALENCE (ABCD(1),A(1))
      EQUIVALENCE (ABCD(19),B(1))
      EQUIVALENCE (ABCD(38),C(1))
      EQUIVALENCE (ABCD(56),D(1))
      DATA A/ - .17D-18, .1222D-16, -.75885D-15, .4125321D-13,          &
     &     -.194383469D-11, .7848696314D-10, -.267925353056D-8,         &
     &     .7608163592419D-7, -.176194690776215D-5,                     &
     &     .3246032882100508D-4, -.4606261662062751D-3,                 &
     &     .4819180069467605D-2, -.3489376941140889D-1,                 &
     &     .1580671023320973D0, -.3700949938726498D0,                   &
     &     .2651786132033368D0, -.8723442352852221D-2,                  &
     &     .3154559429497802D0/
      DATA B/ - .1D-19, .39D-18, -.2698D-16, .164349D-14, -.8747341D-13,&
     &     .402633082D-11, -.15837552542D-9, .524879478733D-8,          &
     &     -.14407233274019D-6, .32065325376548D-5,                     &
     &     -.5632079141056987D-4, .7531135932577742D-3,                 &
     &     -.7287962479552079D-2, .4719668959576339D-1,                 &
     &     -.1773020127811436D0, .2615673462550466D0,                   &
     &     .179034314077183D0, -.2744743055297453D0,                    &
     &     -.6629222640656988D-1/
      DATA C/ - .1D-19, .2D-19, -.11D-18, .55D-18, -.288D-17, .1631D-16,&
     &     -.10012D-15, .67481D-15, -.506903D-14, .4326596D-13,         &
     &     -.43045789D-12, .516826239D-11, -.7864091377D-10,            &
     &     .163064646352D-8, -.5170594537606D-7, .307518478751947D-5,   &
     &     -.5365220468132117D-3, .1998920698695037D1/
      DATA D/.1D-19, -.3D-19, .13D-18, -.62D-18, .311D-17, -.1669D-16,  &
     &     .9662D-16, -.60999D-15, .425523D-14, -.3336328D-13,          &
     &     .30061451D-12, -.320674742D-11, .4220121905D-10,             &
     &     -.72719159369D-9, .1797245724797D-7, -.74144984110606D-6,    &
     &     .683851994261165D-4, -.3111170921067402D-1/
!
!VMS
      DATA XLG/1.7D+38/
!3084      XLG=(1.0D0 - 16.0D0**(-14)) * 16.0D0**(+63)
      X = DBLE(XS)
      Y = DABS(X)
      Z = Y*.125D0
      IF (Z.GT.1D0) GOTO 1
      IF (Z.EQ.0D0) GOTO 2
      X2 = 4D0*Z*Z - 2D0
      JUMP = 1
      N1 = 1
      N2 = 18
    3 Q3 = 0D0
      Q2 = 0D0
      DO I = N1, N2
        Q1 = Q2
        Q2 = Q3
        Q3 = X2*Q2 - Q1 + ABCD(I)
      ENDDO
      FX = (Q3-Q1)*.5D0
      GOTO (10,11,12,13), JUMP
   10 VJ0 = FX
      VJS = SNGL(VJ0)
      IF (N.LE.0) GOTO 100
      JUMP = 2
      N1 = 19
      N2 = 37
      GOTO 3
!
    2 VJ0 = 1D0
      VY0 = -XLG
      GOTO 101
!
   11 VY0 = .6366197723675813D0*DLOG(Y)*VJ0 + FX
      VYS = SNGL(VY0)
      GOTO 100
!
    1 Z = 1D0/Z
      X2 = 4D0*Z*Z - 2D0
      JUMP = 3
      N1 = 38
      N2 = 55
      GOTO 3
!
   12 X1 = FX
      JUMP = 4
      N1 = 56
      N2 = 73
      GOTO 3
!
   13 X2 = DCOS(Y-.7853981633974483D0)
      X3 = DSIN(Y-.7853981633974483D0)
      X4 = .7978845608028654D0/DSQRT(Y)
      FX = FX*Z
      VJ0 = X4*(X1*X2-FX*X3)
      VY0 = X4*(FX*X2+X1*X3)
  101 VJS = SNGL(VJ0)
      VYS = SNGL(VY0)
  100 RETURN
      END SUBROUTINE FF01A
!*==FF02A.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE FF02A(VJS,VYS,S,N)
      SUBROUTINE FF02A(VJS,VYS,S,N)
!
! *** FF02A from HARWELL LIBRARY modified by JCM 17 Jan 85 ***
!
!X
!C 9C
!H Modified Harwell routine for first order Bessel functions.
!
      DOUBLE PRECISION A(18), B(18), C(18), D(18), ABCD(72), XLG
      DOUBLE PRECISION VJ1, VY1, X, Y, Z, Q1, Q2, Q3, FX, X1, X2, X3, X4
      EQUIVALENCE (ABCD(1),A(1))
      EQUIVALENCE (ABCD(19),B(1))
      EQUIVALENCE (ABCD(37),C(1))
      EQUIVALENCE (ABCD(55),D(1))
      DATA A/ - .4D-19, .295D-17, -.19554D-15, .1138572D-13,            &
     &     -.57774042D-12, .2528123664D-10, -.94242129816D-9,           &
     &     .2949707007278D-7, -.76175878054003D-6, .1588701923993213D-4,&
     &     -.2604443893485807D-3, .3240270182683858D-2,                 &
     &     -.2917552480615421D-1, .1777091172397283D0,                  &
     &     -.6614439341345433D0, .1287994098857678D1,                   &
     &     -.1191801160541217D1, .1296717541210530D1/
      DATA B/.9D-19, -.658D-17, .42773D-15, -.2440949D-13,              &
     &     .121143321D-11, -.5172121473D-10, .187547032473D-8,          &
     &     -.5688440039919D-7, .141662436449235D-5,                     &
     &     -.283046401495148D-4, .4404786298670995D-3,                  &
     &     -.5131641161061085D-2, .4231918035333690D-1,                 &
     &     -.2266249915567549D0, .6756157807721877D0,                   &
     &     -.7672963628866459D0, -.1286973843813500D0,                  &
     &     .4060821177186851D-1/
      DATA C/.1D-19, -.2D-19, .12D-18, -.58D-18, .305D-17, -.1731D-16,  &
     &     .10668D-15, -.72212D-15, .545267D-14, -.4684224D-13,         &
     &     .46991955D-12, -.570486364D-11, .881689866D-10,              &
     &     -.187189074911D-8, .6177633960644D-7, -.398728430048891D-5,  &
     &     .8989898330859409D-3, .2001806081720027D1/
      DATA D/ - .1D-19, .3D-19, -.14D-18, .65D-18, -.328D-17, .1768D-16,&
     &     -.10269D-15, .65083D-15, -.456125D-14, .3596777D-13,         &
     &     -.32643157D-12, .351521879D-11, -.4686363688D-10,            &
     &     .82291933277D-9, -.2095978138408D-7, .91386152579555D-6,     &
     &     -.9627723549157079D-4, .9355557413907065D-1/
!
!VMS
      DATA XLG/1.7D+38/
!3084      XLG=(1.0D0 - 16.0D0**(-14)) * 16.0D0**(+63)
      X = DBLE(S)
      Y = DABS(X)
      Z = Y*.125D0
      IF (Z.GT.1D0) GOTO 1
      IF (Z.LE.0D0) GOTO 2
      X2 = 4D0*Z*Z - 2D0
      JUMP = 1
      N1 = 1
      N2 = 18
    3 Q3 = 0D0
      Q2 = 0D0
      DO I = N1, N2
        Q1 = Q2
        Q2 = Q3
        Q3 = X2*Q2 - Q1 + ABCD(I)
      ENDDO
      FX = (Q3-Q1)*.5D0
      GOTO (10,11,12,13), JUMP
!
   10 VJ1 = FX*Z
      VJS = SNGL(VJ1)
      IF (N.LE.0) GOTO 100
      JUMP = 2
      N1 = 19
      N2 = 36
      GOTO 3
    2 VJ1 = 0D0
      VY1 = -XLG
      GOTO 101
!
   11 VY1 = .6366197723675813D0*(DLOG(Y)*VJ1-1D0/Y) + FX*Z
      VYS = SNGL(VY1)
      GOTO 100
!
    1 Z = 1D0/Z
      X2 = 4D0*Z*Z - 2D0
      JUMP = 3
      N1 = 37
      N2 = 54
      GOTO 3
!
   12 X1 = FX
      JUMP = 4
      N1 = 55
      N2 = 72
      GOTO 3
!
   13 X2 = DCOS(Y-2.356194490192345D0)
      X3 = DSIN(Y-2.356194490192345D0)
      X4 = .7978845608028654D0/DSQRT(Y)
      FX = FX*Z
      VJ1 = X4*(X1*X2-FX*X3)
      VY1 = X4*(FX*X2+X1*X3)
  101 VJS = SNGL(VJ1)
      VYS = SNGL(VY1)
  100 RETURN
      END SUBROUTINE FF02A
!*==FRAC3.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 2      SUBROUTINE FRAC3(VEC)
      SUBROUTINE FRAC3(VEC)
!
! *** FRAC3 by JCM 11 Nov 83 ***
!
!X
!C 1C
!H Makes all 3 elements of a vector fractional
!A On entry VEC is a 1x3 real vector.
!A On exit the elements of VEC have each been put into the range 0 =< X < 1
!
      DIMENSION VEC(3)
!
      DO I = 1, 3
        CALL FRACT(VEC(I),A,J)
      ENDDO
      RETURN
      END SUBROUTINE FRAC3
!*==FRACT.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE FRACT(X,Y,N)
      SUBROUTINE FRACT(X,Y,N)
!
! *** FRACT by JCM ***
!
!X
!C 9C
!H Forms the fractional part of a real number.
!A On entry X is a real number
!A On exit  X is in the range 0=< X <1
!A          Y is set so that X+Y=original X
!A          N= 0 if X was unchanged
!A           = 1 if X was >= 1
!A           =-1 if X was < 0
!
      Y = 0.
      N = 1
      IF (X.GE.1.) GOTO 1
      N = 0
      IF (X.GE.0.) GOTO 100
! OUT IN USUAL CASE OF X ALREADY BEING +VE FRACTION
!
      N = -1
    1 Y = AINT(X)
      IF (N.EQ.-1) Y = Y - 1.0
      X = X - Y
  100 RETURN
      END SUBROUTINE FRACT
!*==FT01A.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1    SUBROUTINE FT01A(IT,INV,TR,TI)
      SUBROUTINE FT01A(IT,INV,TR,TI)
!
! *** FT01A updated by JCM FROM HARWELL ROUTINE 9 Sep 91 ***
!
!X
!C 9C
!H Modification of Harwell Fast Fourier Transform.
!
      DIMENSION TR(1024), TI(1024)
      EXTERNAL FFTADD
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /FFTDA / KJUMP, UR(15), UI(15)
!
      GOTO (1,2), KJUMP
    1 UM = 0.5
      DO I = 1, 15
        UM = 0.5*UM
        TH = TWOPI*UM
        UR(I) = COS(TH)
        UI(I) = SIN(TH)
      ENDDO
      KJUMP = 2
!
! SECOND AND SUBSEQUENT ENTRIES:
    2 UM = 1.
      IF (INV.EQ.1) UM = -1.
      IO = 2
      DO I = 2, 16
        IO = IO + IO
        IF (IO-IT) 3, 4, 99
    3 ENDDO
! ERROR EXIT - IT NOT A POWER OF 2, OR TOO BIG:
   99 INV = -1
      GOTO 100
!
    4 IO = I
      II = IO
      I1 = IT/2
      I3 = 1
   10 K = 0
      I2 = I1 + I1
   11 WR = 1.
      WI = 0.
      KK = K
      JO = IO
!
   12 IF (KK.EQ.0) GOTO 13
   14 JO = JO - 1
      KK1 = KK
      KK = KK/2
      IF (KK1.EQ.2*KK) GOTO 14
      WS = WR*UR(JO) - WI*UI(JO)
      WI = WR*UI(JO) + WI*UR(JO)
      WR = WS
      GOTO 12
!
   13 WI = WI*UM
      J = 0
!
    9 L = J*I2 + K
      L1 = L + I1
      ZR = TR(L+1) + TR(L1+1)
      ZI = TI(L+1) + TI(L1+1)
      Z = WR*(TR(L+1)-TR(L1+1)) - WI*(TI(L+1)-TI(L1+1))
      TI(L1+1) = WR*(TI(L+1)-TI(L1+1)) + WI*(TR(L+1)-TR(L1+1))
      TR(L+1) = ZR
      TR(L1+1) = Z
      TI(L+1) = ZI
      J = J + 1
      IF (J.LT.I3) GOTO 9
      K = K + 1
      IF (K.LT.I1) GOTO 11
      I3 = I3 + I3
      IO = IO - 1
      I1 = I1/2
      IF (I1.GT.0) GOTO 10
      J = 1
      UM = 1.
      IF (INV.EQ.1) UM = 1./FLOAT(IT)
!
    7 K = 0
      J1 = J
      DO I = 1, II
        J2 = J1/2
        K = 2*(K-J2) + J1
        J1 = J2
      ENDDO
!
      IF (K.GE.J) THEN
        IF (K.EQ.J) THEN
          TR(J+1) = TR(J+1)*UM
          TI(J+1) = TI(J+1)*UM
        ELSE
          ZR = TR(J+1)
          ZI = TI(J+1)
          TR(J+1) = TR(K+1)*UM
          TI(J+1) = TI(K+1)*UM
          TR(K+1) = ZR*UM
          TI(K+1) = ZI*UM
        ENDIF
      ENDIF
      J = J + 1
      IF (J.LT.IT-1) GOTO 7
      TR(1) = TR(1)*UM
      TI(1) = TI(1)*UM
      TR(IT) = TR(IT)*UM
      TI(IT) = TI(IT)*UM
  100 RETURN
      END SUBROUTINE FT01A
!*==GMADD.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE GMADD(A,B,C,NI,NJ)
      SUBROUTINE GMADD(A,B,C,NI,NJ)
!
! *** GMADD by JCM ***
!
!X
!C 12C
!H Sets matrix C = matrix A plus matrix B.
!A On entry A and B are real matrices of dimension (NIxNJ)
!A On exit  C is a real matrix which is their sum.
!N NI and NJ must be at least 1
!
      DIMENSION A(NI,NJ), B(NI,NJ), C(NI,NJ)
      DO I = 1, NI
        DO J = 1, NJ
          C(I,J) = A(I,J) + B(I,J)
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE GMADD
!*==GMEQ.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE GMEQ(A,B,NI,NJ)
      SUBROUTINE GMEQ(A,B,NI,NJ)
!
! *** GMEQ by PJB/JCM 28 Jun 83 ***
!
!X
!C 12C
!H Sets matrix B = matrix A.
!A On entry A is a real matrix of dimension NIxNJ
!A On exit  B is a real matrix equal to A
!N NI and NJ must be at least 1
!
      DIMENSION A(NI,NJ), B(NI,NJ)
      DO I = 1, NI
        DO J = 1, NJ
          B(I,J) = A(I,J)
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE GMEQ
!*==GMINV.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 3      SUBROUTINE GMINV(A,B,N)
      SUBROUTINE GMINV(A,B,N)
!
! *** GMINV by JCM from SID 11 Oct 88 ***
!
!X
!C 12C
!H Inverts matrix A into matrix B.
!A On entry A is a square NxN real matrix
!A On exit  B is its inverse
!D Based on SID
!
      DIMENSION II(100), IL(100), IG(100), A(N,N), B(N,N)
!
      CALL GMEQ(A,B,N,N)
      D = 1.
      IS = N - 1
      DO K = 1, N
        IL(K) = 0
        IG(K) = K
      ENDDO
!
      DO K = 1, N
        R = 0.
        DO I = 1, N
          IF (IL(I).NE.0) GOTO 40
          W = B(I,K)
          X = ABS(W)
          IF (R.GT.X) GOTO 40
          R = X
          P = W
          KF = I
   40   ENDDO
        II(K) = KF
        IL(KF) = KF
        D = D*P
        IF (D.EQ.0.) CALL ERRMES(1,0,'Zero determinant')
!
        DO I = 1, N
          IF (I.EQ.KF) THEN
            B(I,K) = 1./P
          ELSE
            B(I,K) = -B(I,K)/P
          ENDIF
        ENDDO
!
        DO J = 1, N
          IF (J.EQ.K) GOTO 140
          W = B(KF,J)
          IF (W.EQ.0.) GOTO 140
          DO I = 1, N
            IF (I.EQ.KF) THEN
              B(I,J) = W/P
            ELSE
              B(I,J) = B(I,J) + W*B(I,K)
            ENDIF
          ENDDO
  140   ENDDO
!
      ENDDO
!.....
!
      DO K = 1, IS
        KF = II(K)
        KL = IL(KF)
        KG = IG(K)
        IF (KF.EQ.KG) GOTO 190
        DO I = 1, N
          R = B(I,KF)
          B(I,KF) = B(I,KG)
          B(I,KG) = R
        ENDDO
        DO J = 1, N
          R = B(K,J)
          B(K,J) = B(KL,J)
          B(KL,J) = R
        ENDDO
        IL(KF) = K
        IL(KG) = KL
        IG(KL) = IG(K)
        IG(K) = KF
        D = -D
  190 ENDDO
      RETURN
      END SUBROUTINE GMINV
!*==GMPRD.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE GMPRD(A,B,C,NI,NJ,NK)
      SUBROUTINE GMPRD(A,B,C,NI,NJ,NK)
!
! *** GMPRD by JCM ***
!
!X
!C 12C
!H Sets matrix C = matrix A times matrix B.
!A On entry A is a real NIxNJ matrix
!A          B is a real NJxNK matrix
!A On exit  C is a real NIxNK matrix holding A times B
!
      DIMENSION A(1), B(1), C(1)
      DO I = 1, NI
        IK = I
        JK = 1
        DO K = 1, NK
          IJ = I
          C(IK) = 0.
          DO J = 1, NJ
            C(IK) = C(IK) + A(IJ)*B(JK)
            IJ = IJ + NI
            JK = JK + 1
          ENDDO
          IK = IK + NI
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE GMPRD
!*==GMREV.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE GMREV(A,B,NI,NJ)
      SUBROUTINE GMREV(A,B,NI,NJ)
!
! *** GMREV by PJB/JCM 28 Jun 83 ***
!
!X
!C 12C
!H Reverses the signs of the elements of an NI X NJ matrix.
!A On entry A is a real matrix of dimension NIxNJ
!A On exit  B is a real matrix holding -A
!N A and B may be the same matrix
!
      DIMENSION A(NI,NJ), B(NI,NJ)
      DO I = 1, NI
        DO J = 1, NJ
          B(I,J) = -A(I,J)
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE GMREV
!*==GMSAME.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      LOGICAL FUNCTION GMSAME(A,B,N,TOLER)
      LOGICAL FUNCTION GMSAME(A,B,N,TOLER)
!
! *** GMSAME by JCM 22 Oct 86 ***
!
!X
!C 11C
!H Tells whether one vector is the same as another, to a given tolerance.
!
!A A on entry is an N-sized array, to be compared with:
!A B, also an N-sized array (A and B may of course be parts of larger arrays)
!A N on entry is the number of elements of A and B to compare
!A TOLER on entry is the number within which all the elements of A and B
!A      must agree
!A
!A GMSAME will be set .TRUE. if all elements of A and B agree within TOLER,
!A      and .FALSE. otherwise
!
      DIMENSION A(N), B(N)
!
      GMSAME = .FALSE.
      DO I = 1, N
        IF (ABS(A(I)-B(I)).GT.TOLER) GOTO 100
      ENDDO
      GMSAME = .TRUE.
  100 RETURN
      END FUNCTION GMSAME
!*==GMSCA.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE GMSCA(A,B,SCALE,NI,NJ)
      SUBROUTINE GMSCA(A,B,SCALE,NI,NJ)
!
! *** GMSCA by JCM 22 Nov 84 ***
!
!X
!C 12C
!H Multiplies every element of the matrix A by the scalar SCALE.
!A On entry A is a real matrix of dimension NIxNJ
!A          SCALE is the required multiplying scale
!A On exit  B is a real matrix whose elements are those of A times SCALE.
!N A and B may be the same matrix
!
      DIMENSION A(1), B(1)
      NIJ = NI*NJ
      DO I = 1, NIJ
        B(I) = SCALE*A(I)
      ENDDO
      RETURN
      END SUBROUTINE GMSCA
!*==GMSUB.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE GMSUB(A,B,C,NI,NJ)
      SUBROUTINE GMSUB(A,B,C,NI,NJ)
!
! *** GMSUB by PJB/JCM 28 Jun 83 ***
!
!X
!C 12C
!H Sets matrix C = matrix A minus matrix B.
!A On entry A and B are real matrices of dimension NIxNJ
!A On exit  C is a real matrix whose elements are those of A-B
!
      DIMENSION A(1), B(1), C(1)
      NIJ = NI*NJ
      DO I = 1, NIJ
        C(I) = A(I) - B(I)
      ENDDO
      RETURN
      END SUBROUTINE GMSUB
!*==GMTRAN.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE GMTRAN(A,B,JJ,II)
      SUBROUTINE GMTRAN(A,B,JJ,II)
!
! *** GMTRAN by PJB ***
!
!X
!C 12C
!H Transposes a JJxII matrix A into B.
!A On entry A is a real matrix of dimension (JJxII)
!A On exit  B is a real matrix of dimension (IIxJJ) which is the transpose of A
!
      DIMENSION A(JJ,II), B(II,JJ)
!
      DO J = 1, JJ
        DO I = 1, II
          B(I,J) = A(J,I)
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE GMTRAN
!*==GMUNI.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE GMUNI(A,NI)
      SUBROUTINE GMUNI(A,NI)
!
! *** GMUNI by JCM 7 Jul 83 ***
!
!X
!C 12C
!H Writes a unit matrix into the square matrix A.
!A On entry NI is the dimension of the required matrix
!A On exit  A is a square real matrix of dimension NIxNI, set to a unit matrix
!
      DIMENSION A(NI,NI)
      DO I = 1, NI
        A(I,I) = 1.
        I1 = I + 1
        IF (I1.GT.NI) GOTO 100
        DO J = I1, NI
          A(I,J) = 0.
          A(J,I) = 0.
        ENDDO
      ENDDO
  100 RETURN
      END SUBROUTINE GMUNI
!*==GMZER.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE GMZER(A,NI,NJ)
      SUBROUTINE GMZER(A,NI,NJ)
!
! *** GMZER by JCM 7 Jul 83 ***
!
!X
!C 12C
!H Clears to zero the matrix A.
!A On entry NI and NJ specify A to be of dimension NIxNJ
!A On exit  A is a real matrix of dimension NIxNJ all cleared to zero.
!
      DIMENSION A(1)
      NIJ = NI*NJ
      DO I = 1, NIJ
        A(I) = 0.
      ENDDO
      RETURN
      END SUBROUTINE GMZER
!*==JGMEQ.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE JGMEQ(JA,JB,NI,NJ)
      SUBROUTINE JGMEQ(JA,JB,NI,NJ)
!
! *** JGMEQ by JCM  Jun 88 ***
!
!X
!C 12C
!H Equates an integer matrix to a given integer matrix.
!A On entry JA is an integer matrix of dimension NIxNJ
!A On exit  JB = integer matrix being a copy of JA
!
      DIMENSION JA(NI,NJ), JB(NI,NJ)
      DO I = 1, NI
        DO J = 1, NJ
          JB(I,J) = JA(I,J)
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE JGMEQ
!*==JGMZER.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE JGMZER(JA,NI,NJ)
      SUBROUTINE JGMZER(JA,NI,NJ)
!
! *** JGMZER by JCM Jun 88 ***
!
!X
!C 12C
!H Clears an integer matrix to zero.
!A On entry NI and NJ are the dimensions of the required matrix
!A On exit  the integer matrix JA, of dimensions NIxNJ is cleared to zero.
!
      DIMENSION JA(1)
      NIJ = NI*NJ
      DO I = 1, NIJ
        JA(I) = 0
      ENDDO
      RETURN
      END SUBROUTINE JGMZER
!*==RADIAN.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      FUNCTION RADIAN(X)
      FUNCTION RADIAN(X)
!
! *** RADIAN by JCM ***
!
!X
!C 10C
!H Converts from degrees to radians.
!A On entry X is the value of an angle in degrees
!D On exit  X has been converted to radians.
!N The function DEGREE(X) does the radians to degrees conversion
!N In routines where time matters, it is quicker to declare the COMMON
!N /CONSTA/ and multiply X by RAD.
!
      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      RADIAN = RAD*X

      END FUNCTION RADIAN
!*==RCMPRD.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE RCMPRD(A,B,C,I,J,K)
      SUBROUTINE RCMPRD(A,B,C,I,J,K)
!
! *** RCMPRD by PJB Jun 87 ***
!
!X
!C 12C
!H Performs the multiplication of a COMPLEX by a REAL matrix.
!A On entry A is a real IxJ matrix,
!A          B is a complex JxK matrix,
!A On exit  C is returned as the complex IxK matrix A*B
!
      DIMENSION A(1)
      COMPLEX B(1), C(1)
!
      DO II = 1, I
        IK = II
        JK = 1
        DO KK = 1, K
          IJ = II
          C(IK) = 0.
          DO JJ = 1, J
            C(IK) = C(IK) + A(IJ)*B(JK)
            IJ = IJ + I
            JK = JK + 1
          ENDDO
          IK = IK + I
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE RCMPRD
!*==RSCALP.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      FUNCTION RSCALP(A,B)
      FUNCTION RSCALP(A,B)
!
! *** RSCALP by PJB Jun 87 ***
!
!X
!C 12C
!H Forms the scalar product of two COMPLEX vectors.
!A On entry A and B are complex vectors of dimension 3
!D The function value is the real product (A.Conjg(B) + Conjg(A).B)/2
!
      COMPLEX A(3), B(3), P
      P = CMPLX(0.,0.)
      DO I = 1, 3
        P = P + A(I)*CONJG(B(I))
      ENDDO
      RSCALP = REAL(P)
      RETURN
      END FUNCTION RSCALP
!*==SCALPR.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 2      FUNCTION SCALPR(A,B)
      FUNCTION SCALPR(A,B)
!
! *** SCALPR by JCM ***
!
!X
!C 12C
!H Forms the scalar product of two orthogonal vectors (or of one from
!H real space with one from reciprocal space).
!A On entry A and B are vectors of dimension 3
!D The function is set to the scalar product of A and B (not involving
!D  cell parameters) e.g. with one vector from real & one from reciprocal
!D  space
!N The function SCLPRD gives the scalar product using the cell parameters.
!
      DIMENSION A(3), B(3), C(1)
      CALL GMPRD(A,B,C,1,3,1)
      SCALPR = C(1)
      RETURN
      END FUNCTION SCALPR
!*==SINCOS.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE SINCOS(SN,CS,L1)
      SUBROUTINE SINCOS(SN,CS,L1)
!
! *** SINCOS updated by JCM 1 Jul 86 ***
!
!X
!C 10C
!H Calculates sin from cos or vice-versa.
!A On entry SN is a sine or cosine of an angle
!A On exit  CS is the cosine or sine respectively of the same angle
!A          L1 is a character variable which is used to identify the
!A             calling routine if the absolute value of SN is greater
!A             than 1.
!N If ABS(SN)>1 the routine writes an error message and STOPS
!
      CHARACTER*(*) L1
      COMMON /IOUNIT/ LPT, ITI, ITO, IPLO, LUNI, IOUT
!
      IF (ABS(SN)-10.E-5.GT.1.) THEN
        WRITE (LPT,3000) SN, L1
        WRITE (ITO,3000) SN, L1
!>> JCC        STOP
        CALL BMBOUT
        RETURN
      ENDIF
!
!>> JCC There can be an error raised here. SN can be passed
!>> As a NaN, due to a corruption in the CELL array. Please check
!>>
! NOW DETECT CASE WHERE SN IS ONLY JUST MORE THAN 1:
      IF (ABS(SN).GE.1.0) THEN
        SN = SIGN(1.0,SN)
        CS = 0.
      ELSE
!
! USUAL CASE:
        CS = SQRT(1.-SN*SN)
      ENDIF
      RETURN
 3000 FORMAT (/' Sin or Cos value',E12.4,' greater than unity',         &
     &        ' - called from ',A6)
      END SUBROUTINE SINCOS
!*==SPLINE.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE SPLINE(N,X,F,D)
      SUBROUTINE SPLINE(N,X,F,D)
!
! *** SPLINE from HARWELL TB04A 21 May 85 ***
!
!X
!C 9A
!H Sets up a cubic spline to fit a curve.
!A On entry N is the number of points on a curve
!A          F(I) are the N function values at the N points X(I)
!A          D(I) is set to the derivative if the spline at the point I
!A              for all N points.
!N Uses working space in COMMON /SCRAT/
!N For further details see the description of the Harwell Library
!
      DIMENSION X(N), F(N), D(N)
      COMMON /IOUNIT/ LPT, ITI, ITO, IPLO, LUNI, IOUT
      COMMON /SCRAT / A(2000)
!
! F(I) ARE THE FUNCTION VALUES AT THE POINTS X(I) FOR I=1,N AND
! THE SPLINE DERIVATIVES D(I) ARE FOUND.  THE DIMENSION OF A MUST
! NOT BE LESS THAN 3*N.
!
      DO I = 2, N
        IF (X(I)-X(I-1)) 1, 1, 5
    1   WRITE (LPT,3000) I
 3000   FORMAT (/'  ERROR ** in SPLINE - X',I3,' out of order')
        A(1) = 1.
        GOTO 100
!
    5 ENDDO
!
      DO I = 1, N
        J = 2
        IF (I-1) 6, 10, 6
    6   J = N - 1
        IF (I.EQ.N) GOTO 10
        H1 = 1./(X(I)-X(I-1))
        H2 = 1./(X(I+1)-X(I))
        A(3*I-2) = H1
        A(3*I-1) = 2.*(H1+H2)
        A(3*I) = H2
        D(I) = 3.0*(F(I+1)*H2*H2+F(I)*(H1*H1-H2*H2)-F(I-1)*H1*H1)
        GOTO 2
!
   10   H1 = 1./(X(J)-X(J-1))
        H2 = 1./(X(J+1)-X(J))
        A(3*I-2) = H1*H1
        A(3*I-1) = H1*H1 - H2*H2
        A(3*I) = -H2*H2
        D(I) = 2.*(F(J)*(H2*H2*H2+H1*H1*H1)-F(J+1)*H2*H2*H2-F(J-1)      &
     &         *H1*H1*H1)
    2 ENDDO
      P = A(4)/A(1)
      A(5) = A(5) - P*A(2)
      A(6) = A(6) - P*A(3)
      D(2) = D(2) - P*D(1)
      DO I = 3, N
        K = 3*I - 4
        P = A(K+2)/A(K)
        A(K+3) = A(K+3) - P*A(K+1)
        D(I) = D(I) - P*D(I-1)
        IF (I.NE.N-1) GOTO 4
        P = A(K+5)/A(K)
        A(K+5) = A(K+6) - P*A(K+1)
        A(K+6) = A(K+7)
        D(N) = D(N) - P*D(N-2)
    4 ENDDO
!
      D(N) = D(N)/A(3*N-1)
      DO I = 3, N
        J = N + 2 - I
        D(J) = (D(J)-A(3*J)*D(J+1))/A(3*J-1)
      ENDDO
      D(1) = (D(1)-D(2)*A(2)-D(3)*A(3))/A(1)
      A(1) = 0.
  100 RETURN
      END SUBROUTINE SPLINE
!*==SPLINT.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      FUNCTION SPLINT(IX,N,U,S,D,X)
      FUNCTION SPLINT(IX,N,U,S,D,X)
!
! *** SPLINT from HARWELL TG01B 21 May 85 ***
!
!X
!C 9C
!H Evaluates a cubic spline given spline values and first derivative
!H values at the given knots.
!
!
!A On entry IX allows the caller to take advantage of spline parameters
!A            set on a previous call in cases when an X point follows
!A            the previous X point.
!A      If IX < 0 the whole range is searched for knot interval;
!A      If IX > 0 it is assumed that X is greater than the X of the
!A      previous call and search started from there.
!A           N=The number of knots.
!A           U holds the knots.
!A           S holds the spline values.
!A           D holds the first derivative values of the spline at the
!A             knots.
!A           X=the point at which the spline value is required.
!
!P A call of SPLINE must have set up the knots in D.
!
!N The spline value is defined as zero outside the knot range, which
!N is extended by a rounding error for the purpose.
!
!     ALLOWABLE ROUNDING ERROR ON POINTS AT EXTREMES OF KNOT RANGE
!     IS 2**IEPS*MAX(\U(1)\,\U(N)\).
      DIMENSION U(N), S(N), D(N)
      DATA IFLG, IEPS/0, -19/
!
!       TEST WETHER POINT IN RANGE.
      IF (X.LT.U(1)) GOTO 3
      IF (X.GT.U(N)) GOTO 4
!
!       JUMP IF KNOT INTERVAL REQUIRES RANDOM SEARCH.
      IF (IX.LT.0 .OR. IFLG.EQ.0) GOTO 12
!       JUMP IF KNOT INTERVAL SAME AS LAST TIME.
      IF (X.LE.U(J+1)) GOTO 8
!       LOOP TILL INTERVAL FOUND.
    1 J = J + 1
   11 IF (X.GT.U(J+1)) GOTO 1
      GOTO 7
!
!       ESTIMATE KNOT INTERVAL BY ASSUMING EQUALLY SPACED KNOTS.
   12 J = ABS(X-U(1))/(U(N)-U(1))*(N-1) + 1
!       ENSURE CASE X=U(N) GIVES J=N-1.
      J = MIN0(J,N-1)
!       INDICATE THAT KNOT INTERVAL INSIDE RANGE HAS BEEN USED.
      IFLG = 1
!       SEARCH FOR KNOT INTERVAL CONTAINING X.
      IF (X.GE.U(J)) GOTO 11
    2 J = J - 1
      IF (X.LT.U(J)) GOTO 2
!
!       CALCULATE SPLINE PARAMETERS FOR JTH INTERVAL.
    7 H = U(J+1) - U(J)
      Q1 = H*D(J)
      Q2 = H*D(J+1)
      SS = S(J+1) - S(J)
      B = 3.0*SS - 2.0*Q1 - Q2
      A = Q1 + Q2 - 2.0*SS
!
!       CALCULATE SPLINE VALUE.
    8 Z = (X-U(J))/H
      SPLINT = ((A*Z+B)*Z+Q1)*Z + S(J)
      GOTO 100
!
!       TEST IF X WITHIN ROUNDING ERROR OF U(1).
    3 IF (X.LE.U(1)-2.0**IEPS*AMAX1(ABS(U(1)),ABS(U(N)))) GOTO 101
      J = 1
      GOTO 7
!       TEST IF X WITHIN ROUNDING ERROR OF U(N).
    4 IF (X.GE.U(N)+2.0**IEPS*AMAX1(ABS(U(1)),ABS(U(N)))) GOTO 101
      J = N - 1
      GOTO 7
!
! POINTS OUTSIDE RANGE:
  101 IFLG = 0
      SPLINT = 0.0
  100 RETURN
      END FUNCTION SPLINT
!*==TB02A.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE TB02A(A,F,X,Y,N)
      SUBROUTINE TB02A(A,F,X,Y,N)
!
! *** TB02A updated by JCM from HARWELL ***
!
!X
!C 9C
!H Interpolates in non-equal interval table.
!A On entry real arrays A and F hold the arguments and function values
!A          X holds the required argument
!A          N is the dimension of A and F, and so is the number of items
!A On exit  Y holds the interpolated function value
!
      DIMENSION A(N), F(N)
      Y = 0.0
      DO L = 1, N
        PL = 1.0
        DO J = 1, N
          IF (L.EQ.J) GOTO 3
          PL = (X-A(J))*PL/(A(L)-A(J))
    3   ENDDO
        Y = Y + PL*F(L)
      ENDDO
      RETURN
      END SUBROUTINE TB02A
!*==TQLI.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 3      SUBROUTINE TQLI(D,E,N,NP,Z,BOTH)
      SUBROUTINE TQLI(D,E,N,NP,Z,BOTH)
!
! *** TQLI by JCM from "NUMERICAL RECIPES"  Dec 89 ***
!
!X
!C 9C
!H Performs the QL algorithm for eigenvalues and vectors of a real
!H symmetric matrix previously reduced to tridiagonal form.
!A On entry D is a real vector of length NP whose first N elements are the
!A            diagonal elements of the required tridiagonal matrix;
!A          E similarly holds the sub-diagonal matrix;  E(1) is arbitrary.
!A          N is the size of all required matrices and vectors;
!A          NP is their DIMENSION in the routine, and may be larger.
!A          Z is an NPxNP array holding an NxN matrix.  If the routine
!A            TRED2 has been used to reduce the matrix to tridiagonal, Z should
!A            on entry to TQLI contain the matrix on exit from TRED2.
!A            Otherwise it should contain a unit matrix.
!A          BOTH is a logical, set TRUE if both eigenvalues and eigenvectors
!A             are wanted.
!A On exit  D holds the eigenvalues, and the Kth column of Z holds the
!A            normalised eigenvector corresponding to D(K).
!
!N Adapted from Press, Flannery, Teukolsky & Vetterling, Numerical Recipes.
!
      LOGICAL BOTH
      DIMENSION D(NP), E(NP), Z(NP,NP)
!
!>> JCC Numerical trap
      INTEGER IBMBER
      COMMON /CCSLER/ IBMBER
!
      IF (N.LE.1) GOTO 100
      DO I = 2, N
        E(I-1) = E(I)
      ENDDO
!
      E(N) = 0.
      DO L = 1, N
        ITER = 0
    1   DO M = L, N - 1
          DD = ABS(D(M)) + ABS(D(M+1))
! @ JvdS Isn't the following test very odd? Isn't it effectively testing E(M) = 0.0 ?
          IF (ABS(E(M))+DD.EQ.DD) GOTO 2
        ENDDO
!
        M = N
    2   IF (M.NE.L) THEN
          IF (ITER.EQ.30) CALL ERRMES(1,0,'too many iterations IN TQLI')
          ITER = ITER + 1
          G = (D(L+1)-D(L))/(2.*E(L))
          R = SQRT(G**2+1.)
          G = D(M) - D(L) + E(L)/(G+SIGN(R,G))
          S = 1.
          C = 1.
          P = 0.
          DO I = M - 1, L, -1
            F = S*E(I)
            B = C*E(I)
!>> @ JCC It seems that the next line causes numerous crashes in the Pawley refinement.
!>> As such, Ive added in a check on the values of G and F to prevent it happening
!>> This ought to be tested - why is it causing the crash: the input data must be corrupt
!>> somehow.
!
            IF (ABS(F).GT.0.0 .OR. ABS(G).GT.0.0) THEN
              IF (ABS(F).GE.ABS(G)) THEN
                C = G/F
                R = SQRT(C**2+1.)
                E(I+1) = F*R
                S = 1./R
                C = C*S
              ELSE
                S = F/G
                R = SQRT(S**2+1.)
                E(I+1) = G*R
                C = 1./R
                S = S*C
              ENDIF
            ELSE
!>> Ok: An error has occurred in the passed data
              IBMBER = 1
              RETURN
            ENDIF
            G = D(I+1) - P
            R = (D(I)-G)*S + 2.*C*B
            P = S*R
            D(I+1) = G + P
            G = C*R - B
            IF (BOTH) THEN
              DO K = 1, N
                F = Z(K,I+1)
                Z(K,I+1) = S*Z(K,I) + C*F
                Z(K,I) = C*Z(K,I) - S*F
              ENDDO
            ENDIF
!
          ENDDO
          D(L) = D(L) - P
          E(L) = G
          E(M) = 0.
          GOTO 1
        ENDIF
      ENDDO
  100 RETURN
      END SUBROUTINE TQLI
!*==TRANSQ.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE TRANSQ(A,ND)
      SUBROUTINE TRANSQ(A,ND)
!
! *** TRANSQ by JCM 12 Jul 83 ***
!
!X
!C 12C
!H Replaces a real square matrix by its transpose.
!A On entry A is an NDxND real square matrix
!A On exit  A has been replaced by its transpose
!
      DIMENSION A(ND,ND)
!
      DO I = 1, ND - 1
        DO J = I + 1, ND
          B = A(I,J)
          A(I,J) = A(J,I)
          A(J,I) = B
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE TRANSQ
!*==TRED2.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE TRED2(A,N,NP,D,E,BOTH)
      SUBROUTINE TRED2(A,N,NP,D,E,BOTH)
!
! *** TRED2 by JCM from "NUMERICAL RECIPES" 4 Dec 89 ***
!
!X
!C 9C
!H Performs the Householder reduction of a real symmetric matrix to
!H tridiagonal form.
!A On entry A is a real, symmetric NxN matrix held in an NPxNP array
!A          BOTH is a LOGICAL which is TRUE if both eigenvalues and eigen-
!A          vectors will subsequently be wanted.
!A On exit  D is a vector of length NP whose first N elements hold the
!A            diagonal of the tridiagonal matrix,
!A          E similarly holds the sub-diagonal elements, and E(1)=0.
!A          If BOTH, then A is replaced by the orthogonal matrix Q which
!A          effects the transformation;  if .NOT. BOTH, A holds rubbish.
!
!N For eigenvalues and eigenvectors, should be followed by a call to TQLI.
!N Adapted from Press, Flannery, Teukolsky & Vetterling, Numerical Recipes.
!
      LOGICAL BOTH
      DIMENSION A(NP,NP), D(NP), E(NP)
!
      IF (N.LE.1) GOTO 1
      DO I = N, 2, -1
        L = I - 1
        H = 0.
        SCALE = 0.
        IF (L.GT.1) THEN
          DO K = 1, L
            SCALE = SCALE + ABS(A(I,K))
          ENDDO
          IF (SCALE.EQ.0.) THEN
            E(I) = A(I,L)
          ELSE
            DO K = 1, L
              A(I,K) = A(I,K)/SCALE
              H = H + A(I,K)**2
            ENDDO
            F = A(I,L)
            G = -SIGN(SQRT(H),F)
            E(I) = SCALE*G
            H = H - F*G
            A(I,L) = F - G
            F = 0.
            DO J = 1, L
              IF (BOTH) A(J,I) = A(I,J)/H
              G = 0.
              DO K = 1, J
                G = G + A(J,K)*A(I,K)
              ENDDO
!
              IF (L.GT.J) THEN
                DO K = J + 1, L
                  G = G + A(K,J)*A(I,K)
                ENDDO
              ENDIF
              E(J) = G/H
              F = F + E(J)*A(I,J)
            ENDDO
            HH = F/(H+H)
            DO J = 1, L
              F = A(I,J)
              G = E(J) - HH*F
              E(J) = G
              DO K = 1, J
                A(J,K) = A(J,K) - F*E(K) - G*A(I,K)
              ENDDO
            ENDDO
          ENDIF
        ELSE
          E(I) = A(I,L)
        ENDIF
        D(I) = H
      ENDDO
!
    1 IF (BOTH) D(1) = 0.
      E(1) = 0.
      DO I = 1, N
        IF (BOTH) THEN
          L = I - 1
          IF (D(I).NE.0.) THEN
            DO J = 1, L
              G = 0.
              DO K = 1, L
                G = G + A(I,K)*A(K,J)
              ENDDO
              DO K = 1, L
                A(K,J) = A(K,J) - G*A(K,I)
              ENDDO
            ENDDO
          ENDIF
        ENDIF
        D(I) = A(I,I)
!
        IF (BOTH) THEN
          A(I,I) = 1.
          IF (L.GE.1) THEN
            DO J = 1, L
              A(I,J) = 0.
              A(J,I) = 0.
            ENDDO
          ENDIF
        ENDIF
      ENDDO
      RETURN
      END SUBROUTINE TRED2
!*==TRIAN1.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE TRIAN1(A,B,C,D,I)
      SUBROUTINE TRIAN1(A,B,C,D,I)
!
! *** TRIAN1 by JCM 25 Jan 85 ***
!
!X
!C 10C
!H Applies the cosine formula for the solution of spherical triangles.
!A The entry and exit conditions vary according to the entry value of I:
!A     I=1: sets D=angle A given sides A,B,C
!A     I=2: sets A=side A given sides B,C and D=angle A.
!A     I=3: sets D=side A given angles A,B,C
!A     I=4: sets A=angle A given angles B,C and D=side A.
!D The angles are given and returned as their cosines.
!O Writes an error message is a cosine found >1
!
      COMMON /IOUNIT/ LPT, ITI, ITO, IPLO, LUNI, IOUT
!
      CS = B*C
      S = SQRT((1-B*B)*(1-C*C))
      GOTO (1,2,2,1), I
    1 CS = -CS
    2 GOTO (3,4,3,4), I
    3 IF (ABS(S).GT.1.E-3) GOTO 9
      IF (ABS(A+CS).GT.1.E-3) GOTO 10
      D = 0.
      GOTO 100
    9 X = (A+CS)/S
      GOTO 5
    4 X = D*S + CS
    5 IF (ABS(X)-1..LT.10.E-3) GOTO 8
   10 WRITE (LPT,3000) A, B, C, D, I, S, CS, X
      WRITE (ITO,3000) A, B, C, D, I, S, CS, X
!>> JCC      STOP
      CALL BMBOUT
      RETURN
!
    8 IF (ABS(X).GT.1.) X = SIGN(1.,X)
      GOTO (6,7,6,7), I
    6 D = X
      GOTO 100
    7 A = X
  100 RETURN
 3000 FORMAT (/' ERROR ** in spherical triangle - cosine greater',      &
     &        ' than 1.0'/' A,B,C,D=',4F10.4,'I=',I2,'S,CS,X=',3F10.4)
      END SUBROUTINE TRIAN1
!*==UNIVEC.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE UNIVEC(X,U)
      SUBROUTINE UNIVEC(X,U)
!
! *** UNIVEC 1 May 84 by JCM ***
!
!X
!C 12C
!H Replaces a vector by a parallel unit vector, and gives its length.
!A On entry X is a real 1x3 vector referred to the CCSL orthogonal axes
!A On exit  X is replaced by a parallel unit vector
!A          U is set to the original length of X
!D If the length would be very small, X is not changed.
!N Works in double precision
!
      DOUBLE PRECISION DU
      DIMENSION X(3)
!
      U = 0.
      DU = 0.D0
      DO I = 1, 3
        DU = DU + X(I)*X(I)
      ENDDO
      IF (DU.LT.10.D-8) GOTO 100
      U = SNGL(DSQRT(DU))
      DO I = 1, 3
        X(I) = X(I)/U
      ENDDO
  100 RETURN
      END SUBROUTINE UNIVEC
!*==VECPRD.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE VECPRD(VEC1,VEC2,PRD)
      SUBROUTINE VECPRD(VEC1,VEC2,PRD)
!
! *** VECPRD by JCM ***
!
!X
!C 12C
!H Calculates the vector product of two 1x3 vectors.
!A On entry VEC1 and VEC2 hold 1x3 real vectors
!A On exit PRD = the vector product of VEC1 cross VEC2
!
      DIMENSION VEC1(3), VEC2(3), PRD(3)
      J = 2
      K = 3
      DO I = 1, 3
        PRD(I) = VEC1(J)*VEC2(K) - VEC1(K)*VEC2(J)
        J = K
        K = I
      ENDDO
      RETURN
      END SUBROUTINE VECPRD
