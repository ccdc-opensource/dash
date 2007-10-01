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
!*==BJ.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 3      FUNCTION BJ(N,X)
      FUNCTION BJ(N,X)
!
! *** BJ by JCM 17 Jan 85 ***
!
!X
!C 9C
!H Calculates Bessel functions of order 1 or 2.
!A On entry N is the order required
!A          X is the argument
!A On exit BJ holds the required Bessel function
!D Uses Harwell Library FF01A or FF02A, which have been included as
!D part of the CCSL.
!
!
      IF (N-1) 1, 2, 99
!
! ORDER 1:
    1 CALL FF01A(BJ,Y,X,0)
      GOTO 100
!
! ORDER 2:
    2 CALL FF02A(BJ,Y,X,0)
      GOTO 100
!
! ANY OTHER ORDER:
   99 CALL ERRIN2(N,0,'Bessel function order',' not calculated by BJ')
  100 RETURN
      END FUNCTION BJ
!*==BRILL.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 2      FUNCTION BRILL(T,TN,S)
      FUNCTION BRILL(T,TN,S)
!
! *** BRILL from PJB 14 Jun 88 ***
!
!X
!C 9C
!H Returns the value of the Brillouin function.
!A On entry T is the absolute temperature at which the function is required,
!A          TN the transition temperature,
!A          S is the spin.
!N  Uses the subprogram NB01A from the Harwell library.
!
      COMMON /IOUNIT/ LPT, ITI, ITO, IPLO, LUNI, IOUT
!
      IF (T.LE.0) THEN
        BRILL = 1.
        GOTO 100
      ELSEIF (T.GE.TN) THEN
        BRILL = 0.
        GOTO 100
      ENDIF
      K = 0
      NUM = 0
      B = 1.
      ERR = .0001
      MAXIT = 100
      X = 0.002
      GOTO 1
    5 CALL NB01A(K,A,B,ERR,X,Y,MAXIT)
      GOTO (1,2,3,4), K
!
    1 S2 = (2*S+1)/(2*S)
      Z = 3*X*S*TN/((S+1)*T)
      Y = X - (S2/TANH(S2*Z)) + (1./(TANH(Z/(2*S))*2*S))
      IF (K.EQ.1) GOTO 5
      IF (Y.LE.ERR) THEN
        X = 2*X
        GOTO 1
      ENDIF
      A = X
      GOTO 5
!
    2 BRILL = X
      GOTO 100
!
    3 K = 0
      B = A
      A = A/2.
      NUM = NUM + 1
      IF (NUM.LE.10) GOTO 5
      I = 0
    6 WRITE (ITO,3000) I, T, TN, S
      WRITE (LPT,3000) I, T, TN, S
      BRILL = -1
      GOTO 100
!
    4 I = 1
      GOTO 6
!
  100 RETURN
 3000 FORMAT (' ERROR **',I2,' calculating Brillouin function: ',3F8.2)
      END FUNCTION BRILL
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
!*==CGMPRD.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE CGMPRD(A,B,C,I,J,K)
      SUBROUTINE CGMPRD(A,B,C,I,J,K)
!
! *** CGMPRD by JCM ***
!
!X
!C 12C
!H Multiplies together two COMPLEX matrices.
!A On entry A (of dimensions IxJ) and B (of dimensions JxK) are COMPLEX matrices
!A On exit  C (of dimensions IxK) is the product of A and B
!N C is unchanged if any of I,J or K is zero.
!
      COMPLEX A(1), B(1), C(1)
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
      END SUBROUTINE CGMPRD
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
!*==CGMSUB.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE CGMSUB(A,B,C,NI,NJ)
      SUBROUTINE CGMSUB(A,B,C,NI,NJ)
!
! *** CGMSUB by PJB 20 Feb 91 ***
!
!X
!C 12C
!H Sets COMPLEX matrix C = COMPLEX matrix A - COMPLEX matrix B.
!A On entry A nnd B are NI by NJ COMPLEX matrices
!A On exit C is set to the difference of A and B
!
      COMPLEX A(NI,NJ), B(NI,NJ), C(NI,NJ)
      DO I = 1, NI
        DO J = 1, NJ
          C(I,J) = A(I,J) - B(I,J)
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE CGMSUB
!*==CGMUNI.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE CGMUNI(A,NI)
      SUBROUTINE CGMUNI(A,NI)
!
! *** CGMUNI modified by PJB c17 17 sept 93 ***
!
!X
!C 12C
!H Clears a COMPLEX square matrix to contain the unit matrix.
!A On exit A is an NIxNI unit matrix
!N NI must be at least 1
!
      COMPLEX A(NI,NI)
      DATA C0, C1/0., 1./
!
      DO I = 1, NI
        A(I,I) = CMPLX(C1,C0)
        I1 = I + 1
        IF (I1.GT.NI) GOTO 100
        DO J = I1, NI
          A(I,J) = CMPLX(C0,C0)
          A(J,I) = A(J,I)
        ENDDO
      ENDDO
  100 RETURN
      END SUBROUTINE CGMUNI
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
!*==CMIMAG.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE CMIMAG(A,B,I,J)
      SUBROUTINE CMIMAG(A,B,I,J)
!
! *** CMIMAG by PJB Nov 89 ***
!
!X
!C 12C
!H Gives the imaginary parts of a COMPLEX matrix.
!A On entry A is a complex I by J matrix
!A On exit the I by J real matrix B contains the imaginary parts of A
!
      COMPLEX A(I,J)
      DIMENSION B(I,J)
      DO II = 1, I
        DO JJ = 1, J
          B(II,JJ) = AIMAG(A(II,JJ))
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE CMIMAG
!*==CMREAL.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE CMREAL(A,B,I,J)
      SUBROUTINE CMREAL(A,B,I,J)
!
! *** CMREAL by PJB Nov 89 ***
!
!X
!C 12C
!H Gives the real parts of a COMPLEX matrix.
!A On entry A is a complex I by J matrix
!A On exit the I by J real matrix B contains the real parts of A
!
      COMPLEX A(I,J)
      DIMENSION B(I,J)
      DO II = 1, I
        DO JJ = 1, J
          B(II,JJ) = REAL(A(II,JJ))
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE CMREAL
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
!*==CRMPRD.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE CRMPRD(A,B,C,I,J,K)
      SUBROUTINE CRMPRD(A,B,C,I,J,K)
!
! *** CRMPRD by PJB Nov 89 ***
!
!X
!C 12C
!H Performs COMPLEX by REAL matrix multiplication.
!A On entry A (of dimensions IxJ) is a COMPLEX matrix
!A          B (of dimensions JxK) is a REAL matrix
!A On exit  C (of dimensions IxK and COMPLEX) is the product of A and B.
!N C is unchanged if any of I,J or K is zero.
!N Note also the existence of SUBROUTINE RCMPRD which multiplies them
!N the other way round.
!
      DIMENSION B(1)
      COMPLEX A(1), C(1)
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
      END SUBROUTINE CRMPRD
!*==CRSCLP.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      COMPLEX FUNCTION CRSCLP(A,B)
      COMPLEX FUNCTION CRSCLP(A,B)
!
! *** CRSCLP by PJB Nov 89 ***
!
!X
!C 12C
!H Finds the COMPLEX scalar product of a COMPLEX with a REAL vector.
!A On entry A is a complex vector of dimension 3
!A          B is a real vector of dimension 3 in a space orthogonal to
!A            that of A
!D On exit the function value is the scalar product of A and B
!
      COMPLEX A(3), C
      DIMENSION B(3)
      C = CMPLX(0.,0.)
      DO I = 1, 3
        C = C + A(I)*B(I)
      ENDDO
      CRSCLP = C
      RETURN
      END FUNCTION CRSCLP
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
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8,&
     &                VALMUB
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
!*==EXPINT.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 4      FUNCTION EXPINT(A,P,N,L)
      FUNCTION EXPINT(A,P,N,L)
!
! *** EXPINT by PJB Dec 84 ***
!
!X
!C 9C
!H Calculates an exponential radial integral.
!A On entry A, P N and L are set up for the routine to calculate the integral
!A between 0 and infinity of the Lth order spherical Bessel function of
!A A*X times X**N*exp(-P*X)
!
!D Uses hypergeometric series.
!N Used to calculate form-factors from Slater type wave-functions
      DIMENSION B(4)
      B(1) = FLOAT(N+L+3)
      B(2) = 0.5*FLOAT(L-N-1)
      B(3) = FLOAT(L) + 1.5
      EX = 2.0
      GAM = B(3) - 1.0
      L1 = L + 1
      DO I = 1, L1
        EX = EX*GAM
        GAM = GAM - 1.0
      ENDDO
!
      EX = FACT(N+L+2)/EX
      IF (L.NE.0) THEN
        X = (0.5*A)**L
        EX = EX*X
      ENDIF
      X = A*A + P*P
      B(1) = 0.5*B(1)
      EX = EX/(X**B(1))
      X = A*A/X
!
!     HYPERGEOMETRIC SERIES
      SUM = 1.
      TERM = 1.
      B(4) = 1.
    3 T = TERM
      TERM = TERM*B(1)*B(2)*X/(B(3)*B(4))
      SUM = SUM + TERM
      E = ABS(TERM/SUM)
      IF (E.GT..001) THEN
        DO I = 1, 4
          B(I) = B(I) + 1.0
        ENDDO
        GOTO 3
      ENDIF
!
      EXPINT = EX*SUM
      RETURN
      END FUNCTION EXPINT
!*==FACT.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 3      FUNCTION FACT(K)
      FUNCTION FACT(K)
!
! *** FACT by PJB 19 Jan 85 ***
!
!X
!C 9C
!H Calculates factorial K.
!A On entry K holds an integer which is positive or zero.
!A On exit FACT holds factorial K
!
!
      FAC = 1.
      IF (K) 1, 101, 2
    1 CALL ERRIN2(K,1,'negative argument for factorial',' ')
!
    2 DO I = 1, K
        FAC = FAC*FLOAT(I)
      ENDDO
  101 FACT = FAC
      RETURN
      END FUNCTION FACT
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
!%
!      DIMENSION TR(%FFT2%),TI(%FFT2%)
      DIMENSION TR(1024), TI(1024)
      EXTERNAL FFTADD
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8,&
     &                VALMUB
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
!*==GMNORM.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
!
! LEVEL 2       SUBROUTINE GMNORM(A,B,II,JJ)
      SUBROUTINE GMNORM(A,B,II,JJ)
!
! *** GMNORM by PJB ***
!
!X
!C 12C
!H Normalises the rows of a matrix.
!A On entry A is an IIxJJ real matrix
!A On exit A has been normalised using B, A(I,J) out = A(I,J) in / B(J)
!A         B holds the normalising coefficients
!
      DIMENSION A(II,JJ), B(JJ)
!
      DO J = 1, JJ
        CALL GMPRD(A(1,J),A(1,J),B(J),1,II,1)
        IF (B(J).GT..0001) THEN
          B(J) = SQRT(B(J))
          ANORM = 1./B(J)
          CALL GMSCA(A(1,J),A(1,J),ANORM,II,1)
        ENDIF
      ENDDO
      RETURN
      END SUBROUTINE GMNORM
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
!*==JGMADD.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE JGMADD(JA,JB,JC,NI,NJ)
      SUBROUTINE JGMADD(JA,JB,JC,NI,NJ)
!
! *** JGMADD by JCM Jun 88 ***
!
!X
!C 12C
!H In integers, sets matrix C = matrix A plus matrix B.
!A On entry JA and JB are integer matrices both of dimension NIxNJ
!A On exit  JC=JA+JB also NIxNJ
!
      DIMENSION JA(NI,NJ), JB(NI,NJ), JC(NI,NJ)
      DO I = 1, NI
        DO J = 1, NJ
          JC(I,J) = JA(I,J) + JB(I,J)
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE JGMADD
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
!*==JGMPRD.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE JGMPRD(JA,JB,JC,NI,NJ,NK)
      SUBROUTINE JGMPRD(JA,JB,JC,NI,NJ,NK)
!
! *** JGMPRD by JCM Jun 88 ***
!
!X
!C 12C
!H In integers, sets matrix C = matrix A times matrix B.
!A On entry JA is an integer matrix of dimension NIxNJ
!A          JB is an integer matrix of dimension NJxNK
!A On exit  JC is an integer matrix of dimension NIxNK, being JA times JB
!
      DIMENSION JA(1), JB(1), JC(1)
      DO I = 1, NI
        IK = I
        JK = 1
        DO K = 1, NK
          IJ = I
          JC(IK) = 0.
          DO J = 1, NJ
            JC(IK) = JC(IK) + JA(IJ)*JB(JK)
            IJ = IJ + NI
            JK = JK + 1
          ENDDO
          IK = IK + NI
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE JGMPRD
!*==JGMREV.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE JGMREV(JA,JB,NI,NJ)
      SUBROUTINE JGMREV(JA,JB,NI,NJ)
!
! *** JGMREV by JCM Jun 88 ***
!
!X
!C 12C
!H Reverses the signs of the elements of an integer matrix.
!A On entry JA is an integer matrix of dimension NIxNJ
!A On exit  JB is an integer matrix, being a copy of JA with reversed signs.
!N JB may be the same as JA.
!
      DIMENSION JA(NI,NJ), JB(NI,NJ)
      DO I = 1, NI
        DO J = 1, NJ
          JB(I,J) = -JA(I,J)
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE JGMREV
!*==JGMSUB.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE JGMSUB(JA,JB,JC,NI,NJ)
      SUBROUTINE JGMSUB(JA,JB,JC,NI,NJ)
!
! *** JGMSUB by JCM Jun 88 ***
!
!X
!C 12C
!H In integers, sets matrix C = matrix A minus matrix B.
!A On entry JA and JB are integer matrices of dimension NIxNJ
!A On exit  JC is an integer matrix = JA - JB
!
      DIMENSION JA(1), JB(1), JC(1)
      NIJ = NI*NJ
      DO I = 1, NIJ
        JC(I) = JA(I) - JB(I)
      ENDDO
      RETURN
      END SUBROUTINE JGMSUB
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
!*==JTERMS.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 4      SUBROUTINE JTERMS(G,R,H,S)
      SUBROUTINE JTERMS(G,R,H,S)
!
! *** JTERMS by PJB 17 Jan 85 ***
!
!X
!C 9C
!H Calculates the terms in the 2D averaged spherically symmetric form factor
!H summation which depend on S,K, and R only.
!
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8,&
     &                VALMUB
!
!     TEST FOR SPECIAL CASES:
      IF (H.LT.001) GOTO 1
      IF (S.LT..001) GOTO 2
      IF (ABS(H-S).LT..001) GOTO 3
      G = H*BJ(1,H*R)*BJ(0,S*R) - S*BJ(1,S*R)*BJ(0,H*R)
      G = TWOPI*G*R/(H*H-S*S)
      GOTO 100
    1 IF (S.LT..001) GOTO 4
      G = TWOPI*R*BJ(1,S*R)/S
      GOTO 100
    2 G = TWOPI*R*BJ(1,H*R)/H
      GOTO 100
    3 G = PI*R*R*(BJ(1,H*R)*BJ(1,S*R)+BJ(0,H*R)*BJ(0,S*R))
      GOTO 100
    4 G = PI*R*R
  100 RETURN
      END SUBROUTINE JTERMS
!*==MB11A.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 3      SUBROUTINE MB11A(M,N,A,IA,W)
      SUBROUTINE MB11A(M,N,A,IA,W)
!
! *** MB11A from HARWELL 25 May 79 ***
!
!C 12C
!H Inverts a rectangular matrix whose order is the smaller dimension;
!H used by the Harwell refinement routine VA05A.
!
      DIMENSION A(IA,1), W(1)
!     PARTITION THE WORKING SPACE ARRAY W
!     THE FIRST PARTITION HOLDS THE FIRST COMPONENTS OF THE VECTORS OF
!     THE ELEMENTARY TRANSFORMATIONS
      NRW = M
!     THE SECOND PARTITION RECORDS ROW INTERCHANGES
      NCW = M + M
!     THE THIRD PARTITION RECORDS COLUMN INTERCHANGES
!     SET THE INITIAL RECORDS OF ROW AND COLUMN INTERCHANGES
      DO I = 1, M
        N1 = NRW + I
        W(N1) = 0.5 + FLOAT(I)
      ENDDO
      DO I = 1, N
        N1 = NCW + I
        W(N1) = 0.5 + FLOAT(I)
      ENDDO
!     'KK' COUNTS THE SEPARATE ELEMENTARY TRANSFORMATIONS
      KK = 1
!     FIND LARGEST ROW AND MAKE ROW INTERCHANGES
    3 RMAX = 0.0
      DO I = KK, M
        SUM = 0.
        DO J = KK, N
          SUM = SUM + A(I,J)**2
        ENDDO
        IF (RMAX.LT.SUM) THEN
          RMAX = SUM
          IR = I
        ENDIF
      ENDDO
      IF (RMAX.EQ.0.0) CALL ERRIN2(M-KK,0,'in MB11A:',                  &
     &                             'reduced rows found to be zero')
      IF (IR.LE.KK) GOTO 7
      N3 = NRW + KK
      SUM = W(N3)
      N4 = NRW + IR
      W(N3) = W(N4)
      W(N4) = SUM
      DO J = 1, N
        SUM = A(KK,J)
        A(KK,J) = A(IR,J)
        A(IR,J) = SUM
      ENDDO
!     FIND LARGEST ELEMENT OF PIVOTAL ROW, AND MAKE COLUMN INTERCHANGES
    7 RMAX = 0.
      SUM = 0.
      DO J = KK, N
        SUM = SUM + A(KK,J)**2
        IF (RMAX.GE.ABS(A(KK,J))) GOTO 10
        RMAX = ABS(A(KK,J))
        IR = J
   10 ENDDO
      IF (IR.LE.KK) GOTO 12
      N5 = NCW + KK
      RMAX = W(N5)
      N6 = NCW + IR
      W(N5) = W(N6)
      W(N6) = RMAX
      DO I = 1, M
        RMAX = A(I,KK)
        A(I,KK) = A(I,IR)
        A(I,IR) = RMAX
      ENDDO
!     REPLACE THE PIVOTAL ROW BY THE VECTOR OF THE TRANSFORMATION
   12 SIGMA = SQRT(SUM)
      BSQ = SQRT(SUM+SIGMA*ABS(A(KK,KK)))
      W(KK) = SIGN(SIGMA+ABS(A(KK,KK)),A(KK,KK))/BSQ
      A(KK,KK) = -SIGN(SIGMA,A(KK,KK))
      KP = KK + 1
      IF (KP.GT.N) GOTO 16
      DO J = KP, N
        A(KK,J) = A(KK,J)/BSQ
      ENDDO
!     APPLY THE TRANSFORMATION TO THE REMAINING ROWS OF A
      IF (KP.GT.M) GOTO 16
      DO I = KP, M
        SUM = W(KK)*A(I,KK)
        DO J = KP, N
          SUM = SUM + A(KK,J)*A(I,J)
        ENDDO
        A(I,KK) = A(I,KK) - SUM*W(KK)
        DO J = KP, N
          A(I,J) = A(I,J) - SUM*A(KK,J)
        ENDDO
      ENDDO
      KK = KP
      GOTO 3
!     AT THIS STAGE THE REDUCTION OF A IS COMPLETE
!     NOW WE BUILD UP THE GENERALIZED INVERSE
!     APPLY THE FIRST ELEMENTARY TRANSFORMATION
   16 KK = M
      KP = M + 1
      SUM = W(M)/A(M,M)
      IF (N.LE.M) GOTO 33
      DO J = KP, N
        A(M,J) = -SUM*A(M,J)
      ENDDO
   33 A(M,M) = 1./A(M,M) - SUM*W(M)
!     NOW APPLY THE OTHER (M-1) TRANSFORMATIONS
   36 KP = KK
      KK = KP - 1
      IF (KK.LE.0) GOTO 37
!     FIRST TRANSFORM THE LAST (M-KK) ROWS
      DO I = KP, M
        SUM = 0.
        DO J = KP, N
          SUM = SUM + A(KK,J)*A(I,J)
        ENDDO
        DO J = KP, N
          A(I,J) = A(I,J) - SUM*A(KK,J)
        ENDDO
        W(I) = -SUM*W(KK)
      ENDDO
!     THEN CALCULATE THE NEW ROW IN POSITION KK
      DO J = KP, N
        SUM = -W(KK)*A(KK,J)
        DO I = KP, M
          SUM = SUM - A(I,KK)*A(I,J)
        ENDDO
        A(KK,J) = SUM/A(KK,KK)
      ENDDO
!     AND REVISE THE COLUMN IN POSITION KK
      SUM = 1. - W(KK)**2
      DO I = KP, M
        SUM = SUM - A(I,KK)*W(I)
        A(I,KK) = W(I)
      ENDDO
      A(KK,KK) = SUM/A(KK,KK)
      GOTO 36
!     RESTORE THE ROW INTERCHANGES
   37 DO I = 1, M
   46   N1 = NRW + I
        IR = IFIX(W(N1))
        IF (I.GE.IR) GOTO 45
        SUM = W(N1)
        N2 = NRW + IR
        W(N1) = W(N2)
        W(N2) = SUM
        DO J = 1, N
          SUM = A(I,J)
          A(I,J) = A(IR,J)
          A(IR,J) = SUM
        ENDDO
        GOTO 46
   45 ENDDO
!     RESTORE THE COLUMN INTERCHANGES
      DO J = 1, N
   50   N1 = NCW + J
        IR = IFIX(W(N1))
        IF (J.GE.IR) GOTO 49
        SUM = W(N1)
        N2 = NCW + IR
        W(N1) = W(N2)
        W(N2) = SUM
        DO I = 1, M
          SUM = A(I,J)
          A(I,J) = A(I,IR)
          A(I,IR) = SUM
        ENDDO
        GOTO 50
   49 ENDDO
      RETURN
      END SUBROUTINE MB11A
!*==NB01A.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE NB01A(K,AZ,BZ,E2,X,Y,MAXIT)
      SUBROUTINE NB01A(K,AZ,BZ,E2,X,Y,MAXIT)
!
! *** NB01A updated by PJB 15-Nov-1994  ***
!
!X
!C 9C
!H Harwell routine NB01A to find the zero of a function.
!D Finds the value of X in the range AZ <= X <= BZ for which Y(X)=0
!A Called initially with K=0, AZ and BZ set to the minimum and maximum values
!A of X to be searched, E2 to the precision in X required and ABS(MAXIT) to
!A the maximum number of iterations allowed.
!A Subsequent calls depend on the value of K returned
!A     K=0 Initial call as above
!A     K=1 calculate Y for the value of X returned and call again
!A     K=2 The value of X retuned is the required solution
!A     K=3 No zero value of Y was found X=AZ and BZ
!A     K=4 MAXIT calls have been made without finding the zero value within the
!A         required precision
!N the private common /NB01AP has been included (C43) to preserve the values
!N of A,B,IT AND THE Js between calls
      COMMON /NB01AP/ A, B, IT, J1, J2, J3
!
      IF (K.GT.0) GOTO 30
!
!     CALCULATE Y(X) AT X=AZ.
      A = AZ
      B = BZ
      X = A
      J1 = 1
      IT = 1
      M = IABS(MAXIT)
   10 K = J1
      GOTO 100
!
!     PRINT X AND Y(X) WHEN REQUESTED.
   30 IF (MAXIT.LE.0) WRITE (6,2000) X, Y
 2000 FORMAT (2X,'NB01A   X=',E16.7,'  Y(X)=',E16.7)
!
!     TEST WHETHER Y(X) IS SUFFICIENTLY SMALL.
      IF (ABS(Y).GT.E2) GOTO 50
   45 K = 2
      GOTO 100
!
!     BRANCH DEPENDING ON THE VALUE OF J1.
   50 GOTO (60,70,88,170), J1
!
!     CALCULATE Y(X) AT X=BZ.
   60 YA = Y
      X = B
      J1 = 2
      GOTO 100
!
!     TEST WHETHER THE SIGNS OF Y(AZ) AND Y(BZ) ARE DIFFERENT.
   70 IF (YA*Y.LT.0.) GOTO 120
!
!     BEGIN THE BINARY SUBDIVISION TO SEARCH FOR A BRACKET.
      X1 = A
      Y1 = YA
      J1 = 3
      H = B - A
      J2 = 1
   80 X2 = A + 0.5*H
      J3 = 1
!
!     CHECK WHETHER MAXIT FUNCTION VALUES HAVE BEEN CALCULATED.
   90 IT = IT + 1
      IF (IT.GE.M) GOTO 10
      X = X2
      GOTO 100
!
!     TEST WHETHER A BRACKET HAS BEEN FOUND.
   88 IF (YA*Y.LT.0.) GOTO 120
!
!     CONTINUE THE SEARCH FOR A BRACKET.
      IF (J3.GE.J2) GOTO 110
      A = X
      YA = Y
      X2 = X + H
      J3 = J3 + 1
      GOTO 90
  110 A = X1
      YA = Y1
      H = 0.5*H
      J2 = J2 + J2
      GOTO 80
!
!     AT THIS POINT THE FIRST BRACKET HAS BEEN FOUND.
  120 B = X
      YB = Y
      J1 = 4
!
!     CALCULATE THE NEXT X BY THE SECANT METHOD BASED ON THE BRACKET.
  130 IF (ABS(YA).LE.ABS(YB)) GOTO 140
      X1 = A
      Y1 = YA
      X = B
      Y = YB
      GOTO 150
  140 X1 = B
      Y1 = YB
      X = A
      Y = YA
!
!     USE THE SECANT METHOD BASED ON THE FUNCTION VALUES Y1 AND Y.
  150 U = Y*(X-X1)/(Y-Y1)
  155 X2 = X - U
      IF (X2.EQ.X) GOTO 195
      X1 = X
      Y1 = Y
      YTEST = 0.5*AMIN1(ABS(YA),ABS(YB))
!
!     CHECK THAT X2 IS INSIDE THE INTERVAL (A,B).
      IF ((X2-A)*(X2-B).LT.0.) GOTO 90
!
!     CALCULATE THE NEXT VALUE OF X BY BISECTION.
  160 X2 = 0.5*(A+B)
      YTEST = 0.
!
!     CHECK WHETHER THE MAXIMUM ACCURACY HAS BEEN ACHIEVED.
      IF ((X2-A)*(X2-B)) 90, 45, 45
!
!     REVISE THE BRACKET (A,B).
  170 IF (YA*Y.GE.0.) GOTO 180
      B = X
      YB = Y
      GOTO 190
  180 A = X
      YA = Y
!
!     USE YTEST TO DECIDE THE METHOD FOR THE NEXT VALUE OF X.
  190 IF (YTEST.LE.0.) GOTO 130
      IF (ABS(Y)-YTEST) 150, 150, 160
  195 IF (U.EQ.0.) GOTO 45
      U = U + U
      GOTO 155
!
  100 RETURN
      END SUBROUTINE NB01A
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
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8,&
     &                VALMUB
!
      RADIAN = RAD*X
      RETURN
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
!*==SID.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE SID(A,N,ND1,ND2,D)
      SUBROUTINE SID(A,N,ND1,ND2,D)
!
! *** SID from PJB by A. Bartelemy ***
!
!X
!C 12C
!H Solves a set of simultaneous linear equations.
!A  A   is an N by N+1 sized matrix written in a table of dimension ND1
!A      by ND2
!A  On entry A(1:N,1:N) contains the matrix of coefficients
!A           A(1:N,N+1) contains the column of right-hand sides
!A  On exit  D is the value of the determinant
!A           A(1:N,N+1) contains the solution if D is not zero
!
!  SUBROUTINE *** SID ***         A BARTHELEMY  19/2/75        *
!  INVERSION DE MATRICE AVEC CALCUL DE DETERMINANT             *
!  MATRICE A(N,N+1)    DANS TABLEAU (ND1,ND2)                  *
!   EN ENTREE POUR APPEL PAR RAFIN MAIN :                      *
!        A(I=1 A N,J=1 A N)=AMAT                               *
!        A(I=1 A N,J=N+1  )=VVEC                               *
!  N=NOMBRE DE COLONNES DE A                                   *
!
      DIMENSION II(16), IL(16), IG(16), A(ND1,ND2)
!
      M = N + 1
      D = 1.
      IS = N - 1
      DO K = 1, N
        IL(K) = 0
        IG(K) = K
      ENDDO
!
!.....
      DO K = 1, N
        R = 0.
        DO I = 1, N
          IF (IL(I).NE.0) GOTO 40
          W = A(I,K)
          X = ABS(W)
          IF (X.LT.R) GOTO 40
          R = X
          P = W
          KF = I
   40   ENDDO
!
        II(K) = KF
        IL(KF) = KF
        D = D*P
!.....ERROR NUL DETERMINANT
        IF (D.EQ.0.) GOTO 100
!
        DO I = 1, N
          IF (I.EQ.KF) THEN
            A(I,K) = 1./P
          ELSE
            A(I,K) = -A(I,K)/P
          ENDIF
        ENDDO
!
        DO J = 1, M
          IF (J.EQ.K) GOTO 140
          W = A(KF,J)
          IF (W.EQ.0.) GOTO 140
          DO I = 1, N
            IF (I.EQ.KF) THEN
              A(I,J) = W/P
            ELSE
              A(I,J) = A(I,J) + W*A(I,K)
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
!
        DO I = 1, N
          R = A(I,KF)
          A(I,KF) = A(I,KG)
          A(I,KG) = R
        ENDDO
!
        DO J = 1, M
          R = A(K,J)
          A(K,J) = A(KL,J)
          A(KL,J) = R
        ENDDO
!
        IL(KF) = K
        IL(KG) = KL
        IG(KL) = IG(K)
        IG(K) = KF
        D = -D
  190 ENDDO
  100 RETURN
      END SUBROUTINE SID
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
!A             calling routine if the abssolute value of SN is greater
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
!*==SPHARM.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 2      SUBROUTINE SPHARM(Y,T,P,LMAX,NUM)
      SUBROUTINE SPHARM(Y,T,P,LMAX,NUM)
!
! *** SPHARM by PJB Apr 84 ***
!
!X
!C 9C
!H Calculates spherical harmonics.
!A  On entry T and P are the spherical polar angles theta and phi
!A             defining the orientation of the spherical harmonic function
!A             (in radians).
!A           LMAX is the number of different l values to calculate
!A             (LMAX=maximum l +1)
!A           NUM is the total number of spherical harmonic functions
!A              which will be calculated (NUM=LMAX(LMAX+1)/2)
!A On exit  Y(N) is complex and is set to the value of the Nth
!A              spherical harmonic, these being in sets of constant l
!A              arranged in order of increasing l with m running from
!A              0 to l within each l value.
!N Only values for positive m are stored
!
      COMPLEX Y(NUM), EXPHI(20)
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8,&
     &                VALMUB
!
      D = 1./SQRT(TWOPI)
      C = COS(T)
      S = SIN(T)
      Y(1) = CMPLX(SQRT(0.5),0.)
      IF (LMAX.EQ.1) GOTO 6
      Y(2) = CMPLX(SQRT(1.5),0.)*C
      Y(3) = CMPLX(-SQRT(.75),0.)*S
      IF (LMAX.LT.3) GOTO 6
      LL = LMAX - 2
      N = 2
      DO L = 1, LL
        A = SQRT(FLOAT((2*L+1)*(2*L+3)))
        B = SQRT(FLOAT((2*L+1)*(2*L-1)))
        MAX = L + 1
        DO MM = 1, MAX
          M = MM - 1
          Y(N+L+1) = C*Y(N)
          IF (L.NE.M) Y(N+L+1) = Y(N+L+1) - SQRT(FLOAT((L-M)*(L+M)))    &
     &                           *Y(N-L)/B
          Y(N+L+1) = Y(N+L+1)*A/SQRT(FLOAT((L+1-M)*(L+1+M)))
          N = N + 1
        ENDDO
!
        Y(N+L+1) = CMPLX(0.,0.)
        IF (S.LT..0001) GOTO 1
        Y(N+L+1) = -FLOAT(2*L)*C*Y(N+L)/S - SQRT(FLOAT(4*L+2))*Y(N+L-1)
        Y(N+L+1) = Y(N+L+1)/SQRT(FLOAT(2*L+2))
    1 ENDDO
!
    6 CALL TRIG(EXPHI,P,LMAX)
      N = 1
!
      DO L = 1, LMAX
        DO M = 1, L
          Y(N) = Y(N)*EXPHI(M)*D
          N = N + 1
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE SPHARM
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
!*==STERMS.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE STERMS(G,R,H,S)
      SUBROUTINE STERMS(G,R,H,S)
!
! *** STERMS by PJB 17 Jan 85 ***
!
!X
!C 9C
!H Calculates a term in the spherically symmetric form factor summation,
!H for a 3D averaged form-factor involving Sk and r only.
!
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8,&
     &                VALMUB
!
      FOURPI = 4.*PI
!  TEST FOR SPECIAL CASES:
      IF (H.LT.001) GOTO 1
      IF (S.LT..001) GOTO 2
      IF (ABS(H-S).LT..001) GOTO 3
      G = H*COS(H*R)*SIN(S*R) - S*COS(S*R)*SIN(H*R)
      G = FOURPI*G/(H*S*(S*S-H*H))
      GOTO 100
    1 IF (S.LT..001) GOTO 4
      G = SIN(S*R) - S*R*COS(S*R)
      G = FOURPI*G/S**3
      GOTO 100
    2 G = SIN(H*R) - H*R*COS(H*R)
      G = FOURPI*G/H**3
      GOTO 100
    3 G = -SIN((H+S)*R) + (H+S)*R
      G = TWOPI*G/(H+S)*H*S
      GOTO 100
    4 G = (FOURPI*R**3)/3.
  100 RETURN
      END SUBROUTINE STERMS
!*==SUMVEC.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE SUMVEC(A,I,J,K,SUM)
      SUBROUTINE SUMVEC(A,I,J,K,SUM)
!
! *** SUMVEC by JCM 26 Sep 85 ***
!
!X
!C 12C
!H Calculates the sum of elements of a real array
!A On entry A is a real vector of length at least J
!A          I is the position in the array of the first element in the sum
!A          J is the position beyond which elements are not included
!A          K is the "step" between included elements
!A On exit  SUM is the sum of the chosen elements
!
!D Sets SUM=the sum of elements of array A by a DO LOOP 'I,J,K', dealing
!D correctly with J < I, and therefore usable in both FORTRAN 66 & 77.
!
      DIMENSION A(1)
!
      SUM = 0.
      IF (J.LT.I) GOTO 100
!
      DO N = I, J, K
        SUM = SUM + A(N)
      ENDDO
  100 RETURN
      END SUBROUTINE SUMVEC
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
!*==TRANSC.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE TRANSC(A,ND)
      SUBROUTINE TRANSC(A,ND)
!
! *** TRANSC by JCM 12 Jul 83 ***
!
!X
!C 12C
!H Replaces a COMPLEX square matrix by its transposed conjugate.
!A On entry A is a COMPLEX NDxND matrix
!A On exit  the elements of A have been replaced by their conjugates, and A
!A          has been transposed.
!
      COMPLEX A(ND,ND), B
!
      DO I = 1, ND
        A(I,I) = CONJG(A(I,I))
      ENDDO
!
      DO I = 1, ND - 1
        DO J = I + 1, ND
          B = CONJG(A(I,J))
          A(I,J) = CONJG(A(J,I))
          A(J,I) = B
        ENDDO
      ENDDO
      RETURN
      END SUBROUTINE TRANSC
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
!*==TRIG.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE TRIG(A,B,K)
      SUBROUTINE TRIG(A,B,K)
!
! *** TRIG by JCM 18 Apr 84 ***
!
!X
!C 10C
!H Sets up cos(nx) and sin(nx) for a range of n by recursion.
!A On entry B is the reqired argument x
!A          K is the required number of terms
!A On exit  A is the COMPLEX array holding the answers
!D Calculates the COMPLEX trigonometric functions A(i) = CEXP(0,m*B)
!D for i = m+1, m = 0,K-1.
!
      COMPLEX A(K), MULT
      A(1) = CMPLX(1.,0.)
      IF (K.LT.2) GOTO 100
      MULT = CEXP(CMPLX(0.,B))
      DO I = 2, K
        A(I) = A(I-1)*MULT
      ENDDO
  100 RETURN
      END SUBROUTINE TRIG
!*==TRINV3.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 1      SUBROUTINE TRINV3(A,D)
      SUBROUTINE TRINV3(A,D)
!
! *** TRINV3 by JCM ***
!
!X
!C 12C
!H Replaces a 3x3 matrix by the transpose of its inverse.
!A On entry A holds a 3x3 real matrix
!A On exit  D is the value of the determinant of the matrix.
!A          A has been replaced by its inverse
!D If D is less than 10E-5 no replacement takes place.
!
      DIMENSION A(9), P(9)
      D = 0.
      II = 0
      IO = 0
      JO = 3
      KO = 6
      DO IC = 1, 3
        J = 2
        K = 3
        DO I = 1, 3
          II = II + 1
          P(II) = A(JO+J)*A(KO+K) - A(JO+K)*A(KO+J)
          J = K
          K = I
        ENDDO
        D = D + P(II)*A(II)
        JO = KO
        KO = IO
        IO = IO + 3
      ENDDO
      IF (ABS(D).LT.10.E-5) GOTO 100
      DO I = 1, 9
        A(I) = P(I)/D
      ENDDO
  100 RETURN
      END SUBROUTINE TRINV3
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
!*==VA05A.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 4      SUBROUTINE VA05A(M,N,F,X,DSTEP,DMAX,ACC,MAXFUN,IPRINT,W)
      SUBROUTINE VA05A(M,N,F,X,DSTEP,DMAX,ACC,MAXFUN,IPRINT,W)
!
! *** VA05A updated by JCM from HARWELL ***
!
!X
!C 6C
!H Minimises the sum of squares of given functions without requiring
!H the explicit calculation of derivatives.
!A On entry M,N,X,DSTEP,DMAX,ACC,MAXFUN,IPRINT,W should be set according to the
!A          specification of VA05A
!A On exit  X is set to contain the refined values of the parameters
!A          F is set to the corresponding function values
!P A routine called CALFUN(M,N,F,X) must be provided to set the array F
!
!N IPRINT must be the name of an integer, NOT an explicit integer
!
      DIMENSION F(1), X(1), W(1)
      COMMON /IOUNIT/ LPT, ITI, ITO, IPLO, LUNI, IOUT
!
! MAXC COUNTS CALLS OF CALFUN:
      MAXC = 0
      MPN = M + N
!     'NT' AND 'NTEST' CAUSE AN ERROR RETURN IF F(X) DOES NOT DECREASE
      NT = N + 2
      NTEST = 0
!     'DTEST' IS USED IN A TEST TO MAINTAIN LINEAR INDEPENDENCE
      DTEST = FLOAT(N+N) - 0.5
!     PARTITION THE WORKING SPACE ARRAY W
!     THE FIRST PARTITION HOLDS THE JACOBIAN APPROXIMATION
      NWI = M*N
!     THE NEXT PARTITION HOLDS THE GENERALIZED INVERSE
      NWX = NWI + MPN*N
!     THE NEXT PARTITION HOLDS THE BEST VECTOR X
      NWF = NWX + N
!     THE NEXT PARTITION HOLDS THE BEST VECTOR F
      NWC = NWF + M
!     THE NEXT PARTITION HOLDS THE COUNTS OF THE INDEPENDENT DIRECTIONS
      NWD = NWC + N
!     THE NEXT PARTITION HOLDS THE INDEPENDENT DIRECTIONS
      NWW = NWD + N*N
!     THE REMAINDER OF W IS USED FOR SCRATCH VECTORS
      NWV = NWW + N
      NWT = NWV + M
      NWU = NWT + N
!     USUALLY 'FMIN' IS THE LEAST CALCULATED VALUE OF F(X)
      FMIN = -1.
!     USUALLY 'DD' IS THE SQUARE OF THE CURRENT STEP LENGTH
      DD = 0.
      DSS = DSTEP*DSTEP
      DM = DMAX*DMAX
!     'PARM' IS THE LEAST VALUE OF THE MARQUARDT PARAMETER
      PARM = SQRT(ACC)/DMAX
!     'DPAR' AND 'NTPAR' ARE USED TO REGULATE THE MARQUARDT PARAMETER
      DPAR = 10.*DM
!     'IS' CONTROLS A GO TO STATEMENT FOLLOWING A CALL OF CALFUN
      IS = 4
      IC = 0
!     'TINC' IS USED IN THE CRITERION TO INCREASE THE STEP LENGTH
      TINC = 1.
!
!     START A NEW PAGE FOR PRINTING
      IF (IPRINT.NE.0) THEN
        CALL NEWPAG(LPT)
        CALL MESS(LPT,0,'   THE FOLLOWING OUTPUT IS PROVIDED BY '//     &
     &            'SUBROUTINE VA05A')
        CALL NEWLIN(LPT)
        CALL NEWLIN(LPT)
        IPC = 0
      ENDIF
      GOTO 3
!
!     TEST WHETHER THERE HAVE BEEN MAXFUN CALLS OF CALFUN
    4 IF (MAXFUN.GT.MAXC) GOTO 3
      IF (IPRINT.EQ.0) THEN
        IPRINT = 2
        GOTO 19
      ELSE
        WRITE (LPT,3000) MAXC
        WRITE (ITO,3000) MAXC
        GOTO 7
      ENDIF
!
!     CALL THE SUBROUTINE CALFUN
    3 MAXC = MAXC + 1
      CALL CALFUN(M,N,F,X)
!     CALCULATE THE SUM OF SQUARES
      FSQ = 0.
      DO I = 1, M
        FSQ = FSQ + F(I)*F(I)
      ENDDO
!     TEST FOR ERROR RETURN BECAUSE F(X) DOES NOT DECREASE
      GOTO (9,10,9,10), IS
    9 IF (FSQ.LT.FMIN) GOTO 11
      IF (DD.GT.DSS) GOTO 10
      NTEST = NTEST - 1
      IF (NTEST.GT.0) GOTO 10
      IF (IPRINT.NE.0) GOTO 15
      IPRINT = 1
      GOTO 19
   15 WRITE (LPT,3001)
      WRITE (ITO,3001)
!     PROVIDE PRINTING OF FINAL SOLUTION IF REQUESTED
    7 IF (IPRINT.EQ.0) GOTO 19
      WRITE (LPT,2001) MAXC
 2001 FORMAT (///5X,'THE FINAL SOLUTION CALCULATED BY VA05A REQUIRED',  &
     &        I5,' CALLS OF CALFUN, AND IS')
      WRITE (LPT,2002) (I,W(NWX+I),I=1,N)
      WRITE (LPT,2003) (I,W(NWF+I),I=1,M)
      WRITE (LPT,2004) FMIN
!     RESTORE THE BEST VALUES OF X AND F
   19 DO I = 1, N
        X(I) = W(NWX+I)
      ENDDO
      DO I = 1, M
        F(I) = W(NWF+I)
      ENDDO
      GOTO 100
!
   11 NTEST = NT
!     PROVIDE ORDINARY PRINTING IF REQUESTED
   10 IF (IABS(IPRINT)-1) 39, 38, 40
   38 WRITE (LPT,2005) MAXC
 2005 FORMAT (///5X,'AT THE',I5,' TH CALL OF CALFUN WE HAVE')
   42 WRITE (LPT,2002) (I,X(I),I=1,N)
      WRITE (LPT,2004) FSQ
      IF (IPRINT.GT.0) WRITE (LPT,2003) (I,F(I),I=1,M)
      GOTO 39
!
   40 IPC = IPC - 1
      IF (IPC.GT.0) GOTO 39
      WRITE (LPT,2006) MAXC
 2006 FORMAT (///5X,'THE BEST ESTIMATE AFTER',I5,' CALLS OF CALFUN IS')
      IPC = IABS(IPRINT)
      IF (FSQ.LT.FMIN) GOTO 42
      IF (FMIN.LT.0) GOTO 42
      WRITE (LPT,2002) (I,W(NWX+I),I=1,N)
      WRITE (LPT,2004) FMIN
      IF (IPRINT.GT.0) WRITE (LPT,2003) (I,W(NWF+I),I=1,M)
!
   39 GOTO (49,47,47,48), IS
!     STORE THE INITIAL VECTORS X AND F
   48 IF (IC.GT.0) GOTO 51
      DO I = 1, N
        W(NWX+I) = X(I)
      ENDDO
      GOTO 54
!
!     CALCULATE THE INITIAL JACOBIAN APPROXIMATION
   51 K = IC
      DO I = 1, M
        W(K) = (F(I)-W(NWF+I))/DSTEP
        K = K + N
      ENDDO
!     TEST WHETHER THE MOST RECENT X IS BEST
      IF (FMIN.GT.FSQ) GOTO 57
      X(IC) = W(NWX+IC)
      GOTO 58
!
   57 W(NWX+IC) = X(IC)
   54 DO I = 1, M
        W(NWF+I) = F(I)
      ENDDO
      FMIN = FSQ
!
!     SET X FOR THE NEXT CALL OF CALFUN
   58 IC = IC + 1
      IF (IC.GT.N) GOTO 60
      X(IC) = W(NWX+IC) + DSTEP
      GOTO 3
!     SET THE DIRECTION MATRIX
   60 K = NWD
      DO I = 1, N
        DO J = 1, N
          K = K + 1
          W(K) = 0.
        ENDDO
        W(K+I-N) = 1.
        W(NWC+I) = 1. + FLOAT(N-I)
      ENDDO
!     SET THE MARQUARDT PARAMETER TO ITS LEAST VALUE
   24 PAR = PARM
!     COPY THE JACOBIAN AND APPEND THE MARQUARDT MATRIX
   25 PPAR = PAR*PAR
      NTPAR = 0
   63 KK = 0
      K = NWI + NWI
      DO I = 1, N
        DO J = 1, M
          KK = KK + 1
          W(KK+NWI) = W(KK)
        ENDDO
        DO J = 1, N
          K = K + 1
          W(K) = 0.
        ENDDO
        W(K+I-N) = PAR
      ENDDO
!     CALCULATE THE GENERALIZED INVERSE OF J
      CALL MB11A(N,MPN,W(NWI+1),N,W(NWW+1))
!     NOTE THAT THE THIRD AND FIFTH ENTRIES OF THIS ARGUMENT LIST
!     STAND FOR ONE-DIMENSIONAL ARRAYS.
!     START THE ITERATION BY TESTING FMIN
   64 IF (FMIN.LE.ACC) GOTO 7
!     NEXT PREDICT THE DESCENT AND MARQUARDT MINIMA
      DS = 0.
      DN = 0.
      SP = 0.
      DO I = 1, N
        X(I) = 0.
        F(I) = 0.
        K = I
        DO J = 1, M
          X(I) = X(I) - W(K)*W(NWF+J)
          F(I) = F(I) - W(NWI+K)*W(NWF+J)
          K = K + N
        ENDDO
        DS = DS + X(I)*X(I)
        DN = DN + F(I)*F(I)
        SP = SP + X(I)*F(I)
      ENDDO
!     PREDICT THE REDUCTION IN F(X) DUE TO THE MARQUARDT STEP
!     AND ALSO PREDICT THE LENGTH OF THE STEEPEST DESCENT STEP
      PRED = SP + SP
      DMULT = 0.
      K = 0
      DO I = 1, M
        AP = 0.
        AD = 0.
        DO J = 1, N
          K = K + 1
          AP = AP + W(K)*F(J)
          AD = AD + W(K)*X(J)
        ENDDO
        PRED = PRED - AP*AP
        DMULT = DMULT + AD*AD
      ENDDO
!     TEST FOR CONVERGENCE
      IF (DN.GT.DM) GOTO 29
      AP = SQRT(DN)
      IF (PRED+2.*PPAR*AP*(DMAX-AP)-ACC) 7, 7, 70
   29 IF (PRED+PPAR*(DM-DN)-ACC) 7, 7, 70
!     TEST WHETHER TO APPLY THE FULL MARQUARDT CORRECTION
   70 DMULT = DS/DMULT
      DS = DS*DMULT*DMULT
   71 IS = 2
      IF (DN.GT.DD) GOTO 73
!     TEST THAT THE MARQUARDT PARAMETER HAS ITS LEAST VALUE
      IF (PAR.GT.PARM) GOTO 24
      DD = AMAX1(DN,DSS)
      DS = 0.25*DN
      TINC = 1.
      IF (DN.GE.DSS) GOTO 132
      IS = 3
      GOTO 103
!
!     TEST WHETHER TO INCREASE THE MARQUARDT PARAMETER
   73 IF (DN.GT.DPAR) GOTO 32
      NTPAR = 0
      GOTO 33
   32 IF (NTPAR.GT.0) GOTO 35
      NTPAR = 1
      PTM = DN
      GOTO 33
   35 NTPAR = NTPAR + 1
      PTM = AMIN1(PTM,DN)
      IF (NTPAR.LT.NT) GOTO 33
!     SET THE LARGER VALUE OF THE MARQUARDT PARAMETER
      PAR = PAR*(PTM/DM)**0.25
      IF (6.*DD.GE.DM) GOTO 25
      AP = SQRT(PRED/DN)
      IF (AP.LE.PAR) GOTO 25
      PAR = AMIN1(AP,PAR*(DM/(6.*DD))**0.25)
      GOTO 25
!
!     TEST WHETHER TO USE THE STEEPEST DESCENT DIRECTION
   33 IF (DS.LT.DD) GOTO 75
!     TEST WHETHER THE INITIAL VALUE OF DD HAS BEEN SET
      IF (DD.GT.0.) GOTO 78
      DD = AMIN1(DM,DS)
      IF (DD.GE.DSS) GOTO 78
      DD = DSS
      GOTO 71
!
!     SET THE MULTIPLIER OF THE STEEPEST DESCENT DIRECTION
   78 ANMULT = 0.
      DMULT = DMULT*SQRT(DD/DS)
      GOTO 80
!     INTERPOLATE BETWEEN THE STEEPEST DESCENT AND MARQUARDT DIRECTIONS
   75 SP = SP*DMULT
      ANMULT = (DD-DS)/((SP-DS)+SQRT((SP-DD)**2+(DN-DD)*(DD-DS)))
      DMULT = DMULT*(1.-ANMULT)
!     CALCULATE THE CORRECTION TO X, AND ITS ANGLE WITH THE FIRST
!     DIRECTION
   80 DN = 0.
      SP = 0.
      DO I = 1, N
        F(I) = DMULT*X(I) + ANMULT*F(I)
        DN = DN + F(I)*F(I)
        SP = SP + F(I)*W(NWD+I)
      ENDDO
      DS = 0.25*DN
!     TEST WHETHER AN EXTRA STEP IS NEEDED FOR INDEPENDENCE
      IF (W(NWC+1).LE.DTEST) GOTO 132
      IF (SP*SP.GE.DS) GOTO 132
!
!     TAKE THE EXTRA STEP AND UPDATE THE DIRECTION MATRIX
   83 DO I = 1, N
        X(I) = W(NWX+I) + DSTEP*W(NWD+I)
        W(NWC+I) = W(NWC+I+1) + 1.
      ENDDO
      W(NWD) = 1.
      IF (N.LE.1) GOTO 4
      DO I = 1, N
        K = NWD + I
        SP = W(K)
        DO J = 2, N
          W(K) = W(K+N)
          K = K + N
        ENDDO
        W(K) = SP
      ENDDO
      GOTO 4
!     EXPRESS THE NEW DIRECTION IN TERMS OF THOSE OF THE DIRECTION
!     MATRIX, AND UPDATE THE COUNTS IN W(NWC+1) ETC.
  132 IF (N.GE.2) GOTO 153
      IS = 1
      GOTO 152
  153 SP = 0.
      K = NWD
      DO I = 1, N
        X(I) = DW
        DW = 0.
        DO J = 1, N
          K = K + 1
          DW = DW + F(J)*W(K)
        ENDDO
        GOTO (89,90), IS
   90   W(NWC+I) = W(NWC+I) + 1.
        SP = SP + DW*DW
        IF (SP.LE.DS) GOTO 87
        IS = 1
        KK = I
        X(1) = DW
        GOTO 92
   89   X(I) = DW
   92   W(NWC+I) = W(NWC+I+1) + 1.
   87 ENDDO
      W(NWD) = 1.
!     REORDER THE DIRECTIONS SO THAT KK IS FIRST
      IF (KK.LE.1) GOTO 93
      KS = NWC + KK*N
      DO I = 1, N
        K = KS + I
        SP = W(K)
        DO J = 2, KK
          W(K) = W(K-N)
          K = K - N
        ENDDO
        W(K) = SP
      ENDDO
!     GENERATE THE NEW ORTHOGONAL DIRECTION MATRIX
   93 DO I = 1, N
        W(NWW+I) = 0.
      ENDDO
      SP = X(1)*X(1)
      K = NWD
      DO I = 2, N
        DS = SQRT(SP*(SP+X(I)*X(I)))
        DW = SP/DS
        DS = X(I)/DS
        SP = SP + X(I)*X(I)
        DO J = 1, N
          K = K + 1
          W(NWW+J) = W(NWW+J) + X(I-1)*W(K)
          W(K) = DW*W(K+N) - DS*W(NWW+J)
        ENDDO
      ENDDO
      SP = 1./SQRT(DN)
      DO I = 1, N
        K = K + 1
        W(K) = SP*F(I)
      ENDDO
!     PREDICT THE NEW RIGHT HAND SIDES
  152 FNP = 0.
      K = 0
      DO I = 1, M
        W(NWW+I) = W(NWF+I)
        DO J = 1, N
          K = K + 1
          W(NWW+I) = W(NWW+I) + W(K)*F(J)
        ENDDO
        FNP = FNP + W(NWW+I)**2
      ENDDO
!     CALCULATE THE NEXT VECTOR X, AND THEN CALL CALFUN
  103 DO I = 1, N
        X(I) = W(NWX+I) + F(I)
      ENDDO
      GOTO 4
!     UPDATE THE STEP SIZE
   49 DMULT = 0.9*FMIN + 0.1*FNP - FSQ
      IF (DMULT.GE.0.) GOTO 108
      DD = AMAX1(DSS,0.25*DD)
      TINC = 1.
      IF (FSQ-FMIN) 106, 107, 107
!     TRY THE TEST TO DECIDE WHETHER TO INCREASE THE STEP LENGTH
  108 SP = 0.
      SS = 0.
      DO I = 1, M
        SP = SP + ABS(F(I)*(F(I)-W(NWW+I)))
        SS = SS + (F(I)-W(NWW+I))**2
      ENDDO
      PJ = 1. + DMULT/(SP+SQRT(SP*SP+DMULT*SS))
      SP = AMIN1(4.,TINC,PJ)
      TINC = PJ/SP
      DD = AMIN1(DM,SP*DD)
      GOTO 106
!     IF F(X) IMPROVES STORE THE NEW VALUE OF X
   47 IF (FSQ.GE.FMIN) GOTO 110
  106 FMIN = FSQ
      DO I = 1, N
        SP = X(I)
        X(I) = W(NWX+I)
        W(NWX+I) = SP
      ENDDO
      DO I = 1, M
        SP = F(I)
        F(I) = W(NWF+I)
        W(NWF+I) = SP
      ENDDO
  110 GOTO (107,107,113), IS
  113 IS = 2
      IF (FMIN-ACC) 7, 7, 83
!     CALCULATE THE CHANGES IN X AND IN F
  107 DS = 0.
      DO I = 1, N
        X(I) = X(I) - W(NWX+I)
        DS = DS + X(I)*X(I)
      ENDDO
      DO I = 1, M
        F(I) = F(I) - W(NWF+I)
      ENDDO
!     CALCULATE THE GENERALIZED INVERSE TIMES THE CHANGE IN X
      K = NWI
      SS = 0.
      DO I = 1, MPN
        SP = 0.
        DO J = 1, N
          K = K + 1
          SP = SP + W(K)*X(J)
        ENDDO
        W(NWV+I) = SP
        SS = SS + SP*SP
      ENDDO
!     CALCULATE J TIMES THE CHANGE IN F
!     ALSO APPLY PROJECTION TO THE GENERALIZED INVERSE
      DO I = 1, N
        ST = 0.
        K = NWI + I
        DO J = 1, MPN
          ST = ST + W(K)*W(J+NWV)
          K = K + N
        ENDDO
        ST = ST/SS
        K = NWI + I
        DO J = 1, MPN
          W(K) = W(K) - ST*W(J+NWV)
          K = K + N
        ENDDO
        ST = PPAR*X(I)
        K = I
        DO J = 1, M
          ST = ST + W(K)*F(J)
          K = K + N
        ENDDO
        W(NWW+I) = ST
      ENDDO
!     REVISE J AND CALCULATE ROW VECTOR FOR CORRECTION TO INVERSE
      IC = 0
      K = 0
      KK = NWI
      SP = 0.
      SPP = 0.
      DO I = 1, M
        SS = F(I)
        ST = F(I)
        DO J = 1, N
          IC = IC + 1
          KK = KK + 1
          SS = SS - W(IC)*X(J)
          ST = ST - W(KK)*W(NWW+J)
        ENDDO
        SS = SS/DS
        W(NWV+I) = ST
        SP = SP + F(I)*ST
        SPP = SPP + ST*ST
        DO J = 1, N
          K = K + 1
          W(K) = W(K) + SS*X(J)
        ENDDO
      ENDDO
      DO I = 1, N
        ST = PAR*X(I)
        DO J = 1, N
          KK = KK + 1
          ST = ST - W(KK)*W(NWW+J)
        ENDDO
        W(NWT+I) = ST
        SP = SP + PAR*X(I)*ST
        SPP = SPP + ST*ST
      ENDDO
!     TEST THAT THE SCALAR PRODUCT IS SUFFICIENTLY ACCURATE
      IF (0.01*SPP-ABS(SP-SPP)) 63, 63, 127
!     CALCULATE THE NEW GENERALIZED INVERSE
  127 DO I = 1, N
        K = NWI + I
        ST = X(I)
        DO J = 1, M
          ST = ST - W(K)*F(J)
          K = K + N
        ENDDO
        SS = 0.
        DO J = 1, N
          SS = SS + W(K)*X(J)
          K = K + N
        ENDDO
        ST = (ST-PAR*SS)/SP
        K = NWI + I
        DO J = 1, MPN
          W(K) = W(K) + ST*W(NWV+J)
          K = K + N
        ENDDO
      ENDDO
      GOTO 64
!
  100 RETURN
 3000 FORMAT (///5X,'ERROR RETURN FROM VA05A BECAUSE THERE HAVE BEEN',  &
     &        I5,' CALLS OF CALFUN')
 3001 FORMAT (///5X,'ERROR RETURN FROM VA05A BECAUSE F(X) NO LONGER',   &
     &        ' DECREASES'//5X,'THIS MAY BE DUE TO THE VALUES OF DSTEP',&
     &        ' AND ACC, OR TO LOSS OF RANK IN THE JACOBIAN MATRIX')
 2002 FORMAT (//4X,'I',7X,'X(I)',10X,'I',7X,'X(I)',10X,'I',7X,'X(I)',   &
     &        10X,'I',7X,'X(I)',10X,'I',7X,'X(I)'//5(I5,E17.8))
 2003 FORMAT (//4X,'I',7X,'F(I)',10X,'I',7X,'F(I)',10X,'I',7X,'F(I)',   &
     &        10X,'I',7X,'F(I)',10X,'I',7X,'F(I)'//5(I5,E17.8))
 2004 FORMAT (/5X,'THE SUM OF SQUARES IS',E17.8)
      END SUBROUTINE VA05A
!*==VECOUP.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 4      FUNCTION VECOUP(J1,M1,J2,M2,J,M)
      FUNCTION VECOUP(J1,M1,J2,M2,J,M)
!
! *** VECOUP by PJB 17 Jan 85 ***
!
!X
!C 9C
!H Calculates Clebsch-Gordon vector coupling coefficients.
!
!A On exit VECOUP holds the answer
!
      DIMENSION IFAC(6)
!
      VECOUP = 0.
      IF (J.GT.J1+J2) GOTO 100
      IF (IABS(J1-J2).GT.J) GOTO 100
      IF (M.NE.M1+M2) GOTO 100
      ANUM = FACT(J1+J2-J)*FACT(J1-J2+J)*FACT(J2+J-J1)
      ANUM = ANUM*FACT(J1+M1)*FACT(J2+M2)*FACT(J+M)
      ANUM = ANUM*FACT(J1-M1)*FACT(J2-M2)*FACT(J-M)
      VECOUP = SQRT(FLOAT(2*J+1)*ANUM/FACT(J1+J2+J+1))
      VEC = VECOUP
      IFAC(1) = J1 + J2 - J
      IFAC(2) = J1 - M1
      IFAC(3) = J2 + M2
      IFAC(4) = J - J2 + M1
      IFAC(5) = J - J1 - M2
      IFAC(6) = 0
      N = 0
      DO I = 4, 6
        IF (IFAC(I).LT.N) N = IFAC(I)
      ENDDO
!
      K = 0
      DO I = 1, 3
        IFAC(I) = IFAC(I) + N
        IF (IFAC(I).EQ.0) K = 1
      ENDDO
!
      DO I = 4, 6
        IFAC(I) = IFAC(I) - N
      ENDDO
!
      SUM = 1
      DO I = 1, 6
        SUM = SUM/FACT(IFAC(I))
      ENDDO
!
      SUM = SUM*FLOAT(1-2*MOD(IABS(N),2))
      TERM = SUM
      IF (K.EQ.1) GOTO 101
    5 TERM = -TERM
!
      DO I = 4, 6
        IFAC(I) = IFAC(I) + 1
        TERM = TERM/FLOAT(IFAC(I))
      ENDDO
!
      DO I = 1, 3
        TERM = TERM*FLOAT(IFAC(I))
        IFAC(I) = IFAC(I) - 1
        IF (IFAC(I).EQ.0) K = 1
      ENDDO
      SUM = SUM + TERM
      IF (K.NE.1) GOTO 5
!
  101 VECOUP = VECOUP*SUM
  100 RETURN
      END FUNCTION VECOUP
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
!*==WTMEAN.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 2      SUBROUTINE WTMEAN(X,DX,IFUN,SUMS)
      SUBROUTINE WTMEAN(X,DX,IFUN,SUMS)
!
! *** WTMEAN by JCM 17 Jan 85 ***
!
!X
!C 9C
!H Multiple entry routine for the calculation of weighted averages.
!A On entry IFUN indicates which action is required:
!A          IFUN= 0 initialses an array SUMS for subsequent summing.
!A          IFUN= a positive integer adds an observation X with a  weight
!A                deduced from the value of IFUN:
!A                IFUN=1 gives unit weights
!A                IFUN=2 gives 1/DX squared
!A          IFUN= a negative integer returns the mean in X, the standard
!A                deviation in DX, and the number of observations in
!A                the 4th element of given array SUMS.
!A If an observation has zero DX, it is ignored.  -ve DX is treated as +ve.
!
!A SUMS is a 1x5 array holding values for an individual set of data, so that
!A      several sets may be collected simultaneously.
!A
!A SUMS HOLDS:
!A      Sum of weighted observations
!A      Sum of weighted squares of observations
!A      Sum of 1/DX squared
!A      Number of observations
!A      Denominator for mean
!A             If unit weights, this is the number of observations,
!A             If 1/DX sqrd weights this is the sum of the weights
!
!
      DIMENSION SUMS(5)
      IF (IFUN) 1, 2, 3
!
!  INITIALISE:
    2 CALL GMZER(SUMS,1,5)
      GOTO 100
!
!  ADD AN OBSERVATION:
    3 SUMS(4) = SUMS(4) + 1.
      W = 1.
      IF (DX.EQ.0.) GOTO 100
      D = 1./(DX)**2
      IF (IFUN.EQ.2) W = D
      SUMS(1) = SUMS(1) + X*W
      SUMS(2) = SUMS(2) + X*(X*W)
      SUMS(3) = SUMS(3) + D
      SUMS(5) = SUMS(5) + W
      GOTO 100
!
!  CALCULATE MEAN AND SDEV:
    1 X = 0.
      DX = 0.
      IF (SUMS(4)) 100, 100, 8
    8 X = SUMS(1)/SUMS(5)
      DEL = 1./SUMS(3)
      IF (SUMS(4).EQ.1.) GOTO 5
      DDX = SUMS(2) - SUMS(1)*X
      DX = DDX/(SUMS(5)*(SUMS(4)-1.))
    5 IF (DEL.GT.DX) DX = DEL
      DX = SQRT(DX)
  100 RETURN
      END SUBROUTINE WTMEAN
!*==ARCCOS.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
!
!
!
! LEVEL 2      FUNCTION ARCCOS(X)
      FUNCTION ARCCOS(X)
!
! *** ARCCOS FOR IBM by JCM 26 Sep 83 ***
!
!X
!C 10C
!H Calculates an arc cosine.
!A On entry X= a cosine
!A On exit ARCCOS is the arc cosine of X in radians in range 0 to Pi.
!N Written because IBM did not have it at the time.
!
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8,&
     &                VALMUB
!
      CALL SINCOS(X,Y,'ARCCOS')
      IF (ABS(X).LT.0.5) GOTO 1
      ARCCOS = ATAN(Y/X)
      GOTO 2
    1 ARCCOS = PIBY2 - ATAN(X/Y)
    2 IF (ARCCOS.LT.0.) ARCCOS = ARCCOS + PI
      RETURN
      END FUNCTION ARCCOS
!*==CALFUN.f90  processed by SPAG 6.11Dc at 13:14 on 17 Sep 2001
!
      SUBROUTINE CALFUN(M,N,F,X)
      RETURN
      END SUBROUTINE CALFUN
