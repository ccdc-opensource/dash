!
!*****************************************************************************
!
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

      DO I = 2, N
        IF (A(I).GT.X) GOTO 2
      ENDDO
      I = N
    2 ALNINT = (B(I-1)*(A(I)-X)-B(I)*(A(I-1)-X))/(A(I)-A(I-1))

      END FUNCTION ALNINT
!
!*****************************************************************************
!
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

      END SUBROUTINE C1MSCA
!
!*****************************************************************************
!
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

      END SUBROUTINE CGMADD
!
!*****************************************************************************
!
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

      END SUBROUTINE CGMEQ
!
!*****************************************************************************
!
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

      END SUBROUTINE CGMSCA
!
!*****************************************************************************
!
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

      END SUBROUTINE CGMZER
!
!*****************************************************************************
!
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

      END SUBROUTINE CMCONJ
!
!*****************************************************************************
!
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

      END SUBROUTINE CMRSCA
!
!*****************************************************************************
!
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

      DEGREE = DEG*X

      END FUNCTION DEGREE
!
!*****************************************************************************
!
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

      D = 0.0
      J = 2
      K = 3
      DO I = 1, 3
        D = D + A(1,I)*(A(2,J)*A(3,K)-A(2,K)*A(3,J))
        J = K
        K = I
      ENDDO
      DETER3 = D

      END FUNCTION DETER3
!
!*****************************************************************************
!
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

      Z = ABS(X)
      ZZ = Z*Z
      ZZZ = Z*Z*Z
      E = 1.0 + A1*Z + A2*ZZ + A3*ZZZ + A4*ZZ*ZZ + A5*ZZ*ZZZ + A6*ZZZ*ZZZ
      IF (E.LT.10.0) GOTO 1
      E = 0.0
      GOTO 2
    1 E = 1.0/E**16
      IF (E.LE.0.0) E = 0.0
    2 IF (X.LT.0.0) E = 2.0 - E
      ERFNC = E

      END FUNCTION ERFNC
!
!*****************************************************************************
!
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

      DO I = 1, 3
        CALL FRACT(VEC(I),A,J)
      ENDDO

      END SUBROUTINE FRAC3
!
!*****************************************************************************
!
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
      IF (X.GE.0.) RETURN
! OUT IN USUAL CASE OF X ALREADY BEING +VE FRACTION
      N = -1
    1 Y = AINT(X)
      IF (N.EQ.-1) Y = Y - 1.0
      X = X - Y

      END SUBROUTINE FRACT
!
!*****************************************************************************
!
      SUBROUTINE FT01A(IT,INV,TR,TI)
!
! *** FT01A updated by JCM FROM HARWELL ROUTINE 9 Sep 91 ***
!
!X
!C 9C
!H Modification of Harwell Fast Fourier Transform.
!
      DIMENSION TR(1024), TI(1024)

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      REAL            UR,     UI
      COMMON /FFTDA / UR(15), UI(15)

    2 UM = 1.
      IF (INV.EQ.1) UM = -1.
      IO = 2
      DO I = 2, 16
        IO = IO + IO
        IF (IO-IT) 3, 4, 99
    3 ENDDO
! ERROR EXIT - IF NOT A POWER OF 2, OR TOO BIG:
   99 INV = -1
      CALL DebugErrorMessage('NOT A POWER OF 2, OR TOO BIG in FT01A')
      RETURN
    4 IO = I
      II = IO
      I1 = IT/2
      I3 = 1
   10 K = 0
      I2 = I1 + I1
   11 WR = 1.0
      WI = 0.0
      KK = K
      JO = IO
   12 IF (KK.EQ.0) GOTO 13
   14 JO = JO - 1
      KK1 = KK
      KK = KK/2
      IF (KK1.EQ.2*KK) GOTO 14
      WS = WR*UR(JO) - WI*UI(JO)
      WI = WR*UI(JO) + WI*UR(JO)
      WR = WS
      GOTO 12
   13 WI = WI*UM
      J = 0
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
    7 K = 0
      J1 = J
      DO I = 1, II
        J2 = J1/2
        K = 2*(K-J2) + J1
        J1 = J2
      ENDDO
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
!
!*****************************************************************************
!
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

      END SUBROUTINE GMADD
!
!*****************************************************************************
!
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

      END SUBROUTINE GMEQ
!
!*****************************************************************************
!
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
          C(IK) = 0.0
          DO J = 1, NJ
            C(IK) = C(IK) + A(IJ)*B(JK)
            IJ = IJ + NI
            JK = JK + 1
          ENDDO
          IK = IK + NI
        ENDDO
      ENDDO

      END SUBROUTINE GMPRD
!
!*****************************************************************************
!
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

      END SUBROUTINE GMREV
!
!*****************************************************************************
!
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

      GMSAME = .FALSE.
      DO I = 1, N
        IF (ABS(A(I)-B(I)).GT.TOLER) RETURN
      ENDDO
      GMSAME = .TRUE.

      END FUNCTION GMSAME
!
!*****************************************************************************
!
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

      END SUBROUTINE GMSCA
!
!*****************************************************************************
!
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

      END SUBROUTINE GMSUB
!
!*****************************************************************************
!
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

      DO J = 1, JJ
        DO I = 1, II
          B(I,J) = A(J,I)
        ENDDO
      ENDDO

      END SUBROUTINE GMTRAN
!
!*****************************************************************************
!
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
        IF (I1.GT.NI) RETURN
        DO J = I1, NI
          A(I,J) = 0.
          A(J,I) = 0.
        ENDDO
      ENDDO

      END SUBROUTINE GMUNI
!
!*****************************************************************************
!
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
        A(I) = 0.0
      ENDDO

      END SUBROUTINE GMZER
!
!*****************************************************************************
!
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

      END SUBROUTINE JGMEQ
!
!*****************************************************************************
!
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
      INTEGER, INTENT (  OUT) :: JA(*)
      INTEGER, INTENT (IN   ) :: NI, NJ

      INTEGER I, NIJ

      NIJ = NI*NJ
      DO I = 1, NIJ
        JA(I) = 0
      ENDDO

      END SUBROUTINE JGMZER
!
!*****************************************************************************
!
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
!
!*****************************************************************************
!
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

      END SUBROUTINE RCMPRD
!
!*****************************************************************************
!
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

      END FUNCTION RSCALP
!
!*****************************************************************************
!
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

      END FUNCTION SCALPR
!
!*****************************************************************************
!
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
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      IF (ABS(SN)-10.E-5.GT.1.) THEN
        WRITE (LPT,3000) SN, L1
 3000   FORMAT (/' Sin or Cos value',E12.4,' greater than unity',' - called from ',A6)
        CALL BMBOUT
        CALL DebugErrorMessage('Error in SINCOS() in Mvec')
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
! USUAL CASE:
        CS = SQRT(1.-SN*SN)
      ENDIF

      END SUBROUTINE SINCOS
!
!*****************************************************************************
!
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
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
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
    5 ENDDO
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
   10   H1 = 1./(X(J)-X(J-1))
        H2 = 1./(X(J+1)-X(J))
        A(3*I-2) = H1*H1
        A(3*I-1) = H1*H1 - H2*H2
        A(3*I) = -H2*H2
        D(I) = 2.*(F(J)*(H2*H2*H2+H1*H1*H1)-F(J+1)*H2*H2*H2-F(J-1)*H1*H1*H1)
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
      D(N) = D(N)/A(3*N-1)
      DO I = 3, N
        J = N + 2 - I
        D(J) = (D(J)-A(3*J)*D(J+1))/A(3*J-1)
      ENDDO
      D(1) = (D(1)-D(2)*A(2)-D(3)*A(3))/A(1)
      A(1) = 0.
  100 RETURN
      END SUBROUTINE SPLINE
!
!*****************************************************************************
!
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
!       TEST WETHER POINT IN RANGE.
      IF (X.LT.U(1)) GOTO 3
      IF (X.GT.U(N)) GOTO 4
!       JUMP IF KNOT INTERVAL REQUIRES RANDOM SEARCH.
      IF (IX.LT.0 .OR. IFLG.EQ.0) GOTO 12
!       JUMP IF KNOT INTERVAL SAME AS LAST TIME.
      IF (X.LE.U(J+1)) GOTO 8
!       LOOP TILL INTERVAL FOUND.
    1 J = J + 1
   11 IF (X.GT.U(J+1)) GOTO 1
      GOTO 7
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
!       CALCULATE SPLINE PARAMETERS FOR JTH INTERVAL.
    7 H = U(J+1) - U(J)
      Q1 = H*D(J)
      Q2 = H*D(J+1)
      SS = S(J+1) - S(J)
      B = 3.0*SS - 2.0*Q1 - Q2
      A = Q1 + Q2 - 2.0*SS
!       CALCULATE SPLINE VALUE.
    8 Z = (X-U(J))/H
      SPLINT = ((A*Z+B)*Z+Q1)*Z + S(J)
      GOTO 100
!       TEST IF X WITHIN ROUNDING ERROR OF U(1).
    3 IF (X.LE.U(1)-2.0**IEPS*AMAX1(ABS(U(1)),ABS(U(N)))) GOTO 101
      J = 1
      GOTO 7
!       TEST IF X WITHIN ROUNDING ERROR OF U(N).
    4 IF (X.GE.U(N)+2.0**IEPS*AMAX1(ABS(U(1)),ABS(U(N)))) GOTO 101
      J = N - 1
      GOTO 7
! POINTS OUTSIDE RANGE:
  101 IFLG = 0
      SPLINT = 0.0
  100 RETURN
      END FUNCTION SPLINT
!
!*****************************************************************************
!
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

      END SUBROUTINE TB02A
!
!*****************************************************************************
!
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

      INTEGER         IBMBER
      COMMON /CCSLER/ IBMBER

      IF (N.LE.1) RETURN
      DO I = 2, N
        E(I-1) = E(I)
      ENDDO
      E(N) = 0.
      DO L = 1, N
        ITER = 0
    1   DO M = L, N - 1
          DD = ABS(D(M)) + ABS(D(M+1))
! @@ JvdS Isn't the following test very odd? Isn't it effectively testing E(M) = 0.0 ?
          IF (ABS(E(M))+DD.EQ.DD) GOTO 2
        ENDDO
        M = N
    2   IF (M.NE.L) THEN
          IF (ITER.EQ.30) THEN
            CALL ERRMES(1,0,'too many iterations IN TQLI')
            IF (IBMBER .NE. 0) RETURN
          ENDIF
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
! @@ JCC It seems that the next line causes numerous crashes in the Pawley refinement.
! As such, I've added in a check on the values of G and F to prevent it happening
! This ought to be tested - why is it causing the crash: the input data must be corrupt
! somehow.
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
              CALL BMBOUT
              CALL DebugErrorMessage('F or G equal 0 in TQLI() in Mvec.')
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
          ENDDO
          D(L) = D(L) - P
          E(L) = G
          E(M) = 0.
          GOTO 1
        ENDIF
      ENDDO

      END SUBROUTINE TQLI
!
!*****************************************************************************
!
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

      DO I = 1, ND - 1
        DO J = I + 1, ND
          B = A(I,J)
          A(I,J) = A(J,I)
          A(J,I) = B
        ENDDO
      ENDDO

      END SUBROUTINE TRANSQ
!
!*****************************************************************************
!
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

      END SUBROUTINE TRED2
!
!*****************************************************************************
!
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
      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI
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
!
!*****************************************************************************
!
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
!
!*****************************************************************************
!
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

      END SUBROUTINE VECPRD
!
!*****************************************************************************
!
