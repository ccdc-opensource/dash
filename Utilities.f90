!
!*****************************************************************************
!
      SUBROUTINE SORT_REAL(VAL,IP,N)
!
!X
!C 16C
!H Sorts pointers to a real array using Heapsort.
!
!A On entry VAL is an array of N real numbers.
!A On exit IP is an array of N pointers to VAL in ascending order
!
!N Copyright John Matthewman 18 July 1983
!N  HEAPSORT
!N  (See Knuth 'Art of Computer Programming' Vol 3, Section 5.2.3)
!
      REAL,    INTENT (IN   ) :: VAL(N)
      INTEGER, INTENT (  OUT) :: IP(N)
      INTEGER, INTENT (IN   ) :: N

! EXTRA PART (WHICH MAY BE REMOVED AGAIN) - SET UP POINTERS:
      DO I = 1, N
        IP(I) = I
      ENDDO
      IF (N.LT.2) RETURN
!  INITIALISE
      L = N/2 + 1
      IR = N
    1 L = L - 1
      K = IP(L)
    3 J = L
      GOTO 4
!  SIFTING LOOP
    5 IF (VAL(IP(J)).LT.VAL(IP(J+1))) J = J + 1
    7 IP(I) = IP(J)
    4 I = J
      J = J + J
      IF (J-IR) 5, 7, 8
!  FLOYDS MODIFICATION
   10 IP(J) = IP(I)
    8 J = I
      I = I/2
      IF (I) 6, 6, 9
    9 IF (J.GT.L .AND. VAL(K).GT.VAL(IP(I))) GOTO 10
    6 IP(J) = K
!  END OF A SIFT
      IF (L.GT.1) GOTO 1
      K = IP(IR)
      IP(IR) = IP(1)
      IR = IR - 1
      IF (IR.GT.1) GOTO 3
      IP(1) = K

      END SUBROUTINE SORT_REAL
!
!*****************************************************************************
!
      SUBROUTINE InverseMatrix(A,B,N)
!
!X
!C 12C
!H Inverts matrix A into matrix B.
!A On entry A is a square NxN real matrix
!A On exit  B is its inverse
!
      DIMENSION II(100), IL(100), IG(100), A(N,N), B(N,N)

      DO J = 1, N
        DO I = 1, N
          B(I,J) = A(I,J)
        ENDDO
      ENDDO
      D = 1.0
      IS = N - 1
      DO K = 1, N
        IL(K) = 0
        IG(K) = K
      ENDDO
      DO K = 1, N
        R = 0.0
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
        D = D * P
        IF (D.EQ.0.) GOTO 999
        DO I = 1, N
          IF (I.EQ.KF) THEN
            B(I,K) = 1.0/P
          ELSE
            B(I,K) = -B(I,K)/P
          ENDIF
        ENDDO
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
      ENDDO
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
  999 RETURN
      END SUBROUTINE InverseMatrix
!
!*****************************************************************************
!
      SUBROUTINE INC(i)

      IMPLICIT NONE

      INTEGER, INTENT (INOUT) :: i

      i = i + 1

      END SUBROUTINE INC
!
!*****************************************************************************
!
