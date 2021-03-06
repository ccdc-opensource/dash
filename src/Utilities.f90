! This file is part of DASH.
! SPDX-Identifier: MIT
!
! Copyright 2001 Science and Technology Facilities Council
! Copyright 2001 Cambridge Crystallographic Data Centre
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
!*****************************************************************************
!
      LOGICAL FUNCTION NearlyEqual(Value1, Value2)
!
! This function compares two REALs and determines if they are effectively equal
!
! INPUT   : Value1 and Value2 = the values to be compared
!
! RETURNS : .TRUE.  if Value1 and Value2 differ by less than 0.000001
!           .FALSE. otherwise
!
      IMPLICIT NONE

      REAL, INTENT (IN   ) :: Value1, Value2

      NearlyEqual = (ABS(Value1 - Value2) .LT. 0.000001)

      END FUNCTION NearlyEqual
!
!*****************************************************************************
!
      LOGICAL FUNCTION nearly_equal(value1, value2, tolerance)
!
! This function compares two REALs and determines if they are equal within tolerance
!
! INPUT   : value1 and value2 = the values to be compared
!
! RETURNS : .TRUE.  if value1 and value2 differ by less than tolerance
!           .FALSE. otherwise
!
      IMPLICIT NONE

      REAL, INTENT (IN   ) :: value1, value2, tolerance

      nearly_equal = (ABS(value1 - value2) .LT. tolerance)

      END FUNCTION nearly_equal
!
!*****************************************************************************
!
      REAL FUNCTION Radians2Degrees(TheAngle)     

      IMPLICIT NONE

      REAL, INTENT (IN   ) :: TheAngle

      Radians2Degrees = TheAngle * (30.0 / ASIN(0.5))

      END FUNCTION Radians2Degrees
!
!*****************************************************************************
!
      REAL FUNCTION Degrees2Radians(TheAngle)     

      IMPLICIT NONE

      REAL, INTENT (IN   ) :: TheAngle

      Degrees2Radians = TheAngle * (ASIN(0.5) / 30.0)

      END FUNCTION Degrees2Radians
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
      SUBROUTINE InverseMatrix(A, B, N)
!
! Inverts matrix A into matrix B.
! On entry A is a square NxN real matrix
! On exit  B is its inverse
!
      IMPLICIT NONE
      
      INTEGER, INTENT (IN   ) :: N
      REAL,    INTENT (IN   ) :: A(N,N)
      REAL,    INTENT (  OUT) :: B(N,N)

      INTEGER II(N), IL(N), IG(N)
      INTEGER I, J, IS, K, KF, KG, KL
      REAL    D, R, W, X, P

! Initialise b with values from a
      B = A
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
        IF (D .EQ. 0.0) THEN
          CALL DebugErrorMessage('D .EQ. 0.0 in InverseMatrix()')
          RETURN
        ENDIF
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
      SUBROUTINE GenerateAtomLabel(TheElement, TheNumber, TheLabelStr)

! Converts a CCDC element number + serial number to an atom label string, e.g.:
! "1" + "12" gives "C12"

      USE ATMVAR

      IMPLICIT NONE

      INTEGER,       INTENT (IN   ) :: TheElement
      INTEGER,       INTENT (IN   ) :: TheNumber
      CHARACTER*(*), INTENT (  OUT) :: TheLabelStr

      CHARACTER*20, EXTERNAL :: Integer2String
      CHARACTER*3 tElementStr
      CHARACTER*20 tStr
      INTEGER iLen1, iLen2

      tStr = Integer2String(TheNumber)
      iLen1 = LEN_TRIM(tStr)
      tElementStr = ElementStr(TheElement)
      iLen2 = LEN_TRIM(tElementStr)
      TheLabelStr = tElementStr(1:iLen2)//tStr(1:iLen1)

      END SUBROUTINE GenerateAtomLabel
!
!*****************************************************************************
!
      LOGICAL FUNCTION assembly_contains(the_assembly, the_value)

      USE ATMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: the_assembly(1:MaxAtm_3)
      INTEGER, INTENT (IN   ) :: the_value

      INTEGER I

      DO I = 1, MaxAtm_3
        IF ( the_assembly(I) .EQ. 0 ) THEN
          assembly_contains = .FALSE.
          RETURN
        ENDIF
        IF ( the_assembly(I) .EQ. the_value ) THEN
          assembly_contains = .TRUE.
          RETURN
        ENDIF
      ENDDO
      assembly_contains = .FALSE.

      END FUNCTION assembly_contains
!
!*****************************************************************************
!
      SUBROUTINE assembly_add(the_assembly, the_value)

      USE ATMVAR

      IMPLICIT NONE

      INTEGER, INTENT (INOUT) :: the_assembly(1:MaxAtm_3)
      INTEGER, INTENT (IN   ) :: the_value

      INTEGER I

      DO I = 1, MaxAtm_3
        IF ( the_assembly(I) .EQ. 0 ) THEN
          the_assembly(I) = the_value
          RETURN
        ENDIF
      ENDDO
      CALL DebugErrorMessage('assembly_add() overflow.')

      END SUBROUTINE assembly_add
!
!*****************************************************************************
!
      INTEGER FUNCTION assembly_size(the_assembly)

      USE ATMVAR

      IMPLICIT NONE

      INTEGER, INTENT (INOUT) :: the_assembly(1:MaxAtm_3)

      INTEGER I

      DO I = 1, MaxAtm_3
        IF ( the_assembly(I) .EQ. 0 ) THEN
          assembly_size = I-1
          RETURN
        ENDIF
      ENDDO
      assembly_size = MaxAtm_3

      END FUNCTION assembly_size
!
!*****************************************************************************
!
