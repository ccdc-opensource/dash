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
! This unit contains subroutines for the manipulation of vectors and lattices.
!
!*****************************************************************************
!
      SUBROUTINE LatticeCellParameters2Lattice(a, b, c, tAlpha, tBeta, tGamma, TheLattice)
!
! Calculates right-handed lattice from cell parameters, c along z, b in yz plane
!
! The left-most subscript must vary most rapidly
!
! Corresponding subroutine in CCSL: ORTHG()
! See also ORTHO()
      IMPLICIT NONE

      REAL, INTENT (IN   ) :: a, b, c, tAlpha, tBeta, tGamma
      REAL, INTENT (  OUT) :: TheLattice(1:3,1:3)

      REAL, EXTERNAL :: Degrees2Radians
      LOGICAL, EXTERNAL :: ValidCellAxisLength
      REAL    cSTAR(1:3)
      REAL    alpha, beta, gamma ! to resolve call by value / call by reference conflict

      IF (.NOT. ValidCellAxisLength(a)) THEN
        CALL ErrorMessage('Invalid length of a axis')
        RETURN
      ENDIF
      IF (.NOT. ValidCellAxisLength(b)) THEN
        CALL ErrorMessage('Invalid length of b axis')
        RETURN
      ENDIF
      IF (.NOT. ValidCellAxisLength(c)) THEN
        CALL ErrorMessage('Invalid length of c axis')
        RETURN
      ENDIF
      alpha = Degrees2Radians(tAlpha)
      beta  = Degrees2Radians(tBeta)
      gamma = Degrees2Radians(tGamma)
! c-axis
      TheLattice(1,3) = 0.0
      TheLattice(2,3) = 0.0
      TheLattice(3,3) = c
! b-axis
      TheLattice(1,2) = 0.0
      TheLattice(2,2) = b * SIN(alpha)
      TheLattice(3,2) = b * COS(alpha)
! a-axis
      TheLattice(3,1) = a * COS(beta)
! Maybe an odd way to do this, but it works:
! the projection of a on the "b*c*" plane equals the projection of a on the yz plane:
! (ba)b* + (ca)c* = (ay)y + (az)z
! Multiplying from the right by y* yields:
! (ba)(b*y) + (ca)(c*y) = ay
! Note that y = y* and z = z*
! Since b* = y/|b|, b*y = 1/|b|:
! a(cos(gamma)) + (ca)(c*y) = ay
! We know ca and y and can calculate c*, leaving the unknown ay
      cSTAR = TheLattice(:,3)
      CALL VectorOrthogonalise(cSTAR,TheLattice(:,2))
      CALL VectorSetLength(cSTAR,1.0/c)
      TheLattice(2,1) = (c*a*COS(beta)) * cSTAR(2)   +   a * COS(gamma)
      TheLattice(1,1) = SQRT(a**2-TheLattice(2,1)**2-TheLattice(3,1)**2)

      END SUBROUTINE LatticeCellParameters2Lattice
!
!*****************************************************************************
!
      SUBROUTINE LatticeCellParameters2Lattice_2(a, b, c, tAlpha, tBeta, tGamma, TheLattice)

! Same as FRAC2PDB()

!
! To be used when writing out mol2 files only
!
! mol2 files contain an unresolved ambiguity: atom co-ordinates are given wrt. the 
! the orthogonal axes, but the unit cell is given as a, b, c, alpha, beta, gamma.
! It is not specified how the unit cell is to be constructed from the unit cell parameters.
! Mercury turns out to choose: a along x. Everywhere else in DASH, we have used c along z
!
! Calculates right-handed lattice from cell parameters, a along x, b in xy plane
!
! The left-most subscript must vary most rapidly
!
      IMPLICIT NONE

      REAL, INTENT (IN   ) :: a, b, c, tAlpha, tBeta, tGamma
      REAL, INTENT (  OUT) :: TheLattice(1:3,1:3)

      REAL, EXTERNAL :: Degrees2Radians
      LOGICAL, EXTERNAL :: ValidCellAxisLength
      REAL    aSTAR(1:3)
      REAL    alpha, beta, gamma ! to resolve call by value / call by reference conflict

      IF (.NOT. ValidCellAxisLength(a)) THEN
        CALL ErrorMessage('Invalid length of a axis')
        RETURN
      ENDIF
      IF (.NOT. ValidCellAxisLength(b)) THEN
        CALL ErrorMessage('Invalid length of b axis')
        RETURN
      ENDIF
      IF (.NOT. ValidCellAxisLength(c)) THEN
        CALL ErrorMessage('Invalid length of c axis')
        RETURN
      ENDIF
      alpha = Degrees2Radians(tAlpha)
      beta  = Degrees2Radians(tBeta)
      gamma = Degrees2Radians(tGamma)
! a-axis
      TheLattice(1,1) = a
      TheLattice(2,1) = 0.0
      TheLattice(3,1) = 0.0
! b-axis
      TheLattice(1,2) = b * COS(gamma)
      TheLattice(2,2) = b * SIN(gamma)
      TheLattice(3,2) = 0.0
! c-axis
      TheLattice(1,3) = c * COS(beta)
! Maybe an odd way to do this, but it works:
! the projection of c on the "a*b*" plane equals the projection of c on the xy plane:
! (ac)a* + (bc)b* = (cx)x + (cy)y
! Multiplying from the right by y* yields:
! (ac)(a*y) + (bc)(b*y) = cy
! Note that y = y* and z = z*
! Since b* = y/|b|, b*y = 1/|b|:
! (ac)(a*y) + c(cos(alpha)) = cy
! We know ac and y and can calculate a*, leaving the unknown cy
      aSTAR = TheLattice(:,1)
      CALL VectorOrthogonalise(aSTAR,TheLattice(:,2))
      CALL VectorSetLength(aSTAR,1.0/a)
      TheLattice(2,3) = (a*c*COS(beta)) * aSTAR(2)   +   c * COS(alpha)
      TheLattice(3,3) = SQRT(c**2-TheLattice(1,3)**2-TheLattice(2,3)**2)

      END SUBROUTINE LatticeCellParameters2Lattice_2
!
!*****************************************************************************
!
      SUBROUTINE VectorNormalise(TheVector)
!
! Normalises a vector to 1
!
      IMPLICIT NONE

      REAL, INTENT (INOUT) :: TheVector(1:3)

      LOGICAL, EXTERNAL ::  VectorIsNullVector
      REAL, EXTERNAL ::     VectorGetLength

      IF (VectorIsNullVector(TheVector)) RETURN
      TheVector = TheVector / VectorGetLength(TheVector)

      END SUBROUTINE VectorNormalise
!
!*****************************************************************************
!
      SUBROUTINE VectorOrthogonalise(TheVector1, TheVector2)
!
! Orthogonalises Vector1 to Vector2
!
      IMPLICIT NONE

      REAL, INTENT (INOUT) :: TheVector1(1:3)
      REAL, INTENT (IN   ) :: TheVector2(1:3)

      LOGICAL, EXTERNAL :: VectorIsNullVector
      REAL, EXTERNAL :: VectorGetLength
      REAL    tVector(1:3)
      REAL    Coefficient
      REAL    OriginalLength

      IF (VectorIsNullVector(TheVector1) .OR. VectorIsNullVector(TheVector2)) RETURN
      OriginalLength = VectorGetLength(TheVector1)
      tVector = TheVector2
      CALL VectorNormalise(tVector)
      Coefficient = DOT_PRODUCT(TheVector1, tVector)
      TheVector1 = TheVector1 - Coefficient * tVector
      CALL VectorSetLength(TheVector1, OriginalLength)

      END SUBROUTINE VectorOrthogonalise
!
!*****************************************************************************
!
      REAL FUNCTION VectorGetLength(TheVector)
!
! Guess
!
      IMPLICIT NONE

      REAL, INTENT (IN   ) :: TheVector(1:3)

      VectorGetLength = SQRT(DOT_PRODUCT(TheVector,TheVector))

      END FUNCTION VectorGetLength
!
!*****************************************************************************
!
      LOGICAL FUNCTION VectorIsNullVector(TheVector)
!
! Guess
!
      IMPLICIT NONE

      REAL, INTENT (IN   ) :: TheVector(1:3)

      REAL, EXTERNAL ::     VectorGetLength
      LOGICAL, EXTERNAL ::  NearlyEqual

      VectorIsNullVector = NearlyEqual(VectorGetLength(TheVector),0.0)

      END FUNCTION VectorIsNullVector
!
!*****************************************************************************
!
      SUBROUTINE VectorSetLength(TheVector, TheLength)
!
! Guess
!
      IMPLICIT NONE

      REAL, INTENT (INOUT) :: TheVector(1:3)
      REAL, INTENT (IN   ) :: TheLength

      CALL VectorNormalise(TheVector)
      TheVector = TheVector * TheLength

      END SUBROUTINE VectorSetLength
!
!*****************************************************************************
!
      SUBROUTINE VectorCrossProduct(Vector1, Vector2, ResultVector);

      IMPLICIT NONE

      REAL, INTENT (IN   ) :: Vector1(1:3), Vector2(1:3)
      REAL, INTENT (  OUT) :: ResultVector(1:3)

      ResultVector(1) = Vector1(2) * Vector2(3) - Vector1(3) * Vector2(2)
      ResultVector(2) = Vector1(3) * Vector2(1) - Vector1(1) * Vector2(3)
      ResultVector(3) = Vector1(1) * Vector2(2) - Vector1(2) * Vector2(1)

      END SUBROUTINE VectorCrossProduct
!
!*****************************************************************************
!
      SUBROUTINE Vector2Quaternion(Vector, Q)

      IMPLICIT NONE

      REAL, INTENT (IN   ) :: Vector(1:3)
      REAL, INTENT (  OUT) :: Q(0:3)

      REAL            PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB
      COMMON /CONSTA/ PI, RAD, DEG, TWOPI, FOURPI, PIBY2, ALOG2, SQL2X8, VALMUB

      REAL tVector(1:3)
      REAL Length, Alpha, Beta
      INTEGER iAxis

      tVector = Vector  ! to resolve call by value / call by reference conflict
      ! Normalise the axis
      Length = SQRT(tVector(1)**2 + tVector(2)**2 + tVector(3)**2)
      DO iAxis = 1, 3
        tVector(iAxis) = tVector(iAxis) / Length
        IF (tVector(iAxis) .GT.  0.99999) tVector(iAxis) =  0.99999
        IF (tVector(iAxis) .LT. -0.99999) tVector(iAxis) = -0.99999
      ENDDO
! Calculate the orientation of the axis
! Note: Alpha and Beta in radians
      Beta  = ACOS(tVector(3))
      IF (ABS(tVector(3)) .GT. 0.99998) THEN
! The axis coincides with the z-axis, so alpha becomes undefined: set alpha to 0.0
        Alpha = 0.0
! I turns out that we can get problems with rounding errors here
      ELSE IF ((-tVector(2)/SIN(Beta)) .GT.  0.99999) THEN
        Alpha = 0.0
      ELSE IF ((-tVector(2)/SIN(Beta)) .LT. -0.99999) THEN
        Alpha = PI
      ELSE
        Alpha = ACOS(-tVector(2)/SIN(Beta))
        IF ((ASIN((tVector(1)))/SIN(Beta)) .LT. 0.0) Alpha = TWOPI - Alpha
      ENDIF
! It's an axis, so Gamma can be set to 0.0
      Q(0) = COS(0.5*Beta) * COS(0.5*Alpha)
      Q(1) = SIN(0.5*Beta) * COS(0.5*Alpha)
      Q(2) = SIN(0.5*Beta) * SIN(0.5*Alpha)
      Q(3) = COS(0.5*Beta) * SIN(0.5*Alpha)

      END SUBROUTINE Vector2Quaternion
!
!*****************************************************************************
!
      SUBROUTINE PremultiplyVectorByMatrix(Matrix, Vector, Ret)

      IMPLICIT NONE

      REAL, INTENT (IN   ) :: Matrix(1:3, 1:3)
      REAL, INTENT (IN   ) :: Vector(1:3)
      REAL, INTENT (  OUT) :: Ret(1:3)

      Ret(1) = Matrix(1,1)*Vector(1) + Matrix(1,2)*Vector(2) + Matrix(1,3)*Vector(3)
      Ret(2) = Matrix(2,1)*Vector(1) + Matrix(2,2)*Vector(2) + Matrix(2,3)*Vector(3)
      Ret(3) = Matrix(3,1)*Vector(1) + Matrix(3,2)*Vector(2) + Matrix(3,3)*Vector(3)

      END SUBROUTINE PremultiplyVectorByMatrix
!
!*****************************************************************************
!
      SUBROUTINE QuaternionMultiply(Q1, Q2, Ret)

! quaternion A times quaternion B:
!
! {A0*1 + A1*i + A2*j + A3*k} * {B0*1 + B1*i + B2*j + B3*k} =
!   (A0*B0 - A1*B1 - A2*B2 - A3*B3)*1 +
!   (A0*B1 + A1*B0 + A2*B3 - A3*B2)*i +
!   (A0*B2 - A1*B3 + A2*B0 + A3*B1)*j +
!   (A0*B3 + A1*B2 - A2*B1 + A3*B0)*k

      IMPLICIT NONE

      REAL, INTENT (IN   ) :: Q1(0:3), Q2(0:3)
      REAL, INTENT (  OUT) :: Ret(0:3)

      Ret(0) = Q1(0)*Q2(0) - Q1(1)*Q2(1) - Q1(2)*Q2(2) - Q1(3)*Q2(3)
      Ret(1) = Q1(0)*Q2(1) + Q1(1)*Q2(0) + Q1(2)*Q2(3) - Q1(3)*Q2(2)
      Ret(2) = Q1(0)*Q2(2) - Q1(1)*Q2(3) + Q1(2)*Q2(0) + Q1(3)*Q2(1)
      Ret(3) = Q1(0)*Q2(3) + Q1(1)*Q2(2) - Q1(2)*Q2(1) + Q1(3)*Q2(0)

      END SUBROUTINE QuaternionMultiply
!
!*****************************************************************************
!
      SUBROUTINE QuaternionInverse(Q)

      IMPLICIT NONE

      REAL, INTENT (INOUT) :: Q(0:3)

      Q(1) = -Q(1)
      Q(2) = -Q(2)
      Q(3) = -Q(3)

      END SUBROUTINE QuaternionInverse
!
!*****************************************************************************
!
