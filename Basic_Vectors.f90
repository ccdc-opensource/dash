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
