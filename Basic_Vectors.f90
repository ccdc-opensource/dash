!
!*****************************************************************************
!
! This unit contains subroutines for the manipulation of vectors and lattices.
!
!*****************************************************************************
!
      SUBROUTINE LatticeCellParameters2Lattice(a, b, c, alpha, beta, gamma, TheLattice)
!
! Calculates rigth-handed lattice from cell parameters, a along x, b in xy plane
!
! The left-most subscript must vary most rapidly
!
      IMPLICIT NONE

      REAL, INTENT (IN   ) :: a, b, c, alpha, beta, gamma
      REAL, INTENT (  OUT) :: TheLattice(1:3,1:3)

      REAL    Degrees2Radians ! Function
      LOGICAL ValidCellAxisLength ! Function
      REAL    aSTAR(1:3)

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
! a-axis
      TheLattice(1,1) = a
      TheLattice(2,1) = 0.0
      TheLattice(3,1) = 0.0
! b-axis
      TheLattice(1,2) = b*COS(Degrees2Radians(gamma))
      TheLattice(2,2) = b*SIN(Degrees2Radians(gamma))
      TheLattice(3,2) = 0.0
! c-axis
      TheLattice(1,3) = c*COS(Degrees2Radians(beta))
! Maybe an odd way to do this, but it works:
! the projection of c on the a*b* plane equals the projection of c on the xy plane:
! (ac)a* + (bc)b* = (cx)x + (cy)y
! Multiplying from the right by y* yields:
! (ac)(a*y) + (bc)(b*y) = cy
! Note that x = x* and y = y*
! Since b* = y/b, b*y = 1/b:
! (ac)(a*y) + c(cos(alpha)) = cy
! We know ac, bc and y and can calculate a*, leaving the unknown cy
      aSTAR = TheLattice(:,1)
      CALL VectorOrthogonalise(aSTAR,TheLattice(:,2))
      CALL VectorSetLength(aSTAR,1.0/a)
      TheLattice(2,3) = c*COS(Degrees2Radians(alpha)) + aSTAR(2) * (a*c*COS(Degrees2Radians(beta)))
      TheLattice(3,3) = SQRT(c**2-TheLattice(1,3)**2-TheLattice(2,3)**2)

      END SUBROUTINE LatticeCellParameters2Lattice
!
!*****************************************************************************
!
      SUBROUTINE LatticeGetReciprocal(TheRealLattice, TheReciprocalLattice)
!
! This routine uses Gram-Schmidt orthogonalisation to calculate the reciprocal lattice
!
! The left-most subscript must vary most rapidly
!
      IMPLICIT NONE

      REAL, INTENT (IN   ) :: TheRealLattice(1:3,1:3)
      REAL, INTENT (INOUT) :: TheReciprocalLattice(1:3,1:3)





      END SUBROUTINE LatticeGetReciprocal
!
!*****************************************************************************
!
      SUBROUTINE VectorNormalise(TheVector)
!
! Normalises a vector to 1
!
      IMPLICIT NONE

      REAL, INTENT (INOUT) :: TheVector(1:3)

      LOGICAL VectorIsNullVector ! Function
      REAL    VectorGetLength    ! Function

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

      LOGICAL VectorIsNullVector ! Function
      REAL    tVector(1:3)
      REAL    Coefficient
      REAL    OriginalLength
      REAL    VectorGetLength ! Function

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

      REAL    VectorGetLength ! Function
      LOGICAL NearlyEqual     ! Function

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
