!
!*****************************************************************************
!
      SUBROUTINE FCN(X,H,CurrentParameter)

      IMPLICIT NONE
      
      DOUBLE PRECISION, INTENT (IN   ) :: X(*)
      DOUBLE PRECISION, INTENT (  OUT) :: H
      INTEGER,          INTENT (IN   ) :: CurrentParameter

      INTEGER         NStPar
      COMMON /pextra/ NStPar

      REAL snglh

! If only e.g. the preferred orientation has changed, there is no need to 
! recalculate all the fractional co-ordinates
      IF (CurrentParameter .LE. NStPar) CALL makefrac(X)
      CALL valchi(snglh,CurrentParameter)
      H = DBLE(snglh)

      END SUBROUTINE FCN
!
!*****************************************************************************
!
