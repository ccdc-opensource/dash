!
!*****************************************************************************
!
      SUBROUTINE FCN(X, H, CurrentParameter)

      IMPLICIT NONE
      
      REAL,    INTENT (IN   ) :: X(*)
      REAL,    INTENT (  OUT) :: H
      INTEGER, INTENT (IN   ) :: CurrentParameter

      INTEGER         NStPar
      COMMON /pextra/ NStPar

! If only e.g. the preferred orientation has changed, there is no need to 
! recalculate all the fractional co-ordinates
      IF (CurrentParameter .LE. NStPar) CALL makefrac(X)
      CALL valchi(H,CurrentParameter)

      END SUBROUTINE FCN
!
!*****************************************************************************
!
