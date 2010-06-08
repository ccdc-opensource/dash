!
!*****************************************************************************
!
      SUBROUTINE FCN(X, H, CurrentParameter, IncPenalty)

      IMPLICIT NONE
      
      REAL,    INTENT (IN   ) :: X(*)
      REAL,    INTENT (  OUT) :: H
      INTEGER, INTENT (IN   ) :: CurrentParameter
      LOGICAL, INTENT (IN   ) :: IncPenalty

      INTEGER         NStPar
      COMMON /pextra/ NStPar
      
      INCLUDE 'SA_restrain.inc'

! If only e.g. the preferred orientation has changed, there is no need to 
! recalculate all the fractional co-ordinates
      IF (CurrentParameter .LE. NStPar) THEN
        SAPenalty = 0.0
        CALL makefrac(X)
      ENDIF
      CALL valchi(H,CurrentParameter)
      
      IF ( IncPenalty .AND. DRestrNumb .GT. 0 ) THEN
        CALL AddPenalty()
        H = H + SpringWeight * SAPenalty
      ENDIF

      END SUBROUTINE FCN
!
!*****************************************************************************
!
