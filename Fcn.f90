!
!*****************************************************************************
!
      SUBROUTINE FCN(X,H)

      DOUBLE PRECISION X(*), H

      REAL snglh

      CALL makefrac(X)
      CALL valchi(snglh)
      h = DBLE(snglh)

      END SUBROUTINE FCN
!
!*****************************************************************************
!
