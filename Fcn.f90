!
!*****************************************************************************
!
      SUBROUTINE FCN(N,THETA,H)

      DOUBLE PRECISION THETA(*), H

      REAL snglh

!O      CALL makefrac(theta)
      CALL makefrac_2(theta)
      CALL valchi(snglh)
      h = DBLE(snglh)

      END SUBROUTINE FCN
!
!*****************************************************************************
!
