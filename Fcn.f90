!
!*****************************************************************************
!
      SUBROUTINE FCN(N,THETA,H)

      DOUBLE PRECISION THETA(*), H

      REAL snglh

      call makefrac(theta)
      call valchi(snglh)
      h=dble(snglh)

      END SUBROUTINE FCN
!
!*****************************************************************************
!
