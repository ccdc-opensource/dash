!
!*****************************************************************************
!
      SUBROUTINE FCN(N,THETA,H)

!
      use winteracter
      use druid_header
!
      DOUBLE PRECISION THETA(*), H

      INCLUDE 'PARAMS.INC'
      integer ioptb,iopta,ioptt,iz1,iz2,iz3
      common /zmcomi/ ntatm,natoms(maxfrg),&
      ioptb(maxatm,maxfrg),iopta(maxatm,maxfrg),ioptt(maxatm,maxfrg),&
     iz1(maxatm,maxfrg),iz2(maxatm,maxfrg),iz3(maxatm,maxfrg)
      common /zmcomg/ icomflg(maxfrg)

      call makefrac(theta,n)
      call valchi(snglh)
      h=dble(snglh)

      RETURN
      END
!
!*****************************************************************************
!
