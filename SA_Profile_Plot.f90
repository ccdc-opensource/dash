!
!*****************************************************************************
!
      SUBROUTINE SA_Profile_Plot
!
! Not much plotting here. We're synchronising COMMON blocks as usual.
!
      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER          NOBSA, NFITA, IFITA
      REAL                                          CHIOBSA, WTSA
      REAL             XOBSA,         YOBSA,         YCALA,         ESDA
      COMMON /CHISTOP/ NOBSA, NFITA, IFITA(MCHSTP), CHIOBSA, WTSA(MCHSTP),    &
                       XOBSA(MCHSTP), YOBSA(MCHSTP), YCALA(MCHSTP), ESDA(MCHSTP)

      REAL             ycalbest
      COMMON /chibest/ ycalbest(MCHSTP)

      REAL YOSUM, YCSUM, RESCL
      INTEGER I, II

      YOSUM = 0.0
      YCSUM = 0.0
      DO II = 1, NFITA
        I = IFITA(II)
        YOSUM = YOSUM + YOBSA(I)
        YCSUM = YCSUM + YCALbest(I)
      ENDDO
      RESCL = YOSUM / YCSUM
      DO I = 1, NBIN
        YCALbest(I) = RESCL * YCALbest(I)
        YCBIN(I) = YCALBEST(I)
      ENDDO
      CALL Profile_Plot

      END SUBROUTINE SA_Profile_Plot
!
!*****************************************************************************
!
