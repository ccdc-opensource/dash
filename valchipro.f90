!
!*****************************************************************************
!
      SUBROUTINE VALCHIPRO(chivalpro)

      IMPLICIT NONE

      REAL, INTENT (  OUT) :: chivalpro

      INCLUDE 'PARAMS.INC'

      REAL              AIOBS,         AICALC
      COMMON /SAREFLNS/ AIOBS(MSAREF), AICALC(MSAREF)

      INTEGER          NFITA, IFITA
      REAL                                 WTSA
      COMMON /CHISTOP/ NFITA, IFITA(MOBS), WTSA(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER         KREFT,         KNIPT
      REAL                                             PIKVAL
      COMMON /FPINF1/ KREFT(MFPINF), KNIPT(50,MFPINF), PIKVAL(50,MFPINF)

      REAL SUM1, SUM2, YCALC, RESCL, CVP
      INTEGER II, I, K, KK

! JvdS VALCHIPRO, which calculates the profile chi-squared, is always
! called after VALCHI. VALCHI already fills AICALC in COMMON /SAREFLNS/ (the COMMON block needs
! to be specified here: they are kept in COMMON /REFLNS/ in Reflns.inc as well)). 
  !    INCLUDE 'AllFFCalc.inc'
      SUM1 = 0.0
      SUM2 = 0.0
      DO II = 1, NFITA
        I = IFITA(II)
        YCALC = 0.0
        DO K = 1, KREFT(I)
          KK = KNIPT(K,I)
          YCALC = YCALC + AICALC(KK) * PIKVAL(K,I)
        ENDDO
        YCBIN(I) = YCALC
        SUM1 = SUM1 + YCALC
        SUM2 = SUM2 + YOBIN(I)
      ENDDO
      RESCL = SUM2 / SUM1
      CVP = 0.0
      DO II = 1, NFITA
        I = IFITA(II)
        YCBIN(I) = RESCL * YCBIN(I)
        CVP = CVP + WTSA(I) * (YOBIN(I) - YCBIN(I))**2
      END DO
!      CHIVALPRO=CHIOBS!-SUM2*SUM2/SUM1
      CHIVALPRO = CVP/FLOAT(NFITA-2)

      END SUBROUTINE VALCHIPRO
!
!*****************************************************************************
!
