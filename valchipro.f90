!
!*****************************************************************************
!
      SUBROUTINE VALCHIPRO(chivalpro)
!
! Must be called after VALCHI, because it needs the BICALC to have been set up.
!
      USE REFVAR
      
      IMPLICIT NONE

      REAL, INTENT (  OUT) :: chivalpro

      INCLUDE 'PARAMS.INC'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX

      INTEGER          NFITA, IFITA
      REAL                                 WTSA
      COMMON /CHISTOP/ NFITA, IFITA(MOBS), WTSA(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL            CummChiSqd
      COMMON /CMN007/ CummChiSqd(MOBS)

      INTEGER         KNIPT
      REAL                            PIKVAL
      COMMON /FPINF1/ KNIPT(50,MOBS), PIKVAL(50,MOBS)

      INTEGER         KREFT
      COMMON /FPINF2/ KREFT(MOBS)

      REAL    SUM1, SUM2, YCALC, RESCL, CVP, LastValue
      INTEGER II, I, K, KK
    !  LOGICAL, EXTERNAL :: Get_ShowCumChiSqd
      REAL    tScale

! JvdS VALCHIPRO, which calculates the profile chi-squared, is always
! called after VALCHI. VALCHI already fills BICALC in COMMON /SAREFLN2/
      SUM1 = 0.0
      SUM2 = 0.0
      CummChiSqd = -1.0
      DO II = 1, NFITA
        I = IFITA(II)
        YCALC = 0.0
        DO K = 1, KREFT(I)
          KK = KNIPT(K,I)
          YCALC = YCALC + BICALC(KK) * PIKVAL(K,I)
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
        CummChiSqd(I) = CVP
      ENDDO
      CHIVALPRO = CVP/FLOAT(NFITA-2)
      tScale = ypmax / CVP
      LastValue = 0.0
      DO i = 1, NBIN
        IF (CummChiSqd(i) .LT. 0.0) THEN
          CummChiSqd(i) = LastValue
        ELSE
! Rescale cummulative profile chi-squared
          CummChiSqd(i) = CummChiSqd(i) * tScale
          LastValue = CummChiSqd(i)
        ENDIF
      ENDDO

      END SUBROUTINE VALCHIPRO
!
!*****************************************************************************
!
