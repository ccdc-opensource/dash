!
!*****************************************************************************
!
      SUBROUTINE InitRene

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      INTEGER          NFITA, IFITA
      REAL                                 WTSA
      COMMON /CHISTOP/ NFITA, IFITA(MOBS), WTSA(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER         KNIPT
      REAL                            PIKVAL
      COMMON /FPINF1/ KNIPT(50,MOBS), PIKVAL(50,MOBS)

      INTEGER         KREFT
      COMMON /FPINF2/ KREFT(MOBS)

      LOGICAL         UseRene, UseESD
      INTEGER                          nwidth
      REAL                                     width, minstep, rwidth, SqrtCorrObs 
      LOGICAL                                                                       InPeak
      COMMON / RENE / UseRene, UseESD, nwidth, width, minstep, rwidth, SqrtCorrObs, InPeak(1-100:MOBS+100)

      REAL             YOBIN2,         PIKVAL2,          WidthFac
      COMMON / RENE2 / YOBIN2(1:MOBS), PIKVAL2(50,MOBS), WidthFac(-100:100)

      REAL, EXTERNAL :: Correl
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      REAL x_step
      REAL Last_X
      INTEGER I, II, K, n

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_input3_2)
      PAWLEYCHISQ = 1.0
      x_step = (XBIN(NBIN)-XBIN(1)) / FLOAT(NBIN-1)
      UseESD = WDialogGetCheckBoxLogical(IDC_UseESD)
      CALL WDialogGetReal(IDF_Width, width)
      nwidth = NINT((width+0.00001) / x_step) ! NINT() rounds correctly
      nwidth = MAX(1, nwidth)
      nwidth = MIN(100, nwidth)
      rwidth = 1.0 / FLOAT(nwidth)
      CALL WDialogGetReal(IDF_MinStep, minstep)
      InPeak = .FALSE. ! Initialise all to .FALSE.
      Last_X = -1.0
      DO II = 1, NFITA
        I = IFITA(II)
        IF ((XBIN(I) - Last_X) .GE. minstep) THEN
          Last_X = XBIN(I)-0.0001 ! To account for rounding errors
          InPeak(I) = .TRUE.
          DO K = 1, KREFT(I)
            IF (UseESD) THEN
              PIKVAL2(K,I) = PIKVAL(K,I) / EBIN(I)
            ELSE
              PIKVAL2(K,I) = PIKVAL(K,I)
            ENDIF
          ENDDO
          IF (UseESD) THEN
            YOBIN2(I) = YOBIN(I) / EBIN(I)
          ELSE
            YOBIN2(I) = YOBIN(I)
          ENDIF
        ENDIF
      ENDDO
      DO n = -nwidth+1, nwidth-1
        WidthFac(n) = (1.0 - rwidth*ABS(FLOAT(n)))
      ENDDO
      SqrtCorrObs = SQRT(Correl(YOBIN2, YOBIN2, NBIN))
      CALL PopActiveWindowID

      END SUBROUTINE InitRene
!
!*****************************************************************************
!
      SUBROUTINE CalculateSimilarity(Similarity)
!
! Must be called after VALCHI, because it needs the BICALC to have been set up.
!
      USE REFVAR
      
      IMPLICIT NONE

      REAL, INTENT (  OUT) :: Similarity

      INCLUDE 'PARAMS.INC'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

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

      LOGICAL         UseRene, UseESD
      INTEGER                          nwidth
      REAL                                     width, minstep, rwidth, SqrtCorrObs 
      LOGICAL                                                                       InPeak
      COMMON / RENE / UseRene, UseESD, nwidth, width, minstep, rwidth, SqrtCorrObs, InPeak(1-100:MOBS+100)

      REAL             YOBIN2,         PIKVAL2,          WidthFac
      COMMON / RENE2 / YOBIN2(1:MOBS), PIKVAL2(50,MOBS), WidthFac(-100:100)

      REAL, EXTERNAL :: Correl
      REAL    YCALC(1:MOBS), CorrCal, CorrCross
      INTEGER II, I, K, KK

! VALCHI() fills BICALC
      DO II = 1, NFITA
        I = IFITA(II)
        IF (InPeak(I)) THEN
          YCALC(I) = 0.0
          DO K = 1, KREFT(I)
            KK = KNIPT(K,I)
            YCALC(I) = YCALC(I) + BICALC(KK) * PIKVAL(K,I) ! Using PIKVAL2 here does not seem to work (SA for Tut 1 takes about 3 times as long)
          ENDDO
          IF (UseESD) YCALC(I) = YCALC(I) / EBIN(I)
        ENDIF
      ENDDO
      CorrCal   = Correl(YCALC, YCALC, NBIN)
      CorrCross = Correl(YOBIN2, YCALC, NBIN)
      Similarity = 100.0 * (1.0 - CorrCross / (SqrtCorrObs*SQRT(CorrCal)))

      END SUBROUTINE CalculateSimilarity
!
!*****************************************************************************
!
      REAL FUNCTION Correl(y1, y2, NBIN)

      IMPLICIT NONE

      REAL,    INTENT (IN   ) :: y1(*), y2(*)
      INTEGER, INTENT (IN   ) :: NBIN

      INCLUDE 'PARAMS.INC'

      LOGICAL         UseRene, UseESD
      INTEGER                          nwidth
      REAL                                     width, minstep, rwidth, SqrtCorrObs 
      LOGICAL                                                                       InPeak
      COMMON / RENE / UseRene, UseESD, nwidth, width, minstep, rwidth, SqrtCorrObs, InPeak(1-100:MOBS+100)

      REAL             YOBIN2,         PIKVAL2,          WidthFac
      COMMON / RENE2 / YOBIN2(1:MOBS), PIKVAL2(50,MOBS), WidthFac(-100:100)

      INTEGER i, j, n
      REAL ret

      ret = 0.0
      DO i = 1, NBIN
        IF (InPeak(I)) THEN
          DO n = -nwidth+1, nwidth-1
            j = i + n
            IF (InPeak(j)) ret = ret + y1(i) * y2(j) * WidthFac(n)
          ENDDO
        ENDIF
      ENDDO
      Correl = ret

!A Alternative version if nwidth = 1
!A      ret = 0.0
!A      DO i = 1, NBIN
!A        IF (InPeak(I)) THEN
!A          ret = ret + y1(i) * y2(i)
!A        ENDIF
!A      ENDDO
!A      Correl = ret

      END FUNCTION Correl
!
!*****************************************************************************
!
