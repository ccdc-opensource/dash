!
! Code having to do with calculating the similarity measure described in:
! R. de Gelder, R. Wehrens, J.A. Hageman (2001), J. Comp. Chem. 22, 273-289.
!
!*****************************************************************************
!
      SUBROUTINE InitRene

      USE WINTERACTER
      USE DRUID_HEADER
      USE REFVAR

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

      LOGICAL         UseRene, UseRelease, UseESD
      INTEGER                                     nwidth
      REAL                                                width, minstep, rwidth, SqrtCorrObs 
      LOGICAL                                                                                   InPeak
      COMMON / RENE / UseRene, UseRelease, UseESD, nwidth, width, minstep, rwidth, SqrtCorrObs, InPeak(1-100:MOBS+100)

      REAL             YOBIN2,         PIKVAL2
      INTEGER                                            NFITA2, IFITA2
      REAL                                                                     WidthFac
      COMMON / RENE2 / YOBIN2(1:MOBS), PIKVAL2(50,MOBS), NFITA2, IFITA2(MOBS), WidthFac(-100:100)

 !     INTEGER
 !     COMMON / RENE3 / 

      REAL, EXTERNAL :: Correl
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      REAL x_step
      REAL Last_X
      INTEGER I, II, K, KK, KK2, n

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_input3_2)
      UseRelease = WDialogGetCheckBoxLogical(IDC_UseRelease)
      IF (UseRelease) THEN
        CALL InitRene_1
        CALL PopActiveWindowID
        RETURN
      ENDIF
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
      KK = 0
      DO II = 1, NFITA
        I = IFITA(II)
        IF ((XBIN(I) - Last_X) .GE. minstep) THEN
          Last_X = XBIN(I)-0.0001 ! To account for rounding errors
          InPeak(I) = .TRUE.
          KK = KK + 1
          IFITA2(KK) = I
          YOBIN2(I) = 0.0
          DO K = 1, KREFT(I)
            KK2 = KNIPT(K,I)
            YOBIN2(I) = YOBIN2(I) + AIOBS(KK2) * PIKVAL(K,I) ! Using PIKVAL2 here does not seem to work (SA for Tut 1 takes about 3 times as long)
          ENDDO
    !      IF (UseESD) YOBIN2(I) = SQRT(MAX(0.0, YOBIN2(I)))
          IF (UseESD) YOBIN2(I) = YOBIN2(I) * (XBIN(I)-XBIN(1))/(XBIN(NBIN)-XBIN(1))
    !      IF (UseESD) YOBIN2(I) = YOBIN2(I) / EBIN(I)
        ENDIF
      ENDDO
      NFITA2 = KK
      DO n = -nwidth+1, nwidth-1
        WidthFac(n) = (1.0 - rwidth*ABS(FLOAT(n)))
      ENDDO
      SqrtCorrObs = SQRT(Correl(YOBIN2, YOBIN2))
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

      INTEGER         KNIPT
      REAL                            PIKVAL
      COMMON /FPINF1/ KNIPT(50,MOBS), PIKVAL(50,MOBS)

      INTEGER         KREFT
      COMMON /FPINF2/ KREFT(MOBS)

      LOGICAL         UseRene, UseRelease, UseESD
      INTEGER                                     nwidth
      REAL                                                width, minstep, rwidth, SqrtCorrObs 
      LOGICAL                                                                                   InPeak
      COMMON / RENE / UseRene, UseRelease, UseESD, nwidth, width, minstep, rwidth, SqrtCorrObs, InPeak(1-100:MOBS+100)

      REAL             YOBIN2,         PIKVAL2
      INTEGER                                            NFITA2, IFITA2
      REAL                                                                     WidthFac
      COMMON / RENE2 / YOBIN2(1:MOBS), PIKVAL2(50,MOBS), NFITA2, IFITA2(MOBS), WidthFac(-100:100)

      REAL, EXTERNAL :: Correl
      REAL    YCALC(1:MOBS), CorrCal, CorrCross
      INTEGER II, I, K, KK

! VALCHI() fills BICALC
      DO II = 1, NFITA2
        I = IFITA2(II)
        YCALC(I) = 0.0
        DO K = 1, KREFT(I)
          KK = KNIPT(K,I)
          YCALC(I) = YCALC(I) + BICALC(KK) * PIKVAL(K,I) ! Using PIKVAL2 here does not seem to work (SA for Tut 1 takes about 3 times as long)
        ENDDO
    !    IF (UseESD) YCALC(I) = SQRT(MAX(0.0, YCALC(I)))
        YCALC(I) = YCALC(I) * (XBIN(I)-XBIN(1))/(XBIN(NBIN)-XBIN(1))
    !    IF (UseESD) YCALC(I) = YCALC(I) / EBIN(I)
      ENDDO
      CorrCal   = Correl(YCALC, YCALC)
      CorrCross = Correl(YOBIN2, YCALC)
      Similarity = 100.0 * (1.0 - CorrCross / (SqrtCorrObs*SQRT(CorrCal)))

      END SUBROUTINE CalculateSimilarity
!
!*****************************************************************************
!
      REAL FUNCTION Correl(y1, y2)

      IMPLICIT NONE

      REAL,    INTENT (IN   ) :: y1(*), y2(*)

      INCLUDE 'PARAMS.INC'

      LOGICAL         UseRene, UseRelease, UseESD
      INTEGER                                     nwidth
      REAL                                                width, minstep, rwidth, SqrtCorrObs 
      LOGICAL                                                                                   InPeak
      COMMON / RENE / UseRene, UseRelease, UseESD, nwidth, width, minstep, rwidth, SqrtCorrObs, InPeak(1-100:MOBS+100)

      REAL             YOBIN2,         PIKVAL2
      INTEGER                                            NFITA2, IFITA2
      REAL                                                                     WidthFac
      COMMON / RENE2 / YOBIN2(1:MOBS), PIKVAL2(50,MOBS), NFITA2, IFITA2(MOBS), WidthFac(-100:100)

      INTEGER I, II, j, n
      REAL ret

      ret = 0.0
      DO II = 1, NFITA2
        I = IFITA2(II)
        DO n = -nwidth+1, nwidth-1
          j = i + n
          IF (InPeak(j)) ret = ret + y1(i) * y2(j) * WidthFac(n)
        ENDDO
      ENDDO
      Correl = ret

      END FUNCTION Correl
!
!*****************************************************************************
!
      SUBROUTINE InitRene_1

      USE WINTERACTER
      USE DRUID_HEADER
      USE REFVAR

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

      LOGICAL         UseRene, UseRelease, UseESD
      INTEGER                                     nwidth
      REAL                                                width, minstep, rwidth, SqrtCorrObs 
      LOGICAL                                                                                   InPeak
      COMMON / RENE / UseRene, UseRelease, UseESD, nwidth, width, minstep, rwidth, SqrtCorrObs, InPeak(1-100:MOBS+100)

      REAL             YOBIN2,         PIKVAL2
      INTEGER                                            NFITA2, IFITA2
      REAL                                                                     WidthFac
      COMMON / RENE2 / YOBIN2(1:MOBS), PIKVAL2(50,MOBS), NFITA2, IFITA2(MOBS), WidthFac(-100:100)

      REAL, EXTERNAL :: Correl_1
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      REAL Last_X
      INTEGER I, II, K, KK, KK2

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_input3_2)
      PAWLEYCHISQ = 1.0
      CALL WDialogGetReal(IDF_MinStep, minstep)
      Last_X = -1.0
      KK = 0
      DO II = 1, NFITA
        I = IFITA(II)
        IF ((XBIN(I) - Last_X) .GE. minstep) THEN
          Last_X = XBIN(I)-0.0001 ! To account for rounding errors
          KK = KK + 1
          IFITA2(KK) = I
          YOBIN2(KK) = 0.0
          DO K = 1, KREFT(I)
            KK2 = KNIPT(K,I)
            YOBIN2(KK) = YOBIN2(KK) + AIOBS(KK2) * PIKVAL(K,I) ! Using PIKVAL2 here does not seem to work (SA for Tut 1 takes about 3 times as long)
          ENDDO
        !  IF (UseESD) YOBIN2(KK) = SQRT(MAX(0.0, YOBIN2(KK)))
        IF (UseESD) YOBIN2(KK) = YOBIN2(KK) * (1.0+2.0*(XBIN(I)-XBIN(1))/(XBIN(NBIN)-XBIN(1)))
       !   IF (UseESD) YOBIN2(KK) = YOBIN2(KK) / EBIN(I)
        ENDIF
      ENDDO
      NFITA2 = KK
      SqrtCorrObs = SQRT(Correl_1(YOBIN2, YOBIN2))
      CALL PopActiveWindowID

      END SUBROUTINE InitRene_1
!
!*****************************************************************************
!
      SUBROUTINE CalculateSimilarity_1(Similarity)
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

      INTEGER         KNIPT
      REAL                            PIKVAL
      COMMON /FPINF1/ KNIPT(50,MOBS), PIKVAL(50,MOBS)

      INTEGER         KREFT
      COMMON /FPINF2/ KREFT(MOBS)

      LOGICAL         UseRene, UseRelease, UseESD
      INTEGER                                     nwidth
      REAL                                                width, minstep, rwidth, SqrtCorrObs 
      LOGICAL                                                                                   InPeak
      COMMON / RENE / UseRene, UseRelease, UseESD, nwidth, width, minstep, rwidth, SqrtCorrObs, InPeak(1-100:MOBS+100)

      REAL             YOBIN2,         PIKVAL2
      INTEGER                                            NFITA2, IFITA2
      REAL                                                                     WidthFac
      COMMON / RENE2 / YOBIN2(1:MOBS), PIKVAL2(50,MOBS), NFITA2, IFITA2(MOBS), WidthFac(-100:100)

      REAL, EXTERNAL :: Correl_1
      REAL    YCALC(1:MOBS), CorrCal, CorrCross
      INTEGER II, I, K, KK

! VALCHI() fills BICALC
      DO II = 1, NFITA2
        I = IFITA2(II)
        YCALC(II) = 0.0
        DO K = 1, KREFT(I)
          KK = KNIPT(K,I)
          YCALC(II) = YCALC(II) + BICALC(KK) * PIKVAL(K,I) ! Using PIKVAL2 here does not seem to work (SA for Tut 1 takes about 3 times as long)
        ENDDO
    !    YCALC(II) = SQRT(MAX(0.0, YCALC(II)))
        YCALC(II) = YCALC(II) * (1.0+2.0*(XBIN(I)-XBIN(1))/(XBIN(NBIN)-XBIN(1)))
    !    YCALC(II) = YCALC(II) / EBIN(I)
      ENDDO
      CorrCal   = Correl_1(YCALC, YCALC)
      CorrCross = Correl_1(YOBIN2, YCALC)
      Similarity = 100.0 * (1.0 - CorrCross / (SqrtCorrObs*SQRT(CorrCal)))

      END SUBROUTINE CalculateSimilarity_1
!
!*****************************************************************************
!
      REAL FUNCTION Correl_1(y1, y2)

      IMPLICIT NONE

      REAL,    INTENT (IN   ) :: y1(*), y2(*)

      INCLUDE 'PARAMS.INC'

      REAL             YOBIN2,         PIKVAL2
      INTEGER                                            NFITA2, IFITA2
      REAL                                                                     WidthFac
      COMMON / RENE2 / YOBIN2(1:MOBS), PIKVAL2(50,MOBS), NFITA2, IFITA2(MOBS), WidthFac(-100:100)

      INTEGER I
      REAL ret

      ret = 0.0
      DO I = 1, NFITA2
        ret = ret + y1(I) * y2(I)
      ENDDO
      Correl_1 = ret

      END FUNCTION Correl_1
!
!*****************************************************************************
!
