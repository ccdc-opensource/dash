!
!*****************************************************************************
!
      SUBROUTINE Profile_Plot

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT)

      LOGICAL, EXTERNAL :: PlotBackground

!   Setup hardcopy options
      IF (IPTYPE .LT. 0) THEN
        CALL IGrInit('HP')
        CALL IGrHardCopyOptions(1,700)
        CALL IGrHardCopyOptions(2,375)
        CALL IGrHardCopy(' ')
      ENDIF
      CALL Plot_Initialise()
      CALL Plot_Panel()
      CALL Plot_Custom_Axes()
      CALL IPgBorder()
      IF (.NOT. NoData) THEN
! Observed profile
        SELECT CASE (ABS(IPTYPE))
          CASE (1) 
            CALL Plot_Observed_Profile()
          CASE (2) 
            CALL Plot_ObsCalc_Profile()
        END SELECT
        IF (.NOT. PastPawley) THEN
! Plot peakfit ranges
          IF (NumPeakFitRange .GT. 0) CALL Plot_PeakFit_Info()
! Plot Background
          IF (PlotBackground()) CALL Plot_Background()
        ENDIF
!  Plot tic marks etc. if appropriate
        CALL Plot_Calculated_Tics()
!   Switch off hardcopy
      ENDIF
      IF (IPTYPE .LT. 0) THEN
        CALL IGrHardCopy('S')
        CALL IGrInit('P')
      ENDIF

      END SUBROUTINE Profile_Plot
!
!*****************************************************************************
!
      SUBROUTINE Plot_Calculated_Tics

      USE WINTERACTER
      USE REFVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'POLY_COLOURS.INC'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX

      REAL    YPGDIF, YTIC1, YTIC2, xgtem, ygtem
      INTEGER II

      IF (NumOfRef .EQ. 0) RETURN
      CALL IGrColourN(KolNumCTic)
      YPGDIF = (YPGMAX-YPGMIN)
      YTIC1 = YPGMIN + 0.94 * YPGDIF
      YTIC2 = YPGMIN + 0.97 * YPGDIF
      DO II = 1, NumOfRef
        IF (RefArgK(II) .GE. XPGMIN .AND. RefArgK(II) .LE. XPGMAX) THEN
          CALL IPgUnitsToGrUnits(RefArgK(II),ytic1,xgtem,ygtem)
          CALL IGrMoveTo(xgtem,ygtem)
          CALL IPgUnitsToGrUnits(RefArgK(II),ytic2,xgtem,ygtem)
          CALL IGrLineTo(xgtem,ygtem)
        ENDIF
      ENDDO
      CALL IGrColourN(KolNumMain)

      END SUBROUTINE Plot_Calculated_Tics
!
!*****************************************************************************
!
      SUBROUTINE Plot_Panel

      USE WINTERACTER

      INCLUDE 'POLY_COLOURS.INC'

      REAL            XPG1, XPG2, YPG1, YPG2
      COMMON /PLTINI/ XPG1, XPG2, YPG1, YPG2

      CALL IGrPaletteRGB(100,253,253,245)
      CALL IGrPaletteRGB(101,250,250,235)
      CALL IGrFillPattern(4,1,1)
      CALL IGrColourN(KolNumPanelOuter)
      CALL IGrRectangle(0.0,0.0,1.0,1.0)
      XD = 0.01
      YD = 0.02
      XT1 = XPG1 - XD
      XT2 = XPG2 + XD
      YT1 = YPG1 - YD
      YT2 = YPG2 + YD
      CALL IGrColourN(KolNumPanelVDark)
      CALL IGrRectangle(XT1,YPG2,XT2,YT2)
      CALL IGrColourN(KolNumPanelDark)
      CALL IGrTriangle(XT1,YT1,XPG1,YPG1,XT1,YPG1)
      CALL IGrRectangle(XT1,YPG1,XPG1,YPG2)
      CALL IGrTriangle(XPG1,YPG2,XT1,YPG2,XT1,YT2)
      CALL IGrColourN(KolNumPanelVLite)
      CALL IGrTriangle(XT1,YT1,XPG1,YT1,XPG1,YPG1)
      CALL IGrRectangle(XPG1,YT1,XT2,YPG1)
      CALL IGrColourN(KolNumPanelLite)
      CALL IGrTriangle(XPG2,YPG2,XT2,YPG2,XT2,YT2)
      CALL IGrTriangle(XPG2,YPG1,XT2,YPG1,XT2,YT1)
      CALL IGrRectangle(XPG2,YPG1,XT2,YPG2)
      CALL IGrColourN(KolNumPGWindow)
      CALL IGrRectangle(XPG1,YPG1,XPG2,YPG2)

      END SUBROUTINE Plot_Panel
!
!*****************************************************************************
!
      SUBROUTINE Plot_Initialise

      USE WINTERACTER

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'POLY_COLOURS.INC'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX

      INTEGER          IPMIN, IPMAX
      COMMON /PROFIPM/ IPMIN, IPMAX

      REAL            XPG1, XPG2, YPG1, YPG2
      COMMON /PLTINI/ XPG1, XPG2, YPG1, YPG2

      CALL IGrSelect(1,0)
      CALL IGrArea(0.0,0.0,1.0,1.0)
      CALL IGrUnits(0.0,0.0,1.0,1.0)
      CALL IGrAreaClear
      CALL IGrCharSpacing('P') ! Proportional
      CALL IGrCharFont(3)
      CALL IGrColourN(KolNumMain)
      CALL IPgScaling('LIN','LIN')
! Draw axes and add scales
      CALL IPgArea(XPG1,YPG1,XPG2,YPG2)
      CALL IPgUnits(xpgmin,ypgmin,xpgmax,ypgmax)
      CALL IRealToString(xpgmin,statbarstr(4)(1:),'(F10.3)')
      CALL IRealToString(xpgmax,statbarstr(5)(1:),'(F10.3)')
      IF ((ypgmax-ypgmin) .LE. 100.0) THEN      
        CALL IRealToString(ypgmin,statbarstr(6)(1:),'(F10.3)')
        CALL IRealToString(ypgmax,statbarstr(7)(1:),'(F10.3)')
      ELSE
        CALL IRealToString(ypgmin,statbarstr(6)(1:),'(F10.1)')
        CALL IRealToString(ypgmax,statbarstr(7)(1:),'(F10.1)')
      END IF
      DO ISB = 4, 7
        CALL WindowOutStatusBar(ISB,STATBARSTR(ISB))
      END DO
      CALL IPgClipRectangle('P')
      CALL IPgBorder()

      END SUBROUTINE Plot_Initialise
!
!*****************************************************************************
!
      SUBROUTINE Plot_Custom_Axes

      USE WINTERACTER

      INCLUDE 'POLY_COLOURS.INC'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX

      INTEGER          IPMIN, IPMAX
      COMMON /PROFIPM/ IPMIN, IPMAX

      REAL YGGMIN, YGGMAX

      CALL IGrColourN(KolNumMain)
      CALL IGrCharSize(1.,1.)
      CALL IPgXScale('TN')
      CALL IPgXScaleTop('T')
      CALL IGrColourN(KolNumMain)
      CALL IPgUnitsToGrUnits(xpgmin,ypgmin,xggmin,yggmin)
      CALL IPgUnitsToGrUnits(xpgmax,ypgmax,xggmax,yggmax)
! Plot y=0 for tidiness
      zero = 0.0
      IF (ypgmin.lt.zero .and. ypgmax.gt.zero) THEN
        CALL IPgUnitsToGrUnits(xpgmin,zero,xggt,yggt)
        CALL IGrMoveTo(xggt,yggt)
        CALL IPgUnitsToGrUnits(xpgmax,zero,xggt,yggt)
        CALL IGrLineTo(xggt,yggt)
      ENDIF
      CALL IGrCharSize(1.,1.2)
      CALL IPgYScaleLeft('TN')
      CALL IPgYScaleRight('T')
      CALL IGrCharSize(1.,1.2)
      IF (XPGMAX .LT. 200.0) THEN
        CALL IPgXLabel('2 theta','C')
      ELSE
        CALL IPgXLabel('time-of-flight (microseconds)','C')
      ENDIF

      ENDSUBROUTINE Plot_Custom_Axes
!
!*****************************************************************************
!
      SUBROUTINE Plot_Background

      USE WINTERACTER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'POLY_COLOURS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      CALL IPgNewPlot(PgPolyLine,1,NBIN)
      CALL IPgStyle(1,0,0,0,KolNumBack,0)
      CALL IPgXYPairs(XBIN,YBBIN)

      END SUBROUTINE Plot_Background
!
!*****************************************************************************
!
      SUBROUTINE Plot_Observed_Profile

      USE WINTERACTER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'POLY_COLOURS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX

      INTEGER          IPMIN, IPMAX
      COMMON /PROFIPM/ IPMIN, IPMAX

      LOGICAL, EXTERNAL :: PlotErrorBars, ConnectPointsObs
      INTEGER I
      REAL sizmtem, xtem, ytem, xgtem, ygtem

      CALL IGrColourN(KolNumMain)
      CALL IPgYLabelLeft('Observed profile','C9')
! Do the error bars - we've precalculated the min & max pointers
      CALL IGrColourN(KolNumObs)
      IF ( PlotErrorBars() ) THEN
        DO I = IPMIN, IPMAX
          xtem = XBIN(I)
          ytem = MAX(YOBIN(I)-EBIN(I),ypgmin)
          ytem = MIN(ytem,ypgmax)
          CALL IPgUnitsToGrUnits(xtem,ytem,xgtem,ygtem)
          CALL IGrMoveTo(xgtem,ygtem)
          ytem = MIN(YOBIN(I)+EBIN(I),ypgmax)
          ytem = MAX(ytem,ypgmin)
          CALL IPgUnitsToGrUnits(xtem,ytem,xgtem,ygtem)
          CALL IGrLineTo(xgtem,ygtem)
        ENDDO
      ENDIF
!      CALL IPgNewGraph(1,NBIN,' ',' ','XY')
! Must allow options here ...      CALL IPgStyle(1,0,3,0,KolNumCal,KolNumObs)
      IF (ConnectPointsObs()) THEN
        CALL IPgNewPlot(PgPolyLine,   1,NBIN)
      ELSE
        CALL IPgNewPlot(PgScatterPlot,1,NBIN)
      ENDIF
      IF (ConnectPointsObs()) THEN
        CALL IPgStyle(1,14,3,0,KolNumObs,KolNumObs)
      ELSE
        CALL IPgStyle(1,14,3,0,0,KolNumObs)
      ENDIF
! 13 specifies a square
      CALL IPgMarker( 1, 13)
      sizmtem = marker_size*FLOAT(500)/FLOAT(ipmax-ipmin)
      sizmtem = MIN(marker_size,sizmtem)
      CALL IGrCharSize(sizmtem,sizmtem)
      IF (ConnectPointsObs()) THEN
        CALL IPgXYPairs(XBIN,YOBIN)
      ELSE
        CALL IPgScatterPlot(XBIN,YOBIN)
      ENDIF
      CALL IGrColourN(KolNumMain)

      ENDSUBROUTINE Plot_Observed_Profile
!
!*****************************************************************************
!
      SUBROUTINE Plot_ObsCalc_Profile

      USE WINTERACTER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'POLY_COLOURS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX

      INTEGER          IPMIN, IPMAX
      COMMON /PROFIPM/ IPMIN, IPMAX

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      REAL            CummChiSqd
      COMMON /CMN007/ CummChiSqd(MOBS)

      REAL    YDIF(MOBS), YADD
      LOGICAL, EXTERNAL :: PlotErrorBars, ConnectPointsObs
      INTEGER I, II 
      REAL    sizmtem, xtem, ytem, xgtem, ygtem
      REAL    tScale

! Rescale cummulative profile chi-squared
      tScale = ypmax / CummChiSqd(NBIN)
      DO i = 1, NBIN
        CummChiSqd(i) = tScale * CummChiSqd(i)
      ENDDO
      CALL IGrColourN(KolNumMain)
      CALL IPgYLabelLeft('Observed profile','C9')
! The y-values of the difference profile.
      YADD = 0.5*(YPGMAX+YPGMIN)
      DO II = MAX(1,IPMIN-1), MIN(NBIN,IPMAX+1)
        YDIF(II) = YADD + YOBIN(II) - YCBIN(II)
      ENDDO
      CALL IPgNewPlot(PgPolyLine,4,NBIN)
      CALL IPgStyle(2,0,0,0,KolNumDif,0)
! Q & D hack
      IF (ConnectPointsObs()) THEN
        CALL IPgStyle(1,0,3,0,KolNumObs,KolNumObs)
      ELSE
        CALL IPgStyle(1,0,3,0,0,KolNumObs)
      ENDIF
! Calculated
      CALL IPgStyle(3,0,0,0,KolNumCal,0)
! Cummulative profile chi-squared
      CALL IPgStyle(4,0,0,0,KolNumMTic,0)
! The following four lines set the markers for the observed profile.
      CALL IPgMarker( 2, 13)
      sizmtem = marker_size*FLOAT(500)/FLOAT(ipmax-ipmin)
      sizmtem = MIN(marker_size,sizmtem)
      CALL IGrCharSize(sizmtem,sizmtem)
! Draw the observed profile. Markers can still be used. Consecutive data points are joined 
! by lines. These can be straight or splines
      CALL IPgXYPairs(XBIN,YOBIN)
! Now draw the difference plofile
      CALL IPgXYPairs(XBIN,YDIF)
!
! Do the error bars - we've precalculated the min & max pointers
!
      CALL IGrColourN(KolNumObs)
      IF (PlotErrorBars()) THEN
        DO I = IPMIN, IPMAX
          xtem = XBIN(I)
          ytem = MAX(YOBIN(I)-EBIN(I),ypgmin)
          ytem = MIN(ytem,ypgmax)
          CALL IPgUnitsToGrUnits(xtem,ytem,xgtem,ygtem)
          CALL IGrMoveTo(xgtem,ygtem)
          ytem = MIN(YOBIN(I)+EBIN(I),ypgmax)
          ytem = MAX(ytem,ypgmin)
          CALL IPgUnitsToGrUnits(xtem,ytem,xgtem,ygtem)
          CALL IGrLineTo(xgtem,ygtem)
        ENDDO
      ENDIF
      CALL IPgXYPairs(XBIN,YCBIN)
      CALL IPgXYPairs(XBIN,CummChiSqd)
      CALL IGrCharSize(1.0,1.0)
      CALL IGrColourN(KolNumMain)

      END SUBROUTINE Plot_ObsCalc_Profile
!
!*****************************************************************************
!
      SUBROUTINE Plot_PeakFit_Info
!
! Plots all the information about the Peak Fitting Option
! Highlight (recolour) the peak fit range and label the peaks.
! If already fitted then show the fitted profile as well.
!
      USE WINTERACTER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'POLY_COLOURS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)
      
      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX

      INTEGER          IPMIN, IPMAX
      COMMON /PROFIPM/ IPMIN, IPMAX

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT)

      CHARACTER*1 ChrPkNum
      CHARACTER*2 ChrPkNum2

      REAL Difference(1:MAX_FITPT)
      INTEGER JJ, iord_peak, I, ipf1, ipf2, II, ix1, ix2, IPresColN, J
      LOGICAL, EXTERNAL :: PlotPeakFitDifferenceProfile
      REAL gxleft, gybot, gxright, gytop, xgtem, ygtem, yt1, yt2, xg1, yg1, yg2, AveESD

      iord_peak = 0
      DO i = 1, NumPeakFitRange
! Draw the hatched rectangle indicating the area swept out by the user
        CALL IGrColourN(KolNumPanelDark)
        CALL IGrFillPattern(Hatched,Medium,DiagUp)
        CALL IPgUnitsToGrUnits(XPF_Range(1,i),ypgmin,gxleft,gybot)
        CALL IPgUnitsToGrUnits(XPF_Range(2,i),ypgmax,gxright,gytop)
        gxleft  = MAX(gxleft,xggmin)
        gxleft  = MIN(gxleft,xggmax)
        gxright = MIN(gxright,xggmax)
        gxright = MAX(gxright,xggmin)
        CALL IGrRectangle(gxleft,gybot,gxright,gytop) 
! Then the fitted peak
        ipf1 = IPF_RPt(i) + 1
        ipf2 = IPF_RPt(i+1)
! Now we do a quick check to see if the range has been set
        IF (RangeFitYN(i)) THEN
          IF (XPeakFit(ipf2) .GT. xpgmin .AND. XPeakFit(ipf1) .LT. xpgmax) THEN
! We can plot the calculated fit
            DO II = ipf1, ipf2
              IF (XpeakFit(II) .GE. xpgmin) THEN
! If this two theta is within the drawing area, and the previous wasn't
! then we need to start at the _previous_ point. Unless that point isn't part of this peak, of course.
                ix1 = MAX(II-1,ipf1)
                GOTO 110
              ENDIF
            ENDDO
! ix1 is now a pointer into XPeakFit, YPeakFit, pointing to the start of the range
 110        DO II = ipf2, ipf1, -1
              IF (XpeakFit(II) .LE. xpgmax) THEN
                ix2 = MIN(II+1,ipf2)
                GOTO 120
              ENDIF
            ENDDO
! ix2 is now a pointer into XPeakFit, YPeakFit, pointing to the end of the range
 120        CONTINUE
            IF (PlotPeakFitDifferenceProfile()) THEN
! Calculate average ESD
              AveESD = 0.0
              DO JJ = ipf1, ipf2
                 AveESD = AveESD + EBIN(IPF_Lo(i)+JJ-ipf1)
              ENDDO
              AveESD = AveESD / SNGL(1+ipf2-ipf1)
              DO JJ = ipf1, ipf2
                 Difference(JJ) = (((YOBIN(IPF_Lo(i)+JJ-ipf1) - YPeakFit(JJ)) * 2.50 * AveESD) / EBIN(IPF_Lo(i)+JJ-ipf1)) + 0.5*(YPGMAX+YPGMIN)
!O                 Difference(JJ) = ((YOBIN(IPF_Lo(i)+JJ-ipf1) - YPeakFit(JJ))) + 0.5*(YPGMAX+YPGMIN)
              ENDDO
              CALL IPgNewPlot(PgPolyLine,2, (1+ix2-ix1) )
              CALL IPgStyle(1,0,0,0,KolNumPeakFit,0)
              CALL IPgStyle(2,0,0,0,KolNumCal,0)
              CALL IPgXYPairs(XPeakFit(ix1),YPeakFit(ix1))
              CALL IPgXYPairs(XPeakFit(ix1),Difference(ix1))
            ELSE
              CALL IPgNewPlot(PgPolyLine,1, (1+ix2-ix1) )
              CALL IPgStyle(1,0,0,0,KolNumPeakFit,0)
              CALL IPgXYPairs(XPeakFit(ix1),YPeakFit(ix1))
            ENDIF
          ENDIF
        ENDIF
        IPresColN = InfoGrScreen(ColourReq)
! label the peaks in this peak fit range with their numbers
        DO j = 1, NumInPFR(i)
          iord_peak = iord_peak + 1
          IF (iord_peak .LT. 10) THEN
            CALL IntegerToString(iord_peak,ChrPkNum,'(I1)')
          ELSE
            CALL IntegerToString(iord_peak,ChrPkNum2,'(I2)')
          ENDIF
          CALL IGrCharSize(Char_Size,Char_Size)
          CALL IPgUnitsToGrUnits(XPF_Pos(j,i),YPF_Pos(j,i),xgtem,ygtem)
          ygtem = MIN(gytop,ygtem+0.05*ABS(gytop-gybot))
          IF (xgtem .GT. xggmin .AND. xgtem .LT. xggmax) THEN
            yt1 = MAX(ypmin,0.)
            yt1 = MIN(yt1,ypmax)
            CALL IPgUnitsToGrUnits(XPF_Pos(j,i),yt1,xg1,yg1)
            yg1 = MIN(yg1,gytop)
            yg1 = MAX(yg1,gybot)
            CALL IGrMoveTo(xg1,yg1)
            CALL IGrColourN(KolNumPeakPos)
            yt2 = MAX(ypmin,YPF_Pos(j,i))
            yt2 = MIN(yt2,ypmax)
            CALL IPgUnitsToGrUnits(XPF_Pos(j,i),yt2,xg1,yg2)
            yg2 = MIN(yg2,gytop)
            yg2 = MAX(yg2,gybot)
            CALL IGrLineTo(xg1,yg2)
            IF (ygtem .GT. gybot .AND. ygtem .LT. gytop) THEN
              IF (iord_peak .LT. 10) THEN
                CALL IGrCharOut(xgtem,ygtem,ChrPkNum)
              ELSE
                CALL IGrCharOut(xgtem,ygtem,ChrPkNum2)
              ENDIF
            ENDIF
          ENDIF   
        ENDDO
        CALL IGrColourN(IPresColN)
      ENDDO
      CALL IGrFillPattern(Outline)

      ENDSUBROUTINE Plot_PeakFit_Info
!
!*****************************************************************************
!
