!
!*****************************************************************************
!
      SUBROUTINE Plot_Alter
!
!  Enable button up and mouse movement events
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'Poly_Colours.inc'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      INTEGER          IPMIN, IPMAX, iStart, iStop, nPoints
      COMMON /PROFIPM/ IPMIN, IPMAX, iStart, iStop, nPoints

      REAL            XPG1, XPG2, YPG1, YPG2
      COMMON /PLTINI/ XPG1, XPG2, YPG1, YPG2

      REAL XCUR(2),YCUR(2),XGCUR(2),YGCUR(2)
      REAL xgcurold, ygcurold

      CALL WindowSelect(0)
      CALL WMessageEnable(MouseButUp, Enabled)
! Set the scale correctly. 
      CALL IGrUnits(0.0, 0.0, 1.0, 1.0)
      CALL IPgArea(XPG1,YPG1,XPG2,YPG2)
      CALL IPgUnits(xpgmin, ypgmin, xpgmax, ypgmax)
      xgcur(1) = EventInfo%GX
      ygcur(1) = EventInfo%GY
      CALL IPgUnitsFromGrUnits(xgcur(1), ygcur(1), xcur(1), ycur(1))
      xgcurold = xgcur(1)
      ygcurold = ygcur(1)
      XPGMINOLD = XPGMIN
      XPGMAXOLD = XPGMAX
      YPGMINOLD = YPGMIN
      YPGMAXOLD = YPGMAX
      CALL IGrFillPattern(0,1,1)
      CALL IGrPlotMode('EOR')
      CALL IGrColourN(KolNumRectSelect)
      ! Draw new
      CALL IGrRectangle(xgcur(1),ygcur(1),xgcurold,ygcurold)
      CALL IGrPlotMode('Normal')
      CALL IGrColourN(InfoGrScreen(PrevColReq))
      DO WHILE ( .TRUE. )
        CALL GetEvent
        IF ( EventInfo%WIN .EQ. 0 ) THEN
          CALL IGrUnits(0.0 ,0.0, 1.0, 1.0)
          CALL IPgArea(XPG1, YPG1, XPG2, YPG2)
          CALL IPgUnits(xpgmin, ypgmin, xpgmax, ypgmax)
          CALL IPgUnitsFromGrUnits(EventInfo%GX,EventInfo%GY,xcur(2),ycur(2))
          SELECT CASE (EventType)
            CASE (MouseMove)
              xgcur(2) = EventInfo%GX
              ygcur(2) = EventInfo%GY
              CALL IGrPlotMode('EOR')
              CALL IGrColourN(KolNumRectSelect)
              CALL IGrFillPattern(0, 1, 1)
              ! Remove old
              CALL IGrRectangle(xgcur(1), ygcur(1), xgcurold, ygcurold)
              ! Draw new
              CALL IGrRectangle(xgcur(1), ygcur(1), xgcur(2), ygcur(2))
              xgcurold = xgcur(2)
              ygcurold = ygcur(2)
              CALL IGrPlotMode('Normal')
              CALL IGrColourN(InfoGrScreen(PrevColReq))
            CASE (MouseButUp)
              xgcur(2) = EventInfo%GX
              ygcur(2) = EventInfo%GY
              CALL WMessageEnable(MouseButUp, Disabled)
              IF (EventInfo%VALUE1 .EQ. LeftButton) THEN
                CALL IGrColourN(KolNumRectSelect)
                CALL IGrPlotMode('EOR')
                CALL IGrFillPattern(0, 1, 1)
                ! Remove old
                CALL IGrRectangle(xgcur(1), ygcur(1), xgcurold, ygcurold)
                CALL IGrPlotMode('Normal')
                CALL IGrColourN(InfoGrScreen(PrevColReq))
                IF (ABS(XCUR(2)-XCUR(1)).LT.0.003*(XPGMAX-XPGMIN)) RETURN
                IF (ABS(YCUR(2)-YCUR(1)).LT.0.003*(YPGMAX-YPGMIN)) RETURN
                XPGMIN = MIN(XCUR(1),XCUR(2))
                XPGMAX = MAX(XCUR(1),XCUR(2))  
                YPGMIN = MIN(YCUR(1),YCUR(2))
                YPGMAX = MAX(YCUR(1),YCUR(2))
              ENDIF
              CALL Get_IPMaxMin
              CALL Profile_Plot
              RETURN  
          END SELECT
        ENDIF
      ENDDO

      END SUBROUTINE Plot_Alter
!
!*****************************************************************************
!
      SUBROUTINE Check_KeyDown

      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      INTEGER          IPMIN, IPMAX, iStart, iStop, nPoints
      COMMON /PROFIPM/ IPMIN, IPMAX, iStart, iStop, nPoints

      REAL xpgdif, xpgav, xtem, ypgdif, xpgmint, xpgmaxt, ypgmint, ypgmaxt
      INTEGER II

! acts on various KeyDown options for the main window
      IF (EventInfo%VALUE1 .NE. KeyBackSpace) THEN
        XPGMINOLD = XPGMIN
        XPGMAXOLD = XPGMAX
        YPGMINOLD = YPGMIN
        YPGMAXOLD = YPGMAX
      ENDIF
      SELECT CASE (EventInfo%VALUE1)
        CASE (KeyPageLeft)
          xpgdif = xpgmax - xpgmin
          xpgmin = MAX(XPMIN,xpgmin-0.02*xpgdif)
          xpgmax = xpgmin + xpgdif
          CALL Get_IPMaxMin 
          CALL Profile_Plot
        CASE (KeyPageRight)
! We're going to move the graph to the right if we can
          xpgdif = xpgmax - xpgmin
          xpgmax = MIN(XPMAX,xpgmax+0.02*xpgdif)
          xpgmin = xpgmax - xpgdif
          CALL Get_IPMaxMin 
          CALL Profile_Plot
        CASE (KeyCursorLeft)
! We're going to move the graph to the left if we can
          xpgdif = xpgmax - xpgmin
          xpgmin = MAX(XPMIN,xpgmin-0.25*xpgdif)
          xpgmax = xpgmin + xpgdif
          CALL Get_IPMaxMin 
          CALL Profile_Plot
        CASE (KeyCursorRight)
! We're going to move the graph to the right if we can
          xpgdif = xpgmax - xpgmin
          xpgmax = MIN(XPMAX,xpgmax+0.25*xpgdif)
          xpgmin = xpgmax - xpgdif
          CALL Get_IPMaxMin 
          CALL Profile_Plot
        CASE (KeyLeftExtreme)
! We're going to move the graph as far left as we can
          xpgdif = xpgmax - xpgmin
          xpgmin = xpmin
          xpgmax = xpgmin + xpgdif
          CALL Get_IPMaxMin 
          CALL Profile_Plot
        CASE (KeyRightExtreme)
! We're going to move the graph as far right as we can
          xpgdif = xpgmax - xpgmin
          xpgmax = xpmax
          xpgmin = xpgmax - xpgdif
          CALL Get_IPMaxMin 
          CALL Profile_Plot      
        CASE (KeyPageDown)
! We're going to expand the xscale by sqrt(2) if we can
          xpgdif = xpgmax - xpgmin
          xpgav = 0.5*(xpgmax+xpgmin)
          xtem = MIN(0.5*(xpmax-xpmin),0.7071*xpgdif)
          xpgmin = xpgav - xtem
          xpgmax = xpgav + xtem
          IF (xpgmin.LT.xpmin) THEN
            xpgmin = xpmin
            xpgmax = xpgmin + 2.0 * xtem
          ELSE IF (xpgmax.GT.xpmax) THEN
            xpgmax = xpmax
            xpgmin = xpgmax - 2.0 * xtem
          ENDIF
          CALL Get_IPMaxMin 
          CALL Profile_Plot
        CASE (KeyPageUp)
! We're going to contract the xscale by sqrt(2)
          xpgdif = xpgmax - xpgmin
          xpgav = 0.5 * (xpgmax+xpgmin)
          xtem = 0.3536 * xpgdif
          xpgmin = xpgav - xtem
          xpgmax = xpgav + xtem
          CALL Get_IPMaxMin 
          CALL Profile_Plot
        CASE (KeyCursorDown)
! We're going to move the graph down if we can
          ypgdif = ypgmax - ypgmin
          ypgmin = MAX(ypmin,ypgmin-0.25*ypgdif)
          ypgmax = ypgmin + ypgdif
          CALL Get_IPMaxMin
          CALL Profile_Plot
        CASE (KeyCursorUp)
! We're going to move the graph up if we can
          ypgdif = ypgmax - ypgmin
          ypgmax = MIN(ypmax,ypgmax+0.25*ypgdif)
          ypgmin = ypgmax - ypgdif
          CALL Get_IPMaxMin 
          CALL Profile_Plot
        CASE (KeyUpExtreme)
! We're going to scale to min/max y over the current range
          ii = ypgmin
          ypgmin = ypgmax
          ypgmax = ii
          DO ii = 1, NBIN
            IF(XBIN(ii).GE.xpgmin .AND. XBIN(ii).LE.xpgmax) THEN
              ypgmin = MIN(YOBIN(ii),ypgmin)
              ypgmax = MAX(YOBIN(ii),ypgmax)
            ENDIF
          ENDDO
          CALL Get_IPMaxMin 
          CALL Profile_Plot
        CASE (KeyBackspace)
! Undo last zoom action
          xpgmint = xpgmin
          xpgmaxt = xpgmax
          ypgmint = ypgmin
          ypgmaxt = ypgmax
          xpgmin = xpgminold
          xpgmax = xpgmaxold
          ypgmin = ypgminold
          ypgmax = ypgmaxold
          xpgminold = xpgmint
          xpgmaxold = xpgmaxt
          ypgminold = ypgmint
          ypgmaxold = ypgmaxt
          CALL Get_IPMaxMin 
          CALL Profile_Plot
        CASE (KeyHome)
! Back to full profile range
          xpgmin = xpmin
          xpgmax = xpmax
          ypgmin = ypmin
          ypgmax = ypmax
          CALL Get_IPMaxMin 
          CALL Profile_Plot     
      END SELECT

      END SUBROUTINE Check_KeyDown
!
!*****************************************************************************
!
      SUBROUTINE Get_IPMaxMin

      INCLUDE 'PARAMS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER          IPMIN, IPMAX, iStart, iStop, nPoints
      COMMON /PROFIPM/ IPMIN, IPMAX, iStart, iStop, nPoints

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      DO I = 1, NBIN
        IF (XBIN(I) .GT. XPGMIN) THEN
          IPMIN = I
          GOTO 110
        ENDIF
      ENDDO
  110 DO I = NBIN, 1, -1
        IF (XBIN(I) .LT. XPGMAX) THEN
          IPMAX = I
          GOTO 112
        ENDIF
      ENDDO
  112 CONTINUE
      iStart = MAX(   1, IPMIN-1)
      iStop  = MIN(NBIN, IPMAX+1)
      nPoints = 1 + iStop - iStart

      END SUBROUTINE Get_IPMaxMin
!
!*****************************************************************************
!
      SUBROUTINE HighLightPFR

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Poly_Colours.inc'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      REAL              PF_FWHM,                    PF_IntBreadth
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT),         &
                        PF_FWHM(MAX_NPFR),          PF_IntBreadth(MAX_NPFR)

      INTEGER        CurrHiLiPFR
      COMMON /HLPFR/ CurrHiLiPFR

      REAL xCur, yCur
      REAL ulpx, lrpx, ulgx, ulgy, lrgx, lrgy

      IF (PastPawley .OR. (NumPeakFitRange .EQ. 0)) RETURN
      CALL IPgUnitsFromGrUnits(EventInfo%GX, EventInfo%GY, xCur, yCur)
! Get current PFR
      CALL DetermineCurrentPeakFitRange(xCur, yCur)
! Check if current range equal current highlighted
      IF (CurrentRange .NE. CurrHiLiPFR) THEN
        CALL IGrPlotMode('EOR')
        CALL IGrColourN(KolNumPGWindow)
        CALL IGrFillPattern(Outline, Medium, DiagUp)
! Unhighlight current highlighted
        IF (CurrHiLiPFR .NE. 0) THEN
          ulpx = MAX(XPF_Range(1,CurrHiLiPFR), XPGMIN)
          ulpx = MIN(ulpx, XPGMAX)
          lrpx = MAX(XPF_Range(2,CurrHiLiPFR), XPGMIN)
          lrpx = MIN(lrpx, XPGMAX)
          CALL IPgUnitsToGrUnits(ulpx, YPGMAX, ulgx, ulgy)
          CALL IPgUnitsToGrUnits(lrpx, YPGMIN, lrgx, lrgy)
          CALL IGrColourN(KolNumPanelDark)
          CALL IGrRectangle(ulgx, ulgy, lrgx, lrgy) 
        ENDIF
! Highlight current PFR
        IF (CurrentRange .NE. 0) THEN
          ulpx = MAX(XPF_Range(1,CurrentRange), XPGMIN)
          ulpx = MIN(ulpx, XPGMAX)
          lrpx = MAX(XPF_Range(2,CurrentRange), XPGMIN)
          lrpx = MIN(lrpx, XPGMAX)
          CALL IPgUnitsToGrUnits(ulpx, YPGMAX, ulgx, ulgy)
          CALL IPgUnitsToGrUnits(lrpx, YPGMIN, lrgx, lrgy)
          CALL IGrColourN(KolNumPanelDark)
          CALL IGrRectangle(ulgx, ulgy, lrgx, lrgy) 
        ENDIF
        CALL IGrFillPattern(Outline, Medium, DiagUp)
        CALL IGrPlotMode('Normal')
        CALL IGrColourN(InfoGrScreen(PrevColReq))
        CurrHiLiPFR = CurrentRange
      ENDIF

      END SUBROUTINE HighLightPFR
!
!*****************************************************************************
!
      SUBROUTINE DisplayHKL
!
! Called whenever the mouse is moved. Display h,k,l if over a tick mark.

      USE WINTERACTER
      USE VARIABLES
      USE REFVAR

      IMPLICIT NONE

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      REAL            XPG1, XPG2, YPG1, YPG2
      COMMON /PLTINI/ XPG1, XPG2, YPG1, YPG2

      REAL xCur, yCur, diff
      INTEGER II, ClosestRef
      CHARACTER*(12) hklString

      IF (NumOfRef .EQ. 0) RETURN
! Set the scale correctly. 
      CALL IGrUnits(0.0,0.0,1.0,1.0)
      CALL IPgArea(XPG1, YPG1, XPG2, YPG2)
      CALL IPgUnits(xpgmin,ypgmin,xpgmax,ypgmax)
      CALL IPgUnitsFromGrUnits(EventInfo%GX, EventInfo%GY, xCur, yCur)
! xCur and yCur are now the current mouse coordinates in Pg units
      diff = ABS(RefArgK(1) - xCur)
      ClosestRef = 1
      DO II = 2, NumOfRef
        IF (RefArgK(II) .GE. XPGMIN .AND. RefArgK(II) .LE. XPGMAX) THEN
          IF (ABS(RefArgK(II) - xCur) .LT. diff) THEN
            diff = ABS(RefArgK(II) - xCur)
            ClosestRef = II
          ENDIF
        ENDIF
      ENDDO
      WRITE(hklString,'(I3,",",I3,",",I3)') iHKL(1,ClosestRef), iHKL(2,ClosestRef), iHKL(3,ClosestRef)
      CALL WindowOutStatusBar(5, hklString)

      END SUBROUTINE DisplayHKL
!
!*****************************************************************************
!
      SUBROUTINE UpdateMousePosition

      USE VARIABLES

      IMPLICIT NONE

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      REAL            XPG1, XPG2, YPG1, YPG2
      COMMON /PLTINI/ XPG1, XPG2, YPG1, YPG2

      REAL xCur, yCur
      CHARACTER*(12) tString

      IF (NoData) RETURN
! Set the scale correctly. 
      CALL IGrUnits(0.0, 0.0, 1.0, 1.0)
      CALL IPgArea(XPG1, YPG1, XPG2, YPG2)
      CALL IPgUnits(XPGMIN, YPGMIN, XPGMAX, YPGMAX)
      CALL IPgUnitsFromGrUnits(EventInfo%GX, EventInfo%GY, xCur, yCur)
      CALL IRealToString(xCur, tString, '(F10.3)')
      CALL WindowOutStatusBar(3, tString)
      IF (YPGMAX-YPGMIN .LE. 100.0) THEN
        CALL IRealToString(yCur,tString, '(F10.3)')
      ELSE
        CALL IRealToString(yCur, tString, '(F10.1)')
      ENDIF
      CALL WindowOutStatusBar(4, tString)

      END SUBROUTINE UpdateMousePosition
!
!*****************************************************************************
!
      SUBROUTINE Move_CrossHair_Fit
!
! Draws peak fit ranges ("hatched areas")
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Poly_Colours.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      INTEGER          IPMIN, IPMAX, iStart, iStop, nPoints
      COMMON /PROFIPM/ IPMIN, IPMAX, iStart, iStop, nPoints

      REAL XCUR(2), YCUR(2), XGCUR(2)

      REAL            XCurFirst
      COMMON /CURVAL/ XCurFirst

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      REAL              PF_FWHM,                    PF_IntBreadth
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT),         &
                        PF_FWHM(MAX_NPFR),          PF_IntBreadth(MAX_NPFR)
 
      REAL xgcurold
      SAVE xgcurold

      INTEGER ii, iPFRange, iPFL1, iPFL2
      REAL    XPFR1, XPFR2
      REAL    GXMIN, GYMIN, GXMAX, GYMAX

! Get ready to put up the big cursor
      CALL WMessageEnable(MouseButUp, Enabled)
      CALL IPgUnitsToGrUnits(xpgmin, ypgmin, gxmin, gymin)
      CALL IPgUnitsToGrUnits(xpgmax, ypgmax, gxmax, gymax)
      xgcur(1) = EventInfo%GX
      CALL IPgUnitsFromGrUnits(xgcur(1), EventInfo%GY, xcur(1), ycur(1))
      XCurFirst = xcur(1)
! The GetEvent() loop is solely concerned with determining the range
! over which we will fit the Bragg peak(s) so we will only check out
! MouseMove, MouseButUp and a very limited number of
! KeyDown options at this stage
      xgcurold = xgcur(1)
      CALL IGrPlotMode('EOR')
      CALL IGrColourN(KolNumLargeCrossHair)
      CALL IGrFillPattern(Hatched,Medium,DiagUp)
      CALL IGrRectangle(xgcur(1),gymin,xgcurold,gymax)
      CALL IGrFillPattern(Outline, Medium, DiagUp)
      CALL IGrPlotMode('Normal')
      CALL IGrColourN(InfoGrScreen(PrevColReq))
      DO WHILE (.TRUE.)
        CALL GetEvent
        xgcur(2) = EventInfo%GX
        CALL IPgUnitsFromGrUnits(xgcur(2),EventInfo%GY,xcur(2),ycur(2))
        SELECT CASE (EventType)
          CASE (KeyDown)
            CALL Check_KeyDown_PeakFit_Inner
          CASE (MouseMove)
! Set up the cross-hairs for peak finding
! Draw cross-hair
! Remove old cross-hair
            CALL IGrPlotMode('EOR')
            CALL IGrColourN(KolNumLargeCrossHair)
            CALL IGrFillPattern(Hatched,Medium,DiagUp)
            CALL IGrRectangle(xgcur(1),gymin,xgcurold,gymax)
! Paint new cross-hair
            CALL IGrRectangle(xgcur(1),gymin,xgcur(2),gymax)
            CALL IGrFillPattern(Outline)
            xgcurold = xgcur(2)
            CALL IGrPlotMode('Normal')
            CALL IGrColourN(InfoGrScreen(PrevColReq))
          CASE (MouseButUp)
            CALL WMessageEnable(MouseButUp, Disabled)
! MouseButUp action for selecting the peak fitting region
! Remove old cross-hair
            CALL IGrPlotMode('EOR')
            CALL IGrColourN(KolNumLargeCrossHair)
            CALL IGrFillPattern(Hatched,Medium,DiagUp)
            CALL IGrRectangle(xgcur(1),gymin,xgcurold,gymax)
            CALL IGrFillPattern(Outline)
            CALL IGrPlotMode('Normal')
            CALL IGrColourN(InfoGrScreen(PrevColReq))
            XPFR1 = MIN(xcur(1),xcur(2))
            XPFR2 = MAX(xcur(1),xcur(2))
! Determine peak fitting range
            IPFL1 = 1
            DO ii = 1, NBIN
              IF (XBIN(ii) .GE. XPFR1) THEN
                IPFL1 = ii
                GOTO 55
              ENDIF
            ENDDO
   55       IPFL2 = NBIN
            DO ii = NBIN, 1, -1
              IF (XBIN(ii) .LE. XPFR2) THEN
                IPFL2 = ii
                GOTO 60
              ENDIF
            ENDDO
   60       CONTINUE
            IPFRANGE = 1 + IPFL2 - IPFL1
            IF (IPFRANGE .EQ. 0) THEN
              CALL InfoMessage('To select a peak, drag the mouse while pressing the right mouse button.')
            ELSE IF (IPFRANGE .LT. 15) THEN
              CALL ErrorMessage('Not enough points for peak fitting!'//CHAR(13)//'Try a larger range.')
            ELSE
              NumPeakFitRange = NumPeakFitRange + 1
              RangeFitYN(NumPeakFitRange) = .FALSE.
              XPF_Range(1,NumPeakFitRange) = XPFR1
              XPF_Range(2,NumPeakFitRange) = XPFR2
              IPF_Lo(NumPeakFitRange) = IPFL1
              IPF_Hi(NumPeakFitRange) = IPFL2
              IPF_Range(NumPeakFitRange) = 1 + IPF_Hi(NumPeakFitRange) - IPF_Lo(NumPeakFitRange)
              NumInPFR(NumPeakFitRange) = 0
              CALL UpdatePeaksButtonsStates
! Now we have the range in terms of the profile point index
              CALL IGrColourN(KolNumPanelDark)
              CALL IGrFillPattern(Hatched, Medium, DiagUp)
              CALL IGrRectangle(xgcur(1), gymin, xgcurold, gymax)
              CALL IGrFillPattern(Outline)
              CALL IGrColourN(InfoGrScreen(PrevColReq))
            ENDIF
            RETURN 
        END SELECT
      ENDDO 

      END SUBROUTINE Move_CrossHair_Fit
!
!*****************************************************************************
!
      SUBROUTINE Check_KeyDown_PeakFit_Inner

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Poly_Colours.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      INTEGER          IPMIN, IPMAX, iStart, iStop, nPoints
      COMMON /PROFIPM/ IPMIN, IPMAX, iStart, iStop, nPoints

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      REAL              PF_FWHM,                    PF_IntBreadth
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT),         &
                        PF_FWHM(MAX_NPFR),          PF_IntBreadth(MAX_NPFR)

      REAL            XCurFirst
      COMMON /CURVAL/ XCurFirst
      
      REAL              PkFnVal,                      PkFnEsd,                      &
                        PkFnCal,                                                    &
                        PkAreaVal,                    PkAreaEsd,                    &
                        PkPosVal,                     PkPosEsd,                     &
                        PkPosAv
      COMMON /PEAKFIT2/ PkFnVal(MPkDes,Max_NPFR),     PkFnEsd(MPkDes,Max_NPFR),     &
                        PkFnCal(MPkDes,Max_NPFR),                                   &
                        PkAreaVal(MAX_NPPR,MAX_NPFR), PkAreaEsd(MAX_NPPR,MAX_NPFR), &
                        PkPosVal(MAX_NPPR,MAX_NPFR),  PkPosEsd(MAX_NPPR,MAX_NPFR),  &
                        PkPosAv(MAX_NPFR)

      LOGICAL, EXTERNAL :: Confirm
      INTEGER II, IP, NTPEAK, iNewNumber, iOldNumber, iPeakNr, iPoint
      REAL    ATEM, ANEW
      LOGICAL ReplotNecessary, RecalculationNecessary
      REAL    xCur(2), yCur(2)

      ReplotNecessary        = .FALSE.
      RecalculationNecessary = .FALSE.
      xCur(1) = XCurFirst
      CALL IPgUnitsFromGrUnits(EventInfo%GX,EventInfo%GY,xCur(2),yCur(2))
      CALL DetermineCurrentPeakFitRange(XCur(2), yCur(2))
      SELECT CASE (EventInfo%VALUE1)
        CASE (KeyDeleteUnder)
! Delete the nearest peak fitting range but ask first ...
          IF (NumPeakFitRange .EQ. 0) THEN
            CALL InfoMessage('No peak fitting ranges to delete.')
          ELSE
! Check if in a peak range - if not tell the user...
            IF (CurrentRange .EQ. 0) THEN
! Tell the user to place the cursor in the range to be fitted.
              CALL InfoMessage('Place the cursor in a peak fitting range.')
            ELSE
              IF (Confirm('Do you really want to'//CHAR(13)//' delete this peak fitting range?')) THEN
                ReplotNecessary        = .TRUE.
                RecalculationNecessary = .TRUE.
! The cursor is sitting inside the peak range - remove the range
                IF (CurrentRange .EQ. NumPeakFitRange) THEN
! The range to be deleted is the last range in the list, no re-shuffling needed
                  NumPeakFitRange = NumPeakFitRange - 1
                ELSE
! CurrentRange now holds the peak fit range we will delete. Re-shuffle all other peak fit ranges.
                  iNewNumber = 0
                  DO iOldNumber = 1, NumPeakFitRange
                    IF (iOldNumber .NE. CurrentRange) THEN
                      CALL INC(iNewNumber)
                      IF (iOldNumber .GT. iNewNumber) THEN
                        XPF_Range(1,iNewNumber) = XPF_Range(1,iOldNumber)
                        XPF_Range(2,iNewNumber) = XPF_Range(2,iOldNumber)
                        RangeFitYN(iNewNumber) = RangeFitYN(iOldNumber)
                        IPF_Lo(iNewNumber) = IPF_Lo(iOldNumber)
                        IPF_Hi(iNewNumber) = IPF_Hi(iOldNumber)
                        IPF_Range(iNewNumber) = IPF_Range(iOldNumber)
                        NumInPFR(iNewNumber) = NumInPFR(iOldNumber)
                        DO iPeakNr = 1, NumInPFR(iOldNumber)
                          XPF_Pos(iPeakNr,iNewNumber) = XPF_Pos(iPeakNr,iOldNumber)
                          YPF_Pos(iPeakNr,iNewNumber) = YPF_Pos(iPeakNr,iOldNumber)
                        ENDDO
                        DO iPoint = 1, IPF_Range(iOldNumber)
                          XPeakFit(IPF_RPt(iNewNumber)+iPoint) = XPeakFit(IPF_RPt(iOldNumber)+iPoint)
                          YPeakFit(IPF_RPt(iNewNumber)+iPoint) = YPeakFit(IPF_RPt(iOldNumber)+iPoint)
                        ENDDO
                        IPF_RPt(iNewNumber+1) = IPF_RPt(iNewNumber) + IPF_Range(iNewNumber)
                        DO IP = 1, MPkDes
                          PkFnVal(IP,iNewNumber) = PkFnVal(IP,iOldNumber)
                          PkFnEsd(IP,iNewNumber) = PkFnEsd(IP,iOldNumber)
                          ! PkFnCal, PkFnVarVal and PkFnVarEsd are recalculated in Upload_Widths()
                        ENDDO
                        DO iPeakNr = 1, NumInPFR(iOldNumber)
                          PkAreaVal(iPeakNr,iNewNumber) = PkAreaVal(iPeakNr,iOldNumber)
                          PkAreaEsd(iPeakNr,iNewNumber) = PkAreaEsd(iPeakNr,iOldNumber)
                          PkPosVal(iPeakNr,iNewNumber) = PkPosVal(iPeakNr,iOldNumber)
                          PkPosEsd(iPeakNr,iNewNumber) = PkPosEsd(iPeakNr,iOldNumber)
                        ENDDO
                        PkPosAv(iNewNumber) = PkPosAv(iOldNumber)
                      ENDIF ! This PFR needed re-shuffling                            
                    ENDIF
                  ENDDO
                  NumPeakFitRange = NumPeakFitRange - 1
                  IPF_RPt(1) = 0
                  DO II = 1, NumPeakFitRange
                    IPF_RPt(II+1) = IPF_RPt(II) + IPF_Range(II)
                  ENDDO
                ENDIF ! Re-shuffling necessary
              ENDIF ! Confirm that this PFR must be deleted
            ENDIF ! Cursor in a PFR?
          ENDIF ! NumPeakFitRange.EQ.0
        CASE (49:57,KeyInsert)
! KeyNumber=1-9: locating peak positions...
! Insert: add another peak
! Are we in a peak range?
          IF (CurrentRange .NE. 0) THEN
            IF (EventInfo%VALUE1 .EQ. KeyInsert) THEN
              NTPeak = NumInPFR(CurrentRange) + 1
            ELSE
              NTPeak = EventInfo%VALUE1 - 48
            ENDIF
! Three cases : 1. existing peak, 2. next peak, 3. too big.
! Next peak
            IF (NTPeak .EQ. (NumInPFR(CurrentRange) + 1)) CALL INC(NumInPFR(CurrentRange))
            IF (NTPeak .LE. NumInPFR(CurrentRange)) THEN
              ReplotNecessary = .TRUE.
              IF (RangeFitYN(CurrentRange)) RecalculationNecessary = .TRUE.
! When we are here, we are either adding a peak or shifting an old one.
! Either way, mark the hatched area as 'not fitted'
              RangeFitYN(CurrentRange) = .FALSE.
              XPF_Pos(NTPeak,CurrentRange) = XCur(2)
              ATem = ABS(XCur(2)-XBIN(IPF_Lo(CurrentRange)))
              DO IP = IPF_Lo(CurrentRange), IPF_Hi(CurrentRange)
                ANew = ABS(XCur(2)-XBIN(IP))
                IF (ANew.LE.ATem) THEN
                  ATem = ANew
                  YPF_Pos(NTPeak,CurrentRange) = YOBIN(IP)
                ENDIF
              ENDDO
            ENDIF
          ENDIF
! We've got ourselves a new initial peak position
        CASE (48,KeyReturn)
! KeyNumber=0 or KeyReturn: get ready to fit peaks ...
! Check if in a peak range - if not tell the user...
          IF (CurrentRange .EQ. 0) THEN
! Tell the user to place the cursor in the range to be fitted.
            CALL ErrorMessage('Place the cursor in a peak fitting range.')
          ELSE
! We're ready to fit the Bragg peaks
! One or more peaks to be fitted - initial positions determined by user
! If NumInPFR(CurrentRange).EQ.0 we're going to search & fit a single peak
            CALL WCursorShape(CurHourGlass)
            CALL MultiPeak_Fitter
            CALL WCursorShape(CurCrossHair)
            ReplotNecessary        = .TRUE.
            RecalculationNecessary = .TRUE.
          ENDIF
      END SELECT
      IF (RecalculationNecessary) THEN
        CALL Upload_Positions
! Now do a refinement ...
        CALL RefineLattice
        CALL Upload_Widths
      ENDIF
      IF (ReplotNecessary) CALL Profile_Plot
      CALL UpdatePeaksButtonsStates
      CALL CheckIfWeCanDoAPawleyRefinement

      END SUBROUTINE Check_KeyDown_PeakFit_Inner
!
!*****************************************************************************
!
      SUBROUTINE DetermineCurrentPeakFitRange(xCur, yCur)
!
! This routine determines which peak fit range the mouse cursor is currently in
! and returns the number in the global variable CurrentRange.
! CurrentRange is set to 0 if there are no peak fit ranges or 
! if the mouse cursor is not inside one.
! If peak fit ranges overlap, the most recent one is returned. This way, the sequence
! sweep area, press return, sweep area, press return will make sense even if the mouse
! cursor is placed in an area where the two areas overlap.
!
      IMPLICIT NONE

      REAL, INTENT (IN   ) :: xCur, yCur ! Mouse cursor position in PgUnits

      INCLUDE 'PARAMS.INC'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      REAL              PF_FWHM,                    PF_IntBreadth
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT),         &
                        PF_FWHM(MAX_NPFR),          PF_IntBreadth(MAX_NPFR)

      INTEGER I

      CurrentRange = 0
      IF ((yCur .LT. YPGMIN) .OR. (yCur .GT. YPGMAX)) RETURN
      IF (NumPeakFitRange .EQ. 0) RETURN
      DO I = 1, NumPeakFitRange
        IF ((xCur .GE. XPF_Range(1,I)) .AND. (xCur .LE. XPF_Range(2,I))) THEN
          CurrentRange = I
        ENDIF
      ENDDO

      END SUBROUTINE DetermineCurrentPeakFitRange
!
!*****************************************************************************
!
      SUBROUTINE UpdatePeaksButtonsStates

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      REAL              PF_FWHM,                    PF_IntBreadth
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT),         &
                        PF_FWHM(MAX_NPFR),          PF_IntBreadth(MAX_NPFR)

      INTEGER I, iState, NPeaksFitted, IndexOption

      CALL PushActiveWindowID
! Determines whether the Fit Peaks button should be greyed out or not.
! The button is ungreyed if at least one non-fitted peak fit range exists
      iState = WintOff
      IF (NumPeakFitRange .NE. 0) THEN
        DO I = 1, NumPeakFitRange
          IF (.NOT. RangeFitYN(I)) iState = WintOn
        ENDDO
      ENDIF
      CALL WMenuSetState(ID_FitPeaks, ItemEnabled, iState)
! Ungrey / grey out 'Delete all peak fit ranges' button on toolbar
      IF (NumPeakFitRange .EQ. 0) THEN
        iState = WintOff
      ELSE
        iState = WintOn
      ENDIF
      CALL WMenuSetState(ID_ClearPeakFitRanges, ItemEnabled, iState)
! Ungrey / grey out 'Clear Peaks' button in Wizard window
      CALL WDialogSelect(IDD_PW_Page10)
      CALL WDialogFieldStateLogical(IDF_ClearPeakFitRanges, NumPeakFitRange .NE. 0)
      NPeaksFitted = 0
! Loop over all hatched areas. Per area, count all peaks that the user has indicated to be present.
      IF (NumPeakFitRange .GT. 0) THEN
        DO I = 1, NumPeakFitRange
          IF (NumInPFR(I) .EQ. 0) THEN
            NPeaksFitted = NPeaksFitted + 1
          ELSE
            NPeaksFitted = NPeaksFitted + NumInPFR(I)
          ENDIF
        ENDDO
      ENDIF
      IF (NPeaksFitted .GE. 10) THEN
        CALL WDialogSelect(IDD_PW_Page7)
        CALL WDialogFieldState(IDNEXT, Enabled)
        CALL WDialogSelect(IDD_PW_Page8)
        CALL WDialogFieldState(IDNEXT, Enabled) ! The 'Run >' button
        CALL WDialogSelect(IDD_PW_Page8b)
        CALL WDialogFieldState(IDNEXT, Enabled) ! The 'Run >' button
      ELSE
        CALL WDialogSelect(IDD_PW_Page7)
        CALL WDialogGetRadioButton(IDF_RADIO1, IndexOption) ! 'Index now' or 'Enter known cell'
        CALL WDialogFieldStateLogical(IDNEXT, IndexOption .EQ. 3)
        CALL WDialogSelect(IDD_PW_Page8)
        CALL WDialogFieldState(IDNEXT, Disabled) ! The 'Run >' button
        CALL WDialogSelect(IDD_PW_Page8b)
        CALL WDialogFieldState(IDNEXT, Disabled) ! The 'Run >' button
      ENDIF
      CALL PopActiveWindowID

      END SUBROUTINE UpdatePeaksButtonsStates
!
!*****************************************************************************
!
      SUBROUTINE CheckIfWeCanDoAPawleyRefinement

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: WeCanDoAPawleyRefinement

      CALL PushActiveWindowID
      IF (WeCanDoAPawleyRefinement()) THEN
        CALL SetModeMenuState(0,1)
        CALL WDialogSelect(IDD_PW_Page10)
        CALL WDialogFieldState(IDNEXT,Enabled)
        CALL WDialogSelect(IDD_Pawley_Status)
        CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
      ELSE
        CALL SetModeMenuState(0,-1)
        CALL WDialogSelect(IDD_PW_Page10)
        CALL WDialogFieldState(IDNEXT,Disabled)
        CALL WDialogSelect(IDD_Pawley_Status)
        CALL WDialogFieldState(IDF_PawRef_Refine,Disabled)
      ENDIF
      CALL PopActiveWindowID

      END SUBROUTINE CheckIfWeCanDoAPawleyRefinement
!
!*****************************************************************************
!
      LOGICAL FUNCTION WeCanDoAPawleyRefinement

      USE DRUID_HEADER

      IMPLICIT NONE

      REAL               PeakShapeSigma(1:2), PeakShapeGamma(1:2), PeakShapeHPSL, PeakShapeHMSL
      COMMON /PEAKFIT3/  PeakShapeSigma,      PeakShapeGamma,      PeakShapeHPSL, PeakShapeHMSL

      LOGICAL, EXTERNAL :: Check_TicMark_Data
      REAL    tPeakShapeSigma(1:2), tPeakShapeGamma(1:2), tPeakShapeHPSL, tPeakShapeHMSL

      WeCanDoAPawleyRefinement = .FALSE.
      IF (.NOT. Check_TicMark_Data()) RETURN
!C Try to get the peak shape parameters from the View Pawley dialogue. If this fails, use the
!C values in memory.
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_ViewPawley)
      CALL WDialogGetReal(IDF_Sigma1, tPeakShapeSigma(1))
      CALL WDialogGetReal(IDF_Sigma2, tPeakShapeSigma(2))
      CALL WDialogGetReal(IDF_Gamma1, tPeakShapeGamma(1))
      CALL WDialogGetReal(IDF_Gamma2, tPeakShapeGamma(2))
      CALL WDialogGetReal(IDF_HPSL, tPeakShapeHPSL)
      CALL WDialogGetReal(IDF_HMSL, tPeakShapeHMSL)
      IF ((tPeakShapeSigma(1) .GT. -100.0)  .AND. (tPeakShapeSigma(1) .LT. 100.0) .AND.   &
          (tPeakShapeSigma(2) .GT. -100.0)  .AND. (tPeakShapeSigma(2) .LT. 100.0) .AND.   &
          (tPeakShapeGamma(1) .GT. -100.0)  .AND. (tPeakShapeGamma(1) .LT. 100.0) .AND.   &
          (tPeakShapeGamma(2) .GT. -100.0)  .AND. (tPeakShapeGamma(2) .LT. 100.0) .AND.   &
          (tPeakShapeHPSL     .GT. -100.0)  .AND. (tPeakShapeHPSL     .LT. 100.0) .AND.   &
          (tPeakShapeHMSL     .GT. -100.0)  .AND. (tPeakShapeHMSL     .LT. 100.0)) THEN
        PeakShapeSigma(1) = tPeakShapeSigma(1)
        PeakShapeSigma(2) = tPeakShapeSigma(2)
        PeakShapeGamma(1) = tPeakShapeGamma(1)
        PeakShapeGamma(2) = tPeakShapeGamma(2)
        PeakShapeHPSL     = tPeakShapeHPSL
        PeakShapeHMSL     = tPeakShapeHMSL
      ENDIF
      CALL PopActiveWindowID
      IF (PeakShapeSigma(1) .LT. -900.0) RETURN
      IF (PeakShapeSigma(2) .LT. -900.0) RETURN
      IF (PeakShapeGamma(1) .LT. -900.0) RETURN
      IF (PeakShapeGamma(2) .LT. -900.0) RETURN
      IF (PeakShapeHPSL     .LT. -900.0) RETURN
      IF (PeakShapeHMSL     .LT. -900.0) RETURN
      WeCanDoAPawleyRefinement = .TRUE.

      END FUNCTION WeCanDoAPawleyRefinement
!
!*****************************************************************************
!
      SUBROUTINE Create_DicvolIndexFile
     
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER           NTPeak
      REAL              AllPkPosVal,         AllPkPosEsd
      REAL              AllPkAreaVal
      REAL              PkProb
      INTEGER           IOrdTem
      INTEGER           IHPk
      COMMON /ALLPEAKS/ NTPeak,                                                  &
                        AllPkPosVal(MTPeak), AllPkPosEsd(MTPeak),                &
                        AllPkAreaVal(MTPeak),                                    &
                        PkProb(MTPeak),                                          &
                        IOrdTem(MTPeak),                                         &
                        IHPk(3,MTPeak)

      INTEGER IFTYPE
      INTEGER iFlags
      CHARACTER(LEN=75) :: FILTER
      REAL    Rvpar(2), Rcpar(5), Lambda, Rdens, Rmolwt, tFoM, Rexpzp, Reps
      INTEGER iSystem(6), UseErr, I, iOrd, hFile
      REAL    Epsilon
      CHARACTER(MaxPathLength) tFileName

! Get a file name

! Extract the information from the dialog
      iFlags = SaveDialog  + PromptOn + AppendExt
      FILTER = 'All files (*.*)|*.*|DICVOL files (*.dat)|*.dat|'
      tFileName = ' '
      IFTYPE = 2
      hFile = 117
      CALL WSelectFile(FILTER, iFlags, tFileName, 'Enter DICVOL file name', IFTYPE)
      IF ((LEN_TRIM(tFileName) .EQ. 0) .OR. (WInfoDialog(ExitButtonCommon) .NE. CommonOK)) RETURN
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Index_Preparation)
      CALL WDialogGetReal(IDF_wavelength1, Lambda)
      CALL WDialogGetReal(IDF_Indexing_MinVol, Rvpar(1))
      CALL WDialogGetReal(IDF_Indexing_MaxVol, Rvpar(2))
      CALL WDialogGetReal(IDF_Indexing_Maxa, Rcpar(1))
      CALL WDialogGetReal(IDF_Indexing_Maxb, Rcpar(2))
      CALL WDialogGetReal(IDF_Indexing_Maxc, Rcpar(3))
      CALL WDialogGetReal(IDF_Indexing_MinAng, Rcpar(4))
      CALL WDialogGetReal(IDF_Indexing_MaxAng, Rcpar(5))
      CALL WDialogGetReal(IDF_Indexing_Density, Rdens)
      CALL WDialogGetReal(IDF_Indexing_MolWt,   Rmolwt)
      CALL WDialogGetReal(IDF_Indexing_Fom,     tFoM)
      CALL WDialogGetReal(IDF_ZeroPoint,        Rexpzp)
      CALL WDialogGetCheckBox(IDF_Indexing_Cubic,      iSystem(1))
      CALL WDialogGetCheckBox(IDF_Indexing_Tetra,      iSystem(2))
      CALL WDialogGetCheckBox(IDF_Indexing_Hexa,       iSystem(3))
      CALL WDialogGetCheckBox(IDF_Indexing_Ortho,      iSystem(4))
      CALL WDialogGetCheckBox(IDF_Indexing_Monoclinic, iSystem(5))
      CALL WDialogGetCheckBox(IDF_Indexing_Triclinic,  iSystem(6))
      CALL WDialogGetRadioButton(IDF_Indexing_UseErrors,  UseErr)
      CALL WDialogGetReal(IDF_eps,Epsilon)
! Write it out 
      OPEN(UNIT=hFile, FILE=tFileName, STATUS='UNKNOWN', ERR=999)
      WRITE(hFile,*,ERR=999) 'DICVOL input file created by DASH'
      WRITE(hFile,'(8(I3,1X))',ERR=999)  NTPeak, 2, (iSystem(i),i=1,6)
      WRITE(hFile,'(7(F8.2,1X))',ERR=999)  Rcpar(1),Rcpar(2),Rcpar(3),Rvpar(1),Rvpar(2),Rcpar(4),Rcpar(5)
      WRITE(hFile,'(F10.6,1X,3(F8.4,1X))',ERR=999)  Lambda, Rmolwt, Rdens, Rdens/50.0
      IF (UseErr .EQ. 2) THEN
        Reps = 1.0
      ELSE
        Reps = Epsilon
      ENDIF
      WRITE(hFile,'(F5.3,1X,F6.2,1X,F9.6)',ERR=999) Reps, tFoM, Rexpzp
      IF (UseErr .EQ. 2) THEN
        DO I = 1, NTPeak
          iOrd = iOrdTem(i)
          WRITE(hFile,'(F12.4,1X,F12.4)',ERR=999) AllPkPosVal(iOrd), AllPkPosEsd(iOrd)*10.0
        ENDDO
      ELSE
        DO I = 1, NTPeak
          iOrd = iOrdTem(i)
          WRITE(hFile,'(F12.4)',ERR=999) AllPkPosVal(iOrd)
        ENDDO
      ENDIF        
      CLOSE(hFile)
      CALL PopActiveWindowID
      RETURN
 999  CALL ErrorMessage("Error writing DICVOL input file.")
      CLOSE(hFile)
      CALL PopActiveWindowID

      END SUBROUTINE Create_DicvolIndexFile
!
!*****************************************************************************
!
