!
!*****************************************************************************
!
      SUBROUTINE Profile_Plot(TheIPTYPE)
!
! JvdS There is only one place in the DASH code where this function is NOT
! called with 'IPTYPE' as argument: and that's when it's called with '-IPTYPE'
! to produce hard copy.
!
      USE WINTERACTER
      USE VARIABLES

      INTEGER, INTENT (IN   ) :: TheIPTYPE

      INCLUDE 'POLY_COLOURS.INC'
      INCLUDE 'PARAMS.INC'

      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
        itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR), &
        NumPeakFitRange,CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), &
        XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), &
        IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT) 

      LOGICAL PlotBackground ! Function

! IGrArea
! IPgArea
! IPgClipRectangle

!   Setup hardcopy options
      IF (TheIPTYPE .LT. 0) THEN
        CALL IGrInit('HP')
        CALL IGrHardCopyOptions(1,700)
        CALL IGrHardCopyOptions(2,375)
        CALL IGrHardCopy(' ')
      END IF
      CALL Plot_Initialise()
      CALL Plot_Panel()
      CALL Plot_Custom_Axes()
! Plot peakfit ranges
      IF (NumPeakFitRange .GT. 0) CALL Plot_PeakFit_Info()
! Observed profile
      SELECT CASE (ABS(TheIPTYPE))
        CASE (1) 
          CALL Plot_Observed_Profile()
        CASE (2) 
          CALL Plot_ObsCalc_Profile()
      END SELECT
      IF (PlotBackground()) CALL Plot_Background()
!  Plot tic marks etc. if appropriate
      IF (Ntic .NE. 0) CALL Plot_Calculated_Tics()
      IF (NumObsTic .NE. 0) CALL Plot_Manual_Tics()
      CALL IPgBorder()
      PLOTT = .TRUE.
!   Switch off hardcopy
      IF (TheIPTYPE .LT. 0) THEN
        CALL IGrHardCopy('S')
        CALL IGrInit('P')
      END IF
      RETURN

      END SUBROUTINE Profile_Plot
!
!*****************************************************************************
!
      SUBROUTINE Plot_Manual_Tics
!
! Plot the manual tickmarks
!
      USE WINTERACTER

      INCLUDE 'POLY_COLOURS.INC'
      INCLUDE 'PARAMS.INC'

      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
        YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
        XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
        itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)
!
!      CALL IPgUnitsToGrUnits(xpgmin,ypgmin,xggmin,yggmin)
!      CALL IPgUnitsToGrUnits(xpgmax,ypgmax,xggmax,yggmax)
      CALL IGrColourN(KolNumMTic)
      zero = 0.0
      DO ii = 1, numobstic
        y0tem = MIN(ypgmax,zero)
        y0tem = MAX(y0tem,ypgmin)
        ytem  = MIN(ypgmax,yobstic(ii))
        ytem  = MAX(ytem,ypgmin)
        CALL IPgUnitsToGrUnits(xobstic(ii),y0tem,x0gtem,y0gtem)
        CALL IPgUnitsToGrUnits(xobstic(ii),ytem,xgtem,ygtem)
        IF (itypot(ii) .EQ. 0) THEN
          IF (xgtem .GE. xggmin .AND. xgtem .LE. xggmax) THEN
            CALL IGrColourN(KolNumPeakPos)
            CALL IGrMoveTo(x0gtem,y0gtem)
            CALL IGrLineTo(xgtem,ygtem)
            CALL IGrColourN(KolNumMTic)
            CALL IGrCharSize(1.0,1.0)
            CALL IGrMarker(xgtem,ygtem,5)
          END IF
        ELSE IF (itypot(ii) .EQ. 1) THEN
          IF (xgtem .GE. xggmin .AND. xgtem .LE. xggmax) THEN
            CALL IGrMarker(xgtem,ygtem,4)
            CALL IPgUnitsToGrUnits(uobstic(1,ii),zobstic(1,ii),ugtem,zgtem)
            CALL IGrMoveTo(ugtem,zgtem)
            DO k = 2, 9
              CALL IPgUnitsToGrUnits(uobstic(k,ii),zobstic(k,ii),ugtem,zgtem)
              CALL IGrLineTo(ugtem,zgtem)
            END DO
          END IF            
        END IF
      END DO
      CALL IGrColourN(KolNumMain)

      END SUBROUTINE Plot_Manual_Tics
!
!*****************************************************************************
!
      SUBROUTINE Plot_Calculated_Tics

      USE WINTERACTER

      INCLUDE 'POLY_COLOURS.INC'
      INCLUDE 'PARAMS.INC'

      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
        YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
        XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      CALL IGrColourN(KolNumCTic)
      YPGDIF = (YPGMAX-YPGMIN)
      YTIC1 = YPGMIN + 0.94 * YPGDIF
      YTIC2 = YPGMIN + 0.97 * YPGDIF
      DO II = 1, NTIC
        IF (ARGK(II) .GE. XPGMIN .AND. ARGK(II) .LE. XPGMAX) THEN
          CALL IPgUnitsToGrUnits(ARGK(II),ytic1,xgtem,ygtem)
          CALL IGrMoveTo(xgtem,ygtem)
          CALL IPgUnitsToGrUnits(ARGK(II),ytic2,xgtem,ygtem)
          CALL IGrLineTo(xgtem,ygtem)
        END IF
      END DO
      CALL IGrColourN(KolNumMain)

      END SUBROUTINE Plot_Calculated_Tics
!
!*****************************************************************************
!
      SUBROUTINE Plot_Panel()

      USE WINTERACTER

      INCLUDE 'POLY_COLOURS.INC'

      COMMON /PLTINI/ XPG1,XPG2,YPG1,YPG2

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
      SUBROUTINE Plot_Initialise()

      USE WINTERACTER

      INCLUDE 'POLY_COLOURS.INC'
      INCLUDE 'GLBVAR.INC'

      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
        YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
        XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
      COMMON /PLTINI/ XPG1,XPG2,YPG1,YPG2

      CALL IGrSelect(1,0)
      CALL IGrArea(0.0,0.0,1.0,1.0)
      CALL IGrUnits(0.,0.,1.,1.)
      CALL IGrAreaClear
      CALL IGrCharSpacing('P')
      CALL IGrCharFont(3)
      CALL IGrColourN(KolNumMain)
      CALL IPgScaling('LIN','LIN')
! Draw axes and add scales
      CALL IPgArea(XPG1,YPG1,XPG2,YPG2)
      CALL IPgUnits(xpgmin,ypgmin,xpgmax,ypgmax)
      IF ((xpgmax-xpgmin) .LE. 200.0) THEN
        CALL IRealToString(xpgmin,statbarstr(4)(1:),'(f10.3)')
        CALL IRealToString(xpgmax,statbarstr(5)(1:),'(f10.3)')
      ELSE
        CALL IRealToString(xpgmin,statbarstr(4)(1:),'(f10.1)')
        CALL IRealToString(xpgmax,statbarstr(5)(1:),'(f10.1)')
      END IF
      IF ((ypgmax-ypgmin) .LE. 100.0) THEN      
        CALL IRealToString(ypgmin,statbarstr(6)(1:),'(f10.3)')
        CALL IRealToString(ypgmax,statbarstr(7)(1:),'(f10.3)')
      ELSE
        CALL IRealToString(ypgmin,statbarstr(6)(1:),'(f10.1)')
        CALL IRealToString(ypgmax,statbarstr(7)(1:),'(f10.1)')
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

      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
        YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
        XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      CALL IGrColourN(KolNumMain)
      CALL IGrCharSize(1.,1.)
      CALL IPgXScale('TN')
      CALL IPgXScaleTop('T')
      CALL IGrColourN(KolNumMain)
      CALL IPgUnitsToGrUnits(xpgmin,ypgmin,xggmin,yggmin)
      CALL IPgUnitsToGrUnits(xpgmax,ypgmax,xggmax,yggmax)
! Plot y=0 for tidiness
      zero=0.0
      IF (ypgmin.lt.zero .and. ypgmax.gt.zero) THEN
        CALL IPgUnitsToGrUnits(xpgmin,zero,xggt,yggt)
        CALL IGrMoveTo(xggt,yggt)
        CALL IPgUnitsToGrUnits(xpgmax,zero,xggt,yggt)
        CALL IGrLineTo(xggt,yggt)
      END IF
      CALL IGrCharSize(1.,1.2)
      CALL IPgYScaleLeft('TN')
      CALL IPgYScaleRight('T')
      CALL IGrCharSize(1.,1.2)
      IF (XPGMAX .LT. 200.0) THEN
        CALL IPgXLabel('2 theta','C')
      ELSE
        CALL IPgXLabel('time-of-flight (microseconds)','C')
      END IF

      ENDSUBROUTINE Plot_Custom_Axes
!
!*****************************************************************************
!
      SUBROUTINE Plot_Observed_Profile()

      USE WINTERACTER

      INCLUDE 'POLY_COLOURS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'PARAMS.INC'

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
        YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
        XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      LOGICAL PlotErrorBars ! Function

      CALL IGrColourN(KolNumMain)
      CALL IPgYLabelLeft('Observed profile','C9')
! Do the error bars - we've precalculated the min & max pointers
      CALL IGrColourN(KolNumObs)
      IF ( PlotErrorBars() ) THEN
        DO I = IPMIN, IPMAX
          xtem = xbin(i)
          ytem = MAX(yobin(i)-ebin(i),ypgmin)
          ytem = MIN(ytem,ypgmax)
          CALL IPgUnitsToGrUnits(xtem,ytem,xgtem,ygtem)
          CALL IGrMoveTo(xgtem,ygtem)
          ytem = MIN(yobin(i)+ebin(i),ypgmax)
          ytem = MAX(ytem,ypgmin)
          CALL IPgUnitsToGrUnits(xtem,ytem,xgtem,ygtem)
          CALL IGrLineTo(xgtem,ygtem)
        END DO
      END IF
!      CALL IPgNewGraph(1,nbin,' ',' ','XY')
! Must allow options here ...      CALL IPgStyle(1,0,3,0,KolNumCal,KolNumObs)
      CALL IPgNewPlot(PgScatterPlot,1,NBIN)
      CALL IPgStyle(1,14,3,0,0,KolNumObs)
! 13 specifies a square
      CALL IPgMarker( 1, 13)
      sizmtem = marker_size*FLOAT(500)/FLOAT(ipmax-ipmin)
      sizmtem = MIN(marker_size,sizmtem)
      CALL IGrCharSize(sizmtem,sizmtem)
      CALL IPgScatterPlot(xbin,yobin)
!      CALL IPgXYPairs(xbin,yobin)
      CALL IGrColourN(KolNumMain)
!
      ENDSUBROUTINE Plot_Observed_Profile
!
!*****************************************************************************
!
      SUBROUTINE Plot_Background()

      USE WINTERACTER

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'POLY_COLOURS.INC'
      INCLUDE 'PARAMS.INC'

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
        YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
        XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      CALL IPgNewPlot(PgPolyLine,1,NBIN)
      CALL IPgStyle(1,0,0,0,KolNumBack,0)
      CALL IPgXYPairs(xbin,ybbin)

      END SUBROUTINE Plot_Background
!
!*****************************************************************************
!
      SUBROUTINE Plot_ObsCalc_Profile()
!
      USE WINTERACTER

      INCLUDE 'POLY_COLOURS.INC'
      INCLUDE 'GLBVAR.INC'

      INCLUDE 'PARAMS.INC'
      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
      YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
      XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
      LOGICAL PlotErrorBars ! Function
      REAL YDIF(MOBS)

      CALL IGrColourN(KolNumMain)
      CALL IPgYLabelLeft('Observed profile','C9')
      CALL IPgNewPlot(PgPolyLine,3,NBIN)
      YADD = 0.5*(YPGMAX+YPGMIN)
! I think that the bounds of this loop are wrong: if a point is not inside the plotting area,
! it might still be needed when its neighbour is inside the plot area
      DO II = IPMIN, IPMAX
        YDIF(II) = YADD + YOBIN(II) - YCBIN(II)
      END DO
      CALL IPgStyle(1,0,0,0,KolNumDif,0)
      CALL IPgStyle(2,0,3,0,0,KolNumObs)
      CALL IPgStyle(3,0,0,0,KolNumCal,0)
! Now draw the difference plofile
      CALL IPgXYPairs(xbin,ydif)
! The following four lines set the markers for the observed profile.
      CALL IPgMarker( 2, 13)
      sizmtem = marker_size*FLOAT(500)/FLOAT(ipmax-ipmin)
      sizmtem = MIN(marker_size,sizmtem)
      CALL IGrCharSize(sizmtem,sizmtem)
! Draw the observed profile. Markers can still be used. Consecutive data points are joined 
! by lines. These can be straight or splines
      CALL IPgXYPairs(XBIN,YOBIN)
!
! Do the error bars - we've precalculated the min & max pointers
!
      CALL IGrColourN(KolNumObs)
      IF (PlotErrorBars()) THEN
      DO I = IPMIN, IPMAX
        xtem = xbin(I)
        ytem = MAX(YOBIN(I)-EBIN(I),ypgmin)
        ytem = MIN(ytem,ypgmax)
        CALL IPgUnitsToGrUnits(xtem,ytem,xgtem,ygtem)
        CALL IGrMoveTo(xgtem,ygtem)
        ytem = MIN(YOBIN(I)+EBIN(I),ypgmax)
        ytem = MAX(ytem,ypgmin)
        CALL IPgUnitsToGrUnits(xtem,ytem,xgtem,ygtem)
        CALL IGrLineTo(xgtem,ygtem)
      END DO
      END IF
      CALL IPgXYPairs(XBIN,YCBIN)
      CALL IGrCharSize(1.0,1.0)
      CALL IGrColourN(KolNumMain)

      END SUBROUTINE Plot_ObsCalc_Profile
!
!*****************************************************************************
!
      SUBROUTINE Plot_PeakFit_Info()
!
!... Plots all the information about the Peak Fitting Option
! Highlight (recolour) the peak fit range and label the peaks.
! If already fitted then show the fitted profile as well.
!
      USE WINTERACTER

      INCLUDE 'POLY_COLOURS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'PARAMS.INC'

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
        YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
        XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR), &
      NumPeakFitRange,CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), &
      XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), &
      IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)

      CHARACTER*1 ChrPkNum
      CHARACTER*2 ChrPkNum2

      iord_peak = 0
      DO i = 1, NumPeakFitRange
        CALL IGrColourN(KolNumPanelDark)
        CALL IGrFillPattern(Hatched,Medium,DiagUp)
        CALL IPgUnitsToGrUnits(XPF_Range(1,i),ypgmin,gxleft,gybot)
        CALL IPgUnitsToGrUnits(XPF_Range(2,i),ypgmax,gxright,gytop)
        gxleft  = MAX(gxleft,xggmin)
        gxleft  = MIN(gxleft,xggmax)
        gxright = MIN(gxright,xggmax)
        gxright = MAX(gxright,xggmin)
        CALL IGrRectangle(gxleft,gybot,gxright,gytop) 
        ipf1 = IPF_RPt(i) + 1
        ipf2 = IPF_RPt(i+1)
! Now we do a quick check to see if the range has been set
        IF (ipf2 .NE. 0) THEN
          xp1=XPeakFit(ipf1)
          xp2=XPeakFit(ipf2)
          IF (xp2 .GT. xpgmin .AND. xp1 .LT. xpgmax) THEN
! We can plot the calculated fit
            DO ii = ipf1, ipf2
              IF (XpeakFit(ii) .GE. xpgmin) THEN
                ix1 = ii
                GOTO 110
              END IF
            END DO
 110        DO ii = ipf2, ipf1, -1
              IF (XpeakFit(ii) .LE. xpgmax) THEN
                ix2 = ii
                GOTO 120
              END IF
            END DO
 120        CALL IPgUnitsToGrUnits(xpeakfit(ix1),ypeakfit(ix1),xgtem,ygtem)
            CALL IGrMoveTo(xgtem,ygtem)
            CALL IGrColourN(KolNumCTic)
! Software clipping - there must be a better way
!
! JCC - I've totally recoded this so that its clearer. It will be a bit slower, since it recalculates both
! points on every iteration, but it does now work!
            DO ip=ix1+1,ix2
! Get the coordinates between the 2 current points
              CALL IPgUnitsToGrUnits(xpeakfit(ip - 1),ypeakfit(ip - 1),xg_coord1,yg_coord1)
              CALL IPgUnitsToGrUnits(xpeakfit(ip),    ypeakfit(ip),    xg_coord2,yg_coord2)
! Check the y-coordinate ranges
! 1. All out of box so ignore
              IF (yg_coord1 .LT. gybot .AND. yg_coord2 .LT. gybot) CYCLE
              IF (yg_coord1 .GT. gytop .AND. yg_coord2 .GT. gytop) CYCLE
              CALL IGrMoveTo(xg_coord1, yg_coord1)
              IF (yg_coord1 .LT. gybot) THEN
! Move the start point to the intercept point in the box
                xtem = xg_coord1 + (xg_coord2 - xg_coord1)*(gybot-yg_coord1)/(yg_coord2-yg_coord1)
                CALL IGrMoveTo(xtem,gybot)
              ELSE IF (yg_coord2 .LT. gybot) THEN
! Move the end point to the intercept point in the box
                xg_coord2 = xg_coord2 + (xg_coord2 - xg_coord1)*(gybot-yg_coord1)/(yg_coord2-yg_coord1)
                yg_coord2 = gybot
              ENDIF
              IF (yg_coord1 .GT. gytop) THEN
! Move the start point to the intercept point in the box
                xtem = xg_coord1 + (xg_coord2 - xg_coord1)*(gytop-yg_coord1)/(yg_coord2-yg_coord1)
                CALL IGrMoveTo(xtem,gytop)
              ELSE IF (yg_coord2 .GT. gytop) THEN
! Move the end point to the intercept point in the box
                xg_coord2 = xg_coord2 + (xg_coord2 - xg_coord1)*(gytop-yg_coord1)/(yg_coord2-yg_coord1)
                yg_coord2 = gytop
              END IF
              CALL IGrLineTo(xg_coord2,yg_coord2)
            END DO
          END IF
        END IF
        IPresColN=InfoGrScreen(ColourReq)
        DO j = 1, NumInPFR(i)
          iord_peak = iord_peak + 1
          IF (iord_peak .LT. 10) THEN
            CALL IntegerToString(iord_peak,ChrPkNum,'(I1)')
          ELSE
            CALL IntegerToString(iord_peak,ChrPkNum2,'(I2)')
          END IF
          CALL IGrColourN(KolNumPeakFit)
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
              END IF
            END IF
          END IF   
        END DO
        CALL IGrColourN(IPresColN)
      END DO
      CALL IGrFillPattern(Outline)

      ENDSUBROUTINE Plot_PeakFit_Info
!
!*****************************************************************************
!
!C>> JCC Subroutine that cross-references the two sets of common
!C>> blocks so that the profile ones contain the data thats been
!C>> read in. Basically hacked out of SA_Profile_Plot.
      SUBROUTINE Synchronize_Data

      USE WINTERACTER

      INCLUDE 'POLY_COLOURS.INC'
      INCLUDE 'PARAMS.INC'
      INCLUDE 'statlog.inc'

      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),&
        YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),&
        YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
        YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
        XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),&
        IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),&
        NumPeakFitRange,CurrentRange,&
        IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR),&
        XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR),&
        IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)
      COMMON /ZSTORE/ NPTS,ZARGI(MPPTS),ZOBS(MPPTS),ZDOBS(MPPTS),&
        ZWT(MPPTS),ICODEZ(MPPTS),KOBZ(MPPTS)
      COMMON /YSTORE/ ZCAL(MPPTS),ZBAK(MPPTS)
      COMMON /ZSTOR1/ ZXDELT,IIMIN,IIMAX,XDIFT,XMINT
      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
        itypot(mobstic),iordot(mobstic),&
        uobstic(20,mobstic),zobstic(20,mobstic)
      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)
      COMMON /CHISTOP/ NOBSA,NFITA,IFITA(MCHSTP),CHIOBSA,&
        WTSA(MCHSTP),XOBSA(MCHSTP),YOBSA(MCHSTP),YCALA(MCHSTP),ESDA(MCHSTP)
      COMMON /chibest/ ycalbest(MCHSTP)

      LBIN = 1
      NOBS = NOBSA
      NBIN = NOBSA
      YOSUM = 0.0
      YCSUM = 0.0
      DO II = 1, NFITA
        I = IFITA(II)
        YOSUM = YOSUM + YOBSA(I)
        YCSUM = YCSUM + YCALbest(I)
      END DO
      RESCL = YOSUM / YCSUM
      DO I = 1, NOBS
        YCALbest(I) = RESCL * YCALbest(I)
      END DO
      DO I = 1, NBIN
        XBIN(I)  = XOBSA(I)
        YOBIN(I) = YOBSA(I)
        YCBIN(I) = YCALBEST(I)
        YBBIN(I) = 0.0
        EBIN(I)  = EsdA(I)
        XOBS(I)  = XOBSA(I)
        YOBS(I)  = YOBSA(I)
        YCAL(I)  = YCALBEST(I)
        YBAK(I)  = 0.0
        EOBS(I)  = EsdA(I)
      END DO
      XPMIN = XOBSA(1)
      XPMAX = XOBSA(1)
      YPMIN = YOBSA(1)
      YPMAX = YOBSA(1)
      DO I = 1, NOBS
        XPMIN = MIN(XOBSA(I),XPMIN)
        XPMAX = MAX(XOBSA(I),XPMAX)
        YPMIN = MIN(YOBSA(I),YPMIN)
        YPMAX = MAX(YOBSA(I),YPMAX)
      END DO
      XPGMIN = XPMIN
      XPGMAX = XPMAX
      YPGMIN = YPMIN
      YPGMAX = YPMAX
      CALL UPLOAD_RANGE()
      XPGMINOLD = XPMIN
      XPGMAXOLD = XPMAX
      YPGMINOLD = YPMIN
      YPGMAXOLD = YPMAX
      IPMIN = 1
      IPMAX = NBIN
      IPMINOLD = IPMIN
      IPMAXOLD = IPMAX

      END SUBROUTINE Synchronize_Data
!
!*****************************************************************************
!
