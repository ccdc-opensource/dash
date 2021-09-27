! This file is part of DASH.
! SPDX-Identifier: MIT
!
! Copyright 2001 Science and Technology Facilities Council
! Copyright 2001 Cambridge Crystallographic Data Centre
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
!*****************************************************************************
!
      SUBROUTINE OpenChiSqPlotWindow

      USE DRUID_HEADER
      USE WINTERACTER

      IMPLICIT NONE

      INCLUDE 'params.inc'

      INTEGER                    ChiHandle
      COMMON /ChiSqdWindowsUsed/ ChiHandle

      REAL                    chi_sqd
      INTEGER                                           Curr_Iter, MaxIterationSoFar
      REAL                    chi_x_max, chi_x_min, chi_y_min, chi_y_max
      LOGICAL                                                             Zoomed
      INTEGER                 RunStart
      COMMON /CHISQDPLOTDATA/ chi_sqd(MaxIter, MaxRun), Curr_Iter, MaxIterationSoFar, &
                              chi_x_max, chi_x_min, chi_y_min, chi_y_max, Zoomed, &
                              RunStart

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER         nmpert, bmIHANDLE
      COMMON /sagdat/ nmpert, bmIHANDLE

! Variables used to specify Child Window position
      INTEGER                 Ix, Iy
      COMMON /WindowPosition/ Ix, Iy
      DATA Ix /10/, Iy /450/

      LOGICAL           Resume_SA
      COMMON /RESUMESA/ Resume_SA

      EXTERNAL DealWithChiSqdPlot

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( in_batch ) &
        RETURN
      IF (ChiHandle .NE. -1) THEN
        CALL DebugErrorMessage('OpenChiSqPlotWindow() while ChiHandle .NE. -1')
        RETURN
      ENDIF
      CALL WindowOpenChild(ChiHandle,SysMenuOn+MinButton+OwnedByRoot, x=Ix, y=Iy, width=400, height=300, title='SA Run Progress')
      CALL RegisterChildWindow(Chihandle, DealWithChiSqdPlot)
      Zoomed = .FALSE.
      chi_x_min = FLOAT(nmpert)/100000.0
      chi_y_min = 0.0
      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SA_Action1)
      CALL WDialogFieldState(IDB_Prog3, Disabled)
      CALL SelectDASHDialog(IDD_SAW_Page5)
      CALL WDialogFieldState(IDB_Prog3, Disabled)
      CALL PopActiveWindowID
      IF (Resume_SA) THEN
        RunStart = NumOf_SA_Runs
      ELSE
        RunStart = 0
      ENDIF
      CALL plotting_Chi_sqd

      END SUBROUTINE OpenChiSqPlotWindow
!
!*****************************************************************************
!
      SUBROUTINE ChiSqPlot_EndOfSARun
!
! At the end of an SA run, its remaining values should be filled with the current last value
! and the Chi sqrd plot should be redrawn in red

      IMPLICIT NONE

      INCLUDE 'params.inc'

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      REAL                    chi_sqd
      INTEGER                                           Curr_Iter, MaxIterationSoFar
      REAL                    chi_x_max, chi_x_min, chi_y_min, chi_y_max
      LOGICAL                                                             Zoomed
      INTEGER                 RunStart
      COMMON /CHISQDPLOTDATA/ chi_sqd(MaxIter, MaxRun), Curr_Iter, MaxIterationSoFar, &
                              chi_x_max, chi_x_min, chi_y_min, chi_y_max, Zoomed, &
                              RunStart

      INTEGER J

! Fill rest of array with current Chi-sqd Value
      DO J = Curr_Iter+1, MaxIter
        Chi_sqd(J, Curr_SA_Run) = Chi_sqd(Curr_Iter, Curr_SA_Run)
      ENDDO
      Curr_Iter = 0
      ! Current chi squared plot should be redrawn in red
      CALL plotting_Chi_sqd

      END SUBROUTINE ChiSqPlot_EndOfSARun
!
!*****************************************************************************
!
      SUBROUTINE ChiSqPlot_UpdateIterAndChiProBest(TheIter)
!
!  Graph plotting code generated by GraphEd at 09:32 on 01 Aug 2001.
!
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheIter
!
!  Definitions and array declarations.
!
      INCLUDE 'params.inc'

      REAL             CHIPROBEST
      COMMON /PLTSTO2/ CHIPROBEST

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      REAL                    chi_sqd
      INTEGER                                           Curr_Iter, MaxIterationSoFar
      REAL                    chi_x_max, chi_x_min, chi_y_min, chi_y_max
      LOGICAL                                                             Zoomed
      INTEGER                 RunStart
      COMMON /CHISQDPLOTDATA/ chi_sqd(MaxIter, MaxRun), Curr_Iter, MaxIterationSoFar, &
                              chi_x_max, chi_x_min, chi_y_min, chi_y_max, Zoomed, &
                              RunStart

      LOGICAL           Is_SX
      COMMON  / SXCOM / Is_SX

      INTEGER         nmpert, bmIHANDLE
      COMMON /sagdat/ nmpert, bmIHANDLE

      REAL              XOPT,       C,       FOPT
      COMMON / sacmn /  XOPT(MVAR), C(MVAR), FOPT

      REAL Curr_CHI

      Curr_Iter = TheIter
      IF (Is_SX) THEN
        Curr_CHI = FOPT
      ELSE
        Curr_CHI = CHIPROBEST
      ENDIF
      IF (Curr_Iter .LE. 0) RETURN
! chi_sqd is initialised to all zeros in BeginSa()
      IF (Curr_Iter .GT. MaxIterationSoFar) THEN
        MaxIterationSoFar = Curr_Iter
        IF (.NOT. Zoomed) chi_x_max = (MaxIterationSoFar * FLOAT(nmpert))/100000.0
      ENDIF
! Record chi-sqd value in array.
! Ensure that profile chi-sqd value never increases....
      chi_sqd(Curr_Iter, Curr_SA_Run) = Curr_CHI
      IF (Curr_Iter .GT. 1) THEN
        IF (Curr_CHI .GT. chi_sqd(Curr_Iter-1, Curr_SA_Run)) THEN
          chi_sqd(Curr_Iter, Curr_SA_Run) = Chi_sqd(Curr_Iter-1, Curr_SA_Run)
        ENDIF
      ENDIF
      IF ((Curr_Iter .EQ. 2) .AND. (.NOT. Zoomed)) chi_y_max = chi_sqd(1, Curr_SA_Run)*1.25
      CALL plotting_chi_sqd

      END SUBROUTINE ChiSqPlot_UpdateIterAndChiProBest
!
!*****************************************************************************
!
      SUBROUTINE plotting_Chi_sqd

      USE WINTERACTER

      IMPLICIT NONE

      INCLUDE 'params.inc'
      INCLUDE 'Poly_Colours.inc'

      REAL                    chi_sqd
      INTEGER                                           Curr_Iter, MaxIterationSoFar
      REAL                    chi_x_max, chi_x_min, chi_y_min, chi_y_max
      LOGICAL                                                             Zoomed
      INTEGER                 RunStart
      COMMON /CHISQDPLOTDATA/ chi_sqd(MaxIter, MaxRun), Curr_Iter, MaxIterationSoFar, &
                              chi_x_max, chi_x_min, chi_y_min, chi_y_max, Zoomed, &
                              RunStart

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult
! Required for Pawley Chi Sq value
      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      INTEGER         nmpert, bmIHANDLE
      COMMON /sagdat/ nmpert, bmIHANDLE

      INTEGER                    ChiHandle
      COMMON /ChiSqdWindowsUsed/ ChiHandle

      INTEGER J, ISET
      REAL   Xarray(MaxIter+1)
      REAL, DIMENSION (2) :: EndPoint, XEndPoint

      IF (ChiHandle .EQ. -1) RETURN
      CALL WindowSelect(ChiHandle)
      CALL WindowClear
! MaxIterationSoFar is initialised to zero in BeginSa
      IF (MaxIterationSoFar .LT. 2) RETURN
! calculate array of x values for graph
      DO j = 1, MaxIterationSoFar
        Xarray(j) = (FLOAT(j * nmpert) / 100000)
      ENDDO

!  If zoomed, values for x_min etc taken from ChiSqdPlotData Common Block - max and min
!  values determined by mouse.
!
!  Start new presentation graphics plot
!
!  Set Clipping Rectangle
!
      CALL IPgClipRectangle('P')
!
!  Set style for each data set
!
      CALL IPgStyle(  1,  0,  0,  0,  KolNumobs)
!
!  Set marker number for data sets not using default marker
!
!
!  Set units for plot
!
      CALL IGrArea(0.0,0.0,1.0,1.0)
      CALL IGrUnits(0.0,0.0,1.0,1.0)
!
!  Set presentation graphics area
!
      CALL IPgArea(0.2, 0.3, 0.9, 0.9)
      CALL IPgUnits(  chi_x_min,      chi_y_min,   &
                      chi_x_max,      chi_y_max)
!
!  Draw main title
!
      CALL IGrCharSet('H')
      CALL IGrCharFont(3)
      CALL IGrCharSpacing('P')
      CALL IGrCharSize( 2.0, 1.5)
      CALL IGrColourN(KolNumMain)
!!      CALL IPgTitle('Profile Chi-squared vs. Number of Moves','C')
!
!  Label bottom X axis
!
      CALL IPgXLabelPos(  0.70)
      CALL IPgXLabel('Number of Moves / 100000','C')
!
!  Label left Y axis
!
      CALL IPgYLabelPos(  0.70)
      CALL IPgYLabelLeft('Profile Chi-squared','C9')
!
!  Draw axes
!
      CALL IGrColourN(KolNumMain)
      CALL IPgAxes
      CALL IPgBorder
!
!  Adjust tick position for X Axes
!
      CALL IPgXTickPos(     1.0000    ,    -1.0000    )
!
!  Scale for bottom X Axis
!
      CALL IPgXTickLength(  1.00)
      CALL IPgDecimalPlaces(      -1)
      CALL IPgXUserScale((/0.0/),0)
      CALL IPgXScaleAngle(  0.00,  -90.00)
      CALL IPgXScalePos(  0.1)
      CALL IPgXScale('NT')
!
!  Adjust tick position for Y Axes
!
      CALL IPgYTickPos(     1.0000    ,    -1.0000    )
!
!  Scale for left Y Axis
!
      CALL IPgYTickLength(  1.00)
      CALL IPgDecimalPlaces(      -1)
      CALL IPgYUserScale((/0.0/),0)
      CALL IPgYScaleAngle(  0.00,  0.00)
      CALL IPgYScalePos(  1.50)
      CALL IPgYScaleLeft('NT')
!
!  Draw graph.
!
! Plot Chi-sqd values to maximum number of moves for completed SA runs
      IF ((NumOf_SA_Runs-RunStart) .GT. 0) THEN
        CALL IPgNewPlot(PgPolyLine,NumOf_SA_Runs-RunStart,MaxIterationSoFar,0,1)
        DO j = 1, NumOf_SA_Runs-RunStart
          CALL IPgStyle(  j,  0,  0,  0,  KolNumObs)
        ENDDO
        DO ISET = RunStart+1, NumOf_SA_Runs
          CALL IPgXYPairs(Xarray, Chi_sqd(1,ISET))
        ENDDO
      ENDIF
! Plot Chi-sqd values for current run iteration by iteration
      IF (Curr_Iter .GT. 1) THEN
        CALL IPgNewPlot(PgPolyLine,1,Curr_Iter,0,1)
        CALL IPgStyle(  1,  0,  0,  0,  KolNumCal)
        CALL IPgXYPairs(Xarray, chi_sqd(1,Curr_SA_Run))
      ENDIF
! Plot line which indicates endpoint
      XEndPoint(1) = chi_x_min
      XEndPoint(2) = chi_x_max
! Y Values for endpoint line
      DO j = 1,2
        EndPoint(j) = PAWLEYCHISQ * ChiMult
      ENDDO
      CALL IPgNewPlot(PgPolyLine,1,2,0,1)
      CALL IPgStyle(  1,  0,  0,  0,  KolNumDif)
      CALL IPgXYPairs(XEndPoint, EndPoint)

      END SUBROUTINE plotting_chi_sqd
!
!*****************************************************************************
!
      SUBROUTINE DealWithChiSqdPlot

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'params.inc'

      INTEGER                    ChiHandle
      COMMON /ChiSqdWindowsUsed/ ChiHandle

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      REAL                    chi_sqd
      INTEGER                                           Curr_Iter, MaxIterationSoFar
      REAL                    chi_x_max, chi_x_min, chi_y_min, chi_y_max
      LOGICAL                                                             Zoomed
      INTEGER                 RunStart
      COMMON /CHISQDPLOTDATA/ chi_sqd(MaxIter, MaxRun), Curr_Iter, MaxIterationSoFar, &
                              chi_x_max, chi_x_min, chi_y_min, chi_y_max, Zoomed, &
                              RunStart

      INTEGER         nmpert, bmIHANDLE
      COMMON /sagdat/ nmpert, bmIHANDLE

      SELECT CASE (EventType)
! will close the profile plot window
        CASE (CloseRequest)
          CALL Close_Chisq_Plot
! exposing or resizing of profile plot windows 
        CASE (Expose, Resize)
          CALL Plotting_chi_sqd
        CASE (MouseButDown)
          IF (EventInfo%VALUE1 .EQ. LeftButton) THEN
            CALL Plotting_Chi_Sqd
            CALL ZoomChiSqdPlot
          ENDIF
        CASE (KeyDown) ! home key resets the plot to original axes
           IF (EventInfo%VALUE1 .EQ. KeyHome) THEN 
             Zoomed = .FALSE.   
             chi_x_min = FLOAT(nmpert)/100000.0
             chi_x_max = (MaxIterationSoFar * FLOAT(nmpert))/100000.0
             chi_y_min = 0.0
             chi_y_max = chi_sqd(1, Curr_SA_Run)*1.25
             CALL plotting_Chi_Sqd
           ENDIF
      END SELECT

      END SUBROUTINE DealWithChiSqdPlot
!
!*****************************************************************************
!
      SUBROUTINE Close_Chisq_Plot

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER                    ChiHandle
      COMMON /ChiSqdWindowsUsed/ ChiHandle

      INTEGER Ix, Iy
      COMMON /WindowPosition/ Ix, Iy

      IF (ChiHandle .EQ. -1) RETURN
! save position of window before close.
      CALL WindowSelect(ChiHandle)
! JvdS If the window has been minimised, this turns out to return negative values
      IF (WinfoWindow(WindowXpos) .GE. 0) THEN
        Ix = WinfoWindow(WindowXpos)
        Iy = WinfoWindow(WindowYpos)
      ENDIF
      CALL WindowCloseChild(ChiHandle)
      CALL UnRegisterChildWindow(ChiHandle)
      ChiHandle = -1
      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SA_Action1)
      CALL WDialogFieldState(IDB_Prog3,Enabled)
      CALL SelectDASHDialog(IDD_SAW_Page5)
      CALL WDialogFieldState(IDB_Prog3,Enabled)
      CALL PopActiveWindowID

      END SUBROUTINE Close_Chisq_Plot
!
!***************************************************************************************

      SUBROUTINE ZoomChiSqdPlot
!
!  Enable button up and mouse movement events
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'params.inc'
      INCLUDE 'Poly_Colours.inc'

      REAL                    chi_sqd
      INTEGER                                           Curr_Iter, MaxIterationSoFar
      REAL                    chi_x_max, chi_x_min, chi_y_min, chi_y_max
      LOGICAL                                                             Zoomed
      INTEGER                 RunStart
      COMMON /CHISQDPLOTDATA/ chi_sqd(MaxIter, MaxRun), Curr_Iter, MaxIterationSoFar, &
                              chi_x_max, chi_x_min, chi_y_min, chi_y_max, Zoomed, &
                              RunStart

      INTEGER                    ChiHandle
      COMMON /ChiSqdWindowsUsed/ ChiHandle

      REAL XCUR(2),YCUR(2),XGCUR(2),YGCUR(2)
      REAL xgcurold, ygcurold

      CALL WindowSelect(ChiHandle)
      CALL WMessageEnable(MouseButUp, Enabled)
! Set the scale correctly. 
      CALL IGrUnits(0.0, 0.0, 1.0, 1.0)
      CALL IPgArea(0.2, 0.3, 0.9, 0.9)
      CALL IPgUnits(chi_x_min, chi_y_min, chi_x_max, chi_y_max)
      xgcur(1) = EventInfo%GX
      ygcur(1) = EventInfo%GY
      CALL IPgUnitsFromGrUnits(xgcur(1),ygcur(1),xcur(1),ycur(1))
      xgcurold = xgcur(1)
      ygcurold = ygcur(1)
      DO WHILE (.TRUE.)
!Can't use PeekEvent since the following events aren't handled for ChildWindows
         CALL WMessagePeek(EventType, EventInfo)
         IF (EventType .NE. (-1)) THEN
!           IF (EventInfo%WIN .GT. 0) THEN
           CALL WindowSelect(ChiHandle)
           CALL IGrUnits(0.0,0.0,1.0,1.0)
           CALL IPgArea(0.2,0.3,0.9,0.9)
           CALL IPgUnits(chi_x_min, chi_y_min, chi_x_max, chi_y_max)
           CALL IPgUnitsFromGrUnits(EventInfo%GX,EventInfo%GY,xcur(2),ycur(2))
           SELECT CASE (EventType)
            CASE (MouseMove)
              xgcur(2) = EventInfo%GX
              ygcur(2) = EventInfo%GY
              CALL IGrPlotMode('EOR')
              CALL IGrColourN(KolNumRectSelect)
              CALL IGrFillPattern(0,1,1)
              ! Remove old
              CALL IGrRectangle(xgcur(1),ygcur(1),xgcurold,ygcurold)
              ! Draw new
              CALL IGrRectangle(xgcur(1),ygcur(1),xgcur(2),ygcur(2))
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
                CALL IGrFillPattern(0,1,1)
                ! Remove old
                CALL IGrRectangle(xgcur(1),ygcur(1),xgcurold,ygcurold)
                CALL IGrPlotMode('Normal')
                CALL IGrColourN(InfoGrScreen(PrevColReq))
                IF (ABS(XCUR(2)-XCUR(1)).LT.0.006*(chi_x_max - chi_x_min)) RETURN
                IF (ABS(YCUR(2)-YCUR(1)).LT.0.006*(chi_y_max - chi_y_min)) RETURN
                chi_x_min = MIN(XCUR(1),XCUR(2))
                chi_x_max = MAX(XCUR(1),XCUR(2))  
                chi_y_min = MIN(YCUR(1),YCUR(2))
                chi_y_max = MAX(YCUR(1),YCUR(2))
              ENDIF
              CALL WindowClear
              Zoomed = .TRUE.
              CALL Plotting_Chi_sqd
              RETURN  
          END SELECT
        ENDIF
!        ENDIF
      ENDDO

      END SUBROUTINE ZoomChiSqdPLot
!
!************************************************************************************************
!
      SUBROUTINE OutputChi2vsMoves

      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'params.inc'

      REAL                    chi_sqd
      INTEGER                                           Curr_Iter, MaxIterationSoFar
      REAL                    chi_x_max, chi_x_min, chi_y_min, chi_y_max
      LOGICAL                                                             Zoomed
      INTEGER                 RunStart
      COMMON /CHISQDPLOTDATA/ chi_sqd(MaxIter, MaxRun), Curr_Iter, MaxIterationSoFar, &
                              chi_x_max, chi_x_min, chi_y_min, chi_y_max, Zoomed, &
                              RunStart

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER                  OFBN_Len
      CHARACTER(MaxPathLength)           OutputFilesBaseName
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OFBN_Len, OutputFilesBaseName, SA_RunNumberStr

      INTEGER         nmpert, bmIHANDLE
      COMMON /sagdat/ nmpert, bmIHANDLE

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      CHARACTER*20, EXTERNAL :: GetSeed1SuffixString
      LOGICAL, EXTERNAL :: Get_OutputChi2vsMoves
      INTEGER hFile, I, J
      CHARACTER*20 tString

      IF (.NOT. Get_OutputChi2vsMoves()) RETURN
      hFile = 10
      tString = ''
      IF (in_batch) tString = GetSeed1SuffixString()
      OPEN(UNIT=hFile,FILE=OutputFilesBaseName(1:OFBN_Len)//TRIM(tString)//'.chi',ERR=999)
      DO I = 1, MaxIterationSoFar
        WRITE(hFile,'(I10,999(1X,F9.2))',ERR=999) I*nmpert,(chi_sqd(I,J),J=1,NumOf_SA_Runs) ! NumOf_SA_Runs = last completed SA run
      ENDDO
      CLOSE(hFile)
      RETURN
  999 CALL ErrorMessage('Could not access profile chi-squared versus moves file.')
      CLOSE(hFile)

      END SUBROUTINE OutputChi2vsMoves
!
!*****************************************************************************
!
