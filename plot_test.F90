!
!*****************************************************************************
!
      SUBROUTINE organise_sa_result_data(irow)
!ep July 2001
!   called from SASummary.f90
!     This subroutine manipulates the data required to plot the observed  
!   diffraction pattern with the calculated pattern and difference.  The
!   data is read in from the .bin file and stored in COMMON BLOCK ProFilePLotStore
!  ihandle is used to identify the column of the store_ arrays where the data
!  for each child window (ihandle) is stored

      USE WINTERACTER
      USE DRUID_HEADER
!
!  Definitions and array declarations.
!
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: irow

      INCLUDE 'PARAMS.INC'

      LOGICAL         RESTART
      INTEGER                  SA_Run_Number
      INTEGER                                 MaxRuns, MaxMoves
      REAL                                                       ChiMult
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MaxMoves, ChiMult

! Used to manage the child windows which display the profile plots
      INTEGER                 SAUsedChildWindows
      COMMON /SAChildWindows/ SAUsedChildWindows(MaxNumChildWin)

      REAL store_ycalc, store_diff
! the number of columns in the store-arrays is set to the maximum number of 
! child windows allowed
      COMMON /ProFilePlotStore/ store_ycalc(MOBS,MaxNumChildWin), store_diff(MOBS,MaxNumChildWin)

      LOGICAL          PRO_saved
      COMMON /PROCOM/  PRO_saved(1:MaxRun)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER iz
      REAL yadd
      REAL Ymin
      REAL Ymax
      CHARACTER*255 Grid_Buffer
      CHARACTER*75 filename
      EXTERNAL DealWithProfilePlot
      CHARACTER*2 RunStr
      INTEGER I, II, RunNr, tFileHandle, iHandle
      REAL tReal1, tReal2, tReal3
!
!   reading in the data from the saved .bin files
!
      CALL WGridGetCellString(IDF_SA_Summary,1,irow,Grid_Buffer)
      Iz = LEN_TRIM(Grid_Buffer)
! As it is now possible to switch saving .pro files on/off, even during SA,
! we must now test if the .pro requested has been saved.
! The solutions have been ordered wrt chi**2. We must parse the original run nr from the
! number of the .pdb file. Unless we didn't do a multirun of course.
      IF (RESTART) THEN
        RunStr = Grid_Buffer(Iz-5:Iz-4)
        IF (RunStr(1:1) .EQ. '0') THEN
          RunStr(1:1) = RunStr(2:2)
          RunStr(2:2) = ' '
        ENDIF
        READ(RunStr,'(I2)') RunNr
      ELSE
        RunNr  = 1
      ENDIF
      IF (.NOT. PRO_saved(RunNr)) RETURN
!
!   open the plotting window, ihandle is the window's unique identifier
!     
      Iz = Iz-4
      filename = grid_buffer(1:Iz)//'.pro'
      CALL WindowOpenChild(iHandle, x=10, y=450, width=800, height=400, title=filename)
      IF (iHandle.EQ.-1) THEN
        CALL ErrorMessage("Exceeded maximum number of allowed windows.  Close a profile window.")
        RETURN
      ENDIF 
      CALL RegisterChildWindow(iHandle,DealWithProfilePlot)
      SAUsedChildWindows(iHandle) = 1
      CALL WindowSelect(iHandle)
!
!   open the file
!     
      tFileHandle = 10
      OPEN(UNIT=tFileHandle,FILE=filename,status='unknown',ERR=999)
      DO I = 1, NBIN
        READ(tFileHandle,12,END=999,ERR=999) tReal1, tReal2, store_ycalc(I,ihandle), tReal3
12      FORMAT(F12.4,3(1X,F12.4))
      ENDDO
      CLOSE(tFileHandle)
!
!   calculate the offset for the difference plot
!
      YMin = MINVAL(YOBIN(1:NBIN))
      YMax = MAXVAL(YOBIN(1:NBIN))
      YADD = 0.5 * (YMax+YMin)
      DO II = 1, NBIN
        store_diff(II,ihandle) = YADD + YOBIN(II) - store_ycalc(II,ihandle)
      ENDDO
!   call subroutine which plots data
      CALL plot_pro_file(ihandle)
      RETURN
999   CLOSE(tFileHandle)
! Now close the child window that we had already opened.
      CALL UnRegisterChildWindow(iHandle)
      SAUsedChildWindows(iHandle) = 0
      CALL WindowCloseChild(iHandle)
      CALL ErrorMessage('Error while reading .pro file.')

      END SUBROUTINE organise_sa_result_data
!
!*****************************************************************************
!
      SUBROUTINE plot_pro_file(ihandle)

      USE WINTERACTER
      USE DRUID_HEADER

      INTEGER, INTENT (IN   ) :: ihandle
!
!  Definitions and array declarations.
!
      INTEGER, PARAMETER :: NSETS =  3

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Poly_Colours.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      REAL                      store_ycalc,                      store_diff
      COMMON /ProFilePlotStore/ store_ycalc(MOBS,MaxNumChildWin), store_diff(MOBS,MaxNumChildWin)

      REAL Ymax, Ymin
      REAL Xmax, Xmin

      YMin = MINVAL(YOBIN(1:NBIN))
      YMax = MAXVAL(YOBIN(1:NBIN))
      Xmin = XBIN(1)
      Xmax = XBIN(NBIN)

      CALL WindowSelect(ihandle)
!  Start of all the plotting calls

      CALL IGrArea(0.0, 0.0, 1.0, 1.0)
      CALL IGrUnits(0.0, 0.0, 100.0, 100.0)           
!
!  Start new presentation graphics plot
!
      CALL IPgNewPlot(PgPolyLine,NSETS,NBIN,0,1)
!   Set Clipping Rectangle
!
      CALL IPgClipRectangle('P')
!
!  Set style for each data set
!
      CALL IPgStyle(  1,  1,  0,  0,     KolNumObs)
      CALL IPgStyle(  2,  0,  0,  0,     KolNumCal)
      CALL IPgStyle(  3,  0,  0,  0,     KolNumDif)
!
!  Set marker number for data sets not using default marker
!
!
!  Set units for plot
!
      CALL IPgUnits(      xmin,    ymin, &
                          xmax,    ymax)
!
!  Set presentation graphics area
!
      CALL IPgArea(0.100,0.100,0.900,0.900)
!
!  Draw main title
!
      CALL IGrCharSet('H')
      CALL IGrCharFont(3)
      CALL IGrCharSpacing('P')
      CALL IGrCharSize(  .75,  0.75)
      CALL IGrColourN(KolNumMain)
      CALL IPgTitle('Simulated Annealing Results','C')
!
!  Label bottom X axis
!
      CALL IPgXLabelPos(  0.70)
      CALL IPgXLabel('2 Theta (degrees)','C')
!
!  Label left Y axis
!
      CALL IPgYLabelPos(  0.80)
      CALL IPgYLabelLeft('Intensity','CV')
!
!  Draw axes
!
      CALL IGrColourN(KolNumMain)
      CALL IPgBorder()
!
!  Adjust tick position for X Axes
!
!     CALL IPgXTickPos(     0.0000    ,     1.0000    )
!
!  Scale for bottom X Axis
!
      CALL IPgXTickLength(  1.00)
      CALL IPgDecimalPlaces(      -1)
      CALL IPgXUserScale((/0.0/),0)
      CALL IPgXScaleAngle(  0.00,  0.00)
      CALL IPgXScalePos(  0.38)
      CALL IPgXScale('NT')
!
!  Adjust tick position for Y Axes
!
!      CALL IPgYTickPos(     0.0000    ,     1.0000    )
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
      CALL IPgXYPairs(XBIN(1),YOBIN(1))
      CALL IPgXYPairs(XBIN(1),store_ycalc(1,ihandle))
      CALL IPgXYPairs(XBIN(1),store_diff(1,ihandle))

!  Draw axes
!
      CALL IGrColourN(KolNumMain)
      CALL IPgBorder()

      END SUBROUTINE plot_pro_file
!
!*****************************************************************************
!
