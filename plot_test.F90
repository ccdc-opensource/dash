!O!
!O!*****************************************************************************
!O!
!O      SUBROUTINE organise_sa_result_data(irow)
!O!ep July 2001
!O!   called from SASummary.f90
!O!     This subroutine manipulates the data required to plot the observed  
!O!   diffraction pattern with the calculated pattern and difference.  The
!O!   data is read in from the .pro file and stored in COMMON BLOCK ProFilePLotStore
!O!  ihandle is used to identify the column of the store_ arrays where the data
!O!  for each child window (ihandle) is stored
!O
!O      USE WINTERACTER
!O      USE DRUID_HEADER
!O      USE VARIABLES
!O!
!O!  Definitions and array declarations.
!O!
!O      IMPLICIT NONE
!O
!O      INTEGER, INTENT (IN   ) :: irow
!O
!O      INCLUDE 'PARAMS.INC'
!O
!O! Used to manage the child windows which display the profile plots
!O      INTEGER                 SAUsedChildWindows
!O      COMMON /SAChildWindows/ SAUsedChildWindows(MaxNumChildWin)
!O
!O      REAL store_ycalc, store_diff
!O! the number of columns in the store-arrays is set to the maximum number of 
!O! child windows allowed
!O      COMMON /ProFilePlotStore/ store_ycalc(MOBS,MaxNumChildWin), store_diff(MOBS,MaxNumChildWin)
!O
!O      LOGICAL          PRO_saved
!O      COMMON /PROCOM/  PRO_saved(1:MaxRun)
!O
!O      INTEGER          NBIN, LBIN
!O      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
!O      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)
!O
!O      INTEGER iz
!O      REAL yadd
!O      REAL Ymin
!O      REAL Ymax
!O      CHARACTER*MaxPathLength Grid_Buffer
!O      CHARACTER*MaxPathLength filename
!O      EXTERNAL DealWithProfilePlot
!O      CHARACTER*2 RunStr
!O      INTEGER I, II, RunNr, tFileHandle, iHandle
!O      REAL tReal1, tReal2, tReal3
!O!
!O!   reading in the data from the saved .pro files
!O!
!O      CALL WGridGetCellString(IDF_SA_Summary,1,irow,Grid_Buffer)
!O      Iz = LEN_TRIM(Grid_Buffer)
!O! As it is now possible to switch saving .pro files on/off, even during SA,
!O! we must now test if the .pro requested has been saved.
!O! The solutions have been ordered wrt chi**2. We must parse the original run nr from the
!O! number of the .pdb file.
!O      RunStr = Grid_Buffer(Iz-5:Iz-4)
!O      IF (RunStr(1:1) .EQ. '0') THEN
!O        RunStr(1:1) = RunStr(2:2)
!O        RunStr(2:2) = ' '
!O      ENDIF
!O      READ(RunStr,'(I2)') RunNr
!O      IF (.NOT. PRO_saved(RunNr)) RETURN
!O!
!O!   open the plotting window, ihandle is the window's unique identifier
!O!     
!O      Iz = Iz-4
!O      filename = grid_buffer(1:Iz)//'.pro'
!O      CALL WindowOpenChild(iHandle, x=10, y=450, width=800, height=400, title=filename)
!O      IF (iHandle.EQ.-1) THEN
!O        CALL ErrorMessage("Exceeded maximum number of allowed windows.  Close a profile window.")
!O        RETURN
!O      ENDIF 
!O      CALL RegisterChildWindow(iHandle,DealWithProfilePlot)
!O      SAUsedChildWindows(iHandle) = 1
!O      CALL WindowSelect(iHandle)
!O!
!O!   open the file
!O!     
!O      tFileHandle = 10
!O      OPEN(UNIT=tFileHandle,FILE=filename,status='unknown',ERR=999)
!O      DO I = 1, NBIN
!O        READ(tFileHandle,12,END=999,ERR=999) tReal1, tReal2, store_ycalc(I,ihandle), tReal3
!O12      FORMAT(F12.4,3(1X,F12.4))
!O      ENDDO
!O      CLOSE(tFileHandle)
!O!
!O!   calculate the offset for the difference plot
!O!
!O      YMin = MINVAL(YOBIN(1:NBIN))
!O      YMax = MAXVAL(YOBIN(1:NBIN))
!O      YADD = 0.5 * (YMax+YMin)
!O      DO II = 1, NBIN
!O        store_diff(II,ihandle) = YADD + YOBIN(II) - store_ycalc(II,ihandle)
!O      ENDDO
!O!   call subroutine which plots data
!O      CALL plot_pro_file(ihandle)
!O      RETURN
!O999   CLOSE(tFileHandle)
!O! Now close the child window that we had already opened.
!O      CALL UnRegisterChildWindow(iHandle)
!O      SAUsedChildWindows(iHandle) = 0
!O      CALL WindowCloseChild(iHandle)
!O      CALL ErrorMessage('Error while reading .pro file.')
!O
!O      END SUBROUTINE organise_sa_result_data
!
!*****************************************************************************
!
      SUBROUTINE organise_sa_result_data(TheSolutionNr)
!ep July 2001
!   called from SASummary.f90
!     This subroutine manipulates the data required to plot the observed  
!   diffraction pattern with the calculated pattern and difference.  The
!   data is read in from the .pro file and stored in COMMON BLOCK ProFilePlotStore
!  iHandle is used to identify the column of the store_ arrays where the data
!  for each child window (iHandle) is stored

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE SOLVAR
!
!  Definitions and array declarations.
!
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheSolutionNr

      INCLUDE 'PARAMS.INC'

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
      REAL, DIMENSION (20):: Ymin
      REAL, DIMENSION (20):: Ymax
      REAL, DIMENSION (20):: Xmax
      REAL, DIMENSION (20):: Xmin
      COMMON /PROFPLOTAXES/ Ymin, Ymax, XMin, XMax

      INTEGER         NATOM
      REAL                   Xato
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, Xato(3,150), KX(3,150), AMULT(150), TF(150),  &
                      KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
                      SDX(3,150), SDTF(150), SDSITE(150), KOM17

      EXTERNAL DealWithProfilePlot
      INTEGER  I, iHandle, TheRunNr
      REAL     rDummy
      CHARACTER(20), EXTERNAL :: Integer2String
!
!   open the plotting window, iHandle is the window's unique identifier
!     
      TheRunNr = iSol2Run(TheSolutionNr)
      CALL WindowOpenChild(iHandle, x=10, y=450, width=800, height=400, title='Solution number '//Integer2String(TheSolutionNr))
      IF (iHandle.EQ.-1) THEN
        CALL ErrorMessage("Exceeded maximum number of allowed windows.  Close a profile window.")
        RETURN
      ENDIF 
      CALL RegisterChildWindow(iHandle,DealWithProfilePlot)
      SAUsedChildWindows(iHandle) = 1
      CALL WindowSelect(iHandle)
      CALL WindowClear

! Fill Xato
      DO I = 1, NATOM
        Xato(1,I) = XAtmCoords(1,I,TheRunNr)
        Xato(2,I) = XAtmCoords(2,I,TheRunNr)
        Xato(3,I) = XAtmCoords(3,I,TheRunNr)
      ENDDO
! VALCHI fills BICALC
! @@      CALL VALCHI(rDummy,1000) ! Preferrred orientation part if appropriate
      CALL VALCHI(rDummy,0)    ! Structural part
! Valchipro fills YCBIN
      CALL VALCHIPRO(rDummy)

      DO I = 1, NBIN
        store_ycalc(I,iHandle) = YCBIN(I)
      ENDDO
!   Initialise the x and y max min values for the plot

      YMin(ihandle) = MINVAL(YOBIN(1:NBIN))
      YMax(ihandle) = MAXVAL(YOBIN(1:NBIN))
      Xmin(ihandle) = XBIN(1)
      Xmax(ihandle) = XBIN(NBIN)
!
! calculate the offset for the difference plot
!
!!      YADD = 0.5 * (YMax(ihandle)+YMin(ihandle))
!!      DO I = 1, NBIN
!!        store_diff(I,ihandle) = YADD + YOBIN(I) - store_ycalc(I,ihandle)
!!      ENDDO

! call subroutine which plots data
      CALL plot_pro_file(iHandle)

      END SUBROUTINE organise_sa_result_data
!
!*****************************************************************************
!
      SUBROUTINE plot_pro_file(iHandle)

      USE WINTERACTER
      USE DRUID_HEADER

      INTEGER, INTENT (IN   ) :: iHandle
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

      REAL, DIMENSION (20):: Ymin
      REAL, DIMENSION (20):: Ymax
      REAL, DIMENSION (20):: Xmax
      REAL, DIMENSION (20):: Xmin
      COMMON /PROFPLOTAXES/ Ymin, Ymax, XMin, XMax

!
!  Calculate offset for difference plot.  Position will move as zoom in on profile plot.
!
      YADD = 0.5 * (YMax(iHandle)+YMin(iHandle))
      DO II = 1, NBIN
        store_diff(II,iHandle) = YADD + YOBIN(II) - store_ycalc(II,iHandle)
      ENDDO
      CALL WindowSelect(iHandle)
!
!  Start of all the plotting calls
!
      CALL IGrArea(0.0, 0.0, 1.0, 1.0)
      CALL IGrUnits(0.0, 0.0, 1.0, 1.0)           
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
      CALL IPgUnits(      xmin(iHandle),    ymin(iHandle), &
                          xmax(iHandle),    ymax(iHandle))
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
      CALL IPgBorder
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
      CALL IPgXYPairs(XBIN(1),store_ycalc(1,iHandle))
      CALL IPgXYPairs(XBIN(1),store_diff(1,iHandle))

!  Draw axes
!
      CALL IGrColourN(KolNumMain)
      CALL IPgBorder

      END SUBROUTINE plot_pro_file
!
!*****************************************************************************
!
