!
!*****************************************************************************
!
      SUBROUTINE organise_sa_result_data(irow)
!ep July 2001
!   called from SASummary.for
!     This subroutine manipulates the data required to plot the observed  
!   diffraction pattern with the calculated pattern and difference.  The
!   data is read in from the .pro file and stored in COMMON BLOCK ProFilePLotStore
!  ihandle is used to identify the column of the store_ arrays where the data
!  for each child window (ihandle) is stored

      USE WINTERACTER
      USE DRUID_HEADER
!
!  Definitions and array declarations.
!
      INCLUDE 'PARAMS.INC'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX

      INTEGER          IPMIN, IPMAX, IPMINOLD, IPMAXOLD
      COMMON /PROFIPM/ IPMIN, IPMAX, IPMINOLD, IPMAXOLD

! Used to manage the child windows which display the profile plots
      INTEGER                 SAUsedChildWindows
      COMMON /SAChildWindows/ SAUsedChildWindows(MaxNumChildWin)

      REAL store_ycalc, store_diff
! the number of columns in the store-arrays is set to the maximum number of 
! child windows allowed
      COMMON /ProFilePlotStore/ store_ycalc(MOBS,MaxNumChildWin), store_diff(MOBS,MaxNumChildWin)

      INTEGER irow, iz, temprow
      REAL yadd, ydif
      CHARACTER*255 Grid_Buffer
      CHARACTER*75 filename
      DIMENSION xobsep(MOBS)
      DIMENSION yobsep(MOBS)
      DIMENSION ycalcep(MOBS)
      DIMENSION ydif(MOBS)
      EXTERNAL DealWithProfilePlot
!
!   reading in the data from the saved .pro files
!
      temprow = irow
      CALL WGridGetCellString(IDF_SA_Summary,1,temprow,Grid_Buffer)
      Iz = LEN_TRIM(Grid_Buffer)
      Iz = Iz-4
      filename = grid_buffer(1:Iz)//'.pro'
      OPEN(unit=61, file=filename, status = 'old', err=999)
      DO i = ipmin,ipmax
        READ(61,20) xobsep(i), yobsep(i), ycalcep(i)
20      FORMAT(3(x, f12.4))
      ENDDO
      CLOSE(61)
!
!   calculate the offset for the difference plot
!
      YADD=0.5*(YPGMAX+YPGMIN)
      DO II = IPMIN, IPMAX
        YDIF(II) = YADD + yobsep(II) - ycalcep(II)
      ENDDO
!
!   open the plotting window, ihandle is the window's unique identifier
!     
      CALL WindowOpenChild(ihandle, x=10, y=450, width=800, height=400, title=filename)
      IF(ihandle.eq.-1) THEN
        CALL ErrorMessage("Exceeded Maximum Number of Allowed Windows.  Close a profile window")
        RETURN
      ENDIF 
      CALL RegisterChildWindow(ihandle,DealWithProfilePlot)
      SAUsedChildWindows(ihandle) = 1
      CALL WindowSelect(ihandle)
!
!   configuring y data for plotting
!
      DO in = ipmin, ipmax
        store_ycalc(in,(ihandle)) = ycalcep(in)
        store_diff(in,(ihandle)) = ydif(in)
      ENDDO      
!   call subroutine which plots data
      CALL plot_pro_file(ihandle)
999   CONTINUE

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

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX

      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      REAL                      store_ycalc,                      store_diff
      COMMON /ProFilePlotStore/ store_ycalc(MOBS,MaxNumChildWin), store_diff(MOBS,MaxNumChildWin)

      CALL WindowSelect(ihandle)
!  Start of all the plotting calls

      CALL IGrArea(0.0, 0.0, 1.0, 1.0)
      CALL IGrUnits(0.0, 0.0, 100.0, 100.0)           
!
!  Start new presentation graphics plot
!
      CALL IPgNewPlot(PgPolyLine,NSETS,ipmax,0,1)
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
      CALL IPgUnits(      xpmin,    ypmin, &
                          xpmax,    ypmax)
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
!      DO ISET = 1,NSETS
!          CALL IPgXYPairs(store_x(1,ISET),store_y(link(ihandle),(link(ihandle)-1+ISET)))
!      END DO
      CALL IPgXYPairs(XBIN(1),YOBIN(1))
      CALL IPgXYPairs(XBIN(1),store_ycalc(ihandle,ihandle))
      CALL IPgXYPairs(XBIN(1),store_diff(ihandle,ihandle))

      END SUBROUTINE plot_pro_file
!
!*****************************************************************************
!
