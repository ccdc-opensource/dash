      SUBROUTINE organise_sa_result_data(irow)
!ep July 2001
!   called from SASummary.for
!	This subroutine manipulates the data required to plot the observed  
!   diffraction pattern with the calculated pattern and difference.  The
!   data is read in from the .pro file and stored in COMMON BLOCK Plotlink
!   as x_store and y_store.  The array Link (also stored in Plotlink common
!   block) relates the childwindow identifier to the correct y data.


      USE WINTERACTER
	  USE Druid_header
!
!
!  Definitions and array declarations.
!
      INTEGER irow, iz, temprow
      REAL yadd
	  CHARACTER*255 Grid_Buffer
	  CHARACTER*75 filename
	  INCLUDE 'PARAMS.INC'
	  DIMENSION xobsep(MOBS)
	  DIMENSION yobsep(MOBS)
	  DIMENSION ycalcep(MOBS)
	  DIMENSION ydif(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
      YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
      XGGMIN,XGGMAX,YGGMIN,YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
        INTEGER link
	  REAL store_x, store_y
!     the store_y array size means a maximum of 30 runs can be performed at once
	  COMMON /plotlink/ link(30), store_x(MOBS,3), store_y(MOBS,90)
!
!   reading in the data from the saved .pro files
!
	temprow = irow
!
	CALL WGridGetCellString(IDF_SA_Summary,1,temprow,Grid_Buffer)
	Iz = len_trim(Grid_Buffer)
	Iz = Iz-4
	filename = grid_buffer(1:Iz)//'.pro'
!
	OPEN(unit=61, file=filename, status = 'old', err=999)
	DO i = ipmin,ipmax
		READ(61,20) xobsep(i), yobsep(i), ycalcep(i)
20		FORMAT(3(f12.6))
	END DO
	CLOSE(61)
!
!   calculate the offset for the difference plot
!
    YADD=0.5*(YPGMAX+YPGMIN)
      DO II=IPMIN,IPMAX
      YDIF(II)=YADD+yobsep(II)-ycalcep(II)
      END DO
!
!   Configure the x data for plotting function
!
	DO ik = ipmin,ipmax
		DO ij = 1, 3
        store_x(ik,ij) = xobsep(ik)
		END DO
	END DO
!
!   open the plotting window, ihandle is the window's unique identifier
!	
	CALL WindowOpenChild(ihandle, x=10, y=450, width=800, height=400, title=filename)
	CALL WindowSelect(ihandle)
!
!   Calculate link(ihandle). The Link array provides a link between the child window
!   identifier (ihandle) and its store_y data. There are 3 columns of store_y data for
!   each childwindow, and link(ihandle) is the number of the first column of data. 
! 
    link(ihandle) = (ihandle*3)-2
!
!   configuring y data for plotting
!
	DO in = ipmin, ipmax
       store_y(in,link(ihandle)) = yobsep(in)
	   store_y(in,(link(ihandle)+1)) = ycalcep(in)
	   store_y(in,(link(ihandle)+2)) = ydif(in)
	END DO   	
!   call subroutine which plots data
    CALL plot_pro_file(ihandle)    
	
			
999	CONTINUE

      RETURN
      END SUBROUTINE organise_sa_result_data

!***********************************************************************************
     
	 
	  subroutine replot_pro_file(ihandle)
! ep July 2001
!     When the profile plots, generated from the "view" button of the 
!     IDD_SA_Multi_completed_ep grid are resized or exposed this subroutine 
!     handles the replotting of them 

      USE WINTERACTER
	  Use Druid_header
!
      integer ihandle
!
!  select the correct window for redrawing	
	 call windowselect(ihandle)
!    
	 call plot_pro_file(ihandle)
	 end subroutine replot_pro_file


!*************************************************************************************************


      subroutine plot_pro_file(ihandle)


	  USE WINTERACTER
	  Use Druid_header
!
!
!  Definitions and array declarations.
!
      INTEGER,PARAMETER :: NSETS   =     3
      INTEGER           :: ISET
      INCLUDE 'PARAMS.INC'
	integer ihandle
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
      YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD, &
      XGGMIN,XGGMAX,YGGMIN,YGGMAX
	COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
      INTEGER Link
      REAL store_x, store_y
	COMMON /plotlink/ link(30), store_x(MOBS,3), store_y(MOBS,90)

!  Start of all the plotting calls

	call IGrArea(0.0, 0.0, 1.0, 1.0)
	call IGrUnits(0.0, 0.0, 100.0, 100.0)		
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
      CALL IPgStyle(  1,  1,  0,  0,      32,      64)
      CALL IPgStyle(  2,  0,  0,  0,     223,     101)
      CALL IPgStyle(  3,  0,  0,  0,     176,     139)
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
      CALL IGrCharFont(       1)
      CALL IGrCharSpacing('F')
      CALL IGrCharSize(  .75,  0.75)
      CALL IGrColourN(223)
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
      CALL IGrColourN(223)
      CALL IPgBorder()
!
!  Adjust tick position for X Axes
!
      CALL IPgXTickPos(     0.0000    ,     1.0000    )
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
      CALL IPgYTickPos(     0.0000    ,     1.0000    )
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
      DO ISET = 1,NSETS
          CALL IPgXYPairs(store_x(1,ISET),store_y(link(ihandle),(link(ihandle)-1+ISET)))
      END DO

      end subroutine plot_pro_file
