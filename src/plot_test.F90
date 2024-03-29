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
      SUBROUTINE organise_sa_result_data(TheSolutionNr)
!ep July 2001
!   called from SASummary.f90
! This subroutine manipulates the data required to plot the observed  
! diffraction pattern with the calculated pattern and difference.  The
! data is calculated from solution # TheSolutionNr and stored in COMMON BLOCK ProFilePlotStore
! iHandle is used to identify the column of the store_ycalc array where the data
! for each child window (iHandle) is stored

      USE WINTERACTER
      USE VARIABLES
      USE SOLVAR
      USE PO_VAR
      USE ATMVAR
!
!  Definitions and array declarations.
!
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheSolutionNr

      INCLUDE 'params.inc'

! Used to manage the child windows which display the profile plots
      INTEGER                 SAUsedChildWindows
      COMMON /SAChildWindows/ SAUsedChildWindows(MaxNumChildWin)

! The number of columns in the store-array is set to the maximum number of 
! child windows allowed
      REAL                      store_ycalc
      COMMON /ProFilePlotStore/ store_ycalc(1:MOBS, 1:MaxNumChildWin)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL                  Ymin,                   Ymax,                   XMin,                   XMax
      COMMON /PROFPLOTAXES/ Ymin(1:MaxNumChildWin), Ymax(1:MaxNumChildWin), XMin(1:MaxNumChildWin), XMax(1:MaxNumChildWin)

      INTEGER         NATOM
      REAL                   Xato
      INTEGER                          KX
      REAL                                        AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, Xato(3,MaxAtm_3), KX(3,MaxAtm_3), AMULT(MaxAtm_3), TF(MaxAtm_3),  &
                      KTF(MaxAtm_3), SITE(MaxAtm_3), KSITE(MaxAtm_3), ISGEN(3,MaxAtm_3),    &
                      SDX(3,MaxAtm_3), SDTF(MaxAtm_3), SDSITE(MaxAtm_3), KOM17

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      EXTERNAL DealWithProfilePlot
      CHARACTER(20), EXTERNAL :: Integer2String
      INTEGER  I, iHandle, TheRunNr
      REAL     rDummy
!
!   Open the plotting window, iHandle is the window's unique identifier
!     
      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( in_batch ) &
        RETURN
      TheRunNr = iSol2Run(TheSolutionNr)
      CALL WindowOpenChild(iHandle, x=10, y=450, width=800, height=400, title='Solution number '//Integer2String(TheSolutionNr))
      IF (iHandle .EQ. -1) THEN
        CALL ErrorMessage("Exceeded maximum number of allowed windows. Please close a profile window.")
        RETURN
      ENDIF 
      CALL RegisterChildWindow(iHandle, DealWithProfilePlot)
      SAUsedChildWindows(iHandle) = 1
      CALL WindowSelect(iHandle)
      CALL WindowClear
! Make sure that we use hydrogens for the calculation of the powder pattern.
      LOG_HYDROGENS = .TRUE.
      CALL CREATE_FOB(.FALSE.)

! Fill Xato
      DO I = 1, NATOM
        Xato(1,I) = XAtmCoords(1, I, TheRunNr)
        Xato(2,I) = XAtmCoords(2, I, TheRunNr)
        Xato(3,I) = XAtmCoords(3, I, TheRunNr)
      ENDDO
! Fill Preferred Orientation part
      IF (PrefParExists) THEN
        CALL PO_PRECFC(BestValuesDoF(iPrfPar, TheRunNr))
      ENDIF
! VALCHI fills BICALC
      CALL VALCHI(rDummy, 0)    ! Structural part
! Valchipro fills YCBIN
      CALL VALCHIPRO(rDummy)

      DO I = 1, NBIN
        store_ycalc(I,iHandle) = YCBIN(I)
      ENDDO
! Initialise the x and y max min values for the plot

      YMin(ihandle) = MINVAL(YOBIN(1:NBIN))
      YMax(ihandle) = MAXVAL(YOBIN(1:NBIN))
      Xmin(ihandle) = XBIN(1)
      Xmax(ihandle) = XBIN(NBIN)

! Call subroutine which plots data
      CALL plot_pro_file(iHandle)

      END SUBROUTINE organise_sa_result_data
!
!*****************************************************************************
!
      SUBROUTINE plot_pro_file(iHandle)

      USE WINTERACTER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iHandle
!
!  Definitions and array declarations.
!
      INTEGER, PARAMETER :: NSETS =  3

      INCLUDE 'params.inc'
      INCLUDE 'Poly_Colours.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

! The number of columns in the store-array is set to the maximum number of 
! child windows allowed
      REAL                      store_ycalc
      COMMON /ProFilePlotStore/ store_ycalc(1:MOBS, 1:MaxNumChildWin)

      REAL                  Ymin,                   Ymax,                   XMin,                   XMax
      COMMON /PROFPLOTAXES/ Ymin(1:MaxNumChildWin), Ymax(1:MaxNumChildWin), XMin(1:MaxNumChildWin), XMax(1:MaxNumChildWin)

      INTEGER II
      REAL    YADD
      REAL    diff(1:MOBS)

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( in_batch ) &
        RETURN
!
!  Calculate offset for difference plot.  Position will move as zoom in on profile plot.
!
      YADD = 0.5 * (YMax(iHandle)+YMin(iHandle))
      DO II = 1, NBIN
        diff(II) = YADD + YOBIN(II) - store_ycalc(II,iHandle)
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
      CALL IPgNewPlot(PgPolyLine, NSETS, NBIN, 0, 1)
!   Set Clipping Rectangle
!
      CALL IPgClipRectangle('P')
!
!  Set style for each data set
!
      CALL IPgStyle(  1,  1,  0,  0, KolNumObs)
      CALL IPgStyle(  2,  0,  0,  0, KolNumCal)
      CALL IPgStyle(  3,  0,  0,  0, KolNumDif)
!
!  Set marker number for data sets not using default marker
!
!
!  Set units for plot
!
      CALL IPgUnits( xmin(iHandle), ymin(iHandle), &
                     xmax(iHandle), ymax(iHandle))
!
!  Set presentation graphics area
!
      CALL IPgArea(0.100, 0.100, 0.900, 0.900)
!
!  Draw main title
!
      CALL IGrCharSet('H')
      CALL IGrCharFont(3)
      CALL IGrCharSpacing('P')
      CALL IGrCharSize(  0.75,  0.75)
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
      CALL IPgXTickLength(1.0)
      CALL IPgDecimalPlaces(-1)
      CALL IPgXUserScale((/0.0/), 0)
      CALL IPgXScaleAngle(0.0, 0.0)
      CALL IPgXScalePos(0.38)
      CALL IPgXScale('NT')
!
!  Adjust tick position for Y Axes
!
!      CALL IPgYTickPos(     0.0000    ,     1.0000    )
!
!  Scale for left Y Axis
!
      CALL IPgYTickLength(1.0)
      CALL IPgDecimalPlaces(-1)
      CALL IPgYUserScale((/0.0/), 0)
      CALL IPgYScaleAngle(0.0, 0.0)
      CALL IPgYScalePos(1.5)
      CALL IPgYScaleLeft('NT')
!
!  Draw graph.
!
      CALL IPgXYPairs(XBIN(1), YOBIN(1))
      CALL IPgXYPairs(XBIN(1), store_ycalc(1,iHandle))
      CALL IPgXYPairs(XBIN(1), diff(1))

!  Draw axes
!
      CALL IGrColourN(KolNumMain)
      CALL IPgBorder

      END SUBROUTINE plot_pro_file
!
!*****************************************************************************
!
