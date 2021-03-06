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
      SUBROUTINE Clear_Bins

      IMPLICIT NONE

      INCLUDE 'params.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      LBIN = 1
!      CALL Update_Bins

      END SUBROUTINE Clear_Bins
!
!*****************************************************************************
!
      SUBROUTINE Delete_SA_run(TheRunNr)
! Deleting an SA run involves updating the iSol2Run variable--this routine takes care of that.

      USE SOLVAR

      IMPLICIT NONE

      INTEGER,       INTENT (IN   ) :: TheRunNr

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER iSol, SolForRun

      ! Find the solution that points to the run that is to be deleted
      DO iSol = 1, NumOf_SA_Runs
        IF ( iSol2Run(iSol) .EQ. TheRunNr ) &
          SolForRun = iSol
      ENDDO
      ! Remove this solution from the list by reshuffling the solutions that follow
      DO iSol = SolForRun, NumOf_SA_Runs-1
        iSol2Run(iSol) = iSol2Run(iSol+1)
      ENDDO
      ! Now update the run numbers: decrement if greater than the run number that is removed
      DO iSol = 1, NumOf_SA_Runs-1
        IF ( iSol2Run(iSol) .GT. TheRunNr ) &
          iSol2Run(iSol) = iSol2Run(iSol) - 1
      ENDDO

      END SUBROUTINE Delete_SA_run
!
!*****************************************************************************
!
      SUBROUTINE Update_Solutions

      USE dash_gui_resources
      USE SOLVAR

      IMPLICIT NONE

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER iSol
      CHARACTER*3 RowLabelStr
      
      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( IN_BATCH ) RETURN

      CALL PushActiveWindowID
      IF (NumOf_SA_Runs .EQ. 0) THEN
! Winteracter doesn't seem able to cope with setting the number of rows in a grid to zero,
! so if no SA runs, the number of rows is set such that it fills the screen but doesn't allow
! scrolling down.
        CALL SelectDASHDialog(IDD_SAW_Page5)
        CALL WGridRows(IDF_SA_Summary, 5)
        CALL WDialogClearField(IDF_SA_Summary)
        CALL WDialogPutInteger(IDF_Limit1, 1)
        CALL WDialogPutInteger(IDF_Limit2, 1)
        CALL SelectDASHDialog(IDD_Summary)
        CALL WGridRows(IDF_SA_Summary, 5)
        CALL WDialogClearField(IDF_SA_Summary)
        CALL WDialogPutInteger(IDF_Limit1, 1)
        CALL WDialogPutInteger(IDF_Limit2, 1)
      ELSE
        CALL SelectDASHDialog(IDD_SAW_Page5)
        CALL WGridRows(IDF_SA_Summary, NumOf_SA_Runs)
        DO iSol = 1, NumOf_SA_Runs
          WRITE(RowLabelStr,'(I3)') iSol
          CALL WGridLabelRow(IDF_SA_summary, iSol, RowLabelStr)
          CALL WGridPutCellInteger (IDF_SA_Summary, 1, iSol, iSol2Run(iSol)) 
          CALL WGridPutCellCheckBox(IDF_SA_Summary, 3, iSol, iSolTicked(iSol2Run(iSol)))
          CALL WGridPutCellReal    (IDF_SA_Summary, 4, iSol, ProfileChiSqd(iSol2Run(iSol)), '(F7.2)')
          CALL WGridPutCellReal    (IDF_SA_Summary, 5, iSol, IntensityChiSqd(iSol2Run(iSol)), '(F7.2)')
        ENDDO
        CALL WDialogPutInteger(IDF_Limit1, 1)
        CALL WDialogPutInteger(IDF_Limit2, NumOf_SA_Runs)
        CALL SelectDASHDialog(IDD_Summary)
        CALL WGridRows(IDF_SA_Summary, NumOf_SA_Runs)
        DO iSol = 1, NumOf_SA_Runs
          WRITE(RowLabelStr,'(I3)') iSol
          CALL WGridLabelRow(IDF_SA_summary, iSol, RowLabelStr)
          CALL WGridPutCellInteger (IDF_SA_Summary, 1, iSol, iSol2Run(iSol)) 
          CALL WGridPutCellCheckBox(IDF_SA_Summary, 3, iSol, iSolTicked(iSol2Run(iSol)))
          CALL WGridPutCellReal    (IDF_SA_Summary, 4, iSol, ProfileChiSqd(iSol2Run(iSol)), '(F7.2)')
          CALL WGridPutCellReal    (IDF_SA_Summary, 5, iSol, IntensityChiSqd(iSol2Run(iSol)), '(F7.2)')
        ENDDO
        CALL WDialogPutInteger(IDF_Limit1, 1)
        CALL WDialogPutInteger(IDF_Limit2, NumOf_SA_Runs)
      ENDIF
      CALL PopActiveWindowID

      END SUBROUTINE Update_Solutions
!
!*****************************************************************************
!
      SUBROUTINE Clear_PO

      USE PO_VAR

      IMPLICIT NONE

      PO_Direction(1) = 0
      PO_Direction(2) = 0
      PO_Direction(3) = 1
      PrefParExists = .FALSE.
      CALL Update_PO

      END SUBROUTINE Clear_PO
!
!*****************************************************************************
!
      SUBROUTINE Update_PO

      USE dash_gui_resources
      USE PO_VAR

      IMPLICIT NONE

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SAW_Page2)
      CALL WDialogPutCheckBoxLogical(IDF_Use_PO, PrefParExists)
      CALL WDialogPutInteger(IDF_PO_a, PO_Direction(1))
      CALL WDialogPutInteger(IDF_PO_b, PO_Direction(2))
      CALL WDialogPutInteger(IDF_PO_c, PO_Direction(3))
      CALL WDialogFieldStateLogical(IDF_PO_a, PrefParExists)
      CALL WDialogFieldStateLogical(IDF_PO_b, PrefParExists)
      CALL WDialogFieldStateLogical(IDF_PO_c, PrefParExists)
      CALL WDialogFieldStateLogical(IDF_LABELa, PrefParExists)
      CALL WDialogFieldStateLogical(IDF_LABELb, PrefParExists)
      CALL WDialogFieldStateLogical(IDF_LABELc, PrefParExists)
      CALL PopActiveWindowID

      END SUBROUTINE Update_PO
!
!*****************************************************************************
!
      SUBROUTINE Clear_PeakFitRanges

      USE WINTERACTER
      USE dash_gui_resources

      IMPLICIT NONE

      INCLUDE 'params.inc'

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

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch


! Clear all variables
      NumPeakFitRange = 0
! Update 'View'|'Peak Positions'...
      CALL Upload_Positions
!... and 'View'|'Peak Widths' tabs
      CALL Upload_Widths

      IF ( in_batch ) return

! Redraw
      CALL Profile_Plot
! Disable Pawley refinement button and 'Next >' button in Wizard window
      CALL CheckIfWeCanDoAPawleyRefinement
! Grey out 'Fit Peaks' button on toolbar
      CALL UpdatePeaksButtonsStates

      END SUBROUTINE Clear_PeakFitRanges
!
!*****************************************************************************
!
      SUBROUTINE Clear_BackGround

      IMPLICIT NONE

      INCLUDE 'params.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER         PR_NumBack
      REAL                        PR_BackGround
      COMMON /PRBACK/ PR_NumBack, PR_BackGround(1:10)

      INTEGER I

      PR_BackGround = 0.0
      DO I = 1, NBIN
        YBBIN(I) = 0.0
      ENDDO

      END SUBROUTINE Clear_BackGround
!
!*****************************************************************************
!
      SUBROUTINE Clear_UnitCell_WithConfirmation 

      IMPLICIT NONE
      
      LOGICAL, EXTERNAL :: Confirm
           
      IF (Confirm('Do you wish to clear all cell parameters?')) CALL Clear_UnitCell

      END SUBROUTINE Clear_UnitCell_WithConfirmation
!
!*****************************************************************************
!
      SUBROUTINE Clear_UnitCell

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      INTEGER I

      ZeroPoint = 0.0
      CALL Upload_ZeroPoint
      LatBrav = 1 ! Triclinic
      DO I = 1, 6
        CellPar(I) = -999.0
      ENDDO
      CALL Upload_CrystalSystem

      END SUBROUTINE Clear_UnitCell
!
!*****************************************************************************
!
      SUBROUTINE Clear_Zmatrices

      USE ZMVAR

      IMPLICIT NONE

! Blow away the Z-matrices
      nFrag = 0
      CALL UpdateZmatrixSelection

      END SUBROUTINE Clear_Zmatrices
!
!*****************************************************************************
!
      SUBROUTINE Clear_SA

      USE WINTERACTER
      USE dash_gui_resources

      IMPLICIT NONE

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      NumOf_SA_Runs = 0
      IF (in_batch ) return

      CALL PushActiveWindowID
      ! Now clear the solutions from the analyse solutions window
      CALL SelectDASHDialog(IDD_SAW_Page5)
      CALL WDialogClearField(IDF_SA_Summary)
      CALL PopActiveWindowID

      END SUBROUTINE Clear_SA
!
!*****************************************************************************
!
      SUBROUTINE Update_TruncationLimits
!
! Set minimum and maximum truncation values in Wizard in accordance with data read in.
! This routine should be called whenever a new pattern is loaded, so that can
! be a new powder pattern, a .sdi file or a .dash file

      USE dash_gui_resources
      USE VARIABLES

      IMPLICIT NONE

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      REAL, EXTERNAL :: TwoTheta2dSpacing, dSpacing2TwoTheta
      LOGICAL, EXTERNAL :: FnPatternOK, FnWavelengthOK
      REAL       tMaxResolution

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch


      IF ( IN_BATCH ) RETURN

      CALL PushActiveWindowID
! In principle, set resolution so as to truncate at DefaultMaxResolution.
! However, if truncation resolution not attainable with current data range / wavelength,
! adjust the setting of the maximum resolution to maximum possible.
      CALL SelectDASHDialog(IDD_ViewPawley)
      IF (FnPatternOK() .AND. FnWavelengthOK()) THEN
        tMaxResolution = MAX(TwoTheta2dSpacing(XPMAX), DefaultMaxResolution)
        CALL WDialogPutReal(IDF_MaxResolution, tMaxResolution)
      ELSE
        tMaxResolution = DefaultMaxResolution
        CALL WDialogClearField(IDF_MaxResolution)
      ENDIF
      CALL SelectDASHDialog(IDD_PW_Page5)
! Initialise truncation of start of powder pattern
      CALL WDialogPutReal(IDF_Min2Theta, XPMIN, '(F6.3)')
      CALL WDialogPutReal(IDF_MaxResolution, tMaxResolution)
      CALL WDialogPutReal(IDF_Max2Theta, dSpacing2TwoTheta(tMaxResolution))
      CALL PopActiveWindowID

      END SUBROUTINE Update_TruncationLimits
!
!*****************************************************************************
!
