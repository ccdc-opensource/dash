!
!*****************************************************************************
!
      SUBROUTINE Clear_Bins

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      LBIN = 1
!      CALL Update_Bins

      END SUBROUTINE Clear_Bins

!
!*****************************************************************************
!
      SUBROUTINE Update_Solutions

      USE DRUID_HEADER
      USE SOLVAR

      IMPLICIT NONE

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER iSol
      CHARACTER*2 RowLabelStr

      CALL PushActiveWindowID
      IF (NumOf_SA_Runs .EQ. 0) THEN
! Winteracter doesn't seem able to cope with setting the number of rows in a grid to zero,
! so if no SA runs, the number of rows is set such that it fills the screen but doesn't allow
! scrolling down.
        CALL WDialogSelect(IDD_SAW_Page5)
        CALL WGridRows(IDF_SA_Summary, 5)
        CALL WDialogClearField(IDF_SA_Summary)
        CALL WDialogPutInteger(IDF_Limit1,1)
        CALL WDialogPutInteger(IDF_Limit2,1)
        CALL WDialogSelect(IDD_Summary)
        CALL WGridRows(IDF_SA_Summary, 5)
        CALL WDialogClearField(IDF_SA_Summary)
        CALL WDialogPutInteger(IDF_Limit1,1)
        CALL WDialogPutInteger(IDF_Limit2,1)
      ELSE
        CALL WDialogSelect(IDD_SAW_Page5)
        CALL WGridRows(IDF_SA_Summary, NumOf_SA_Runs)
        DO iSol = 1, NumOf_SA_Runs
          WRITE(RowLabelStr,'(I2)') iSol
          CALL WGridLabelRow(IDF_SA_summary,iSol,RowLabelStr)
          CALL WGridPutCellInteger (IDF_SA_Summary,1,iSol,iSol2Run(iSol)) 
          CALL WGridPutCellCheckBox(IDF_SA_Summary,3,iSol,iSolTicked(iSol2Run(iSol)))
          CALL WGridPutCellReal    (IDF_SA_Summary,4,iSol,ProfileChiSqd(iSol2Run(iSol)),'(F7.2)')
          CALL WGridPutCellReal    (IDF_SA_Summary,5,iSol,IntensityChiSqd(iSol2Run(iSol)),'(F7.2)')
        ENDDO
        CALL WDialogPutInteger(IDF_Limit1,1)
        CALL WDialogPutInteger(IDF_Limit2,NumOf_SA_Runs)
        CALL WDialogSelect(IDD_Summary)
        CALL WGridRows(IDF_SA_Summary, NumOf_SA_Runs)
        DO iSol = 1, NumOf_SA_Runs
          WRITE(RowLabelStr,'(I2)') iSol
          CALL WGridLabelRow(IDF_SA_summary,iSol,RowLabelStr)
          CALL WGridPutCellInteger (IDF_SA_Summary,1,iSol,iSol2Run(iSol)) 
          CALL WGridPutCellCheckBox(IDF_SA_Summary,3,iSol,iSolTicked(iSol2Run(iSol)))
          CALL WGridPutCellReal    (IDF_SA_Summary,4,iSol,ProfileChiSqd(iSol2Run(iSol)),'(F7.2)')
          CALL WGridPutCellReal    (IDF_SA_Summary,5,iSol,IntensityChiSqd(iSol2Run(iSol)),'(F7.2)')
        ENDDO
        CALL WDialogPutInteger(IDF_Limit1,1)
        CALL WDialogPutInteger(IDF_Limit2,NumOf_SA_Runs)
      ENDIF
      CALL PopActiveWindowID

      END SUBROUTINE Update_Solutions
!
!*****************************************************************************
!
      SUBROUTINE Clear_PO

      USE PO_VAR

      IMPLICIT NONE

      PrefPars(1) = 0.0
      PrefPars(2) = 0.0
      PrefPars(3) = 1.0
      PrefParExists = .FALSE.
      CALL Update_PO

      END SUBROUTINE Clear_PO
!
!*****************************************************************************
!
      SUBROUTINE Update_PO

      USE DRUID_HEADER
      USE PO_VAR

      IMPLICIT NONE

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page2)
      CALL WDialogPutCheckBoxLogical(IDF_Use_PO,PrefParExists)
      CALL WDialogPutInteger(IDF_PO_a,NINT(PrefPars(1)))
      CALL WDialogPutInteger(IDF_PO_b,NINT(PrefPars(2)))
      CALL WDialogPutInteger(IDF_PO_c,NINT(PrefPars(3)))
      CALL WDialogFieldStateLogical(IDF_PO_a,PrefParExists)
      CALL WDialogFieldStateLogical(IDF_PO_b,PrefParExists)
      CALL WDialogFieldStateLogical(IDF_PO_c,PrefParExists)
      CALL WDialogFieldStateLogical(IDF_LABELa,PrefParExists)
      CALL WDialogFieldStateLogical(IDF_LABELb,PrefParExists)
      CALL WDialogFieldStateLogical(IDF_LABELc,PrefParExists)
      CALL PopActiveWindowID

      END SUBROUTINE Update_PO
!
!*****************************************************************************
!
      SUBROUTINE Clear_PeakFitRanges

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

! Clear all variables
      NumPeakFitRange = 0
! Update 'View'|'Peak Positions'...
      CALL Upload_Positions
!... and 'View'|'Peak Widths' tabs
      CALL Upload_Widths
! Redraw
      CALL Profile_Plot
! Grey out 'Fit Peaks' button on toolbar
      CALL UpdateFitPeaksButtonState
! Grey out 'Delete all peak fit ranges' button on toolbar
      CALL WMenuSetState(ID_ClearPeakFitRanges,ItemEnabled,WintOff)
! Grey out 'Clear Peaks' button in Wizard window
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page10)
      CALL WDialogFieldState(IDF_ClearPeakFitRanges,Disabled)
      CALL PopActiveWindowID
! Disable Pawley refinement button and 'Next >' button in Wizard window
      CALL CheckIfWeCanDoAPawleyRefinement
      CALL CheckIfWeCanIndex

      END SUBROUTINE Clear_PeakFitRanges
!
!*****************************************************************************
!
      SUBROUTINE Clear_BackGround

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

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

      INCLUDE 'lattice.inc'

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
      gotzmfile = .FALSE.
      zmNumberOfCopies = 0
      CALL UpdateZmatrixSelection

      END SUBROUTINE Clear_Zmatrices
!
!*****************************************************************************
!
      SUBROUTINE Clear_SA

      IMPLICIT NONE

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      NumOf_SA_Runs = 0

      END SUBROUTINE Clear_SA
!
!*****************************************************************************
!
      SUBROUTINE Update_TruncationLimits
!
! Set minimum and maximum truncation values in Wizard in accordance with data read in.
! This routine should be called whenever a new pattern is loaded, so that can
! be a new powder pattern, a .sdi file or a .dash file

      USE DRUID_HEADER
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

      CALL PushActiveWindowID
! In principle, set resolution so as to truncate at DefaultMaxResolution.
! However, if truncation resolution not attainable with current data range / wavelength,
! adjust the setting of the maximum resolution to maximum possible.
      CALL WDialogSelect(IDD_ViewPawley)
      IF (FnPatternOK() .AND. FnWavelengthOK()) THEN
        tMaxResolution = MAX(TwoTheta2dSpacing(XPMAX), DefaultMaxResolution)
        CALL WDialogPutReal(IDF_MaxResolution, tMaxResolution)
      ELSE
        tMaxResolution = DefaultMaxResolution
        CALL WDialogClearField(IDF_MaxResolution)
      ENDIF
      CALL WDialogSelect(IDD_PW_Page5)
! Initialise truncation of start of powder pattern
      CALL WDialogPutReal(IDF_Min2Theta, XPMIN, '(F6.3)')
      CALL WDialogPutReal(IDF_MaxResolution, tMaxResolution)
      CALL WDialogPutReal(IDF_Max2Theta, dSpacing2TwoTheta(tMaxResolution))
      CALL PopActiveWindowID

      END SUBROUTINE Update_TruncationLimits
!
!*****************************************************************************
!
