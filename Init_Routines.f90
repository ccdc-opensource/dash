!
!*****************************************************************************
!
      SUBROUTINE Clear_Project

      USE PRJVAR

      IMPLICIT NONE

      CALL Clear_PeakFitRanges
      CALL Clear_UnitCell
      CALL Clear_Zmatrices
      PrjFileName = ''
! CALL UpdatePrjFileName('')
      CALL Clear_PO

      END SUBROUTINE Clear_Project
!
!*****************************************************************************
!
      SUBROUTINE Clear_PO

      USE WINTERACTER
      USE DRUID_HEADER
      USE PO_VAR

      IMPLICIT NONE

      PrefPars(1) = 0.0
      PrefPars(2) = 0.0
      PrefPars(3) = 1.0
      PrefPars(4) = 1.0
      PrefParExists = .FALSE.
      CALL Update_PO

      END SUBROUTINE Clear_PO
!
!*****************************************************************************
!
      SUBROUTINE Update_PO

      USE WINTERACTER
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
      INCLUDE 'GLBVAR.INC'

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT)

! Clear all variables
      NumPeakFitRange = 0
! Update 'View'|'Peak Positions'...
      CALL Upload_Positions
!... and 'View'|'Peak Widths' tabs
      CALL Upload_Widths
! Signal to Pawley refinement that we should start with a clean slate
      CALL IOsDeleteFile('polyp.niw')
! Redraw
      CALL Profile_Plot
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

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       YBAK,        EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), YBAK(MOBS),  EOBS(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER I

      DO I = 1, NOBS
        YBAK(I) = 0.0
      ENDDO
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

      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      NumOf_SA_Runs = 0

      END SUBROUTINE Clear_SA
!
!*****************************************************************************
!
