!
!*****************************************************************************
!
      SUBROUTINE Init_PeakFitRanges

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

      INTEGER I

! Clear all variables
      CurrentRange = 0
      NumPeakFitRange = 0
      DO I = 1, MAX_NPFR
        NumInPFR(I) = 0
        IPF_RPt(I) = 0
      ENDDO
! Update 'View'|'Peak Positions'...
      CALL Upload_Positions()
!... and 'View'|'Peak Widths' tabs
      CALL Upload_Widths()
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

      END SUBROUTINE Init_PeakFitRanges
!
!*****************************************************************************
!
      SUBROUTINE Init_BackGround

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,        YCAL,        YBAK,        EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS),  YCAL(MOBS),  YBAK(MOBS),  EOBS(MOBS)

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

      END SUBROUTINE Init_BackGround
!
!*****************************************************************************
!
