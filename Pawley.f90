!
!*****************************************************************************
!
      SUBROUTINE ShowPawleyFitWindow
!
! This routine pops up the window for the Pawley refinement
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'DialogPosCmn.inc'
      INCLUDE 'lattice.inc'
      INCLUDE 'statlog.inc'

      INTEGER PawleyErrorLog ! Function
      INTEGER IDUMMY
      INTEGER NTCycles

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Pawley_Status)
      CALL WDialogShow(IXPos_IDD_Wizard,IYPos_IDD_Wizard,0,Modeless)
      CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
      CALL WDialogFieldState(IDB_PawRef_Accept,Disabled)
      CALL WDialogFieldState(IDB_PawRef_Reject,Disabled)
      CALL WDialogFieldState(IDB_PawRef_Save,Disabled)
      CALL WDialogFieldState(IDF_PawRef_Solve,Disabled)
! If the background has been subtracted after the pattern was read in, then the
! order of the background polynomial defaults to 2, otherwise to 10.
      IF (.NOT. BACKREF) THEN
        CALL WDialogPutInteger(IDF_IDF_PawRef_NBack,2)
      ELSE
        CALL WDialogPutInteger(IDF_IDF_PawRef_NBack,10)
      ENDIF
      CALL WDialogClearField(IDF_Pawley_Cycle_Number)
      CALL WDialogClearField(IDF_Pawley_Refinement_Number)
      IDUMMY = PawleyErrorLog(2) ! Reset the log messages
      NumPawleyRef = 0
      CALL WDialogFieldState(IDF_PawRef_UseInts_Check,Disabled)
      CALL WDialogFieldState(IDF_PawRef_RefSigm1_Check,Disabled)
      CALL WDialogFieldState(IDF_PawRef_RefSigm2_Check,Disabled)
      CALL WDialogFieldState(IDF_PawRef_RefGamm1_Check,Disabled)
      CALL WDialogFieldState(IDF_PawRef_RefGamm2_Check,Disabled)
      CALL WDialogPutCheckBox(IDF_PawRef_UseInts_Check,Unchecked)
      CALL WDialogPutCheckBox(IDF_PawRef_RefBack_Check,Checked)
      CALL WDialogPutCheckBox(IDF_PawRef_RefCell_Check,Unchecked)
      CALL WDialogPutCheckBox(IDF_PawRef_RefZero_Check,Unchecked)
      CALL WDialogPutCheckBox(IDF_PawRef_RefSigm1_Check,Unchecked)
      CALL WDialogPutCheckBox(IDF_PawRef_RefSigm2_Check,Unchecked)
      CALL WDialogPutCheckBox(IDF_PawRef_RefGamm1_Check,Unchecked)
      CALL WDialogPutCheckBox(IDF_PawRef_RefGamm2_Check,Unchecked)
      NTCycles = 3
      CALL WDialogPutInteger(IDF_Pawley_Total_Cycles,NTCycles)
      CALL IOsDeleteFile('polyp.niw')
      CALL PopActiveWindowID
      PastPawley = .FALSE.

      END SUBROUTINE ShowPawleyFitWindow
!
!*****************************************************************************
!
      SUBROUTINE DealWithPawleyFitWindow

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'DialogPosCmn.inc'
      INCLUDE 'lattice.inc'
      INCLUDE 'statlog.inc'

      REAL            CELL,       V,     ORTH,        CPARS
      INTEGER                                                     KCPARS
      REAL                                                                   CELESD,        CELLSD
      INTEGER                                                                                            KOM4
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2), KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4

      REAL            ZEROSP
      INTEGER                        KZROSP
      REAL                                          DKDZER
      INTEGER                                                  NZERSP
      COMMON /PRZERO/ ZEROSP(6,9,5), KZROSP(6,9,5), DKDZER(6), NZERSP(9,5)

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      REAL              XPF_Range
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),                                   &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT)

      INTEGER         NPTS
      REAL                  ZARGI,        ZOBS,        ZDOBS,        ZWT
      INTEGER                                                                    ICODEZ
      REAL                                                                                      KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MPPTS), ZOBS(MPPTS), ZDOBS(MPPTS), ZWT(MPPTS), ICODEZ(MPPTS), KOBZ(MPPTS)

      REAL            ZCAL,        ZBAK
      COMMON /YSTORE/ ZCAL(MPPTS), ZBAK(MPPTS)

      REAL            ZXDELT
      INTEGER                 IIMIN, IIMAX
      REAL                                  XDIFT, XMINT
      COMMON /ZSTOR1/ ZXDELT, IIMIN, IIMAX, XDIFT, XMINT

! Save the boxes from Pawley fit to Pawley fit
      REAL RLastValues(3)
      INTEGER ILastValues(2)
      DATA RLastValues / 0.0,0.0,0.0 /
      DATA ILastValues / 0,0/
      SAVE RLastValues,ILastValues
! Local variables logging errors in the pawley fit
      INTEGER IDUMMY, ipt
      INTEGER PawleyErrorLog  
      INTEGER Quick_Pawley_Fit ! Function
      REAL    DEGREE ! Function
      INTEGER ieocc, II, JJ
      LOGICAL LastValuesSet
      SAVE    LastValuesSet
      LOGICAL SaveProject ! Function

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Pawley_Status)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL, IDCLOSE)
              CALL SelectMode(ID_Peak_Fitting_Mode)
              CALL EndWizardCommon
            CASE (IDF_PawRef_Refine)
              CALL WritePawleyRefinementFile
              ieocc  = Quick_Pawley_Fit()
              ipt = 0
              CALL WDialogPutProgressBar(IDF_Pawley_Progress_Bar,ipt,Absolute)
              SELECT CASE (ieocc)
                CASE (0,-2)
! An error occurred, so pop up a box to say so and then skip this refinement
                  CALL ErrorMessage("The refinement was unsuccessful!"//CHAR(13)//                  &
                                    "Possible causes could be too many peak parameters"//CHAR(13)// &
                                    "or bad data at high angles.")
! Reset the R-values if possible
                  IF (LastValuesSet) THEN
                    CALL WDialogPutReal(IDF_Pawley_Cycle_Rwp,RLastValues(1),'(F12.2)') 
                    CALL WDialogPutReal(IDF_Pawley_Cycle_ChiSq,RLastValues(2),'(F12.3)')
                    CALL WDialogPutReal(IDF_Pawley_Cycle_RwpExp,RLastValues(3),'(F12.2)')
                    CALL WDialogPutInteger(IDF_Pawley_Cycle_NumPts,ILastValues(1))
                    CALL WDialogPutInteger(IDF_Pawley_Cycle_NumRefs,ILastValues(2))
                    CALL retrieve_polybackup()
                  END IF
! JCC Need to back-copy the arrays here 
! Also decrement the number of Pawley refinements since it failed
                  NumPawleyRef = NumPawleyRef - 1
! We want to ignore the newly created .ccn and use the old .ccl, therefore, copy .ccl to .niw
                  CALL IOSCopyFile('polyp.ccl','polyp.niw')
                  CALL WDialogClearField(IDF_Pawley_Cycle_Number)
                  CALL WDialogClearField(IDF_Pawley_Refinement_Number)
                  IDUMMY = PawleyErrorLog(2) ! Reset the log messages
                  CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
                  CALL WDialogFieldState(IDB_PawRef_Accept,Disabled)
                  CALL WDialogFieldState(IDB_PawRef_Reject,Disabled)
                  IF (NumPawleyRef .GT. 0) THEN
                    CALL WDialogFieldState(IDB_PawRef_Save,Enabled)
                    CALL WDialogFieldState(IDF_PawRef_Solve,Enabled)
                  ENDIF
                  CALL PopActiveWindowID
                  RETURN
                CASE (-1)
                  NumPawleyRef = NumPawleyRef - 1
! Return to data viewing
                  CALL SelectMode(ID_Peak_Fitting_Mode)
                  CALL WDialogHide()
! This handles cases where the number of reflections is exceeded
                  CALL ErrorMessage("Sorry, can only Pawley refine a maximum of 400 reflections."//CHAR(13)// &
                                    "You must truncate your data set.")
                  CALL PopActiveWindowID
                  RETURN
                CASE DEFAULT
                  IF (PawleyErrorLog(2) .GT. 0) CALL PawleyWarning ! Check the log messages and reset
              END SELECT
              CALL WDialogFieldState(IDF_PawRef_Refine,Disabled)
              CALL WDialogFieldState(IDB_PawRef_Accept,Enabled)
              CALL WDialogFieldState(IDB_PawRef_Reject,Enabled)
              CALL WDialogFieldState(IDB_PawRef_Save,Disabled)
              CALL WDialogFieldState(IDF_PawRef_Solve,Disabled)
            CASE (IDB_PawRef_Accept)
! update the profile and stay with the Pawley refinement
! upload the cell constants and zeropoint from the Pawley refinement
              DO II=1,3
                CELLPAR(II)=CELL(II,1,1)
                JJ=II+3
                CELLPAR(JJ)=DEGREE(ACOS(CELL(II,2,1)))
              END DO
              CALL Upload_Cell_Constants()
              ZeroPoint = ZEROSP(1,1,1)
              CALL Upload_ZeroPoint() 
              CALL Generate_TicMarks()
! The CCSL code has written out a new input file for the next Pawley refinement--polyp.ccn
! As the user has accepted the fit, we can use this file to generate our new input file.
! To flag this to the subroutine, we create the file 'polyp.niw'
              CALL IOsCopyFile('polyp.ccn','polyp.niw')
              CALL Load_Pawley_Pro
! JCC Save the settings
              CALL WDialogSelect(IDD_Pawley_Status)
              CALL WDialogGetReal(IDF_Pawley_Cycle_Rwp,RLastValues(1)) 
              CALL WDialogGetReal(IDF_Pawley_Cycle_ChiSq,RLastValues(2))
              CALL WDialogGetReal(IDF_Pawley_Cycle_RwpExp,RLastValues(3))
              CALL WDialogGetInteger(IDF_Pawley_Cycle_NumPts,ILastValues(1))
              CALL WDialogGetInteger(IDF_Pawley_Cycle_NumRefs,ILastValues(2))
              LastValuesSet = .TRUE.
              CALL make_polybackup
! Set the file names to point to the poly files
              CALL set_saFileNames('polyp')
! Disable the Solve button until the user does a Save
              CALL WDialogFieldState(IDF_PawRef_Solve,Disabled)
              CALL WDialogSelect(IDD_Pawley_Status)
              IF (LastValuesSet) THEN
!              CALL WDialogFieldState(IDF_PawRef_Solve,Enabled)
                CALL WDialogFieldState(IDB_PawRef_Save,Enabled)
              END IF
              CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
              CALL WDialogFieldState(IDB_PawRef_Accept,Disabled)
              CALL WDialogFieldState(IDB_PawRef_Reject,Disabled)
              CALL SetModeMenuState(0,0,1)
! JCC Only change the setting if this is the second Pawley fit
      IF (NumPawleyRef .EQ. 1) THEN
        CALL WDialogFieldState(IDF_PawRef_UseInts_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefSigm1_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefSigm2_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefGamm1_Check,Enabled)
        CALL WDialogFieldState(IDF_PawRef_RefGamm2_Check,Enabled)
        CALL WDialogPutCheckBox(IDF_PawRef_UseInts_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefCell_Check,Checked)
        CALL WDialogPutCheckBox(IDF_PawRef_RefZero_Check,Checked)
        CALL WDialogPutInteger(IDF_Pawley_Total_Cycles,5)
      ENDIF
            CASE (IDB_PawRef_Reject)
              CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
! JCC Reset the R-values if possible
              IF (LastValuesSet) THEN
                CALL WDialogPutReal(IDF_Pawley_Cycle_Rwp,RLastValues(1),'(F12.2)') 
                CALL WDialogPutReal(IDF_Pawley_Cycle_ChiSq,RLastValues(2),'(F12.3)')
                CALL WDialogPutReal(IDF_Pawley_Cycle_RwpExp,RLastValues(3),'(F12.2)')
                CALL WDialogPutInteger(IDF_Pawley_Cycle_NumPts,ILastValues(1))
                CALL WDialogPutInteger(IDF_Pawley_Cycle_NumRefs,ILastValues(2))
                CALL retrieve_polybackup
                CALL WDialogFieldState(IDB_PawRef_Save,Enabled)
              END IF
              CALL WDialogFieldState(IDB_PawRef_Accept,Disabled)
              CALL WDialogFieldState(IDB_PawRef_Reject,Disabled)
            CASE (IDB_PawRef_Save)
              IF (SaveProject()) CALL WDialogFieldState(IDF_PawRef_Solve,Enabled)
            CASE (IDF_PawRef_Solve)
              CALL Load_Pawley_Pro
              IXPos_IDD_Wizard = WInfoDialog(6)
              IYPos_IDD_Wizard = WInfoDialog(7)
              CALL WDialogHide()
              FromPawleyFit = .TRUE.
              CALL Pawley_Limits_Save()
              PastPawley = .TRUE.
         !     CALL ShowWizardWindowZmatrices
              CALL SA_Main()
!.. Reload the Pawley profile ...
              CALL Pawley_Limits_Restore()
              CALL Load_Pawley_Pro
              SkipPawleyRef = .TRUE.
          END SELECT
        CASE (FieldChanged)
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithPawleyFitWindow')
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithPawleyFitWindow
!O!
!O!*****************************************************************************
!O!
!O      SUBROUTINE Quick_Pawley
!O
!O      USE WINTERACTER
!O      USE DRUID_HEADER
!O      USE VARIABLES
!O!
!O!   Type declarations
!O!
!O      INCLUDE 'PARAMS.INC'
!O      INCLUDE 'GLBVAR.INC'
!O      INCLUDE 'DialogPosCmn.inc'
!O      INCLUDE 'lattice.inc'
!O      INCLUDE 'statlog.inc'
!O
!O      INTEGER          NOBS
!O      REAL                         XOBS,       YOBS,        YCAL,        YBAK,        EOBS
!O      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS),  YCAL(MOBS),  YBAK(MOBS),  EOBS(MOBS)
!O
!O      INTEGER          NBIN, LBIN
!O      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
!O      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)
!O
!O      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
!O                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
!O                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
!O                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX
!O      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
!O                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
!O                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
!O                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX
!O      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD
!O      INTEGER ieocc
!O      INTEGER Quick_Pawley_Fit
!O
!O!.. CCSL common blocks included - take care!
!O      COMMON /CELPAR/CELL(3,3,2),V(2),ORTH(3,3,2),CPARS(6,2),KCPARS(6),CELESD(6,6,2),CELLSD(6,6),KOM4
!O      COMMON /PRZERO/ZEROSP(6,9,5),KZROSP(6,9,5),DKDZER(6),NZERSP(9,5)
!O      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
!O      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP
!O
!O      INTEGER CurrentRange 
!O      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),                          &
!O        IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),                                &
!O        NumPeakFitRange,CurrentRange,                                     &
!O        IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR),                           &
!O        XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR),            &
!O        IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)
!O
!O      COMMON /ZSTORE/ NPTS,ZARGI(MPPTS),ZOBS(MPPTS),ZDOBS(MPPTS),       &
!O        ZWT(MPPTS),ICODEZ(MPPTS),KOBZ(MPPTS)
!O      COMMON /YSTORE/ ZCAL(MPPTS),ZBAK(MPPTS)
!O      COMMON /ZSTOR1/ ZXDELT,IIMIN,IIMAX,XDIFT,XMINT
!O
!O! Save the boxes from Pawley fit to Pawley fit
!O      REAL RLastValues(3)
!O      INTEGER ILastValues(2)
!O      DATA RLastValues / 0.0,0.0,0.0 /
!O      DATA ILastValues / 0,0/
!O      SAVE RLastValues,ILastValues
!O! Local variables logging errors in the pawley fit
!O      INTEGER IDUMMY
!O      INTEGER PawleyErrorLog  
!O
!O      CALL WDialogSelect(IDD_Pawley_Status)
!O      CALL WDialogShow(IXPos_IDD_Pawley_Status,IYPos_IDD_Pawley_Status,0,Modeless)
!O      CALL IOsDeleteFile('polyp.niw')
!O      CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
!O      CALL WDialogFieldState(IDCLOSE,Enabled)
!O      IF (.NOT. BACKREF) THEN
!O        CALL WDialogPutInteger(IDF_IDF_PawRef_NBack,2)
!O      ELSE
!O        CALL WDialogPutInteger(IDF_IDF_PawRef_NBack,10)
!O      ENDIF
!O 555  CALL WDialogClearField(IDF_Pawley_Cycle_Number)
!O      CALL WDialogClearField(IDF_Pawley_Refinement_Number)
!O      IDUMMY = PawleyErrorLog(2) ! Reset the log messages
!O
!O      CALL Quick_Pawley_Preparation 
!O
!O      IF (SkipPawleyRef) THEN
!O!.. We don't want to stay in Pawley mode anymore - back to peak fitting mode
!O        CALL WDialogHide()
!O        RETURN
!O      ENDIF
!O      ieocc  = Quick_Pawley_Fit()
!O! Check for an error
!O      IF (ieocc .eq. 0 .OR. ieocc .eq. -2) THEN
!O! An error occurred, so pop up a box to say so and then skip this refinement
!O        CALL ErrorMessage("The refinement was unsuccessful!"//CHAR(13)//                  &
!O                          "Possible causes could be too many peak parameters"//CHAR(13)// &
!O                          "or bad data at high angles.")
!O! Reset the R-values if possible
!O        IF (LastValuesSet) THEN
!O          CALL WDialogPutReal(IDF_Pawley_Cycle_Rwp,RLastValues(1),'(f12.2)') 
!O          CALL WDialogPutReal(IDF_Pawley_Cycle_ChiSq,RLastValues(2),'(F12.3)')
!O          CALL WDialogPutReal(IDF_Pawley_Cycle_RwpExp,RLastValues(3),'(F12.2)')
!O          CALL WDialogPutInteger(IDF_Pawley_Cycle_NumPts,ILastValues(1))
!O          CALL WDialogPutInteger(IDF_Pawley_Cycle_NumRefs,ILastValues(2))
!O          CALL retrieve_polybackup()
!O        END IF
!O! JCC Need to back-copy the arrays here 
!O! Also decrement the number of Pawley refinements since it failed
!O        NumPawleyRef = NumPawleyRef - 1
!O! We want to ignore the newly created .ccn and use the old .ccl, therefore, copy .ccl to .niw
!O        CALL IOSCopyFile('polyp.ccl','polyp.niw')
!O        IF (InfoError(1) .EQ. 13) CALL DebugErrorMessage('cp polyp.ccl polyp.niw unsuccessful')
!O        GOTO 555
!O      ELSE IF (ieocc .eq. -1) THEN
!O        NumPawleyRef = NumPawleyRef - 1
!O! Return to data viewing
!O        CALL WDialogHide()
!O! This handles cases where the number of reflections is exceeded
!O        CALL ErrorMessage("Sorry, can only Pawley refine a maximum of 400 reflections."//CHAR(13)// &
!O                          "You must truncate your data set.")
!O        RETURN
!O      ELSE
!O        IF (PawleyErrorLog(2) .GT. 0) CALL PawleyWarning ! Check the log messages and reset
!O! Question - should we get the backup back here rather than allow users to continue
!O! with this?
!O      ENDIF
!O      ipt=0
!O      CALL WDialogPutProgressBar(IDF_Pawley_Progress_Bar,ipt,Absolute)
!O      CALL WDialogFieldState(IDF_PawRef_Refine,Disabled)
!O      CALL WDialogFieldState(IDCLOSE,Disabled)
!O      CALL WDialogFieldState(IDB_PawRef_Accept,Enabled)
!O      CALL WDialogFieldState(IDB_PawRef_Reject,Enabled)
!O      CALL WDialogFieldState(IDF_PawRef_Solve,Disabled)
!O      CALL WDialogFieldState(IDB_PawRef_Save,Disabled)
!O      DO WHILE (.TRUE.)
!O        CALL GetEvent
!O        SELECT CASE (EventType)
!O          CASE (PushButton)
!O            SELECT CASE (EventInfo%Value1)
!O              CASE (IDB_PawRef_Reject)
!O                CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
!O                CALL WDialogFieldState(IDCLOSE,Enabled)
!O! JCC Reset the R-values if possible
!O                IF (LastValuesSet) THEN
!O                  CALL WDialogPutReal(IDF_Pawley_Cycle_Rwp,RLastValues(1),'(F12.2)') 
!O                  CALL WDialogPutReal(IDF_Pawley_Cycle_ChiSq,RLastValues(2),'(F12.3)')
!O                  CALL WDialogPutReal(IDF_Pawley_Cycle_RwpExp,RLastValues(3),'(F12.2)')
!O                  CALL WDialogPutInteger(IDF_Pawley_Cycle_NumPts,ILastValues(1))
!O                  CALL WDialogPutInteger(IDF_Pawley_Cycle_NumRefs,ILastValues(2))
!O                  CALL retrieve_polybackup
!O                END IF
!O                GOTO 555
!O              CASE (IDB_PawRef_Accept)
!O! update the profile and stay with the Pawley refinement
!O! upload the cell constants and zeropoint from the Pawley refinement
!O                DO II=1,3
!O                  CELLPAR(II)=CELL(II,1,1)
!O                  JJ=II+3
!O                  CELLPAR(JJ)=DEGREE(ACOS(CELL(II,2,1)))
!O                END DO
!O                CALL Upload_Cell_Constants()
!O                ZeroPoint = ZEROSP(1,1,1)
!O                CALL Upload_ZeroPoint() 
!O                CALL Generate_TicMarks()
!O! The CCSL code has written out a new input file for the next Pawley refinement--polyp.ccn
!O! As the user has accepted the fit, we can use this file to generate our new input file.
!O! To flag this to the subroutine, we create the file 'polyp.niw'
!O                CALL IOsCopyFile('polyp.ccn','polyp.niw')
!O                CALL Load_Pawley_Pro
!O! JCC Save the settings
!O                CALL WDialogSelect(IDD_Pawley_Status)
!O                CALL WDialogGetReal(IDF_Pawley_Cycle_Rwp,RLastValues(1)) 
!O                CALL WDialogGetReal(IDF_Pawley_Cycle_ChiSq,RLastValues(2))
!O                CALL WDialogGetReal(IDF_Pawley_Cycle_RwpExp,RLastValues(3))
!O                CALL WDialogGetInteger(IDF_Pawley_Cycle_NumPts,ILastValues(1))
!O                CALL WDialogGetInteger(IDF_Pawley_Cycle_NumRefs,ILastValues(2))
!O                LastValuesSet = .TRUE.
!O                CALL make_polybackup
!O! Set the file names to point to the poly files
!O                CALL set_saFileNames('polyp')
!O! Disable the Solve button until the user does a Save
!O                CALL WDialogFieldState(IDF_PawRef_Solve,Disabled)
!O                CALL WDialogSelect(IDD_Pawley_Status)
!O                IF (LastValuesSet) THEN
!O!                CALL WDialogFieldState(IDF_PawRef_Solve,Enabled)
!O                  CALL WDialogFieldState(IDB_PawRef_Save,Enabled)
!O                END IF
!O                CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
!O                CALL WDialogFieldState(IDCLOSE,Enabled)
!O                CALL SetModeMenuState(0,0,1)
!O                GOTO 555
!O            END SELECT
!O        END SELECT
!O      END DO
!O
!O      END SUBROUTINE Quick_Pawley
!O!
!O!*****************************************************************************
!O!
!O      SUBROUTINE Quick_Pawley_Preparation
!O
!O      USE WINTERACTER
!O      USE DRUID_HEADER
!O      USE VARIABLES
!O
!O      IMPLICIT NONE
!O
!O      INCLUDE 'params.inc'
!O      INCLUDE 'GLBVAR.INC'
!O      INCLUDE 'statlog.inc'
!O      INCLUDE 'DialogPosCmn.inc'
!O      INCLUDE 'Lattice.inc'
!O
!O      REAL              PkFnVal,                      PkFnEsd,                      &
!O                        PkFnCal,                                                    &
!O                        PkFnVarVal,                   PkFnVarEsd,                   &
!O                        PkAreaVal,                    PkAreaEsd,                    &
!O                        PkPosVal,                     PkPosEsd,                     &
!O                        PkPosAv
!O      COMMON /PEAKFIT2/ PkFnVal(MPkDes,Max_NPFR),     PkFnEsd(MPkDes,Max_NPFR),     &
!O                        PkFnCal(MPkDes,Max_NPFR),                                   &
!O                        PkFnVarVal(3,MPkDes),         PkFnVarEsd(3,MPkDes),         &
!O                        PkAreaVal(MAX_NPPR,MAX_NPFR), PkAreaEsd(MAX_NPPR,MAX_NPFR), &
!O                        PkPosVal(MAX_NPPR,MAX_NPFR),  PkPosEsd(MAX_NPPR,MAX_NPFR),  &
!O                        PkPosAv(MAX_NPFR)
!O      INTEGER msymmin
!O      PARAMETER (msymmin=10)
!O
!O      INTEGER            nsymmin
!O      REAL                        symmin
!O      CHARACTER*20                                     symline
!O      COMMON /symgencmn/ nsymmin, symmin(4,4,msymmin), symline(msymmin)
!O
!O      LOGICAL SaveProject
!O      INTEGER NTCycles
!O
!O!... Check what's happening in IDD_Pawley_Status
!O      CALL WDialogFieldState(IDB_PawRef_Accept,Disabled)
!O      CALL WDialogFieldState(IDB_PawRef_Reject,Disabled)
!O! Now check on which button was pressed ...
!O      IF (NumPawleyRef .EQ. 0) THEN
!O        CALL WDialogFieldState(IDF_PawRef_UseInts_Check,Disabled)
!O        CALL WDialogFieldState(IDF_PawRef_RefBack_Check,Enabled)
!O        CALL WDialogFieldState(IDF_PawRef_RefSigm1_Check,Disabled)
!O        CALL WDialogFieldState(IDF_PawRef_RefSigm2_Check,Disabled)
!O        CALL WDialogFieldState(IDF_PawRef_RefGamm1_Check,Disabled)
!O        CALL WDialogFieldState(IDF_PawRef_RefGamm2_Check,Disabled)
!O        CALL WDialogPutCheckBox(IDF_PawRef_UseInts_Check,Unchecked)
!O        CALL WDialogPutCheckBox(IDF_PawRef_RefBack_Check,Checked)
!O        CALL WDialogPutCheckBox(IDF_PawRef_RefCell_Check,Unchecked)
!O        CALL WDialogPutCheckBox(IDF_PawRef_RefZero_Check,Unchecked)
!O        CALL WDialogPutCheckBox(IDF_PawRef_RefSigm1_Check,Unchecked)
!O        CALL WDialogPutCheckBox(IDF_PawRef_RefSigm2_Check,Unchecked)
!O        CALL WDialogPutCheckBox(IDF_PawRef_RefGamm1_Check,Unchecked)
!O        CALL WDialogPutCheckBox(IDF_PawRef_RefGamm2_Check,Unchecked)
!O        NTCycles = 3
!O        CALL WDialogPutInteger(IDF_Pawley_Total_Cycles,NTCycles)
!O! JCC Only change the setting if this is the second Pawley fit
!O      ELSE IF (NumPawleyRef .EQ. 1) THEN
!O        CALL WDialogFieldState(IDF_PawRef_UseInts_Check,Enabled)
!O        CALL WDialogFieldState(IDF_PawRef_RefSigm1_Check,Enabled)
!O        CALL WDialogFieldState(IDF_PawRef_RefSigm2_Check,Enabled)
!O        CALL WDialogFieldState(IDF_PawRef_RefGamm1_Check,Enabled)
!O        CALL WDialogFieldState(IDF_PawRef_RefGamm2_Check,Enabled)
!O        CALL WDialogPutCheckBox(IDF_PawRef_UseInts_Check,Checked)
!O        CALL WDialogPutCheckBox(IDF_PawRef_RefCell_Check,Checked)
!O        CALL WDialogPutCheckBox(IDF_PawRef_RefZero_Check,Checked)
!O        NTCycles=5
!O        CALL WDialogPutInteger(IDF_Pawley_Total_Cycles,NTCycles)
!O      END IF
!O      CALL WDialogPutInteger(IDF_Pawley_Refinement_Number,NumPawleyRef)
!O      SkipPawleyRef = .FALSE.
!O      CALL delete_polybackup
!O      DO WHILE (.NOT. SkipPawleyRef)
!O        CALL GetEvent
!O        SELECT CASE (EventType)
!O          CASE (PushButton)
!O            SELECT CASE (EventInfo%Value1)
!O              CASE (IDF_PawRef_Refine)
!O                CALL WritePawleyRefinementFile
!O                RETURN
!O              CASE (IDCLOSE)
!O                CALL SelectMode(ID_Peak_Fitting_Mode)
!O                SkipPawleyRef = .TRUE.
!O                RETURN
!O              CASE (IDB_PawRef_Save)
!O                IF (SaveProject()) THEN
!O                  CALL WDialogFieldState(IDF_PawRef_Solve,Enabled)
!O                  CALL WDialogFieldState(IDB_PawRef_Save,Disabled)
!O                END IF
!O              CASE (IDF_PawRef_Solve)
!O                CALL Load_Pawley_Pro
!O                CALL WDialogHide()
!O                IXPos_IDD_Pawley_Status = WInfoDialog(6)
!O                IYPos_IDD_Pawley_Status = WInfoDialog(7)
!O                IXPos_IDD_SA_Input = IXPos_IDD_Pawley_Status
!O                IYPos_IDD_SA_Input = IYPos_IDD_Pawley_Status
!O                FromPawleyFit = .TRUE.
!O                CALL Pawley_Limits_Save()
!O                CALL SA_Main()
!O!.. Reload the Pawley profile ...
!O                CALL Pawley_Limits_Restore()
!O                CALL Load_Pawley_Pro
!O                SkipPawleyRef = .TRUE.
!O                RETURN
!O            END SELECT
!O        END SELECT
!O      END DO
!O
!O      END SUBROUTINE Quick_Pawley_Preparation
!
!*****************************************************************************
!
      SUBROUTINE WritePawleyRefinementFile

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'
      INCLUDE 'DialogPosCmn.inc'
      INCLUDE 'Lattice.inc'

      CHARACTER(LEN=80) :: BackStr

      REAL              PkFnVal,                      PkFnEsd,                      &
                        PkFnCal,                                                    &
                        PkFnVarVal,                   PkFnVarEsd,                   &
                        PkAreaVal,                    PkAreaEsd,                    &
                        PkPosVal,                     PkPosEsd,                     &
                        PkPosAv
      COMMON /PEAKFIT2/ PkFnVal(MPkDes,Max_NPFR),     PkFnEsd(MPkDes,Max_NPFR),     &
                        PkFnCal(MPkDes,Max_NPFR),                                   &
                        PkFnVarVal(3,MPkDes),         PkFnVarEsd(3,MPkDes),         &
                        PkAreaVal(MAX_NPPR,MAX_NPFR), PkAreaEsd(MAX_NPPR,MAX_NPFR), &
                        PkPosVal(MAX_NPPR,MAX_NPFR),  PkPosEsd(MAX_NPPR,MAX_NPFR),  &
                        PkPosAv(MAX_NPFR)

      INTEGER msymmin
      PARAMETER (msymmin = 10)
      INTEGER            nsymmin
      REAL                        symmin
      CHARACTER*20                                     symline
      COMMON /symgencmn/ nsymmin, symmin(4,4,msymmin), symline(msymmin)

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,        YCAL,        YBAK,        EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS),  YCAL(MOBS),  YBAK(MOBS),  EOBS(MOBS)
      
      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)
      
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

      INTEGER          NTIC
      INTEGER                IH
      REAL                               ARGK
      REAL                                           DSTAR
      COMMON /PROFTIC/ NTIC, IH(3,MTIC), ARGK(MTIC), DSTAR(MTIC)

      REAL XRANMIN, XRANMAX
      SAVE XRANMIN, XRANMAX
      INTEGER NPawBack

      INTEGER NPawBack_OLD
      SAVE    NPawBack_OLD ! To test if number of background parameters has changed

      CHARACTER*4 ChRadOption(4)
      DATA CHRADOPTION /'LABX','SYNX','SYNX','TOFN'/

      INTEGER I
      LOGICAL FnUnitCellOK ! Function
      LOGICAL FnWaveLengthOK ! Function
      REAL    WavelengthOf ! Function
      INTEGER NTCycles
      INTEGER JNB, NBLIN, INB, ITEM, ISYM, IRTYP, ISIGM1, ISIGM2, IGAMM1, IGAMM2
      INTEGER N1, N2, K1, KNB
      INTEGER tFileHandle
      LOGICAL UsePrevious, FirstVaryLine
      INTEGER nl
      CHARACTER*255 Line

! Are these checks in place here? If one of them fails, we shouldn't have been here in the first place.
!
!.. We should only proceed with this if we have good cell constants 
!.. If no wavelength then assume Cu Ka1 wvln=1.54051
!..
!.. Write out the data file ...
!.. We should check if there are data to write out!
      IF (NBIN .LE. 0) RETURN
      IF (.NOT. FnUnitCellOK()) RETURN
      IF (NTIC .EQ. 0) RETURN
!.. Allow a maximum of 300 reflections
! JvdS Why?
      IF (NTIC .GT. 300) THEN
        xranmax = MIN(xpmax,ARGK(300))
      ELSE
        xranmax = xpmax
      ENDIF
      xranmin = xpmin
! JCC Original code - seems to cause the reflection loss bug
!        xranmax=xbin(nbin)

! Substituting with this line seems to fix this bug? Is this a reasonable fix?
! JvdS @ Doesn't this undo all of the above range checking?
! JvdS if there >300 reflections / tic marks, all of them will be included this way?
      xranmax = xpmax
      IF (NumInternalDSC .NE. DataSetChange) THEN
        tFileHandle = 41
        OPEN(tFileHandle,file='polyp.dat',status='unknown')
        DO I = 1, NBIN
          IF (XBIN(I) .GT. xranmax) GOTO 4110
          WRITE(tFileHandle,'(F10.4,F12.2,F12.2)') XBIN(I), YOBIN(I), EBIN(I)
        ENDDO
 4110   CLOSE(tFileHandle)
      ENDIF
      NumInternalDSC = DataSetChange
      NumPawleyRef = NumPawleyRef + 1
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Pawley_Status)
      CALL WDialogGetInteger(IDF_IDF_PawRef_NBack,NPawBack)
      CALL WDialogGetInteger(IDF_Pawley_Total_Cycles,NTCycles)    
      INQUIRE(FILE = 'polyp.niw', exist=UsePrevious)
      IF (UsePrevious) THEN
        OPEN(42,file='polyp.ccl',status='unknown')
        OPEN(43,file='polyp.niw',status='old')
        FirstVaryLine = .TRUE.
   10   READ(43,5300,END=900) nl, line
 5300   FORMAT(Q,A)
        SELECT CASE (line(1:1))
          CASE ('I')
            WRITE(42,4240) NTCycles
 4240       FORMAT('I NCYC ',I3,' PRCV 14 MCOR 0 FRIE 1 PRPR 0')
          CASE ('L')
            SELECT CASE (line(3:6))
              CASE('RTYP')
                CALL WDialogGetCheckBox(IDF_PawRef_UseInts_Check,Item)
                IRtyp = 2 - Item
                WRITE(42,4245) IRTYP, xranmin, xranmax
 4245           FORMAT('L RTYP  'I3,2F10.3,'  0.001')
              CASE ('VARY')
                IF (FirstVaryLine) THEN
                  WRITE(42,'(A)') 'L VARY ONLY ALL INTS'
                  CALL WDialogGetCheckBox(IDF_PawRef_RefBack_Check,Item)
                  IF (Item .EQ. 1) WRITE(42,'(A)') 'L VARY ALL BACK '
                  CALL WDialogGetCheckBox(IDF_PawRef_RefCell_Check,Item)
                  IF (Item .EQ. 1) WRITE(42,'(A)') 'L VARY ALL CELL '
                  CALL WDialogGetCheckBox(IDF_PawRef_RefZero_Check,Item)
                  IF (Item .EQ. 1) WRITE(42,'(A)') 'L VARY ZERO 1 '
                  CALL WDialogGetCheckBox(IDF_PawRef_RefSigm1_Check,ISigm1)
                  CALL WDialogGetCheckBox(IDF_PawRef_RefSigm2_Check,ISigm2)
                  CALL WDialogGetCheckBox(IDF_PawRef_RefGamm1_Check,IGamm1)
                  CALL WDialogGetCheckBox(IDF_PawRef_RefGamm2_Check,IGamm2)
                  IF (ISigm1.EQ.1) WRITE(42,'(A)') 'L VARY SIGM 1'
                  IF (ISigm2.EQ.1) WRITE(42,'(A)') 'L VARY SIGM 2'
                  IF (IGamm1.EQ.1) WRITE(42,'(A)') 'L VARY GAMM 1'
                  IF (IGamm2.EQ.1) WRITE(42,'(A)') 'L VARY GAMM 2'
                END IF
                FirstVaryLine = .FALSE.
              CASE ('BACK')
                CALL WDialogGetCheckBox(IDF_PawRef_RefBack_Check,Item)
! If the background should not be varied, or the number of parameters hasn't changed,
! just copy the old lines.
                IF ((Item.EQ.0) .OR. (NPawBack.EQ.NPawBack_OLD)) THEN
                  WRITE(42,'(A)') line(1:nl)              
                ELSE
! Otherwise, set all back ground parameters to zero
! Read all old background lines
                  nblin = 1 + (NPawBack_OLD-1)/5
                  IF (nblin .GT. 1) THEN
                    DO inb = 2, nblin ! we have already read the first line
                      READ(43,5300,END=900) nl, line
                    ENDDO
                  ENDIF
! Write all zeros for the new background
                  nblin = 1 + (NPawBack-1)/5
                  DO inb = 1, nblin
                    n1 = 5*(inb-1)
                    n2 = MIN(n1+5,NPawBack)-n1
                    backstr = 'L BACK 2'
                    knb = 7
                    IF (inb.EQ.1) knb=9
                    DO jnb = 1, n2
                      k1 = knb + 12*(jnb-1)
                      backstr(k1:k1+11) = '      0.000'
                    ENDDO
                    WRITE(42,'(A)') backstr
                  ENDDO
                  NPawBack_OLD = NPawBack
                ENDIF
              CASE DEFAULT
                WRITE(42,'(A)') line(1:nl)              
            END SELECT
          CASE DEFAULT
            WRITE(42,'(A)') line(1:nl)
        END SELECT
        GOTO 10
 900    CLOSE(42)
        CLOSE(43)
        CALL IOsDeleteFile('polyp.niw')
! The file we have just created will remain the new input file until either
! a. the user starts the Pawley refinement from scratch (i.e. exits and re-opens the dialogue)
! b. the user presses 'Accept' to accept the refined parameters
        CALL IOsCopyFile('polyp.ccl', 'polyp.niw')
        CALL PopActiveWindowID
      ELSE
        CALL WMenuSetState(ID_Pawley_Refinement_Mode,ItemEnabled,WintOn)
        tFileHandle = 41
        OPEN(tFileHandle,FILE='polyp.ccl',status='unknown')
        WRITE(tFileHandle,4210) 
 4210 FORMAT('N Polyfitter file for quick Pawley refinement')
        WRITE(tFileHandle,4220) (CellPar(I),I=1,6)
 4220 FORMAT('C ',3F10.5,3F10.3)
        WRITE(tFileHandle,4230) 
 4230 FORMAT('F C 2 2.31 20.8439 1.02 10.2075 1.5886 0.5687 0.865 51.6512 .2156'/'A C1 0 0 0 0') 
        IF (NumberSGTable .GE. 1) THEN
          CALL DecodeSGSymbol(SGShmStr(NumberSGTable))
          IF (nsymmin .GT. 0) THEN
            DO isym = 1, nsymmin
              WRITE(tFileHandle,4235) symline(isym)
 4235         FORMAT('S ',a)
            ENDDO
          ENDIF
        ENDIF
        WRITE(tFileHandle,4241) NTCycles, ChRadOption(JRadOption)
 4241   FORMAT('I NCYC ',I3,' PRCV 14 MCOR 0 FRIE 1 PRPR 0'/              &
        'L REFI PAWL'/                                                    &
        'L SORC ', A4/                                                    &
        'L WGHT 3')
        CALL WDialogGetCheckBox(IDF_PawRef_UseInts_Check,Item)
        IRtyp = 2 - Item
        WRITE(tFileHandle,4246) IRTYP, xranmin, xranmax
 4246   FORMAT('L RTYP  'I3,2F10.3,'  0.001')
        IF (.NOT. FnWaveLengthOK()) ALambda = WavelengthOf('Cu')
        WRITE(tFileHandle,4250) ALambda
 4250   FORMAT('L WVLN ',F10.5)
        IF ((ZeroPoint .LT. -1.0) .OR. (ZeroPoint .GT. 1.0)) ZeroPoint = 0.0
        WRITE(tFileHandle,4260) ZeroPoint
 4260   FORMAT('L ZERO ',F10.5)
!        WRITE(tFileHandle,"('L EXCL ',F7.3,1X,F7.3)") 0.5, 43.0
        CALL WDialogGetReal(IDF_Slim_Parameter,SLIMVALUE)
        WRITE(tFileHandle,4270) SCALFAC, SLIMVALUE
 4270   FORMAT('L SCAL   ',F7.5,/                                         &
     &  'L SLIM ',F5.2,' '/                                               &
     &  'L REFK 10.0'/                                                    &
     &  'L PKCN TYPE 1'/                                                  &
     &  'L PKFN TYPE 3'/                                                  &
     &  'L PKFN LIMS 0.005')
        WRITE(tFileHandle,4271) PkFnVarVal(1,1), PkFnVarVal(2,1)
        WRITE(tFileHandle,4272) PkFnVarVal(1,2), PkFnVarVal(2,2)
        WRITE(tFileHandle,4273) PkFnVarVal(1,3)
        WRITE(tFileHandle,4274) PkFnVarVal(1,4)
 4271   FORMAT('L PKFN SIGM ',2F8.4)
 4272   FORMAT('L PKFN GAMM ',2F8.4)
 4273   FORMAT('L PKFN HPSL ',F8.4)
 4274   FORMAT('L PKFN HMSL ',F8.4)
        IF (.NOT. BACKREF) THEN
          NPawBack =  2
        ELSE
          NPawBack = 10
        ENDIF
        NPawBack_OLD = NPawBack
        nblin = 1 + (NPawBack-1)/5
        DO inb = 1, nblin
          n1 = 5*(inb-1)
          n2 = MIN(n1+5,NPawBack)-n1
          backstr = 'L BACK 2'
          knb = 7
          IF (inb.EQ.1) knb=9
          DO jnb = 1, n2
            k1 = knb + 12*(jnb-1)
            backstr(k1:k1+11) = '      0.000'
          ENDDO
          WRITE(tFileHandle,'(A)') backstr
        ENDDO
        WRITE(tFileHandle,'(A)') 'L VARY ONLY ALL INTS'
        CALL WDialogGetCheckBox(IDF_PawRef_RefBack_Check,Item)
        IF (Item .EQ. 1) WRITE(tFileHandle,'(A)') 'L VARY ALL BACK '
        CALL WDialogGetCheckBox(IDF_PawRef_RefCell_Check,Item)
        IF (Item .EQ. 1) WRITE(tFileHandle,'(A)') 'L VARY ALL CELL '
        CALL WDialogGetCheckBox(IDF_PawRef_RefZero_Check,Item)
        IF (Item .EQ. 1) WRITE(tFileHandle,'(A)') 'L VARY ZERO 1 '
        CALL WDialogGetCheckBox(IDF_PawRef_RefSigm1_Check,ISigm1)
        CALL WDialogGetCheckBox(IDF_PawRef_RefSigm2_Check,ISigm2)
        CALL WDialogGetCheckBox(IDF_PawRef_RefGamm1_Check,IGamm1)
        CALL WDialogGetCheckBox(IDF_PawRef_RefGamm2_Check,IGamm2)
        IF (ISigm1.EQ.1) WRITE(tFileHandle,'(A)') 'L VARY SIGM 1'
        IF (ISigm2.EQ.1) WRITE(tFileHandle,'(A)') 'L VARY SIGM 2'
        IF (IGamm1.EQ.1) WRITE(tFileHandle,'(A)') 'L VARY GAMM 1'
        IF (IGamm2.EQ.1) WRITE(tFileHandle,'(A)') 'L VARY GAMM 2'
        CLOSE(tFileHandle)   
      ENDIF   

      END SUBROUTINE WritePawleyRefinementFile
!
!*****************************************************************************
!
      INTEGER FUNCTION Quick_Pawley_Fit

      USE WINTERACTER

! DIMENSION OF ALSQ BELOW, AND SETTING OF MATSZ, TO BE ALTERED TO BE SOMETHING
! A LITTLE LARGER THAN N*(N+3)/2 WHERE THERE WILL BE N BASIC VARIABLES

      INCLUDE 'PARAMS.INC'
      
      EXTERNAL PCCN01,PFCN03,DUMMY,CALPR
      COMMON /GLOBAL/NINIT,NBATCH,NSYSTM,MULFAS,MULSOU,MULONE
      DIMENSION ALSQ(QPFDIM)
      COMMON /CARDRC/ICRYDA,NTOTAL(9),NYZ,NTOTL,INREA(26,9),ICDN(26,9),IERR,IO10,SDREAD
      LOGICAL SDREAD
      COMMON/iounit/lpt,iti,ito,iplo,luni,iout
      INTEGER matsz
      CHARACTER*6 xxx
      CHARACTER*10 fname
      INTEGER FORTY

      fname='polyp'
      xxx='CN11LS'
      MATSZ=QPFDIM
      NINIT=1
! JCC trap the return status
      CALL make_polybackup ! make a backup of the polyp files
      Quick_Pawley_Fit = FORTY(xxx,ALSQ,MATSZ,PCCN01,PFCN03,DUMMY,CALPR,fname)
! JCC Trap for an error on file opening
      IF (ICRYDA .NE. -1) CALL CLOFIL(ICRYDA)
      IF (IO10 .NE. -1)   CALL CLOFIL(IO10)
      CALL CLOFIL(lpt)

      END FUNCTION Quick_Pawley_Fit
!
!*****************************************************************************
!
      SUBROUTINE Load_Pawley_PRO

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,        YCAL,        YBAK,        EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS),  YCAL(MOBS),  YBAK(MOBS),  EOBS(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER         NPTS
      REAL                  ZARGI,        ZOBS,        ZDOBS,        ZWT
      INTEGER                                                                    ICODEZ
      REAL                                                                                      KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MPPTS), ZOBS(MPPTS), ZDOBS(MPPTS), ZWT(MPPTS), ICODEZ(MPPTS), KOBZ(MPPTS)

      REAL            ZCAL,        ZBAK
      COMMON /YSTORE/ ZCAL(MPPTS), ZBAK(MPPTS)

      INTEGER I

      NOBS = NPTS
      NBIN = NPTS
      DO I = 1, NBIN
        XBIN(I)  = ZARGI(I)
        YOBIN(I) = ZOBS(I)
        YCBIN(I) = ZCAL(I)
        YBBIN(I) = ZBAK(I)
        EBIN(I)  = ZDOBS(I)
        XOBS(I)  = ZARGI(I)
        YOBS(I)  = ZOBS(I)
        YCAL(I)  = ZCAL(I)
        YBAK(I)  = ZBAK(I)
        EOBS(I)  = ZDOBS(I)
      ENDDO
      IPTYPE = 2
      CALL Profile_Plot(IPTYPE)

      END SUBROUTINE Load_Pawley_PRO
!
!*****************************************************************************
!
      SUBROUTINE PAWLEY_LIMITS_SAVE

      INCLUDE 'PARAMS.INC'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX

      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      COMMON /PAWPROFRAN/ XPPMIN,XPPMAX,YPPMIN,YPPMAX,                  &
     &XPPGMIN,XPPGMAX,                                                  &
     &YPPGMIN,YPPGMAX,XPPGMINOLD,XPPGMAXOLD,YPPGMINOLD,YPPGMAXOLD
      COMMON /PAWPROFIPM/ IPPMIN,IPPMAX,IPPMINOLD,IPPMAXOLD
      COMMON /PAWPROFMOR/ NPPFR 
    
      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),                          &
     &IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange,                &
     &CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR),              &
     &XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR),            &
     &IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)    

      NPPFR=NumPeakFitRange
      NumPeakFitRange=0
      XPPMIN=XPMIN
      XPPMAX=XPMAX
      YPPMIN=YPMIN
      YPPMAX=YPMAX
      XPPGMIN=XPGMIN
      XPPGMAX=XPGMAX
      YPPGMIN=YPGMIN
      YPPGMAX=YPGMAX

      XPPGMINOLD=XPGMINOLD
      XPPGMAXOLD=XPGMAXOLD
      YPPGMINOLD=YPGMINOLD
      YPPGMAXOLD=YPGMAXOLD
      IPPMIN=IPMIN
      IPPMAX=IPMAX
      IPPMINOLD=IPMINOLD
      IPPMAXOLD=IPMAXOLD

      END SUBROUTINE PAWLEY_LIMITS_SAVE
!
!*****************************************************************************
!
      SUBROUTINE PAWLEY_LIMITS_RESTORE

      INCLUDE 'PARAMS.INC'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX

      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX
      COMMON /PROFIPM/ IPMIN,IPMAX,IPMINOLD,IPMAXOLD

      COMMON /PAWPROFRAN/ XPPMIN,XPPMAX,YPPMIN,YPPMAX,                  &
     &XPPGMIN,XPPGMAX,                                                  &
     &YPPGMIN,YPPGMAX,XPPGMINOLD,XPPGMAXOLD,YPPGMINOLD,YPPGMAXOLD
      COMMON /PAWPROFIPM/ IPPMIN,IPPMAX,IPPMINOLD,IPPMAXOLD
      COMMON /PAWPROFMOR/ NPPFR 
   
      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),                          &
     &IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange,                &
     &CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR),              &
     &XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR),            &
     &IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)      

      NumPeakFitRange=NPPFR

      XPMIN=XPPMIN
      XPMAX=XPPMAX
      YPMIN=YPPMIN
      YPMAX=YPPMAX
      XPGMIN=XPPGMIN
      XPGMAX=XPPGMAX
      YPGMIN=YPPGMIN
      YPGMAX=YPPGMAX

      XPGMINOLD=XPPGMINOLD
      XPGMAXOLD=XPPGMAXOLD
      YPGMINOLD=YPPGMINOLD
      YPGMAXOLD=YPPGMAXOLD
      IPMIN=IPPMIN
      IPMAX=IPPMAX
      IPMINOLD=IPPMINOLD
      IPMAXOLD=IPPMAXOLD

      END SUBROUTINE PAWLEY_LIMITS_RESTORE
!
!*****************************************************************************
!
      SUBROUTINE make_polybackup

      USE WINTERACTER

      LOGICAL copypik, copytic, copyhcv, copyhkl
      COMMON / PBCKUP / copypik, copytic, copyhcv, copyhkl

      copypik = .FALSE.
      copytic = .FALSE.
      copyhcv = .FALSE.

! Make a backup copy of the polyp.pik file to recover in event of an error
      INQUIRE(FILE = 'polyp.pik', exist=copypik)
      inferr = InfoError(1)
      IF (copypik) THEN
        CALL IOsCopyFile('polyp.pik', 'polyp.pbk')
        inferr = InfoError(1)
        IF (inferr .NE. 0) copypik = .FALSE.
      END IF
      INQUIRE(FILE = 'polyp.tic', exist=copytic)
      inferr = InfoError(1)
      IF (copytic) THEN
        CALL IOsCopyFile('polyp.tic', 'polyp.tbk')
        inferr = InfoError(1)
        IF (inferr .NE. 0) copytic = .FALSE.
      END IF
      INQUIRE(FILE = 'polyp.hcv', exist=copyhcv)
      inferr = InfoError(1)
      IF (copyhcv) THEN
        CALL IOsCopyFile('polyp.hcv', 'polyp.hbk')
        inferr = InfoError(1)
        if (inferr .NE. 0) copyhcv = .FALSE.
      END IF
      INQUIRE(FILE = 'polyp.hkl', exist=copyhkl)
      inferr = InfoError(1)
      IF (copyhkl) THEN
        CALL IOsCopyFile('polyp.hkl', 'polyp.hbl')
        inferr = InfoError(1)
        if (inferr .NE. 0) copyhkl = .FALSE.
      END IF

      END SUBROUTINE make_polybackup
!
!*****************************************************************************
!
      SUBROUTINE retrieve_polybackup

      LOGICAL copypik, copytic, copyhcv, copyhkl
      COMMON / PBCKUP / copypik, copytic, copyhcv, copyhkl

      IF (copypik) CALL IOsCopyFile('polyp.pbk','polyp.pik')
      IF (InfoError(1) .NE. 0) CALL DebugErrorMessage('cp polyp.pbk polyp.pik unsuccessful')
      IF (copytic) CALL IOsCopyFile('polyp.tbk','polyp.tic')
      IF (InfoError(1) .NE. 0) CALL DebugErrorMessage('cp polyp.tbk polyp.tic unsuccessful')
      IF (copyhcv) CALL IOsCopyFile('polyp.hbk','polyp.hcv')
      IF (InfoError(1) .NE. 0) CALL DebugErrorMessage('cp polyp.hbk polyp.hcv unsuccessful')
      IF (copyhkl) CALL IOsCopyFile('polyp.hbl','polyp.hkl')
      IF (InfoError(1) .NE. 0) CALL DebugErrorMessage('cp polyp.hbl polyp.hkl unsuccessful')

      END SUBROUTINE retrieve_polybackup
!
!*****************************************************************************
!
      SUBROUTINE delete_polybackup

      LOGICAL copypik, copytic, copyhcv, copyhkl
      COMMON / PBCKUP / copypik, copytic, copyhcv, copyhkl

      IF (copypik) CALL IOsDeleteFile('polyp.pbk')
      IF (copytic) CALL IOsDeleteFile('polyp.tbk')
      IF (copyhcv) CALL IOsDeleteFile('polyp.hbk')
      IF (copyhkl) CALL IOsDeleteFile('polyp.hbl')
      copypik = .FALSE.
      copytic = .FALSE.
      copyhcv = .FALSE. 
      copyhkl = .FALSE. 

      END SUBROUTINE delete_polybackup
!
!*****************************************************************************
!
      SUBROUTINE set_saFileNames(base)

      USE VARIABLES

      CHARACTER*(*), INTENT (IN   ) :: base

      INTEGER n

      n = LEN_TRIM(base)
      IF (n .GT. 75) THEN
        CALL DebugErrorMessage('base too long in set_saFileNames')
        n = 75
      ENDIF
      WRITE(DashHcvFile,'(A,A)') base(1:n),'.hcv'
      WRITE(DashPikFile,'(A,A)') base(1:n),'.pik'
      WRITE(DashTicFile,'(A,A)') base(1:n),'.tic'

      END SUBROUTINE set_saFileNames
!
!*****************************************************************************
!
      SUBROUTINE CreateSDIFile(SDIFileName)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      CHARACTER*(*), INTENT (IN   ) :: SDIFileName

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'statlog.inc'

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP
!
!.. First copy the .pik .tic and .hcv files
!
      LSDI = LEN_TRIM(SDIFileName)
      IF (LSDI .GT. 80) THEN
        CALL DebugErrorMessage('SDIFileName too long in CreateSDIFile')
        LSDI = 80
      ENDIF
      IF (LSDI .EQ. 0) THEN
        CALL ErrorMessage('Filename not provided.'//CHAR(13)//'Try again!')
        RETURN
      ENDIF
	DashPikFile = ' '
      DashTicFile = ' '
      DashHcvFile = ' '
      DashDslFile = ' '
      IDot = 0
      DO I = LSDI, 1, -1
        IF (SDIFileName(I:I) .EQ. '.') THEN
          IDot = I
          GOTO 50
        END IF
      END DO
 50   DashPikFile(1:LSDI) = SDIFileName(1:LSDI)
      DashTicFile(1:LSDI) = SDIFileName(1:LSDI)
      DashHcvFile(1:LSDI) = SDIFileName(1:LSDI)
      DashDslFile(1:LSDI) = SDIFileName(1:LSDI)
      IF (IDot .EQ. 0) THEN
        L1 = LSDI + 1
        L4 = LSDI + 4
      ELSE
        L1 = LSDI - 3
        L4 = LSDI
      END IF
      DashPikFile(L1:L4)='.pik'
      DashTicFile(L1:L4)='.tic'
      DashHcvFile(L1:L4)='.hcv'
      DashDslFile(L1:L4)='.dsl'
      CALL WRTDSL(DashDslFile,L4) ! Ignore any errors
      CALL IOSCopyFile('polyp.pik',DashPikFile)
      CALL IOSCopyFile('polyp.tic',DashTicFile)
      CALL IOSCopyFile('polyp.hcv',DashHcvFile)
      OPEN(81,file=SDIFileName(1:LSDI),status='unknown')
      WRITE(81,8110) DashTicFile(1:LEN_TRIM(DashTicFile))
      WRITE(81,8120) DashHcvFile(1:LEN_TRIM(DashHcvFile))
      WRITE(81,8130) DashPikFile(1:LEN_TRIM(DashPikFile))
      WRITE(81,8136) DashRawFile(1:LEN_TRIM(DashRawFile))
      WRITE(81,8135) DashDslFile(1:LEN_TRIM(DashDslFile))
      WRITE(81,8140) (CellPar(I),I=1,6)
      WRITE(81,8150) NumberSGTable,SGNumStr(NumberSGTable),SGHMaStr(NumberSGTable)
      WRITE(81,8160) PAWLEYCHISQ
 8110 FORMAT(' TIC ',A)
 8120 FORMAT(' HCV ',A)
 8130 FORMAT(' PIK ',A)
 8135 FORMAT(' DSL ',A)
 8136 FORMAT(' RAW ',A)
 8140 FORMAT(' Cell ',3F10.5,3F10.4)
 8150 FORMAT(' SpaceGroup ',I4,4X,A12,A12)
 8160 FORMAT(' PawleyChiSq ',F10.2)
      CLOSE(81)

      END SUBROUTINE CreateSDIFile
!
!*****************************************************************************
!
      LOGICAL FUNCTION SaveProject

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      CHARACTER(LEN=80) :: SDIFileName
      CHARACTER(LEN=45) :: FILTER
      INTEGER IFLAGS
      
!.. Save the project
      SaveProject = .FALSE.
      IFLAGS = SaveDialog + AppendExt + PromptOn
      FILTER = 'Diffraction information files (*.sdi)|*.sdi|'
      SDIFileName = ' '
      CALL WSelectFile(FILTER,IFLAGS,SDIFileName,'Save diffraction information for structure solution')
      IF ((WinfoDialog(4) .EQ. CommonOk) .AND. (SDIFileName .NE. ' ')) THEN
        CALL CreateSDIFile(SDIFileName)
        CALL Set_saFileNames(SDIFileName(1:LEN_TRIM(SDIFileName) - 4))
        SaveProject = .TRUE.
      ENDIF

      END FUNCTION SaveProject
!
!*****************************************************************************
!
