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
      INCLUDE 'lattice.inc'
      INCLUDE 'statlog.inc'

      INTEGER         CurrentWizardWindow
      COMMON /Wizard/ CurrentWizardWindow

      INTEGER         PR_NumBack
      REAL                        PR_BackGround
      COMMON /PRBACK/ PR_NumBack, PR_BackGround(1:10)

      INTEGER, EXTERNAL :: PawleyErrorLog
      INTEGER IDUMMY

! JvdS This window popping up automatically makes it impossible to 
! play around with indexing. Therefore: when in one of the indexing windows, ignore
! this routine.
      IF ((CurrentWizardWindow .EQ. IDD_PW_Page7) .OR.       &
          (CurrentWizardWindow .EQ. IDD_PW_Page8) .OR.       &
          (CurrentWizardWindow .EQ. IDD_PW_Page9)) RETURN
      CALL SetModeMenuState(-1,1,0)
      CALL SelectMode(ID_Pawley_Refinement_Mode)
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Pawley_Status)
      IF (SpaceGroupDetermination) THEN
        CALL WDialogPutString(IDF_PawRef_Solve, 'Run>')
      ELSE
        CALL WDialogPutString(IDF_PawRef_Solve, '&Solve>')
      ENDIF
      CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
      CALL WDialogFieldState(IDB_PawRef_Accept,Disabled)
      CALL WDialogFieldState(IDB_PawRef_Reject,Disabled)
      CALL WDialogFieldState(IDB_PawRef_Save,Disabled)
      CALL WDialogFieldState(IDF_PawRef_Solve,Disabled)
!!ep added
!!      CALL WDialogFieldState(IDF_Paw_Ref_SgDet_Proceed,Disabled)
! If the background has been subtracted after the pattern was read in, then the
! order of the background polynomial defaults to 2, otherwise to 10.
      IF (.NOT. BACKREF) THEN
        PR_NumBack = 2
        CALL WDialogPutInteger(IDF_IDF_PawRef_NBack,PR_NumBack)
        CALL WDialogRangeInteger(IDF_IDF_PawRef_NBack,2,6)
      ELSE
        PR_NumBack = 10
        CALL WDialogPutInteger(IDF_IDF_PawRef_NBack,PR_NumBack)
        CALL WDialogRangeInteger(IDF_IDF_PawRef_NBack,2,10)
      ENDIF
      PR_BackGround = 0.0
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
      CALL WDialogPutInteger(IDF_Pawley_Total_Cycles,3)
      CALL WizardWindowShow(IDD_Pawley_Status)
      CALL PopActiveWindowID

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
      INCLUDE 'lattice.inc'
      INCLUDE 'statlog.inc'

      REAL            CELL,       V,     ORTH,        CPARS
      INTEGER                                                     KCPARS
      REAL                                                                   CELESD,        CELLSD
      INTEGER                                                                                            KOM4
      COMMON /CELPAR/ CELL(3,3,2), V(2), ORTH(3,3,2), CPARS(6,2), KCPARS(6), CELESD(6,6,2), CELLSD(6,6), KOM4

      REAL            ARGI, YNORM, PKFNSP
      INTEGER                                       KPFNSP
      REAL            DERPFN
      INTEGER                      NPKFSP
      REAL                                        TOLER
      INTEGER         NPKGEN
      REAL                         PKFNVA,    DYNDVQ,    DYNDKQ
      LOGICAL                                                    REFUSE
      LOGICAL         CYC1, NOPKRF
      REAL                          TOLR
      INTEGER                                  NFFT
      REAL                                           AKNOTS
      INTEGER         NBASF4,             L4END
      COMMON /PRPKFN/ ARGI, YNORM, PKFNSP(8,6,9,5), KPFNSP(8,6,9,5),     &
                      DERPFN(8,6), NPKFSP(8,9,5), TOLER(8,9,5),          &
                      NPKGEN(9,5), PKFNVA(8), DYNDVQ(8), DYNDKQ, REFUSE, &
                      CYC1, NOPKRF, TOLR(2,5), NFFT, AKNOTS,             &
                      NBASF4(MPRPKF,2,9), L4END(9)

      REAL            ZEROSP
      INTEGER                        KZROSP
      REAL                                          DKDZER
      INTEGER                                                  NZERSP
      COMMON /PRZERO/ ZEROSP(6,9,5), KZROSP(6,9,5), DKDZER(6), NZERSP(9,5)

      INTEGER         IBACK, NBACK
      REAL                             ARGBAK,        BACKGD
      INTEGER         KBCKGD,        NBK, LBKD
      LOGICAL                                       ZBAKIN
      COMMON /GRDBCK/ IBACK, NBACK(5), ARGBAK(100,5), BACKGD(100,5),    &
                      KBCKGD(100,5), NBK, LBKD(20), ZBAKIN

      INTEGER         PR_NumBack
      REAL                        PR_BackGround
      COMMON /PRBACK/ PR_NumBack, PR_BackGround(1:10)

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

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

      INTEGER         NPTS
      REAL                  ZARGI,       ZOBS,       ZDOBS,       ZWT
      INTEGER                                                                ICODEZ
      REAL                                                                                 KOBZ
      COMMON /ZSTORE/ NPTS, ZARGI(MOBS), ZOBS(MOBS), ZDOBS(MOBS), ZWT(MOBS), ICODEZ(MOBS), KOBZ(MOBS)

      REAL               PeakShapeSigma(1:2), PeakShapeGamma(1:2), PeakShapeHPSL, PeakShapeHMSL
      COMMON /PEAKFIT3/  PeakShapeSigma,      PeakShapeGamma,      PeakShapeHPSL, PeakShapeHMSL

! Save the boxes from Pawley fit to Pawley fit
      REAL RLastValues(3)
      INTEGER ILastValues(2)
      DATA RLastValues / 0.0,0.0,0.0 /
      DATA ILastValues / 0,0/
      SAVE RLastValues, ILastValues
! Local variables logging errors in the pawley fit
      INTEGER IDUMMY, ipt
      INTEGER PawleyErrorLog  
      INTEGER, EXTERNAL :: Quick_Pawley_Fit, GETTIC
      REAL, EXTERNAL :: DEGREE
      INTEGER ieocc, II, JJ
      LOGICAL LastValuesSet
      SAVE    LastValuesSet
      LOGICAL, EXTERNAL :: SaveProject
      INTEGER Ilen, IER
      CHARACTER(MaxPathLength) SDIFile
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      INTEGER Inum
      CHARACTER(MaxPathLength) :: CurrentDirectory
      LOGICAL Exists

      Exists = .FALSE.
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Pawley_Status)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              IF (SpaceGroupDetermination) THEN 
                CALL IosDirName(CurrentDirectory)
                INQUIRE(FILE=CurrentDirectory(1:LEN_TRIM(CurrentDirectory))//DIRSPACER//'table.asc',EXIST=exists)
                IF (exists) THEN
                  CALL SpaceGroupFileDialog
                ENDIF
              ENDIF
              IPTYPE = 1
              CALL Profile_Plot
              CALL WizardWindowShow(IDD_PW_Page1)
            CASE (IDCANCEL, IDCLOSE)
              SpaceGroupDetermination = .FALSE.
              CALL EndWizardCommon
            CASE (IDF_PawRef_Refine)
              CALL WritePawleyRefinementFile
              ieocc = Quick_Pawley_Fit()
              ipt = 0
              CALL WDialogPutProgressBar(IDF_Pawley_Progress_Bar,ipt,Absolute)
              CALL WDialogPutInteger(IDF_Pawley_Refinement_Number,NumPawleyRef)
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
                    CALL retrieve_polybackup
                  ENDIF
! JCC Need to back-copy the arrays here 
! Also decrement the number of Pawley refinements since it failed
                  NumPawleyRef = NumPawleyRef - 1
                  CALL WDialogPutInteger(IDF_Pawley_Refinement_Number,NumPawleyRef)
                  CALL WDialogClearField(IDF_Pawley_Cycle_Number)
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
                  CALL EndWizardCommon
! This handles cases where the number of reflections is exceeded
                  CALL ErrorMessage("Sorry, can only Pawley refine a maximum of 350 reflections."//CHAR(13)// &
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
! ep added
              IF (SpaceGroupDetermination) THEN
                CALL WDialogFieldState(IDF_PawRef_Solve, Disabled)
              ELSE
               CALL WDialogFieldState(IDF_PawRef_Solve,Enabled)
              ENDIF
            CASE (IDB_PawRef_Accept)
! update the profile and stay with the Pawley refinement
              IPTYPE = 2
! The new peak shape parameters are now in 
!        PKFNSP(L,I,JPHASE,JSOURC)
! L = 1 : 'SIGM', I = 1, 2
! L = 2 : 'GAMM', I = 1, 2
! L = 3 : 'HPSL', I = 1
! L = 4 : 'HMSL', I = 1
! JPHASE = 1, JSOURC = 1
              PeakShapeSigma(1) = PKFNSP(1,1,1,1)
              PeakShapeSigma(2) = PKFNSP(1,2,1,1)
              PeakShapeGamma(1) = PKFNSP(2,1,1,1)
              PeakShapeGamma(2) = PKFNSP(2,2,1,1)
              CALL WDialogSelect(IDD_ViewPawley)
              CALL WDialogPutReal(IDF_Sigma1,PeakShapeSigma(1),'(F10.4)')
              CALL WDialogPutReal(IDF_Sigma2,PeakShapeSigma(2),'(F10.4)')
              CALL WDialogPutReal(IDF_Gamma1,PeakShapeGamma(1),'(F10.4)')
              CALL WDialogPutReal(IDF_Gamma2,PeakShapeGamma(2),'(F10.4)')
!C              PeakShapeHPSL 
!C              PeakShapeHMSL
! Store new background parameters
              PR_NumBack = NBACK(1)
              PR_BackGround = 0.0
              DO II = 1, PR_NumBack
                PR_BackGround(II) = BACKGD(II,1)
              ENDDO
! upload the cell constants and zeropoint from the Pawley refinement
              DO II = 1, 3
                CELLPAR(II) = CELL(II,1,1)
                JJ = II + 3
                CELLPAR(JJ) = DEGREE(ACOS(CELL(II,2,1)))
              ENDDO
              CALL Upload_Cell_Constants
              ZeroPoint = ZEROSP(1,1,1)
              CALL Upload_ZeroPoint 
              CALL Generate_TicMarks
! JCC Save the settings
              CALL WDialogSelect(IDD_Pawley_Status)
              CALL WDialogGetReal(IDF_Pawley_Cycle_Rwp,RLastValues(1)) 
              CALL WDialogGetReal(IDF_Pawley_Cycle_ChiSq,RLastValues(2))
              CALL WDialogGetReal(IDF_Pawley_Cycle_RwpExp,RLastValues(3))
              CALL WDialogGetInteger(IDF_Pawley_Cycle_NumPts,ILastValues(1))
              CALL WDialogGetInteger(IDF_Pawley_Cycle_NumRefs,ILastValues(2))
              LastValuesSet = .TRUE.
              CALL make_polybackup
! Disable the Solve button until the user does a Save
              CALL WDialogFieldState(IDF_PawRef_Solve,Disabled)

              CALL WDialogFieldState(IDF_PawRef_Solve,Enabled)

              CALL WDialogSelect(IDD_Pawley_Status)
              IF (LastValuesSet) CALL WDialogFieldState(IDB_PawRef_Save,Enabled)
              CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
              CALL WDialogFieldState(IDB_PawRef_Accept,Disabled)
              CALL WDialogFieldState(IDB_PawRef_Reject,Disabled)
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
! If entered window by going through "Space Group>" route then do not have the option
! to save sdi file.  Can only perform Pawley Refinement then go back to cell parameters
! window
                IF(SpaceGroupDetermination) THEN 
                  CALL WDialogFieldState(IDB_PawRef_Save,Disabled)
                  CALL WDialogGetInteger(IDF_Pawley_Refinement_Number,Inum)
                    IF (Inum .LE. 1) THEN
                      CALL WDialogFieldState(IDF_PawRef_Solve, Disabled)
                    ELSE
! Have hit "Refine" at least twice so are now able to call Space Group Determination program
                      IF (Inum .gt. 1) THEN
!!ep                     CALL SpaceGroupDeterminationCode(LatBrav, RLastValues(2)) ! RlastValues(2) = Pawley chisqd
                       CALL WDialogFieldState(IDB_PawRef_Save,Disabled)
                       CALL WDialogFieldState(IDF_PawRef_Solve, Enabled)
                      END IF
                    END IF
                END IF
            CASE (IDB_PawRef_Reject)
              CALL WDialogFieldState(IDF_PawRef_Refine,Enabled)
! JCC Reset the R-values if possible
              IF (LastValuesSet) THEN
                CALL WDialogPutReal(IDF_Pawley_Cycle_Rwp,RLastValues(1),'(F12.2)')
                RWPOBS = RLastValues(1)
                CALL WDialogPutReal(IDF_Pawley_Cycle_ChiSq,RLastValues(2),'(F12.3)')
                PAWLEYCHISQ = RLastValues(2)
                CALL WDialogPutReal(IDF_Pawley_Cycle_RwpExp,RLastValues(3),'(F12.2)')
                RWPEXP = RLastValues(3)
                CALL WDialogPutInteger(IDF_Pawley_Cycle_NumPts,ILastValues(1))
                CALL WDialogPutInteger(IDF_Pawley_Cycle_NumRefs,ILastValues(2))
                CALL retrieve_polybackup
                CALL WDialogFieldState(IDB_PawRef_Save,Enabled)
              ENDIF
              CALL WDialogFieldState(IDB_PawRef_Accept,Disabled)
              CALL WDialogFieldState(IDB_PawRef_Reject,Disabled)
              NumPawleyRef = NumPawleyRef - 1
              CALL WDialogPutInteger(IDF_Pawley_Refinement_Number,NumPawleyRef)
              IF (SpaceGroupDetermination) THEN
                CALL WDialogFieldState(IDB_PawRef_Save, Disabled)
              ENDIF
            CASE (IDB_PawRef_Save)
!!              IF (SpaceGroupDetermination) THEN
!!                CALL SpaceGroupDeterminationCode(LatBrav, RLastValues(2)) ! RlastValues(2) = Pawley chisqd                
!!                CALL WDialogFieldState(IDF_PawRef_Solve,Disabled)
!!              ELSE
                IF (SaveProject()) CALL WDialogFieldState(IDF_PawRef_Solve,Enabled)
!!              ENDIF
            CASE (IDF_PawRef_Solve)
              IF (SpaceGroupDetermination) THEN
                CALL SpaceGroupDeterminationCode(LatBrav, RLastValues(2)) ! RlastValues(2) = Pawley chisqd                
                CALL WDialogFieldState(IDB_PawRef_Save,Disabled)
              ELSE
! Emulate loading .SDI file for next window
                CALL WDialogSelect(IDD_SAW_Page1)
! Read in the HCV, PIK and TIC files from POLYP
                Ilen = LEN_TRIM(DashPikFile)
                SDIFile = DashPikFile(1:Ilen-3)//'sdi'
                CALL WDialogPutString(IDF_SA_Project_Name,SDIFile)
                CALL GETHCV(DashHcvFile,IER)
                CALL GETPIK(DashPikFile,IER)
                CALL WDialogSelect(IDD_ExclRegions)
                CALL WDialogHide
                CALL ShowWizardWindowZmatrices
!O            CASE (IDB_ExclRegions)
!O              CALL WDialogSelect(IDD_ExclRegions)
!O              CALL WDialogShow(-1,-1,0,Modeless)
              ENDIF
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_PawRef_RefSigm1_Check)
              CALL WDialogFieldStateLogical(IDF_PawRef_RefSigm2_Check,.NOT. WDialogGetCheckBoxLogical(IDF_PawRef_RefSigm1_Check))
            CASE (IDF_PawRef_RefSigm2_Check)
              CALL WDialogFieldStateLogical(IDF_PawRef_RefSigm1_Check,.NOT. WDialogGetCheckBoxLogical(IDF_PawRef_RefSigm2_Check))
            CASE (IDF_PawRef_RefGamm1_Check)
              CALL WDialogFieldStateLogical(IDF_PawRef_RefGamm2_Check,.NOT. WDialogGetCheckBoxLogical(IDF_PawRef_RefGamm1_Check))
            CASE (IDF_PawRef_RefGamm2_Check)
              CALL WDialogFieldStateLogical(IDF_PawRef_RefGamm1_Check,.NOT. WDialogGetCheckBoxLogical(IDF_PawRef_RefGamm2_Check))
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithPawleyFitWindow
!
!*****************************************************************************
!
      SUBROUTINE DealWithExclRegionsWindow

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_ExclRegions)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDB_ClearAll)
              CALL Clear_ExclRegions
            CASE (IDCANCEL, IDCLOSE)
              CALL WDialogHide
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithExclRegionsWindow
!
!*****************************************************************************
!
      SUBROUTINE WritePawleyRefinementFile

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE REFVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'
      INCLUDE 'Lattice.inc'

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

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER         PR_NumBack
      REAL                        PR_BackGround
      COMMON /PRBACK/ PR_NumBack, PR_BackGround(1:10)

      REAL               PeakShapeSigma(1:2), PeakShapeGamma(1:2), PeakShapeHPSL, PeakShapeHMSL
      COMMON /PEAKFIT3/  PeakShapeSigma,      PeakShapeGamma,      PeakShapeHPSL, PeakShapeHMSL

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      
      INTEGER NPawBack
      CHARACTER*4 ChRadOption(4)
      DATA CHRADOPTION /'LABX','SYNX','SYNX','TOFN'/
      INTEGER I
      LOGICAL, EXTERNAL :: FnUnitCellOK, FnWaveLengthOK, FnPatternOK
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical, NearlyEqual
      REAL,    EXTERNAL :: WavelengthOf
      INTEGER NTCycles
      INTEGER ITEM, ISYM, IRTYP
      INTEGER tFileHandle, hFile
      INTEGER iRow, ii, J
      REAL    tReal
! Following variables are needed to sort out the excluded regions
      INTEGER iOrd(1:10)
      REAL    rStart(1:10), rEnd(1:10)
      REAL    trStart(1:10), trEnd(1:10)
      INTEGER NumUsed
      LOGICAL Used(1:10)

! Are these checks in place here? If one of them fails, we shouldn't have been here in the first place.
!
! We should only proceed with this if we have good cell constants 
!
! Write out the data file ...
! We should check if there are data to write out!
      IF (.NOT. FnPatternOK()) RETURN
      IF (.NOT. FnUnitCellOK()) RETURN
      IF (NumOfRef .EQ. 0) RETURN
      IF (NumInternalDSC .NE. DataSetChange) THEN
        tFileHandle = 41
        OPEN(tFileHandle,file='polyp.dat',status='unknown',ERR=999)
        DO I = 1, NBIN
          WRITE(tFileHandle,'(F10.4,F12.2,F12.2)',ERR=999) XBIN(I), YOBIN(I), EBIN(I)
        ENDDO
        CLOSE(tFileHandle)
      ENDIF
      NumInternalDSC = DataSetChange
      NumPawleyRef = NumPawleyRef + 1
      CALL PushActiveWindowID
! Get Excluded regions from dialogue
      CALL WDialogSelect(IDD_ExclRegions)
      Used = .TRUE.
      NumUsed = 0
      DO iRow = 1, 10
        CALL WGridGetCellReal(IDF_ExclRegionsGrid,1,iRow,rStart(iRow))
        CALL WGridGetCellReal(IDF_ExclRegionsGrid,2,iRow,rEnd(iRow))
! If either invalid, mark as unused
        IF ((rStart(iRow) .LT. 0.0) .OR. (rEnd(iRow) .LT. 0.0)) Used(iRow) = .FALSE.
! make Start < End
        IF (rStart(iRow) .GT. rEnd(iRow)) THEN
          tReal = rStart(iRow)
          rStart(iRow) = rEnd(iRow)
          rEnd(iRow) = tReal
        ENDIF
! Check Start not lower than start of data
        IF (rStart(iRow) .LT. XBIN(1)   ) rStart(iRow) = XBIN(1)
! Check End not greater than end of data
        IF (rEnd(iRow)   .GT. XBIN(NBIN)) rEnd(iRow)   = XBIN(NBIN)
! Check Start and End not same
        IF ( NearlyEqual(rStart(iRow),rEnd(iRow))) Used(iRow) = .FALSE.
! If not used, set Start to 200.0, that way they will end up at the end of the list
        IF (Used(iRow)) THEN
          NumUsed = NumUsed + 1
        ELSE
          rStart(iRow) = 200.0
        ENDIF
      ENDDO
! Now sort
      CALL SORT_REAL(rStart,iOrd,10)
      DO iRow = 1, 10
        trStart(iRow) = rStart(iOrd(iRow))
        trEnd(iRow)   = rEnd(iOrd(iRow))
      ENDDO
      DO iRow = 1, 10
        rStart(iRow) = trStart(iRow)
        rEnd(iRow)   = trEnd(iRow)
      ENDDO
      Used = .TRUE.
      IF (NumUsed .LT. 10) THEN
        DO iRow = NumUsed+1, 10
          Used(iRow) = .FALSE.
        ENDDO
      ENDIF
! now check for overlap
      iRow = 1
   20 IF ((iRow+1) .LE. NumUsed) THEN
        IF (rStart(iRow+1) .LE. (rEnd(iRow)+0.0001)) THEN
          rEnd(iRow) = MAX(rEnd(iRow),rEnd(iRow+1))
! Shift remaining by -1
          IF ((iRow+1) .NE. NumUsed) THEN
            DO ii = iRow+1, NumUsed-1
              rStart(ii) = rStart(ii+1)
              rEnd(ii)   = rEnd(ii+1)
            ENDDO
          ENDIF
          NumUsed = NumUsed - 1
        ELSE
          iRow = iRow + 1
        ENDIF
        GOTO 20
      ENDIF
! End of Excluded Regions processing, NumUsed, rStart and rEnd have now been filled
! Now update this to the user:
      CALL WDialogClearField(IDF_ExclRegionsGrid)
      DO iRow = 1, NumUsed
        CALL WGridPutCellReal(IDF_ExclRegionsGrid,1,iRow,rStart(iRow))
        CALL WGridPutCellReal(IDF_ExclRegionsGrid,2,iRow,rEnd(iRow))
      ENDDO
      CALL WDialogSelect(IDD_Pawley_Status)
      CALL WDialogGetInteger(IDF_IDF_PawRef_NBack,NPawBack)
      CALL WDialogGetInteger(IDF_Pawley_Total_Cycles,NTCycles)    
      hFile = 42
      OPEN(hFile,FILE='polyp.ccl',status='unknown',ERR=999)
      WRITE(hFile,4210,ERR=999) 
 4210 FORMAT('N Polyfitter file for quick Pawley refinement')
      WRITE(hFile,4220,ERR=999) (CellPar(I),I=1,6)
 4220 FORMAT('C ',3F10.5,3F10.3)
      WRITE(hFile,4230,ERR=999) 
 4230 FORMAT('F C 2 2.31 20.8439 1.02 10.2075 1.5886 0.5687 0.865 51.6512 .2156'/'A C1 0 0 0 0') 
      IF (NumberSGTable .GE. 1) THEN
        CALL DecodeSGSymbol(SGShmStr(NumberSGTable))
        IF (nsymmin .GT. 0) THEN
          DO isym = 1, nsymmin
            WRITE(hFile,4235,ERR=999) symline(isym)
 4235       FORMAT('S ',A)
          ENDDO
        ENDIF
      ENDIF
      WRITE(hFile,4241,ERR=999) NTCycles, ChRadOption(JRadOption)
 4241 FORMAT('I NCYC ',I3,' PRCV 14 MCOR 0 FRIE 1 PRPR 0'/              &
      'L REFI PAWL'/                                                    &
      'L SORC ', A4/                                                    &
      'L WGHT 3')
      CALL WDialogGetCheckBox(IDF_PawRef_UseInts_Check,Item)
      IRtyp = 2 - Item
      WRITE(hFile,4246,ERR=999) IRTYP, xpmin, xpmax
 4246 FORMAT('L RTYP  'I3,2F10.3,'  0.001')
      IF (.NOT. FnWaveLengthOK()) ALambda = WavelengthOf('Cu')
      WRITE(hFile,4250,ERR=999) ALambda
 4250 FORMAT('L WVLN ',F10.5)
      IF ((ZeroPoint .LT. -1.0) .OR. (ZeroPoint .GT. 1.0)) ZeroPoint = 0.0
      WRITE(hFile,4260,ERR=999) ZeroPoint
 4260 FORMAT('L ZERO ',F10.5)
      DO iRow = 1, NumUsed
        WRITE(hFile,"('L EXCL ',F7.3,1X,F7.3)",ERR=999) rStart(iRow), rEnd(iRow)
      ENDDO
      CALL WDialogGetReal(IDF_Slim_Parameter,SLIMVALUE)
      WRITE(hFile,4270,ERR=999) SCALFAC, SLIMVALUE
 4270 FORMAT('L SCAL   ',F7.5,/                                         &
      'L SLIM ',F5.2,' '/                                               &
      'L REFK 10.0')
      WRITE(hFile,"('L PKCN TYPE 1')",ERR=999)
      WRITE(hFile,"('L PKFN TYPE 3')",ERR=999)
      WRITE(hFile,"('L PKFN LIMS 0.005')",ERR=999)
      WRITE(hFile,4271,ERR=999) PeakShapeSigma(1), PeakShapeSigma(2)
      WRITE(hFile,4272,ERR=999) PeakShapeGamma(1), PeakShapeGamma(2)
      WRITE(hFile,4273,ERR=999) PeakShapeHPSL
      WRITE(hFile,4274,ERR=999) PeakShapeHMSL
 4271 FORMAT('L PKFN SIGM ',2F8.4)
 4272 FORMAT('L PKFN GAMM ',2F8.4)
 4273 FORMAT('L PKFN HPSL ',F8.4)
 4274 FORMAT('L PKFN HMSL ',F8.4)
      IF (NPawBack .LE. 5) THEN
        WRITE(hFile,"('L BACK    2',4X,6(F10.5,1X))",ERR=999) (PR_BackGround(J),J=1,NPawBack)
      ELSE
        WRITE(hFile,"('L BACK    2',4X,6(F10.5,1X))",ERR=999) (PR_BackGround(J),J=1,5)
        WRITE(hFile,"('L BACK ',7(F10.5,1X))",ERR=999) (PR_BackGround(J),J=6,NPawBack)
      ENDIF
      WRITE(hFile,'(A)',ERR=999) 'L VARY ONLY ALL INTS'
      IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefBack_Check )) WRITE(hFile,'(A)',ERR=999) 'L VARY ALL BACK'
      IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefCell_Check )) WRITE(hFile,'(A)',ERR=999) 'L VARY ALL CELL'
      IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefZero_Check )) WRITE(hFile,'(A)',ERR=999) 'L VARY ZERO 1 '
      IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefSigm1_Check)) WRITE(hFile,'(A)',ERR=999) 'L VARY SIGM 1'
      IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefSigm2_Check)) WRITE(hFile,'(A)',ERR=999) 'L VARY SIGM 2'
      IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefGamm1_Check)) WRITE(hFile,'(A)',ERR=999) 'L VARY GAMM 1'
      IF (WDialogGetCheckBoxLogical(IDF_PawRef_RefGamm2_Check)) WRITE(hFile,'(A)',ERR=999) 'L VARY GAMM 2'
      CLOSE(hFile)
      CALL PopActiveWindowID
      RETURN
  999 CALL ErrorMessage('Error writing temporary input file for Pawley refinement.')
      CLOSE(hFile)
      CLOSE(tFileHandle)
      CALL PopActiveWindowID

      END SUBROUTINE WritePawleyRefinementFile
!
!*****************************************************************************
!
      INTEGER FUNCTION Quick_Pawley_Fit

      USE WINTERACTER

      IMPLICIT NONE

! DIMENSION OF ALSQ BELOW, AND SETTING OF MATSZ, TO BE ALTERED TO BE SOMETHING
! A LITTLE LARGER THAN N*(N+3)/2 WHERE THERE WILL BE N BASIC VARIABLES

      INCLUDE 'PARAMS.INC'
      
      EXTERNAL PCCN01,PFCN03,DUMMY,CALPR

      INTEGER         NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE
      COMMON /GLOBAL/ NINIT, NBATCH, NSYSTM, MULFAS, MULSOU, MULONE

      REAL ALSQ(QPFDIM)

      INTEGER         ICRYDA, NTOTAL,    NYZ, NTOTL, INREA,       ICDN,       IERR, IO10
      LOGICAL                                                                             SDREAD
      COMMON /CARDRC/ ICRYDA, NTOTAL(9), NYZ, NTOTL, INREA(26,9), ICDN(26,9), IERR, IO10, SDREAD

      INTEGER         LPT, LUNI
      COMMON /IOUNIT/ LPT, LUNI

      INTEGER MATSZ
      CHARACTER*6 xxx
      CHARACTER*10 fname
      INTEGER, EXTERNAL :: FORTY

      fname = 'polyp'
      xxx = 'CN11LS'
      MATSZ = QPFDIM
      NINIT = 1
      ALSQ = 0.0
! JCC trap the return status
      Quick_Pawley_Fit = FORTY(xxx,ALSQ,MATSZ,PCCN01,PFCN03,DUMMY,CALPR,fname)
! JCC Trap for an error on file opening
      IF (ICRYDA .NE. -1) CALL CLOFIL(ICRYDA)
      IF (IO10 .NE. -1)   CALL CLOFIL(IO10)
      CALL CLOFIL(LPT)

      END FUNCTION Quick_Pawley_Fit
!
!*****************************************************************************
!
      SUBROUTINE make_polybackup

      USE WINTERACTER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      LOGICAL           copypik, copytic, copyhcv, copyhkl
      COMMON / PBCKUP / copypik, copytic, copyhcv, copyhkl

      REAL             YCBINP,       YBBINP
      COMMON /PBCKUP2/ YCBINP(MOBS), YBBINP(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER inferr

      copypik = .FALSE.
      copytic = .FALSE.
      copyhcv = .FALSE.
      copyhkl = .FALSE.
! Make a backup copy of the polyp.pik file to recover in event of an error
      INQUIRE(FILE = 'polyp.pik', exist=copypik)
      inferr = InfoError(1)
      IF (copypik) THEN
        CALL IOsCopyFile('polyp.pik', 'polyp.pbk')
        IF (InfoError(1) .NE. 0) copypik = .FALSE.
      ENDIF
      INQUIRE(FILE = 'polyp.tic', exist=copytic)
      inferr = InfoError(1)
      IF (copytic) THEN
        CALL IOsCopyFile('polyp.tic', 'polyp.tbk')
        IF (InfoError(1) .NE. 0) copytic = .FALSE.
      ENDIF
      INQUIRE(FILE = 'polyp.hcv', exist=copyhcv)
      inferr = InfoError(1)
      IF (copyhcv) THEN
        CALL IOsCopyFile('polyp.hcv', 'polyp.hbk')
        IF (InfoError(1) .NE. 0) copyhcv = .FALSE.
      ENDIF
      INQUIRE(FILE = 'polyp.hkl', exist=copyhkl)
      inferr = InfoError(1)
      IF (copyhkl) THEN
        CALL IOsCopyFile('polyp.hkl', 'polyp.hbl')
        IF (InfoError(1) .NE. 0) copyhkl = .FALSE.
      ENDIF
      IF (.NOT. (copypik .AND. copytic .AND. copyhcv .AND. copyhkl)) CALL DebugErrorMessage('Error backing up Pawley')
      YCBINP = YCBIN
      YBBINP = YBBIN

      END SUBROUTINE make_polybackup
!
!*****************************************************************************
!
      SUBROUTINE retrieve_polybackup

      USE WINTERACTER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      LOGICAL           copypik, copytic, copyhcv, copyhkl
      COMMON / PBCKUP / copypik, copytic, copyhcv, copyhkl

      REAL             YCBINP,       YBBINP
      COMMON /PBCKUP2/ YCBINP(MOBS), YBBINP(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      INTEGER iDummy

      iDummy = InfoError(1)
      IF (copypik) CALL IOsCopyFile('polyp.pbk','polyp.pik')
      IF (InfoError(1) .NE. 0) CALL DebugErrorMessage('cp polyp.pbk polyp.pik unsuccessful')
      IF (copytic) CALL IOsCopyFile('polyp.tbk','polyp.tic')
      IF (InfoError(1) .NE. 0) CALL DebugErrorMessage('cp polyp.tbk polyp.tic unsuccessful')
      IF (copyhcv) CALL IOsCopyFile('polyp.hbk','polyp.hcv')
      IF (InfoError(1) .NE. 0) CALL DebugErrorMessage('cp polyp.hbk polyp.hcv unsuccessful')
      IF (copyhkl) CALL IOsCopyFile('polyp.hbl','polyp.hkl')
      IF (InfoError(1) .NE. 0) CALL DebugErrorMessage('cp polyp.hbl polyp.hkl unsuccessful')
      YCBIN = YCBINP
      YBBIN = YBBINP

      END SUBROUTINE retrieve_polybackup
!
!*****************************************************************************
!
      SUBROUTINE delete_polybackup

      LOGICAL           copypik, copytic, copyhcv, copyhkl
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
      INTEGER FUNCTION CreateSDIFile(SDIFileName)
!
! Returns 1 for failure, 0 for success
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: SDIFileName

      INCLUDE 'Lattice.inc'

      REAL             PAWLEYCHISQ, RWPOBS, RWPEXP
      COMMON /PRCHISQ/ PAWLEYCHISQ, RWPOBS, RWPEXP

      INTEGER LSDI, iDot, I, L1, L4, iDummy
      INTEGER, EXTERNAL :: WRTDSL

! Initialise to error
      CreateSDIFile = 1
      LSDI = LEN_TRIM(SDIFileName)
      IF (LSDI .GT. MaxPathLength) THEN
        CALL DebugErrorMessage('SDIFileName too long in CreateSDIFile')
        LSDI = MaxPathLength
      ENDIF
! First copy the .pik, .tic, .hcv and .khl files
	DashPikFile = ' '
      DashTicFile = ' '
      DashHcvFile = ' '
      DashHklFile = ' '
      DashDslFile = ' '
      IDot = 0
      DO I = LSDI, 1, -1
        IF (SDIFileName(I:I) .EQ. '.') THEN
          IDot = I
          GOTO 50
        ENDIF
      ENDDO
 50   DashPikFile(1:LSDI) = SDIFileName(1:LSDI)
      DashTicFile(1:LSDI) = SDIFileName(1:LSDI)
      DashHcvFile(1:LSDI) = SDIFileName(1:LSDI)
      DashHklFile(1:LSDI) = SDIFileName(1:LSDI)
      DashDslFile(1:LSDI) = SDIFileName(1:LSDI)
      IF (IDot .EQ. 0) THEN
        L1 = LSDI + 1
        L4 = LSDI + 4
      ELSE
        L1 = LSDI - 3
        L4 = LSDI
      ENDIF
      DashPikFile(L1:L4)='.pik'
      DashTicFile(L1:L4)='.tic'
      DashHcvFile(L1:L4)='.hcv'
      DashHklFile(L1:L4)='.hkl'
      DashDslFile(L1:L4)='.dsl'
      IF (WRTDSL(DashDslFile,L4) .NE. 0) RETURN
! Clear errors
      iDummy = InfoError(1)
      CALL IOSCopyFile('polyp.pik',DashPikFile)
      IF (InfoError(1) .NE. 0) THEN
        CALL ErrorMessage('Error while writing .pik file.')
        RETURN
      ENDIF
      CALL IOSCopyFile('polyp.tic',DashTicFile)
      IF (InfoError(1) .NE. 0) THEN
        CALL ErrorMessage('Error while writing .tic file.')
        RETURN
      ENDIF
      CALL IOSCopyFile('polyp.hcv',DashHcvFile)
      IF (InfoError(1) .NE. 0) THEN
        CALL ErrorMessage('Error while writing .hcv file.')
        RETURN
      ENDIF
      CALL IOSCopyFile('polyp.hkl',DashHklFile)
      IF (InfoError(1) .NE. 0) THEN
        CALL ErrorMessage('Error while writing .hkl file.')
        RETURN
      ENDIF
      OPEN(81,file=SDIFileName(1:LSDI),status='unknown',ERR=999)
      WRITE(81,8110,ERR=999) DashTicFile(1:LEN_TRIM(DashTicFile))
 8110 FORMAT(' TIC ',A)
      WRITE(81,8120,ERR=999) DashHcvFile(1:LEN_TRIM(DashHcvFile))
 8120 FORMAT(' HCV ',A)
      WRITE(81,8121,ERR=999) DashHklFile(1:LEN_TRIM(DashHklFile))
 8121 FORMAT(' HKL ',A)
      WRITE(81,8130,ERR=999) DashPikFile(1:LEN_TRIM(DashPikFile))
 8130 FORMAT(' PIK ',A)
      WRITE(81,8136,ERR=999) DashRawFile(1:LEN_TRIM(DashRawFile))
 8136 FORMAT(' RAW ',A)
      WRITE(81,8135,ERR=999) DashDslFile(1:LEN_TRIM(DashDslFile))
 8135 FORMAT(' DSL ',A)
      WRITE(81,8140,ERR=999) (CellPar(I),I=1,6)
 8140 FORMAT(' Cell ',3F10.5,3F10.4)
      WRITE(81,8150,ERR=999) NumberSGTable,SGNumStr(NumberSGTable),SGHMaStr(NumberSGTable)
 8150 FORMAT(' SpaceGroup ',I4,4X,A12,A12)
      WRITE(81,8160,ERR=999) PAWLEYCHISQ
 8160 FORMAT(' PawleyChiSq ',F10.2)
      CLOSE(81)
      CreateSDIFile = 0
      RETURN
  999 CALL ErrorMessage('Error writing .sdi file.')
      CLOSE(81)

      END FUNCTION CreateSDIFile
!
!*****************************************************************************
!
      INTEGER FUNCTION WRTDSL(FileName,LenFn)
! Routine for writing a 'DSL' file. This file contains
! The additional data that is part of the Winteracter front end: Namely
! radiation type/wavelength etc.
!
      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'GLBVAR.INC'

      CHARACTER*(*) FileName
      INTEGER       LenFn, Idum

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

! Initialise to error      
      WRTDSL = 1
      OPEN (UNIT = 77,FILE=FileName(1:LenFn),STATUS='UNKNOWN',ERR=999)
      WRITE(77,*,ERR=999)'! Radiation wavelength and data type'
      WRITE(77,'(A3,1X,F10.5,I2)',ERR=999) 'rad', ALambda, JRadOption
      WRITE(77,*,ERR=999)'! Sigma shape parameters: format sigma1 esd sigma2 esd'
      WRITE(77,100,ERR=999) 'sig',PkFnVarVal(1,1),PkFnVarEsd(1,1),PkFnVarVal(2,1),PkFnVarEsd(2,1)
      WRITE(77,*,ERR=999)'! Gamma shape parameters: format gamma1 esd gamma2 esd'
      WRITE(77,100,ERR=999) 'gam',PkFnVarVal(1,2),PkFnVarEsd(1,2),PkFnVarVal(2,2),PkFnVarEsd(2,2)
      WRITE(77,*,ERR=999)'! Asymmetry parameters: format HPSL esd HMSL esd'
      WRITE(77,100,ERR=999) 'asy',PkFnVarVal(1,3),PkFnVarEsd(1,3),PkFnVarVal(1,4),PkFnVarEsd(1,4)
      WRITE(77,*,ERR=999)'! Calculated zero point'
      WRITE(77,110,ERR=999) 'zer',ZeroPoint
      WRITE(77,*,ERR=999)'! Pawley-fit SLIM parameter setting'
      WRITE(77,110,ERR=999) 'sli',SLIMVALUE
      WRITE(77,*,ERR=999)'! Pawley-fit Scale factor setting'
      WRITE(77,110,ERR=999) 'sca',SCALFAC
  100 FORMAT(A3,1X,4(F10.4,1X))
  110 FORMAT(A3,1X,F10.4)
      CLOSE(77)
      WRTDSL = 0
      RETURN
! Error if we get here
  999 CALL ErrorMessage('Error writing .dsl file.')
      CLOSE(77,IOSTAT=IDUM)

      END FUNCTION WRTDSL
!
!*****************************************************************************
!
      LOGICAL FUNCTION SaveProject

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER(MaxPathLength) :: SDIFileName
      CHARACTER(LEN=45) :: FILTER
      INTEGER IFLAGS

      INTEGER, EXTERNAL :: CreateSDIFile
      
! Save the project
      SaveProject = .FALSE.
      IFLAGS = SaveDialog + AppendExt + PromptOn
      FILTER = 'Diffraction information files (*.sdi)|*.sdi|'
      SDIFileName = ' '
      CALL WSelectFile(FILTER,IFLAGS,SDIFileName,'Save diffraction information for structure solution')
      IF ((WinfoDialog(4) .EQ. CommonOk) .AND. (LEN_TRIM(SDIFileName) .NE. 0)) THEN
        IF (CreateSDIFile(SDIFileName) .EQ. 0) THEN
          CALL sa_SetOutputFiles(SDIFileName)
          SaveProject = .TRUE.
        ENDIF
      ENDIF

      END FUNCTION SaveProject
!
!*****************************************************************************
!
