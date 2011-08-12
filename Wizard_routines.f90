!
!*****************************************************************************
!
      SUBROUTINE StartWizard

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER         CurrentWizardWindow
      COMMON /Wizard/ CurrentWizardWindow

      CALL SetModeMenuState(1,0)
      CALL SelectMode(ID_Peak_Fitting_Mode)
      CALL WizardWindowShow(IDD_Polyfitter_Wizard_01)

      END SUBROUTINE StartWizard
!
!*****************************************************************************
!
      SUBROUTINE WizardWindowHide

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER                 IXPos_IDD_Wizard, IYPos_IDD_Wizard
      COMMON /DialoguePosCmn/ IXPos_IDD_Wizard, IYPos_IDD_Wizard

      INTEGER         CurrentWizardWindow
      COMMON /Wizard/ CurrentWizardWindow

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      IF (CurrentWizardWindow .EQ. 0) RETURN
! Things go all wrong if some intelligent part of DASH decides that the current Wizard window should be
! hidden while the main DASH window has been minimised. Therefore, just enter an infinite loop...
      CALL WindowSelect(0)
      DO WHILE (WinfoWindow(WindowState) .EQ. WinMinimised)
        CALL PeekEvent
        CALL WindowSelect(0)
        CALL IOsWait(50) ! wait half a sec
      ENDDO
      CALL SelectDASHDialog(CurrentWizardWindow)
      IF (WInfoDialog(6) .GT. 0) THEN
        IXPos_IDD_Wizard = WInfoDialog(6)
        IYPos_IDD_Wizard = WInfoDialog(7)
      ENDIF
      CALL WDialogHide()

! Because of limitations in WinterActer, some Wizard windows need to be swapped
! out of memory and back again.
      SELECT CASE ( CurrentWizardWindow )
        CASE ( IDD_RR_External )
          CALL UnloadDASHDialog(IDD_RR_External)
        CASE ( IDD_SX_Page1a )
          CALL DASHWDialogGetReal(IDF_MaxResolution, SXMaxResolution)
          CALL UnloadDASHDialog(IDD_SX_Page1a)
        CASE ( IDD_SAW_Page6a )
          CALL DASHWDialogGetRadioButton(IDF_RADIO1, iRietveldMethodOpt)
          CALL UnloadDASHDialog(IDD_SAW_Page6a)
        CASE ( IDD_PW_Page8c )
          CALL DASHWDialogGetRadioButton(IDF_RADIO1, iMcMailleNgridOpt)
          CALL UnloadDASHDialog(IDD_PW_Page8c)
        CASE ( IDD_PW_Page3a )
          lRebin = DASHWDialogGetCheckBoxLogical(IDF_BinData)
          CALL DASHWDialogGetInteger(IDF_LBIN, iBinWidth)
          CALL UnloadDASHDialog(IDD_PW_Page3a)
      END SELECT

      CurrentWizardWindow = 0

      END SUBROUTINE WizardWindowHide
!
!*****************************************************************************
!
      SUBROUTINE WizardWindowShow(TheDialogID)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheDialogID

      INTEGER                 IXPos_IDD_Wizard, IYPos_IDD_Wizard
      COMMON /DialoguePosCmn/ IXPos_IDD_Wizard, IYPos_IDD_Wizard

      INTEGER         CurrentWizardWindow
      COMMON /Wizard/ CurrentWizardWindow

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch
      LOGICAL, EXTERNAL :: SetRRMethodRadioState

      IF ( in_batch ) &
        RETURN
! Hide any visible Wizard window
      CALL WizardWindowHide

! Because of limitations in WinterActer, some Wizard windows need to be swapped
! out of memory and back again.
      SELECT CASE ( TheDialogID )
        CASE ( IDD_RR_External )
          CALL LoadDASHDialog(IDD_RR_External)
        CASE ( IDD_SX_Page1a )
          CALL LoadDASHDialog(IDD_SX_Page1a)
          CALL WDialogPutReal(IDF_MaxResolution, SXMaxResolution)
        CASE ( IDD_SAW_Page6a )
          CALL LoadDASHDialog(IDD_SAW_Page6a)
          IF ( SetRRMethodRadioState() ) iRietveldMethodOpt = 1
          SELECT CASE ( iRietveldMethodOpt )
             CASE ( 2 )
               CALL WDialogPutRadioButton(IDF_RADIO2)
             CASE ( 3 )
               CALL WDialogPutRadioButton(IDF_RADIO3)
             CASE ( 4 )
               CALL WDialogPutRadioButton(IDF_RADIO4)
             CASE DEFAULT
               CALL WDialogPutRadioButton(IDF_RADIO1)
          END SELECT
        CASE ( IDD_PW_Page8c )
          CALL LoadDASHDialog(IDD_PW_Page8c)
          SELECT CASE ( iMcMailleNgridOpt )
             CASE ( 2 )
               CALL WDialogPutRadioButton(IDF_RADIO2)
             CASE ( 3 )
               CALL WDialogPutRadioButton(IDF_RADIO3)
             CASE DEFAULT
               CALL WDialogPutRadioButton(IDF_RADIO1)
          END SELECT
        CASE ( IDD_PW_Page3a )
          CALL LoadDASHDialog(IDD_PW_Page3a)
          CALL WDialogPutCheckBoxLogical(IDF_BinData, lRebin)
          CALL WDialogPutInteger(IDF_LBIN, iBinWidth)
          CALL WDialogFieldStateLogical(IDF_LBIN, lRebin)
      END SELECT

      CALL SelectDASHDialog(TheDialogID)
      CurrentWizardWindow = TheDialogID
      CALL WDialogShow(IXPos_IDD_Wizard, IYPos_IDD_Wizard, IDNEXT, Modeless)
      CALL WMenuSetState(ID_Start_Wizard, ItemEnabled, WintOff)

      END SUBROUTINE WizardWindowShow
!
!*****************************************************************************
!
      SUBROUTINE EndWizard

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER         CurrentWizardWindow
      COMMON /Wizard/ CurrentWizardWindow

      LOGICAL, EXTERNAL :: WeCanDoAPawleyRefinement

      CALL WizardWindowHide
      IF (WeCanDoAPawleyRefinement()) THEN
        CALL SetModeMenuState(1, 1)
      ELSE
        CALL SetModeMenuState(1, -1)
      ENDIF
      CALL SelectMode(ID_Peak_Fitting_Mode)
      CALL WMenuSetState(ID_Start_Wizard, ItemEnabled, WintOn)

      END SUBROUTINE EndWizard
!
!*****************************************************************************
!
      SUBROUTINE EndWizardPastPawley
!
! After Pawley refinement (i.e., at the moment we enter the window where a .sdi file can be loaded)
! several things in DASH change state. Therefore, if we leave one of 
! those windows, they must be shown/ungreyed again.
! Effectively, this subroutine is supposed to be the inverse of ShowWizardWindowZmatrices
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'params.inc'
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

      LOGICAL, EXTERNAL :: WeCanDoAPawleyRefinement

      CALL EndWizard
      CALL SelectDASHDialog(IDD_Polyfitter_Wizard_01)
      CALL WDialogPutRadioButton(IDF_PW_Option4)
      PastPawley = .FALSE.
! Ungrey 'Clear cell parameters' button on toolbar
      CALL WMenuSetState(ID_Delabc, ItemEnabled, WintOn)
! Ungrey 'Remove background' button on toolbar
      IF (.NOT. NoData) CALL WMenuSetState(ID_Remove_Background, ItemEnabled, WintOn)
! Ungrey 'Load diffraction pattern' button on toolbar
      CALL WMenuSetState(ID_import_xye_file, ItemEnabled, WintOn)
! Ungrey 'Fit Peaks' button on toolbar
      CALL UpdatePeaksButtonsStates
! Ungrey 'Load DASH Pawley file' button on toolbar
      CALL WMenuSetState(ID_import_dpj_file, ItemEnabled, WintOn)
! Make unit cell etc. under 'View' no longer read only 
      CALL Upload_Cell_Constants
      CALL SelectDASHDialog(IDD_Crystal_Symmetry)
      CALL WDialogFieldState(IDF_Space_Group_Menu, Enabled)
      CALL WDialogFieldState(IDF_Crystal_System_Menu, Enabled)
      CALL WDialogFieldState(IDF_ZeroPoint,Enabled)
      CALL WDialogFieldState(IDAPPLY, Enabled)
      CALL WDialogFieldState(IDB_Delabc, Enabled)
      CALL SelectDASHDialog(IDD_Data_Properties)
      CALL WDialogFieldState(IDAPPLY, Enabled)
      IF (JRadOption .EQ. 1) CALL WDialogFieldState(IDF_Wavelength_Menu, Enabled)
      CALL WDialogFieldState(IDF_wavelength1, Enabled)
      CALL WDialogFieldState(IDF_LabX_Source, Enabled)
      CALL WDialogFieldState(IDF_SynX_Source, Enabled)
      CALL WDialogFieldState(IDF_CWN_Source, Disabled)
      CALL WDialogFieldState(IDF_TOF_source, Disabled)
! @@ ?
      CALL SelectDASHDialog(IDD_Peak_Positions)
      CALL WDialogFieldState(ID_Index_Output, DialogReadOnly)
      CALL SelectDASHDialog(IDD_ViewPawley)
      CALL WDialogFieldState(IDF_Sigma1, Enabled)
      CALL WDialogFieldState(IDF_Sigma2, Enabled)
      CALL WDialogFieldState(IDF_Gamma1, Enabled)
      CALL WDialogFieldState(IDF_Gamma2, Enabled)
      CALL WDialogFieldState(IDF_HPSL, Enabled)
      CALL WDialogFieldState(IDF_HMSL, Enabled)
      IPTYPE = 1
      CALL Profile_Plot

      END SUBROUTINE EndWizardPastPawley
!
!*****************************************************************************
!
      LOGICAL FUNCTION InWizard()

      USE WINTERACTER

      IMPLICIT NONE

      INTEGER         CurrentWizardWindow
      COMMON /Wizard/ CurrentWizardWindow

      IF (CurrentWizardWindow .GT. 0) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(CurrentWizardWindow)
        CALL WDialogShow(-1,-1)
        CALL PopActiveWindowID
        InWizard = .TRUE.
      ELSE
        InWizard = .FALSE.
      ENDIF
      RETURN

      END FUNCTION InWizard
!
!*****************************************************************************
!
      SUBROUTINE DealWithMainWizardWindow

      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      LOGICAL, EXTERNAL :: FnPatternOK
      INTEGER IPW_Option
      CHARACTER(MaxPathLength) tFile

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Polyfitter_Wizard_01)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
            CASE (IDNEXT)
! We're off the main page and on to new pages depending on the option.
              CALL DASHWDialogGetRadioButton(IDF_PW_Option1, IPW_Option)
              SELECT CASE (IPW_Option)
                CASE (1) ! View data / determine peaks positions
                  CALL SelectDASHDialog(IDD_PW_Page3)
! If no data => grey out 'Next >' button
                  CALL WDialogFieldStateLogical(IDNEXT, FnPatternOK())
                  CALL WDialogFieldStateLogical(IDB_Bin, FnPatternOK())
                  CALL WizardWindowShow(IDD_PW_Page3)
                CASE (2) ! Single crystal
                  CALL SelectDASHDialog(IDD_SX_Page1)
                  ZeroPoint = 0.0
                  CALL Upload_ZeroPoint               
                  CALL Generate_TicMarks
                  CALL WizardWindowShow(IDD_SX_Page1)
                CASE (3) ! Simulated annealing structure solution
                  CALL WizardWindowShow(IDD_SA_method)
                CASE (4) ! Analyse solutions
                  CALL WizardWindowShow(IDD_SAW_Page5)
                  CALL SelectMode(IDB_AnalyseSolutions)
                CASE (5) ! Rietveld refinement
                  CALL WizardWindowShow(IDD_SAW_Page6)
                  CALL DASHWDialogGetString(IDF_Xtal_File_Name, tFile)
                  IF (LEN_TRIM(tFile) .LE. 0) THEN
                    CALL WDialogFieldState(IDNEXT, Disabled)
                    CALL WDialogFieldState(IDB_Restart, Disabled)
                  ENDIF
              END SELECT
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithMainWizardWindow
!
!*****************************************************************************
!
      SUBROUTINE WizardApplyDiffractionFileInput

      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'params.inc'

      INTEGER          NOBS
      REAL                         XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS,       XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

! Not too pretty, but safe
      INTEGER                BackupNOBS
      REAL                               BackupXOBS,       BackupYOBS,       BackupEOBS
      COMMON /BackupPROFOBS/ BackupNOBS, BackupXOBS(MOBS), BackupYOBS(MOBS), BackupEOBS(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER I

      NOBS = BackupNOBS
      DO I = 1, NOBS
        XOBS(I) = BackupXOBS(I)
        YOBS(I) = BackupYOBS(I)
        EOBS(I) = BackupEOBS(I)
      ENDDO
! All DASH FUNCTIONs and SUBROUTINEs use the re-binned profile, so emulate
! binning with a bin width of 1
      NBIN = NOBS
      LBIN = 1
      DO I = 1, NBIN
        XBIN(I)  = XOBS(I)
        YOBIN(I) = YOBS(I)
        YCBIN(I) = 0.0
        YBBIN(I) = 0.0
        EBIN(I)  = EOBS(I)
      ENDDO
      CALL GetProfileLimits
      CALL Get_IPMaxMin 

      END SUBROUTINE WizardApplyDiffractionFileInput
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowDiffractionFileInput

      USE DRUID_HEADER
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'

      INTEGER, EXTERNAL :: DiffractionFileBrowse, DiffractionFileOpen
      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical, FnPatternOK
      CHARACTER(MaxPathLength) CTEMP
      INTEGER ISTAT, iTmpJRadOption

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page3)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              IF ( iRietveldMethod .NE. INTERNAL_RB ) THEN
                CALL WizardWindowShow(IDD_RR_External)
              ELSE
                CALL WizardWindowShow(IDD_Polyfitter_Wizard_01)
              ENDIF
            CASE (IDNEXT, IDB_Bin)
              IF ( iRietveldMethod .NE. INTERNAL_RB ) THEN
                CALL DASHWDialogGetString(IDF_PWa_DataFileName_String, CTEMP)
                IF (LEN_TRIM(CTEMP) .LE. 0) THEN
                  CALL ErrorMessage('Have you chosen a diffaction file?')
                  GOTO 230
                ENDIF
                IF (DiffractionFileOpen(CTEMP) .NE. 0) GOTO 230
              ENDIF
              CALL WizardApplyDiffractionFileInput
              CALL Profile_Plot
! If the user is re-binning this profile, make sure we pass the binning
              IF ((EventInfo%VALUE1 .EQ. IDB_Bin) .OR. lRebin) THEN
                CALL WizardWindowShow(IDD_PW_Page3a)
              ELSE
                CALL WizardWindowShow(IDD_PW_Page4)
                IF ( iRietveldMethod .NE. INTERNAL_RB ) THEN
                  ! Match radiation and state of monochromated option
                  CALL DASHWDialogGetRadioButton(IDF_LabX_Source, iTmpJRadOption)
                  CALL WDialogFieldStateLogical(IDC_Monochromated, iTmpJRadOption .EQ. 1)
                ENDIF
              ENDIF
  230         CONTINUE
            CASE (IDCANCEL, IDCLOSE)
              IF ( iRietveldMethod .NE. INTERNAL_RB ) THEN
                CALL CopyBackup2Pattern()
                iRietveldMethod = INTERNAL_RB
                CALL EndWizardPastPawley
              ELSE
                CALL EndWizard
              ENDIF
            CASE (ID_PWa_DF_Open)
              CALL DASHWDialogGetString(IDF_PWa_DataFileName_String, CTEMP)
              ISTAT = DiffractionFileOpen(CTEMP)
! If no data => grey out 'Next >' button
              CALL WDialogFieldStateLogical(IDNEXT,  (ISTAT .EQ. 0 .AND. FnPatternOK()))
              CALL WDialogFieldStateLogical(IDB_Bin, (ISTAT .EQ. 0 .AND. FnPatternOK()))
            CASE (IDBBROWSE)
              ISTAT = DiffractionFileBrowse()
! If no data => grey out 'Next >' button
              CALL WDialogFieldStateLogical(IDNEXT, FnPatternOK())
              CALL WDialogFieldStateLogical(IDB_Bin, FnPatternOK())
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowDiffractionFileInput
!
!*****************************************************************************
!
      SUBROUTINE WizardApplyRebin

      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'params.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page3a)
      IF (DASHWDialogGetCheckBoxLogical(IDF_BinData)) THEN
        CALL DASHWDialogGetInteger(IDF_LBIN,LBIN)
      ELSE
        LBIN = 1
      ENDIF
      CALL PopActiveWindowID
      CALL Rebin_Profile

      END SUBROUTINE WizardApplyRebin
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowRebin

      USE DRUID_HEADER
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      INTEGER iTmpJRadOption

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page3a)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardApplyDiffractionFileInput
              CALL Profile_Plot 
              CALL WizardWindowShow(IDD_PW_Page3)
            CASE (IDNEXT)
              CALL WizardApplyRebin
              CALL Profile_Plot
              CALL WizardWindowShow(IDD_PW_Page4)
              IF ( iRietveldMethod .NE. INTERNAL_RB ) THEN
                ! Match radiation and state of monochromated option
                CALL DASHWDialogGetRadioButton(IDF_LabX_Source, iTmpJRadOption)
                CALL WDialogFieldStateLogical(IDC_Monochromated, iTmpJRadOption .EQ. 1)
              ENDIF
            CASE (IDCANCEL, IDCLOSE)
              IF ( iRietveldMethod .NE. INTERNAL_RB ) THEN
                CALL CopyBackup2Pattern()
                iRietveldMethod = INTERNAL_RB
                CALL EndWizardPastPawley
              ELSE
                CALL EndWizard
              ENDIF
            CASE (IDAPPLY)
              CALL WizardApplyRebin
              CALL Profile_Plot
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_BinData)
! If set to 'TRUE', ungrey value field and vice versa
              CALL WDialogFieldStateLogical(IDF_LBIN, DASHWDialogGetCheckBoxLogical(IDF_BinData))
          END SELECT                
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowRebin
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowDiffractionSetup
!
! Effectively, the user is asked to provide / confirm the wavelength
!
      USE DRUID_HEADER
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL, EXTERNAL :: TwoTheta2dSpacing
      LOGICAL, EXTERNAL :: FnWavelengthOK
      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      REAL Temp
      INTEGER IRadSelection, iFlags
      CHARACTER(MaxPathLength) tFileName
      CHARACTER(LEN=45) :: FILTER

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page4)
      SELECT CASE (EventType)
        CASE ( PushButton ) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
! If the user is re-binning this profile, make sure we pass the binning
              IF ( lRebin ) THEN
                CALL WizardWindowShow(IDD_PW_Page3a)
              ELSE
                CALL WizardWindowShow(IDD_PW_Page3)
              ENDIF
            CASE (IDNEXT, IDB_PO)
              IF (.NOT. FnWavelengthOK()) THEN
                CALL ErrorMessage('Invalid wavelength.')
                GOTO 100
              ENDIF
              CALL DASHWDialogGetReal(IDF_wavelength1, Temp)
              CALL Set_Wavelength(Temp)
              IF (iRietveldMethod .NE. INTERNAL_RB) THEN
                CALL SelectDASHDialog(IDD_SAW_Page2)
                IF ((EventInfo%VALUE1 .EQ. IDB_PO) .OR. &
                     DASHWDialogGetCheckBoxLogical(IDF_Use_PO)) THEN
                  CALL WizardWindowShow(IDD_SAW_Page2)
                  GOTO 100
                ENDIF
              ENDIF
              CALL WizardWindowShow(IDD_PW_Page5)
 100          CONTINUE
            CASE (IDCANCEL, IDCLOSE)
              IF ( iRietveldMethod .NE. INTERNAL_RB ) THEN
                CALL CopyBackup2Pattern()
                iRietveldMethod = INTERNAL_RB
                CALL EndWizardPastPawley
              ELSE
                CALL EndWizard
              ENDIF
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_LabX_Source, IDF_SynX_Source, IDF_CWN_Source, IDF_TOF_source)
              CALL DASHWDialogGetRadioButton(IDF_LabX_Source, JRadOption)
              CALL Upload_Source
              CALL Generate_TicMarks 
              IF ( iRietveldMethod .NE. INTERNAL_RB ) THEN ! Enable/disable "Monochromated" check box
                CALL WDialogFieldStateLogical(IDC_Monochromated, JRadOption .EQ. 1)
              ENDIF
            CASE (IDF_wavelength1)
              CALL DASHWDialogGetReal(IDF_wavelength1, Temp)
              CALL Set_Wavelength(Temp)
              CALL Generate_TicMarks 
            CASE (IDF_Wavelength_Menu)
              CALL DASHWDialogGetMenu(IDF_Wavelength_Menu, IRadSelection)
              CALL SetWavelengthToSelection(IRadSelection)
              CALL Generate_TicMarks 
            CASE (IDF_GSAS_Import_ins)
              IF ( EventInfo%VALUE2 .EQ. IDF_GSAS_Import_ins .AND. &
                   iRietveldMethod .EQ. FOR_GSAS ) THEN
                IF (DASHWDialogGetCheckBoxLogical(IDF_GSAS_Import_ins)) THEN
                   CALL InfoMessage('When click on OK, you will be prompted to select'//CHAR(13)//&
                               'an INS file to be used in GSAS. It will be copied'//CHAR(13)//&
                               'to the directory where the generated EXP file goes '//CHAR(13)//&
                               'and renamed as the same base name.'//CHAR(13)//&
                               'Note: only BANK 1 will be used in the refinement.')
                   tFileName = GSASINS
                   FILTER = 'GSAS ins file (*.ins)|*.ins;*.INS|'
                   iFlags = LoadDialog + AppendExt + PromptOn
                   CALL WSelectFile(FILTER, iFlags, tFileName, 'Select a GSAS ins file')
                   IF ((WinfoDialog(4) .EQ. CommonOk) .AND. (LEN_TRIM(tFileName) .NE. 0)) &
                     GSASINS = tFileName
                ENDIF
              ENDIF
          END SELECT                
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowDiffractionSetup
!
!*****************************************************************************
!
      SUBROUTINE WizardApplyProfileRange

      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical, FnPatternOK
      REAL, EXTERNAL :: TwoTheta2dSpacing
      REAL    tMin, tMax

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page5)
      IF (DASHWDialogGetCheckBoxLogical(IDF_TruncateStartYN)) THEN
        CALL DASHWDialogGetReal(IDF_Min2Theta, tMin)
      ELSE
! If the user doesn't want to truncate the data, just restore the old values
        tMin = 0.0
      ENDIF
      IF (DASHWDialogGetCheckBoxLogical(IDF_TruncateEndYN)) THEN
        CALL DASHWDialogGetReal(IDF_Max2Theta, tMax)
      ELSE
! If the user doesn't want to truncate the data, just restore the old values
        tMax = 180.0
      ENDIF
      CALL TruncateData(tMin, tMax)
! Now check if we have reasonable data left. If not, don't allow pressing 'Next >'
      IF (FnPatternOK()) THEN
! Update the values in the min/max fields to reflect what has actually happened
! (e.g. in case min was .GT. max)
        IF (DASHWDialogGetCheckBoxLogical(IDF_TruncateStartYN)) THEN
          CALL WDialogPutReal(IDF_Min2Theta, tMin)
        ENDIF
        IF (DASHWDialogGetCheckBoxLogical(IDF_TruncateEndYN)) THEN
          CALL WDialogPutReal(IDF_Max2Theta, tMax)
          CALL WDialogPutReal(IDF_MaxResolution, TwoTheta2dSpacing(tMax))
        ENDIF
        CALL SelectDASHDialog(IDD_ViewPawley)
        CALL WDialogPutReal(IDF_MaxResolution, TwoTheta2dSpacing(tMax))
      ENDIF
      CALL PopActiveWindowID

      END SUBROUTINE WizardApplyProfileRange
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowProfileRange

      USE DRUID_HEADER
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'

      INTEGER                BackupNOBS
      REAL                               BackupXOBS,       BackupYOBS,       BackupEOBS
      COMMON /BackupPROFOBS/ BackupNOBS, BackupXOBS(MOBS), BackupYOBS(MOBS), BackupEOBS(MOBS)

      REAL, EXTERNAL :: TwoTheta2dSpacing, dSpacing2TwoTheta
      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical, FnPatternOK
      REAL tReal, tMin, tMax
      INTEGER tFieldState

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page5)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardApplyDiffractionFileInput
              CALL Profile_Plot
              CALL WizardWindowShow(IDD_PW_Page4)
            CASE (IDNEXT)
              CALL WizardApplyProfileRange
! Check if we have reasonable data left. If not, don't allow pressing 'Next >'
              IF ( .NOT. FnPatternOK()) THEN
                CALL ErrorMessage('Invalid profile range.')
                tMin = 0.0
                tMax = 180.0
                CALL TruncateData(tMin,tMax)
              ELSE
                CALL WizardWindowShow(IDD_PW_Page6)
              ENDIF
              CALL Profile_Plot
            CASE (IDCANCEL, IDCLOSE)
              IF ( iRietveldMethod .NE. INTERNAL_RB ) THEN
                CALL CopyBackup2Pattern()
                iRietveldMethod = INTERNAL_RB
                CALL EndWizardPastPawley
              ELSE
                CALL EndWizard
              ENDIF
            CASE (IDAPPLY)
              CALL WizardApplyProfileRange
! Check if we have reasonable data left. If not, don't allow the truncation
              IF (.NOT. FnPatternOK()) THEN
                CALL ErrorMessage('Invalid profile range.')
                tMin = 0.0
                tMax = 180.0
                CALL TruncateData(tMin,tMax)
              ENDIF
              CALL Profile_Plot
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_TruncateStartYN)
! If set to 'TRUE', ungrey value field and vice versa
              CALL WDialogFieldStateLogical(IDF_Min2Theta, DASHWDialogGetCheckBoxLogical(IDF_TruncateStartYN))
            CASE (IDF_TruncateEndYN)
! If set to 'TRUE', ungrey value fields and vice versa
              IF (DASHWDialogGetCheckBoxLogical(IDF_TruncateEndYN)) THEN
                tFieldState = Enabled
              ELSE
                tFieldState = Disabled
              ENDIF
              CALL WDialogFieldState(IDF_Max2Theta, tFieldState)
              CALL WDialogFieldState(IDF_MaxResolution, tFieldState)
              CALL WDialogFieldState(IDB_Convert, tFieldState)
            CASE (IDF_Min2Theta)
              CALL DASHWDialogGetReal(IDF_Min2Theta, tMin)
              IF (tMin .LT. BackupXOBS(1)) tMin = BackupXOBS(1)
              CALL DASHWDialogGetReal(IDF_Max2Theta, tMax)
              IF (tMin .GT. tMax) tMin = tMax
              CALL WDialogPutReal(IDF_Min2Theta, tMin)
            CASE (IDF_Max2Theta)
! When entering a maximum value for 2 theta, update maximum value for the resolution
              CALL DASHWDialogGetReal(IDF_Max2Theta, tMax)
              IF (tMax .GT. BackupXOBS(BackupNOBS)) tMax = BackupXOBS(BackupNOBS)
              CALL DASHWDialogGetReal(IDF_Min2Theta, tMin)
              IF (tMax .LT. tMin) tMax = tMin
              CALL WDialogPutReal(IDF_Max2Theta, tMax)
              CALL WDialogPutReal(IDF_MaxResolution, TwoTheta2dSpacing(tMax))
              CALL SelectDASHDialog(IDD_ViewPawley)
              CALL WDialogPutReal(IDF_MaxResolution, TwoTheta2dSpacing(tMax))
            CASE (IDF_MaxResolution)
! When entering a maximum value for the resolution, update maximum value for 2 theta
              CALL DASHWDialogGetReal(IDF_MaxResolution, tReal)
              tMax = dSpacing2TwoTheta(tReal)
              IF (tMax .GT. BackupXOBS(BackupNOBS)) tMax = BackupXOBS(BackupNOBS)
              CALL DASHWDialogGetReal(IDF_Min2Theta, tMin)
              IF (tMax .LT. tMin) tMax = tMin
              CALL WDialogPutReal(IDF_Max2Theta, tMax)
              CALL WDialogPutReal(IDF_MaxResolution, TwoTheta2dSpacing(tMax))
              CALL SelectDASHDialog(IDD_ViewPawley)
              CALL WDialogPutReal(IDF_MaxResolution, TwoTheta2dSpacing(tMax))
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowProfileRange
!
!*****************************************************************************
!
      SUBROUTINE WizardApplyBackground

      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      INTEGER tInt1, tInt2, tInt3

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page6)
      IF (DASHWDialogGetCheckBoxLogical(IDF_SubtractBackground)) THEN
        CALL DASHWDialogGetInteger(IDF_NumOfIterations, tInt2)
        CALL DASHWDialogGetInteger(IDF_WindowWidth, tInt1)
        CALL DASHWDialogGetInteger(IDF_SmoothWindow, tInt3)
        CALL SubtractBackground(tInt1, tInt2, &
                                DASHWDialogGetCheckBoxLogical(IDF_UseMCYN), &
                                DASHWDialogGetCheckBoxLogical(IDF_UseSmooth), tInt3)
      ELSE
        CALL Clear_BackGround
        BACKREF = .TRUE.
      ENDIF
      CALL PopActiveWindowID

      END SUBROUTINE WizardApplyBackground
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowBackground

      USE DRUID_HEADER
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      INTEGER          IPMIN, IPMAX, iStart, iStop, nPoints
      COMMON /PROFIPM/ IPMIN, IPMAX, iStart, iStop, nPoints

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER                BackupNOBS
      REAL                               BackupXOBS,       BackupYOBS,       BackupEOBS
      COMMON /BackupPROFOBS/ BackupNOBS, BackupXOBS(MOBS), BackupYOBS(MOBS), BackupEOBS(MOBS)

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      INTEGER tInt1, tInt2, tInt3, tFieldState
      REAL             tYPMIN,     tYPMAX,       &
                       tXPGMIN,    tXPGMAX,    tYPGMIN,    tYPGMAX,      &
                       tXPGMINOLD, tXPGMAXOLD, tYPGMINOLD, tYPGMAXOLD
      INTEGER          tIPMIN, tIPMAX, tiStart, tiStop, tnPoints

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page6)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardApplyProfileRange
              CALL Profile_Plot
              CALL WizardWindowShow(IDD_PW_Page5)
            CASE (IDNEXT)
              CALL WizardApplyProfileRange
              CALL WizardApplyBackground
              CALL Profile_Plot
              CALL UpdatePeaksButtonsStates
              IF ( iRietveldMethod .EQ. INTERNAL_RB ) THEN
                CALL WizardWindowShow(IDD_PW_Page7)
              ELSE
                CALL StartExternalRR
              ENDIF
            CASE (IDCANCEL, IDCLOSE)
              IF ( iRietveldMethod .NE. INTERNAL_RB ) THEN
                CALL CopyBackup2Pattern()
                iRietveldMethod = INTERNAL_RB
                CALL EndWizardPastPawley
              ELSE
                CALL EndWizard
              ENDIF
            CASE (IDB_Preview)
              tYPMIN     = YPMIN
              tYPMAX     = YPMAX
              tXPGMIN    = XPGMIN
              tXPGMAX    = XPGMAX
              tYPGMIN    = YPGMIN
              tYPGMAX    = YPGMAX
              tXPGMINOLD = XPGMINOLD
              tXPGMAXOLD = XPGMAXOLD
              tYPGMINOLD = YPGMINOLD
              tYPGMAXOLD = YPGMAXOLD
              tIPMIN     = IPMIN
              tIPMAX     = IPMAX
              tiStart    = iStart
              tiStop     = iStop
              tnPoints   = nPoints
              CALL WizardApplyProfileRange
              IF ( DASHWDialogGetCheckBoxLogical(IDF_SubtractBackground) ) THEN
                CALL DASHWDialogGetInteger(IDF_NumOfIterations, tInt2)
                CALL DASHWDialogGetInteger(IDF_WindowWidth, tInt1)
                CALL DASHWDialogGetInteger(IDF_SmoothWindow, tInt3)
                CALL CalculateBackground(tInt1, tInt2, &
                                         DASHWDialogGetCheckBoxLogical(IDF_UseMCYN), &
                                         DASHWDialogGetCheckBoxLogical(IDF_UseSmooth), tInt3)
              ELSE
                CALL Clear_BackGround
              ENDIF
! Force display of background
              CALL SelectDASHDialog(IDD_Plot_Option_Dialog)
              CALL WDialogPutCheckBoxLogical(IDF_background_check, .TRUE.)
              XPGMIN    = tXPGMIN
              XPGMAX    = tXPGMAX
              YPGMIN    = tYPGMIN
              YPGMAX    = tYPGMAX
              XPGMINOLD = tXPGMINOLD
              XPGMAXOLD = tXPGMAXOLD
              YPGMINOLD = tYPGMINOLD
              YPGMAXOLD = tYPGMAXOLD
              IPMIN     = tIPMIN
              IPMAX     = tIPMAX
              iStart    = tiStart
              iStop     = tiStop
              nPoints   = tnPoints
              CALL Profile_Plot
            CASE (IDAPPLY)
              CALL WizardApplyProfileRange
              CALL WizardApplyBackground
              CALL Profile_Plot
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE ( IDF_SubtractBackground )
! If set to 'TRUE', ungrey value field and vice versa
              IF ( DASHWDialogGetCheckBoxLogical(IDF_SubtractBackground) ) THEN
                tFieldState = Enabled
              ELSE
                tFieldState = Disabled
                CALL WizardApplyProfileRange
                CALL Clear_BackGround
                CALL Profile_Plot
              ENDIF
              CALL WDialogFieldState(IDF_LABEL7, tFieldState)
              CALL WDialogFieldState(IDF_NumOfIterations, tFieldState)
              CALL WDialogFieldState(IDF_LABEL8, tFieldState)
              CALL WDialogFieldState(IDF_WindowWidth, tFieldState)
              CALL WDialogFieldState(IDF_UseMCYN, tFieldState)
              CALL WDialogFieldState(IDB_Preview, tFieldState)
              CALL WDialogFieldState(IDAPPLY, tFieldState)
              CALL WDialogFieldState(IDF_UseSmooth, tFieldState)
              CALL WDialogFieldState(IDF_LABEL3, tFieldState)
              CALL WDialogFieldState(IDF_SmoothWindow, tFieldState)
            CASE ( IDF_UseSmooth )
              IF ( DASHWDialogGetCheckBoxLogical(IDF_UseSmooth) ) THEN
                CALL WDialogFieldState(IDF_LABEL3, Enabled)
                CALL WDialogFieldState(IDF_SmoothWindow, Enabled)
              ELSE
                CALL WDialogFieldState(IDF_LABEL3, Disabled)
                CALL WDialogFieldState(IDF_SmoothWindow, Disabled)
              ENDIF
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowBackground
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowIndexing1

      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'

      LOGICAL, EXTERNAL :: FnUnitCellOK
      INTEGER IndexOption, p

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page7)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_PW_Page6)
            CASE (IDNEXT)
              CALL DASHWDialogGetRadioButton(IDF_RADIO1, IndexOption) ! 'Index now', 'DICVOL04' or 'Enter known cell'
              SELECT CASE (IndexOption)
                CASE (1) ! Index pattern now
                  CALL SelectDASHDialog(IDD_PW_Page8)
                  DICVOL_ver = DICVOL_internal
! If this is synchrotron data, then set the default error in the peak positions to 0.03 rather than 0.04.
! This decreases the number of solutions and increases the speed of the search.
                  IF (JRadOption .EQ. 2) THEN
                    CALL WDialogPutReal(IDF_eps, 0.03, '(F5.3)')
                  ELSE
                    CALL WDialogPutReal(IDF_eps, 0.04, '(F5.3)')
                  ENDIF
                  CALL WizardWindowShow(IDD_PW_Page8)
                CASE (2) ! external DICVOL
                  CALL SelectDASHDialog(IDD_PW_Page8b)
                  p = LEN_TRIM(DICVOLEXE)
                  IF ( p .GE. 6 .AND. (DICVOLEXE(p-5:p) .EQ. '06.exe' .OR. &
                       DICVOLEXE(p-5:p) .EQ. '06.EXE') ) THEN
                    DICVOL_ver = DICVOL06
                    CALL WDialogFieldState(IDC_Exhaustive,Enabled)
                  ELSE
                    DICVOL_ver = DICVOL04
                    CALL WDialogPutCheckBoxLogical(IDC_Exhaustive, .FALSE.)
                    CALL WDialogFieldState(IDC_Exhaustive,Disabled)
                  ENDIF
! If this is synchrotron data, then set the default error in the peak positions to 0.03 rather than 0.04.
! This decreases the number of solutions and increases the speed of the search.
                  IF (JRadOption .EQ. 2) THEN
                    CALL WDialogPutReal(IDF_eps, 0.03, '(F5.3)')
                  ELSE
                    CALL WDialogPutReal(IDF_eps, 0.04, '(F5.3)')
                  ENDIF
                  CALL WizardWindowShow(IDD_PW_Page8b)
                CASE (3) ! Stand-alone McMaille
                  CALL WizardWindowShow(IDD_PW_Page8c)
                CASE (4) ! Enter known unit cell parameters
                  CALL SelectDASHDialog(IDD_PW_Page1)
! If the cell is OK, the Next> button should be enabled
                  CALL WDialogFieldStateLogical(IDNEXT, FnUnitCellOK())
                  CALL WizardWindowShow(IDD_PW_Page1)
              END SELECT
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
            CASE (IDF_AutoSelectPeaks)
              CALL AutoFitPeaks
          END SELECT
        CASE (FieldChanged)
          CALL UpdatePeaksButtonsStates
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowIndexing1
!
!*****************************************************************************
!
      SUBROUTINE DealWithDICVOLRunning

      USE DRUID_HEADER
      USE VARIABLES
      USE DICVAR

      IMPLICIT NONE

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_DICVOLRunning)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBSTOP, IDCANCEL)
              DICVOL_Error = cDICVOL_ErrorInterrupted
            CASE (IDBPause)
              CALL WMessageBox(OKOnly, ExclamationIcon, CommonOK, 'Press OK to continue', 'Pause')
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithDICVOLRunning
!
!*****************************************************************************
!
      SUBROUTINE DICVOL_FinishedCrystalSystem(TheCrystalSystem)
!
! Called when DICVOL has finished searching a crystal system. Updates the
! "DICVOL Running..." dialogue.
!
      USE DRUID_HEADER
      USE VARIABLES
      USE DICVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheCrystalSystem 

      CHARACTER*20, EXTERNAL :: Integer2String
      CHARACTER*20 tString

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_DICVOLRunning)
      IF (IAND(TheCrystalSystem, cCubic       ) .EQ. cCubic       ) THEN
        tString = Integer2String(DICVOL_NumOfSolutions(1))
        CALL WDialogPutString(IDF_LabProgCubic, tString(1:LEN_TRIM(tString)))
      ENDIF
      IF (IAND(TheCrystalSystem, cTetragonal  ) .EQ. cTetragonal  ) THEN
        tString = Integer2String(DICVOL_NumOfSolutions(2))
        CALL WDialogPutString(IDF_LabProgTetragonal, tString(1:LEN_TRIM(tString)))
      ENDIF
      IF (IAND(TheCrystalSystem, cHexagonal   ) .EQ. cHexagonal   ) THEN
        tString = Integer2String(DICVOL_NumOfSolutions(3))
        CALL WDialogPutString(IDF_LabProgHexagonal, tString(1:LEN_TRIM(tString)))
      ENDIF
      IF (IAND(TheCrystalSystem, cOrthorhombic) .EQ. cOrthorhombic) THEN
        tString = Integer2String(DICVOL_NumOfSolutions(4))
        CALL WDialogPutString(IDF_LabProgOrthorhombic, tString(1:LEN_TRIM(tString)))
      ENDIF
      IF (IAND(TheCrystalSystem, cMonoclinic  ) .EQ. cMonoclinic  ) THEN
        tString = Integer2String(DICVOL_NumOfSolutions(5))
        CALL WDialogPutString(IDF_LabProgMonoclinic, tString(1:LEN_TRIM(tString)))
      ENDIF
      IF (IAND(TheCrystalSystem, cTriclinic   ) .EQ. cTriclinic   ) THEN
        tString = Integer2String(DICVOL_NumOfSolutions(6))
        CALL WDialogPutString(IDF_LabProgTriclinic, tString(1:LEN_TRIM(tString)))
      ENDIF
      CALL PeekEvent
      CALL PopActiveWindowID

      END SUBROUTINE DICVOL_FinishedCrystalSystem
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowIndexing2

      USE DRUID_HEADER
      USE VARIABLES
      USE DICVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      INTEGER           NTPeak
      REAL              AllPkPosVal,         AllPkPosEsd
      REAL              AllPkAreaVal
      REAL              PkProb
      INTEGER           IOrdTem
      INTEGER           IHPk
      COMMON /ALLPEAKS/ NTPeak,                                                  &
                        AllPkPosVal(MTPeak), AllPkPosEsd(MTPeak),                &
                        AllPkAreaVal(MTPeak),                                    &
                        PkProb(MTPeak),                                          &
                        IOrdTem(MTPeak),                                         &
                        IHPk(3,MTPeak)

      LOGICAL, EXTERNAL :: FnUnitCellOK, Confirm, DASHWDialogGetCheckBoxLogical
      REAL,    EXTERNAL :: TwoTheta2dSpacing
      REAL    Rvpar(2), Rdens, Rmolwt, Rexpzp
      LOGICAL system(6)
      INTEGER I, iOrd, iHandle, NumDoF, iLen
      REAL    Epsilon, MaxLen, MaxSinBeta, tBeta
      CHARACTER*2 nStr

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page8)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_PW_Page7)
            CASE (IDNEXT)
              CALL CheckIfPeaksFitted
              CALL DASHWDialogGetReal(IDF_Indexing_MinVol, Rvpar(1))
              CALL DASHWDialogGetReal(IDF_Indexing_MaxVol, Rvpar(2))
              CALL DASHWDialogGetReal(IDF_Indexing_Maxa,   amax)
              Bmax = amax
              Cmax = amax
              CALL DASHWDialogGetReal(IDF_Indexing_MinAng,      Bemin)
              CALL DASHWDialogGetReal(IDF_Indexing_MaxAng,      Bemax)
              CALL DASHWDialogGetReal(IDF_Indexing_Density,     Rdens)
              CALL DASHWDialogGetReal(IDF_Indexing_MolWt,       Rmolwt)
              CALL DASHWDialogGetReal(IDF_ZeroPoint,            Rexpzp)
              system(1) = DASHWDialogGetCheckBoxLogical(IDF_Indexing_Cubic)
              system(2) = DASHWDialogGetCheckBoxLogical(IDF_Indexing_Tetra)
              system(3) = DASHWDialogGetCheckBoxLogical(IDF_Indexing_Hexa)
              system(4) = DASHWDialogGetCheckBoxLogical(IDF_Indexing_Ortho)
              system(5) = DASHWDialogGetCheckBoxLogical(IDF_Indexing_Monoclinic)
              system(6) = DASHWDialogGetCheckBoxLogical(IDF_Indexing_Triclinic)
              CALL DASHWDialogGetReal(IDF_eps,                  Epsilon)
              CALL DASHWDialogGetReal(IDF_Indexing_Fom,         fom)
              CALL DASHWDialogGetReal(IDF_Indexing_ScaleFactor, DV_ScaleFactor)
! Number of degrees of freedom, we don't even count the zero point
              NumDoF = 0
              IF (system(1)) NumDof = MAX(NumDoF, 1)
              IF (system(2)) NumDof = MAX(NumDoF, 2)
              IF (system(3)) NumDof = MAX(NumDoF, 2)
              IF (system(4)) NumDof = MAX(NumDoF, 3)
              IF (system(5)) NumDof = MAX(NumDoF, 4)
              IF (system(6)) NumDof = MAX(NumDoF, 6)
! Check if any crystal system checked at all
              IF (NumDoF .EQ. 0) THEN
                CALL ErrorMessage('Please check at least one crystal system.')
                GOTO 999
              ENDIF
! Check if the number of observed lines is consistent with the crystal systems
              IF (NTPeak .LT. NumDoF) THEN
                CALL ErrorMessage('The number of observed lines is less than the number of degrees of freedom.')
                GOTO 999
              ENDIF
! Warn the user if we have less observed lines than twice the number of degrees of freedom including the zero point
              IF ((2*(NumDoF+1)) .GT. NTPeak) THEN
                IF (.NOT. Confirm('The number of observed lines is less than twice the number of degrees of freedom,'//CHAR(13)//&
                                  'do you wish to continue anyway?')) THEN
                  GOTO 999
                ENDIF
              ENDIF
              IF (DV_ScaleFactor .LT. 0.1) DV_ScaleFactor = 0.1
              IF (DV_ScaleFactor .GT. 5.0) DV_ScaleFactor = 5.0
! Check if the maximum angle has a reasonable value. Only necessary when monoclinic is searched
              IF (system(5)) THEN
                IF (Bemin .GT. Bemax) THEN
                  tBeta = Bemin
                  Bemin = Bemax
                  Bemax = tBeta
                ENDIF
                IF ((Bemin .LT. 45.0) .OR. (Bemax .GT. 150.0)) THEN
                  CALL ErrorMessage('The range of the angle beta does not make sense.')
                  GOTO 999
                ELSE
! Correct maximum cell length for the angle beta
 ! If 90.0 is inside the range, then that's the maximum
                  IF ((Bemin .LT. 90.0) .AND. (Bemax .GT. 90.0)) THEN
                    MaxSinBeta = 1.0 ! Beta = 90.0
                  ELSE         
                    MaxSinBeta = MAX(SIN(Bemin),SIN(Bemax))
                  ENDIF
                ENDIF
              ELSE
                MaxSinBeta = 1.0 ! Beta = 90.0
              ENDIF
! Add in very quick check: is the d-spacing belonging to the first peak greater
! than the maximum cell length requested? If so, tell the user he/she is a moron.
              MaxLen = MaxSinBeta*amax
! Lowest 2 theta value for which a peak has been fitted: AllPkPosVal(IOrdTem(1))
              IF ((TwoTheta2dSpacing(AllPkPosVal(IOrdTem(1)))*DV_ScaleFactor) .GT. MaxLen) THEN
                CALL ErrorMessage('The maximum cell axis length is shorter than required for indexing the first peak.')
                GOTO 999
              ENDIF
              n = NTPeak
              wave2 = (ALambda / 2) * DV_ScaleFactor
              epst = (Epsilon + 0.015) * DV_ScaleFactor
              DO I = 1, n
                epsil(I) = Epsilon * DV_ScaleFactor
              ENDDO
              DO I = 1, NTPeak
                iOrd = IOrdTem(I)
                d(I) = AllPkPosVal(iOrd) - Rexpzp
              ENDDO
              CALL WCursorShape(CurHourGlass)
              NumOfDICVOLSolutions = 0
              CALL LoadDASHDialog(IDD_DICVOLRunning)
              IF (system(1)) THEN
                CALL WDialogFieldState(IDF_LabCubic            ,DialogReadOnly)
                CALL WDialogFieldState(IDF_LabProgCubic        ,DialogReadOnly)
              ELSE
                CALL WDialogFieldState(IDF_LabCubic            ,Disabled)
                CALL WDialogFieldState(IDF_LabProgCubic        ,Disabled)
              ENDIF
              IF (system(2)) THEN
                CALL WDialogFieldState(IDF_LabTetragonal       ,DialogReadOnly)
                CALL WDialogFieldState(IDF_LabProgTetragonal   ,DialogReadOnly)
              ELSE
                CALL WDialogFieldState(IDF_LabTetragonal       ,Disabled)
                CALL WDialogFieldState(IDF_LabProgTetragonal   ,Disabled)
              ENDIF
              IF (system(3)) THEN
                CALL WDialogFieldState(IDF_LabHexagonal        ,DialogReadOnly)
                CALL WDialogFieldState(IDF_LabProgHexagonal    ,DialogReadOnly)
              ELSE
                CALL WDialogFieldState(IDF_LabHexagonal        ,Disabled)
                CALL WDialogFieldState(IDF_LabProgHexagonal    ,Disabled)
              ENDIF
              IF (system(4)) THEN
                CALL WDialogFieldState(IDF_LabOrthorhombic     ,DialogReadOnly)
                CALL WDialogFieldState(IDF_LabProgOrthorhombic ,DialogReadOnly)
              ELSE
                CALL WDialogFieldState(IDF_LabOrthorhombic     ,Disabled)
                CALL WDialogFieldState(IDF_LabProgOrthorhombic ,Disabled)
              ENDIF
              IF (system(5)) THEN
                CALL WDialogFieldState(IDF_LabMonoclinic       ,DialogReadOnly)
                CALL WDialogFieldState(IDF_LabProgMonoclinic   ,DialogReadOnly)
              ELSE
                CALL WDialogFieldState(IDF_LabMonoclinic       ,Disabled)
                CALL WDialogFieldState(IDF_LabProgMonoclinic   ,Disabled)
              ENDIF
              IF (system(6)) THEN
                CALL WDialogFieldState(IDF_LabTriclinic        ,DialogReadOnly)
                CALL WDialogFieldState(IDF_LabProgTriclinic    ,DialogReadOnly)
              ELSE
                CALL WDialogFieldState(IDF_LabTriclinic        ,Disabled)
                CALL WDialogFieldState(IDF_LabProgTriclinic    ,Disabled)
              ENDIF
              CALL WDialogPutString(IDF_LabProgCubic, ' ')
              CALL WDialogPutString(IDF_LabProgTetragonal, ' ')
              CALL WDialogPutString(IDF_LabProgHexagonal, ' ')
              CALL WDialogPutString(IDF_LabProgOrthorhombic, ' ')
              CALL WDialogPutString(IDF_LabProgMonoclinic, ' ')
              CALL WDialogPutString(IDF_LabProgTriclinic, ' ')
              CALL WDialogShow(-1, -1, 0, Modeless)
              CALL WCursorShape(CurHourGlass)
              CALL DICVOL91(system(1),system(2),system(3),system(4),system(5),system(6),Rvpar(1),Rvpar(2),Rmolwt,Rdens,Rdens/50.0)
              CALL UnloadDASHDialog(IDD_DICVOLRunning)
              CALL WCursorShape(CurCrossHair)
              IF (NumOfDICVOLSolutions .EQ. 0) THEN
! Pop up a window showing the DICVOL output file in a text editor
                CALL WindowOpenChild(iHandle)
                CALL WEditFile(DV_FileName,Modeless, 0, FileMustExist+ViewOnly+NoToolBar, 4)
                CALL SetChildWinAutoClose(iHandle)
                CALL ErrorMessage('No solutions were found.')
! Grey out the "Previous Results >" button in the DICVOL Wizard window
                CALL SelectDASHDialog(IDD_PW_Page8)
                CALL WDialogFieldState(IDB_PrevRes, Disabled)
                GOTO 999
              ENDIF  
              IF (DICVOL_Error .EQ. cDICVOL_TooManySolutions) CALL WarningMessage('More than 30 solutions found, please check your data.')
! If only a single solution, and no valid cell available, import that solution by default
              IF ((NumOfDICVOLSolutions.EQ.1) .AND. (.NOT. FnUnitCellOK())) THEN
! Import the unit cell parameters into DASH
                CellPar(1) = DICVOLSolutions(1)%a
                CellPar(2) = DICVOLSolutions(1)%b
                CellPar(3) = DICVOLSolutions(1)%c
                CellPar(4) = DICVOLSolutions(1)%alpha
                CellPar(5) = DICVOLSolutions(1)%beta
                CellPar(6) = DICVOLSolutions(1)%gamma
                LatBrav = DICVOLSolutions(1)%CrystalSystem
                CALL Upload_CrystalSystem
                CALL UpdateCell
              ENDIF
! Pop up the next Wizard window showing the solutions, so that the user can choose one to be imported into DASH
              CALL SelectDASHDialog(IDD_PW_Page9)
! Clear all fields in the grid
              CALL WDialogClearField(IDF_DV_Summary_0)
              WRITE(nStr,'(I2)') n
              CALL StrClean(nStr,iLen)
              CALL WGridLabelColumn(IDF_DV_Summary_0,10,'M('//nStr(1:iLen)//')')
              CALL WGridLabelColumn(IDF_DV_Summary_0,11,'F('//nStr(1:iLen)//')')
! Set the number of rows in the grid to the number of solutions.
              CALL WGridRows(IDF_DV_Summary_0,NumOfDICVOLSolutions)
              DO I = 1, NumOfDICVOLSolutions
                CALL WGridPutCellString(IDF_DV_Summary_0, 2, I, CrystalSystemString(DICVOLSolutions(I)%CrystalSystem))
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 3, I, DICVOLSolutions(I)%a,'(F8.4)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 4, I, DICVOLSolutions(I)%b,'(F8.4)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 5, I, DICVOLSolutions(I)%c,'(F8.4)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 6, I, DICVOLSolutions(I)%alpha,'(F7.3)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 7, I, DICVOLSolutions(I)%beta,'(F7.3)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 8, I, DICVOLSolutions(I)%gamma,'(F7.3)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 9, I, DICVOLSolutions(I)%Volume,'(F9.2)')
                IF (DICVOLSolutions(I)%M .GT. 0.0) CALL WGridPutCellReal (IDF_DV_Summary_0, 10, I, DICVOLSolutions(I)%M,'(F7.1)')
                IF (DICVOLSolutions(I)%F .GT. 0.0) CALL WGridPutCellReal (IDF_DV_Summary_0, 11, I, DICVOLSolutions(I)%F,'(F7.1)')
              ENDDO
! Ungrey the "Previous Results >" button in the DICVOL Wizard window
              CALL SelectDASHDialog(IDD_PW_Page8)
              CALL WDialogFieldState(IDB_PrevRes,Enabled)
              CALL WizardWindowShow(IDD_PW_Page9)
            CASE (IDB_PrevRes)
              CALL WizardWindowShow(IDD_PW_Page9)
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_ZeroPoint)
              CALL DASHWDialogGetReal(IDF_ZeroPoint,ZeroPoint)
              CALL Upload_ZeroPoint               
          END SELECT
      END SELECT
  999 CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowIndexing2
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowExtDICVOL

      USE DRUID_HEADER
      USE VARIABLES
      USE DICVAR

      IMPLICIT NONE

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      INTEGER           NTPeak
      REAL              AllPkPosVal,         AllPkPosEsd
      REAL              AllPkAreaVal
      REAL              PkProb
      INTEGER           IOrdTem
      INTEGER           IHPk
      COMMON /ALLPEAKS/ NTPeak,                                                  &
                        AllPkPosVal(MTPeak), AllPkPosEsd(MTPeak),                &
                        AllPkAreaVal(MTPeak),                                    &
                        PkProb(MTPeak),                                          &
                        IOrdTem(MTPeak),                                         &
                        IHPk(3,MTPeak)

      LOGICAL, EXTERNAL :: FnUnitCellOK, Confirm
      REAL,    EXTERNAL :: TwoTheta2dSpacing
      INTEGER ParseDICVOLOutputFile
      REAL    Rvpar(2), Rdens, Rmolwt, Rexpzp
      INTEGER iSystem(6), hFile, MaxImpurityLines
      INTEGER I, iOrd, iHandle, NumDoF
      REAL    Epsilon, MaxLen, MaxSinBeta, tBeta, tFoM, wave
      CHARACTER(MaxPathLength) tFileName
      LOGICAL exists
      INTEGER M, iEstimateZeroPointError, iRefineZeroPointError, iExhaustive
!      REAL Amax, Bmax, Cmax, Bemin, Bemax, DV_ScaleFactor
      CHARACTER*2 nStr
      INTEGER nFOM, iLen

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page8b)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_PW_Page7)
            CASE (IDNEXT)
              CALL CheckIfPeaksFitted
              CALL DASHWDialogGetReal(IDF_Indexing_MinVol, Rvpar(1))
              CALL DASHWDialogGetReal(IDF_Indexing_MaxVol, Rvpar(2))
              CALL DASHWDialogGetReal(IDF_Indexing_Maxa,   Amax)
              Bmax = Amax
              Cmax = Amax
              CALL DASHWDialogGetReal(IDF_Indexing_MinAng,      Bemin)
              CALL DASHWDialogGetReal(IDF_Indexing_MaxAng,      Bemax)
              CALL DASHWDialogGetReal(IDF_Indexing_Density,     Rdens)
              CALL DASHWDialogGetReal(IDF_Indexing_MolWt,       Rmolwt)
              CALL DASHWDialogGetReal(IDF_ZeroPoint,            Rexpzp)
              CALL DASHWDialogGetCheckBox(IDF_Indexing_Cubic,      iSystem(1))
              CALL DASHWDialogGetCheckBox(IDF_Indexing_Tetra,      iSystem(2))
              CALL DASHWDialogGetCheckBox(IDF_Indexing_Hexa,       iSystem(3))
              CALL DASHWDialogGetCheckBox(IDF_Indexing_Ortho,      iSystem(4))
              CALL DASHWDialogGetCheckBox(IDF_Indexing_Monoclinic, iSystem(5))
              CALL DASHWDialogGetCheckBox(IDF_Indexing_Triclinic,  iSystem(6))
              CALL DASHWDialogGetReal(IDF_eps,                  Epsilon)
              CALL DASHWDialogGetReal(IDF_Indexing_Fom,         tFoM)
              CALL DASHWDialogGetReal(IDF_Indexing_ScaleFactor, DV_ScaleFactor)
              CALL DASHWDialogGetInteger(IDF_MaxImpLines, MaxImpurityLines)
              CALL DASHWDialogGetCheckBox(IDC_Estimate_zp, iEstimateZeroPointError)
              CALL DASHWDialogGetCheckBox(IDC_Refine_zp, iRefineZeroPointError)
              CALL DASHWDialogGetCheckBox(IDC_Exhaustive, iExhaustive)
! Number of degrees of freedom, we don't even count the zero point
              NumDoF = 0
              IF ( iSystem(1) .EQ. 1 ) NumDof = MAX(NumDoF, 1)
              IF ( iSystem(2) .EQ. 1 ) NumDof = MAX(NumDoF, 2)
              IF ( iSystem(3) .EQ. 1 ) NumDof = MAX(NumDoF, 2)
              IF ( iSystem(4) .EQ. 1 ) NumDof = MAX(NumDoF, 3)
              IF ( iSystem(5) .EQ. 1 ) NumDof = MAX(NumDoF, 4)
              IF ( iSystem(6) .EQ. 1 ) NumDof = MAX(NumDoF, 6)
! Check if any crystal system checked at all
              IF ( NumDoF .EQ. 0 ) THEN
                CALL ErrorMessage('Please check at least one crystal system.')
                GOTO 999
              ENDIF
! Check if the number of observed lines is consistent with the crystal systems
              IF ( NTPeak .LT. NumDoF ) THEN
                CALL ErrorMessage('The number of observed lines is less than the number of degrees of freedom.')
                GOTO 999
              ENDIF
! Warn the user if we have less observed lines than twice the number of degrees of freedom including the zero point
              IF ( (2*(NumDoF+1)) .GT. NTPeak ) THEN
                IF (.NOT. Confirm('The number of observed lines is less than twice the number of degrees of freedom,'//CHAR(13)//&
                                  'do you wish to continue anyway?')) THEN
                  GOTO 999
                ENDIF
              ENDIF
              IF ( DV_ScaleFactor .LT. 0.1 ) DV_ScaleFactor = 0.1
              IF ( DV_ScaleFactor .GT. 5.0 ) DV_ScaleFactor = 5.0
! Check if the maximum angle has a reasonable value. Only necessary when monoclinic is searched
              IF ( iSystem(5) .EQ. 1 ) THEN
                IF (Bemin .GT. Bemax) THEN
                  tBeta = Bemin
                  Bemin = Bemax
                  Bemax = tBeta
                ENDIF
                IF ((Bemin .LT. 45.0) .OR. (Bemax .GT. 150.0)) THEN
                  CALL ErrorMessage('The range of the angle beta does not make sense.')
                  GOTO 999
                ELSE
! Correct maximum cell length for the angle beta
 ! If 90.0 is inside the range, then that's the maximum
                  IF ((Bemin .LT. 90.0) .AND. (Bemax .GT. 90.0)) THEN
                    MaxSinBeta = 1.0 ! Beta = 90.0
                  ELSE         
                    MaxSinBeta = MAX(SIN(Bemin),SIN(Bemax))
                  ENDIF
                ENDIF
              ELSE
                MaxSinBeta = 1.0 ! Beta = 90.0
              ENDIF
! Add in very quick check: is the d-spacing belonging to the first peak greater
! than the maximum cell length requested? If so, tell the user he/she is a moron.
              MaxLen = MaxSinBeta*amax
! Lowest 2 theta value for which a peak has been fitted: AllPkPosVal(IOrdTem(1))
              IF ((TwoTheta2dSpacing(AllPkPosVal(IOrdTem(1)))*DV_ScaleFactor) .GT. MaxLen) THEN
                CALL ErrorMessage('The maximum cell axis length is shorter than required for indexing the first peak.')
                GOTO 999
              ENDIF
              wave = ALambda * DV_ScaleFactor
              CALL WCursorShape(CurHourGlass)
              ! DICVOL04 no longer accepts command line arguments, which means that the user must type
              ! the input and output file names *while DICVOL04 is running*
              ! Fortunately, this can be fed to the program using I/O redirection.
              ! Therefore, we write out a little file that contains the names of the input and the output file
              ! and then I/O redirect the contents of the file to DICVOL04.
              ! Note that
              !
              ! CALL IOSCommand(DICVOLEXE(1:I)//' < in.txt', ProcBlocked)
              !
              ! does not work, but instead we must start a new CMD:
              !
              ! CALL IOSCommand('CMD.EXE /C '//DICVOLEXE(1:I)//' < in.txt', ProcBlocked)
              !
              ! The switch /C causes the CMD.exe to exit when it's finished
              hFile = 116
              OPEN(UNIT=hFile, FILE='in.txt', STATUS='UNKNOWN', ERR=997)
              WRITE(hFile,'(A)',ERR=997) 'DASHDV.in'  !'DICVOL.in'
              WRITE(hFile,'(A)',ERR=997)  DV_FileName !'DICVOL.out'
              CLOSE(hFile)
              CALL IOsDeleteFile(DV_FileName)
! Write it out 
              tFileName = 'DASHDV.in'
              hFile = 117
              OPEN(UNIT=hFile, FILE=tFileName, STATUS='UNKNOWN', ERR=997)
              WRITE(hFile,*,ERR=997) 'DICVOL input file created by DASH'
              WRITE(hFile,'(8(I3,1X))',ERR=997) NTPeak, 2, (iSystem(i),i=1,6)
              WRITE(hFile,'(7(F8.2,1X))',ERR=997) amax, Bmax, Cmax, Rvpar(1), Rvpar(2), Bemin, Bemax
              WRITE(hFile,'(F10.6,1X,3(F8.4,1X))',ERR=997) wave, Rmolwt, Rdens, Rdens/50.0
              WRITE(hFile,'(F5.3,1X,F6.2,1X,I3,3(1X,I1))',ERR=997) Epsilon * DV_ScaleFactor, &
                                            tFoM, MaxImpurityLines, iEstimateZeroPointError, &
                                            iRefineZeroPointError, iExhaustive
              DO I = 1, NTPeak
                iOrd = iOrdTem(i)
                WRITE(hFile,'(F12.4)',ERR=997) AllPkPosVal(iOrd)-Rexpzp
              ENDDO
              CLOSE(hFile)
              ! Launch external DICVOL and wait for it to return
              CALL PushActiveWindowID
              CALL SelectDASHDialog(IDD_Configuration)
              CALL DASHWDialogGetString(IDF_DICVOLExe, DICVOLEXE)
              CALL PopActiveWindowID
              I = LEN_TRIM(DICVOLEXE)
              IF ( I .EQ. 0 ) THEN
                CALL ErrorMessage("DASH could not launch your DICVOL. No executable is currently specified."//CHAR(13)//&
                                  "This can be changed in the Configuration... window"//CHAR(13)//&
                                  "under Options in the menu bar.")
                CALL PopActiveWindowID
                CALL WCursorShape(CurCrossHair)
                RETURN
              ENDIF
              INQUIRE(FILE = DICVOLEXE(1:I),EXIST=exists)
              IF (.NOT. exists) GOTO 998
              M = InfoError(1) ! Clear errors
              CALL IOSCommand('CMD.EXE /C "'//DICVOLEXE(1:I)//'" < in.txt', ProcBlocked)
              IF (InfoError(1) .NE. 0) GOTO 998
              CALL WCursorShape(CurCrossHair)
              NumOfDICVOLSolutions = ParseDICVOLOutputFile(nFOM)
              IF ( NumOfDICVOLSolutions .LE. 0 ) THEN
! Pop up a window showing the DICVOL output file in a text editor
                CALL WindowOpenChild(iHandle)
                CALL WEditFile(DV_FileName, Modeless, 0, FileMustExist+ViewOnly+NoToolBar, 4)
                CALL SetChildWinAutoClose(iHandle)
! Grey out the "Previous Results >" button in the DICVOL Wizard window
                CALL WDialogFieldState(IDB_PrevRes, Disabled)
                CALL ErrorMessage('No solutions were found.')
                GOTO 999
              ENDIF
! If only a single solution, and no valid cell available, import that solution by default
              IF ((NumOfDICVOLSolutions.EQ.1) .AND. (.NOT. FnUnitCellOK())) THEN
! Import the unit cell parameters into DASH
                CellPar(1) = DICVOLSolutions(1)%a
                CellPar(2) = DICVOLSolutions(1)%b
                CellPar(3) = DICVOLSolutions(1)%c
                CellPar(4) = DICVOLSolutions(1)%alpha
                CellPar(5) = DICVOLSolutions(1)%beta
                CellPar(6) = DICVOLSolutions(1)%gamma
                LatBrav = DICVOLSolutions(1)%CrystalSystem
                CALL Upload_CrystalSystem
                CALL UpdateCell
              ENDIF
! Pop up the next Wizard window showing the solutions, so that the user can choose one to be imported into DASH
              CALL SelectDASHDialog(IDD_PW_Page9)
! Clear all fields in the grid
              CALL WDialogClearField(IDF_DV_Summary_0)
              WRITE(nStr,'(I2)') nFOM
              CALL StrClean(nStr,iLen)
              CALL WGridLabelColumn(IDF_DV_Summary_0,10,'M('//nStr(1:iLen)//')')
              CALL WGridLabelColumn(IDF_DV_Summary_0,11,'F('//nStr(1:iLen)//')')
! Set the number of rows in the grid to the number of solutions.
              CALL WGridRows(IDF_DV_Summary_0,NumOfDICVOLSolutions)
              DO I = 1, NumOfDICVOLSolutions
                CALL WGridPutCellString(IDF_DV_Summary_0, 2, I, CrystalSystemString(DICVOLSolutions(I)%CrystalSystem))
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 3, I, DICVOLSolutions(I)%a,'(F8.4)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 4, I, DICVOLSolutions(I)%b,'(F8.4)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 5, I, DICVOLSolutions(I)%c,'(F8.4)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 6, I, DICVOLSolutions(I)%alpha,'(F7.3)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 7, I, DICVOLSolutions(I)%beta,'(F7.3)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 8, I, DICVOLSolutions(I)%gamma,'(F7.3)')
                CALL WGridPutCellReal  (IDF_DV_Summary_0, 9, I, DICVOLSolutions(I)%Volume,'(F9.2)')
                IF (DICVOLSolutions(I)%M .GT. 0.0) CALL WGridPutCellReal (IDF_DV_Summary_0, 10, I, DICVOLSolutions(I)%M,'(F7.1)')
                IF (DICVOLSolutions(I)%F .GT. 0.0) CALL WGridPutCellReal (IDF_DV_Summary_0, 11, I, DICVOLSolutions(I)%F,'(F7.1)')
              ENDDO
! Ungrey the "Previous Results >" button in the DICVOL Wizard window
              CALL SelectDASHDialog(IDD_PW_Page8b)
              CALL WDialogFieldState(IDB_PrevRes,Enabled)
              CALL WizardWindowShow(IDD_PW_Page9)
            CASE (IDB_PrevRes)
              CALL WizardWindowShow(IDD_PW_Page9)
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_ZeroPoint)
              CALL DASHWDialogGetReal(IDF_ZeroPoint,ZeroPoint)
              CALL Upload_ZeroPoint               
          END SELECT
      END SELECT
  999 CALL PopActiveWindowID
      RETURN
  998 CALL ErrorMessage("DASH could not launch DICVOL. The executable is currently configured"//CHAR(13)//&
                        "to launch the program "//TRIM(DICVOLEXE)//CHAR(13)//&
                        "This can be changed in the Configuration... window"//CHAR(13)//&
                        "under Options in the menu bar.")
      CALL PopActiveWindowID
      CALL WCursorShape(CurCrossHair)
      RETURN
  997 CALL ErrorMessage("Error writing DICVOL input file.")
      CLOSE(hFile)
      CALL PopActiveWindowID
      CALL WCursorShape(CurCrossHair)

      END SUBROUTINE DealWithWizardWindowExtDICVOL
!
!*****************************************************************************
!
      INTEGER FUNCTION ParseDICVOLOutputFile(nFOM)

      USE DICVAR

      IMPLICIT NONE

      INTEGER, INTENT (OUT) :: nFOM

      CHARACTER*1, EXTERNAL ::  ChrUpperCase, ChrLowerCase
      INTEGER hFile
      PARAMETER ( hFile = 117 )
      INTEGER iLen, iPos, iStage, nSolutions, i, j
      LOGICAL SolutionAppend
      CHARACTER(255) tLine
      CHARACTER(10) sys_str
      REAL          a, b, c, alpha, betaa, gamma, Volume, M, F

      ParseDICVOLOutputFile = 0
      nSolutions = 0
      nFOM = 0
      SolutionAppend = .FALSE.
      iStage = 0
      OPEN(UNIT=hFile, FILE=DV_FileName, STATUS='OLD', ERR=999)
      DO WHILE ( .TRUE. )
        READ(hFile, '(A)', ERR=999, END=100) tLine
        iLen = LEN_TRIM(tLine)
        IF ( iLen .LT. 1 ) CYCLE
    
!        iPos =  INDEX(tLine, 'S Y S T E M', back=.TRUE.)
        iPos =  MatchAnchoredString(tLine, 'S Y S T E M', .FALSE.)
        IF ( iPos .GT. 1 ) THEN
          IF ( SolutionAppend ) CALL AddOneSolution(nSolutions)
          iPos = iPos - 1
          j = 0
          sys_str = ' '
          DO i = 1, iPos
            IF ( tLine(i:i) .EQ. ' ' ) CYCLE
            j = j + 1
            IF ( j .GT. LEN(sys_str) ) EXIT
            sys_str(j:j) = tLine(i:i)
!            IF ( j .EQ. 1 ) THEN
!              sys_str(j:j) = ChrUpperCase(tLine(i:i))
!            ELSE
!              sys_str(j:j) = ChrLowerCase(tLine(i:i))
!            ENDIF
          ENDDO
          IF ( j .GT. 0 ) iStage = 1
          CYCLE
        ENDIF
  
        SELECT CASE (iStage)
          CASE (1)
 !           iPos = INDEX(tLine, 'DIRECT PARAMETERS')
            iPos = MatchAnchoredString(tLine, 'DIRECT PARAMETERS', .TRUE.)
            IF ( iPos .LE. 0 ) CYCLE
            M = 0.0
            F = 0.0
! DICVOL does not test for Trigonal and Rhombohedral systems
            a = 0.0
            alpha = 90.0
            betaa = 90.0
            gamma = 90.0
            IF ( sys_str(1:4) .EQ. 'HEXA' ) gamma = 120.0
            iPos = INDEX(tLine, ' A=')
            IF ( iPos .GT. 0 ) THEN
              iPos = INDEX(tLine, ' ALPHA=')
              IF ( iPos .GT. 0 ) READ(tLine(iPos+7:), *, ERR=999, END=100) alpha 
              iPos = INDEX(tLine, ' BETA=')
              IF ( iPos .GT. 0 ) READ(tLine(iPos+6:), *, ERR=999, END=100) betaa
              iPos = INDEX(tLine, ' GAMMA=')
              IF ( iPos .GT. 0 ) READ(tLine(iPos+7:), *, ERR=999, END=100) gamma
              iPos = INDEX(tLine, ' VOLUME=')
              IF ( iPos .GT. 0 ) READ(tLine(iPos+8:), *, ERR=999, END=100) Volume
            ELSE
              READ(hFile, '(A)', ERR=999, END=100) tLine
              iPos = INDEX(tLine, ' ALP=')
              IF ( iPos .GT. 0 ) READ(tLine(iPos+5:), *, ERR=999, END=100) alpha 
              iPos = INDEX(tLine, ' BET=')
              IF ( iPos .GT. 0 ) READ(tLine(iPos+5:), *, ERR=999, END=100) betaa
              iPos = INDEX(tLine, ' GAM=')
              IF ( iPos .GT. 0 ) READ(tLine(iPos+5:), *, ERR=999, END=100) gamma
              iPos = INDEX(tLine, ' VOL=')
              IF ( iPos .GT. 0 ) READ(tLine(iPos+5:), *, ERR=999, END=100) Volume
            ENDIF
            iPos = INDEX(tLine, ' A=')
            IF ( iPos .GT. 0 ) READ(tLine(iPos+3:), *, ERR=999, END=100) a
            b = a
            c = a
            iPos = INDEX(tLine, ' B=')
            IF ( iPos .GT. 0 ) READ(tLine(iPos+3:), *, ERR=999, END=100) b 
            iPos = INDEX(tLine, ' C=')
            IF ( iPos .GT. 0 ) READ(tLine(iPos+3:), *, ERR=999, END=100) c 
            IF ( a .EQ. 0.0 ) GOTO 999
            nSolutions = nSolutions + 1
            IF ( nSolutions .GT. MaxDICVOLSolutions ) GOTO 996
            SolutionAppend = .TRUE.
            iStage = 2
          CASE (2)
!            iPos = INDEX(tLine, '* FIGURES OF MERIT')
            iPos = MatchAnchoredString(tLine, '* FIGURES OF MERIT', .TRUE.)
            IF ( iPos .LE. 0 ) CYCLE
            READ(hFile, '(A)', ERR=999, END=100) tLine
            IF ( nFOM .EQ. 0 ) THEN
              iPos = INDEX(tLine,'M(') + 2
              READ(tLine(iPos:), *, ERR=999, END=100) nFOM
            ENDIF
            iPos = INDEX(tLine,'=') + 1
            READ(tLine(iPos:), *, ERR=999, END=100) M 
    
            READ(hFile, '(A)', ERR=999, END=100) tLine
            iPos = INDEX(tLine,'=') + 1
            READ(tLine(iPos:iPos+6), *, ERR=999, END=100) F 
            iStage = 0
        END SELECT
      ENDDO

  100 CLOSE(hFile)
      IF ( SolutionAppend ) CALL AddOneSolution(nSolutions)

      ParseDICVOLOutputFile = nSolutions
      RETURN

996   WRITE(sys_str,'(I2)') MaxDICVOLSolutions
      CALL WarningMessage('More than '//TRIM(sys_str)// &
                          ' solutions found, please check your data.')
      GOTO 100
999   CALL ErrorMessage("Error reading DICVOL output file.")
      SolutionAppend = .FALSE.
      nSolutions = 0
      GOTO 100

      CONTAINS
      
! Match 'string' anchored at left/right of 'line' (exclude lead/tail spaces)
! Return its position if matched, 0 otherwise
      INTEGER FUNCTION MatchAnchoredString(line, string, left)

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN) :: line, string
      LOGICAL, INTENT (IN) :: left
      INTEGER p, l

      MatchAnchoredString = 0
      l = LEN(string)
      IF (left) THEN
        p = VERIFY(line,' '//CHAR(9)) ! space or tab
        IF ( l .GT. 0 .AND. p .GT. 0 .AND. LEN(line) .GE. l+p-1 ) THEN
          IF ( line(p:p+l-1) .EQ. string ) MatchAnchoredString = p
        ENDIF
      ELSE
        p = LEN_TRIM(line) - l + 1
        IF ( l .GT. 0 .AND. p .GT. 0 ) THEN
          IF ( line(p:p+l-1) .EQ. string ) MatchAnchoredString = p
        ENDIF
      ENDIF
      RETURN

      END FUNCTION MatchAnchoredString
      
! Map DICVOL crystal system string to DASH
      INTEGER FUNCTION MapLatBrav(str, alpha, gamma)

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN) :: str
      REAL, INTENT (IN) :: alpha, gamma

      SELECT CASE (str(1:4))
        CASE ('MONO')
          IF ( ABS(alpha-90.0) .GT. 1E-7 ) THEN
            MapLatBrav = 2
          ELSE IF ( ABS(gamma-90.0) .GT. 1E-7 ) THEN
            MapLatBrav = 4
          ELSE
            MapLatBrav = 3
          ENDIF
        CASE ('ORTH')
          MapLatBrav = 5
        CASE ('TETR')
          MapLatBrav = 6
        CASE ('TRIG')
          MapLatBrav = 7
        CASE ('RHOM')
          MapLatBrav = 8
        CASE ('HEXA')
          MapLatBrav = 9
        CASE ('CUBI')
          MapLatBrav = 10
        CASE DEFAULT
          MapLatBrav = 1
      END SELECT
      RETURN

      END FUNCTION MapLatBrav

      SUBROUTINE AddOneSolution(I)

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: I

      DICVOLSolutions(I)%a = a
      DICVOLSolutions(I)%b = b
      DICVOLSolutions(I)%c = c
      DICVOLSolutions(I)%alpha = alpha
      DICVOLSolutions(I)%beta = betaa
      DICVOLSolutions(I)%gamma = gamma
      DICVOLSolutions(I)%Volume = Volume
      DICVOLSolutions(I)%CrystalSystem = MapLatBrav(sys_str(1:4), alpha, gamma)
      DICVOLSolutions(I)%M = M
      DICVOLSolutions(I)%F = F
      SolutionAppend = .FALSE.
      
      END SUBROUTINE AddOneSolution

      END FUNCTION ParseDICVOLOutputFile
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowDICVOLResults

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE DICVAR

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      INTEGER iRow, iStatus, iHandle
      CHARACTER(MaxPathLength) :: tFileName
      CHARACTER(LEN=45) :: FILTER
      INTEGER iFlags

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page9)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
            CASE (IDBACK)
              CALL PushActiveWindowID
              IF (DICVOL_ver .EQ. DICVOL_internal) THEN
                CALL WizardWindowShow(IDD_PW_Page8)
              ELSE
                CALL WizardWindowShow(IDD_PW_Page8b)
              ENDIF
              CALL PopActiveWindowID
            CASE (IDNEXT)
              CALL WizardWindowShow(IDD_PW_Page1)
            CASE (IDB_View)
! Pop up a window showing the DICVOL output file in a text editor
              CALL WindowOpenChild(iHandle)
              CALL WEditFile(DV_FileName, Modeless, 0, FileMustExist+ViewOnly+NoToolBar, 4)
! If 'ViewOnly' is specified:
! 1. The file can be accessed while it is displayed.
! 2. There is no 'Save as...' option in the menu.
! If the output file is viewed without 'ViewOnly', the file cannot be accessed, which means that
! DICVOL returns with an error message which means that there are no solutions.
! Hence, this way, DICVOL can be run several times in succession and the results can be compared
! on screen. To save one of the output files (that all have the same name),
! the user must use the "Save As..." button from the same window.
              CALL SetChildWinAutoClose(iHandle)
            CASE (IDBSAVE)
              iFlags = SaveDialog + AppendExt + PromptOn
              FILTER = 'DICVOL output files (*.out)|*.out|'
              tFileName = ''
              CALL WSelectFile(FILTER,iFlags,tFileName,'Save DICVOL output file')
! The following is also correct if we save to a different directory.
              IF ((WInfoDialog(4) .EQ. CommonOK) .AND. (LEN_TRIM(tFileName) .NE. 0)) THEN
                CALL IOsCopyFile(DV_FileName,tFileName)
              ENDIF
          END SELECT
          CALL PopActiveWindowID
          RETURN
      END SELECT
      DO iRow = 1, NumOfDICVOLSolutions
        CALL DASHWGridGetCellCheckBox(IDF_DV_Summary_0, 1, iRow, iStatus)
        IF (iStatus .EQ. 1) THEN
! Import the unit cell parameters into DASH
          CellPar(1) = DICVOLSolutions(iRow)%a
          CellPar(2) = DICVOLSolutions(iRow)%b
          CellPar(3) = DICVOLSolutions(iRow)%c
          CellPar(4) = DICVOLSolutions(iRow)%alpha
          CellPar(5) = DICVOLSolutions(iRow)%beta
          CellPar(6) = DICVOLSolutions(iRow)%gamma
          LatBrav = DICVOLSolutions(iRow)%CrystalSystem
          CALL WGridPutCellCheckBox(IDF_DV_Summary_0, 1, iRow, 0)
!C Reset the space group to "no systematic absences"
          NumberSGTable = 1
          CALL Upload_CrystalSystem
          CALL UpdateCell
        ENDIF
      ENDDO               
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowDICVOLResults
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowUnitCellParameters

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      LOGICAL SpaceGroupDetermination
      COMMON /SGFLAG/ SpaceGroupDetermination

      INTEGER iOption
      LOGICAL, EXTERNAL :: Confirm

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page1)
      SpaceGroupDetermination = .FALSE. 
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCLOSE, IDCANCEL)
              CALL EndWizard
              CALL Download_SpaceGroup(IDD_PW_Page1)
              CALL Download_Cell_Constants(IDD_PW_Page1)
            CASE (IDBACK)
              CALL Download_SpaceGroup(IDD_PW_Page1)
              CALL Download_Cell_Constants(IDD_PW_Page1)
! There were two ways to get here:
!   1. choose 'Enter known cell' in wizard window Indexing I
!   2. after DICVOL, after choosing 'Index now' in wizard window Indexing I
! Did we get here from 'Enter known cell' in wizard window Indexing I?
              CALL SelectDASHDialog(IDD_PW_Page7)
              CALL DASHWDialogGetRadioButton(IDF_RADIO3, iOption) ! 'Index now' or 'Enter known cell'
              IF (iOption .LE. 2) THEN
                CALL WizardWindowShow(IDD_PW_Page9)
              ELSE IF (iOption .EQ. 3) THEN
                CALL WizardWindowShow(IDD_PW_Page8c)
              ELSE
                CALL WizardWindowShow(IDD_PW_Page7)
              ENDIF
            CASE (IDNEXT)
              CALL Download_SpaceGroup(IDD_PW_Page1)
              CALL Download_Cell_Constants(IDD_PW_Page1)
              CALL CheckUnitCellConsistency
              IF (NumberSGTable .EQ. LPosSG(LatBrav)) CALL WarningMessage('Space-group symmetry has not been reset.')
              CALL SelectDASHDialog(IDD_PW_Page10)
              CALL WDialogPutString(IDF_LABEL5, 'The next step is Pawley Refinement')  
              CALL WizardWindowShow(IDD_PW_Page10)
            CASE (IDAPPLY)
              CALL DASHWDialogGetReal(IDF_ZeroPoint, ZeroPoint)
              CALL Upload_ZeroPoint               
              CALL Download_SpaceGroup(IDD_PW_Page1)
              CALL Download_Cell_Constants(IDD_PW_Page1)
              CALL CheckUnitCellConsistency
            CASE (IDBBROWSE) ! Read unit cell
              CALL UnitCellParametersFileBrowse
            CASE (IDB_Delabc)
              CALL Clear_UnitCell_WithConfirmation
! Space Group Program Interface
            CASE (IDF_SGDet) 
               NumberSGTable = LPosSG(LatBrav) ! Most general space group of crystal system chosen
               CALL SetSpaceGroupMenu 
               CALL Generate_TicMarks
               CALL Download_SpaceGroup(IDD_PW_Page1) 
               SpaceGroupDetermination = .TRUE.
               CALL SelectDASHDialog(IDD_PW_Page10)
               CALL WDialogPutString(IDF_LABEL5, 'The next step in Space Group Determination is Pawley Refinement')
               CALL WizardWindowShow(IDD_PW_Page10)
          END SELECT
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_a_latt)
              CALL DASHWDialogGetReal(IDF_a_latt, CellPar(1))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_b_latt)
              CALL DASHWDialogGetReal(IDF_b_latt, CellPar(2))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_c_latt)
              CALL DASHWDialogGetReal(IDF_c_latt, CellPar(3))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_alp_latt)
              CALL DASHWDialogGetReal(IDF_alp_latt, CellPar(4))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_bet_latt)
              CALL DASHWDialogGetReal(IDF_bet_latt, CellPar(5))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_gam_latt)
              CALL DASHWDialogGetReal(IDF_gam_latt, CellPar(6))
              CALL UpdateCell
              CALL CheckUnitCellConsistency
            CASE (IDF_Crystal_System_Menu)
              CALL DASHWDialogGetMenu(IDF_Crystal_System_Menu, LatBrav)
              CALL Upload_CrystalSystem
              CALL Generate_TicMarks
            CASE (IDF_Space_Group_Menu)
              CALL Download_SpaceGroup(IDD_PW_Page1)
              CALL Generate_TicMarks
            CASE (IDF_ZeroPoint)
              CALL DASHWDialogGetReal(IDF_ZeroPoint, ZeroPoint)
              CALL Upload_ZeroPoint               
              CALL Generate_TicMarks
          END SELECT                
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowUnitCellParameters
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowPawley1

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      LOGICAL SpaceGroupDetermination
      COMMON /SGFLAG/ SpaceGroupDetermination

      LOGICAL, EXTERNAL :: Confirm

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page10)
      SELECT CASE (EventType)
        CASE (PushButton)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              SpaceGroupDetermination = .FALSE.
              CALL WizardWindowShow(IDD_PW_Page1)
            CASE (IDNEXT)
              CALL ShowPawleyFitWindow
            CASE (IDCANCEL, IDCLOSE)
              SpaceGroupDetermination = .FALSE.
              CALL EndWizard
            CASE (IDF_ClearPeakFitRanges)
              IF (Confirm('Do you wish to delete all peak fit ranges?')) CALL Clear_PeakFitRanges
!            CASE (IDF_AutoSelectPeaks)
!                IF (Confirm('Do you wish to attempt to select suitable peaks?')) THEN
!                    CALL Clear_PeakFitRanges
!                    CALL AutoFitPeaksForPawley
!                ENDIF
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowPawley1
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardWindowMcMaille

      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'

      LOGICAL, EXTERNAL :: FnUnitCellOK, LaunchMcMaille
      INTEGER Ngrid

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page8c)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_PW_Page7)
            CASE (IDNEXT)
              CALL CheckIfPeaksFitted
              CALL DASHWDialogGetRadioButton(IDF_RADIO1, iMcMailleNgridOpt)
              Ngrid = 3
              IF ( iMcMailleNgridOpt .EQ. 2 ) Ngrid = -3
              IF ( iMcMailleNgridOpt .EQ. 3 ) Ngrid = 4
              IF ( LaunchMcMaille(Ngrid) ) THEN
                  CALL SelectDASHDialog(IDD_PW_Page1)
! If the cell is OK, the Next> button should be enabled
                  CALL WDialogFieldStateLogical(IDNEXT, FnUnitCellOK())
                  CALL WizardWindowShow(IDD_PW_Page1)
              ENDIF
            CASE (IDCANCEL, IDCLOSE)
              CALL EndWizard
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardWindowMcMaille
!
!*****************************************************************************
!
      LOGICAL FUNCTION LaunchMcMaille(Ngrid)

      USE DRUID_HEADER
      USE VARIABLES
      USE DICVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: Ngrid

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'

      INTEGER           NTPeak
      REAL              AllPkPosVal,         AllPkPosEsd
      REAL              AllPkAreaVal
      REAL              PkProb
      INTEGER           IOrdTem
      INTEGER           IHPk
      COMMON /ALLPEAKS/ NTPeak,                                                  &
                        AllPkPosVal(MTPeak), AllPkPosEsd(MTPeak),                &
                        AllPkAreaVal(MTPeak),                                    &
                        PkProb(MTPeak),                                          &
                        IOrdTem(MTPeak),                                         &
                        IHPk(3,MTPeak)

      INTEGER I, M, iOrd, hFile
      CHARACTER(MaxPathLength) tDirName, tFileName !, tCurrentDirName
      LOGICAL exists
      
      LaunchMcMaille = .FALSE.
      IF (NTPeak .LE. 3) GOTO 994
      hFile = 117
      OPEN(UNIT=hFile, FILE='DASHMcMaille.dat', STATUS='UNKNOWN', ERR=997)
      WRITE(hFile,*,ERR=997) 'McMaille input file created by DASH'
      WRITE(hFile,'(F10.6,1X,F10.5,1X,I5)',ERR=997) ALambda, 0.0, Ngrid
      WRITE(hFile, '(A)' ,ERR=997) '!    2-Theta    Intensity'
      DO I = 1, NTPeak
        iOrd = iOrdTem(i)
        WRITE(hFile,'(F12.5,1X,F12.2)',ERR=997) AllPkPosVal(iOrd), AllPkAreaVal(iOrd)
      ENDDO
      CLOSE(hFile)
      IF (LEN_TRIM(McMailleEXE) .EQ. 0) GOTO 998
      INQUIRE(FILE=McMailleEXE, EXIST=exists)
      IF (.NOT. exists) GOTO 999
! McMaille requires to run from its install directory, or copy all .hkl files
      CALL SplitPath(McMailleEXE, tDirName, tFileName)
! There is bug (McMaille-v4) in handling path with spaces, the author has agreed to fix
! it in future release. Use copy .hkl method as a work-around
!      CALL IOsDirName(tCurrentDirName)
!      CALL IOsDirChange(tDirName)
      CALL IOsCopyFile(TRIM(tDirName)//'*.hkl', '.')
      M = InfoError(1) ! Clear errors
!      CALL IOSCommand('CMD /C '//TRIM(tFileName)//' "'//TRIM(tCurrentDirName)//'\DASHMcMaille" && '// &
!                      'NOTEPAD "'//TRIM(tCurrentDirName)//'\DASHMcMaille.imp"')
      CALL IOSCommand('CMD /C "'//TRIM(McMailleEXE)//'" DASHMcMaille && '// &
                      'NOTEPAD DASHMcMaille.imp')
      M = InfoError(1)
!      CALL IOsDirChange(tCurrentDirName)
      IF (M .NE. 0) GOTO 999
      LaunchMcMaille = .TRUE.
      RETURN
 
  997 CALL ErrorMessage("Error writing McMaille input file.")
      CLOSE(hFile)
      RETURN
  998 CALL ErrorMessage("DASH could not launch McMaille. No executable is currently specified."//CHAR(13)//&
                        "This can be changed in the Configuration... window"//CHAR(13)//&
                        "under Options in the menu bar.")
      RETURN
  999 CALL ErrorMessage("DASH could not launch McMaille. The executable is currently configured"//CHAR(13)//&
                        "to launch the program "//TRIM(McMailleEXE)//CHAR(13)//&
                        "This can be changed in the Configuration... window"//CHAR(13)//&
                        "under Options in the menu bar.")
      RETURN
  994 CALL ErrorMessage("DASH could not launch McMaille. Too few peak are currently fitted. ")
      RETURN

      END FUNCTION LaunchMcMaille
!
!*****************************************************************************
!
