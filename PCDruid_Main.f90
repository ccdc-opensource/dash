! JvdS The following global variables seem to be needed:
!
! - start of profile (to indicate range)
! - end  of profile
! - filename (this is now done partially by 'FNAME', which is also used as a dummy)

! NoData is regularly set to .FALSE. but is it ever set to .TRUE.?
! Length of SDIFile/SDIFileName inconsistent (especially suspect: Pawley.for around line 1326/1273
! length was 255, is now 80)
! Many bugs when selecting space group

      MODULE VARIABLES

      USE WINTERACTER
!
!   Shared variables for any routine with 'USE VARIABLES'
!
      IMPLICIT NONE
!
      LOGICAL                       :: SAVEF = .FALSE. ! File needs saving

      INTEGER, PARAMETER :: MaxPathLength =  255
      LOGICAL                       :: PLOTT = .FALSE. ! Graphic plotted?
      CHARACTER(LEN=MaxPathLength)  :: FNAME = ' '     ! Current filename
!C>> JCC Added these in for portability reasons       
      CHARACTER(LEN=MaxPathLength)  :: INSTDIR =     'C:\Program Files\DASH' ! Default installation directory
      CHARACTER(LEN=21)             :: SPACEGROUPS = 'SpaceGroupSymbols.dat' ! Table name
      CHARACTER                     :: DIRSPACER   = '\' ! Windows spacer
      CHARACTER(LEN=8)              :: CONFIG      = 'Dash.cfg'
!C>> External binaries
      CHARACTER(LEN=MaxPathLength)  :: VIEWEXE     = 'C:\Program Files\DASH\mercury.exe'
      CHARACTER(LEN=MaxPathLength)  :: CONVEXE     = 'C:\Program Files\DASH\zmconv.exe'
      CHARACTER(LEN=20)             :: VIEWARG     = ''

      LOGICAL ViewOn
      LOGICAL ConvOn       ! Set if external z-matix conversion program available
      LOGICAL ViewAct
      LOGICAL AutoUpdate
      COMMON / EXTPRG / ViewOn, ConvOn,  ViewAct,  AutoUpdate, ViewExe, ConvExe, ViewArg
      COMMON / EXTDIR / INSTDIR

!C>> File information; Names of files used by DASH For I/O
      CHARACTER(LEN = MaxPathLength) :: DashRawFile
      CHARACTER(LEN = MaxPathLength) :: DashHcvFile
      CHARACTER(LEN = MaxPathLength) :: DashPikFile
      CHARACTER(LEN = MaxPathLength) :: DashTicFile
      LOGICAL RawExists
      LOGICAL HcvExists
      LOGICAL PikExists
      LOGICAL TicExists

! JvdS @ These are needed at startup only: should be made local
!C>> License information
      INTEGER :: EXPIRY_DAY   =   31
      INTEGER :: EXPIRY_MONTH =   12
      INTEGER :: EXPIRY_YEAR  = 2000
      CHARACTER(LEN=18) :: EXPIRY_STRING = '31st December 2000'

! JvdS @ These are needed at startup only: should be made local
!C>> New license information structure    
      TYPE License_Info
        INTEGER          :: Day
        INTEGER          :: Month
        INTEGER          :: Year
        INTEGER          :: DateCode
        INTEGER          :: SerialNumber
        INTEGER          :: LicenseType
        INTEGER          :: Valid
      END TYPE
      INTEGER NodeKey,DemoKey,SiteKey
      PARAMETER (NodeKey = 1)
      PARAMETER (DemoKey = 2)
      PARAMETER (SiteKey = 3)

      INTEGER           :: EventType
      TYPE(WIN_MESSAGE) :: EventInfo
! These global variables hold the last event reported by Winteracter

      END MODULE VARIABLES
!
!*****************************************************************************
!
      PROGRAM PCDruid_Main

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE
!
!   Type declarations
!
      TYPE(WIN_STYLE)   :: MAIN_WINDOW
!
!   Variable declarations
!
      LOGICAL           :: NoData  = .TRUE.
      INTEGER           :: IWIDTHS(10)
      INTEGER           :: IWID

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'lattice.inc'

      INCLUDE 'STATLOG.INC'
      LOGICAL Run_Wizard

      CALL WInitialise(' ')
      CALL Init_StdOut()
!   Initialise Winteracter
      XBSWidth   = WInfoScreen(1)
      XBSHeight  = WInfoScreen(2)
      XBSColours = WInfoScreen(3)
!>> JCC
!>> Try to redirect stdout - change working directory if unsuccessful

!   Set up root window options
!
!     - System menu
!     - Minimise button
!     - Maximise button
!     - Status bar
!
      MAIN_WINDOW%FLAGS  = SysMenuOn + MinButton + MaxButton + StatusBar
!   Place window on the screen with a fractional size determined from the screen size
      MAIN_WINDOW%WIDTH  = 0.8*XBSWidth
      MAIN_WINDOW%HEIGHT = 0.375*XBSHeight
      MAIN_WINDOW%X      = 0.1*XBSWidth
      MAIN_WINDOW%Y      = 0.06*FLOAT(XBSHeight)+262 !0.4*XBSHeight
!   Set druid_header menu id and window title
      MAIN_WINDOW%MENUID = IDR_MENU1
      MAIN_WINDOW%TITLE  = "DASH"
!   Open root window
      CALL WindowOpen(MAIN_WINDOW,128)
!   Load and display the toolbar
      CALL WMenuToolbar(ID_TOOLBAR1)
! Disable the menu buttons
      CALL SetModeMenuState(-1,-1,-1)
!   Setup array of widths for status bar
      IWIDTHS(1) = 3800
      DO IWID = 2, 7
        IWIDTHS(IWID) = 800
      END DO
      IWIDTHS(8)= 1500 
!   Split status bar into more than one part
      CALL WindowStatusBarParts(8,IWIDTHS)
      CALL WMessageEnable(FieldChanged, Enabled)
      CALL WMessageEnable(PushButton  , Enabled)
! Load all Winteracter dialogues into memory
      CALL PolyFitter_UploadDialogues()
! Initialise space group information
      CALL PolyFitterInitialise()
      CALL InitialiseVariables
      CALL Check_License()
! JvdS Now we can remove the licence dialogue from memory:
      CALL WDialogSelect(IDD_License_Dialog)
      CALL WDialogUnload
      CALL WDialogSelect(0)
!   Main message loop
!   Go through the PolyFitter wizard
!c>> Comment this next line out to remove the wizard
      NoData = Run_Wizard()
      CALL Generate_TicMarks()
!C JCC Enable tab changing
      CALL WMessageEnable(TabChanged, Enabled)
      DO WHILE(.TRUE.)
        CALL GetEvent
        CALL process_mainwindow_message
      END DO

      END PROGRAM PCDruid_Main
!
!*****************************************************************************
!
      SUBROUTINE process_mainwindow_message

      USE VARIABLES
      USE DRUID_HEADER
      USE WINTERACTER

      SELECT CASE (EventType)
        CASE (MouseButDown)
          CALL Plot_Alter
        CASE (PushButton)
          CALL Check_PushButton
        CASE (KeyDown)
          CALL Check_KeyDown
        CASE (MenuSelect)
          CALL ProcessMenu
        CASE (FieldChanged)
          CALL Main_Field_Changed_Routines(EventInfo%Value1,EventInfo%Value2)
        CASE (TabChanged)
          CALL Main_Field_Changed_Routines(EventInfo%Value1,EventInfo%Value2)
        CASE (Expose,Resize)
          CALL Redraw()
      END SELECT

      END SUBROUTINE process_mainwindow_message
!
!*****************************************************************************
!
      SUBROUTINE ProcessMenu
!
!   This subroutine processes the menu selections
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
!
      IMPLICIT NONE
!
      LOGICAL NoData

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'

!C>> JCC data to indicate whether we are coming out of peak-fitting mode
      LOGICAL FromPeakFit
      LOGICAL Run_Wizard ! Function
      LOGICAL Confirm ! Function

      FromPeakFit = .FALSE.
!
!   Branch depending on chosen menu item
!
 10   CALL WCursorShape(CurArrow)
      STATBARSTR(8)=' '
      CALL WindowOutStatusBar(8,STATBARSTR(8))
      SELECT CASE (EventInfo%VALUE1)
        CASE (ID_import_xye_file)
          CALL Diffraction_File_Browse(NoData)
        CASE (ID_import_dpj_file)
          CALL SDI_file_Open(NoData)
        CASE (ID_Structure_Solution_Mode)
          CALL SA_Main()
!        CASE (ID_FILE_SAVE)
!          CALL Save(0)
!        CASE (ID_FILE_SAVEAS)
!          CALL Save(1)
        CASE (ID_FILE_PRINT)
          CALL Profile_Plot(-IPTYPE)
        CASE (ID_FILE_EXIT)
          CALL WExit
!        CASE (ID_EDIT_DIAG)
!          CALL Edit()
!        CASE (ID_PLOT)
!          CALL Profile_Plot(IPTYPE)
        CASE (ID_Plot_Options)
          CALL PushActiveWindowID
          CALL WDialogSelect(IDD_Plot_Option_Dialog)
          CALL WDialogShow(-1,-1,0,Modeless)
          CALL PopActiveWindowID
        CASE (ID_Default_Mode)
          STATBARSTR(8)='Default visualisation mode'
          CALL WindowOutStatusBar(8,STATBARSTR(8))
          CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
          IDCurrent_Cursor_mode=ID_Default_Mode
          CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOn)
!        CASE (ID_CrossHair_Cursor_Mode)
!          STATBARSTR(8)='Cursor Mode'
!          CALL WindowOutStatusBar(8,STATBARSTR(8))
!          CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
!          IDCurrent_Cursor_mode=ID_CrossHair_Cursor_Mode
!          CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOn)
!          CALL PeakFind_Manual(IDENT)
!          Goto 10
        CASE (ID_Peak_Fitting_Mode)
          STATBARSTR(8)='Peak fitting mode'
          CALL WindowOutStatusBar(8,STATBARSTR(8))
          CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
          IDCurrent_Cursor_mode=ID_Peak_Fitting_Mode
          CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOn)
          FromPeakFit = .TRUE.
          CALL PeakFit(EventInfo%VALUE1)
          GOTO 10
        CASE (ID_Pawley_Refinement_Mode)
          IF (NumPawleyRef .EQ. 0) THEN
            IF (.NOT. Confirm('Lattice constants may not have been refined'//CHAR(13)//&
                              'Do you wish to continue?')) RETURN
          END IF
          CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
          IDCurrent_Cursor_mode=ID_Pawley_Refinement_Mode
          CALL WMenuSetState(ID_Pawley_Refinement_Mode,ItemChecked,WintOn)
          STATBARSTR(8)='Pawley refinement mode'
          CALL WindowOutStatusBar(8,STATBARSTR(8))
          CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
          IDCurrent_Cursor_mode=ID_Pawley_Refinement_Mode
          CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOn)
          CALL Quick_Pawley()
!.. Now go back to the PeakFit mode
          IF (FromPeakFit) THEN
            STATBARSTR(8)='Peak fitting mode'
            CALL WindowOutStatusBar(8,STATBARSTR(8))
            CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
            IDCurrent_Cursor_mode = ID_Peak_Fitting_Mode
            CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOn)
            CALL PeakFit(EventInfo%VALUE1)
            GOTO 10
          END IF
        CASE (ID_get_crystal_symmetry)
          CALL PushActiveWindowID
          CALL WDialogSelect(IDD_Structural_Information)
          CALL WDialogShow(-1,-1,0,Modeless)
          CALL WDialogSetTab(IDF_Structural_Information_tab,IDD_Crystal_Symmetry)
          CALL PopActiveWindowID
        CASE (ID_get_data_properties)
          CALL PushActiveWindowID
          CALL WDialogSelect(IDD_Structural_Information)
          CALL WDialogShow(-1,-1,0,Modeless)
          CALL WDialogSetTab(IDF_Structural_Information_tab,IDD_Data_Properties)
          CALL PopActiveWindowID
        CASE (ID_get_peak_positions)
          CALL PushActiveWindowID
          CALL WDialogSelect(IDD_Structural_Information)
          CALL WDialogShow(-1,-1,0,Modeless)
          CALL WDialogSetTab(IDF_Structural_Information_tab,IDD_Peak_Positions)
          CALL PopActiveWindowID
        CASE (ID_get_peak_widths)
          CALL PushActiveWindowID
          CALL WDialogSelect(IDD_Structural_Information)
          CALL WDialogShow(-1,-1,0,Modeless)
          CALL WDialogSetTab(IDF_Structural_Information_tab,IDD_Peak_Widths)
          CALL PopActiveWindowID
!        CASE (ID_get_peak_intensities)
        CASE (ID_PolyFitter_Help)
          CALL LaunchHelp()
        CASE (ID_Tutorial_1, ID_Tutorial_2, ID_Tutorial_3, ID_Tutorial_4, ID_Tutorial_5)
          CALL LaunchTutorial(EventInfo%VALUE1)
        CASE (ID_help_about_Polyfitter)
          CALL About()
        CASE(ID_Start_Wizard)
          NoData = Run_Wizard()
          IF (.NOT. NoData) CALL Generate_TicMarks()
      END SELECT

      RETURN
      END SUBROUTINE ProcessMenu
!
!*****************************************************************************
!
! This doesnt work for chm files, only hlp files - I think we have to upgrade the compiler for it to work with chm files
!     SUBROUTINE LaunchWinHelp()
!       USE VARIABLES
!       USE dfwin
!     CHARACTER*100 lpszHelpFileName, lpszContents
!       integer hWnd
!       logical ret

!       lpszHelpFileName ="G:\\ConQuest.chm"C
!      lpszContents = "CONTENTS"C

!      ret = WinHelp (hWnd, lpszHelpFileName, HELP_KEY, LOC(lpszContents) )
!       END SUBROUTINE LaunchWinHelp
!
!*****************************************************************************
!
      SUBROUTINE LaunchHelp()

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      CALL WHelpFile(INSTDIR(1:LEN_TRIM(INSTDIR))//DIRSPACER//'Documentation'//DIRSPACER//'manual'//DIRSPACER//'dash.html')

      END SUBROUTINE LaunchHelp
!
!*****************************************************************************
!
!      SUBROUTINE TIC_file_Open()
!
!   This subroutine processes Open TIC file selection
!
!      USE WINTERACTER
!      USE VARIABLES
!
!      IMPLICIT NONE
!
!      CHARACTER(LEN=256) :: FILTER
!      INTEGER            :: IFLAGS
!      CHARACTER(LEN=80) STATBARSTR
!      COMMON /STATBAR/ STATBARSTR(10)
!C>> JCC Declaration for trapping tic file return
!      INTEGER TicRead
!      INTEGER Load_TIC_File
!      INTEGER IPTYPE
!      COMMON /PLTYPE/ IPTYPE

!   Check if file needs saving
!      IF (SAVEF) THEN
!        CALL WMessageBox(YesNo,QuestionIcon,CommonOK,'Program contains an'//&
!        ' unsaved project.'//CHAR(13)//'Do you wish to '// &
!        'continue?','Open Project')
!   If answer 'No', return
!        IF (WInfoDialog(4) .EQ. 2) RETURN
!      END IF
!   If answer 'Yes'
!      SAVEF = .FALSE.
!      CALL FieldUpdate()
!      IFLAGS = LoadDialog + DirChange + PromptOn
!      FILTER = 'Peak position files (*.tic)|*.tic|'
!      FNAME=' '
!      CALL WSelectFile(FILTER,IFLAGS,FNAME,'Open peak position file')
!   Place your file load code here
!C>> JCC Trap return, and if successful replot the data
!      TicRead = Load_TIC_File(LEN_TRIM(FNAME),FNAME)
!      IF (TicRead .EQ. 1) CALL Profile_Plot(iptype)
!C>> Was
!     CALL Load_TIC_File(LEN_TRIM(FNAME),FNAME)
!      STATBARSTR(1)=FNAME
!      CALL WindowOutStatusBar(1,STATBARSTR(1))
!      RETURN
!
!      END SUBROUTINE TIC_file_Open
!
!*****************************************************************************
!
!U      SUBROUTINE CCL_file_Open()
!U
!U!   This subroutine processes Open CCL file selection
!U
!U      USE WINTERACTER
!U      USE VARIABLES
!U
!U      IMPLICIT NONE
!U
!U      CHARACTER(LEN=256) :: FILTER
!U      INTEGER            :: IFLAGS
!U
!U      INCLUDE 'GLBVAR.INC'
!U
!U      LOGICAL Confirm ! Function
!U
!U!   Check if file needs saving
!U      IF (SAVEF) THEN
!U        IF (.NOT. Confirm('Program contains an unsaved project.'//CHAR(13)//'Do you wish to continue?')) RETURN
!U      END IF
!U      SAVEF  = .FALSE.
!U      CALL FieldUpdate()
!U      IFLAGS = LoadDialog + DirChange + PromptOn
!U      FILTER = 'CCL crystal data files (*.ccl)|*.ccl|'
!U      FNAME=' '
!U      CALL WSelectFile(FILTER,IFLAGS,FNAME,'Open CCL file')
!U!   Place your file load code here
!U      CALL Load_CCL_File(LEN_TRIM(FNAME),FNAME)
!U      STATBARSTR(1)=FNAME
!U      CALL WindowOutStatusBar(1,STATBARSTR(1))
!U      RETURN
!U
!U      END SUBROUTINE CCL_file_Open
!
!*****************************************************************************
!
!      SUBROUTINE Save(IDENT)
!
!   This subroutine processes Save/As selection
!
!      USE WINTERACTER
!      USE VARIABLES
!
!      IMPLICIT NONE
!
!      INTEGER, INTENT (IN) :: IDENT
!
!      CHARACTER(LEN=256)   :: FILTER
!      INTEGER              :: IFLAGS
!
!      IF ((FNAME .EQ. ' ') .OR. (IDENT .EQ. 1)) THEN
!        IFLAGS = SaveDialog + DirChange + PromptOn
!        FILTER = 'All files (*.*)|*.*|'
!        FNAME=' '
!        CALL WSelectFile(FILTER,IFLAGS,FNAME,'Save File')
!      END IF
!      IF (WInfoDialog(4) .EQ. 1) THEN
!   Place save code here
!        SAVEF = .FALSE.
!        CALL FieldUpdate()
!      END IF
!      RETURN

!      END SUBROUTINE Save
!
!*****************************************************************************
!
!      SUBROUTINE Edit()
!
!   This subroutine processes Edit selection
!
!      USE WINTERACTER
!      USE VARIABLES
!      USE druid_header
!
!      IMPLICIT NONE
!
!   Load and show edit dialog
!
!!!      CALL WDialogLoad(IDD_EDITDIAG)
!!!      CALL WDialogShow(-1,-1,0,Modal)
!
!      IF (WInfoDialog(1) .EQ. 1) THEN
!        SAVEF = .TRUE.
!        CALL FieldUpdate()
!      END IF
!
!      RETURN
!      END SUBROUTINE Edit
!
!*****************************************************************************
!
      SUBROUTINE About()
!
!   This subroutine processes About selection
!
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      CHARACTER(LEN=512)  :: CABOUT
!
!   Set about message
!
      CABOUT = 'DASH: A structure solution package for X-ray powder '//CHAR(13)//&
               'diffraction, developed and distributed in collaboration'//CHAR(13)//&
               'between the ISIS Facility of the Rutherford Appleton'//CHAR(13)//&
               'Laboratory and the Cambridge Crystallographic Data Centre.'//CHAR(13)//&
               'Access to this software product is permitted only under the'//CHAR(13)//&
               'terms and conditions of a valid software licence, obtainable'//CHAR(13)//&
               'from the Cambridge Crystallographic Data Centre.'//CHAR(13)//&
               CHAR(13)//&
               'Copyright February 2001'
      CALL WMessageBox(OkOnly,InformationIcon,CommonOk,CABOUT,'About DASH')

      RETURN
      END SUBROUTINE About
!
!*****************************************************************************
!
      SUBROUTINE Redraw()
!
!   This subroutine redraws the window
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'

      INTEGER ICurPlotMode
!
!   Update window
!
      ICurPlotMode = InfoGrScreen(PlotModeReq)
      CALL IGrPlotMode('N')
      IF (PLOTT) CALL Profile_Plot(IPTYPE)
      IF (DoSaRedraw) CALL sa_output_gr()

!       SELECT CASE (ICurPlotMode)
!           CASE (PlotNormal)
!                 CALL IGrPlotMode('N')
!           CASE (PlotOr)
!                 CALL IGrPlotMode('O')
!           CASE (PlotAnd)
!                 CALL IGrPlotMode('A')
!           CASE (PlotEor)
!                 CALL IGrPlotMode('E')
!       END SELECT                        
!           
      RETURN

      END SUBROUTINE Redraw
!
!*****************************************************************************
!
      SUBROUTINE WExit
!
!   This subroutine processes the close requests
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      LOGICAL Confirm ! Function

      IF (Confirm('Do you want to exit DASH?')) CALL DoExit()
      RETURN

      END SUBROUTINE WExit
!
!*****************************************************************************
!
      SUBROUTINE DoExit()

      USE WINTERACTER
      USE VARIABLES
      INTEGER ISTAT

      CLOSE(UNIT=12,STATUS='DELETE',IOSTAT=ISTAT)
      CLOSE(UNIT=6,STATUS='DELETE',IOSTAT=ISTAT)
      CALL DeleteTempFiles
      CALL WindowClose()
      STOP

      END SUBROUTINE DoExit
!
!*****************************************************************************
!
      LOGICAL FUNCTION Run_Wizard

      LOGICAL NoData

      CALL SetWizardState(-1)
      CALL PolyFitter_Wizard(NoData)
      IF (.NOT. NODATA) CALL SetModeMenuState(1,0,0)
      IF (.NOT. NODATA) CALL Upload_Wizard_Information()
      Run_Wizard = NoData
      CALL SetWizardState(1)

      END FUNCTION Run_Wizard
!
!*****************************************************************************
!
      SUBROUTINE ToggleMenus(OnOff)
! JvdS This routine emulates 'modal' by greying out all menus, buttons etc.
! It is necessary when a window pops up that should still be able to communicate
! to the main window.
! JvdS I checked all references to 'ToggleMenus', the argument OnOff is always
! JvdS either 0 or 1
! JvdS It's better to use some sort of a stack mechanism, popping and pushing
! the state of all windows.
! As it is, the routine is not a real 'toggle': a toggle doesn't need an argument
! because it simply flips between two states. This introduces bugs: the series
!
! CALL ToggleMenus(Off)
! CALL ToggleMenus(Off)
! CALL ToggleMenus(On)
!
! does not restore the original state of the program.

      USE WINTERACTER
      USE VARIABLES
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER OnOff
      INTEGER  PeakOn, PawleyOn, SolutionOn, WizardOn
      SAVE PeakOn, PawleyOn, SolutionOn, WizardOn
      DATA PeakOn / 1 /
      DATA PawleyOn / 0 /
      DATA SolutionOn / 0 /
      DATA WizardOn / 1 /
      INTEGER OnOrOff

! WintOn and WintOff are Winteracter constants
      IF (OnOff .EQ. 1) THEN
        OnOrOff = WintOn
      ELSE
        OnOrOff = WintOff
      ENDIF
      CALL WMenuSetState(ID_Import_dpj_file,ItemEnabled,OnOrOff)
      CALL WMenuSetState(ID_Import_data,ItemEnabled,OnOrOff)
!      CALL WMenuSetState(ID_get_data_properties,ItemEnabled,OnOrOff)
!      CALL WMenuSetState(ID_get_peak_positions,ItemEnabled,OnOrOff)
!      CALL WMenuSetState(ID_get_crystal_symmetry,ItemEnabled,OnOrOff)
!      CALL WMenuSetState(ID_get_peak_widths,ItemEnabled,OnOrOff)
!      CALL WMenuSetState(ID_plot_options,ItemEnabled,OnOrOff)
      CALL WMenuSetState(ID_PolyFitter_Help,ItemEnabled,OnOrOff)
      CALL WMenuSetState(ID_Polyfitter_Tips,ItemEnabled,OnOrOff)
      CALL WMenuSetState(ID_help_about_Polyfitter,ItemEnabled,OnOrOff)
      CALL WMenuSetState(ID_import_xye_file,ItemEnabled,OnOrOff)
!      CALL WMenuSetState(ID_import_pro_file,ItemEnabled,OnOrOff)
      IF (OnOff .EQ. 1) THEN
        CALL SetModeMenuState(PeakOn,PawleyOn,SolutionOn)
      ELSE
        PeakOn     = WMenuGetState(ID_Peak_Fitting_Mode,ItemEnabled)
        PawleyOn   = WMenuGetState(ID_Pawley_Refinement_Mode,ItemEnabled)
        SolutionOn = WMenuGetState(ID_Structure_Solution_Mode,ItemEnabled)
        WizardOn   = WMenuGetState(ID_Start_Wizard,ItemEnabled)
        CALL SetModeMenuState(-1,-1,-1)
      ENDIF
      CALL SetWizardState(WizardOn)

      END SUBROUTINE ToggleMenus
!
!*****************************************************************************
!
      SUBROUTINE DeleteTempFiles
      USE WINTERACTER
      USE DRUID_HEADER

! Remove redundant files 
      CALL IOsDeleteFile('polyf.tic')
      CALL IOsDeleteFile('polyf.ccl')
      CALL IOsDeleteFile('polyf.lis')
      CALL IOsDeleteFile('polyf.hkl')
      CALL IOsDeleteFile('polyp.tic')
      CALL IOsDeleteFile('polyp.hkl')
      CALL IOsDeleteFile('polyp.ccl')
      CALL IOsDeleteFile('polyp.ccn')
      CALL IOsDeleteFile('polyp.pik')
      CALL IOsDeleteFile('polyp.hcv')
      CALL IOsDeleteFile('polyp.dat')
      CALL IOsDeleteFile('polys.ccl')
      CALL IOsDeleteFile('polys.lis')

      END SUBROUTINE DeleteTempFiles
