!
!*****************************************************************************
!
! JvdS The following global variables seem to be needed:
!
! - start of profile (to indicate range)
! - end  of profile
! - filename (this is now done partially by 'FNAME', which is also used as a dummy)

! NoData is regularly set to .FALSE. but is it ever set to .TRUE.?

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
      CHARACTER(LEN = 80) :: DashRawFile
      CHARACTER(LEN = 80) :: DashHcvFile
      CHARACTER(LEN = 80) :: DashPikFile
      CHARACTER*80 :: DashTicFile
      LOGICAL RawExists
      LOGICAL HcvExists
      LOGICAL PikExists
      LOGICAL TicExists

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

      LOGICAL NoData
! .TRUE. when a powder diffraction pattern has been read in

      INTEGER OriginalNOBS ! Original number of data points read in for the raw powder pattern

!U      INTEGER FirstDataPointUsed
!U      INTEGER LastDataPointUsed
!U! FirstDataPointUsed & LastDataPointUsed : due to the binning mechanism, it is
!U! possible to rebin the original pattern using only a part of it. These two variables are pointers
!U! to the first and the last data point in the original powder pattern that are actually binned and
!U! therefore actually used by DASH.

      END MODULE VARIABLES
!
!*****************************************************************************
!
      PROGRAM PCDruid_Main

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      TYPE(WIN_STYLE)   :: MAIN_WINDOW

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'lattice.inc'
      INCLUDE 'DialogPosCmn.inc'
      INCLUDE 'STATLOG.INC'

      INTEGER           :: IWIDTHS(10)
      INTEGER           :: IWID

      CALL WInitialise(' ')
      CALL Init_StdOut()
!   Initialise Winteracter
      XBSWidth  = WInfoScreen(1)
      XBSHeight = WInfoScreen(2)
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
      CALL SetModeMenuState(1,-1,-1)
!   Setup array of widths for status bar
      IWIDTHS(1) = 3800
      DO IWID = 2, 7
        IWIDTHS(IWID) = 800
      END DO
      IWIDTHS(8)= 1500 
!      OPEN (UNIT=76,FILE='D:\cvsDASH\dash\Debug\output.log')
!   Split status bar into more than one part
      CALL WindowStatusBarParts(8,IWIDTHS)
      CALL WMessageEnable(PushButton, Enabled)
! Load all Winteracter dialogues into memory
      CALL PolyFitter_UploadDialogues()
! Initialise space group information
      CALL PolyFitterInitialise()
      CALL InitialiseVariables
      CALL Check_License()
      CALL WMessageEnable(FieldChanged, Enabled)
      CALL WMessageEnable(TabChanged, Enabled)
!   Main message loop
!   Go through the PolyFitter wizard
!c>> Comment this next line out to remove the wizard
      CALL StartWizard()
      DO WHILE (.TRUE.)
        CALL GetEvent
      END DO

      END PROGRAM PCDruid_Main
!
!*****************************************************************************
!
      SUBROUTINE SelectMode(TheMode)
!
! This subroutine selects "peak fitting" / "Pawley refinement" / "structure solution" mode,
! ensuring that exactly one mode is active at all times and
! updating the current mode in the menu and in the toolbar.
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheMode

      INCLUDE 'GLBVAR.INC'

! Update the status bar
      SELECT CASE (TheMode)
        CASE (ID_Peak_Fitting_Mode)
          STATBARSTR(8)='Peak fitting mode'
        CASE (ID_Pawley_Refinement_Mode)
          STATBARSTR(8)='Pawley refinement mode'
        CASE (ID_Structure_Solution_Mode)
          STATBARSTR(8)='Structure solution mode'
      END SELECT
      CALL WindowOutStatusBar(8,STATBARSTR(8))
! Update the menu
      CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
      IDCurrent_Cursor_mode = TheMode
      IF (IDCurrent_Cursor_mode .EQ. ID_Default_Mode) IDCurrent_Cursor_mode = ID_Peak_Fitting_Mode
      CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOn)
! Update the toolbar
      
      END SUBROUTINE SelectMode
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

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'

!C>> JCC data to indicate whether we are coming out of peak-fitting mode
      LOGICAL FromPeakFit
      LOGICAL Confirm ! Function

      FromPeakFit = .FALSE.
!
!   Branch depending on chosen menu item
!
 10   CALL WCursorShape(CurArrow)
      STATBARSTR(8)=' '
      CALL WindowOutStatusBar(8,STATBARSTR(8))
      SELECT CASE (EventInfo%VALUE1)
        CASE (ID_import_dpj_file)
          CALL SDI_file_Browse
        CASE (ID_import_xye_file)
          CALL Diffraction_File_Browse
        CASE (ID_Remove_Background)
          CALL Background_Fit
        CASE (ID_FILE_PRINT)
          CALL Profile_Plot(-IPTYPE)
        CASE (ID_FILE_EXIT)
          CALL WExit
        CASE (ID_Plot_Options)
          CALL PushActiveWindowID
          CALL WDialogSelect(IDD_Plot_Option_Dialog)
          CALL WDialogShow(-1,-1,0,Modeless)
          CALL PopActiveWindowID
!U        CASE (ID_Default_Mode)
!U          STATBARSTR(8)='Default visualisation mode'
!U          CALL WindowOutStatusBar(8,STATBARSTR(8))
!U          CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
!U          IDCurrent_Cursor_mode = ID_Default_Mode
!U          CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOn)
        CASE (ID_Peak_Fitting_Mode)
          STATBARSTR(8)='Peak fitting mode'
          CALL WindowOutStatusBar(8,STATBARSTR(8))
          CALL SelectMode(ID_Peak_Fitting_Mode)
          FromPeakFit = .TRUE.
          CALL PeakFit(EventInfo%VALUE1)
          GOTO 10
        CASE (ID_Pawley_Refinement_Mode)
          IF (NumPawleyRef .EQ. 0) THEN
            IF (.NOT. Confirm('Lattice constants may not have been refined'//CHAR(13)//&
                              'Do you wish to continue?')) RETURN
          END IF
          STATBARSTR(8)='Pawley refinement mode'
          CALL WindowOutStatusBar(8,STATBARSTR(8))
          CALL SelectMode(ID_Pawley_Refinement_Mode)
          CALL Quick_Pawley()
!.. Now go back to the PeakFit mode
          IF (FromPeakFit) THEN
            STATBARSTR(8)='Peak fitting mode'
            CALL WindowOutStatusBar(8,STATBARSTR(8))
            CALL SelectMode(ID_Peak_Fitting_Mode)
            CALL PeakFit(EventInfo%VALUE1)
            GOTO 10
          END IF
        CASE (ID_Structure_Solution_Mode)
          CALL SA_Main()
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
        CASE (ID_PolyFitter_Help)
          CALL LaunchHelp()
        CASE (ID_Tutorial_1, ID_Tutorial_2, ID_Tutorial_3, ID_Tutorial_4, ID_Tutorial_5)
          CALL LaunchTutorial(EventInfo%VALUE1)
        CASE (ID_help_about_Polyfitter)
          CALL About()
        CASE(ID_Start_Wizard)
          CALL StartWizard()
      END SELECT
      RETURN

      END SUBROUTINE ProcessMenu
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
      INTEGER  PeakOn, PawleyOn, SolutionOn
      SAVE PeakOn, PawleyOn, SolutionOn
      DATA PeakOn / 1 /
      DATA PawleyOn / 0 /
      DATA SolutionOn / 0 /
      INTEGER OnOrOff

! WintOn and WintOff are Winteracter constants

      RETURN

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
!      CALL WMenuSetState(ID_Polyfitter_Tips,ItemEnabled,OnOrOff)
      CALL WMenuSetState(ID_help_about_Polyfitter,ItemEnabled,OnOrOff)
      CALL WMenuSetState(ID_import_xye_file,ItemEnabled,OnOrOff)
!      CALL WMenuSetState(ID_import_pro_file,ItemEnabled,OnOrOff)
      IF (OnOff .EQ. 1) THEN
        CALL SetModeMenuState(PeakOn,PawleyOn,SolutionOn)
      ELSE
        PeakOn     = WMenuGetState(ID_Peak_Fitting_Mode,ItemEnabled)
        PawleyOn   = WMenuGetState(ID_Pawley_Refinement_Mode,ItemEnabled)
        SolutionOn = WMenuGetState(ID_Structure_Solution_Mode,ItemEnabled)
        CALL SetModeMenuState(-1,-1,-1)
      ENDIF

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
!
!*****************************************************************************
!
