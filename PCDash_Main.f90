!
!*****************************************************************************
!
      PROGRAM PCDash_Main

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      TYPE(WIN_STYLE)   :: MAIN_WINDOW

      INTEGER :: IWIDTHS(10)
      INTEGER :: IWID

!   Initialise Winteracter
      CALL WInitialise(' ')
! Try to redirect stdout - change working directory if unsuccessful
      CALL Init_StdOut()
!   Set up root window options
!
!     - System menu
!     - Minimise button
!     - Maximise button
!     - Status bar
!
      MAIN_WINDOW%FLAGS  = SysMenuOn + MinButton + MaxButton + StatusBar
! Place window on the screen with a fractional size determined from the screen size
      MAIN_WINDOW%WIDTH  = 0.8   * WInfoScreen(1)
      MAIN_WINDOW%HEIGHT = 0.375 * WInfoScreen(2)
      MAIN_WINDOW%X      = 0.1   * WInfoScreen(1)
      MAIN_WINDOW%Y      = 0.01  * FLOAT(WInfoScreen(2)) + 365.0
! Set druid_header menu id and window title
      MAIN_WINDOW%MENUID = IDR_MENU1
      MAIN_WINDOW%TITLE  = "DASH"
! Open root window
      CALL WindowOpen(MAIN_WINDOW,128)
! Load and display the toolbar
      CALL WMenuToolbar(ID_TOOLBAR1)
      CALL WCursorShape(CurCrossHair)
! Disable the menu buttons
      CALL SetModeMenuState(1,-1)
! Setup array of widths for status bar
      IWIDTHS(1) = 3800
      DO IWID = 2, 7
        IWIDTHS(IWID) = 800
      END DO
      IWIDTHS(8)= 1500 
! Split status bar into more than one part
      CALL WindowStatusBarParts(8,IWIDTHS)
    !  CALL IDebugLevel(DbgMsgBox)
      CALL WMessageEnable(PushButton, Enabled)
! Load all Winteracter dialogues into memory
      CALL PolyFitter_UploadDialogues()
! Initialise space group information
      CALL PolyFitterInitialise()
      CALL InitialiseVariables
      CALL Check_License()
      CALL WMessageEnable(FieldChanged, Enabled)
      CALL WMessageEnable(TabChanged, Enabled)
! Main message loop
! Go through the PolyFitter wizard
! Comment this next line out to remove the wizard
      CALL StartWizard()
      DO WHILE (.TRUE.)
        CALL GetEvent
      ENDDO

      END PROGRAM PCDash_Main
!
!*****************************************************************************
!
      SUBROUTINE ProcessMenu
!
! This subroutine processes the menu selections
! This includes the toolbar
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX

      INTEGER          IPMIN, IPMAX
      COMMON /PROFIPM/ IPMIN, IPMAX

! JCC data to indicate whether we are coming out of peak-fitting mode
      LOGICAL Confirm ! Function
      REAL xpgdif, ypgdif
      INTEGER ISTAT, IBpass
      INTEGER DiffractionFileBrowse ! Function

!   Branch depending on chosen menu item

      STATBARSTR(8)=' '
      CALL WindowOutStatusBar(8,STATBARSTR(8))
      SELECT CASE (EventInfo%VALUE1)
        CASE (ID_import_dpj_file)
          CALL SDIFileBrowse
        CASE (ID_import_xye_file)
          ISTAT = DiffractionFileBrowse()
        CASE (ID_Remove_Background)
          CALL PushActiveWindowID
          CALL WDialogSelect(IDD_Background_Fit)
! Initialise the background
          CALL WDialogGetInteger(IDF_Background_Pass,IBpass)
          CALL CalculateBackground(IBpass,20,.TRUE.)
          CALL Profile_Plot
          CALL WDialogShow(-1,-1,0,SemiModeless)
          CALL PopActiveWindowID
        CASE (ID_FILE_PRINT)
          IPTYPE = -IPTYPE
          CALL Profile_Plot
          IPTYPE = -IPTYPE
        CASE (ID_FILE_EXIT)
          CALL WExit
        CASE (ID_Plot_Options)
          CALL PushActiveWindowID
          CALL WDialogSelect(IDD_Plot_Option_Dialog)
          CALL WDialogShow(-1,-1,0,Modeless)
          CALL PopActiveWindowID
        CASE (ID_Configuration)
          CALL PushActiveWindowID
          CALL WDialogSelect(IDD_Configuration)
          CALL WDialogShow(-1,-1,0,Modeless)
          CALL PopActiveWindowID
        CASE (ID_Peak_Fitting_Mode)
          CALL SelectMode(ID_Peak_Fitting_Mode)
        CASE (ID_Pawley_Refinement_Mode)
          CALL ShowPawleyFitWindow
        CASE (ID_Structure_Solution_Mode)
          CALL ShowWizardWindowZmatrices
        CASE (ID_ClearPeakFitRanges)
          IF (Confirm('Do you wish to delete all peak fit ranges?')) CALL Clear_PeakFitRanges
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
        CASE (ID_Left)
! We're going to move the graph to the left if we can
          xpgdif=xpgmax-xpgmin
          xpgmin=MAX(xpmin,xpgmin-0.25*xpgdif)
          xpgmax=xpgmin+xpgdif
          CALL Get_IPMaxMin() 
          CALL Profile_Plot
        CASE (ID_Right)
! We're going to move the graph to the right if we can
          xpgdif=xpgmax-xpgmin
          xpgmax=MIN(xpmax,xpgmax+0.25*xpgdif)
          xpgmin=xpgmax-xpgdif
          CALL Get_IPMaxMin() 
          CALL Profile_Plot
        CASE (ID_Down)
! We're going to move the graph down if we can
          ypgdif=ypgmax-ypgmin
          ypgmin=MAX(ypmin,ypgmin-0.25*ypgdif)
          ypgmax=ypgmin+ypgdif
          CALL Get_IPMaxMin() 
          CALL Profile_Plot
        CASE (ID_Up)
! We're going to move the graph up if we can
          ypgdif=ypgmax-ypgmin
          ypgmax=MIN(ypmax,ypgmax+0.25*ypgdif)
          ypgmin=ypgmax-ypgdif
          CALL Get_IPMaxMin() 
          CALL Profile_Plot
        CASE (ID_Home)
! Back to full profile range
          xpgmin=xpmin
          xpgmax=xpmax
          ypgmin=ypmin
          ypgmax=ypmax
          CALL Get_IPMaxMin() 
          CALL Profile_Plot 
        CASE (ID_PolyFitter_Help)
          CALL LaunchHelp()
        CASE (ID_Tutorial_1, ID_Tutorial_2, ID_Tutorial_3, ID_Tutorial_4, ID_Tutorial_5)
          CALL LaunchTutorial(EventInfo%VALUE1)
        CASE (ID_help_about_Polyfitter)
          CALL About()
        CASE(ID_Start_Wizard)
          CALL StartWizard()
      END SELECT

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
               'Version 2.0'//CHAR(13)//&
               CHAR(13)//&
               'Copyright February 2002'
      CALL WMessageBox(OkOnly,InformationIcon,CommonOk,CABOUT,'About DASH')

      END SUBROUTINE About
!
!*****************************************************************************
!
      SUBROUTINE Redraw()
!
!   This subroutine redraws the window
!
      USE WINTERACTER

      IMPLICIT NONE

!   Update window
      CALL IGrPlotMode('N')
      CALL Profile_Plot

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

      LOGICAL, EXTERNAL :: Confirm

      IF (Confirm('Do you want to exit DASH?')) CALL DoExit

      END SUBROUTINE WExit
!
!*****************************************************************************
!
      SUBROUTINE DoExit

      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER ISTAT

      CALL WriteConfigurationFile
      CLOSE(UNIT=12,STATUS='DELETE',IOSTAT=ISTAT)
      CLOSE(UNIT=6,STATUS='DELETE',IOSTAT=ISTAT)
      CALL DeleteTempFiles
      CALL WindowClose
      STOP

      END SUBROUTINE DoExit
!
!*****************************************************************************
!
      SUBROUTINE DeleteTempFiles

      USE WINTERACTER
      USE DRUID_HEADER

! Remove redundant files 
      CALL IDebugLevel(DbgSilent)
      CALL IOsDeleteFile('polyf.tic')
      CALL IOsDeleteFile('polyf.ccl')
      CALL IOsDeleteFile('polyf.lis')
      CALL IOsDeleteFile('polyf.hkl')
      CALL IOsDeleteFile('polyp.tic')
      CALL IOsDeleteFile('polyp.hkl')
      CALL IOsDeleteFile('polyp.ccl')
      CALL IOsDeleteFile('polyp.ccn')
      CALL IOsDeleteFile('polyp.niw')
      CALL IOsDeleteFile('polyp.pik')
      CALL IOsDeleteFile('polyp.hcv')
      CALL IOsDeleteFile('polyp.dat')
      CALL IOsDeleteFile('polyp.lis')
      CALL IOsDeleteFile('polys.ccl')
      CALL IOsDeleteFile('polys.lis')
      CALL IOsDeleteFile('polyp.pbk')
      CALL IOsDeleteFile('polyp.tbk')
      CALL IOsDeleteFile('polyp.hbk')
      CALL IOsDeleteFile('polyp.hbl')
      CALL IOsDeleteFile('SA_best.pdb')
      CALL IOsDeleteFile('DICVOL.OUT')
      CALL IOSDeleteFile('MakeZmatrix.log')
      CALL IOSDeleteFile('SA_PARAMS.TXT')
      CALL IOSDeleteFile('Overlap_Temp.pdb')
      CALL IDebugLevel(DbgMsgBox)

      END SUBROUTINE DeleteTempFiles
!
!*****************************************************************************
!
