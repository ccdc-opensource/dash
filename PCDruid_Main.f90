      MODULE VARIABLES
!
!   Shared variables for any routine with 'USE VARIABLES'
!
          IMPLICIT NONE
!
          LOGICAL                       :: SAVEF = .FALSE. ! File needs saving
!
          LOGICAL                       :: PLOTT = .FALSE. ! Graphic plotted?
          CHARACTER(LEN=256)            :: FNAME = ' '     ! Current filename
!C>> JCC Added these in for portability reasons       
		  CHARACTER(LEN=255)            :: INSTDIR =     'C:\Program Files\DASH' ! Default installation directory
		  CHARACTER(LEN=21)             :: SPACEGROUPS = 'SpaceGroupSymbols.dat' ! Table name
		  CHARACTER                     :: DIRSPACER   = '\' ! Windows spacer
		  CHARACTER(LEN=8)              :: CONFIG      = 'Dash.cfg'
!C>> External binaries
		  CHARACTER(LEN=255)            :: VIEWEXE     = 'C:\Program Files\WLViewerLite35\MSViewer.exe'
		  CHARACTER(LEN=255)            :: CONVEXE     = 'C:\Program Files\DASH\zmconv.exe'
		  CHARACTER(LEN=20)             :: VIEWARG     = ' '
    	  LOGICAL ViewOn, ConvOn
		  COMMON / EXTPRG / ViewOn, ConvOn, ViewExe, ConvExe, ViewArg
		  COMMON / EXTDIR / INSTDIR

!C>> File information; Names of files used by DASH For I/O
		  INTEGER,PARAMETER :: MaxPathLength =  255

		  CHARACTER(LEN = MaxPathLength) :: DashRawFile
		  CHARACTER(LEN = MaxPathLength) :: DashHcvFile
		  CHARACTER(LEN = MaxPathLength) :: DashPikFile
		  CHARACTER(LEN = MaxPathLength) :: DashTicFile
		  LOGICAL RawExists
		  LOGICAL HcvExists
		  LOGICAL PikExists
		  LOGICAL TicExists
		  INTEGER DataReadMode

!C>> File names of data files
		  COMMON / FLINF1 /   DashRawFile, DashHcvFile, DashPikFile, DashTicFile
		  COMMON / FLINF2 /   RawExists,   HcvExists,   PikExists,   TicExists 
		  COMMON / FLINF3 /   DataReadMode
	      
!C>> License information
		  INTEGER :: EXPIRY_DAY   =   31
		  INTEGER :: EXPIRY_MONTH =   12
		  INTEGER :: EXPIRY_YEAR  = 2000
		  CHARACTER(LEN=18) :: EXPIRY_STRING = '31st December 2000'

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
		  parameter (NodeKey = 1)
		  parameter (DemoKey = 2)
		  parameter (SiteKey = 3)



      END MODULE VARIABLES
!
!*****************************************************************************
!
      PROGRAM PCDruid_Main
!
!
      USE WINTERACTER
      USE druid_header
!
      IMPLICIT NONE
!
!   Type declarations
!
      TYPE(WIN_STYLE)   :: MAIN_WINDOW
      TYPE(WIN_MESSAGE) :: MESSAGE
!
!   Variable declarations
!
      LOGICAL           :: QUIT    = .FALSE.
      LOGICAL           :: NoData  = .TRUE.
      INTEGER           :: IWIDTHS(10)
      INTEGER           :: ITYPE,IWID
	  INTEGER           :: L
      CHARACTER(LEN=80) STATBARSTR
      COMMON /STATBAR/ STATBARSTR(10)
      INCLUDE 'STATLOG.INC'
      LOGICAL Run_Wizard
	  LOGICAL process_mainwindow_message


      CALL WInitialise(' ')
	  CALL Init_StdOut()

!   Initialise Winteracter
!
      XBSWidth   = WInfoScreen(1)
      XBSHeight  = WInfoScreen(2)
      XBSColours = WInfoScreen(3)
      CALL PolyFitterInitialise()


!>> JCC
!>> Try to redirect stdout - change working directory if unsuccessful

!

!
!   Set up root window options
!
!     - System menu
!     - Minimise button
!     - Maximise button
!     - Fixed sized window
!     - Status bar
!
      MAIN_WINDOW%FLAGS  = SysMenuOn + MinButton + MaxButton + StatusBar
!
!   Place window on the screen with a fractional size determined from the screen size
!
!      write(56,*) XBSWidth,XBSHeight
      MAIN_WINDOW%WIDTH  = 0.8*XBSWidth
      MAIN_WINDOW%HEIGHT = 0.375*XBSHeight
      MAIN_WINDOW%X      = 0.1*XBSWidth
      MAIN_WINDOW%Y      = 0.06*float(XBSHeight)+262 !0.4*XBSHeight
!	write(56,*) MAIN_WINDOW%X,MAIN_WINDOW%Y
!
!   Set druid_header menu id and window title
!
      MAIN_WINDOW%MENUID = IDR_MENU1
      MAIN_WINDOW%TITLE  = "DASH"

!
!   Open root window
!
      CALL WindowOpen(MAIN_WINDOW)
!
!   Load and display the toolbar
!
      CALL WMenuToolbar(ID_TOOLBAR1)
!
!   Setup array of widths for status bar
!
      IWIDTHS(1) = 3800
      DO IWID=2,7
        IWIDTHS(IWID) = 800
      END DO
      IWIDTHS(8)= 1500 
!
!   Split status bar into more than one part
!
      CALL WindowStatusBarParts(8,IWIDTHS)
!
!   Disable certain menu options
!
      CALL FieldUpdate()
!
      CALL PP_DEFAULTS
!
!   Main message loop
!
!
!   Go through the PolyFitter wizard
      CALL WMessageEnable(FieldChanged, Enabled)
      CALL WMessageEnable(PushButton, Enabled)
!
!      call WPlaySound('h11500so.wav',0)
!

	  CALL PolyFitter_UploadDialogues()
	  CALL PolyFitter_Defaults()
	  call Check_License()


!C>> JCC Disable the menu buttons
	  CALL SetModeMenuState(-1,-1,-1)

!c>> Comment this next line out to remove the wizard
      NoData = Run_Wizard()

      CALL Generate_TicMarks()
!
!

!C>> JCC Do we need this loop at all? It seems to hang the whole system?
!      DO WHILE(NoData)
!          CALL WMessage(ITYPE,MESSAGE)
!	write(76,*) ' Initial message loop',itype,Message%Value1,Message%Value2,NoData
!          SELECT CASE (ITYPE)
!              CASE (MenuSelect)
!                  CALL ProcessDataMenu(MESSAGE%VALUE1,QUIT,NoData)
!              CASE (CloseRequest)
!                  CALL WExit(QUIT)
!          END SELECT
!          If (Quit) goto 100
!
!      END DO

!
!.. We no have data - load up all the dialogs
!C>> JCC      CALL WDialogLoad(IDD_Structural_Information)
!C>> JCC      CALL WDialogLoad(IDD_Plot_Option_Dialog)
!
!
!C JCC Enable tab changing
      CALL WMessageEnable(TabChanged, Enabled)
      DO WHILE(.NOT.QUIT)
          CALL WMessage(ITYPE,MESSAGE)
!	write(76,*) ' Main loop',itype,Message%Value1,Message%Value2
		  QUIT = process_mainwindow_message(ITYPE,MESSAGE)
      END DO
!
!   Close program
!
  100    CALL WindowClose()
!
      STOP
      END PROGRAM PCDruid_Main
!
!*****************************************************************************
!
      SUBROUTINE ProcessMenu(IDENT,QUIT)
!
!   This subroutine processes the menu selections
!
      USE WINTERACTER
      USE druid_header
!
      IMPLICIT NONE
!
      INTEGER, INTENT (IN)     :: IDENT
      LOGICAL, INTENT (IN OUT) :: QUIT
      LOGICAL NoData
      INTEGER IPTYPE,JPTYPE
      COMMON /PLTYPE/ IPTYPE
      INTEGER IDCurrent_Cursor_Mode
      COMMON /CURSOR_MODE/ IDCurrent_Cursor_Mode
!
      CHARACTER(LEN=80) STATBARSTR
      COMMON /STATBAR/ STATBARSTR(10)
      include 'statlog.inc'

!C>> JCC data to indicate wether we are comming out of peak-fitting mode
	  LOGICAL FromPeakFit
	  LOGICAL Run_Wizard ! Function
	  Integer IFlags
	  CHARACTER(LEN=255) FileName

      FromPeakFit = .FALSE.
!
!   Branch depending on chosen menu item
!
 10   CALL WCursorShape(CurArrow)
      STATBARSTR(8)=' '
      CALL WindowOutStatusBar(8,STATBARSTR(8))
      SELECT CASE (IDENT)
          CASE (ID_import_xye_file)
              CALL XYE_file_Open(NoData)
          CASE (ID_import_pro_file)
              CALL PRO_file_Open(NoData)
!>> JCC Implement project reading now that this works via the SDI interface
          CASE (ID_import_dpj_file)
              CALL SDI_file_Open(NoData)
!>> JCC Implement the structure solution button
		  CASE (ID_Structure_Solution_Mode)
			  call SA_Main()
!          CASE (ID_FILE_SAVE)
!              CALL Save(0)
!          CASE (ID_FILE_SAVEAS)
!              CALL Save(1)
          CASE (ID_FILE_PRINT)
              JPTYPE=-IPTYPE
              CALL Profile_Plot(JPTYPE)
          CASE (ID_FILE_EXIT)
              CALL WExit(QUIT)
!          CASE (ID_EDIT_DIAG)
!              CALL Edit()
!          CASE (ID_PLOT)
!              CALL Profile_Plot(IPTYPE)
          CASE (ID_Plot_Options)
              CALL Plot_Options(IPTYPE)
          CASE (ID_Default_Mode)
              STATBARSTR(8)='Default visualisation mode'
              CALL WindowOutStatusBar(8,STATBARSTR(8))
              CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
              IDCurrent_Cursor_mode=ID_Default_Mode
              CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOn)
!          CASE (ID_CrossHair_Cursor_Mode)
!              STATBARSTR(8)='Cursor Mode'
!              CALL WindowOutStatusBar(8,STATBARSTR(8))
!              CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
!              IDCurrent_Cursor_mode=ID_CrossHair_Cursor_Mode
!              CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOn)
!              CALL PeakFind_Manual(IDENT)
!              Goto 10
          CASE (ID_Peak_Fitting_Mode)
              STATBARSTR(8)='Peak fitting mode'
              CALL WindowOutStatusBar(8,STATBARSTR(8))
              CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
              IDCurrent_Cursor_mode=ID_Peak_Fitting_Mode
              CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOn)
			  FromPeakFit = .TRUE.
              CALL PeakFit(IDENT)
              Goto 10
          CASE (ID_Pawley_Refinement_Mode)
             If (NumPawleyRef.eq.0) then
               Call WMessageBox(YesNo,QuestionIcon,CommonOK,&
               'Lattice constants may not have been refined'//CHAR(13)//&
               'Do you wish to '// &
                           'continue?','Pawley Refinement')
!   If answer 'No', return
!
!C>> JCC Seems this line sets the code in an infinite loop.
! Was               IF (WInfoDialog(4).EQ.2) Goto 10
! Now
			     IF (WInfoDialog(4).EQ.2) THEN
! Just go home to mummy ...
					RETURN
				 END IF
              End If
              CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
              IDCurrent_Cursor_mode=ID_Pawley_Refinement_Mode
              call WMenuSetState(ID_Pawley_Refinement_Mode,ItemChecked,WintOn)
              STATBARSTR(8)='Pawley refinement mode'
              CALL WindowOutStatusBar(8,STATBARSTR(8))
              CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
              IDCurrent_Cursor_mode=ID_Pawley_Refinement_Mode
              CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOn)
              call Quick_Pawley()
!.. Now go back to the PeakFit mode
			  IF (FromPeakFit) THEN
				STATBARSTR(8)='Peak fitting mode'
				CALL WindowOutStatusBar(8,STATBARSTR(8))
				CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOff)
				IDCurrent_Cursor_mode=ID_Peak_Fitting_Mode
				CALL WMenuSetState(IDCurrent_Cursor_mode,ItemChecked,WintOn)
				CALL PeakFit(IDENT)
				Goto 10
			  END IF
          CASE (ID_get_crystal_symmetry)
!              STATBARSTR(8)='Lattice constant mode'
!              CALL WindowOutStatusBar(8,STATBARSTR(8))
!$            CALL WDialogLoad(IDD_Structural_Information)
            CALL WDialogSelect(IDD_Structural_Information)
            CALL WDialogShow(-1,-1,0,Modeless)
            CALL WDialogSetTab(IDF_Structural_Information_tab,IDD_Crystal_Symmetry)
            CALL WDialogSelect(IDD_Crystal_Symmetry)
!C>> JCC  Dont do this, otherwise clicking OK makes the cell change its state          CALL Upload_Crystal_Symmetry()
!C>> No longer needed since the dialogues are not destroyed 
          CASE (ID_get_data_properties)
!$            CALL WDialogLoad(IDD_Structural_Information)
            CALL WDialogSelect(IDD_Structural_Information)
            CALL WDialogShow(-1,-1,0,Modeless)
            CALL WDialogSetTab(IDF_Structural_Information_tab,IDD_Data_Properties)
            CALL WDialogSelect(IDD_Data_Properties)
            CALL Upload_Data_Properties()
            CALL Upload_Range()
          CASE (ID_get_peak_positions)
!$            CALL WDialogLoad(IDD_Structural_Information)
            CALL WDialogSelect(IDD_Structural_Information)
            CALL WDialogShow(-1,-1,0,Modeless)
            CALL WDialogSetTab(IDF_Structural_Information_tab,IDD_Peak_Positions)
          CASE (ID_get_peak_widths)
!$            CALL WDialogLoad(IDD_Structural_Information)
            CALL WDialogSelect(IDD_Structural_Information)
            CALL WDialogShow(-1,-1,0,Modeless)
            CALL WDialogSetTab(IDF_Structural_Information_tab,IDD_Peak_Widths)
!          CASE (ID_get_peak_intensities)
          CASE (ID_PolyFitter_Help)
             CALL LaunchHelp(1)
          CASE (ID_PolyFitter_Tips)
             CALL LaunchHelp(2)
          CASE (ID_help_about_Polyfitter)
              CALL About()
		  CASE(ID_Start_Wizard)
		      NoData = Run_Wizard()
		      IF (.NOT.NoData) CALL Generate_TicMarks()
      END SELECT
!
      RETURN
      END SUBROUTINE ProcessMenu


! This doesnt work for chm files, only hlp files - I think we have to upgrade the compiler for it to work with chm files
!     SUBROUTINE LaunchWinHelp()
!	  USE VARIABLES
!	  use dfwin
!     character*100 lpszHelpFileName, lpszContents
!	  integer hWnd
!	  logical ret

!	  lpszHelpFileName ="G:\\ConQuest.chm"C
!      lpszContents = "CONTENTS"C

!      ret = WinHelp (hWnd, lpszHelpFileName, HELP_KEY, LOC(lpszContents) )
!	  END SUBROUTINE LaunchWinHelp

	  SUBROUTINE LaunchHelp(Mode)
	  USE WINTERACTER
      USE druid_header
	  USE VARIABLES

	  IF (Mode .EQ. 1) THEN
	  	  CALL WHelpFile(INSTDIR(1:LEN_TRIM(INSTDIR))//DIRSPACER//'Documentation'//DIRSPACER//'html'//DIRSPACER//'dash.html')
	  ELSE
	  	  CALL WHelpFile(INSTDIR(1:LEN_TRIM(INSTDIR))//DIRSPACER//'Documentation'//DIRSPACER//'html'//DIRSPACER//'dash_tutorials.html')
	  END IF
	  

	  END SUBROUTINE LaunchHelp
!
!*****************************************************************************
!
      SUBROUTINE ProcessDataMenu(IDENT,QUIT,NoData)
!
!   This subroutine processes the menu selections for input data alone.
!
      USE WINTERACTER
      USE druid_header
!
      IMPLICIT NONE
!
      INTEGER, INTENT (IN)     :: IDENT
      LOGICAL, INTENT (IN OUT) :: QUIT
      LOGICAL, INTENT (IN OUT) :: NoData
      INTEGER IPTYPE,JPTYPE
      COMMON /PLTYPE/ IPTYPE
      INTEGER IDCurrent_Cursor_Mode
      COMMON /CURSOR_MODE/ IDCurrent_Cursor_Mode
!
      CHARACTER(LEN=80) STATBARSTR
      COMMON /STATBAR/ STATBARSTR(10)
      include 'statlog.inc'
!
!   Branch depending on chosen menu item
!
 10   CALL WCursorShape(CurArrow)
      STATBARSTR(8)=' '
      CALL WindowOutStatusBar(8,STATBARSTR(8))
      SELECT CASE (IDENT)
          CASE (ID_import_xye_file)
              CALL XYE_file_Open(NoData)
          CASE (ID_import_pro_file)
              CALL PRO_file_Open(NoData)
          CASE (ID_FILE_EXIT)
              CALL WExit(QUIT)
          CASE (ID_PolyFitter_Help)
              CALL WHelpFile('Ppoly.hlp')
          CASE (ID_Help_About)
              CALL About()
      END SELECT
!
      END SUBROUTINE ProcessDataMenu
!
!*****************************************************************************
!
      SUBROUTINE XYEPRO_file_Open(NoData)
!
!   This subroutine processes Open selection
!
      USE WINTERACTER
      USE druid_header
      USE VARIABLES
!
      IMPLICIT NONE
      LOGICAL, INTENT (IN OUT) :: NoData
!
      CHARACTER(LEN=256) :: FILTER
      INTEGER            :: IFLAGS,KLEN
      CHARACTER(LEN=3)  EXT3

!>> JCC - function declarations for file loaders
	  INTEGER Load_PRO_File, Load_Diffraction_File
!>> And then the return stati
	  INTEGER ISTAT

!
!   Check if file needs saving
!
      IF (SAVEF) THEN
          CALL WMessageBox(YesNo,QuestionIcon,CommonOK,'Program contains an'//&
                           ' unsaved project.'//CHAR(13)//'Do you wish to '// &
                           'continue?','Open Project')
!
!   If answer 'No', return
!
          IF (WInfoDialog(4).EQ.2) RETURN
      END IF
!
!   If answer 'Yes'
!
      SAVEF  = .FALSE.
      CALL FieldUpdate()
      IFLAGS = LoadDialog + DirChange + PromptOn
      FILTER = 'Powder diffraction files (*.xye)|*.xye|Powder profile files (*.pro)|*.pro|'
      FNAME=' '
      CALL WSelectFile(FILTER,IFLAGS,FNAME,'Open powder diffraction file')
	  CALL WDialogSelect(IDD_PW_Page3)
      CALL WDialogPutString(IDF_PWa_DataFileName_String,FNAME)
	  CALL WDialogSelect(IDD_PW_Page2)
      CALL WDialogPutString(IDF_PW_DataFileName_String,FNAME)
	  IF (FNAME .EQ. ' ') RETURN


      CALL XYEPRO_file_Read(FNAME,NoData)

      END SUBROUTINE XYEPRO_file_Open

!>> JCC Sub-divide this routine

      SUBROUTINE XYEPRO_file_Read(FILENAME,NoData)

      USE WINTERACTER
      USE druid_header
	  USE VARIABLES
	  CHARACTER*(*) FILENAME
	  INTEGER ISTAT
	  LOGICAL NODATA
      integer klen
	  character*3 ext3
	  logical FExists
      CHARACTER(LEN=80) STATBARSTR
      COMMON /STATBAR/ STATBARSTR(10)

      ISTAT = 0
      klen=LEN_TRIM(FILENAME)
	  IF (klen .GT.0) THEN
			INQUIRE(FILE=FILENAME(1:klen),EXIST=FExists)
			IF (FExists) THEN
!>> JCC - was
!      ext3=fname( klen-2:klen )
!>> But what if the file extension has less than 3 chars?
!>> So I made it this ... 
				ext3=filename( max(klen-2,1):max(klen,1) )
				CALL ILowerCase(ext3)
				if (ext3.eq.'pro') then
!>> JCC Trap the return status from this
				  ISTAT = Load_PRO_File(klen,FILENAME,NoData)
				else
!>> JCC Trap the return status from this
				  ISTAT = Load_Diffraction_File(klen,FILENAME,NoData)
				end if
			 END IF
      END IF
!>> Check the status
	  IF (ISTAT .EQ. 0) THEN
!>> JCC The file could not be opened - better tell the user
		CALL WMessageBox(OkOnly,ExclamationIcon,CommonOK,'Could not find the file named'//&
                           CHAR(13)//FILENAME(1:klen)//CHAR(13),'No Such File')
!
		RETURN
	  ENDIF
      STATBARSTR(1)=FILENAME
      CALL WindowOutStatusBar(1,STATBARSTR(1))
	  DashRawFile = FILENAME
!
      RETURN
	  END SUBROUTINE XYEPRO_file_Read
!
!*****************************************************************************
!
      SUBROUTINE XYE_file_Open(NoData)
!
!   This subroutine processes Open selection
!
      USE WINTERACTER
      USE VARIABLES
	  USE DRUID_HEADER
!
      IMPLICIT NONE
      LOGICAL, INTENT (IN OUT) :: NoData
!
      CHARACTER(LEN=256) :: FILTER
      INTEGER            :: IFLAGS
      CHARACTER(LEN=80) STATBARSTR
      COMMON /STATBAR/ STATBARSTR(10)

	  LOGICAL FExists
	  INTEGER IFlen
!
!   Check if file needs saving
!
      IF (SAVEF) THEN
          CALL WMessageBox(YesNo,QuestionIcon,CommonOK,'Program contains an'//&
                           ' unsaved project.'//CHAR(13)//'Do you wish to '// &
                           'continue?','Open Project')
!
!   If answer 'No', return
!
          IF (WInfoDialog(4).EQ.2) RETURN
      END IF
!
!   If answer 'Yes'
!
      SAVEF  = .FALSE.
      CALL FieldUpdate()
      IFLAGS = LoadDialog + DirChange + PromptOn
      FILTER = 'Powder diffraction files|*.xye|'
      FNAME=' '
      CALL WSelectFile(FILTER,IFLAGS,FNAME,'Open powder diffraction file')
!
	  IFlen =  LEN_TRIM(FNAME)
	  IF (IFlen .EQ. 0) RETURN
	  INQUIRE(FILE=FNAME(1:IFlen),EXIST=FExists)
	  IF (.NOT.FExists) THEN
	    Call WMessageBox(OkOnly,ExclamationIcon,CommonOk,&
	    "The file "//FNAME(1:IFlen)//" does not exist!",&
	    "No such file")
		RETURN
	  ENDIF

      CALL Load_Diffraction_File(IFlen,FNAME,NoData)
      STATBARSTR(1)=FNAME
      CALL WindowOutStatusBar(1,STATBARSTR(1))
!
! Enable the appropriate menus:

	  CALL SetModeMenuState(1,0,0)
      RETURN
      END SUBROUTINE XYE_file_Open

!>> JCC Added 

    SUBROUTINE SDI_file_Open(NoData)
!
!   This subroutine processes Open selection
!
      USE WINTERACTER
      USE VARIABLES
	  USE DRUID_HEADER
!
      IMPLICIT NONE
      LOGICAL, INTENT (IN OUT) :: NoData
!
      CHARACTER(LEN=256) :: FILTER
      INTEGER            :: IFLAGS
      CHARACTER(LEN=80) STATBARSTR
      COMMON /STATBAR/ STATBARSTR(10)
!
	  LOGICAL FExists
	  INTEGER Iflen
!   Check if file needs saving
!
      IF (SAVEF) THEN
          CALL WMessageBox(YesNo,QuestionIcon,CommonOK,'Program contains an'//&
                           ' unsaved project.'//CHAR(13)//'Do you wish to '// &
                           'continue?','Open Project')
!
!   If answer 'No', return
!
          IF (WInfoDialog(4).EQ.2) RETURN
      END IF
!
!   If answer 'Yes'
!
      SAVEF  = .FALSE.
      CALL FieldUpdate()
      IFLAGS = LoadDialog + DirChange + PromptOn
      FILTER = 'DASH pawley files|*.sdi|'
      FNAME=' '
      CALL WSelectFile(FILTER,IFLAGS,FNAME,'Open DASH pawley file')

	  IFlen =  LEN_TRIM(FNAME)
	  IF (IFlen .EQ. 0) RETURN
	  INQUIRE(FILE=FNAME(1:IFlen),EXIST=FExists)
	  IF (.NOT.FExists) THEN
	    Call WMessageBox(OkOnly,ExclamationIcon,CommonOk,&
	    "The file "//FNAME(1:IFlen)//" does not exist!",&
	    "No such file")
		RETURN
	  ENDIF
      CALL OpenHCVPIKTIC(FNAME(1:IFlen),0,NoData) 
	  IF (NODATA) THEN
			CALL WMessageBox(OKOnly,ExclamationIcon, CommonOk,&
			"Could not read the project file "//FNAME(:IFlen)//&
			CHAR(13)//"successfully","Failed to read project")
			RETURN
	  END IF
      STATBARSTR(1)=FNAME
      CALL WindowOutStatusBar(1,STATBARSTR(1))
!
! Enable all menu functions
	  CALL SetModeMenuState(1,1,1)

      RETURN
      END SUBROUTINE SDI_file_Open
!


!
!*****************************************************************************
!
!
      SUBROUTINE TIC_file_Open()
!
!   This subroutine processes Open TIC file selection
!
      USE WINTERACTER
      USE VARIABLES
!
      IMPLICIT NONE
!
      CHARACTER(LEN=256) :: FILTER
      INTEGER            :: IFLAGS
      CHARACTER(LEN=80) STATBARSTR
      COMMON /STATBAR/ STATBARSTR(10)
!C>> JCC Declaration for trapping tic file return
	  INTEGER TicRead
	  INTEGER Load_TIC_File
	  INTEGER IPTYPE
      COMMON /PLTYPE/ IPTYPE

!
!   Check if file needs saving
!
      IF (SAVEF) THEN
          CALL WMessageBox(YesNo,QuestionIcon,CommonOK,'Program contains an'//&
                           ' unsaved project.'//CHAR(13)//'Do you wish to '// &
                           'continue?','Open Project')
!
!   If answer 'No', return
!
          IF (WInfoDialog(4).EQ.2) RETURN
      END IF
!
!   If answer 'Yes'
!
      SAVEF  = .FALSE.
      CALL FieldUpdate()
      IFLAGS = LoadDialog + DirChange + PromptOn
      FILTER = 'Peak position files|*.tic|'
      FNAME=' '
      CALL WSelectFile(FILTER,IFLAGS,FNAME,'Open peak position file')
!   Place your file load code here
!C>> JCC Trap return, and if successful replot the data
      TicRead =  Load_TIC_File(LEN_TRIM(FNAME),FNAME)
	  IF (TicRead .EQ. 1) CALL Profile_Plot(iptype)
!C>> Was
!     CALL Load_TIC_File(LEN_TRIM(FNAME),FNAME)
      STATBARSTR(1)=FNAME
      CALL WindowOutStatusBar(1,STATBARSTR(1))
!
      RETURN
      END SUBROUTINE TIC_file_Open
!
!*****************************************************************************
!
!
      SUBROUTINE CCL_file_Open()
!
!   This subroutine processes Open CCL file selection
!
      USE WINTERACTER
      USE VARIABLES
!
      IMPLICIT NONE
!
      CHARACTER(LEN=256) :: FILTER
      INTEGER            :: IFLAGS
      CHARACTER(LEN=80) STATBARSTR
      COMMON /STATBAR/ STATBARSTR(10)
!
!   Check if file needs saving
!
      IF (SAVEF) THEN
          CALL WMessageBox(YesNo,QuestionIcon,CommonOK,'Program contains an'//&
                           ' unsaved project.'//CHAR(13)//'Do you wish to '// &
                           'continue?','Open Project')
!
!   If answer 'No', return
!
          IF (WInfoDialog(4).EQ.2) RETURN
      END IF
!
!   If answer 'Yes'
!
      SAVEF  = .FALSE.
      CALL FieldUpdate()
      IFLAGS = LoadDialog + DirChange + PromptOn
      FILTER = 'CCL crystal data files|*.ccl|'
      FNAME=' '
      CALL WSelectFile(FILTER,IFLAGS,FNAME,'Open CCL file')
!   Place your file load code here
      CALL Load_CCL_File(LEN_TRIM(FNAME),FNAME)
      STATBARSTR(1)=FNAME
      CALL WindowOutStatusBar(1,STATBARSTR(1))
!
      RETURN
      END SUBROUTINE CCL_file_Open
!
!*****************************************************************************
!
      SUBROUTINE PRO_file_Open(NoData)
!
!   This subroutine processes Open .PRO selection
!
      USE WINTERACTER
      USE VARIABLES
!
      IMPLICIT NONE
!
      LOGICAL, INTENT (IN OUT) :: NoData
      CHARACTER(LEN=256) :: FILTER
      INTEGER            :: IFLAGS
      CHARACTER(LEN=80) STATBARSTR
      COMMON /STATBAR/ STATBARSTR(10)
!
!   Check if file needs saving
!
      IF (SAVEF) THEN
          CALL WMessageBox(YesNo,QuestionIcon,CommonOK,'Program contains an'//&
                           ' unsaved project.'//CHAR(13)//'Do you wish to '// &
                           'continue?','Open Project')
!
!   If answer 'No', return
!
          IF (WInfoDialog(4).EQ.2) RETURN
      END IF
!
!   If answer 'Yes'
!
      SAVEF  = .FALSE.
      CALL FieldUpdate()
      IFLAGS = LoadDialog + DirChange + PromptOn
      FILTER = 'Refinement profile files|*.pro|'
      FNAME=' '
      CALL WSelectFile(FILTER,IFLAGS,FNAME,'Open refinement profile file')
!   Place your file load code here
      CALL Load_PRO_File(LEN_TRIM(FNAME),FNAME,NoData)
      STATBARSTR(1)=FNAME
      CALL WindowOutStatusBar(1,STATBARSTR(1))
!
      RETURN
      END SUBROUTINE PRO_file_Open
!
!*****************************************************************************
!
      SUBROUTINE Save(IDENT)
!
!   This subroutine processes Save/As selection
!
      USE WINTERACTER
      USE VARIABLES
!
      IMPLICIT NONE
!
      INTEGER, INTENT (IN) :: IDENT
!
      CHARACTER(LEN=256)   :: FILTER
      INTEGER              :: IFLAGS
!
      IF (FNAME.EQ.' '.OR.IDENT.EQ.1) THEN
          IFLAGS = SaveDialog + DirChange + PromptOn
          FILTER = 'All files|*.*|'
          FNAME=' '
          CALL WSelectFile(FILTER,IFLAGS,FNAME,'Save File')
      END IF
!
      IF (WInfoDialog(4).EQ.1) THEN
!   Place save code here
          SAVEF = .FALSE.
          CALL FieldUpdate()
      END IF
!
      RETURN
      END SUBROUTINE Save
!
!*****************************************************************************
!
      SUBROUTINE Edit()
!
!   This subroutine processes Edit selection
!
      USE WINTERACTER
      USE VARIABLES
      USE druid_header
!
      IMPLICIT NONE
!
!   Load and show edit dialog
!
!!!      CALL WDialogLoad(IDD_EDITDIAG)
!!!      CALL WDialogShow(-1,-1,0,Modal)
!
      IF (WInfoDialog(1).EQ.1) THEN
          SAVEF = .TRUE.
          CALL FieldUpdate()
      END IF
!
      RETURN
      END SUBROUTINE Edit


!
!*****************************************************************************
!

!
!*****************************************************************************
!
      SUBROUTINE About()
!
!   This subroutine processes About selection
!
      USE WINTERACTER
      USE druid_header
!
      IMPLICIT NONE
!
      CHARACTER(LEN=512)  :: CABOUT
	  INTEGER ICurSel
!
!   Set about message
!
      CABOUT = 'DASH: A structure solution package for X-ray powder '//CHAR(13)//&
	           'diffraction, developed and distributed in collaboration'//CHAR(13)//&
	           'between the ISIS Facility of the Rutherford Appleton'//CHAR(13)//&
	           'Laboratory and the Cambridge Crystallographic Data'//CHAR(13)//&
			   'Centre'//CHAR(13)//&
			   CHAR(13)//&
			   'Copyright February 2001'

      CALL WMessageBox(OkOnly,InformationIcon,CommonOk,CABOUT,'About DASH')
!
      RETURN
      END SUBROUTINE About
!
!*****************************************************************************
!
      SUBROUTINE FieldUpdate()
!
!   This routine enables/disables save options
!
      USE WINTERACTER
      USE VARIABLES
      USE druid_header
!
      IMPLICIT NONE
!
!   Enable/disable save
!
!      IF (SAVEF) THEN
!          CALL WMenuSetState(ID_FILE_SAVE,1,1)
!      ELSE
!          CALL WMenuSetState(ID_FILE_SAVE,1,0)
!      END IF
!
      RETURN
      END SUBROUTINE FieldUpdate
!
!*****************************************************************************
!
      SUBROUTINE Redraw()
!
!   This subroutine redraws the window
!
      USE WINTERACTER
      USE VARIABLES
!
      IMPLICIT NONE
      INTEGER IPTYPE
      COMMON /PLTYPE/ IPTYPE

	  LOGICAL DoSaRedraw
	  COMMON /SARDRW/ DoSaRedraw

	  INTEGER ICurPlotMode
!
!   Update window
!
	  ICurPlotMode = InfoGrScreen(PlotModeReq)
	  CALL IGrPlotMode('N')

      IF (PLOTT) CALL Profile_Plot(IPTYPE)
	  IF (DoSaRedraw)  CALL sa_output_gr()

!	  SELECT CASE (ICurPlotMode)
!		CASE (PlotNormal)
!			CALL IGrPlotMode('N')
!		CASE (PlotOr)
!			CALL IGrPlotMode('O')
!		CASE (PlotAnd)
!			CALL IGrPlotMode('A')
!		CASE (PlotEor)
!			CALL IGrPlotMode('E')
!	  END SELECT				
!		
      RETURN
      END SUBROUTINE Redraw
!*****************************************************************************
!
      SUBROUTINE WExit(QUIT)
!
!   This subroutine processes the close requests
!
      USE WINTERACTER
      USE VARIABLES
!
      IMPLICIT NONE
!
      LOGICAL,           INTENT (IN OUT) :: QUIT
!
 
      CALL WMessageBox(YesNo,StopIcon,CommonNo,&
!            'Program contains an unsaved project.'//CHAR(13)//
            'Do you want to exit DASH?','Ending DASH Session')
      IF ( WInfoDialog(4).EQ.CommonYes ) CALL DoExit
!
      RETURN
      END SUBROUTINE WExit

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
      Subroutine PolyFitterInitialise()
      USE WINTERACTER
      USE VARIABLES
      USE druid_header
!C>> JCC Cell/Lattice definitions now in an include file
	  INCLUDE 'Lattice.inc'
	  CHARACTER(LEN=128) lintem
!C>> JCC declarations
	  integer OpenFail
	  integer PolyFitter_OpenSpaceGroupSymbols
      data LPosSG/1,1,3,38,73,108,349,430,455,462,489,531/

	  LOGICAL SARDRW
	  COMMON /SARDRW/ DoSa
!

      DoSa = .FALSE.

!>> JCC Init the viewing etc

      CALL PolyFitter_EnableExternal

!
! Get the space group symbols ...
!C>> JCC This assumes that the file is in the current working directory
!C>> I've written a more general function that will handle this better
!C>> Was
!	open(110,file='SpaceGroupSymbols.dat',status='old')
! Now
    OpenFail = PolyFitter_OpenSpaceGroupSymbols(110)
	if (OpenFail .NE. 0) goto 999 ! fail gracefully!
	i=0
 10	lintem=' '
	read(110,1100,end=100) nl,lintem
 1100	format(q,a)
	if (lintem(1:1).eq.'-') goto 100
	if (nl.lt.70) then
	  do ii=nl+1,70
	    lintem(ii:ii)=' '
	  end do
	end if
	i=i+1
     SGNumStr(i) = lintem(4:13)
     SGHMaStr(i) = lintem(15:26)
     SGHalStr(i) = lintem(29:46)
     SGShmStr(i) = lintem(47:70)
	goto 10
 100 NumSG=i
     IPosSG=1
	close(110)
!C>> JCC Added in next lines
	return
 999 continue
! Failure, so exit gracefully
!
     CALL WindowClose()
	 STOP
     EndSubroutine PolyFitterInitialise



!C>> Handle file opening. Exit with a message to say that is wrong if all attempts fail
    integer function PolyFitter_OpenSpaceGroupSymbols(unit)

	use winteracter
	use variables
    use dflib ! Windows environment variable handling: for GETENVQQ
    use dfport

	integer unit, errstat, lval, dlen
	character*255 DashDir, Command
	PolyFitter_OpenSpaceGroupSymbols = 0

! Try the default installaiton directory first
	open(110,file=INSTDIR(1:len_trim(INSTDIR))//&
	            DIRSPACER(1:len_trim(DIRSPACER))//&
				SPACEGROUPS,status='old', err = 10)
	return
 10 continue

!   Fail so Look in current working directory
    open(110,file=SPACEGROUPS,status='old', err = 20, iostat = errstat)
	dlen = GETCWD(INSTDIR)
	return
 20 continue

!   Failed to open in the current working directory: try getting the environment variable DASH_DIR
	lval = GETENVQQ("DASH_DIR",DashDir)
	if (lval.LE. len(DashDir) .AND. lval .GT. 0) THEN
!		Environment variable is set
			open(110,file=DashDir(1:len_trim(DashDir))//DIRSPACER//SPACEGROUPS,status='old', err = 30)
			INSTDIR = DASHDIR
			return
 30         continue

! If DASH_DIR is set, then use a different message
			Call WMessageBox(OKOnly, ExclamationIcon, CommonOk, &
			"Sorry, DASH is not installed correctly: Could not find the file"//CHAR(13)//CHAR(13)&
			//SPACEGROUPS//CHAR(13)//CHAR(13)//"in the default installation directory "//CHAR(13)&
			//CHAR(13)//INSTDIR(1:len_trim(INSTDIR))&
			//CHAR(13)//CHAR(13)//"in your current working directory, or in the directory "//CHAR(13)//CHAR(13)&
			//DashDir(1:len_trim(DashDir)),"Installation failure")			
			PolyFitter_OpenSpaceGroupSymbols = errstat
			return
    endif
! Try looking at the command path itself and deriving the path from that

	CALL Getarg(0,Command)
	dlen = len_trim(command)
	do while (command(dlen:dlen) .ne. DIRSPACER)
		dlen = dlen - 1
	end do
    dlen = dlen - 1
	open(110,file=command(1:dlen)//DIRSPACER//SPACEGROUPS,status='old', err = 40)
	INSTDIR = command(1:dlen)
	return
 40 continue

! If we get here, all attempts failed to open the file so fail gracefully
	Call WMessageBox(OKOnly, ExclamationIcon, CommonOk, &
	"Sorry, DASH is not installed correctly: Could not find the file"//CHAR(13)//CHAR(13)&
	//SPACEGROUPS//CHAR(13)//CHAR(13)//"in the default installation directory "//CHAR(13)&
	//CHAR(13)//INSTDIR(1:len_trim(INSTDIR))&
	//CHAR(13)//CHAR(13)//"or in your current working directory", "Installation failure")
    PolyFitter_OpenSpaceGroupSymbols = errstat
	return
	end function PolyFitter_OpenSpaceGroupSymbols


!C>> 
    subroutine PolyFitter_EnableExternal()

	use winteracter
	use variables
    use dflib ! Windows environment variable handling: for GETENVQQ

	integer unit, errstat, lval
	character*255 DashDir
	character*255 line
	character*3 KeyChar
    integer nl,is,ie,dlen

    ViewOn = .FALSE.
	ConvOn = .FALSE.
	lval = GETENVQQ("DASH_DIR",DashDir)
	if (lval.LE. len(DashDir) .AND. lval .GT. 0) then
	    CONVEXE = DashDir(1:len_trim(DashDir))//DIRSPACER//'zmconv.exe'
		open(121, file=DashDir(1:len_trim(DashDir))//DIRSPACER//CONFIG, status='old', err = 10)
		GOTO 20
 10		open(121, file=INSTDIR(1:len_trim(INSTDIR))//DIRSPACER//CONFIG, status='old', err = 30)
 20     continue
    else
	    open(121, file=INSTDIR(1:len_trim(INSTDIR))//DIRSPACER//CONFIG, status='old', err = 22)
		goto 24
 22     continue
		CALL Getarg(0,line)
	    dlen = len_trim(line)
	    do while (line(dlen:dlen) .ne. DIRSPACER)
		   dlen = dlen - 1
	    end do
        dlen = dlen - 1
        open(121, file=line(1:dlen)//DIRSPACER//CONFIG, status='old', err = 30)
		INSTDIR = line(1:dlen)
 24     continue
        CONVEXE = INSTDIR(1:len_trim(INSTDIR))//DIRSPACER//'zmconv.exe'
    end if
! Read it


	DO WHILE ( .TRUE. )
 25			READ(121,'(a)',END=30,ERR=30) line
			nl=len_trim(line)
			call ILowerCase(line(:nl))
			call INextString(line,keychar)
			select case (KeyChar)
				case ('vie')
					VIEWEXE = line(IlocateChar(line):nl)
				case ('con')
					CONVEXE = line(IlocateChar(line):nl)
				case  ('arg')
				    VIEWARG = line(IlocateChar(line):nl) ! Arguments for the viewer
			end select
	END DO		
 30 continue

	INQUIRE(FILE=VIEWEXE(1:LEN_TRIM(VIEWEXE)),EXIST=ViewOn)
	INQUIRE(FILE=CONVEXE(1:LEN_TRIM(CONVEXE)),EXIST=ConvOn)

	end subroutine PolyFitter_EnableExternal
		

!C>> JCC Rather than continually load/unload the various widgets, upload them all only once
!C>> This way, the state can be memorised from session to session
	subroutine PolyFitter_UploadDialogues
	use winteracter
	use druid_header

!C>> SA bitmap
    Integer it
    INTEGER Ibmhandle
	COMMON / BMPHAN / Ibmhandle

	CALL WDialogLoad(IDD_Structural_Information)
	CALL WDialogLoad(IDD_SA_Action1)
	CALL WDialogLoad(IDD_SA_Action2)
	CALL WDialogLoad(IDD_Plot_Option_Dialog)
	CALL WDialogLoad(IDD_About_Polyfitter)
	CALL WDialogLoad(IDD_Pawley_Status)
	Call WDialogLoad(IDD_Peak_Positions)
	Call WDialogLoad(IDD_Index_Preparation)
! Set the colours of the grid manually
	CALL WDialogLoad(IDD_Plot_Option_Dialog)
	CALL WDialogLoad(IDD_SA_input1)
	CALL WDialogLoad(IDD_SA_input2)
	CALL WDialogLoad(IDD_SA_input3)
	Call WDialogLoad(IDD_Crystal_Symmetry)
	Call WDialogLoad(IDD_Data_Properties)
	call WDialogLoad(IDD_Sigma_info)
	call WDialogLoad(IDD_Gamma_info)
	call WDialogLoad(IDD_HPSL_info)
	call WDialogLoad(IDD_HMSL_info)
	CALL WDialogLoad(IDD_Polyfitter_Wizard_01)
	CALL WDialogLoad(IDD_PW_Page1)
	CALL WDialogLoad(IDD_PW_Page2)
	CALL WDialogLoad(IDD_PW_Page3)
	CALL WDialogLoad(IDD_SA_Completed)
	CALL WDialogLoad(IDD_SA_Multi_Completed)
	CALL WDialogLoad(IDD_License_Dialog)
	CALL WDialogLoad(ID_Background_Fit)
	CALL WDialogLoad(ID_Fit_data_request)
	CALL WDialogLoad(IDD_Pawley_ErrorLog)

! Upload sa bitmap into memory

	ibmhandle = 0
	CALL WDialogSelect(IDD_SA_Action1)
	CALL IGrSelect(3,IDF_minchisq_picture)
	CALL WBitmapGet(ibmhandle,0)
	it = InfoError(1)
	return
	end

 	 SUBROUTINE Init_StdOut()
 	
     USE WINTERACTER
     USE druid_header
	 USE Variables
	 USE DFLIB

	 IMPLICIT NONE
	 INTEGER :: IFlags, I, IPASS, Ilen, Instlen, Idashlen
	 LOGICAL OpenFail
	 CHARACTER(LEN=255) :: Dirname, DashDir, InstDirLc, DashDirLc, DirNameLc
!


	 Idashlen = GETENVQQ("DASH_DIR",DashDir)
	 Instlen = len_trim(INSTDIR)
     IPASS = 0
	 InstDirLc = InstDir
     DashDirLc = DashDir
	 CALL ILowerCase(DashDirLc)
	 CALL ILowerCase(InstDirLc)

	 LOOP_DIRECTORY_SELECT : DO WHILE (.TRUE.)

	   IFlags = DirChange+DirCreate
	   Dirname = ' '
	   Call WSelectDir(IFlags,Dirname,"Select working directory for DASH...")
	   IF (LEN_TRIM(Dirname) .EQ. 0) THEN
			CALL WindowClose()
			STOP
	   END IF

	   DirNameLc = DirName
	   CALL ILowerCase(DirNameLc)
	   Ilen = len_trim(DirNameLc)

	   IF ( DirNameLc(1:Ilen) .EQ. DashDirLc(1:Len_Trim(DashDirLc)) .OR.  &
	        DirNameLc(1:Ilen) .EQ. InstDirLc(1:Len_Trim(DashDirLc)) ) THEN
	       CALL WMessageBox(YesNo,InformationIcon,CommonNo, &
                       "Are you sure you wish to start Dash in"//CHAR(13)//"the installation directory "//&
						CHAR(13)//DirNameLc(1:Ilen)//" ?", &
						"File location")

		   I = WInfoDialog(4)
		   IF ( I .NE. CommonYes ) CYCLE LOOP_DIRECTORY_SELECT

	   END IF

! Open the file
	   OPEN(UNIT = 6, FILE = 'dash.out', STATUS = 'UNKNOWN', ERR = 110)
	   RETURN
 110   CONTINUE

	   CALL WMessageBox(OKCancel,ExclamationIcon,CommonOk, &
                       "DASH problem: Could not open temporary files"//&
						CHAR(13)//"in the directory "//DirName(4:Ilen)//CHAR(13)//&
						"Please pick an alternative directory for your DASH run",&
						"File-open failure")
	   I = WInfoDialog(4)
	   IF ( I .NE. 1 ) THEN
		 CALL WindowClose()
		 STOP
	   END IF
	 END DO LOOP_DIRECTORY_SELECT

	 END SUBROUTINE Init_StdOut

	 subroutine PolyFitter_Defaults()
	 call Default_Crystal_Symmetry
	 end subroutine PolyFitter_Defaults


	 subroutine check_license
	 use winteracter
	 use druid_header
	 use variables

!	 INTEGER TIMEDATA (8)
!	 CHARACTER (LEN = 12) DATEINFO (3)

	 INTEGER valid_license, ndays
	 INTEGER Read_License_Valid
	 CHARACTER*2 Exp
	 TYPE(License_Info) Info

!	 valid_license = 2
!	 CALL DATE_AND_TIME (DATEINFO (1), DATEINFO (2), DATEINFO (3), TIMEDATA)
! Check the year
!	 IF (TIMEDATA(1) .GT. EXPIRY_YEAR) THEN
!		valid_license = 0
!	 ELSE IF (TIMEDATA(1) .EQ. EXPIRY_YEAR) THEN 
! Take it further - the year is the same so check the month
!		IF (TIMEDATA(2) .GT. EXPIRY_MONTH) THEN
!			valid_license = 0
!		ELSE IF (TIMEDATA(2) .EQ. EXPIRY_MONTH) THEN
! Take it further - the month is the same so check the day
!			IF (TIMEDATA(3) .GT. EXPIRY_DAY) THEN
!				valid_license = 0
!			ELSE 
!				ndays = EXPIRY_DAY - TIMEDATA(3)
!				IF (ndays .LE. 7) THEN
!					valid_licence = 1
!				END IF
!			END IF
!		END IF
!	 END IF
     valid_license = 0
     do while (valid_license .LE. 0) 

		valid_license = Read_License_Valid()

		IF (valid_license .LE. -2) THEN
		 CALL WMessageBox(OkCancel,StopIcon,CommonOk, &
                       "DASH problem: could not find or open the license file"//char(13)//&
					   INSTDIR(1:len_trim(INSTDIR))//DIRSPACER//"License.dat"//char(13)//char(13)//&
					   "Would you like to enter a new license?",&
					   "Missing license file")
		ELSE IF (valid_license .EQ. -1) THEN
		  CALL WMessageBox(OkCancel,StopIcon,CommonOk, &
                       "DASH problem: Your DASH license is invalid for this machine"//char(13)//&
					   	"Would you like to enter a new license?",&	
					   "Invalid or expired license")
		ELSE IF (valid_license .EQ. 0) THEN
		  CALL WMessageBox(OkCancel,StopIcon,CommonOk, &
                       "DASH problem: Your DASH license has expired"//char(13)//&
					   	"Would you like to enter a new license?",&	
					   "Invalid or expired license")

	    END IF
		IF (valid_license .LE. 0) THEN
		  IF  ( WinfoDialog(4) .EQ. 1) THEN
			CALL LicensePopup()
		  ELSE
			CALL WExit(.TRUE.)
		  END IF
		END IF
     end do
	 IF (valid_license .LE. 7) THEN
		 write(Exp,'(I2)') valid_license
		 CALL WMessageBox(OKOnly,InformationIcon,CommonOk, &
                       "Information: Your DASH license will expire in "//Exp//" days", &
					   "Soon-to-expire license")
	 ENDIF

	 return
	 end subroutine check_license



    subroutine LicensePopup
	use winteracter
	use druid_header
	use variables
    LOGICAL           :: INLOOP    = .TRUE.
	LOGICAL           :: QUIT
	TYPE(WIN_MESSAGE) :: MESSAGE
	INTEGER Ident, MachId,get_DashSerialNumber, ISite, Valid, ICode
	CHARACTER*255 ClString
	type (License_Info) Info
    Info%Valid = 0
    CALL WDialogSelect(IDD_License_Dialog)
	CALL WDialogShow(-1,-1,0,SemiModeless)
	CALL WDialogFieldState(ID_Enter_License,Enabled)
	CALL WMessageEnable(FieldChanged,Enabled)
	CALL WDialogGetCheckBox(IDF_License_Site,ISite)
	IF (Isite .EQ. 1) then
		CALL WDialogFieldState(IDF_License_SiteCode,Enabled)
		CALL WDialogFieldState(IDF_License_SiteCodeLabel,Enabled)
	ELSE
		CALL WDialogFieldState(IDF_License_SiteCode,Disabled)
		CALL WDialogFieldState(IDF_License_SiteCodeLabel,Disabled)
	END IF


      DO WHILE(INLOOP)
          CALL WMessage(ITYPE,MESSAGE)
!	write(76,*) ' Main loop',itype,Message%Value1,Message%Value2
          SELECT CASE (ITYPE)
              CASE (PushButton)
				  Ident = Message%Value1
				  SELECT CASE(Ident)
					CASE (ID_Licensing_Exit)
					    CALL DoExit()
					CASE (IDCANCEL)
					    CALL WExit(QUIT)
					CASE (ID_Enter_License)
						CALL WDialogGetString(IDF_License_String, CLString)
						CALL Decode_License(CLString,Info)

						IF (Info%Valid .LT. 0 ) THEN
								CALL WMessageBox(OkOnly, ExclamationIcon, CommonOk, &
								"Sorry, the license key is invalid - please check your input",&
								"Invalid key")
						ELSE IF  ( ISite .EQ. 1 .AND.  Info%LicenseType .NE. SiteKey) THEN
								CALL WMessageBox(OkOnly, ExclamationIcon, CommonOk, &
								"Sorry, the license key is not a site license",&
								"Invalid key")
						ELSE IF ( ISite .EQ. 0 .AND. Info%LicenseType .EQ. SiteKey) THEN
								CALL WMessageBox(OkOnly, ExclamationIcon, CommonOk, &
								"The license key is a site license:"//&
								" Please select the Site License check-box and enter your site code as well",&
								"Site key")							    
						ELSE
							Valid = License_Valid(Info)
						    IF (ISite .EQ. 1) THEN
								CALL WDialogGetInteger(IDF_License_SiteCode, ICode)	
								IF (Info%SerialNumber .NE. ICode  ) THEN
									Valid = -99
								END IF
							END IF
							IF (Valid .GT. 0) THEN
								CALL Write_License_File(CLString)
								INLOOP = .FALSE.
							ELSE IF (Valid .EQ. 0) THEN
								CALL WMessageBox(OkOnly, ExclamationIcon, CommonOk, &
								"Sorry, the license key has expired ",&
								"Expired key")								
							ELSE IF (Valid .EQ. -1) THEN
								CALL WMessageBox(OkOnly, ExclamationIcon, CommonOk, &
								"Sorry, the license key is not valid for this machine ",&
								"Invalid key")
							ELSE IF (Valid .EQ. -99) THEN
								CALL WMessageBox(OkOnly, ExclamationIcon, CommonOk, &
								"Sorry, the license key is not valid for this site ",&
								"Invalid site key")																
							END IF
						END IF

					CASE (ID_Licence_Request)
					    CALL Write_License_Request_Form()
						CALL WExit(QUIT)
				  END SELECT
              CASE (Expose,Resize)
                  CALL Redraw()
              CASE (CloseRequest)
                  CALL WExit(QUIT)
			  CASE (FieldChanged)
				  CALL WDialogGetCheckBox(IDF_License_Site,ISite)
				  IF (Isite .EQ. 1) then
						CALL WDialogFieldState(IDF_License_SiteCode,Enabled)
						CALL WDialogFieldState(IDF_License_SiteCodeLabel,Enabled)
				  ELSE
						CALL WDialogFieldState(IDF_License_SiteCode,Disabled)
						CALL WDialogFieldState(IDF_License_SiteCodeLabel,Disabled)
				  END IF
          END SELECT

      END DO
	  CALL WMessageEnable(FieldChanged,Disabled)
      CALL WDialogSelect(IDD_License_Dialog)
	  CALL WDialogHide()

	 end subroutine LicensePopup

    
	 subroutine Decode_License(LString,Info)
	 use variables
	 CHARACTER*(*) LString
	 TYPE (License_Info) Info
	 INTEGER v(2),w(2),k(4), i, lsum
	 INTEGER*2 checksum, cs


 	 k(1) = 2453
	 k(2) = 1768
	 k(3) = 4567
 	 k(4) = 1453
     Info%Valid = 1
     read(Lstring,'(2z8,z4)',err = 99) v(1), v(2), checksum
	 cs =  ieor(v(1),v(2))
	 if (cs .ne. checksum) goto 99


! Check the checksum

     call decipher(v,w,k)

     Info%SerialNumber =  w(1)
	 Info%LicenseType = w(2)/100000000
	 Info%DateCode = w(2) - Info%LicenseType*100000000
	 Info%Year     = Info%DateCode/10000
	 Info%Month    = (Info%DateCode - Info%Year*10000)/100
	 Info%Day      = (Info%DateCode - Info%Year*10000 - Info%Month*100)

	 if (Info%LicenseType .EQ. SiteKey) then
		Info%SerialNumber = Info%SerialNumber - 145789123 ! demangle into a site number
	 endif 
     return 
 99  continue
	 Info%Valid = -1
	 return
	 end subroutine Decode_License


     integer function Read_License_Valid()

	 use variables
	 character*80 line, clstring
     integer today,snum,dummy
	 character*8 dt

	 TYPE(License_Info) Info


	 Read_License_Valid = -2
	 open ( unit=117,&
		    file=INSTDIR(1:len_trim(INSTDIR))//DIRSPACER//'License.dat',&
		    STATUS='OLD',&
		    ERR=99 )

	 do while (Read_License_Valid .le. 0)
		read(117,'(A)',err=99,end=99) line
		if (line(1:1) .NE. '#') then
		    CALL INextString(line,clstring)
			CALL Decode_License(CLString,Info)
			IF (Info%Valid) then
				Read_License_Valid = License_Valid(Info)
			END IF
		end if
	 end do
! Have a decodable key ...
 99  continue
     close(117,iostat=dummy)
	 return

	end function Read_License_Valid



    integer function License_Valid(Info)
	use variables
	character*80 line, clstring
    integer today,snum
	integer Get_DashSerialNumber
	character*8 dt
	TYPE(License_Info) Info

! Check the date
    License_Valid = 0
	call date_and_time(dt)
	read(dt,*,err=99) today
	License_Valid = max (0, Info%DateCode - today)


! For node-locked licenses check the serial id. Site-Wide licenses just encode a serial id for our reference
! so if we catch any non-authorized users using the key, we know where it came from. Perhaps we may want to make
! the user key in this site code on installation for checking purposes.

	 if (Info%LicenseType .EQ. NodeKey) then
		snum =  Get_DashSerialNumber("C:\\"C)
		if (snum .ne. Info%SerialNumber) then
			License_Valid = -1
		end if
	 end if	 
 99  return
	 end function License_Valid 


     subroutine Write_License_File(LString)

	 use winteracter
	 use druid_header
	 use variables
 	 integer IUN
	 parameter (IUN = 117)
     character*(*)LString

     character*8 Months(12)
	 character*11 Ctypestr
     data Months / 'January','February',   'March',   'April', &
	                   'May',    'June',    'July',  'August', &
		 	     'September', 'October','November','December'/
   
	 type (License_Info) Info

     call Decode_License(LString,Info)

	 if (Info%Valid .le. 0 ) goto 99

	 select case ( Info%LicenseType ) 
	  case (DemoKey)
		Ctypestr = 'Demo'
	  case (NodeKey)
	    Ctypestr = 'Node Locked'
	  case (SiteKey)
		Ctypestr = 'Site'
	  case default
		goto 99
	 end select

	 open ( FILE=INSTDIR(1:len_trim(INSTDIR))//DIRSPACER//'License.dat', &
	       UNIT=IUN,&
		   STATUS='UNKNOWN',&
		   ERR=99)
 
	 write (iun,'(A)',ERR=99)         "# License File for DASH"
	 write (iun,'(A)',ERR=99)         "#"
	 write (iun,'(A,A,A)',ERR=99) '# This is a ',Ctypestr(1:len_trim(Ctypestr)),' license '

	 if ( Info%LicenseType .EQ. NodeKey ) THEN
		write (iun,'(A,z8)',ERR=99) '# Your DASH Serial ID for this machine is ',Info%SerialNumber
	 else if (Info%LicenseType .EQ. NodeKey ) THEN
	   	write (iun,'(A,z8)',ERR=99) '# Your DASH Site ID is ',Info%SerialNumber
	 end if
	 
	 if (Info%Year .EQ.9999) then
		write(iun,'(A)',ERR=99)'# The license is non-expiring'
     else
 		write(iun,2,ERR=99)Info%Day, Months(Info%Month),Info%Year
  2     format('# The license expires on ',i3,1x,A,1x,i4)
     end if

	 write (iun,'(A)',ERR=99)"# License key follows :"
     write (iun,'(A)',ERR=99) LString

 99  continue
	 close(iun)
	 return

    end subroutine Write_License_File

	subroutine Write_License_Request_Form()
	 use winteracter
	 use druid_header
	 use variables
	 character*40 fstr
	 integer Iflags,Idummy, Sn
	 integer Iun,IHan
	 parameter (Iun = 117)
	 integer Get_DashSerialNumber

	 IFlags = SaveDialog + DirChange + AppendExt
     fstr = 'Text files|*.txt|All files|*.*|'
	 fname = ' '
	 CALL WSelectFile(fstr,IFlags,Fname,"Please enter a filename",Idummy)
	 if (Fname(1:1) .EQ. ' ') return
	 OPEN(unit = Iun, file = Fname(1:len_trim(Fname)),status = 'unknown',err=99)
	 Sn = Get_DashSerialNumber("C:\\"C)
	 WRITE(Iun,'(A)',ERR=100)'This file is provided to submit requests for DASH licenses'
	 WRITE(Iun,'(A)',ERR=100)'A DASH evaluation  will allow you to run DASH on any PC'
	 WRITE(Iun,'(A)',ERR=100)'A site license will allow you to install DASH on any PC on your own site'
	 WRITE(Iun,'(A)',ERR=100)'Most licenses, however, are node-locked. For this, we use a unique identifier'
	 WRITE(Iun,'(A,z8)',ERR=100)'For this PC, this is ',Sn
	 WRITE(Iun,*,ERR=100)

	 WRITE(Iun,'(A)',ERR=100)'Please complete as applicable:'
	 WRITE(Iun,*)
	 WRITE(Iun,'(A)',ERR=100)'I would like to evaluate/purchase DASH'
	 WRITE(Iun,*,ERR=100)
	 WRITE(Iun,'(A)',ERR=100)'I work in industry/an academic institution'
	 WRITE(Iun,*,ERR=100)
	 WRITE(Iun,'(A)',ERR=100)'Please enter your address here: '
	 WRITE(Iun,*,ERR=100)
	 WRITE(Iun,'(A)',ERR=100)'Name: '
	 WRITE(Iun,'(A)',ERR=100)'Address: '
	 WRITE(Iun,'(A)',ERR=100)'         '
	 WRITE(Iun,'(A)',ERR=100)'         '
	 WRITE(Iun,'(A)',ERR=100)'         '
	 WRITE(Iun,'(A)',ERR=100)'You should send the completed contents of this file to software@ccdc.cam.ac.uk'

	 WRITE(Iun,*,ERR=100)
	 close(iun,iostat=idummy)

	 CALL WMessageBox(YesNo,InformationIcon,CommonYes,&
		"A file "//Fname(1:len_trim(Fname))//" has been created."//char(13)//&
		"You should edit this file and then send it to"//char(13)//char(13)//&
		"software@ccdc.cam.ac.uk"//char(13)//char(13)//&
		"Would you like to edit this file now?","Edit license request file")
	 IF (WinfoDialog(4) .EQ. 1) THEN
	    CALL WindowOpenChild(WIN_STYLE(HideWindow,-1,-1,-1,-1,0,&
									   'Edit license request file'),IHan)
		CALL WEditFile(Fname(1:len_trim(Fname)), Modal)
	 END IF
	 return
 99  continue
 	 CALL WMessageBox(OkOnly, ExclamationIcon, CommonOk, &
				"Sorry, could not open the file "//char(13)//fname(1:len_trim(Fname)),&
				"Could not open file")
	 close(iun,iostat=idummy)
	 return		
 100 continue
	 CALL WMessageBox(OkOnly, ExclamationIcon, CommonOk, &
				"Sorry, could not write to the file "//char(13)//fname(1:len_trim(Fname)),&
				"Could not open file")
	close(iun,iostat=idummy)	
	end subroutine Write_License_Request_Form



	logical Function Run_Wizard
	Logical NoData
    CALL SetWizardState(-1)
	CALL PolyFitter_Wizard(NoData)

	IF (.NOT. NODATA) CALL SetModeMenuState(1,0,0)
    CALL PolyFitter_Wizard_Check_Status()
!	write(76,*) ' Wizard_Check_Status checked'
    IF (.not. NODATA) CALL Upload_Wizard_Information()
!	write(76,*) ' Wizard information uploaded'
	Run_Wizard = NoData
	CALL SetWizardState(1)
	end Function Run_Wizard


   logical function process_mainwindow_message(ITYPE,MESSAGE)
   use variables
   use druid_header
   use winteracter

   INTEGER ITYPE
   TYPE(WIN_MESSAGE) :: MESSAGE
   LOGICAL QUIT

   QUIT = .FALSE.
   SELECT CASE (ITYPE)
              CASE (MouseButDown)
                    CALL Plot_Alter(MESSAGE%GX,MESSAGE%GY)
              CASE (PushButton)
                  CALL Check_PushButton(MESSAGE)
              CASE (KeyDown)
                  CALL Check_KeyDown(MESSAGE)
              CASE (MenuSelect)
                  CALL ProcessMenu(MESSAGE%VALUE1,QUIT)
              CASE (FieldChanged)
                  CALL Main_Field_Changed_Routines(Message%Value1,Message%Value2)
			  CASE (TabChanged)
                  CALL Main_Field_Changed_Routines(Message%Value1,Message%Value2)
              CASE (Expose,Resize)
                  CALL Redraw()
              CASE (CloseRequest)
                  CALL WExit(QUIT)
   END SELECT
   process_mainwindow_message = QUIT
   end function process_mainwindow_message


   subroutine ToggleMenus(OnOff)
   use winteracter
   use variables
   use druid_header

   integer OnOff
   

   integer  PeakOn, PawleyOn, SolutionOn, WizardOn
   save PeakOn, PawleyOn, SolutionOn, WizardOn
   data PeakOn / 1 /
   data PawleyOn / 0 /
   data SolutionOn / 0 /
   data WizardOn / 1 /

   if (OnOff .EQ. 1) then

        CALL WMenuSetState(ID_Import_dpj_file,ItemEnabled,WintOn)
        CALL WMenuSetState(ID_Import_data,ItemEnabled,WintOn)
        CALL WMenuSetState(ID_Import_data,ItemEnabled,WintOn)
		CALL WMenuSetState(ID_get_data_properties,ItemEnabled,WintOn)
		CALL WMenuSetState(ID_get_peak_positions,ItemEnabled,WintOn)
		CALL WMenuSetState(ID_get_crystal_symmetry,ItemEnabled,WintOn)
		CALL WMenuSetState(ID_get_peak_widths,ItemEnabled,WintOn)
		CALL WMenuSetState(ID_plot_options,ItemEnabled,WintOn)
		CALL WMenuSetState(ID_PolyFitter_Help,ItemEnabled,WintOn)
		CALL WMenuSetState(ID_Polyfitter_Tips,ItemEnabled,WintOn)
		CALL WMenuSetState(ID_help_about_Polyfitter,ItemEnabled,WintOn)
		CALL WMenuSetState(ID_import_xye_file,ItemEnabled,WintOn)
		CALL WMenuSetState(ID_import_pro_file,ItemEnabled,WintOn)
		CALL SetModeMenuState(PeakOn,PawleyOn,SolutionOn)
		CALL SetWizardState(WizardOn)
   else

        CALL WMenuSetState(ID_Import_dpj_file,ItemEnabled,WintOff)
        CALL WMenuSetState(ID_Import_data,ItemEnabled,WintOff)
        CALL WMenuSetState(ID_Import_data,ItemEnabled,WintOff)
		CALL WMenuSetState(ID_get_data_properties,ItemEnabled,WintOff)
		CALL WMenuSetState(ID_get_peak_positions,ItemEnabled,WintOff)
		CALL WMenuSetState(ID_get_crystal_symmetry,ItemEnabled,WintOff)
		CALL WMenuSetState(ID_get_peak_widths,ItemEnabled,WintOff)
		CALL WMenuSetState(ID_plot_options,ItemEnabled,WintOff)
		CALL WMenuSetState(ID_PolyFitter_Help,ItemEnabled,WintOff)
		CALL WMenuSetState(ID_Polyfitter_Tips,ItemEnabled,WintOff)
		CALL WMenuSetState(ID_help_about_Polyfitter,ItemEnabled,WintOff)
		CALL WMenuSetState(ID_import_xye_file,ItemEnabled,WintOff)
		CALL WMenuSetState(ID_import_pro_file,ItemEnabled,WintOff)

		PeakOn     = WMenuGetState(ID_Peak_Fitting_Mode,ItemEnabled)
		PawleyOn   = WMenuGetState(ID_Pawley_Refinement_Mode,ItemEnabled)
		SolutionOn = WMenuGetState(ID_Structure_Solution_Mode,ItemEnabled)
		WizardOn   = WMenuGetState(ID_Start_Wizard,ItemEnabled)
		CALL SetModeMenuState(0,0,0)
		CALL SetWizardState(WizardOn)
   endif
   end subroutine ToggleMenus


   	subroutine DeleteTempFiles
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

	end subroutine DeleteTempFiles
