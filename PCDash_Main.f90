!
!*****************************************************************************
!
      PROGRAM PCDash_Main

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE DFLIB
      USE ZMVAR

      IMPLICIT NONE

      INTEGER IWID, IWIDTHS(10)
      CHARACTER(MaxPathLength) ArgString, tDirName, tFileName
      CHARACTER(7) StrFileExtension ! maximum = 7 = .zmatrix
      INTEGER ExtLen, iFrg, iDummy
      INTEGER, EXTERNAL :: Read_One_zm, DiffractionFileOpen
      INTEGER       tNumZMatrices
      CHARACTER(80) tZmatrices(10)
      INTEGER       tNextzmNum
      INTEGER       tCounter

! The following variables are there to allow the dialogue fields in the
! window dealing with Z-matrices to be handled by DO...ENDDO loops.
! The field identifiers assigned by Winteracter are not necessarily consecutive, 
! but these mappings are.

      INTEGER        IDFZMNumber,                    IDFZMFile,                &
                     IDBZMDelete,                    IDBZMBrowse,              &
                     IDBZMView,                      IDBZMEdit,                &
                     IDFZMpars,                                                &
                     IDBZMUp,                        IDBZMDown
      COMMON /IDFZM/ IDFZMNumber(1:maxfrginterface), IDFZMFile(1:maxfrginterface),      &
                     IDBZMDelete(1:maxfrginterface), IDBZMBrowse(1:maxfrginterface),    &
                     IDBZMView(1:maxfrginterface),   IDBZMEdit(1:maxfrginterface),      &
                     IDFZMpars(1:maxfrginterface),                                      &
                     IDBZMUp(1:maxfrginterface),     IDBZMDown(1:maxfrginterface)
      DATA IDFzmNumber / IDF_zmNumOf1,  IDF_zmNumOf2,  IDF_zmNumOf3,  IDF_zmNumOf4,  IDF_zmNumOf5,  IDF_zmNumOf6  /
      DATA IDFZMFile   / IDF_zmFile1,   IDF_zmFile2,   IDF_zmFile3,   IDF_zmFile4,   IDF_zmFile5,   IDF_zmFile6   /
      DATA IDBZMDelete / IDB_zmDelete1, IDB_zmDelete2, IDB_zmDelete3, IDB_zmDelete4, IDB_zmDelete5, IDB_zmDelete6 /
      DATA IDBZMBrowse / IDB_zmBrowse1, IDB_zmBrowse2, IDB_zmBrowse3, IDB_zmBrowse4, IDB_zmBrowse5, IDB_zmBrowse6 /
      DATA IDBZMView   / IDB_zmView1,   IDB_zmView2,   IDB_zmView3,   IDB_zmView4,   IDB_zmView5,   IDB_zmView6   /
      DATA IDBzmEdit   / IDB_zmEdit1,   IDB_zmEdit2,   IDB_zmEdit3,   IDB_zmEdit4,   IDB_zmEdit5,   IDB_zmEdit6   /
      DATA IDFZMpars   / IDF_ZM_pars1,  IDF_ZM_pars2,  IDF_ZM_pars3,  IDF_ZM_pars4,  IDF_ZM_pars5,  IDF_ZM_pars6  /
      DATA IDBZMUp     / IDB_Up1,       IDB_Up2,       IDB_Up3,       IDB_Up4,       IDB_Up5,       IDB_Up6       /
      DATA IDBZMDown   / IDB_Down1,     IDB_Down2,     IDB_Down3,     IDB_Down4,     IDB_Down5,     IDB_Down6       /

! Initialise Winteracter
      CALL WInitialise(' ')
      CALL GetInstallationDirectory
! Check if there are any command line arguments
! Try to redirect stdout - change working directory if unsuccessful
      IF (NARGS() .EQ. 0) CALL Init_StdOut
! Open root window
      CALL WindowOpen(FLAGS = SysMenuOn + MinButton + MaxButton + StatusBar, X = WInfoScreen(1)/10, &
                      Y = (WInfoScreen(2)/100) + 365, WIDTH = (WInfoScreen(1)*4)/5, &
                      HEIGHT = (WInfoScreen(2)*3)/8, MENUID = IDR_MENU1, &
                      TITLE = "DASH",NCOL256=128)
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
      CALL PolyFitter_UploadDialogues
! Initialise space group information
      CALL PolyFitterInitialise
      CALL InitialiseVariables
      CALL WMessageEnable(FieldChanged, Enabled)
      CALL WMessageEnable(TabChanged, Enabled)
      CALL WMessageEnable(MouseMove, Enabled)
      CALL CheckLicence
! Main message loop
      IF (NARGS() .GT. 1) THEN
        CALL GetArg(1,ArgString) 
! Parse directory and go there
        CALL SplitPath(ArgString,tDirName,tFileName)
        CALL IOsDirChange(tDirName)
! Parse directory and go there
        ExtLen = 7
        CALL FileGetExtension(ArgString,StrFileExtension,ExtLen)
        CALL StrUpperCase(StrFileExtension)
        SELECT CASE (StrFileExtension)
          CASE ('DASH   ')
            CALL PrjFileOpen(ArgString)
          CASE ('ZMATRIX')
            iFrg = 1
            frag_file(iFrg) = ArgString
            iDummy = Read_One_ZM(iFrg)
            IF (iDummy .EQ. 0) THEN ! successful read
              gotzmfile(iFrg) = .TRUE.
! Traps for Z-matrix reading
            ELSE 
              gotzmfile(iFrg) = .FALSE. 
              CALL FileErrorPopup(frag_file(iFrg),iDummy)
            ENDIF ! If the read on the Z-matrix was ok
            CALL UpdateZmatrixSelection
            CALL WizardWindowShow(IDD_SAW_Page1)
          CASE ('SDI    ')
            CALL SDIFileOpen(ArgString)
            CALL WizardWindowShow(IDD_SAW_Page1)
          CASE ('PDB    ', 'MOL2   ', 'ML2    ', 'MDL    ', 'RES    ', 'CSSR   ', 'CIF    ')
            CALL WDialogSelect(IDD_SAW_Page1)
            iFrg = 1
            CALL zmConvert(ArgString,tNumZMatrices,tZmatrices)
            IF (tNumZMatrices .EQ. 0) GOTO 999
            tNextzmNum  = 1
   10       CONTINUE
            frag_file(iFrg) = tDirName(1:LEN_TRIM(tDirName))//tZmatrices(tNextzmNum)
            iDummy = Read_One_ZM(iFrg)
            IF (iDummy .EQ. 0) THEN ! successful read
              gotzmfile(iFrg) = .TRUE.
! Find next free slot ("iFrg")
              tCounter = 1
              DO WHILE ((gotzmfile(iFrg)) .AND. (tCounter .LT. maxfrg))
                CALL INC(tCounter)
                CALL INC(iFrg)
                IF (iFrg .GT. maxfrg) iFrg = 1
              ENDDO
            ELSE
              gotzmfile(iFrg) = .FALSE.
              CALL FileErrorPopup(frag_file(iFrg),iDummy)
! Slot still free, so iFrg still OK.
            ENDIF
! More Z-matrices to read?
            CALL INC(tNextzmNum)
            IF (tNextzmNum .GT. tNumZMatrices) GOTO 999
! If no free slot found, exit
            IF (gotzmfile(iFrg)) THEN
              CALL InfoMessage('File contained more Z-matrices than available slots.')
              GOTO 999
            ENDIF
! Read next Z-matrix.
            GOTO 10
  999       CALL UpdateZmatrixSelection
            CALL WizardWindowShow(IDD_SAW_Page1)
          CASE ('RAW    ', 'CPI    ', 'DAT    ', 'TXT    ', 'MDI    ', 'POD    ', &
                'RD     ', 'SD     ', 'UDF    ', 'UXD    ', 'XYE    ', 'X01    ')
            iDummy = DiffractionFileOpen(ArgString)
            CALL WizardWindowShow(IDD_PW_Page3)
          CASE DEFAULT
            CALL ErrorMessage('Unrecognised file format.')
            CALL StartWizard
        END SELECT
      ELSE
! Go through the PolyFitter wizard
! Comment this next line out to remove the wizard
        CALL StartWizard
      ENDIF
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

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD
      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD

      INTEGER          IPMIN, IPMAX, iStart, iStop, nPoints
      COMMON /PROFIPM/ IPMIN, IPMAX, iStart, iStop, nPoints

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      REAL              PF_FWHM
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT),         &
                        PF_FWHM(MAX_NPFR)

      LOGICAL, EXTERNAL :: Confirm
      REAL xpgdif, ypgdif
      INTEGER ISTAT, tInt1, tInt2
      INTEGER, EXTERNAL :: DiffractionFileBrowse, PrjSave, PrjSaveAs
      INTEGER tCurrentRange
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

!   Branch depending on chosen menu item

      SELECT CASE (EventInfo%VALUE1)
        CASE (IDB_Open)
          CALL PrjFileBrowse
        CASE (ID_import_xye_file)
          ISTAT = DiffractionFileBrowse()
        CASE (ID_import_dpj_file)
          CALL SDIFileBrowse
        CASE (ID_Remove_Background)
          CALL PushActiveWindowID
          CALL WDialogSelect(IDD_Background_Fit)
! Initialise the background
          CALL WDialogGetInteger(IDF_NumOfIterations,tInt2)
          CALL WDialogGetInteger(IDF_WindowWidth,tInt1)
          CALL CalculateBackground(tInt1,tInt2,WDialogGetCheckBoxLogical(IDF_UseMCYN))
          CALL Profile_Plot
          CALL WDialogShow(-1,-1,0,Modeless)
          CALL PopActiveWindowID
        CASE (ID_FILE_PRINT)
          IPTYPE = -IPTYPE
          CALL Profile_Plot
          IPTYPE = -IPTYPE
        CASE (ID_SaveXYE)
          CALL SaveXYE
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
        CASE (IDB_AnalyseSolutions)
          CALL SelectMode(IDB_AnalyseSolutions)
          CALL WDialogSelect(IDD_Polyfitter_Wizard_01)
          CALL WDialogPutRadioButton(IDF_PW_Option4)
          CALL WizardWindowShow(IDD_SAW_Page5)
        CASE (ID_FitPeaks)
          CALL WCursorShape(CurHourGlass)
          DO tCurrentRange = 1, NumPeakFitRange
            IF (.NOT. RangeFitYN(tCurrentRange)) THEN
              CurrentRange = tCurrentRange
              CALL MultiPeak_Fitter
              CALL Profile_Plot
            ENDIF
          ENDDO
          CALL WCursorShape(CurCrossHair)
! Grey out 'Fit Peaks' button on toolbar
          CALL UpdateFitPeaksButtonState
! Disable Pawley refinement button and 'Next >' button in Wizard window
          CALL CheckIfWeCanDoAPawleyRefinement
          CALL CheckIfWeCanIndex
        CASE (ID_ClearPeakFitRanges)
          IF (Confirm('Do you wish to delete all peak fit ranges?')) CALL Clear_PeakFitRanges
        CASE (ID_Delabc)
          CALL Clear_UnitCell_WithConfirmation
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
        CASE (IDM_ViewPawley)
          CALL PushActiveWindowID
          CALL WDialogSelect(IDD_Structural_Information)
          CALL WDialogShow(-1,-1,0,Modeless)
          CALL WDialogSetTab(IDF_Structural_Information_tab,IDD_ViewPawley)
          CALL PopActiveWindowID
        CASE (ID_Left)
! We're going to move the graph to the left if we can
          xpgdif = xpgmax - xpgmin
          xpgmin = MAX(XPMIN,xpgmin-0.25*xpgdif)
          xpgmax = xpgmin + xpgdif
          CALL Get_IPMaxMin 
          CALL Profile_Plot
        CASE (ID_Right)
! We're going to move the graph to the right if we can
          xpgdif = xpgmax - xpgmin
          xpgmax = MIN(XPMAX,xpgmax+0.25*xpgdif)
          xpgmin = xpgmax - xpgdif
          CALL Get_IPMaxMin 
          CALL Profile_Plot
        CASE (ID_Down)
! We're going to move the graph down if we can
          ypgdif = ypgmax - ypgmin
          ypgmin = MAX(YPMIN,ypgmin-0.25*ypgdif)
          ypgmax = ypgmin + ypgdif
          CALL Get_IPMaxMin 
          CALL Profile_Plot
        CASE (ID_Up)
! We're going to move the graph up if we can
          ypgdif = ypgmax - ypgmin
          ypgmax = MIN(YPMAX,ypgmax+0.25*ypgdif)
          ypgmin = ypgmax - ypgdif
          CALL Get_IPMaxMin
          CALL Profile_Plot
        CASE (ID_Home)
! Back to full profile range
          xpgmin = XPMIN
          xpgmax = XPMAX
          ypgmin = YPMIN
          ypgmax = YPMAX
          CALL Get_IPMaxMin 
          CALL Profile_Plot 
        CASE (ID_PolyFitter_Help)
          CALL LaunchHelp
        CASE (ID_Tutorial_1, ID_Tutorial_2, ID_Tutorial_3, ID_Tutorial_4, ID_Tutorial_5)
          CALL LaunchTutorial(EventInfo%VALUE1)
        CASE (ID_help_about_Polyfitter)
          CALL About
        CASE(ID_Start_Wizard)
          CALL StartWizard
      END SELECT

      END SUBROUTINE ProcessMenu
!
!*****************************************************************************
!
      SUBROUTINE SaveXYE

      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      CHARACTER(MaxPathLength) :: tFileName
      CHARACTER(LEN=45) :: FILTER
      INTEGER iFlags, hFile, I
      LOGICAL, EXTERNAL :: FnWavelengthOK
      
      iFlags = SaveDialog + AppendExt + PromptOn
      FILTER = 'Powder diffraction files (*.xye)|*.xye|'
      tFileName = ''
      CALL WSelectFile(FILTER,iFlags,tFileName,'Save powder diffraction file')
      IF ((WInfoDialog(4) .EQ. CommonOK) .AND. (LEN_TRIM(tFileName) .NE. 0)) THEN
        hFile = 10
        OPEN(UNIT=hFile,FILE=tFileName,ERR=999)
        IF (FnWavelengthOK()) WRITE(hFile,'(F9.5)',ERR=999) ALambda
        DO I = 1, NBIN
          WRITE(hFile,'(F6.3,X,F11.3,X,F12.5)',ERR=999) XBIN(I), YOBIN(I), EBIN(I)
        ENDDO
        CLOSE(hFile)
      ENDIF
      RETURN
  999 CALL ErrorMessage('Error writing .xye file.')
      CLOSE(hFile)

      END SUBROUTINE SaveXYE
!
!*****************************************************************************
!
      SUBROUTINE LaunchHelp

      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CALL WHelpFile(InstallationDirectory(1:LEN_TRIM(InstallationDirectory))// &
       'Documentation'//DIRSPACER//'Manual'//DIRSPACER//'DASH User Guide.chm')

      END SUBROUTINE LaunchHelp
!
!*****************************************************************************
!
      SUBROUTINE About
!
!   This subroutine shows the About... dialogue
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER(LEN=512) :: CABOUT
      INTEGER tLen

      CABOUT = 'DASH: A structure solution package for X-ray powder '//CHAR(13)//&
               'diffraction, developed and distributed in collaboration'//CHAR(13)//&
               'between the ISIS Facility of the Rutherford Appleton'//CHAR(13)//&
               'Laboratory and the Cambridge Crystallographic Data Centre.'//CHAR(13)//&
               'Access to this software product is permitted only under the'//CHAR(13)//&
               'terms and conditions of a valid software licence, obtainable'//CHAR(13)//&
               'from the Cambridge Crystallographic Data Centre.'//CHAR(13)//&
               CHAR(13)//&
               ProgramVersion
      tLen = LEN_TRIM(CABOUT)
!DEC$ IF DEFINED (ONTBUG)
      CABOUT = CABOUT(1:tLen)//' (Debug version)'
      tLen = LEN_TRIM(CABOUT)
!DEC$ ENDIF
      CABOUT = CABOUT(1:tLen)//CHAR(13)//CHAR(13)//&
               'Copyright March 2004'
      CALL WMessageBox(OkOnly,InformationIcon,CommonOk,CABOUT,'About DASH')

      END SUBROUTINE About
!
!*****************************************************************************
!
      SUBROUTINE WExit
!
!   This subroutine processes the close requests
!
      IMPLICIT NONE

      LOGICAL, EXTERNAL :: Confirm

      IF (Confirm('Do you want to exit DASH?')) CALL DoExit

      END SUBROUTINE WExit
!
!*****************************************************************************
!
      SUBROUTINE DoExit

      IMPLICIT NONE

      INTEGER ISTAT

      CALL WriteConfigurationFile
      CLOSE(UNIT=12,STATUS='DELETE',IOSTAT=ISTAT)
      CLOSE(UNIT=6,STATUS='DELETE',IOSTAT=ISTAT)  ! dash.out
      CALL DeleteTempFiles
      CALL WindowClose
      STOP

      END SUBROUTINE DoExit
!
!*****************************************************************************
!
      SUBROUTINE DeleteTempFiles

      USE WINTERACTER
      USE DICVAR

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
      CALL IOsDeleteFile(DV_FileName)
      CALL IOSDeleteFile('MakeZmatrix.log')
      CALL IOSDeleteFile('SA_PARAMS.TXT')
      CALL IOSDeleteFile('Overlap_Temp.pdb')
      CALL IOSDeleteFile('Rebuild_temp*.zmatrix')
      CALL IOSDeleteFile('Rebuild_temp.glob')
      CALL IOSDeleteFile('Rebuild_temp.mol2')

      END SUBROUTINE DeleteTempFiles
!
!*****************************************************************************
!
