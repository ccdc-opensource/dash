      SUBROUTINE Main_Field_Changed_Routines(IDNumber,JDNumber)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER  IDNUMBER, JDNumber

      INCLUDE 'GLBVAR.INC' ! Contains JRadOption

!C>> JCC A few useful declarations
      INTEGER IRadSelection

      SELECT CASE (IDNumber)
        CASE (IDF_Indexing_Lambda,IDD_Index_Preparation)
          CALL DownloadWavelength(IDD_Index_Preparation)
          CALL Generate_TicMarks   
      END SELECT
      SELECT CASE (JDNumber)
!         CASE (IDF_binning)
! Change the binning
!           CALL WDialogSelect(IDD_Plot_Option_Dialog)
!           CALL WDialogGetInteger(IDF_Binning,LBin)
!           CALL Rebin_Profile()
! Now replot
!           CALL Profile_Plot(IPTYPE)
!C>> JCC Added
        CASE (IDF_Wavelength_Menu) ! tab has changed from the wavelength
          CALL WDialogGetMenu(IDF_Wavelength_Menu,IRadSelection)
          CALL SetWavelengthToSelection(IRadSelection)                
      END SELECT

      END SUBROUTINE Main_Field_Changed_Routines
!
!*****************************************************************************
!
      SUBROUTINE DealWithPlotOptionsWindow

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Poly_Colours.inc'
                
      TYPE(WIN_RGB) :: SelectedColour

      IF (EventInfo%WIN .NE. IDD_Plot_Option_Dialog) THEN
        CALL DebugErrorMessage('WinID wrong in DealWithPlotOptionsWindow')
        RETURN
      ENDIF
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Plot_Option_Dialog)
      SELECT CASE (EventType)
!        CASE (MouseButDown)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
! Note that the checkboxes are handled by Winteracter: there's no source code for them in DASH
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDOK,IDCANCEL)
              CALL WDialogHide()
            CASE (IDF_ObservedData_Colour)
              SelectedColour = KolObs
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) &    ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumObs,SelectedColour%IRed,SelectedColour%IGreen,SelectedColour%IBlue)
            CASE (IDF_CalculatedData_Colour)
              SelectedColour = KolCal
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) &    ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumCal,SelectedColour%IRed,SelectedColour%IGreen,SelectedColour%IBlue)
            CASE (IDF_DifferenceData_Colour)
              SelectedColour = KolDif
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) &    ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumDif,SelectedColour%IRed,SelectedColour%IGreen,SelectedColour%IBlue)
            CASE (IDF_Axes_Colour)
              SelectedColour = KolMain
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) &    ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumMain,SelectedColour%IRed,SelectedColour%IGreen,SelectedColour%IBlue)
            CASE (IDF_TickMark_Colour)
              SelectedColour = KolCTic
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) &    ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumCTic,SelectedColour%IRed,SelectedColour%IGreen,SelectedColour%IBlue)
            CASE (IDF_PeakFitting_Colour)
              SelectedColour = KolMTic
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) &    ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumMTic,SelectedColour%IRed,SelectedColour%IGreen,SelectedColour%IBlue)
          END SELECT
          CALL Profile_Plot(IPTYPE)
!        CASE (KeyDown)
!        CASE (MenuSelect)
        CASE (FieldChanged)
          IF (EventInfo%VALUE1 .EQ. EventInfo%VALUE2) THEN
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDF_ErrorBar_Check)
                CALL Profile_Plot(IPTYPE)
              CASE (IDF_Background_Check)
                CALL Profile_Plot(IPTYPE)
            END SELECT
          ENDIF
!        CASE (TabChanged)
!        CASE (Expose,Resize)
!        CASE (CloseRequest)
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithPlotOptionsWindow')
      END SELECT
      CALL PopActiveWindowID
      RETURN

      END SUBROUTINE DealWithPlotOptionsWindow
!
!*****************************************************************************
!
      SUBROUTINE DealWithStructuralInformation
! This is the window containing the four tabs from the 'View' menu

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'

      INTEGER IDummy

      IF (EventInfo%WIN .NE. IDD_Structural_Information) THEN
        CALL DebugErrorMessage('WinID wrong in DealWithStructuralInformation')
        RETURN
      ENDIF
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Structural_Information)
      SELECT CASE (EventType)
!        CASE (MouseButDown)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
! Note that the checkboxes are handled by Winteracter: there's no source code for them in DASH
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDOK) ! The 'OK' button
              CALL Check_Crystal_Symmetry()
              CALL DownloadWavelength(IDD_Data_Properties)
              CALL WDialogSelect(IDD_Structural_Information)
              CALL WDialogHide()
            CASE (IDCANCEL)
              CALL WDialogHide()
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithStructuralInformation 1')
          END SELECT
          CALL Profile_Plot(IPTYPE)
!        CASE (KeyDown)
!        CASE (MenuSelect)
        CASE (FieldChanged)
! Do nothing
        CASE (TabChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDD_Data_Properties)
              CALL DownloadWavelength(IDD_Data_Properties)
              CALL Generate_TicMarks
            CASE (IDD_Peak_Positions)
            CASE (IDD_Crystal_Symmetry)
              CALL Download_Cell_Constants(IDD_Crystal_Symmetry)
              CALL UpdateCell(IDD_Crystal_Symmetry)
              CALL Update_Space_Group(IDD_Crystal_Symmetry, IDummy, IDummy)
              NumPawleyRef = 0
              CALL Generate_TicMarks             
            CASE (IDD_Peak_Widths)
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithStructuralInformation 2')
          END SELECT
!        CASE (Expose,Resize)
!        CASE (CloseRequest)
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithStructuralInformation')
      END SELECT
      CALL PopActiveWindowID
      RETURN

      END SUBROUTINE DealWithStructuralInformation
!
!*****************************************************************************
!
      SUBROUTINE DealWithDiffractionSetupPane

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
                
      INTEGER IRadSelection

      IF (EventInfo%WIN .NE. IDD_Data_Properties) THEN
        CALL DebugErrorMessage('WinID wrong in DealWithDiffractionSetupPane')
        RETURN
      ENDIF
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Data_Properties)
      SELECT CASE (EventType)
!        CASE (MouseButDown)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
! Note that the checkboxes are handled by Winteracter: there's no source code for them in DASH
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_Data_Download) ! The 'Apply' button
              CALL DownloadWavelength(IDD_Data_Properties)    
! JvdS @ Why isn't there a CALL Generate_TicMarks here?
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithDiffractionSetupPane 1')
          END SELECT
          CALL Profile_Plot(IPTYPE)
!        CASE (KeyDown)
!        CASE (MenuSelect)
        CASE (FieldChanged)
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDF_LabX_Source,IDF_SynX_Source,IDF_CWN_Source,IDF_TOF_source)
                CALL WDialogGetRadioButton(IDF_LabX_Source,JRadOption)
                CALL SetSourceDataState(JRadOption)
                CALL Generate_TicMarks 
              CASE (IDF_Wavelength_Menu) ! Wavelength menu selection
                CALL WDialogGetMenu(IDF_Wavelength_Menu,IRadSelection)
                CALL SetWavelengthToSelection(IRadSelection)
              CASE (IDF_wavelength1)
                CALL DownloadWavelength(IDD_Data_Properties)
                CALL Generate_TicMarks
              CASE DEFAULT
                CALL DebugErrorMessage('Forgot to handle something in DealWithDiffractionSetupPane 2')
            END SELECT
!        CASE (TabChanged)
!        CASE (Expose,Resize)
!        CASE (CloseRequest)
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithDiffractionSetupPane')
      END SELECT
      CALL PopActiveWindowID
      RETURN

      END SUBROUTINE DealWithDiffractionSetupPane
!
!*****************************************************************************
!
      SUBROUTINE DealWithCrystalSymmetryPane

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'LATTICE.INC'
      INCLUDE 'statlog.inc'

      INTEGER IDummy

      IF (EventInfo%WIN .NE. IDD_Crystal_Symmetry) THEN
        CALL DebugErrorMessage('WinID wrong in DealWithCrystalSymmetryPane')
        RETURN
      ENDIF
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      SELECT CASE (EventType)
!        CASE (MouseButDown)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
! Note that the checkboxes are handled by Winteracter: there's no source code for them in DASH
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_Data_Download) ! The 'Apply' button
              CALL Check_Crystal_Symmetry()
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithCrystalSymmetryPane 1')
          END SELECT
          CALL Profile_Plot(IPTYPE)
!        CASE (KeyDown)
!        CASE (MenuSelect)
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_a_latt)
              CALL WDialogGetReal(IDF_a_latt,CellPar(1))
              CALL UpdateCell(IDD_Crystal_Symmetry)
            CASE (IDF_b_latt)
              CALL WDialogGetReal(IDF_b_latt,CellPar(2))
              CALL UpdateCell(IDD_Crystal_Symmetry)
            CASE (IDF_c_latt)
              CALL WDialogGetReal(IDF_c_latt,CellPar(3))
              CALL UpdateCell(IDD_Crystal_Symmetry)
            CASE (IDF_alp_latt)
              CALL WDialogGetReal(IDF_alp_latt,CellPar(4))
              CALL UpdateCell(IDD_Crystal_Symmetry)
            CASE (IDF_bet_latt)
              CALL WDialogGetReal(IDF_bet_latt,CellPar(5))
              CALL UpdateCell(IDD_Crystal_Symmetry)
            CASE (IDF_gam_latt)
              CALL WDialogGetReal(IDF_gam_latt,CellPar(6))
              CALL UpdateCell(IDD_Crystal_Symmetry)               
            CASE (IDF_Crystal_System_Menu)
              CALL WDialogGetMenu(IDF_Crystal_System_Menu,LatBrav)
              CALL SetCrystalSystem(LatBrav)
              CALL SetSpaceGroupMenu(LatBrav)
              CALL Generate_TicMarks
            CASE (IDF_Space_Group_Menu)  
              CALL Update_Space_Group(IDD_Crystal_Symmetry, IDummy, IDummy)
              NumPawleyRef = 0
              CALL Generate_TicMarks
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithCrystalSymmetryPane 2')
          END SELECT
!        CASE (TabChanged)
!        CASE (Expose,Resize)
!        CASE (CloseRequest)
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithCrystalSymmetryPane')
      END SELECT
      CALL PopActiveWindowID
      RETURN

      END SUBROUTINE DealWithCrystalSymmetryPane
!
!*****************************************************************************
!
      SUBROUTINE UpdateCell(IUploadFrom)

      USE WINTERACTER
      USE DRUID_HEADER

      INCLUDE 'statlog.inc'
      LOGICAL FnUnitCellOK ! Function
      INTEGER GetCrystalSystemFromUnitCell ! Function

      CALL PushActiveWindowID
      CALL Upload_Cell_Constants()
      CALL WDialogSelect(IDD_PW_Page1)
      IF (FnUnitCellOK()) THEN
! Enable the wizard next button
        CALL WDialogFieldState(IDNEXT,Enabled)
!O        CALL Check_Crystal_Symmetry()
        CALL SetCrystalSystem(GetCrystalSystemFromUnitCell())
        CALL WDialogGetMenu(IDF_Crystal_System_Menu,LatBrav)
        CALL SetCrystalSystem(LatBrav)
        CALL SetSpaceGroupMenu(LatBrav)
        CALL Generate_TicMarks
      ELSE
! Disable the wizard next button
        CALL WDialogFieldState(IDNEXT,Disabled)
      END IF
      CALL Generate_TicMarks
      CALL PopActiveWindowID

      END SUBROUTINE UpdateCell
!
!*****************************************************************************
!
!U!O      NumBrSG = LPosSG(LatBrav+1) - LPosSG(LatBrav)
!U!O! Only update if the current setting is not of the correct lattice type
!U!O      IF ((LatBrav .EQ. 1) .OR. &
!U!O          (IPosSg .GE. LPosSg(LatBrav) .AND. IPosSg .LT. LPosSg(LatBrav) + NumBrSg) ) THEN
!U!O! Selection of same lattice so retain current space group
!U!O        CALL PopActiveWindowID
!U!O        RETURN
!U!O      END IF
!
!*****************************************************************************
!
      SUBROUTINE SetSpaceGroupMenu(TheCrystalSystem)
!
! This subroutine determines which space groups are possible given the crystal system and
! updates the space-group menus in the main window and the wizard to contain
! only those space groups
!
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheCrystalSystem

      INCLUDE 'Lattice.inc'
      INCLUDE 'statlog.inc'

      INTEGER tISG, tJSG

      CALL PushActiveWindowID
! If crystal system unknown, allow only P 1
      IF (TheCrystalSystem .EQ. 1) THEN
        NumBrSG = 1
      ELSE
        NumBrSG = LPosSG(TheCrystalSystem+1) - LPosSG(TheCrystalSystem)
      ENDIF
      ISPosSG = 1
      DO tISG = 1, NumBrSG
        tJSG = LPosSG(TheCrystalSystem) + tISG - 1
        SGHMaBrStr(tISG)( 1:12) = SGNumStr(tJSG)(1:12)
        SGHMaBrStr(tISG)(13:24) = SGHMaStr(tJSG)(1:12)
      END DO
      IPosSG = LPosSG(TheCrystalSystem)
      NumberSGTable = IPosSG
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumBrSG,1)
      CALL WDialogSelect(IDD_PW_Page1)
      CALL WDialogPutMenu(IDF_PW_Space_Group_Menu,SGHMaBrStr,NumBrSG,1)
      CALL PopActiveWindowID

      END SUBROUTINE SetSpaceGroupMenu
!
!*****************************************************************************
!
      SUBROUTINE Update_Space_Group(IUploadFrom, TheCrystalSystem, ISgnum)
! JvdS A number of things is odd about this routine:
! 2. The subroutine has both the space group and the crystal system as arguments:
!    a. Is their consistency checked?
!    b. The crystal system is determined by the space group => redundant info

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: IUploadFrom
      INTEGER, INTENT (IN   ) :: TheCrystalSystem
      INTEGER, INTENT (IN   ) :: ISgnum

      INCLUDE 'statlog.inc'
      INCLUDE 'Lattice.inc'

      CALL PushActiveWindowID
      IF (IUploadFrom .EQ. IDD_Crystal_Symmetry) THEN
        CALL WDialogSelect(IDD_Crystal_Symmetry)
        CALL WDialogGetMenu(IDF_Crystal_System_Menu,LatBrav)
        CALL WDialogGetMenu(IDF_Space_Group_Menu,ISPosSG)
      ELSE IF (IUploadFrom .EQ. IDD_PW_Page1) THEN
        CALL WDialogSelect(IDD_PW_Page1)
        CALL WDialogGetMenu(IDF_Crystal_System_Menu,LatBrav)
        CALL WDialogGetMenu(IDF_PW_Space_Group_Menu,ISPosSG)
      ELSE
        LatBrav = TheCrystalSystem
        ISPosSG = ISgnum - LPosSG(LatBrav) + 1
      END IF
      CALL SetSpaceGroupMenu(LatBrav)
! Actual space group has been reset to the first of the list belonging to this crystal system,
! reset it to its original value
      NumberSGTable = LPosSG(LatBrav) + ISPosSG - 1 
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogPutOption(IDF_Space_Group_Menu,ISPosSG)
      CALL WDialogSelect(IDD_PW_Page1)
      CALL WDialogPutOption(IDF_PW_Space_Group_Menu,ISPosSG)
      CALL SetCrystalSystem(LatBrav)
      CALL PopActiveWindowID

      END SUBROUTINE Update_Space_Group
!
!*****************************************************************************
!
      INTEGER FUNCTION GetCrystalSystemFromUnitCell
!
! This function determines the crystal system (Unk/Tri/Mon/...) from the unit cell parameters
! In DASH, the crystal system is kept in variable LatBrav
!
! RETURNS :  1 = Unknown
!            2 = Triclinic
!            3 = Monoclinic-a
!            4 = Monoclinic-b
!            5 = Monoclinic-c
!            6 = Orthorhombic
!            7 = Tetragonal
!          ( 8 = Trigonal          Never returned )
!            9 = Rhombohedral
!           10 = Hexagonal
!           11 = Cubic
!
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      LOGICAL ABC_Same, AB_Same, AC_Same, BC_Same, Ang_Same, Alp_90, Bet_90, Gam_90, Gam_120
      INTEGER I
      LOGICAL FnUnitCellOK ! Function
      REAL, PARAMETER :: SmallVal = 1.0E-6

! Check if cell parameters are available and make sense, otherwise set crystal system to unknown
      IF (.NOT. FnUnitCellOK()) THEN
        GetCrystalSystemFromUnitCell = 1 ! Unknown
        RETURN
      ENDIF
      AB_Same  = (ABS(CellPar(2)-CellPar(1)) .LE. SmallVal)
      BC_Same  = (ABS(CellPar(3)-CellPar(2)) .LE. SmallVal)
      AC_Same  = (ABS(CellPar(3)-CellPar(1)) .LE. SmallVal)
      ABC_Same = (AB_Same .AND. BC_Same)
      Alp_90   = (ABS(CellPar(4)- 90.0) .LE. SmallVal)
      Bet_90   = (ABS(CellPar(5)- 90.0) .LE. SmallVal)
      Gam_90   = (ABS(CellPar(6)- 90.0) .LE. SmallVal)
      Gam_120  = (ABS(CellPar(6)-120.0) .LE. SmallVal)
      Ang_Same = (ABS(CellPar(6)-CellPar(5)) .LE. SmallVal) .AND. &
                 (ABS(CellPar(5)-CellPar(4)) .LE. SmallVal)
      IF (ABC_Same .AND. Ang_Same) THEN
        IF (Alp_90) THEN
          GetCrystalSystemFromUnitCell = 11 ! Cubic
          RETURN
        ELSE
          GetCrystalSystemFromUnitCell = 9 ! Rhombohedral
          RETURN
        END IF
      END IF
      IF (AB_Same) THEN
        IF (Ang_Same .AND. Alp_90) THEN
          GetCrystalSystemFromUnitCell = 7 ! Tetragonal
          RETURN
        ELSE IF (Alp_90 .AND. Bet_90 .AND. Gam_120) THEN
          GetCrystalSystemFromUnitCell = 10 ! Hexagonal
          RETURN
        END IF
      END IF
      IF (Ang_Same .AND. Alp_90) THEN
        GetCrystalSystemFromUnitCell = 6 ! Orthorhombic
        RETURN
      END IF
      IF      (      Alp_90 .AND.       Bet_90 .AND. .NOT. Gam_90) THEN
        GetCrystalSystemFromUnitCell = 5 ! Monoclinic c
        RETURN
      ELSE IF (      Alp_90 .AND. .NOT. Bet_90 .AND.       Gam_90) THEN
        GetCrystalSystemFromUnitCell = 4 ! Monoclinic b
        RETURN
      ELSE IF (.NOT. Alp_90 .AND.       Bet_90 .AND.       Gam_90) THEN
        GetCrystalSystemFromUnitCell = 3 ! Monoclinic a
        RETURN
      END IF
      GetCrystalSystemFromUnitCell = 2 ! Triclinic
      RETURN

      END FUNCTION GetCrystalSystemFromUnitCell
!
!*****************************************************************************
!
!C>> Sequence of subroutines that handle the downloading of each field in turn
      SUBROUTINE DownLoadWavelength(From)

      USE WINTERACTER
      USE DRUID_HEADER

      INTEGER, INTENT (IN   ) :: From

      INCLUDE 'statlog.inc'
      INCLUDE 'Lattice.inc'
      REAL Temp

      CALL PushActiveWindowID
      IF (From .EQ. IDD_Data_Properties) THEN
        CALL WDialogSelect(IDD_Data_Properties)
        CALL WDialogGetReal(IDF_wavelength1,Temp)
      ELSE IF (From .EQ. IDD_Index_Preparation) THEN
        CALL WDialogSelect(IDD_Index_Preparation)
        CALL WDialogGetReal(IDF_Indexing_Lambda,Temp)
      END IF
! JvdS @ and what about IDF_PW_wavelength1?
      CALL UpdateWavelength(Temp)
      CALL PopActiveWindowID

      END SUBROUTINE DownLoadWavelength
!
!*****************************************************************************
!
      SUBROUTINE SetWavelengthToSelection(Iselection)

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: Iselection

      REAL FnWavelengthOfMenuOption ! Function

! Winteracter menu:
!     1 = <...>
!     2 = Cu      <==  DEFAULT
!     3 = Mo
!     4 = Co
!     5 = Cr
!     6 = Fe

      IF ((Iselection .GE. 2) .AND. (Iselection .LE. 6)) CALL UpdateWavelength(FnWavelengthOfMenuOption(Iselection))
      RETURN

      END SUBROUTINE SetWavelengthToSelection
!
!*****************************************************************************
!
      REAL FUNCTION FnWavelengthOfMenuOption(TheOption)
!
! This function returns the wavelength that goes with the anode material as selected
! from the Winteracter menus:

! Winteracter menu:
!     1 = <...>
!     2 = Cu      <==  DEFAULT
!     3 = Mo
!     4 = Co
!     5 = Cr
!     6 = Fe
!
! JvdS 5 Aug 2001
!
! INPUT   : TheOption = the number of the selected option, e.g. 'Cu' = 2
!
! RETURNS : The wavelength of the anode material of that menu option in Angstrom
!           ErrorMessage if TheOption not in range (should never happen)
!
! NOTE don't call this function with TheOption 1, because that will initialise the wavelength to 0.0
!
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheOption

      REAL WavelengthOf ! Function

      SELECT CASE (TheOption)
        CASE (2)
          FnWavelengthOfMenuOption = WavelengthOf('Cu')
        CASE (3)
          FnWavelengthOfMenuOption = WavelengthOf('Mo')
        CASE (4)
          FnWavelengthOfMenuOption = WavelengthOf('Co')
        CASE (5)
          FnWavelengthOfMenuOption = WavelengthOf('Cr')
        CASE (6)
          FnWavelengthOfMenuOption = WavelengthOf('Fe')
        CASE DEFAULT
          CALL DebugErrorMessage('Programming error in FnWavelengthOfMenuOption')
          FnWavelengthOfMenuOption = 0.0
      END SELECT
      RETURN

      END FUNCTION FnWavelengthOfMenuOption
!
!*****************************************************************************
!
      SUBROUTINE UpdateWavelength(TheWaveLength)
! Should be renamed to 'SetWavelength'/'UploadWavelength'

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      REAL, INTENT (IN   ) :: TheWaveLength

      INCLUDE 'GLBVAR.INC' ! Contains ALambda

      INTEGER I, IRadSelection
      REAL    FnWavelengthOfMenuOption ! Function

      IF ((TheWaveLength .GT. 0.1) .AND. (TheWaveLength .LT. 20.0)) THEN
        ALambda = TheWaveLength
      ENDIF
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Data_Properties)
      CALL WDialogPutReal(IDF_wavelength1,ALambda,'(f10.5)')
      CALL WDialogSelect(IDD_PW_Page2)
      CALL WDialogPutReal(IDF_PW_wavelength1,ALambda,'(f10.5)')
      CALL WDialogSelect(IDD_Index_Preparation)
      CALL WDialogPutReal(IDF_Indexing_Lambda,ALambda,'(f10.5)')
! Now add in a test: if lab data, and wavelength close to known material,
! set anode material in Winteracter menus. Otherwise, anode is unknown.
      IF (JRadOption .EQ. 1) THEN ! X-ray lab data
! Initialise anode material to unknown
        IRadSelection = 1 ! <...> in the Winteracter menu
        DO I = 2, 6
          IF (ABS(ALambda - FnWavelengthOfMenuOption(I)) .LT. 0.0003) IRadSelection = I
        END DO
        CALL WDialogSelect(IDD_Data_Properties)
        CALL WDialogPutOption(IDF_Wavelength_Menu,IRadSelection)
        CALL WDialogSelect(IDD_PW_Page2)
        CALL WDialogPutOption(IDF_PW_Wavelength_Menu,IRadSelection)
      ENDIF
      CALL PopActiveWindowID

      END SUBROUTINE UpdateWavelength
!
!*****************************************************************************
!
!C>> ID Lattice from space group number
      INTEGER FUNCTION GetCrystalSystem_2(ISpaceGroup,IDashSg)

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: ISpaceGroup
      INTEGER, INTENT (IN   ) :: IDashSg

      INCLUDE 'Lattice.inc'

      INTEGER,PARAMETER ::  TricLim =   2
      INTEGER,PARAMETER ::  MonoLim =  15
      INTEGER,PARAMETER ::  OrthLim =  74
      INTEGER,PARAMETER ::  TetrLim = 142
      INTEGER,PARAMETER ::  TrigLim = 167
      INTEGER,PARAMETER ::  HexaLim = 194

      INTEGER I

      IF      (ISpaceGroup .LE. TricLim) THEN
        GetCrystalSystem_2 = 2
      ELSE IF (ISpaceGroup .LE. MonoLim) THEN
! More work required: a, b or c?
        GetCrystalSystem_2 = 3
        DO I = LEN_TRIM(SGNumStr(IDashSg)),1,-1
          SELECT CASE(SGNumStr(IDashSg)(I:I)) 
            CASE ('a')
              GetCrystalSystem_2 = 3
            CASE ('b')
              GetCrystalSystem_2 = 4
            CASE ('c')
              GetCrystalSystem_2 = 5
            CASE (':')
              GOTO 1
          END SELECT
        END DO
   1    CONTINUE
      ELSE IF (ISpaceGroup .LE. OrthLim) THEN
        GetCrystalSystem_2 = 6
      ELSE IF (ISpaceGroup .LE. TetrLim) THEN
        GetCrystalSystem_2 = 7
      ELSE IF (ISpaceGroup .LE. TrigLim) THEN
! More work required - trigonal or rhombohedral
        GetCrystalSystem_2 = 8
        DO I = LEN_TRIM(SGNumStr(IDashSg)),1,-1
          SELECT CASE(SGNumStr(IDashSg)(I:I)) 
            CASE ('H')
              GetCrystalSystem_2 = 8
            CASE ('R')
              GetCrystalSystem_2 = 9
            CASE (':')
              GOTO 2
          END SELECT
        END DO
   2    CONTINUE
      ELSE IF (ISpaceGroup .LE. HexaLim) THEN
        GetCrystalSystem_2 = 10
      ELSE IF (ISpaceGroup .LE. 230) THEN
        GetCrystalSystem_2 = 11
      ELSE
        CALL ErrorMessage('Space group out of range.')
        GetCrystalSystem_2 = 1 ! Unknown
      ENDIF
      RETURN

      END FUNCTION GetCrystalSystem_2
!
!*****************************************************************************
!
      SUBROUTINE SetSAFileName(filename)

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      CHARACTER*(*) filename

      CALL PushActiveWindowID
! JvdS Started to add SA to Wizard
!      CALL WDialogSelect(IDD_SA_input1)
      CALL WDialogSelect(IDD_SAW_Page1)
      CALL WDialogPutString(IDF_SA_Project_Name,filename)
      CALL PopActiveWindowID

      END SUBROUTINE SetSAFileName
!
!*****************************************************************************
!
      SUBROUTINE SetWizardState(State)

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: State

      IF (State .EQ. 1) THEN
        CALL WMenuSetState(ID_Start_Wizard,ItemEnabled,WintOn)
      ELSE
        CALL WMenuSetState(ID_Start_Wizard,ItemEnabled,WintOff)
      END IF
      RETURN

      END SUBROUTINE SetWizardState
!
!*****************************************************************************
!
!>> JCC Subroutine for controlling the configuration of the menus and tool buttons in DASH
      SUBROUTINE SetModeMenuState(PeakOn,PawleyOn,SolutionOn)
!>> If PeakOn is positive then peak fitting will be enabled
!>> If PawleyOn is positive then Pawley fitting will be enabled
!>> Is SolutionOn is positive solving will be enabled
!>> If PeakOn is negative then peak fitting will be disabled
!>> If PawleyOn is negative then Pawley fitting will be disabled
!>> Is SolutionOn is negative solving will be disabled
!>> If PeakOn is zero then the peak fitting state is left as is
!>> If PawleyOn is zero then the Pawley fitting state is left as is
!>> Is SolutionOn is zero then the solving state is left as is

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: PeakOn, PawleyOn, SolutionOn

      IF (PeakOn .GT. 0) THEN
        CALL WMenuSetState(ID_Peak_Fitting_Mode,ItemEnabled,WintOn)
      ELSE IF (PeakOn .LT. 0) THEN
        CALL WMenuSetState(ID_Peak_Fitting_Mode,ItemEnabled,WintOff)
      END IF
      IF (PawleyOn .GT. 0) THEN
        CALL WMenuSetState(ID_Pawley_Refinement_Mode,ItemEnabled,WintOn)
      ELSE IF (PawleyOn .LT. 0) THEN
        CALL WMenuSetState(ID_Pawley_Refinement_Mode,ItemEnabled,WintOff)
      END IF
      IF (SolutionOn .GT. 0) THEN
        CALL WMenuSetState(ID_Structure_Solution_Mode,ItemEnabled,WintOn)
      ELSE IF (SolutionOn .LT. 0) THEN
        CALL WMenuSetState(ID_Structure_Solution_Mode,ItemEnabled,WintOff)
      END IF
      RETURN

      END SUBROUTINE SetModeMenuState
!
!*****************************************************************************
!
!U!>> JCC Subroutine for controlling the selection of the mode menu and tool buttons in DASH
!U      SUBROUTINE SelectModeMenuState(Selection)
!U
!U      USE WINTERACTER
!U      USE DRUID_HEADER
!U
!U      IMPLICIT NONE
!U
!U      INTEGER, INTENT (IN   ) :: Selection
!U
!U      CALL WMenuSetState(ID_Peak_Fitting_Mode,      ItemChecked,WintOff)
!U      CALL WMenuSetState(ID_Pawley_Refinement_Mode, ItemChecked,WintOff)
!U      CALL WMenuSetState(ID_Structure_Solution_Mode,ItemChecked,WintOff)
!U      IF      (Selection .EQ. ID_Peak_Fitting_Mode)       THEN
!U        CALL WMenuSetState(ID_Peak_Fitting_Mode,      ItemChecked,WintOn)
!U      ELSE IF (Selection .EQ. ID_Pawley_Refinement_Mode)  THEN 
!U        CALL WMenuSetState(ID_Pawley_Refinement_Mode, ItemChecked,WintOn)
!U      ELSE IF (Selection .EQ. ID_Structure_Solution_Mode) THEN 
!U        CALL WMenuSetState(ID_Structure_Solution_Mode,ItemChecked,WintOn)
!U      END IF
!U      RETURN
!U
!U      END SUBROUTINE SelectModeMenuState
!
!*****************************************************************************
!
