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

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Plot_Option_Dialog)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
! Note that the checkboxes are handled by Winteracter: there's no source code for them in DASH
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDOK,IDCANCEL)
              CALL WDialogHide()
            CASE (IDF_ObservedData_Colour)
              SelectedColour = KolObs
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) THEN ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumObs,SelectedColour%IRed,SelectedColour%IGreen,SelectedColour%IBlue)
                KolObs = SelectedColour
              ENDIF
            CASE (IDF_CalculatedData_Colour)
              SelectedColour = KolCal
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) THEN ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumCal,SelectedColour%IRed,SelectedColour%IGreen,SelectedColour%IBlue)
                KolCal = SelectedColour
              ENDIF
            CASE (IDF_DifferenceData_Colour)
              SelectedColour = KolDif
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) THEN ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumDif,SelectedColour%IRed,SelectedColour%IGreen,SelectedColour%IBlue)
                KolDif = SelectedColour
              ENDIF
            CASE (IDF_Axes_Colour)
              SelectedColour = KolMain
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) THEN ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumMain,SelectedColour%IRed,SelectedColour%IGreen,SelectedColour%IBlue)
                KolMain = SelectedColour
              ENDIF
            CASE (IDF_TickMark_Colour)
              SelectedColour = KolCTic
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) THEN ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumCTic,SelectedColour%IRed,SelectedColour%IGreen,SelectedColour%IBlue)
                KolCTic = SelectedColour
              ENDIF
            CASE (IDF_PeakFitting_Colour)
              SelectedColour = KolMTic
              CALL WSelectColour(SelectedColour)
              IF (WInfoDialog(4) .EQ. CommonOK) THEN ! Set colour if user clicked OK 
                CALL IGrPaletteRGB(KolNumMTic,SelectedColour%IRed,SelectedColour%IGreen,SelectedColour%IBlue)
                KolMTic = SelectedColour
              ENDIF
          END SELECT
          CALL Profile_Plot(IPTYPE)
        CASE (FieldChanged)
          IF (EventInfo%VALUE1 .EQ. EventInfo%VALUE2) THEN
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDF_ErrorBar_Check)
                CALL Profile_Plot(IPTYPE)
              CASE (IDF_Background_Check)
                CALL Profile_Plot(IPTYPE)
            END SELECT
          ENDIF
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

      INTEGER GetCrystalSystemFromUnitCell ! Function

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Structural_Information)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDOK) ! The 'OK' button
              CALL Download_Cell_Constants(IDD_Crystal_Symmetry)
              CALL SetCrystalSystem(GetCrystalSystemFromUnitCell())
              CALL Upload_Cell_Constants
              CALL DownloadWavelength(IDD_Data_Properties)
              CALL Generate_TicMarks
              CALL WDialogSelect(IDD_Structural_Information)
              CALL WDialogHide()
            CASE (IDCANCEL)
              CALL WDialogHide()
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithStructuralInformation 1')
          END SELECT
          CALL Profile_Plot(IPTYPE)
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
              CALL UpdateCell
              CALL Update_Space_Group(IDD_Crystal_Symmetry)
              NumPawleyRef = 0
            CASE (IDD_Peak_Widths)
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithStructuralInformation 2')
          END SELECT
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

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Data_Properties)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_Data_Download) ! The 'Apply' button
              CALL DownloadWavelength(IDD_Data_Properties)    
              CALL Generate_TicMarks
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithDiffractionSetupPane 1')
          END SELECT
          CALL Profile_Plot(IPTYPE)
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
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithDiffractionSetupPane')
      END SELECT
      CALL PopActiveWindowID
      RETURN

      END SUBROUTINE DealWithDiffractionSetupPane
!
!*****************************************************************************
!
      SUBROUTINE DealWithPeakPositionsPane

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Peak_Positions)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
          SELECT CASE (EventInfo%VALUE1)
            CASE(ID_Index_Output)
! Set the wavelength
              CALL DownLoadWavelength(IDD_Data_Properties)
              CALL WDialogSelect(IDD_Index_Preparation)
              CALL WDialogShow(-1,-1,0,Modeless)
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithPeakPositionsPane 1')
          END SELECT
        CASE (FieldChanged)
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithPeakPositionsPane')
      END SELECT
      CALL PopActiveWindowID
      RETURN

      END SUBROUTINE DealWithPeakPositionsPane
!
!*****************************************************************************
!
      LOGICAL FUNCTION NearlyEqual(Value1, Value2)
!
! This function compares two REALs and determines if they are effectively equal
!
! INPUT   : Value1 and Value2 = the values to be compared
!
! RETURNS : .TRUE.  if Value1 and Value2 differ by less than 0.000001
!           .FALSE. otherwise
!
      IMPLICIT NONE

      REAL, INTENT (IN   ) :: Value1, Value2

      NearlyEqual = (ABS(Value1 - Value2) .LT. 0.000001)

      END FUNCTION NearlyEqual
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

      INTEGER GetCrystalSystemFromUnitCell ! Function
      INTEGER SGNrMenu2Table ! Function
      REAL    tReal
      LOGICAL ValidCellAxisLength, NearlyEqual ! Functions
      INTEGER ISPosSG

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_Data_Download) ! The 'Apply' button
              CALL WDialogGetReal(IDF_ZeroPoint,ZeroPoint)
              CALL Upload_Zero_Point
              CALL Download_Cell_Constants(IDD_Crystal_Symmetry)
              CALL SetCrystalSystem(GetCrystalSystemFromUnitCell())
              CALL Generate_TicMarks
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithCrystalSymmetryPane 1')
          END SELECT
          CALL Profile_Plot(IPTYPE)
        CASE (FieldChanged)
! Due to the way Winteracter works, a FieldChanged is generated for 'REAL' input boxes
! only when that was the previous field to have the input focus. It doesn't necessarily mean
! that the field content has changed.
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_a_latt)
              CALL WDialogGetReal(IDF_a_latt,tReal)
              IF (.NOT. NearlyEqual(tReal,CellPar(1))) THEN
                CellPar(1) = tReal
                CALL UpdateCell
              ELSE ! Following line just in case user typed -9999.0
                IF (.NOT. ValidCellAxisLength(tReal)) CALL WDialogClearField(IDF_a_latt)
              ENDIF
            CASE (IDF_b_latt)
              CALL WDialogGetReal(IDF_b_latt,tReal)
              IF (.NOT. NearlyEqual(tReal,CellPar(2))) THEN
                CellPar(2) = tReal
                CALL UpdateCell
              ELSE ! Following line just in case user typed -9999.0
                IF (.NOT. ValidCellAxisLength(tReal)) CALL WDialogClearField(IDF_b_latt)
              ENDIF
            CASE (IDF_c_latt)
              CALL WDialogGetReal(IDF_c_latt,tReal)
              IF (.NOT. NearlyEqual(tReal,CellPar(3))) THEN
                CellPar(3) = tReal
                CALL UpdateCell
              ELSE ! Following line just in case user typed -9999.0
                IF (.NOT. ValidCellAxisLength(tReal)) CALL WDialogClearField(IDF_c_latt)
              ENDIF
            CASE (IDF_alp_latt)
              CALL WDialogGetReal(IDF_alp_latt,CellPar(4))
              CALL UpdateCell
            CASE (IDF_bet_latt)
              CALL WDialogGetReal(IDF_bet_latt,CellPar(5))
              CALL UpdateCell
            CASE (IDF_gam_latt)
              CALL WDialogGetReal(IDF_gam_latt,CellPar(6))
              CALL UpdateCell               
            CASE (IDF_Crystal_System_Menu)
              IF (EventInfo%VALUE1 .EQ. EventInfo%VALUE2) THEN
                CALL WDialogGetMenu(IDF_Crystal_System_Menu,LatBrav)
                CALL UserSetCrystalSystem(LatBrav)
                CALL SetSpaceGroupMenu
                CALL Generate_TicMarks
              ENDIF
            CASE (IDF_Space_Group_Menu)  
              IF (EventInfo%VALUE1 .EQ. EventInfo%VALUE2) THEN
!O                CALL Update_Space_Group(IDD_Crystal_Symmetry)
                CALL WDialogGetMenu(IDF_Space_Group_Menu,ISPosSG)
                NumberSGTable = SGNrMenu2Table(ISPosSG)
! Update the wizard
                CALL WDialogSelect(IDD_PW_Page1)
                CALL WDialogPutOption(IDF_Space_Group_Menu,ISPosSG)
                NumPawleyRef = 0
                CALL Generate_TicMarks
              ENDIF
            CASE (IDF_ZeroPoint)
              CALL WDialogGetReal(IDF_ZeroPoint,ZeroPoint)
              CALL Upload_Zero_Point               
!            CASE DEFAULT
!              CALL DebugErrorMessage('Forgot to handle something in DealWithCrystalSymmetryPane 2')
          END SELECT
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithCrystalSymmetryPane')
      END SELECT
      CALL PopActiveWindowID
      RETURN

      END SUBROUTINE DealWithCrystalSymmetryPane
!
!*****************************************************************************
!
      SUBROUTINE DealWithIndexPreparation

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'

      REAL    Temp

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Index_Preparation)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
! Note that the checkboxes are handled by Winteracter: there's no source code for them in DASH
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL)
              CALL WDialogHide()
            CASE (ID_Indexing_Create)
              CALL WDialogGetReal(IDF_Indexing_Lambda,Temp)
              IF (Temp .LT. 0.00001) THEN
                CALL ErrorMessage("The radiation wavelength has not been entered!")
              ELSE                 
                CALL Create_DicvolIndexFile()
                CALL WDialogSelect(IDD_Index_Preparation)
                CALL WDialogHide()
              END IF
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithIndexPreparation 1')
          END SELECT
          CALL Profile_Plot(IPTYPE)
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_Indexing_Lambda)
              CALL DownloadWavelength(IDD_Index_Preparation)
              CALL Generate_TicMarks   
            CASE (IDD_Index_Preparation)
              CALL DebugErrorMessage('Something unexpected happened in DealWithIndexPreparation')
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle FieldChanged in DealWithIndexPreparation')
          END SELECT
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithCrystalSymmetryPane')
      END SELECT
      CALL PopActiveWindowID
      RETURN

      END SUBROUTINE DealWithIndexPreparation
!
!*****************************************************************************
!
      SUBROUTINE UpdateCell
! 
! This routine takes the unit cell parameters as they are in the global variables
! CellPar and updates other menus accordingly.

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'statlog.inc'
      INCLUDE 'lattice.inc'

      LOGICAL FnUnitCellOK ! Function
      INTEGER GetCrystalSystemFromUnitCell ! Function

      CALL PushActiveWindowID
! Update all windows so that they show the contents of the global variables.
! This is in the cell parameters tab, in the wizard, and in the peak positions tab.
      CALL Upload_Cell_Constants()
      CALL WDialogSelect(IDD_PW_Page1)
      IF (FnUnitCellOK()) THEN
! Enable the wizard next button
        CALL WDialogFieldState(IDNEXT,Enabled)
        CALL SetCrystalSystem(GetCrystalSystemFromUnitCell())
        CALL SetSpaceGroupMenu
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
      INTEGER FUNCTION SGNrTable2Menu(TheTableNr)
!
! This function takes the number of a space group (1 - 530) and determines its
! number in the Winteracter space group menu (assuming that that the correct crystal system has been set)
!
! INPUT   : The number of a space group (1 - 530)
!
! RETURNS : The number of that space group in the space group menu
!
! Note: LatBrav must have been set correctly
!
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheTableNr

      INCLUDE 'Lattice.inc'

      SGNrTable2Menu = TheTableNr - LPosSG(LatBrav) + 1

      END FUNCTION SGNrTable2Menu
!
!*****************************************************************************
!
      INTEGER FUNCTION SGNrMenu2Table(TheMenuNr)
!
! This function takes the number of a space group in the Winteracter space group menu
! determines its number in the tables (1 - 530) (assuming that that the correct crystal system has been set)
!
! INPUT   : The number of a space group (1 - 530)
!
! RETURNS : The number of that space group in the space group menu
!
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheMenuNr

      INCLUDE 'Lattice.inc'

      SGNrMenu2Table = TheMenuNr + LPosSG(LatBrav) - 1

      END FUNCTION SGNrMenu2Table
!
!*****************************************************************************
!
      SUBROUTINE SetSpaceGroupMenu
!
! This subroutine determines which space groups are possible given the crystal system
! as held in the global variable LatBrav and
! updates the space-group menus in the main window and the wizard to contain
! only those space groups
!
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'statlog.inc'

! JvdS MaxSPGR is set to 530 in 'lattice.inc'
! Not necessary any more: with 'crystal system = unknown' eliminated,
! the number of possible space groups is always a subset determined by the
! crystal system. It's only a local variable, and it's safer this way, so just leave it for now.
      CHARACTER(LEN=24) :: SGHMaBrStr(MaxSPGR)
      INTEGER tISG, tJSG, ISPosSG, NumBrSG

      CALL PushActiveWindowID
      NumBrSG = LPosSG(LatBrav+1) - LPosSG(LatBrav)
      DO tISG = 1, NumBrSG
        tJSG = LPosSG(LatBrav) + tISG - 1
        SGHMaBrStr(tISG)( 1:12) = SGNumStr(tJSG)(1:12)
        SGHMaBrStr(tISG)(13:24) = SGHMaStr(tJSG)(1:12)
      END DO
      IF ((NumberSGTable .LT. LPosSg(LatBrav)) .OR. (NumberSGTable .GE. LPosSg(LatBrav+1))) THEN
! Current space group not possible in this crystal system: so update the space group to the first
! in the list of possibilities.
        NumberSGTable = LPosSG(LatBrav)
        ISPosSG = 1
      ELSE
        ISPosSG = NumberSGTable - LPosSG(LatBrav) + 1
      END IF
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
      CALL WDialogSelect(IDD_PW_Page1)
      CALL WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
      CALL PopActiveWindowID

      END SUBROUTINE SetSpaceGroupMenu
!
!*****************************************************************************
!
      SUBROUTINE Update_Space_Group(IUploadFrom)

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: IUploadFrom

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'statlog.inc'
      INCLUDE 'Lattice.inc'

      INTEGER SGNrMenu2Table ! Function
      INTEGER ISPosSG

      CALL PushActiveWindowID
      CALL WDialogSelect(IUploadFrom)
      CALL WDialogGetMenu(IDF_Space_Group_Menu,ISPosSG)
      NumberSGTable = SGNrMenu2Table(ISPosSG)
      CALL SetSpaceGroupMenu
      CALL PopActiveWindowID

      END SUBROUTINE Update_Space_Group
!
!*****************************************************************************
!
      SUBROUTINE SetCrystalSystem(TheCrystalSystem)
! This routine sets the menus in the main window and in the wizard to a new
! crystal system. No checks on consistency are performed.
! JvdS Not quite sure when this routine was supposed to be called.

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheCrystalSystem

      INCLUDE 'Lattice.inc'

      IF ((TheCrystalSystem .GE. 1) .AND. (TheCrystalSystem .LE. 10)) THEN
        LatBrav = TheCrystalSystem
      ELSE
        CALL DebugErrorMessage('Crystal Sytem out of range in SetCrystalSystem()')
        LatBrav = 1
      END IF
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogPutOption(IDF_Crystal_System_Menu,LatBrav)
      CALL WDialogSelect(IDD_PW_Page1)
      CALL WDialogPutOption(IDF_Crystal_System_Menu,LatBrav)
      CALL PopActiveWindowID

      END SUBROUTINE SetCrystalSystem
!
!*****************************************************************************
!
      SUBROUTINE UserSetCrystalSystem(TheCrystalSystem)
! 
! This subroutine is only called when the user explicitly requested this crystal system
! therefore, the angles etc. can be changed to the corresponding values
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheCrystalSystem

      INCLUDE 'Lattice.inc'

      IF ((TheCrystalSystem .GE. 1) .AND. (TheCrystalSystem .LE. 10)) THEN
        LatBrav = TheCrystalSystem
      ELSE
        CALL DebugErrorMessage('Crystal Sytem out of range in UserSetCrystalSystem()')
        LatBrav = 1
      END IF
      SELECT CASE (LatBrav)
        CASE ( 1) ! Triclinic
        CASE ( 2) ! Monoclinic a
          CellPar(5) =  90.0
          CellPar(6) =  90.0
        CASE ( 3) ! Monoclinic b
          CellPar(4) =  90.0
          CellPar(6) =  90.0
        CASE ( 4) ! Monoclinic c
          CellPar(4) =  90.0
          CellPar(5) =  90.0
        CASE ( 5) ! Orthorhombic
          CellPar(4) =  90.0
          CellPar(5) =  90.0
          CellPar(6) =  90.0
        CASE ( 6) ! Tetragonal
          CellPar(2) = CellPar(1)
          CellPar(4) =  90.0
          CellPar(5) =  90.0
          CellPar(6) =  90.0
        CASE ( 7, 9) ! Trigonal / Hexagonal
          CellPar(2) = CellPar(1)
          CellPar(4) =  90.0
          CellPar(5) =  90.0
          CellPar(6) = 120.0
        CASE ( 8) ! Rhombohedral
          CellPar(2) = CellPar(1)
          CellPar(3) = CellPar(1)
          CellPar(5) = CellPar(4)
          CellPar(6) = CellPar(4)
        CASE (10) ! Cubic
          CellPar(2) = CellPar(1)
          CellPar(3) = CellPar(1)
          CellPar(4) =  90.0
          CellPar(5) =  90.0
          CellPar(6) =  90.0
      END SELECT
      CALL Upload_Cell_Constants
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogPutOption(IDF_Crystal_System_Menu,LatBrav)
      CALL WDialogSelect(IDD_PW_Page1)
      CALL WDialogPutOption(IDF_Crystal_System_Menu,LatBrav)
      CALL PopActiveWindowID

      END SUBROUTINE UserSetCrystalSystem
!
!*****************************************************************************
!
      INTEGER FUNCTION GetCrystalSystemFromUnitCell
!
! This function determines the crystal system (Tri/Mon/...) from the unit cell parameters
! In DASH, the crystal system is kept in variable LatBrav
!
! RETURNS :  1 = Triclinic
!            2 = Monoclinic-a
!            3 = Monoclinic-b
!            4 = Monoclinic-c
!            5 = Orthorhombic
!            6 = Tetragonal
!          ( 7 = Trigonal          Never returned )
!            8 = Rhombohedral
!            9 = Hexagonal
!           10 = Cubic
!
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      LOGICAL ABC_Same, AB_Same, AC_Same, BC_Same, Ang_Same, Alp_90, Bet_90, Gam_90, Gam_120
      LOGICAL FnUnitCellOK ! Function
      LOGICAL NearlyEqual ! Function

! Check if cell parameters are available and make sense, otherwise set crystal system to unknown
      IF (.NOT. FnUnitCellOK()) THEN
        GetCrystalSystemFromUnitCell = 1 ! Triclinic
        RETURN
      ENDIF
      AB_Same  = NearlyEqual(CellPar(2),CellPar(1))
      BC_Same  = NearlyEqual(CellPar(3),CellPar(2))
      AC_Same  = NearlyEqual(CellPar(3),CellPar(1))
      ABC_Same = (AB_Same .AND. BC_Same)
      Alp_90   = NearlyEqual(CellPar(4),90.0)
      Bet_90   = NearlyEqual(CellPar(5),90.0)
      Gam_90   = NearlyEqual(CellPar(6),90.0)
      Gam_120  = NearlyEqual(CellPar(6),120.0)
      Ang_Same = NearlyEqual(CellPar(6),CellPar(5)) .AND. &
                 NearlyEqual(CellPar(5),CellPar(4))
      IF (ABC_Same .AND. Ang_Same) THEN
        IF (Alp_90) THEN
          GetCrystalSystemFromUnitCell = 10 ! Cubic
          RETURN
        ELSE
          GetCrystalSystemFromUnitCell = 8 ! Rhombohedral
          RETURN
        END IF
      END IF
      IF (AB_Same) THEN
        IF (Ang_Same .AND. Alp_90) THEN
          GetCrystalSystemFromUnitCell = 6 ! Tetragonal
          RETURN
        ELSE IF (Alp_90 .AND. Bet_90 .AND. Gam_120) THEN
          GetCrystalSystemFromUnitCell = 9 ! Hexagonal
          RETURN
        END IF
      END IF
      IF (Ang_Same .AND. Alp_90) THEN
        GetCrystalSystemFromUnitCell = 5 ! Orthorhombic
        RETURN
      END IF
      IF      (      Alp_90 .AND.       Bet_90 .AND. .NOT. Gam_90) THEN
        GetCrystalSystemFromUnitCell = 4 ! Monoclinic c
        RETURN
      ELSE IF (      Alp_90 .AND. .NOT. Bet_90 .AND.       Gam_90) THEN
        GetCrystalSystemFromUnitCell = 3 ! Monoclinic b
        RETURN
      ELSE IF (.NOT. Alp_90 .AND.       Bet_90 .AND.       Gam_90) THEN
        GetCrystalSystemFromUnitCell = 2 ! Monoclinic a
        RETURN
      END IF
      GetCrystalSystemFromUnitCell = 1 ! Triclinic
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
      CALL WDialogSelect(IDD_PW_Page4)
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
        CALL WDialogPutOption(IDF_Wavelength_Menu,IRadSelection)
        CALL WDialogSelect(IDD_PW_Page4)
        CALL WDialogPutOption(IDF_Wavelength_Menu,IRadSelection)
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
        GetCrystalSystem_2 = 1
      ELSE IF (ISpaceGroup .LE. MonoLim) THEN
! More work required: a, b or c?
        GetCrystalSystem_2 = 2
        DO I = LEN_TRIM(SGNumStr(IDashSg)),1,-1
          SELECT CASE(SGNumStr(IDashSg)(I:I)) 
            CASE ('a')
              GetCrystalSystem_2 = 2
            CASE ('b')
              GetCrystalSystem_2 = 3
            CASE ('c')
              GetCrystalSystem_2 = 4
            CASE (':')
              GOTO 1
          END SELECT
        END DO
   1    CONTINUE
      ELSE IF (ISpaceGroup .LE. OrthLim) THEN
        GetCrystalSystem_2 = 5
      ELSE IF (ISpaceGroup .LE. TetrLim) THEN
        GetCrystalSystem_2 = 6
      ELSE IF (ISpaceGroup .LE. TrigLim) THEN
! More work required - trigonal or rhombohedral
        GetCrystalSystem_2 = 7
        DO I = LEN_TRIM(SGNumStr(IDashSg)),1,-1
          SELECT CASE(SGNumStr(IDashSg)(I:I)) 
            CASE ('H')
              GetCrystalSystem_2 = 7
            CASE ('R')
              GetCrystalSystem_2 = 8
            CASE (':')
              GOTO 2
          END SELECT
        END DO
   2    CONTINUE
      ELSE IF (ISpaceGroup .LE. HexaLim) THEN
        GetCrystalSystem_2 = 9
      ELSE IF (ISpaceGroup .LE. 230) THEN
        GetCrystalSystem_2 = 10
      ELSE
        CALL ErrorMessage('Space group out of range.')
        GetCrystalSystem_2 = 1 ! Triclininc
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
