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
        CASE (IDD_Data_Properties)
          CALL DebugErrorMessage('fksdhfrwiugf')
          CALL DownloadWavelength(IDD_Data_Properties)
          CALL Generate_TicMarks
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
            CASE (IDF_Dismiss_StrInf) ! The 'OK' button
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
! @ Does the following do what it is supposed to do
! Download the data from the structural information pages
              CALL Check_Crystal_Symmetry()
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
            CASE (IDF_Data_Download)
              CALL Check_Crystal_Symmetry()
! JvdS @ Following line is very weird: we're in a completely different window
              CALL DownloadWavelength(IDD_Data_Properties)        
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
              CALL Set_Space_Group(IDD_Crystal_Symmetry)
!C>> JCC If the system is reset then regenerate the tic marks
              CALL Generate_TicMarks
            CASE (IDF_Space_Group_Menu)  
              CALL Update_Space_Group(IDD_Crystal_Symmetry, IDummy, IDummy)
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

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page1)
      IF (FnUnitCellOK()) THEN
! Enable the wizard next button
        CALL WDialogFieldState(IDNEXT,Enabled)
        CALL Upload_Cell_Constants()
        CALL Check_Crystal_Symmetry()
        CALL Set_Space_Group(IUploadFrom)
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
      SUBROUTINE Set_Space_Group(IUploadFrom)

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

!C>> JCC Now Cell/Lattice in an include file
      INCLUDE 'Lattice.inc'
      INCLUDE 'statlog.inc'
      INTEGER IUploadFrom
      INTEGER ICurSg

      CALL PushActiveWindowID
      IF (IUpLoadFrom .EQ. IDD_Crystal_Symmetry) THEN
        CALL WDialogSelect(IDD_Crystal_Symmetry)
        CALL WDialogGetMenu(IDF_Crystal_System_Menu,IOption)
!        Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
!        Call WDialogGetMenu(IDF_Space_Group_Menu,ICurSg)
      ELSE
        CALL WDialogSelect(IDD_PW_Page1)
        CALL WDialogGetMenu(IDF_PW_Crystal_System_Menu,IOption)
!        Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
!        Call WDialogGetMenu(IDF_PW_Space_Group_Menu,ICurSg)
      END IF
!      Call WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
11    NumBrSG = LPosSG(IOption+1) - LPosSG(IOption)
!      IActSg = ICurSG + LPosSG(IOption)
! Only update if the current setting is not of the correct lattice type
      IF ((IOption .EQ. 1) .OR. &
          (IPosSg .GE. LPosSg(IOption) .AND. IPosSg .LT. LPosSg(IOption) + NumBrSg) ) THEN
! Selection of same lattice so retain current space group
!C>> Reset to whatever was previously selected
        CALL PopActiveWindowID
        RETURN
      END IF
      ISPosSG = 1
      DO ISG = 1, NumBrSG
        JSG = LPosSG(IOption) + ISG - 1
        SGHMaBrStr(ISG)(1:12)  = SGNumStr(JSG)(1:12)
        SGHMaBrStr(ISG)(13:24) = SGHMaStr(JSG)(1:12)
      END DO
      IPosSG = LPosSG(IOption)
      NumberSGTable = IPosSG
      CALL WDialogSelect(IDD_Crystal_Symmetry)
!O      CALL WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
      CALL WDialogPutOption(IDF_Crystal_System_Menu,IOption)
      CALL WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
! JvdS @ Next line should be obsolete
!U      CALL WDialogPutOption(IDF_Space_Group_Menu,ISPosSG)
!C>> JCC Set in the wizard too
      CALL WDialogSelect(IDD_PW_Page1)
!O      CALL WDialogPutMenu(IDF_PW_Crystal_System_Menu,CS_Options,NCS_Options,IOption)
      CALL WDialogPutOption(IDF_PW_Crystal_System_Menu,IOption)
      CALL WDialogPutMenu(IDF_PW_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
! JvdS @ Next line should be obsolete
!U      CALL WDialogPutOption(IDF_PW_Space_Group_Menu,ISPosSG)
      CALL PopActiveWindowID

      END SUBROUTINE Set_Space_Group
!
!*****************************************************************************
!
!C>> Sets the space group selected
      SUBROUTINE Update_Space_Group(IUploadFrom, TheLatticeSystem, ISgnum)

      USE WINTERACTER
      USE DRUID_HEADER

      INTEGER TheLatticeSystem

      INCLUDE 'statlog.inc'
      INCLUDE 'Lattice.inc'
      INTEGER IUploadFrom


      CALL PushActiveWindowID
      IF (IUploadFrom .EQ. IDD_Crystal_Symmetry) THEN
        CALL WDialogSelect(IDD_Crystal_Symmetry)
        CALL WDialogGetMenu(IDF_Crystal_System_Menu,IOption)
        CALL WDialogGetMenu(IDF_Space_Group_Menu,ISPosSG)
      ELSE IF (IUploadFrom .EQ. IDD_PW_Page1) THEN
        CALL WDialogSelect(IDD_PW_Page1)
        CALL WDialogGetMenu(IDF_PW_Crystal_System_Menu,IOption)
        CALL WDialogGetMenu(IDF_PW_Space_Group_Menu,ISPosSG)
      ELSE
        IOption = TheLatticeSystem
        ISPosSG = ISgnum - LPosSG(IOption) + 1
      END IF
!.. Trap 0 for unknown and make it P 1
      NumBrSG = MAX(1,(LPosSG(IOption+1)-LPosSG(IOption)))
      DO ISG = 1, NumBrSG
       JSG = LPosSG(IOption) + ISG - 1
       SGHMaBrStr(ISG)(1:12)  = SGNumStr(JSG)(1:12)
       SGHMaBrStr(ISG)(13:24) = SGHMaStr(JSG)(1:12)
      END DO
      IPosSG = LPosSG(IOption) + ISPosSg - 1
      NumberSGTable = IPosSG
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogPutOption(IDF_Crystal_System_Menu,IOption)
      CALL WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
! Set in the wizard too
      CALL WDialogSelect(IDD_PW_Page1)
      CALL WDialogPutOption(IDF_PW_Crystal_System_Menu,IOption)
      CALL WDialogPutMenu(IDF_PW_Space_Group_Menu,SGHMaBrStr,NumBrSG,ISPosSG)
! Finally set the number of pawley refinements to zero
      NumPawleyRef = 0
      CALL PopActiveWindowID

      END SUBROUTINE Update_Space_Group
!
!*****************************************************************************
!
!C>> JCC Added this in as a single function
      SUBROUTINE  Check_Lattice_Type
! Although called 'Check_Lattice_Type, it sets the lattice type really
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'
      LOGICAL ABC_Same,AB_Same,AC_Same,BC_Same,Ang_Same,Alp_90,Bet_90,Gam_90,Gam_120

!C>> JCC save static local copies of the previous cell. Only update things if the cell changes on call to
!C>> this routine

      REAL LastCellPar(6)
      DATA LastCellPar / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /
      SAVE LastCellPar
      INTEGER I
      LOGICAL FnUnitCellOK ! Function
      REAL, PARAMETER :: SmallVal = 1.0E-6
! Check if cell parameters are available and make sense
      IF (.NOT. FnUnitCellOK()) RETURN
! C>> JCC run check on last Cell
      IF (   ABS( CellPar(1) - LastCellPar(1) ) .LE. SmallVal  &
       .AND. ABS( CellPar(2) - LastCellPar(2) ) .LE. SmallVal  &   
       .AND. ABS( CellPar(3) - LastCellPar(3) ) .LE. SmallVal  &
       .AND. ABS( CellPar(4) - LastCellPar(4) ) .LE. SmallVal  &
       .AND. ABS( CellPar(5) - LastCellPar(5) ) .LE. SmallVal  &
       .AND. ABS( CellPar(6) - LastCellPar(6) ) .LE. SmallVal ) RETURN
      DO I = 1, 6
        LastCellPar(I) = CellPar(I)
      END DO      
      AB_Same  = ABS(CellPar(2)-CellPar(1)) .LE. SmallVal 
      BC_Same  = ABS(CellPar(3)-CellPar(2)) .LE. SmallVal 
      AC_Same  = ABS(CellPar(3)-CellPar(1)) .LE. SmallVal 
      ABC_Same = AB_Same .AND. BC_Same 
      Alp_90   = (ABS(CellPar(4)-90.0) .LE. SmallVal)
      Bet_90   = (ABS(CellPar(5)-90.0) .LE. SmallVal)
      Gam_90   = (ABS(CellPar(6)-90.0) .LE. SmallVal)
      Gam_120  = (ABS(CellPar(6)-120.0) .LE. SmallVal)
      Ang_Same = ABS(CellPar(6)-CellPar(5)) .LE. SmallVal &
           .AND. ABS(CellPar(5)-CellPar(4)) .LE. SmallVal
!
! Unk=1, Tri=2,Mon_a=3,Mon_b=4,Mon_c=5,Ort=6,Tet=7,Tri=8,Rho=9,Hex=10,Cub=11
! JvdS @ Note that this routine can never return Tri=8

      IF (ABC_Same .AND. Ang_Same) THEN
        IF (Alp_90) THEN
          LatBrav = 11 ! Cubic
          RETURN
        ELSE
          LatBrav = 9 ! Rhombohedral
          RETURN
        END IF
      END IF
      IF (AB_Same) THEN
        IF (Ang_Same .AND. Alp_90) THEN
          LatBrav = 7 ! Tetragonal
          RETURN
        ELSE IF (Alp_90 .AND. Bet_90 .AND. Gam_120) THEN
          LatBrav = 10 ! Hexagonal
          RETURN
        END IF
      END IF
      IF (Ang_Same .AND. Alp_90) THEN
        LatBrav = 6 ! Orthorhombic
        RETURN
      END IF
      IF      (      Alp_90 .AND.       Bet_90 .AND. .NOT. Gam_90) THEN
        LatBrav = 5 ! Monoclinic c
        RETURN
      ELSE IF (      Alp_90 .AND. .NOT. Bet_90 .AND.       Gam_90) THEN
        LatBrav = 4 ! Monoclinic b
        RETURN
      ELSE IF (.NOT. Alp_90 .AND.       Bet_90 .AND.       Gam_90) THEN
        LatBrav = 3 ! Monoclinic a
        RETURN
      END IF
      LatBrav = 2 ! Triclinic
      RETURN

      END SUBROUTINE Check_Lattice_Type
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
! Should be renamed to 'SetWavelength'

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
!C>> JCC Not used anymore
!C>> JCC Sets the popup to contain the defaults for the selected lattice 
!      subroutine Set_Main_Crystal_Symmetry()
!
!
!      use Winteracter
!      use druid_header
!
!C>> JCC Lattice declarations now in an include file 
!       INCLUDE 'Lattice.inc'
!
!      Call WDialogSelect(IDD_Crystal_Symmetry)
!C>> JCC Not needed any more
!Was
! 50  Listbrav=Latbrav+1
! now
! 50   continue
!C>> JCC Was
!      Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,ListBrav)
! Now
!       Call WDialogPutMenu(IDF_Crystal_System_Menu,CS_Options,NCS_Options,LatBrav)
!      Call WDialogGetMenu(IDF_Crystal_System_Menu,IOption)
!      NumBrSG=max(1,(LPosSG(IOption+1)-LPosSG(IOption)))
!      Do ISG=1,NumBrSG
!         JSG=LPosSG(IOption)+ISG-1
!         SGHMaBrStr(ISG)(1:12) =SGNumStr(JSG)(1:12)
!         SGHMaBrStr(ISG)(13:24)=SGHMaStr(JSG)(1:12)
!      End Do
!      ISG=1
!      IPosSG=LPosSG(IOption)
!      Call WDialogPutMenu(IDF_Space_Group_Menu,SGHMaBrStr,NumSG,ISG)
!
!      endsubroutine Set_Main_Crystal_Symmetry
!
!*****************************************************************************
!
!C>> ID Lattice from space group number
      INTEGER FUNCTION LatticeNumber(ISpaceGroup,IDashSg)

      INCLUDE 'Lattice.inc'
      INTEGER,PARAMETER ::  TricLim =   2
      INTEGER,PARAMETER ::  MonoLim =  15
      INTEGER,PARAMETER ::  OrthLim =  74
      INTEGER,PARAMETER ::  TetrLim = 142
      INTEGER,PARAMETER ::  TrigLim = 167
      INTEGER,PARAMETER ::  HexaLim = 194

      IF      (ISpaceGroup .LE. TricLim) THEN
        LatticeNumber = 2
      ELSE IF (ISpaceGroup .LE. MonoLim) THEN
! More work required: a, b or c?
        LatticeNumber = 3
        DO I = LEN_TRIM(SGNumStr(IDashSg)),1,-1
          SELECT CASE(SGNumStr(IDashSg)(I:I)) 
            CASE ('a')
              LatticeNumber = 3
            CASE ('b')
              LatticeNumber = 4
            CASE ('c')
              LatticeNumber = 5
            CASE (':')
              GOTO 1
          END SELECT
        END DO
   1    CONTINUE
      ELSE IF (ISpaceGroup .LE. OrthLim) THEN
        LatticeNumber = 6
      ELSE IF (ISpaceGroup .LE. TetrLim) THEN
        LatticeNumber = 7
      ELSE IF (ISpaceGroup .LE. TrigLim) THEN
! More work required - trigonal or rhombohedral
        LatticeNumber = 8
        DO I = LEN_TRIM(SGNumStr(IDashSg)),1,-1
          SELECT CASE(SGNumStr(IDashSg)(I:I)) 
            CASE ('H')
              LatticeNumber = 8
            CASE ('R')
              LatticeNumber = 9
            CASE (':')
              GOTO 2
          END SELECT
        END DO
   2    CONTINUE
      ELSE IF (ISpaceGroup .LE. HexaLim) THEN
        LatticeNumber = 10
      ENDIF
! JvdS @ How can this function ever assign 11 ( = Cubic) to a 
! 'kristalstelsel'? (The variable LatBrav, probably short for 'Lattice Bravais'
! is used throughout DASH, but there are 14 Bravais lattices, so that's not
! what's kept in the variable).

      RETURN

      END FUNCTION LatticeNumber
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
