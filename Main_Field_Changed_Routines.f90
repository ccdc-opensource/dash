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
      INCLUDE 'lattice.inc'
      INCLUDE 'statlog.inc'

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Structural_Information)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDOK) ! The 'OK' button
              CALL Download_Cell_Constants(IDD_Crystal_Symmetry)
              CALL WDialogSelect(IDD_Crystal_Symmetry)
              CALL WDialogGetReal(IDF_ZeroPoint,ZeroPoint)
              CALL Upload_ZeroPoint               
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
              CALL Download_SpaceGroup(IDD_Crystal_Symmetry)
              NumPawleyRef = 0
            CASE (IDD_Peak_Widths)
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithStructuralInformation 2')
          END SELECT
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithStructuralInformation')
      END SELECT
      CALL PopActiveWindowID

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
                CALL Upload_Source
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

      END SUBROUTINE DealWithDiffractionSetupPane
!
!*****************************************************************************
!
      SUBROUTINE DealWithPeakPositionsPane

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Peak_Positions)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
! Which button was pressed is now in EventInfo%VALUE1
          SELECT CASE (EventInfo%VALUE1)
            CASE (ID_Index_Output)
! Set the wavelength
              CALL DownLoadWavelength(IDD_Data_Properties)
              CALL WDialogSelect(IDD_Index_Preparation)
! If this is synchrotron data, then set the default error in the peak positions to 0.02 rahter than 0.03.
! This decreases the number of solutions and increases the speed of the search.
              IF (JRadOption .EQ. 2) THEN
                CALL WDialogPutReal(IDF_eps,0.02,'(F5.3)')
              ELSE
                CALL WDialogPutReal(IDF_eps,0.03,'(F5.3)')
              ENDIF
              CALL WDialogShow(-1,-1,0,Modeless)
            CASE DEFAULT
              CALL DebugErrorMessage('Forgot to handle something in DealWithPeakPositionsPane 1')
          END SELECT
        CASE (FieldChanged)
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithPeakPositionsPane')
      END SELECT
      CALL PopActiveWindowID

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
              CALL Upload_ZeroPoint
              CALL Download_Cell_Constants(IDD_Crystal_Symmetry)
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
                CALL Upload_CrystalSystem
                CALL Generate_TicMarks
              ENDIF
            CASE (IDF_Space_Group_Menu)  
              IF (EventInfo%VALUE1 .EQ. EventInfo%VALUE2) THEN
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
              CALL Upload_ZeroPoint               
!            CASE DEFAULT
!              CALL DebugErrorMessage('Forgot to handle something in DealWithCrystalSymmetryPane 2')
          END SELECT
        CASE DEFAULT
          CALL DebugErrorMessage('Forgot to handle event in DealWithCrystalSymmetryPane')
      END SELECT
      CALL PopActiveWindowID

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
            CASE (IDF_RunDICVOL)
              CALL RunDICVOL
            CASE DEFAULT
!              CALL DebugErrorMessage('Forgot to handle something in DealWithIndexPreparation 1')
          END SELECT
          CALL Profile_Plot(IPTYPE)
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_Indexing_Lambda)
              CALL DownloadWavelength(IDD_Index_Preparation)
              CALL Generate_TicMarks   
            CASE (IDD_Index_Preparation)
!              CALL DebugErrorMessage('Something unexpected happened in DealWithIndexPreparation')
            CASE DEFAULT
!              CALL DebugErrorMessage('Forgot to handle FieldChanged in DealWithIndexPreparation')
          END SELECT
        CASE DEFAULT
!          CALL DebugErrorMessage('Forgot to handle event in DealWithCrystalSymmetryPane')
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithIndexPreparation
!
!*****************************************************************************
!
      SUBROUTINE DealWithDVResults

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'lattice.inc'

      INTEGER irow, istatus

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_DV_Results)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCLOSE)
              CALL WDialogHide()
! Unload the dialogue from memory
              CALL WDialogUnload
              CALL PopActiveWindowID
              RETURN
          END SELECT
      END SELECT
      DO irow = 1, NumOfDICVOLSolutions
        CALL WGridGetCellCheckBox(IDF_DV_Summary_0,1,irow,istatus)
        IF (istatus .EQ. 1) THEN
! Import the unit cell parameters into DASH
          CellPar(1) = DICVOLSolutions(irow)%a
          CellPar(2) = DICVOLSolutions(irow)%b
          CellPar(3) = DICVOLSolutions(irow)%c
          CellPar(4) = DICVOLSolutions(irow)%alpha
          CellPar(5) = DICVOLSolutions(irow)%beta
          CellPar(6) = DICVOLSolutions(irow)%gamma
          LatBrav = DICVOLSolutions(irow)%CrystalSystem
          CALL WGridPutCellCheckBox(IDF_DV_Summary_0,1,irow,0)
          CALL Upload_CrystalSystem
          CALL UpdateCell()
          CALL PopActiveWindowID
          RETURN
        ENDIF
      END DO               
      CALL PopActiveWindowID

      END SUBROUTINE DealWithDVResults
!
!*****************************************************************************
!
      SUBROUTINE RunDICVOL

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE DICVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'
      INCLUDE 'lattice.inc'

      INTEGER           NTPeak
      REAL              AllPkPosVal,         AllPkPosEsd
      REAL              PkProb
      INTEGER           IOrdTem
      INTEGER           IHPk
      COMMON /ALLPEAKS/ NTPeak,                                                  &
                        AllPkPosVal(MTPeak), AllPkPosEsd(MTPeak),                &
                        PkProb(MTPeak),                                          &
                        IOrdTem(MTPeak),                                         &
                        IHPk(3,MTPeak)

      REAL Rvpar(2), Lambda, Rdens, Rmolwt, Rexpzp
      INTEGER Isystem(6), UseErr, I, Iord
      INTEGER IHANDLE
      REAL    Epsti
      REAL    Epsilon
      REAL    MaxLen
      LOGICAL Confirm ! Function
      REAL    TwoTheta2dSpacing ! Function
      REAL    MaxSinBeta
      REAL    tBeta
      INTEGER NumDoF

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Index_Preparation)
      CALL WDialogGetReal(IDF_Indexing_Lambda, Lambda)
      CALL WDialogGetReal(IDF_Indexing_MinVol, Rvpar(1))
      CALL WDialogGetReal(IDF_Indexing_MaxVol, Rvpar(2))
      CALL WDialogGetReal(IDF_Indexing_Maxa, amax)
      CALL WDialogGetReal(IDF_Indexing_Maxb, Bmax)
      CALL WDialogGetReal(IDF_Indexing_Maxc, Cmax)
      CALL WDialogGetReal       (IDF_Indexing_MinAng,      Bemin)
      CALL WDialogGetReal       (IDF_Indexing_MaxAng,      Bemax)
      IF (Bemin .GT. Bemax) THEN
        tBeta = Bemin
        Bemin = Bemax
        Bemax = tBeta
      ENDIF
      CALL WDialogGetReal       (IDF_Indexing_Density,     Rdens)
      CALL WDialogGetReal       (IDF_Indexing_MolWt,       Rmolwt)
      CALL WDialogGetReal       (IDF_ZeroPoint,            Rexpzp)
      CALL WDialogGetCheckBox   (IDF_Indexing_Cubic,       Isystem(1))
      CALL WDialogGetCheckBox   (IDF_Indexing_Tetra,       Isystem(2))
      CALL WDialogGetCheckBox   (IDF_Indexing_Hexa,        Isystem(3))
      CALL WDialogGetCheckBox   (IDF_Indexing_Ortho,       Isystem(4))
      CALL WDialogGetCheckBox   (IDF_Indexing_Monoclinic,  Isystem(5))
      CALL WDialogGetCheckBox   (IDF_Indexing_Triclinic,   Isystem(6))
      CALL WDialogGetRadioButton(IDF_Indexing_UseErrors,   UseErr)
      CALL WDialogGetReal       (IDF_eps,                  Epsilon)
      CALL WDialogGetReal       (IDF_Indexing_Fom,         fom)
      CALL WDialogGetReal       (IDF_Indexing_ScaleFactor, DV_ScaleFactor)
! Number of degrees of freedom, we don't even count the zero point
      NumDoF = 0
      IF (Isystem(1) .EQ. 1) NumDof = MAX(NumDoF,1)
      IF (Isystem(2) .EQ. 1) NumDof = MAX(NumDoF,2)
      IF (Isystem(3) .EQ. 1) NumDof = MAX(NumDoF,2)
      IF (Isystem(4) .EQ. 1) NumDof = MAX(NumDoF,3)
      IF (Isystem(5) .EQ. 1) NumDof = MAX(NumDoF,4)
      IF (Isystem(6) .EQ. 1) NumDof = MAX(NumDoF,6)
! Check if any crystal system checked at all
      IF (NumDoF .EQ. 0) THEN
        CALL ErrorMessage('Please check at least one crystal system.')
        RETURN
      ENDIF
! Check if the number of observed lines is consistent with the crystal systems
      IF (NTPeak .LT. NumDoF) THEN
        CALL ErrorMessage('The number of observed lines is less than the number of degrees of freedom.')
        RETURN
      ENDIF
! Warn the user if we have less observed lines than twice the number of degrees of freedom including the zero point
      IF ((2*(NumDoF+1)) .GT. NTPeak) THEN
        IF (.NOT. Confirm('The number of observed lines is less than twice the number of degrees of freedom,'//CHAR(13)//&
        'do you wish to continue anyway?')) RETURN
      ENDIF
      IF (DV_ScaleFactor .LT. 0.1) DV_ScaleFactor = 0.1
      IF (DV_ScaleFactor .GT. 5.0) DV_ScaleFactor = 5.0
! Check if the maximum angle has a reasonable value. Only necessary when monoclinic is searched
      IF (Isystem(5) .EQ. 1) THEN
        IF ((Bemin .LT. 45.0) .OR. (Bemax .GT. 150.0)) THEN
          CALL ErrorMessage('The range of the angle beta does not make sense.')
          RETURN
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
      MaxLen = MAX(MaxSinBeta*amax,Bmax)
      MaxLen = MAX(MaxLen,MaxSinBeta*Cmax)
! Lowest 2 theta value for which a peak has been fitted: AllPkPosVal(IOrdTem(1))
      IF ((TwoTheta2dSpacing(AllPkPosVal(IOrdTem(1)))*DV_ScaleFactor) .GT. MaxLen) THEN
        IF (.NOT. Confirm('WARNING: the maximum cell axis length is shorter than required for indexing the first peak.'//CHAR(13)// &
        'Do you wish to continue anyway?')) RETURN
      ENDIF
      n = NTPeak
      wave2 = (Lambda / 2) * DV_ScaleFactor
      IF (UseErr .EQ. 2) THEN
        epst = 0.0
        DO I = 1, n
          IOrd = IOrdTem(I)
          IF (AllPkPosEsd(IOrd) .LE. 0.0001) THEN
            epsil(I) = 0.001
          ELSE 
            epsil(I) = AllPkPosEsd(IOrd) * 10.0
          ENDIF
          Epsti = epsil(I) + 0.015
          IF (Epsti .GT. epst) epst = Epsti
        ENDDO
      ELSE
        epst = Epsilon + 0.015
        DO I = 1, n
          epsil(I) = Epsilon
        ENDDO
      ENDIF
      DO I = 1, NTPeak
        IOrd = IOrdTem(I)
        d(I) = AllPkPosVal(IOrd) - Rexpzp
      END DO
      CALL WCursorShape(CurHourGlass)
      NumOfDICVOLSolutions = 0
      CALL DICVOL91(Isystem(1),Isystem(2),Isystem(3),Isystem(4),Isystem(5),Isystem(6),Rvpar(1),Rvpar(2),Rmolwt,Rdens,Rdens/50.0)
      CALL WCursorShape(CurCrossHair)
! Pop up a window showing the DICVOL output file in a text editor
      CALL WindowOpenChild(IHANDLE)
      CALL WEditFile('DICVOL.OUT',Modeless,0,FileMustExist+ViewOnly+NoToolbar+NoFileNewOpen,4)
      CALL SetChildWinAutoClose(IHANDLE)
      IF (NumOfDICVOLSolutions .EQ. 0) THEN
        CALL ErrorMessage('No solutions were found.')
        CALL PopActiveWindowID
        RETURN
      ENDIF
! Pop up a window showing the solutions, so that the user can choose one to be imported into DASH
      CALL WDialogLoad(IDD_DV_Results)
      CALL WDialogSelect(IDD_DV_Results)
! Clear all fields in the grid
      CALL WDialogClearField(IDF_DV_Summary_0)
! Set the number of rows in the grid to the number of solutions.
      CALL WGridRows(IDF_DV_Summary_0,NumOfDICVOLSolutions)
      DO I = 1, NumOfDICVOLSolutions
        CALL WGridPutCellString(IDF_DV_Summary_0, 2,I,CrystalSystemString(DICVOLSolutions(I)%CrystalSystem))
        CALL WGridPutCellReal  (IDF_DV_Summary_0, 3,I,DICVOLSolutions(I)%a,'(F8.4)')
        CALL WGridPutCellReal  (IDF_DV_Summary_0, 4,I,DICVOLSolutions(I)%b,'(F8.4)')
        CALL WGridPutCellReal  (IDF_DV_Summary_0, 5,I,DICVOLSolutions(I)%c,'(F8.4)')
        CALL WGridPutCellReal  (IDF_DV_Summary_0, 6,I,DICVOLSolutions(I)%alpha,'(F7.3)')
        CALL WGridPutCellReal  (IDF_DV_Summary_0, 7,I,DICVOLSolutions(I)%beta,'(F7.3)')
        CALL WGridPutCellReal  (IDF_DV_Summary_0, 8,I,DICVOLSolutions(I)%gamma,'(F7.3)')
        CALL WGridPutCellReal  (IDF_DV_Summary_0, 9,I,DICVOLSolutions(I)%Volume,'(F9.2)')
        IF (DICVOLSolutions(I)%M .GT. 0.0) CALL WGridPutCellReal (IDF_DV_Summary_0,10,I,DICVOLSolutions(I)%M,'(F7.1)')
        IF (DICVOLSolutions(I)%F .GT. 0.0) CALL WGridPutCellReal (IDF_DV_Summary_0,11,I,DICVOLSolutions(I)%F,'(F7.1)')
      END DO
      CALL WDialogShow(-1,-1,0,Modeless)
      CALL PopActiveWindowID

      END SUBROUTINE RunDICVOL
!
!*****************************************************************************
!
      SUBROUTINE AddDICVOLSolution(TheCrystalSystem,The_a,The_b,The_c,The_alpha,The_beta,The_gamma,TheVolume)
!
! This routine adds a solution generated by DICVOL to an array in DASH
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE DICVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheCrystalSystem
      REAL,    INTENT (IN   ) :: The_a, The_b, The_c, The_alpha, The_beta, The_gamma, TheVolume

      NumOfDICVOLSolutions = NumOfDICVOLSolutions + 1
      IF (NumOfDICVOLSolutions .GT. MaxDICVOLSolutions) THEN
        CALL DebugErrorMessage('NumOfDICVOLSolutions > MaxDICVOLSolutions in AddDICVOLSolution.')
        NumOfDICVOLSolutions = MaxDICVOLSolutions
      ENDIF
      DICVOLSolutions(NumOfDICVOLSolutions)%CrystalSystem = TheCrystalSystem
      DICVOLSolutions(NumOfDICVOLSolutions)%a     = The_a / DV_ScaleFactor
      DICVOLSolutions(NumOfDICVOLSolutions)%b     = The_b / DV_ScaleFactor
      DICVOLSolutions(NumOfDICVOLSolutions)%c     = The_c / DV_ScaleFactor
      DICVOLSolutions(NumOfDICVOLSolutions)%alpha = The_alpha
      DICVOLSolutions(NumOfDICVOLSolutions)%beta  = The_beta
      DICVOLSolutions(NumOfDICVOLSolutions)%gamma = The_gamma
      DICVOLSolutions(NumOfDICVOLSolutions)%Volume = TheVolume
      DICVOLSolutions(NumOfDICVOLSolutions)%F = -1.0
      DICVOLSolutions(NumOfDICVOLSolutions)%M = -1.0
! F and M are calculated in a different part and added only subject to 
! certain conditions. Therefore, they are initialised to silly values and
! we can check later on if they have been calculated.

      END SUBROUTINE AddDICVOLSolution
!
!*****************************************************************************
!
      SUBROUTINE AddDICVOL_F(TheF)
!
! This routine adds a solution generated by DICVOL to an array in DASH
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      REAL,    INTENT (IN   ) :: TheF

      DICVOLSolutions(NumOfDICVOLSolutions)%F = TheF
! F and M are calculated in a different part and added only subject to 
! certain conditions. Therefore, they are initialised to silly values and
! we can check later on if they have been calculated.

      END SUBROUTINE AddDICVOL_F
!
!*****************************************************************************
!
      SUBROUTINE AddDICVOL_M(TheM)
!
! This routine adds a solution generated by DICVOL to an array in DASH
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      REAL,    INTENT (IN   ) :: TheM

      DICVOLSolutions(NumOfDICVOLSolutions)%M = TheM
! F and M are calculated in a different part and added only subject to 
! certain conditions. Therefore, they are initialised to silly values and
! we can check later on if they have been calculated.

      END SUBROUTINE AddDICVOL_M
!
!*****************************************************************************
!
