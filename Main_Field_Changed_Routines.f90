!
!*****************************************************************************
!
      SUBROUTINE DealWithPlotOptionsWindow

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'Poly_Colours.inc'
                
      TYPE(WIN_RGB) :: SelectedColour

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Plot_Option_Dialog)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
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
          CALL Profile_Plot
        CASE (FieldChanged)
          IF (EventInfo%VALUE1 .EQ. EventInfo%VALUE2) THEN
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDF_ErrorBar_Check)
                CALL Profile_Plot
              CASE (IDF_Background_Check)
                CALL Profile_Plot
              CASE (IDF_ConnectObsPoints)
                CALL Profile_Plot
              CASE (IDF_PlotPeakFitDif)
                CALL Profile_Plot
            END SELECT
          ENDIF
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithPlotOptionsWindow
!
!*****************************************************************************
!
      SUBROUTINE DealWithConfiguration

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER IFLAGS, IFTYPE
      CHARACTER*MaxPathLength tFileName
      CHARACTER*75  FILTER

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Configuration)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCLOSE, IDCANCEL)
              CALL WDialogHide()
            CASE (IDBBROWSE)
              IFLAGS = LoadDialog + PromptOn
              FILTER = 'All files (*.*)|*.*|'//&
                       'All executables (*.exe)|*.exe|'
! IFTYPE specifies which of the file types in the list is the default
              IFTYPE = 2
              tFileName = ViewExe
              CALL WSelectFile(FILTER,IFLAGS,tFileName,'Select Viewer',IFTYPE)
! Did the user press cancel?
              IF (WInfoDialog(ExitButtonCommon) .EQ. CommonOK) THEN
                VIEWEXE = tFileName
                CALL WDialogPutString(IDF_ViewExe,VIEWEXE)
              ENDIF
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithConfiguration
!
!*****************************************************************************
!
      SUBROUTINE DealWithStructuralInformation
! This is the window containing the four tabs from the 'View' menu

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'lattice.inc'

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Structural_Information)
      IF (PastPawley) THEN ! Don't do anything
        SELECT CASE (EventType)
          CASE (PushButton) ! one of the buttons was pushed
            SELECT CASE (EventInfo%VALUE1)
              CASE (IDOK, IDCANCEL)
                CALL WDialogHide()
            END SELECT
        END SELECT
      ELSE
        SELECT CASE (EventType)
          CASE (PushButton) ! one of the buttons was pushed
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
            END SELECT
            CALL Profile_Plot
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
              CASE (IDD_Peak_Widths)
            END SELECT
        END SELECT
      ENDIF
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

      IF (PastPawley) RETURN
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Data_Properties)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDAPPLY) ! The 'Apply' button
              CALL DownloadWavelength(IDD_Data_Properties)    
              CALL Generate_TicMarks
          END SELECT
          CALL Profile_Plot
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
            END SELECT
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

      IF (PastPawley) RETURN
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Peak_Positions)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
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
          END SELECT
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

      INTEGER SGNrMenu2Table ! Function
      REAL    tReal
      LOGICAL ValidCellAxisLength, NearlyEqual ! Functions
      INTEGER ISPosSG

      IF (PastPawley) RETURN
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDAPPLY) ! The 'Apply' button
              CALL WDialogGetReal(IDF_ZeroPoint,ZeroPoint)
              CALL Upload_ZeroPoint
              CALL Download_Cell_Constants(IDD_Crystal_Symmetry)
              CALL Generate_TicMarks
          END SELECT
          CALL Profile_Plot
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
                CALL Generate_TicMarks
              ENDIF
            CASE (IDF_ZeroPoint)
              CALL WDialogGetReal(IDF_ZeroPoint,ZeroPoint)
              CALL Upload_ZeroPoint               
              CALL Generate_TicMarks
          END SELECT
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

      REAL    Temp

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Index_Preparation)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDCANCEL)
              CALL WDialogHide()
            CASE (ID_Indexing_Create)
              CALL WDialogGetReal(IDF_wavelength1,Temp)
              IF (Temp .LT. 0.00001) THEN
                CALL ErrorMessage("The radiation wavelength has not been entered!")
              ELSE                 
                CALL Create_DicvolIndexFile()
                CALL WDialogSelect(IDD_Index_Preparation)
                CALL WDialogHide()
              END IF
            CASE (IDF_RunDICVOL)
              CALL RunDICVOL
          END SELECT
          CALL Profile_Plot
        CASE (FieldChanged)
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDF_wavelength1)
              CALL DownloadWavelength(IDD_Index_Preparation)
              CALL Generate_TicMarks   
          END SELECT
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
            CASE (IDCANCEL, IDCLOSE)
              CALL WDialogHide()
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
      ENDDO               
      CALL PopActiveWindowID

      END SUBROUTINE DealWithDVResults
!U!
!U!*****************************************************************************
!U!
!U      DOUBLE PRECISION FUNCTION F(X)
!U
!U      IMPLICIT NONE
!U
!U      DOUBLE PRECISION, INTENT (IN   ) :: X
!U
!U      INCLUDE 'GLBVAR.INC'
!U
!U      DOUBLE PRECISION TwoTheta1, TwoTheta2
!U      INTEGER                               I, J
!U      COMMON /Newton/  TwoTheta1, TwoTheta2, I, J
!U
!U      DOUBLE PRECISION Part1, Part2, a
!U      REAL, EXTERNAL :: Degrees2Radians, TwoTheta2dSpacing
!U
!U      a = Degrees2Radians((TwoTheta1 - x) / 2)
!U      Part1 = DBLE(i)*(ALambda / (2*DSin(a)))
!U      IF (ABS(Part1 - (FLOAT(i)*TwoTheta2dSpacing(TwoTheta1 - x))) .GT. 0.001 ) CALL DebugErrorMessage('Mistake 1')
!U      a = Degrees2Radians((TwoTheta2 - x) / 2)
!U      Part2 = DBLE(j)*(ALambda / (2*DSin(a)))
!U      IF (DABS(Part2 - (DBLE(j)*TwoTheta2dSpacing(TwoTheta2 - x))) .GT. 0.001 ) CALL DebugErrorMessage('Mistake 2')
!U      F = Part1 - Part2
!U
!U      END FUNCTION F
!U!
!U!*****************************************************************************
!U!
!U      DOUBLE PRECISION FUNCTION dfdx(X)
!U
!U      IMPLICIT NONE
!U
!U      DOUBLE PRECISION, INTENT (IN   ) :: X
!U
!U      INCLUDE 'GLBVAR.INC'
!U
!U      DOUBLE PRECISION TwoTheta1, TwoTheta2
!U      INTEGER                               I, J
!U      COMMON /Newton/  TwoTheta1, TwoTheta2, I, J
!U
!U      DOUBLE PRECISION Part1, Part2, a
!U      REAL, EXTERNAL :: Degrees2Radians
!U
!U      a = Degrees2Radians((TwoTheta1 - x) / 2)
!U      Part1 = ((-DBLE(i)*ALambda) / 4.0) * (DCos(a) / (DSin(a)**2))
!U      a = Degrees2Radians((TwoTheta2 - x) / 2)
!U      Part2 = ((-DBLE(j)*ALambda) / 4.0) * (DCos(a) / (DSin(a)**2))
!U      dfdx = Part1 - Part2
!U
!U      END FUNCTION dfdx
!U!
!U!*****************************************************************************
!U!
!U      SUBROUTINE QuickNewton(x) ! just for calculating the zeropoint
!U
!U      IMPLICIT NONE 
!U
!U      DOUBLE PRECISION, INTENT (  OUT) :: x
!U
!U      DOUBLE PRECISION Xold, Shift
!U      DOUBLE PRECISION, EXTERNAL :: f, dfdx
!U
!U      x = 0.0
!U      Xold = x + 1.0
!U      DO WHILE (DABS(Xold-x) .GT. 0.00001) ! Convergence
!U        IF (DABS(dfdx(x)) .LT. 0.00001) THEN
!U          x = 1.1
!U          RETURN
!U        ENDIF
!U! Calculate the shift
!U        Shift = -f(x) / (dfdx(x)*100.0)
!U! The zero point should be very small, less than 1.0
!U! Our initial estimate is always zero, so if the shift is more than 1.0, just stop
!U        IF (DABS(Shift) .GT. 1.0) THEN
!U          x = 1.1
!U          RETURN
!U        ENDIF
!U        Xold = x
!U        x = x + shift      
!U      ENDDO
!U
!U      END SUBROUTINE QuickNewton
!U!
!U!*****************************************************************************
!U!
!U      SUBROUTINE EstimateZeroPointError
!U!
!U! Experiment: can we estimate the zero point assuming that several
!U! of the peak positions are the higher order of an other peak position?
!U
!U      USE WINTERACTER
!U      USE DRUID_HEADER
!U      USE VARIABLES
!U      USE DICVAR
!U
!U      IMPLICIT NONE
!U
!U      INCLUDE 'PARAMS.INC'
!U
!U      INTEGER           NTPeak
!U      REAL              AllPkPosVal,         AllPkPosEsd
!U      REAL              PkProb
!U      INTEGER           IOrdTem
!U      INTEGER           IHPk
!U      COMMON /ALLPEAKS/ NTPeak,                                                  &
!U                        AllPkPosVal(MTPeak), AllPkPosEsd(MTPeak),                &
!U                        PkProb(MTPeak),                                          &
!U                        IOrdTem(MTPeak),                                         &
!U                        IHPk(3,MTPeak)
!U
!U      REAL            TwoTheta1, TwoTheta2
!U      INTEGER                               I, J
!U      COMMON /Newton/ TwoTheta1, TwoTheta2, I, J
!U
!U      REAL    ZeroPointError(1:80000)
!U      INTEGER Peak1, Peak2, IOrd1, IOrd2, II
!U      DOUBLE PRECISION    zp
!U
!U      IF (NTPeak .LT. 2) THEN
!U        CALL DebugErrorMessage("Can't estimate zero point from less than 2 peaks.")
!U        RETURN
!U      ENDIF
!U      II = 0
!U! Peaks are ordered: d-spacing(N) > d-spacing(N+1)
!U      DO Peak1 = 1, NTPeak-1
!U        IOrd1 = IOrdTem(Peak1)
!U        TwoTheta1 = AllPkPosVal(IOrd1)
!U        DO Peak2 = Peak1+1, NTPeak
!U          IOrd2 = IOrdTem(Peak2)
!U          TwoTheta2 = AllPkPosVal(IOrd2)
!U          DO I = 1, 9
!U            DO J = I+1, 10
!U! Now calculate zeropoint
!U              CALL QuickNewton(zp)
!U              IF (DABS(zp) .LT. 1.0) THEN
!U                II = II + 1
!U                IF (II .GT. 80000) THEN
!U                  CALL DebugErrorMessage('ZeroPointError-array out of bounds')
!U                ELSE
!U                  ZeroPointError(II) = zp
!U                ENDIF
!U              ENDIF
!U            ENDDO
!U          ENDDO
!U        ENDDO
!U      ENDDO
!U      OPEN(10,FILE='ZeroPoint.txt',ERR=999)
!U      DO Peak1 = 1, II
!U        WRITE(10,'(F8.6)') ZeroPointError(Peak1)
!U      ENDDO
!U      CLOSE(10)
!U      RETURN
!U  999 CALL ErrorMessage('Could not open zero point error file.')
!U
!U      END SUBROUTINE EstimateZeroPointError
!U!
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

      REAL    Rvpar(2), Lambda, Rdens, Rmolwt, Rexpzp
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
      CALL WDialogGetReal(IDF_wavelength1, Lambda)
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
      CALL WDialogGetCheckBox(IDF_Indexing_Cubic,Isystem(1))
      CALL WDialogGetCheckBox(IDF_Indexing_Tetra,Isystem(2))
      CALL WDialogGetCheckBox(IDF_Indexing_Hexa,Isystem(3))
      CALL WDialogGetCheckBox(IDF_Indexing_Ortho,Isystem(4))
      CALL WDialogGetCheckBox(IDF_Indexing_Monoclinic,Isystem(5))
      CALL WDialogGetCheckBox(IDF_Indexing_Triclinic,Isystem(6))
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
        'do you wish to continue anyway?')) GOTO 999
      ENDIF
      IF (DV_ScaleFactor .LT. 0.1) DV_ScaleFactor = 0.1
      IF (DV_ScaleFactor .GT. 5.0) DV_ScaleFactor = 5.0
! Check if the maximum angle has a reasonable value. Only necessary when monoclinic is searched
      IF (Isystem(5) .EQ. 1) THEN
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
      MaxLen = MAX(MaxSinBeta*amax,Bmax)
      MaxLen = MAX(MaxLen,MaxSinBeta*Cmax)
! Lowest 2 theta value for which a peak has been fitted: AllPkPosVal(IOrdTem(1))
      IF ((TwoTheta2dSpacing(AllPkPosVal(IOrdTem(1)))*DV_ScaleFactor) .GT. MaxLen) THEN
        IF (.NOT. Confirm('WARNING: the maximum cell axis length is shorter than required for indexing the first peak.'//CHAR(13)// &
        'Do you wish to continue anyway?')) GOTO 999
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
      ENDDO
      CALL WCursorShape(CurHourGlass)
      NumOfDICVOLSolutions = 0
      CALL DICVOL91(Isystem(1),Isystem(2),Isystem(3),Isystem(4),Isystem(5),Isystem(6),Rvpar(1),Rvpar(2),Rmolwt,Rdens,Rdens/50.0)
      CALL WCursorShape(CurCrossHair)
! Pop up a window showing the DICVOL output file in a text editor
      CALL WindowOpenChild(IHANDLE)
      CALL WEditFile('DICVOL.OUT',Modeless,0,0,4)
      CALL SetChildWinAutoClose(IHANDLE)
      IF (NumOfDICVOLSolutions .EQ. 0) THEN
        CALL WDialogSelect(IDD_DV_Results)
        CALL WDialogHide()
        CALL ErrorMessage('No solutions were found.')
        GOTO 999
      ENDIF
      IF (DICVOL_Error .EQ. cDICVOL_TooManySolutions) CALL WarningMessage('More than 30 solutions found, please check your data.')
! Pop up a window showing the solutions, so that the user can choose one to be imported into DASH
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
      ENDDO
      CALL WDialogShow(-1,-1,0,Modeless)
  999 CALL PopActiveWindowID

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
      DICVOLSolutions(NumOfDICVOLSolutions)%Volume = TheVolume / (DV_ScaleFactor**3)
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
