! This file contains routines that form the layer between DASH and Winteracter and
! mutually exchange variables.
!
!*****************************************************************************
!
      SUBROUTINE Upload_Zero_Point()

      USE WINTERACTER
      USE DRUID_HEADER

      INCLUDE 'Lattice.inc'

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_Peak_Positions)
      CALL WDialogPutReal(IDF_ZeroPoint,ZeroPoint,'(F10.4)')
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogPutReal(IDF_ZeroPoint,ZeroPoint,'(F10.4)')
      CALL WDialogSelect(IDD_Index_Preparation)
      CALL WDialogPutReal(IDF_ZeroPoint,ZeroPoint,'(F10.4)')
      CALL PopActiveWindowID
      END SUBROUTINE Upload_Zero_Point
!
!*****************************************************************************
!
      SUBROUTINE Upload_Cell_Constants()
!
! Puts the global variables CellPar(1:6) in the Winteracter menus
!
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      LOGICAL ValidCellAxisLength ! Function
      INTEGER WindowNr
      INTEGER CellParID(6)
      INTEGER I

      CellParID(1) = IDF_a_latt
      CellParID(2) = IDF_b_latt
      CellParID(3) = IDF_c_latt
      CellParID(4) = IDF_alp_latt
      CellParID(5) = IDF_bet_latt
      CellParID(6) = IDF_gam_latt
      CALL PushActiveWindowID
      DO WindowNr = 1, 3
        SELECT CASE (WindowNr)
          CASE (1) 
            CALL WDialogSelect(IDD_Crystal_Symmetry)
          CASE (2) 
            CALL WDialogSelect(IDD_PW_Page1)
          CASE (3) 
            CALL WDialogSelect(IDD_Peak_Positions)
        END SELECT
! Update all the unit cell lengths ...
        DO I = 1, 3
          IF (ValidCellAxisLength(CellPar(I))) THEN
            CALL WDialogPutReal(CellParID(I),CellPar(I),'(F10.5)')
          ELSE
            CALL WDialogClearField(CellParID(I))
          ENDIF
          IF (CellParConstrained(I) .OR. (WindowNr .EQ. 3)) THEN
            CALL WDialogFieldState(CellParID(I),Disabled)
          ELSE
            CALL WDialogFieldState(CellParID(I),Enabled)
          ENDIF
        ENDDO
! ... and the unit cell angles
        DO I = 4, 6
          IF (CellPar(I) .LT. 0.00001) THEN
            CALL WDialogClearField(CellParID(I))
          ELSE
            CALL WDialogPutReal(CellParID(I),CellPar(I),'(F10.3)')
          ENDIF
          IF (CellParConstrained(I) .OR. (WindowNr .EQ. 3)) THEN
            CALL WDialogFieldState(CellParID(I),Disabled)
          ELSE
            CALL WDialogFieldState(CellParID(I),Enabled)
          ENDIF
        ENDDO
      ENDDO
      CALL PopActiveWindowID

      END SUBROUTINE Upload_Cell_Constants
!
!*****************************************************************************
!
      SUBROUTINE Download_Cell_Constants(IDownFrom)

      USE WINTERACTER
      USE DRUID_HEADER

      INTEGER, INTENT (IN) :: IDownFrom

      INCLUDE 'Lattice.inc'

      CALL PushActiveWindowID
! Get all the cell constants from the selected area
      CALL WDialogSelect(IDownFrom)
      CALL WDialogGetReal(IDF_a_latt,CellPar(1))
      CALL WDialogGetReal(IDF_b_latt,CellPar(2))      
      CALL WDialogGetReal(IDF_c_latt,CellPar(3))      
      CALL WDialogGetReal(IDF_alp_latt,CellPar(4))      
      CALL WDialogGetReal(IDF_bet_latt,CellPar(5))      
      CALL WDialogGetReal(IDF_gam_latt,CellPar(6))
      CALL PopActiveWindowID

      END SUBROUTINE Download_Cell_Constants
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
      SUBROUTINE UpdateWavelength(TheWaveLength)
! Should be renamed to 'SetWavelength'/'UploadWavelength'

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      REAL, INTENT (IN   ) :: TheWaveLength

      INCLUDE 'GLBVAR.INC'

      REAL             XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX

      COMMON /PROFRAN/ XPMIN,     XPMAX,     YPMIN,     YPMAX,       &
                       XPGMIN,    XPGMAX,    YPGMIN,    YPGMAX,      &
                       XPGMINOLD, XPGMAXOLD, YPGMINOLD, YPGMAXOLD,   &
                       XGGMIN,    XGGMAX,    YGGMIN,    YGGMAX

      REAL    tMaxResolution
      INTEGER I, IRadSelection
      REAL    FnWavelengthOfMenuOption ! Function
      REAL    TwoTheta2dSpacing, dSpacing2TwoTheta ! Function

      IF ((TheWaveLength .LT. 0.01) .OR. (TheWaveLength .GT. 20.0)) THEN
        CALL ErrorMessage('Invalid wavelength')
        RETURN
      ENDIF
      IF (TheWaveLength .EQ. ALambda) RETURN
      ALambda = TheWaveLength
      CALL PushActiveWindowID
! This is the right place to update the maximum resolution (even if it's not necessary)
! In principle, set resolution so as to truncate after DefaultMaxResolution.
! However, if truncation resolution not attainable with current data range / wavelength,
! adjust the setting of the maximum resolution to maximum possible.
      CALL WDialogSelect(IDD_PW_Page5)
      IF (NoData) THEN
        tMaxResolution = DefaultMaxResolution
      ELSE
        tMaxResolution = MAX(TwoTheta2dSpacing(XPMAX),DefaultMaxResolution)
      ENDIF
      CALL WDialogPutReal(IDF_MaxResolution,tMaxResolution)
      CALL WDialogPutReal(IDF_Max2Theta,dSpacing2TwoTheta(tMaxResolution))

      CALL WDialogSelect(IDD_Data_Properties)
      CALL WDialogPutReal(IDF_wavelength1,ALambda,'(F10.5)')
      CALL WDialogSelect(IDD_PW_Page2)
      CALL WDialogPutReal(IDF_PW_wavelength1,ALambda,'(F10.5)')
      CALL WDialogSelect(IDD_PW_Page4)
      CALL WDialogPutReal(IDF_PW_wavelength1,ALambda,'(F10.5)')
      CALL WDialogSelect(IDD_Index_Preparation)
      CALL WDialogPutReal(IDF_Indexing_Lambda,ALambda,'(F10.5)')
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
      SUBROUTINE Upload_Positions()

      USE WINTERACTER
      USE DRUID_HEADER 

      INCLUDE 'PARAMS.INC'
      INCLUDE 'lattice.inc'

      INTEGER CurrentRange 
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR), &
        IPF_Lo(MAX_NPFR),IPF_Hi(MAX_NPFR),NumPeakFitRange, &
        CurrentRange,IPF_Range(MAX_NPFR),NumInPFR(MAX_NPFR), & 
        XPF_Pos(MAX_NPPR,MAX_NPFR),YPF_Pos(MAX_NPPR,MAX_NPFR), &
        IPF_RPt(MAX_NPFR),XPeakFit(MAX_FITPT),YPeakFit(MAX_FITPT)

      COMMON /PEAKFIT2/PkFnVal(MPkDes,Max_NPFR),PkFnEsd(MPkDes,Max_NPFR), &
        PkFnCal(MPkDes,Max_NPFR),PkFnVarVal(3,MPkDes),PkFnVarEsd(3,MPkDes), &
        PkAreaVal(MAX_NPPR,MAX_NPFR),PkAreaEsd(MAX_NPPR,MAX_NPFR), &
        PkPosVal(MAX_NPPR,MAX_NPFR),PkPosEsd(MAX_NPPR,MAX_NPFR),PkPosAv(MAX_NPFR)

      COMMON /PROFTIC/ NTIC,IH(3,MTIC),ARGK(MTIC),DSTAR(MTIC)

      COMMON /ALLPEAKS/ NTPeak, AllPkPosVal(MTPeak), AllPkPosEsd(MTPeak), &
        PkProb(MTPeak), IOrdTem(MTPeak), IHPk(3,MTPeak)

      REAL    PkArgK(MTPeak), PkTicDif(MTPeak)
      REAL    TwoThetaDiff, AbsTwoThetaDiff
      INTEGER ICurSel ,IArgK(MTPeak)

! JvdS Loop over all hatched areas. Per area, count all peaks that the user has indicated to be present.
! Store all peaks thus found in one flat array: AllPkPosVal
! I don't understand why PkPosVal(I,J) is used for this, I would have used XPF_Pos
      NTPeak = 0
      Do J = 1, NumPeakFitRange
        Do I = 1, NumInPFR(J)
          NTPeak = NTPeak + 1
          AllPkPosVal(NTPeak) = PkPosVal(I,J)
          AllPkPosEsd(NTPeak) = PkPosEsd(I,J)
        END DO
      END DO
      CALL SORT_REAL(AllPkPosVal,IOrdTem,NTPeak)
! IOrdTem now contains and oredered list of pointers into AllPkPosVal
! JvdS @ why not order the list itself?
      IF (NTic .NE. 0) THEN
!.. Let's find the closest peaks and their distribution around the observed peak positions
        IR1 = 1 ! Pointer into list of reflections
        DO I = 1, NTPeak
          IOrd = IOrdTem(I) ! IOrd is now a pointer into AllPkPosVal to the next peak position
          TwoThetaDiff = ARGK(IR1) - AllPkPosVal(IOrd)
          AbsTwoThetaDiff = ABS(TwoThetaDiff)
          item = IR1
          DO IR = IR1, NTic
            xnew = ARGK(IR) - AllPkPosVal(IOrd)
            anew = ABS(xnew)
            IF (anew .LE. AbsTwoThetaDiff) THEN
              item = IR
              AbsTwoThetaDiff = anew
              TwoThetaDiff = xnew
            END IF
            IF (xnew .GT. 0.0) THEN
              IR1 = MAX(1,IR-1)
! As both the peaks and the refelctions are ordered, the position of the next peak can only be greater
              GOTO 20
            END IF
          END DO
 20       PkTicDif(I) = TwoThetaDiff
          DO II = 1, 3
            IHPk(II,I) = IH(II,item)
          END DO
          PkArgK(I) = ARGK(Item)
          IArgK(I) = Item
        END DO
        IF (NTPeak .EQ. 1) THEN
          SigmDif = 0.01
        ELSE
          PfTDMin = PkTicDif(1)
          PfTDMax = PkTicDif(1)
          DO II = 1, NTPeak
            PfTDMin = MIN(PfTDMin,PkTicDif(II))
            PfTDMax = MAX(PfTDMax,PkTicDif(II))
          END DO
          SigmDif = 0.2886751345948*Abs(PfTDMax-PfTDMin)
        END IF
        DO I = 1, NTPeak
          IOrd = IOrdTem(I)
          IA = IArgK(I)
          IRef1 = MAX(1,IA-5)
          IRef2 = MIN(NTic,IA+5)
          ProbTot = 0.0
          ProbTop = 0.0
          DifMin = ABS(AllPkPosVal(IOrd)-ArgK(IA))
          DifMinSq = DifMin**2
          ArgBot = 0.5/(SigmDif**2+AllPkPosEsd(IOrd)**2)
          DO IR = IRef1, IRef2
            ArgTop=(AllPkPosVal(IOrd)-ARGK(IR))**2
            ProbAdd=EXP(-ArgTop*ArgBot)
            IF (ABS(ArgTop-DifMinSq).LT.1.e-10) THEN
              ProbTop=ProbTop+ProbAdd
            END IF
            ProbTot=ProbTot+ProbAdd
          END DO
          PkProb(IOrd)=ProbTop/ProbTot
        END DO
      END IF
! Write out all the peak positions in an ordered list ...
      ICurSel = WinfoDialog(CurrentDialog)
      CALL WDialogSelect(IDD_Peak_Positions)
      CALL WDialogClearField(IDD_Peak_Positions_Grid)
      CALL WDialogSelect(IDD_Peak_Positions)
      CALL WGridRows(IDF_Peak_Positions_Grid,NTPeak)
      IF (NTPeak .GT. 0) THEN
        CALL WDialogFieldState(ID_Index_Output,Enabled)
        DO I = 1, NTPeak
          IOrd=IOrdTem(i)
          CALL WGridPutCellReal(IDF_Peak_Positions_Grid,1,I,AllPkPosVal(IOrd),'(F12.4)')
          CALL WGridPutCellReal(IDF_Peak_Positions_Grid,2,I,AllPkPosEsd(IOrd),'(F12.4)')
          CALL WGridPutCellReal(IDF_Peak_Positions_Grid,3,I,PkArgK(I),'(F12.4)')
          DifTem = AllPkPosVal(IOrd)-PkArgK(I)
          CALL WGridPutCellReal(IDF_Peak_Positions_Grid,4,I,DifTem,'(F12.4)')
          CALL WGridPutCellInteger(IDF_Peak_Positions_Grid,5,I,IHPk(1,I))
          CALL WGridPutCellInteger(IDF_Peak_Positions_Grid,6,I,IHPk(2,I))
          CALL WGridPutCellInteger(IDF_Peak_Positions_Grid,7,I,IHPk(3,I))
          CALL WGridPutCellReal(IDF_Peak_Positions_Grid,8,I,PkProb(IOrd),'(F8.3)')
        END DO
      ELSE
        CALL WDialogFieldState(ID_Index_Output,Disabled)
        DO I = 1, 8
          CALL WGridClearCell(IDF_Peak_Positions_Grid,I,1)
        END DO
      END IF
! Now do a refinement ...
      CALL RefineLattice()
      IF (ICurSel .GT. 0) CALL WDialogSelect(ICurSel)

      END SUBROUTINE Upload_Positions
!
!*****************************************************************************
!
! Subroutine to set the state of the global variable JRadOption to either synchrotron or lab data.
! This is updated in the main dialogue window and in the wizard.

      SUBROUTINE SetSourceDataState(tIRadOption)

      USE WINTERACTER
      USE DRUID_HEADER 

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: tIRadOption
      
      INCLUDE 'GLBVAR.INC' ! Contains JRadOption

! JvdS @ this can be reduced by a factor of three
      JRadOption = tIRadOption
      CALL PushActiveWindowID
      SELECT CASE (JRadOption)
        CASE (1) ! Lab X-ray
          CALL WDialogSelect(IDD_PW_Page2)
          CALL WDialogFieldState(IDF_PW_CW_group,Enabled)
          CALL WDialogFieldState(IDF_PW_radiation_label,Enabled)
          CALL WDialogFieldState(IDF_PW_wavelength1,Enabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Enabled)
          CALL WDialogFieldState(IDF_PW_TOF_group,Disabled)
          CALL WDialogFieldState(IDF_PW_Flight_Path_Label,Disabled)
          CALL WDialogFieldState(IDF_PW_flight_path,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta_label,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta0,Disabled)
          CALL WDialogPutRadioButton(IDF_PW_LabX_Source)
          CALL WDialogSelect(IDD_PW_Page4)
          CALL WDialogFieldState(IDF_PW_CW_group,Enabled)
          CALL WDialogFieldState(IDF_PW_radiation_label,Enabled)
          CALL WDialogFieldState(IDF_PW_wavelength1,Enabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Enabled)
          CALL WDialogFieldState(IDF_PW_TOF_group,Disabled)
          CALL WDialogFieldState(IDF_PW_Flight_Path_Label,Disabled)
          CALL WDialogFieldState(IDF_PW_flight_path,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta_label,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta0,Disabled)
          CALL WDialogPutRadioButton(IDF_PW_LabX_Source)
          CALL WDialogSelect(IDD_Data_Properties)
          CALL WDialogFieldState(IDF_CW_group,Enabled)
          CALL WDialogFieldState(IDF_radiation_label,Enabled)
          CALL WDialogFieldState(IDF_wavelength1,Enabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Enabled)
          CALL WDialogFieldState(IDF_TOF_group,Disabled)
          CALL WDialogFieldState(IDF_Flight_Path_Label,Disabled)
          CALL WDialogFieldState(IDF_flight_path,Disabled)
          CALL WDialogFieldState(IDF_2theta_label,Disabled)
          CALL WDialogFieldState(IDF_2theta0,Disabled)
          CALL WDialogPutRadioButton(IDF_LabX_Source)
        CASE (2,3) ! Synchrotron X-ray & CW neutron  
          CALL WDialogSelect(IDD_PW_Page2)
          CALL WDialogFieldState(IDF_PW_CW_group,Enabled)
          CALL WDialogFieldState(IDF_PW_radiation_label,Disabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
          CALL WDialogFieldState(IDF_PW_wavelength1,Enabled)
          CALL WDialogFieldState(IDF_PW_TOF_group,Disabled)
          CALL WDialogFieldState(IDF_PW_Flight_Path_Label,Disabled)
          CALL WDialogFieldState(IDF_PW_flight_path,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta_label,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta0,Disabled)
          IF (JRadOption .EQ. 2) THEN
            CALL WDialogPutRadioButton(IDF_PW_SynX_Source)
          ELSE
            CALL WDialogPutRadioButton(IDF_PW_CWN_Source)
          END IF
          CALL WDialogSelect(IDD_PW_Page4)
          CALL WDialogFieldState(IDF_PW_CW_group,Enabled)
          CALL WDialogFieldState(IDF_PW_radiation_label,Disabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
          CALL WDialogFieldState(IDF_PW_wavelength1,Enabled)
          CALL WDialogFieldState(IDF_PW_TOF_group,Disabled)
          CALL WDialogFieldState(IDF_PW_Flight_Path_Label,Disabled)
          CALL WDialogFieldState(IDF_PW_flight_path,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta_label,Disabled)
          CALL WDialogFieldState(IDF_PW_2theta0,Disabled)
          IF (JRadOption .EQ. 2) THEN
            CALL WDialogPutRadioButton(IDF_PW_SynX_Source)
          ELSE
            CALL WDialogPutRadioButton(IDF_PW_CWN_Source)
          END IF
          CALL WDialogSelect(IDD_Data_Properties)
          CALL WDialogFieldState(IDF_CW_group,Enabled)
          CALL WDialogFieldState(IDF_radiation_label,Disabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
          CALL WDialogFieldState(IDF_wavelength1,Enabled)
          CALL WDialogFieldState(IDF_TOF_group,Disabled)
          CALL WDialogFieldState(IDF_Flight_Path_Label,Disabled)
          CALL WDialogFieldState(IDF_flight_path,Disabled)
          CALL WDialogFieldState(IDF_2theta_label,Disabled)
          CALL WDialogFieldState(IDF_2theta0,Disabled)
          IF (JRadOption .EQ. 2) THEN
            CALL WDialogPutRadioButton(IDF_SynX_Source)
          ELSE
            CALL WDialogPutRadioButton(IDF_CWN_Source)
          END IF
        CASE (4) ! TOF neutron
          CALL WDialogSelect(IDD_PW_Page2)
          CALL WDialogFieldState(IDF_PW_CW_group,Disabled)
          CALL WDialogFieldState(IDF_PW_radiation_label,Disabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
          CALL WDialogFieldState(IDF_PW_wavelength1,Disabled)
          CALL WDialogFieldState(IDF_PW_TOF_group,Enabled)
          CALL WDialogFieldState(IDF_PW_Flight_Path_Label,Enabled)
          CALL WDialogFieldState(IDF_PW_flight_path,Enabled)
          CALL WDialogFieldState(IDF_PW_2theta_label,Enabled)
          CALL WDialogFieldState(IDF_PW_2theta0,Enabled)
          CALL WDialogPutRadioButton(IDF_PW_TOF_source)
          CALL WDialogSelect(IDD_PW_Page4)
          CALL WDialogFieldState(IDF_PW_CW_group,Disabled)
          CALL WDialogFieldState(IDF_PW_radiation_label,Disabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
          CALL WDialogFieldState(IDF_PW_wavelength1,Disabled)
          CALL WDialogFieldState(IDF_PW_TOF_group,Enabled)
          CALL WDialogFieldState(IDF_PW_Flight_Path_Label,Enabled)
          CALL WDialogFieldState(IDF_PW_flight_path,Enabled)
          CALL WDialogFieldState(IDF_PW_2theta_label,Enabled)
          CALL WDialogFieldState(IDF_PW_2theta0,Enabled)
          CALL WDialogPutRadioButton(IDF_PW_TOF_source)
          CALL WDialogSelect(IDD_Data_Properties)
          CALL WDialogFieldState(IDF_CW_group,Disabled)
          CALL WDialogFieldState(IDF_radiation_label,Disabled)
          CALL WDialogFieldState(IDF_Wavelength_Menu,Disabled)
          CALL WDialogFieldState(IDF_wavelength1,Disabled)
          CALL WDialogFieldState(IDF_TOF_group,Enabled)
          CALL WDialogFieldState(IDF_Flight_Path_Label,Enabled)
          CALL WDialogFieldState(IDF_flight_path,Enabled)
          CALL WDialogFieldState(IDF_2theta_label,Enabled)
          CALL WDialogFieldState(IDF_2theta0,Enabled)
          CALL WDialogPutRadioButton(IDF_TOF_source)
      END SELECT
      CALL PopActiveWindowID
      RETURN

      END SUBROUTINE SetSourceDataState
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

      CALL PushActiveWindowID
! Update values of constrained cell parameters to match the parameters they're constrained to
      SELECT CASE (LatBrav)
        CASE ( 1) ! Triclinic
        CASE ( 2) ! Monoclinic a
        CASE ( 3) ! Monoclinic b
        CASE ( 4) ! Monoclinic c
        CASE ( 5) ! Orthorhombic
        CASE ( 6) ! Tetragonal
          CellPar(2) = CellPar(1)
        CASE ( 7, 9) ! Trigonal / Hexagonal
          CellPar(2) = CellPar(1)
        CASE ( 8) ! Rhombohedral
          CellPar(2) = CellPar(1)
          CellPar(3) = CellPar(1)
          CellPar(5) = CellPar(4)
          CellPar(6) = CellPar(4)
        CASE (10) ! Cubic
          CellPar(2) = CellPar(1)
          CellPar(3) = CellPar(1)
      END SELECT
! Update all windows so that they show the contents of the global variables.
! This is in the cell parameters tab, in the wizard, and in the peak positions tab.
      CALL Upload_Cell_Constants()
      CALL WDialogSelect(IDD_PW_Page1)
      IF (FnUnitCellOK()) THEN
! Enable the wizard next button
        CALL WDialogFieldState(IDNEXT,Enabled)
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
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogPutOption(IDF_Space_Group_Menu,ISPosSG)
      CALL WDialogSelect(IDD_PW_Page1)
      CALL WDialogPutOption(IDF_Space_Group_Menu,ISPosSG)
      CALL PopActiveWindowID

      END SUBROUTINE Update_Space_Group
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
      CellParConstrained = .FALSE.
      CALL PushActiveWindowID
      SELECT CASE (LatBrav)
        CASE ( 1) ! Triclinic
        CASE ( 2) ! Monoclinic a
          CellPar(5) =  90.0
          CellPar(6) =  90.0
          CellParConstrained(5) = .TRUE.
          CellParConstrained(6) = .TRUE.
        CASE ( 3) ! Monoclinic b
          CellPar(4) =  90.0
          CellPar(6) =  90.0
          CellParConstrained(4) = .TRUE.
          CellParConstrained(6) = .TRUE.
        CASE ( 4) ! Monoclinic c
          CellPar(4) =  90.0
          CellPar(5) =  90.0
          CellParConstrained(4) = .TRUE.
          CellParConstrained(5) = .TRUE.
        CASE ( 5) ! Orthorhombic
          CellPar(4) =  90.0
          CellPar(5) =  90.0
          CellPar(6) =  90.0
          CellParConstrained(4) = .TRUE.
          CellParConstrained(5) = .TRUE.
          CellParConstrained(6) = .TRUE.
        CASE ( 6) ! Tetragonal
          CellPar(2) = CellPar(1)
          CellPar(4) =  90.0
          CellPar(5) =  90.0
          CellPar(6) =  90.0
          CellParConstrained(2) = .TRUE.
          CellParConstrained(4) = .TRUE.
          CellParConstrained(5) = .TRUE.
          CellParConstrained(6) = .TRUE.
        CASE ( 7, 9) ! Trigonal / Hexagonal
          CellPar(2) = CellPar(1)
          CellPar(4) =  90.0
          CellPar(5) =  90.0
          CellPar(6) = 120.0
          CellParConstrained(2) = .TRUE.
          CellParConstrained(4) = .TRUE.
          CellParConstrained(5) = .TRUE.
          CellParConstrained(6) = .TRUE.
        CASE ( 8) ! Rhombohedral
          CellPar(2) = CellPar(1)
          CellPar(3) = CellPar(1)
          CellPar(5) = CellPar(4)
          CellPar(6) = CellPar(4)
          CellParConstrained(2) = .TRUE.
          CellParConstrained(3) = .TRUE.
          CellParConstrained(5) = .TRUE.
          CellParConstrained(6) = .TRUE.
        CASE (10) ! Cubic
          CellPar(2) = CellPar(1)
          CellPar(3) = CellPar(1)
          CellPar(4) =  90.0
          CellPar(5) =  90.0
          CellPar(6) =  90.0
          CellParConstrained = .TRUE.
          CellParConstrained(1) = .FALSE.
      END SELECT
      CALL Upload_Cell_Constants
      CALL WDialogSelect(IDD_Crystal_Symmetry)
      CALL WDialogPutOption(IDF_Crystal_System_Menu,LatBrav)
      CALL WDialogSelect(IDD_PW_Page1)
      CALL WDialogPutOption(IDF_Crystal_System_Menu,LatBrav)
      CALL SetSpaceGroupMenu
      CALL PopActiveWindowID

      END SUBROUTINE UserSetCrystalSystem
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

      IF ((Iselection .GE. 2) .AND. (Iselection .LE. 6)) THEN
        CALL UpdateWavelength(FnWavelengthOfMenuOption(Iselection))
      ELSE
        CALL DebugErrorMessage('Non-existing item addressed in anode material menu')
      ENDIF

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

      END FUNCTION FnWavelengthOfMenuOption
!
!*****************************************************************************
!
      INTEGER FUNCTION GetCrystalSystem(IDashSg)
!
! This function determines the crystal system given one of the 530 space groups in DASH

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: IDashSg

      INCLUDE 'Lattice.inc'

      INTEGER I

      DO I = 2, 11
        IF (IDashSg .LT. LPosSG(I)) THEN
          GetCrystalSystem = I - 1
          RETURN
        ENDIF
      ENDDO
      CALL DebugErrorMessage('Space group out of range.')
      GetCrystalSystem = 1 ! Triclininc

      END FUNCTION GetCrystalSystem
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

      END SUBROUTINE SetModeMenuState
!
!*****************************************************************************
!
