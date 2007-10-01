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
      CALL PopActiveWindowID

      END SUBROUTINE Upload_Zero_Point
!
!*****************************************************************************
!
      SUBROUTINE Upload_Cell_Constants()

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
          IF (CellParConstrained(I)) THEN
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
          IF (CellParConstrained(I)) THEN
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

      REAL a,b,c,alpha,beta,gamma   

      CALL PushActiveWindowID
! Get all the cell constants from the selected area
      CALL WDialogSelect(IDownFrom)
      CALL WDialogGetReal(IDF_a_latt,a)
      CALL WDialogGetReal(IDF_b_latt,b)      
      CALL WDialogGetReal(IDF_c_latt,c)      
      CALL WDialogGetReal(IDF_alp_latt,alpha)      
      CALL WDialogGetReal(IDF_bet_latt,beta)      
      CALL WDialogGetReal(IDF_gam_latt,gamma)
      IF (a     .GT. 0.0) CellPar(1) = a
      IF (b     .GT. 0.0) CellPar(2) = b
      IF (c     .GT. 0.0) CellPar(3) = c
      IF (alpha .GT. 0.0) CellPar(4) = alpha
      IF (beta  .GT. 0.0) CellPar(5) = beta
      IF (gamma .GT. 0.0) CellPar(6) = gamma
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

      IMPLICIT NONE

      REAL, INTENT (IN   ) :: TheWaveLength

      INCLUDE 'GLBVAR.INC'

      INTEGER I, IRadSelection
      REAL    FnWavelengthOfMenuOption ! Function

      IF ((TheWaveLength .GT. 0.1) .AND. (TheWaveLength .LT. 20.0)) THEN
        ALambda = TheWaveLength
      ENDIF
      CALL PushActiveWindowID
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

      COMMON /TICCOMM/ NUMOBSTIC,XOBSTIC(MOBSTIC),YOBSTIC(MOBSTIC),&
        itypot(mobstic),iordot(mobstic),uobstic(20,mobstic),zobstic(20,mobstic)

      COMMON /PROFTIC/ NTic,IH(3,MTic),ArgK(MTic),DStar(MTic)

      COMMON /ALLPEAKS/ NTPeak,AllPkPosVal(MTPeak),AllPkPosEsd(MTPeak),&
        PkArgK(MTPeak),PkTicDif(MTPeak),PkProb(MTPeak), &
        IOrdTem(MTPeak),IHPk(3,MTPeak),IArgK(MTPeak)

      INTEGER ICurSel

      NTPeak = 0
      Do J = 1, NumPeakFitRange
        Do I = 1, NumInPFR(J)
          NTPeak = NTPeak + 1
          AllPkPosVal(NTPeak) = PkPosVal(I,J)
          AllPkPosEsd(NTPeak) = PkPosEsd(I,J)
        END DO
      END DO
      DO I = 1, NumObsTic
        NTPeak = NTPeak + 1
        AllPkPosVal(NTPeak) = XObsTic(I)
        AllPkPosEsd(NTPeak) = 0.005
      END DO
      CALL SORT_REAL(AllPkPosVal,IOrdTem,NTPeak)
      IF (NTic .NE. 0) THEN
!.. Let's find the closest peaks and their distribution around the observed peak positions
        IR1 = 1
        DO I = 1, NTPeak
          IOrd = IOrdTem(I)
          xtem = ArgK(IR1)-AllPkPosVal(IOrd)
          atem = ABS(xtem)
          item = IR1
          DO IR = IR1, NTic
            xnew = ArgK(IR) - AllPkPosVal(IOrd)
            anew = ABS(xnew)
            IF (anew .LE. atem) THEN
              item = IR
              atem = anew
              xtem = xnew
            END IF
            IF (xnew .GT. 0.0) THEN
              IR1 = MAX(1,IR-1)
              GOTO 20
            END IF
          END DO
 20       PkTicDif(I) = xtem
          DO II = 1, 3
            IHPk(II,I) = IH(II,item)
          END DO
          PkArgK(I) = ArgK(Item)
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
            ArgTop=(AllPkPosVal(IOrd)-ArgK(IR))**2
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
          DifTem=AllPkPosVal(IOrd)-PkArgK(I)
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
      SUBROUTINE Upload_Wizard_Information()

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'statlog.inc'

      CALL PushActiveWindowID
      CALL SetCrystalSystem(LatBrav)
      CALL SetSpaceGroupMenu
      CALL Upload_Cell_Constants()
      CALL Upload_Range()
      CALL PopActiveWindowID

      END SUBROUTINE Upload_Wizard_Information      
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
