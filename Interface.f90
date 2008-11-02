! This file contains routines that form the layer between DASH and Winteracter to
! mutually exchange variables.
!
! Ideally, this file should one day be such that only this file and 'Basic_IO.f90'
! have to be replaced when a new interface is programmed.
!
! One of the requirements for this to be possible is that every variable held in
! a Winteracter dialogue is either also a variable in DASH or is retrieved on the fly
! by means of a function (cf. AutoLocalMinimisation()).
!
! Download_XXXXX = retrieves variable(s) XXXXX from a Winteracter dialogue and stores it in
!                  the corresponding global variable(s) in DASH
! As it is no use downloading a global variable without updating the other Winteracter dialogues,
! these subroutines always update that variable
!
! Upload_XXXXX   = stores the global variable(s) XXXXX from DASH in the corresponding
!                  variable(s) in all Winteracter dialogues
!
!*****************************************************************************
!
        SUBROUTINE WDialogFieldStateLogical(TheFieldIdentifier, TheLogical)
!
! This subroutine provides a wrapper around the Winteracter WDialogFieldState routine,
! which takes an integer as an argument to define the state of a field.
! It is more natural to have a routine that takes a logical as an argument
! to define the state of a field.
! This is that routine.
!
      USE WINTERACTER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheFieldIdentifier
      LOGICAL, INTENT (IN   ) :: TheLogical

      IF ( TheLogical ) THEN
        CALL WDialogFieldState(TheFieldIdentifier, Enabled)
      ELSE
        CALL WDialogFieldState(TheFieldIdentifier, Disabled)
      ENDIF

      END SUBROUTINE WDialogFieldStateLogical
!
!*****************************************************************************
!
        SUBROUTINE WDialogPutCheckBoxLogical(TheFieldIdentifier, TheLogical)
!
! This subroutine provides a wrapper around the Winteracter WDialogPutCheckBox routine,
! which takes an integer as an argument to define the state of a check box.
! As a checkbox is the front-end equivalent of a variable of type LOGICAL,
! it is more natural to have a routine that takes a logical as an argument
! to define the state of a check box.
! This is that routine.
!
      USE WINTERACTER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheFieldIdentifier
      LOGICAL, INTENT (IN   ) :: TheLogical

      IF (TheLogical) THEN
        CALL WDialogPutCheckBox(TheFieldIdentifier, Checked)
      ELSE
        CALL WDialogPutCheckBox(TheFieldIdentifier, UnChecked)
      ENDIF

      END SUBROUTINE WDialogPutCheckBoxLogical
!
!*****************************************************************************
!
      LOGICAL FUNCTION DASHWDialogGetCheckBoxLogical(TheFieldIdentifier)
!
! This function provides a wrapper around the Winteracter DASHWDialogGetCheckBox routine,
! which takes the state of a check box and stores it in an integer.
! As a checkbox is the front-end equivalent of a variable of type LOGICAL,
! it is more natural to have a routine that takes returns the state of a check box in a logical.
! This is that routine.
!
      USE WINTERACTER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheFieldIdentifier

      INTEGER I

      CALL DASHWDialogGetCheckBox(TheFieldIdentifier, I)
      DASHWDialogGetCheckBoxLogical = (I .EQ. Checked)

      END FUNCTION DASHWDialogGetCheckBoxLogical
!
!*****************************************************************************
!
      INTEGER FUNCTION DASHWDialogGetRadioButtonInt(TheFieldIdentifier)
!
! This function provides a wrapper around the Winteracter DASHWDialogGetRadioButton routine,
! which takes the state of a set of radio buttons and stores it in an integer.
! This function returns the integer instead of storing it, eliminating the temporary
! variable in the calling code.
!
      USE WINTERACTER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheFieldIdentifier

      INTEGER I

      CALL DASHWDialogGetRadioButton(TheFieldIdentifier, I)
      DASHWDialogGetRadioButtonInt = I

      END FUNCTION DASHWDialogGetRadioButtonInt
!
!*****************************************************************************
!
      SUBROUTINE ScrUpdateFileName
!
! This routine updates all occurrences of the filename, both
! on the status bar and in the wizard.
!
! JvdS 17 July 2001
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch


      IF ( IN_BATCH ) RETURN
! Note that FNAME is a global variable in VARIABLES

! Remember current dialogue window
      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_PW_Page3)
      CALL WDialogPutString(IDF_PWa_DataFileName_String, FNAME)
      CALL PopActiveWindowID
! Update the status bar at the bottom of the screen.
      CALL WindowOutStatusBar(1, FNAME)

      END SUBROUTINE ScrUpdateFileName

!
!*****************************************************************************
!
      SUBROUTINE ScrUpdateFileNameSDIFile(filename)
!
! This routine updates all occurrences of the SDI filename, both
! on the status bar and in the wizard.
!
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: filename

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SAW_Page1)
      CALL WDialogPutString(IDF_SA_Project_Name, filename)
      CALL SelectDASHDialog(IDD_SAW_Page6)
      CALL WDialogPutString(IDF_SDI_File_Name, filename)
      CALL PopActiveWindowID
! Update the status bar at the bottom of the screen.
      CALL WindowOutStatusBar(1, filename)

      END SUBROUTINE ScrUpdateFileNameSDIFile
!
!*****************************************************************************
!
      REAL FUNCTION WavelengthOf(TheAnodeMaterial)
!
! This function return the wavelength of an X-ray tube given the material the anode is made of.
!
! JvdS 29 July 2001
!
! INPUT   : TheAnodeMaterial = the anode material, e.g. 'Cu'
!
! RETURNS : The wavelength as provided by the International Centre for Diffraction Data
!           0.0 if material not recognised
!
      IMPLICIT NONE

      CHARACTER*2, INTENT (IN   ) :: TheAnodeMaterial ! Chemical symbol for anode material, e.g. 'Cu'

      CHARACTER*1, EXTERNAL :: ChrUpperCase
      CHARACTER*1, EXTERNAL :: ChrLowerCase
      CHARACTER*2 tAnodeMaterial ! To remove call by value / call by reference ambiguity

      tAnodeMaterial(1:1) = ChrUpperCase(TheAnodeMaterial(1:1))
      tAnodeMaterial(2:2) = ChrLowerCase(TheAnodeMaterial(2:2))
      SELECT CASE (tAnodeMaterial)
           CASE ('Cu')
             WavelengthOf = 1.54056
           CASE ('Mo')
             WavelengthOf = 0.70930
           CASE ('Co')
             WavelengthOf = 1.78897
           CASE ('Fe')
             WavelengthOf = 1.93604
           CASE ('Cr')
             WavelengthOf = 2.28970
           CASE DEFAULT
             WavelengthOf = 0.0
         END SELECT

      END FUNCTION  WavelengthOf
!
!*****************************************************************************
!
      REAL FUNCTION TwoTheta2dSpacing(TheTwoTheta)     
!
! Calculates the d-spacing for the given 2 theta value using the wavelength in ALambda
!
      IMPLICIT NONE

      REAL, INTENT (IN   ) :: TheTwoTheta

      INCLUDE 'GLBVAR.INC'

      LOGICAL, EXTERNAL :: FnWaveLengthOK
      REAL, EXTERNAL :: Degrees2Radians, WavelengthOf

      IF (.NOT. FnWaveLengthOK()) THEN
        CALL ErrorMessage('Wavelength invalid, will be set to Cu')
        CALL Set_Wavelength(WavelengthOf('Cu'))
      ENDIF
      IF (TheTwoTheta .LT. 0.1) THEN
        TwoTheta2dSpacing = 1000000.0
        CALL DebugErrorMessage('TheTwoTheta .LT. 0.1 in TwoTheta2dSpacing')
      ELSE
        TwoTheta2dSpacing = ALambda / (2*SIN(Degrees2Radians(TheTwoTheta/2)))
      ENDIF

      END FUNCTION TwoTheta2dSpacing
!
!*****************************************************************************
!
      REAL FUNCTION dSpacing2TwoTheta(ThedSpacing)     
!
! Calculates 2 theta for the given d-spacing using the wavelength in ALambda
!
      IMPLICIT NONE

      REAL, INTENT (IN   ) :: ThedSpacing

      INCLUDE 'GLBVAR.INC'

      LOGICAL, EXTERNAL :: FnWaveLengthOK 
      REAL, EXTERNAL :: WavelengthOf, TwoTheta2dSpacing, Radians2Degrees

      IF (.NOT. FnWaveLengthOK()) THEN
        CALL ErrorMessage('Wavelength invalid, will be set to Cu')
        CALL Set_Wavelength(WavelengthOf('Cu'))
      ENDIF
! Calculate minimum d-spacing for the given wavelength
      IF (ThedSpacing .LT. TwoTheta2dSpacing(89.9999)) THEN
        dSpacing2TwoTheta = 90.0
      ELSE
        dSpacing2TwoTheta = 2 * Radians2Degrees(ASIN(ALambda/(2*ThedSpacing)))
      ENDIF

      END FUNCTION dSpacing2TwoTheta
!
!*****************************************************************************
!
      LOGICAL FUNCTION Get_ShowCumChiSqd

! When .TRUE., the cumulative chi-sqd is plotted

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Plot_Option_Dialog)
      Get_ShowCumChiSqd = DASHWDialogGetCheckBoxLogical(IDF_ShowCumChiSqd)
      CALL PopActiveWindowID

      END FUNCTION Get_ShowCumChiSqd
!
!*****************************************************************************
!
      LOGICAL FUNCTION Get_DivideByEsd

! When .TRUE., the points of the difference curve are divided by their ESDs

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Plot_Option_Dialog)
      Get_DivideByEsd = DASHWDialogGetCheckBoxLogical(IDF_DivDiffByEsd)
      CALL PopActiveWindowID

      END FUNCTION Get_DivideByEsd
!
!*****************************************************************************
!
      LOGICAL FUNCTION Get_OutputChi2vsMoves

! When .TRUE., the profile chi**2 versus moves graph is written out to a file

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL SAVE_CVM
      COMMON / CSAVECVM / SAVE_CVM

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        SAVE_CVM = DASHWDialogGetCheckBoxLogical(IDF_OutputChi2vsMoves)
        CALL PopActiveWindowID
      ENDIF

      Get_OutputChi2vsMoves = SAVE_CVM

      END FUNCTION Get_OutputChi2vsMoves

      SUBROUTINE Set_OutputChi2vsMoves(Setting)

! When .TRUE., the profile chi**2 versus moves graph is written out to a file

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL Setting

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL SAVE_CVM
      COMMON / CSAVECVM / SAVE_CVM

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        CALL WDialogPutCheckBoxLogical(IDF_OutputChi2vsMoves, Setting)
        CALL PopActiveWindowID
      ENDIF

      SAVE_CVM = Setting

      END SUBROUTINE Set_OutputChi2vsMoves
!
!*****************************************************************************
!
      SUBROUTINE Set_AutoLocalMinimisation(TheValue)

! When .TRUE., each run in a multi run ends with a local minimisation

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, INTENT (IN   ) :: TheValue

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch



      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        CALL WDialogPutCheckBoxLogical(IDF_AutoLocalOptimise, TheValue)
      ENDIF

      AutoMinimise = TheValue
      IF (.NOT. IN_BATCH ) THEN
        IF ( AutoMinimise ) THEN
!   If hydrogens are used during SA, force use of hydrogens during autominimise
          CALL WDialogFieldStateLogical(IDF_UseHydrogensAuto, .NOT. LOG_HYDROGENS)
        ELSE
          CALL WDialogFieldState(IDF_UseHydrogensAuto, Disabled)
        ENDIF
        CALL PopActiveWindowID
      ENDIF
      END SUBROUTINE Set_AutoLocalMinimisation
!
!*****************************************************************************
!
      SUBROUTINE Set_UseHydrogensDuringAutoLocalMinimise(TheValue)

! When .TRUE., hydrogens are treated explicitly during auto local minimise

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, INTENT (IN   ) :: TheValue

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment


      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      UseHAutoMin = TheValue

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        CALL WDialogPutCheckBoxLogical(IDF_UseHydrogensAuto, TheValue)
        CALL PopActiveWindowID
      ENDIF

      END SUBROUTINE Set_UseHydrogensDuringAutoLocalMinimise
!
!*****************************************************************************
!
      SUBROUTINE Set_UseCrystallographicCoM(TheValue)

! When .TRUE., when calculating the centre of rotation of a Z-matrix,
! each atom is weighted by the square of its number of electrons

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, INTENT (IN   ) :: TheValue

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch


      UseCCoM = TheValue

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        CALL WDialogPutCheckBoxLogical(IDF_CrystallographicCoM, TheValue)
        CALL PopActiveWindowID
      ENDIF
      END SUBROUTINE Set_UseCrystallographicCoM
!
!*****************************************************************************
!
      SUBROUTINE Set_AutoAlign(TheValue)

! When .TRUE., the molecules of each solution in a multi run are set to a default position/orientation

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, INTENT (IN   ) :: TheValue

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment


      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LAlign = TheValue

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        CALL WDialogPutCheckBoxLogical(IDF_Align, TheValue)
        CALL PopActiveWindowID
      ENDIF

      END SUBROUTINE Set_AutoAlign
!
!*****************************************************************************
!
      LOGICAL FUNCTION Get_AutoAlign

! When .TRUE., the molecules of each solution in a multi run are set to a default position/orientation

      IMPLICIT NONE

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      Get_AutoAlign = LAlign

      END FUNCTION Get_AutoAlign
!
!*****************************************************************************
!
      SUBROUTINE Set_HydrogenTreatment(iState)
! 1 = ignore
! 2 = absorb
! 3 = explicit
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: iState

      LOGICAL           LOG_HYDROGENS
      COMMON /HYDROGEN/ LOG_HYDROGENS

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch


      HydrogenTreatment = iState
      LOG_HYDROGENS = (HydrogenTreatment .EQ. 3)

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        SELECT CASE (HydrogenTreatment)
          CASE (1)
            CALL WDialogPutRadioButton(IDR_HydrogensIgnore)
          CASE (2)
            CALL WDialogPutRadioButton(IDR_HydrogensAbsorb)
          CASE (3)
            CALL WDialogPutRadioButton(IDR_HydrogensExplicit)
        END SELECT


        IF ( LOG_HYDROGENS ) THEN
! If hydrogens are used during SA, force use of hydrogens during autominimise
          CALL WDialogFieldState(IDF_UseHydrogensAuto, Disabled)
        ELSE
          CALL WDialogFieldStateLogical(IDF_UseHydrogensAuto, DASHWDialogGetCheckBoxLogical(IDF_AutoLocalOptimise))
        ENDIF
        CALL PopActiveWindowID
      ENDIF

      END SUBROUTINE Set_HydrogenTreatment
!
!*****************************************************************************
!
      LOGICAL FUNCTION SavePDB

! When .TRUE., a file in .pdb format is written out for each SA solution

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical


      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL SAVE_PDB
      COMMON / CSAVEPDB / SAVE_PDB

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        SAVE_PDB = DASHWDialogGetCheckBoxLogical(IDF_OutputPDB)
        CALL PopActiveWindowID
      ENDIF

      SavePDB = SAVE_PDB

      END FUNCTION SavePDB

      SUBROUTINE Set_SavePDB(Setting)

! When .TRUE., a file in .pdb format is written out for each SA solution

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL Setting

      LOGICAL SAVE_PDB
      COMMON / CSAVEPDB / SAVE_PDB

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        CALL WDialogPutCheckBoxLogical(IDF_OutputPDB, Setting)
        CALL PopActiveWindowID
      ENDIF

      SAVE_PDB = Setting

      END SUBROUTINE Set_SavePDB
!
!*****************************************************************************
!
      LOGICAL FUNCTION Get_SavePRO

! When .TRUE., a file containing the calculated profile is saved for each SA run

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      LOGICAL SAVE_PRO
      COMMON / CSAVEPRO / SAVE_PRO

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch
      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        SAVE_PRO = DASHWDialogGetCheckBoxLogical(IDF_OutputPRO)
        CALL PopActiveWindowID
      ENDIF
      Get_SavePRO = SAVE_PRO

      END FUNCTION Get_SavePRO

      SUBROUTINE Set_SavePRO(Setting)

! When .TRUE., a file containing the calculated profile is saved for each SA run

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE
      LOGICAL Setting

      LOGICAL SAVE_PRO
      COMMON / CSAVEPRO / SAVE_PRO

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        CALL WDialogPutCheckBoxLogical(IDF_OutputPRO, Setting)
        CALL PopActiveWindowID
      ENDIF

      SAVE_PRO = Setting
      

      END SUBROUTINE Set_SavePRO

!
!*****************************************************************************
!
      LOGICAL FUNCTION Get_SavePrjAtEnd

! When .TRUE., a file containing the calculated profile is saved for each SA run

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      LOGICAL SAVE_PRJ
      COMMON / CSAVEPRJ / SAVE_PRJ

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch


      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        SAVE_PRJ = DASHWDialogGetCheckBoxLogical(IDC_OuputDASH)
        CALL PopActiveWindowID
      ENDIF
      Get_SavePrjAtEnd = SAVE_PRJ

      END FUNCTION Get_SavePrjAtEnd

      SUBROUTINE Set_SavePrjAtEnd(Setting)

! When .TRUE., a file containing the calculated profile is saved for each SA run

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL Setting

      LOGICAL SAVE_PRJ
      COMMON / CSAVEPRJ / SAVE_PRJ

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch


      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        CALL WDialogPutCheckBoxLogical(IDC_OuputDASH, Setting)
        CALL PopActiveWindowID
      ENDIF

      SAVE_PRJ = Setting

      END SUBROUTINE Set_SavePrjAtEnd
!
!*****************************************************************************
!
      LOGICAL FUNCTION Get_SaveParamAtEnd

! When .TRUE., a file containing the calculated profile is saved for each SA run

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      LOGICAL SAVE_PAE
      COMMON / CSAVEPAE / SAVE_PAE

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        SAVE_PAE = DASHWDialogGetCheckBoxLogical(IDF_OutputTbl)
        CALL PopActiveWindowID
      ENDIF
      Get_SaveParamAtEnd = SAVE_PAE
      END FUNCTION Get_SaveParamAtEnd

      SUBROUTINE Set_SaveParamAtEnd(Setting)

! When .TRUE., a file containing the calculated profile is saved for each SA run

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      LOGICAL Setting

      LOGICAL SAVE_PAE
      COMMON / CSAVEPAE / SAVE_PAE

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        CALL WDialogPutCheckBoxLogical(IDF_OutputTbl, Setting)
        CALL PopActiveWindowID
      ENDIF
      SAVE_PAE = Setting

      END SUBROUTINE Set_SaveParamAtEnd
!
!*****************************************************************************
!
      LOGICAL FUNCTION Get_ColourFlexibleTorsions

! When .TRUE., flexible torsions are coloured when viewing a Z-matrix

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Configuration)
      Get_ColourFlexibleTorsions = DASHWDialogGetCheckBoxLogical(IDF_ColFlexTors)
      CALL PopActiveWindowID

      END FUNCTION Get_ColourFlexibleTorsions
!
!*****************************************************************************
!
      LOGICAL FUNCTION Get_WriteWavelength2XYEFile

! When .TRUE. (the default), the wavelength is automatically written to the first line of
! every .xye file that is written. This is the DASH default, but disagrees with .xye
! files written and read by other programs.

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Configuration)
      Get_WriteWavelength2XYEFile = DASHWDialogGetCheckBoxLogical(IDC_wl_in_xye)
      CALL PopActiveWindowID

      END FUNCTION Get_WriteWavelength2XYEFile
!
!*****************************************************************************
!
      LOGICAL FUNCTION PlotObservedErrorBars
!
! This function retrieves the value of the 'plot error bars' checkbox in the plot options panel
!
! RETURNS : .TRUE.  if user requested the error bars     to be plotted
!           .FALSE. if user requested the error bars not to be plotted
!
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Plot_Option_Dialog)
      PlotObservedErrorBars = DASHWDialogGetCheckBoxLogical(IDF_ErrorBar_Check) 
      CALL PopActiveWindowID

      END FUNCTION PlotObservedErrorBars

      LOGICAL FUNCTION PlotDifferenceErrorBars
!
! This function retrieves the value of the 'plot difference error bars' checkbox in the plot options panel
!
! RETURNS : .TRUE.  if user requested the error bars     to be plotted
!           .FALSE. if user requested the error bars not to be plotted
!
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Plot_Option_Dialog)
      PlotDifferenceErrorBars = DASHWDialogGetCheckBoxLogical(IDF_DifferenceErrorBar_Check) 
      CALL PopActiveWindowID
      END FUNCTION PlotDifferenceErrorBars


      REAL FUNCTION PlotEsdMultiplier
!
! This function retrieves the value of the 'plot error multiplier field' checkbox in the plot options panel
!
! RETURNS : .TRUE.  if user requested the error bars     to be plotted
!           .FALSE. if user requested the error bars not to be plotted
!
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Plot_Option_Dialog)
      CALL DASHWDialogGetReal(IDF_ErrorMultiplier_RealEntry, PlotEsdMultiplier) 
      CALL PopActiveWindowID
      END FUNCTION PlotEsdMultiplier
!
!*****************************************************************************
!
      LOGICAL FUNCTION PlotBackground
!
! This function retrieves the value of the 'plot background' checkbox in the plot options panel
!
! RETURNS : .TRUE.  if user requested the background     to be plotted
!           .FALSE. if user requested the background not to be plotted
!
      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Plot_Option_Dialog)
      PlotBackground = DASHWDialogGetCheckBoxLogical(IDF_background_check)
      CALL PopActiveWindowID

      END FUNCTION PlotBackground
!
!*****************************************************************************
!
      LOGICAL FUNCTION ConnectPointsObs

! .TRUE. = when drawing the observed profile, the data points are joined by lines

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Plot_Option_Dialog)
      ConnectPointsObs = DASHWDialogGetCheckBoxLogical(IDF_ConnectObsPoints)
      CALL PopActiveWindowID

      END FUNCTION ConnectPointsObs
!
!*****************************************************************************
!
      LOGICAL FUNCTION PlotPeakFitDifferenceProfile

! .TRUE. = when fitting peaks, the difference profile is plotted.

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Plot_Option_Dialog)
      PlotPeakFitDifferenceProfile = DASHWDialogGetCheckBoxLogical(IDF_PlotPeakFitDif)
      CALL PopActiveWindowID

      END FUNCTION PlotPeakFitDifferenceProfile
!
!*****************************************************************************
!
      LOGICAL FUNCTION SaveCSSR

! When .TRUE., a file in .cssr format is written out for each SA solution

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL SAVE_CSSR
      COMMON / CSAVECSSR / SAVE_CSSR

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        SAVE_CSSR = DASHWDialogGetCheckBoxLogical(IDF_OutputCSSR)
        CALL PopActiveWindowID
      ENDIF

      SaveCSSR = SAVE_CSSR

      END FUNCTION SaveCSSR

      SUBROUTINE Set_SaveCSSR(Setting)

! When .TRUE., a file in .cssr format is written out for each SA solution

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      
      LOGICAL Setting

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL SAVE_CSSR
      COMMON / CSAVECSSR / SAVE_CSSR

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        CALL WDialogPutCheckBoxLogical(IDF_OutputCSSR, Setting)
        CALL PopActiveWindowID
      ENDIF

      SAVE_CSSR = Setting

      END SUBROUTINE Set_SaveCSSR
!
!
!*****************************************************************************
!
      LOGICAL FUNCTION SaveCCL

! When .TRUE., a file in .ccl format is written out for each SA solution

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL SAVE_CCL
      COMMON / CSAVECCL / SAVE_CCL

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        SAVE_CCL = DASHWDialogGetCheckBoxLogical(IDF_OutputCCL)
        CALL PopActiveWindowID
      ENDIF

      SaveCCL = SAVE_CCL

      END FUNCTION SaveCCL

      SUBROUTINE Set_SaveCCL(Setting)

! When .TRUE., a file in .ccl format is written out for each SA solution

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL Setting

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL SAVE_CCL
      COMMON / CSAVECCL / SAVE_CCL

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        CALL WDialogPutCheckBoxLogical(IDF_OutputCCL, Setting)
        CALL PopActiveWindowID
      ENDIF

      SAVE_CCL = Setting

      END SUBROUTINE Set_SaveCCL
!
!*****************************************************************************
!
      LOGICAL FUNCTION SaveCIF

! When .TRUE., a file in .cif format is written out for each SA solution

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL SAVE_CIF
      COMMON / CSAVECIF / SAVE_CIF

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        SAVE_CIF = DASHWDialogGetCheckBoxLogical(IDF_OutputCIF)
        CALL PopActiveWindowID
      ENDIF

      SaveCIF = SAVE_CIF
      END FUNCTION SaveCIF

      SUBROUTINE Set_SaveCIF(Setting)

! When .TRUE., a file in .cif format is written out for each SA solution

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL Setting

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL SAVE_CIF
      COMMON / CSAVECIF / SAVE_CIF

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        CALL WDialogPutCheckBoxLogical(IDF_OutputCIF, Setting)
        CALL PopActiveWindowID
      ENDIF

      SAVE_CIF = Setting

      END SUBROUTINE Set_SaveCIF
!
!
!*****************************************************************************
!
      LOGICAL FUNCTION SaveRES

! When .TRUE., a file in .res format is written out for each SA solution

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL SAVE_RES
      COMMON / CSAVERES / SAVE_RES

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        SAVE_RES = DASHWDialogGetCheckBoxLogical(IDF_OutputRES)
        CALL PopActiveWindowID
      ENDIF

      SaveRES = SAVE_RES
      END FUNCTION SaveRES

      SUBROUTINE Set_SaveRES(Setting)

! When .TRUE., a file in .res format is written out for each SA solution

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL Setting

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL SAVE_RES
      COMMON / CSAVERES / SAVE_RES


      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_SA_input4)
        CALL WDialogPutCheckBoxLogical(IDF_OutputRES, Setting)
        CALL PopActiveWindowID
      ENDIF

      SAVE_RES = Setting
      END SUBROUTINE Set_SaveRES
!
!*****************************************************************************
!
      SUBROUTINE Upload_ZeroPoint

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( IN_BATCH ) RETURN


      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Peak_Positions)
      CALL WDialogPutReal(IDF_ZeroPoint, ZeroPoint, '(F10.4)')
      CALL SelectDASHDialog(IDD_Crystal_Symmetry)
      CALL WDialogPutReal(IDF_ZeroPoint, ZeroPoint, '(F10.4)')
      CALL SelectDASHDialog(IDD_Index_Preparation)
      CALL WDialogPutReal(IDF_ZeroPoint, ZeroPoint, '(F10.4)')
      CALL SelectDASHDialog(IDD_PW_Page1)
      CALL WDialogPutReal(IDF_ZeroPoint, ZeroPoint, '(F10.4)')
      CALL SelectDASHDialog(IDD_PW_Page8)
      CALL WDialogPutReal(IDF_ZeroPoint, ZeroPoint, '(F10.4)')
      CALL PopActiveWindowID

      END SUBROUTINE Upload_ZeroPoint
!
!*****************************************************************************
!
      SUBROUTINE Upload_Cell_Constants
!
! Puts the global variables CellPar(1:6) into the Winteracter menus
!
      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      LOGICAL, EXTERNAL :: ValidCellAxisLength, FnUnitCellOK
      REAL, EXTERNAL :: UnitCellVolume
      REAL V
      INTEGER WindowNr
      INTEGER CellParID(6)
      INTEGER I

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( in_batch ) return

      CellParID(1) = IDF_a_latt
      CellParID(2) = IDF_b_latt
      CellParID(3) = IDF_c_latt
      CellParID(4) = IDF_alp_latt
      CellParID(5) = IDF_bet_latt
      CellParID(6) = IDF_gam_latt
      CALL PushActiveWindowID
      DO WindowNr = 1, 4
        SELECT CASE (WindowNr)
          CASE (1) 
            CALL SelectDASHDialog(IDD_Crystal_Symmetry)
          CASE (2) 
            CALL SelectDASHDialog(IDD_PW_Page1)
          CASE (3) 
            CALL SelectDASHDialog(IDD_SX_Page1)
          CASE (4) 
            CALL SelectDASHDialog(IDD_Peak_Positions)
        END SELECT
! Update all the unit cell lengths ...
        DO I = 1, 3
          IF (ValidCellAxisLength(CellPar(I))) THEN
            CALL WDialogPutReal(CellParID(I), CellPar(I), '(F10.5)')
          ELSE
            CALL WDialogClearField(CellParID(I))
          ENDIF
        ENDDO
! ... and the unit cell angles
        DO I = 4, 6
          IF (CellPar(I) .LT. 0.00001) THEN
            CALL WDialogClearField(CellParID(I))
          ELSE
            CALL WDialogPutReal(CellParID(I), CellPar(I), '(F10.3)')
          ENDIF
        ENDDO
! Update their Enabled/Disabled state depending on whether they are constrained by the crystal system
        IF (WindowNr .NE. 4) THEN
          IF (PastPawley) THEN ! Just make everything read only
            DO I = 1, 6
              CALL WDialogFieldState(CellParID(I), DialogReadOnly)
            ENDDO
          ELSE
            DO I = 1, 6
              CALL WDialogFieldStateLogical(CellParID(I), .NOT. CellParConstrained(I))
            ENDDO
          ENDIF
        ENDIF
      ENDDO
! Update volume in View dialogue
      CALL SelectDASHDialog(IDD_Crystal_Symmetry)
      IF (FnUnitCellOK()) THEN
        V = UnitCellVolume(CellPar(1), CellPar(2), CellPar(3), CellPar(4), CellPar(5), CellPar(6))
        CALL WDialogPutReal(IDF_UCVol, V, '(F10.3)')
      ELSE
        CALL WDialogClearField(IDF_UCVol)
      ENDIF
      CALL Upload_Positions
      CALL CheckIfWeCanDoAPawleyRefinement
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
      CALL SelectDASHDialog(IDownFrom)
      CALL DASHWDialogGetReal(IDF_a_latt, CellPar(1))
      CALL DASHWDialogGetReal(IDF_b_latt, CellPar(2))      
      CALL DASHWDialogGetReal(IDF_c_latt, CellPar(3))      
      CALL DASHWDialogGetReal(IDF_alp_latt, CellPar(4))      
      CALL DASHWDialogGetReal(IDF_bet_latt, CellPar(5))      
      CALL DASHWDialogGetReal(IDF_gam_latt, CellPar(6))
      CALL UpdateCell
      CALL CheckIfWeCanDoAPawleyRefinement
      CALL PopActiveWindowID

      END SUBROUTINE Download_Cell_Constants
!
!*****************************************************************************
!
! Sequence of subroutines that handle the downloading of each field in turn
      SUBROUTINE DownLoadWavelength(From)

      USE WINTERACTER
      USE DRUID_HEADER

      INTEGER, INTENT (IN   ) :: From

      REAL Temp

      CALL PushActiveWindowID
      CALL SelectDASHDialog(From)
      CALL DASHWDialogGetReal(IDF_wavelength1,Temp)
      CALL Set_Wavelength(Temp)
      CALL PopActiveWindowID

      END SUBROUTINE DownLoadWavelength
!
!*****************************************************************************
!
      SUBROUTINE Set_Wavelength(TheWaveLength)

      USE VARIABLES
      
      IMPLICIT NONE

      REAL, INTENT (IN   ) :: TheWaveLength

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER                BackupNOBS
      REAL                               BackupXOBS,       BackupYOBS,       BackupEOBS
      COMMON /BackupPROFOBS/ BackupNOBS, BackupXOBS(MOBS), BackupYOBS(MOBS), BackupEOBS(MOBS)

      LOGICAL, EXTERNAL :: NearlyEqual, Confirm, Get_WriteWavelength2XYEFile
      INTEGER tFileHandle, I

      IF ( (TheWaveLength .LT. 0.01) .OR. (TheWaveLength .GT. 20.0) ) THEN
        CALL ErrorMessage('Invalid wavelength')
        RETURN
      ENDIF
! Unlike DF, there is no left-to-right and/or short-circuit evaluation in Intel Fortran (IF10):
! the second term is still evaluated even when the first one is false(even replaced by constant .FALSE.).
! Split the two terms into nesting IF...THEN...ENDIF
!      IF ( NoWavelengthInXYE .AND. Get_WriteWavelength2XYEFile() ) THEN
      IF ( NoWavelengthInXYE ) THEN
        IF ( Get_WriteWavelength2XYEFile() ) THEN
          IF (Confirm('For ease of use, DASH now interprets a single number on the first line of an .xye file as a wavelength.'//CHAR(13)// &
                      'The file you are using does not contain a wavelength yet.'//CHAR(13)// &
                      'Would you like to write the wavelength you have just entered to the file'//CHAR(13)// &
                      FNAME(1:LEN_TRIM(FNAME))//' ?')) THEN
            tFileHandle = 10
            OPEN(UNIT=tFileHandle,FILE=FNAME(1:LEN_TRIM(FNAME)),ERR=999)
            WRITE(tFileHandle,'(F9.5)',ERR=999) TheWaveLength
            DO I = 1, BackupNOBS
              WRITE(tFileHandle,'(F6.3,X,F11.3,X,F12.5)',ERR=999) BackupXOBS(I), BackupYOBS(I), BackupEOBS(I)
            ENDDO
          ENDIF
        ENDIF
      ENDIF
      GOTO 10
  999 CALL ErrorMessage('Error accessing file '//FNAME(1:LEN_TRIM(FNAME)))
   10 CLOSE(tFileHandle)
      NoWavelengthInXYE = .FALSE.
      IF ( NearlyEqual(TheWaveLength,ALambda) ) RETURN
      ALambda = TheWaveLength
      CALL Upload_Wavelength

      END SUBROUTINE Set_Wavelength
!
!*****************************************************************************
!
      SUBROUTINE Set_AnodeMaterial(IRadSelection)

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: IRadSelection

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Data_Properties)
      CALL WDialogPutOption(IDF_Wavelength_Menu, IRadSelection)
      CALL SelectDASHDialog(IDD_PW_Page4)
      CALL WDialogPutOption(IDF_Wavelength_Menu, IRadSelection)
      CALL PopActiveWindowID

      END SUBROUTINE Set_AnodeMaterial
!
!*****************************************************************************
!
      SUBROUTINE Upload_Wavelength

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC'

      REAL, EXTERNAL :: FnWavelengthOfMenuOption
      INTEGER I, IRadSelection


      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( IN_BATCH ) RETURN

      CALL PushActiveWindowID
! This is the right place to update the maximum resolution (even if it's not necessary)
! In principle, set resolution so as to truncate after DefaultMaxResolution.
! However, if truncation resolution not attainable with current data range / wavelength,
! adjust the setting of the maximum resolution to maximum possible.
      CALL Update_TruncationLimits
      CALL SelectDASHDialog(IDD_Data_Properties)
      CALL WDialogPutReal(IDF_wavelength1, ALambda, '(F10.5)')
      CALL SelectDASHDialog(IDD_PW_Page4)
      CALL WDialogPutReal(IDF_wavelength1, ALambda, '(F10.5)')
      CALL SelectDASHDialog(IDD_Index_Preparation)
      CALL WDialogPutReal(IDF_wavelength1, ALambda, '(F10.5)')
! Now add in a test: if lab data, and wavelength close to known material,
! set anode material in Winteracter menus. Otherwise, anode is unknown.
      IF (JRadOption .EQ. 1) THEN ! X-ray lab data
! Initialise anode material to unknown
        IRadSelection = 1 ! <...> in the Winteracter menu
        DO I = 2, 6
          IF (ABS(ALambda - FnWavelengthOfMenuOption(I)) .LT. 0.0003) IRadSelection = I
        ENDDO
        CALL Set_AnodeMaterial(IRadSelection)
      ENDIF
      CALL CheckIfWeCanDoAPawleyRefinement
      CALL PopActiveWindowID

      END SUBROUTINE Upload_Wavelength
!
!*****************************************************************************
!
      SUBROUTINE Upload_Positions

      USE WINTERACTER
      USE DRUID_HEADER 
      USE REFVAR

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      REAL              PF_FWHM,                    PF_IntBreadth
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT),         &
                        PF_FWHM(MAX_NPFR),          PF_IntBreadth(MAX_NPFR)

      REAL              PkFnVal,                      PkFnEsd,                      &
                        PkFnCal,                                                    &
                        PkAreaVal,                    PkAreaEsd,                    &
                        PkPosVal,                     PkPosEsd,                     &
                        PkPosAv
      COMMON /PEAKFIT2/ PkFnVal(MPkDes,Max_NPFR),     PkFnEsd(MPkDes,Max_NPFR),     &
                        PkFnCal(MPkDes,Max_NPFR),                                   &
                        PkAreaVal(MAX_NPPR,MAX_NPFR), PkAreaEsd(MAX_NPPR,MAX_NPFR), &
                        PkPosVal(MAX_NPPR,MAX_NPFR),  PkPosEsd(MAX_NPPR,MAX_NPFR),  &
                        PkPosAv(MAX_NPFR)

      INTEGER           NTPeak
      REAL              AllPkPosVal,         AllPkPosEsd
      REAL              AllPkAreaVal
      REAL              PkProb
      INTEGER           IOrdTem
      INTEGER           IHPk
      COMMON /ALLPEAKS/ NTPeak,                                                  &
                        AllPkPosVal(MTPeak), AllPkPosEsd(MTPeak),                &
                        AllPkAreaVal(MTPeak),                                    &
                        PkProb(MTPeak),                                          &
                        IOrdTem(MTPeak),                                         &
                        IHPk(3,MTPeak)

!      REAL, EXTERNAL :: TwoTheta2dSpacing     
      REAL    PkArgK(MTPeak), PkTicDif(MTPeak)
      REAL    TwoThetaDiff, AbsTwoThetaDiff
      INTEGER IArgK(MTPeak)
      INTEGER J, I, II, IR, IR1, IA, IRef1, IRef2, iOrd, iTem
      REAL    xnew, anew, SigmDif, PfTDMin, PfTDMax, DifTem, ProbTot, ProbTop, DifMin
      REAL    DifMinSq, ArgBot, ArgTop, ProbAdd


      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( .NOT. IN_BATCH ) THEN
        CALL PushActiveWindowID
        CALL SelectDASHDialog(IDD_Peak_Positions)
        CALL WDialogClearField(IDF_Peak_Positions_Grid)
      ENDIF

      NTPeak = 0
! Loop over all hatched areas. Per area, count all peaks that the user has indicated to be present.
! Store all peak positions thus found in one flat array: AllPkPosVal
      IF (NumPeakFitRange .GT. 0) THEN
        DO J = 1, NumPeakFitRange
          IF (RangeFitYN(J)) THEN
            DO I = 1, NumInPFR(J)
              CALL INC(NTPeak)
              AllPkPosVal(NTPeak) = PkPosVal(I,J)
              AllPkPosEsd(NTPeak) = PkPosEsd(I,J)
              AllPkAreaVal(NTPeak) = PkAreaVal(I,J)
            ENDDO
          ENDIF
        ENDDO
      ENDIF
      IF (NTPeak .EQ. 0) THEN
! Winteracter doesn't seem able to cope with setting the number of rows in a grid to zero,
! so instead I set it such that it fills the screen but doesn't allow scrolling down.
        IF ( .NOT. IN_BATCH ) THEN
          CALL WGridRows(IDF_Peak_Positions_Grid,4)
          CALL WDialogFieldState(ID_Index_Output,Disabled)
          CALL PopActiveWindowID
        ENDIF
        RETURN
      ENDIF
      CALL SORT_REAL(AllPkPosVal,IOrdTem,NTPeak)
! IOrdTem now contains an ordered list of pointers into AllPkPosVal
! JvdS @@ why not order the list itself?
      IF (NumOfRef .NE. 0) THEN
! Let's find the closest peaks and their distribution around the observed peak positions
        IR1 = 1 ! Pointer into list of reflections
        DO I = 1, NTPeak
          iOrd = IOrdTem(I) ! IOrd is now a pointer into AllPkPosVal to the next peak position
          TwoThetaDiff = RefArgK(IR1) - AllPkPosVal(iOrd)
          AbsTwoThetaDiff = ABS(TwoThetaDiff)
          iTem = IR1
          DO IR = IR1, NumOfRef
            xnew = RefArgK(IR) - AllPkPosVal(iOrd)
            anew = ABS(xnew)
            IF (anew .LE. AbsTwoThetaDiff) THEN
              iTem = IR
              AbsTwoThetaDiff = anew
              TwoThetaDiff = xnew
            ENDIF
            IF (xnew .GT. 0.0) THEN
              IR1 = MAX(1,IR-1)
! As both the peaks and the reflections are ordered, the position of the next peak can only be greater
              GOTO 20
            ENDIF
          ENDDO
 20       PkTicDif(I) = TwoThetaDiff
          DO II = 1, 3
            IHPk(II,I) = iHKL(II,iTem)
          ENDDO
          PkArgK(I) = RefArgK(iTem)
          IArgK(I) = iTem
        ENDDO
        IF (NTPeak .EQ. 1) THEN
          SigmDif = 0.01
        ELSE
          PfTDMin = PkTicDif(1)
          PfTDMax = PkTicDif(1)
          DO II = 1, NTPeak
            PfTDMin = MIN(PfTDMin,PkTicDif(II))
            PfTDMax = MAX(PfTDMax,PkTicDif(II))
          ENDDO
          SigmDif = 0.2886751345948*ABS(PfTDMax-PfTDMin)
        ENDIF
        DO I = 1, NTPeak
          iOrd = IOrdTem(I)
          IA = IArgK(I)
          IRef1 = MAX(1,IA-5)
          IRef2 = MIN(NumOfRef,IA+5)
          ProbTot = 0.0
          ProbTop = 0.0
          DifMin = ABS(AllPkPosVal(iOrd)-RefArgK(IA))
          DifMinSq = DifMin**2
          ArgBot = 0.5/(SigmDif**2+AllPkPosEsd(iOrd)**2)
          DO IR = IRef1, IRef2
            ArgTop = (AllPkPosVal(iOrd)-RefArgK(IR))**2
            ProbAdd = EXP(-ArgTop*ArgBot)
            IF (ABS(ArgTop-DifMinSq).LT.1.0E-10) THEN
              ProbTop = ProbTop + ProbAdd
            ENDIF
            ProbTot = ProbTot + ProbAdd
          ENDDO
          PkProb(iOrd) = ProbTop / ProbTot
        ENDDO
      ENDIF
! Write out all the peak positions in an ordered list ...

      IF ( IN_BATCH ) RETURN
      CALL WGridRows(IDF_Peak_Positions_Grid,NTPeak)
      CALL WDialogFieldState(ID_Index_Output,Enabled)

      DO I = 1, NTPeak
        iOrd = IOrdTem(I)
        CALL WGridPutCellReal(IDF_Peak_Positions_Grid, 1, I, AllPkPosVal(iOrd), '(F12.4)')
        CALL WGridPutCellReal(IDF_Peak_Positions_Grid, 2, I, AllPkPosEsd(iOrd), '(F12.4)')
        CALL WGridPutCellReal(IDF_Peak_Positions_Grid, 3, I, PkArgK(I), '(F12.4)')
        DifTem = AllPkPosVal(iOrd) - PkArgK(I)

      !  DifTem = AllPkAreaVal(iOrd)
      !  DifTem = TwoTheta2dSpacing(AllPkPosVal(iOrd))

        CALL WGridPutCellReal(IDF_Peak_Positions_Grid, 4, I, DifTem, '(F12.4)')
        CALL WGridPutCellInteger(IDF_Peak_Positions_Grid, 5, I, IHPk(1,I))
        CALL WGridPutCellInteger(IDF_Peak_Positions_Grid, 6, I, IHPk(2,I))
        CALL WGridPutCellInteger(IDF_Peak_Positions_Grid, 7, I, IHPk(3,I))
        CALL WGridPutCellReal(IDF_Peak_Positions_Grid, 8, I, PkProb(iOrd), '(F8.3)')
      ENDDO
      CALL PopActiveWindowID

      END SUBROUTINE Upload_Positions
!
!*****************************************************************************
!
! Subroutine to set the state of the global variable JRadOption to either synchrotron or lab data.
! This is updated in the main dialogue window and in the wizard.

      SUBROUTINE Upload_Source

      USE WINTERACTER
      USE DRUID_HEADER 
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      INCLUDE 'GLBVAR.INC' ! Contains JRadOption

      INTEGER WindowNr
      INTEGER NotDisabled

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF  ( IN_BATCH ) RETURN

      CALL PushActiveWindowID
      IF ( PastPawley ) THEN
        NotDisabled = DialogReadOnly
      ELSE
        NotDisabled = Enabled
      ENDIF
      DO WindowNr = 1, 2
        SELECT CASE (WindowNr)
          CASE (1) 
            CALL SelectDASHDialog(IDD_PW_Page4)
            IF ( iRietveldMethod .NE. INTERNAL_RB ) NotDisabled = Enabled
          CASE (2) 
            CALL SelectDASHDialog(IDD_Data_Properties)
            IF ( iRietveldMethod .NE. INTERNAL_RB ) NotDisabled = DialogReadOnly
        END SELECT
        SELECT CASE (JRadOption)
          CASE (1) ! Lab X-ray
            CALL WDialogFieldState(IDF_CW_group, NotDisabled)
            CALL WDialogFieldState(IDF_radiation_label, NotDisabled)
            CALL WDialogFieldState(IDF_wavelength1, NotDisabled)
            CALL WDialogFieldState(IDF_Wavelength_Menu, NotDisabled)
            CALL WDialogFieldState(IDF_TOF_group, Disabled)
            CALL WDialogFieldState(IDF_Flight_Path_Label, Disabled)
            CALL WDialogFieldState(IDF_flight_path, Disabled)
            CALL WDialogFieldState(IDF_2theta_label, Disabled)
            CALL WDialogFieldState(IDF_2theta0, Disabled)
            CALL WDialogPutRadioButton(IDF_LabX_Source)
          CASE (2, 3) ! Synchrotron X-ray & CW neutron  
            CALL WDialogFieldState(IDF_CW_group, NotDisabled)
            CALL WDialogFieldState(IDF_radiation_label, NotDisabled)
            CALL WDialogFieldState(IDF_Wavelength_Menu, Disabled)
            CALL WDialogFieldState(IDF_wavelength1, NotDisabled)
            CALL WDialogFieldState(IDF_TOF_group, Disabled)
            CALL WDialogFieldState(IDF_Flight_Path_Label, Disabled)
            CALL WDialogFieldState(IDF_flight_path, Disabled)
            CALL WDialogFieldState(IDF_2theta_label, Disabled)
            CALL WDialogFieldState(IDF_2theta0, Disabled)
            IF (JRadOption .EQ. 2) THEN
              CALL WDialogPutRadioButton(IDF_SynX_Source)
            ELSE
              CALL WDialogPutRadioButton(IDF_CWN_Source)
            ENDIF
          CASE (4) ! TOF neutron
            CALL WDialogFieldState(IDF_CW_group, Disabled)
            CALL WDialogFieldState(IDF_radiation_label, Disabled)
            CALL WDialogFieldState(IDF_Wavelength_Menu, Disabled)
            CALL WDialogFieldState(IDF_wavelength1, Disabled)
            CALL WDialogFieldState(IDF_TOF_group, NotDisabled)
            CALL WDialogFieldState(IDF_Flight_Path_Label, NotDisabled)
            CALL WDialogFieldState(IDF_flight_path, NotDisabled)
            CALL WDialogFieldState(IDF_2theta_label, NotDisabled)
            CALL WDialogFieldState(IDF_2theta0, NotDisabled)
            CALL WDialogPutRadioButton(IDF_TOF_source)
        END SELECT
      ENDDO
      CALL PopActiveWindowID

      END SUBROUTINE Upload_Source
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

      INCLUDE 'lattice.inc'

      LOGICAL, EXTERNAL :: FnUnitCellOK

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

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

      IF ( IN_BATCH ) RETURN

      CALL PushActiveWindowID
! Update all windows so that they show the contents of the global variables.
! This is in the cell parameters tab, in the two wizard windows, and in the peak positions tab.
      CALL Upload_Cell_Constants
      CALL SelectDASHDialog(IDD_PW_Page1)
! Enable/disable the wizard next button
      CALL WDialogFieldStateLogical(IDNEXT, FnUnitCellOK())
!!Enable/disable the space group determination button
      CALL WDialogFieldStateLogical(IDF_SGDet, FnUnitCellOK())
! Enable/disable the single xtal wizard next button
      CALL SelectDASHDialog(IDD_SX_Page1)
      CALL WDialogFieldStateLogical(IDNEXT, FnUnitCellOK())
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
! INPUT   : The number of that space group in the space group menu
!
! RETURNS : The number of a space group (1 - 530)
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
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

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
      ENDDO
      IF ((NumberSGTable .LT. LPosSg(LatBrav)) .OR. (NumberSGTable .GE. LPosSg(LatBrav+1))) THEN
! Current space group not possible in this crystal system: so update the space group to the first
! in the list of possibilities.
        NumberSGTable = LPosSG(LatBrav)
        ISPosSG = 1
      ELSE
        ISPosSG = NumberSGTable - LPosSG(LatBrav) + 1
      ENDIF
      CALL SelectDASHDialog(IDD_Crystal_Symmetry)
      CALL WDialogPutMenu(IDF_Space_Group_Menu, SGHMaBrStr, NumBrSG, ISPosSG)
      CALL SelectDASHDialog(IDD_PW_Page1)
      CALL WDialogPutMenu(IDF_Space_Group_Menu, SGHMaBrStr, NumBrSG, ISPosSG)
      CALL SelectDASHDialog(IDD_SX_Page1)
      CALL WDialogPutMenu(IDF_Space_Group_Menu, SGHMaBrStr, NumBrSG, ISPosSG)
      CALL PopActiveWindowID

      END SUBROUTINE SetSpaceGroupMenu
!
!*****************************************************************************
!
      SUBROUTINE Download_SpaceGroup(IUploadFrom)

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: IUploadFrom

      INCLUDE 'Lattice.inc'

      INTEGER, EXTERNAL :: SGNrMenu2Table
      INTEGER ISPosSG

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IUploadFrom)
      CALL DASHWDialogGetMenu(IDF_Space_Group_Menu, ISPosSG)
      NumberSGTable = SGNrMenu2Table(ISPosSG)
      CALL SelectDASHDialog(IDD_Crystal_Symmetry)
      CALL WDialogPutOption(IDF_Space_Group_Menu, ISPosSG)
      CALL SelectDASHDialog(IDD_PW_Page1)
      CALL WDialogPutOption(IDF_Space_Group_Menu, ISPosSG)
      CALL SelectDASHDialog(IDD_SX_Page1)
      CALL WDialogPutOption(IDF_Space_Group_Menu, ISPosSG)
      CALL PopActiveWindowID

      END SUBROUTINE Download_SpaceGroup
!
!*****************************************************************************
!
      SUBROUTINE Upload_CrystalSystem

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      CellParConstrained = .FALSE.
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
          CellPar(4) =  90.0
          CellPar(5) =  90.0
          CellPar(6) =  90.0
          CellParConstrained = .TRUE.
          CellParConstrained(1) = .FALSE.
          CellParConstrained(3) = .FALSE.
        CASE ( 7, 9) ! Trigonal / Hexagonal
          CellPar(4) =  90.0
          CellPar(5) =  90.0
          CellPar(6) = 120.0
          CellParConstrained = .TRUE.
          CellParConstrained(1) = .FALSE.
          CellParConstrained(3) = .FALSE.
        CASE ( 8) ! Rhombohedral
          CellParConstrained = .TRUE.
          CellParConstrained(1) = .FALSE.
          CellParConstrained(4) = .FALSE.
        CASE (10) ! Cubic
          CellPar(4) =  90.0
          CellPar(5) =  90.0
          CellPar(6) =  90.0
          CellParConstrained = .TRUE.
          CellParConstrained(1) = .FALSE.
      END SELECT

      IF ( .NOT. in_batch ) &
        CALL PushActiveWindowID
      CALL UpdateCell

      IF ( in_batch ) &
         RETURN

      CALL SelectDASHDialog(IDD_Crystal_Symmetry)
      CALL WDialogPutOption(IDF_Crystal_System_Menu, LatBrav)
      CALL SelectDASHDialog(IDD_PW_Page1)
      CALL WDialogPutOption(IDF_Crystal_System_Menu, LatBrav)
      CALL SelectDASHDialog(IDD_SX_Page1)
      CALL WDialogPutOption(IDF_Crystal_System_Menu, LatBrav)
      CALL SetSpaceGroupMenu
      CALL PopActiveWindowID

      END SUBROUTINE Upload_CrystalSystem
!
!*****************************************************************************
!
      SUBROUTINE SetWavelengthToSelection(Iselection)

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: Iselection

      REAL, EXTERNAL :: FnWavelengthOfMenuOption

! Winteracter menu:
!     1 = <...>
!     2 = Cu      <==  DEFAULT
!     3 = Mo
!     4 = Co
!     5 = Cr
!     6 = Fe

      IF ((Iselection .GE. 2) .AND. (Iselection .LE. 6)) THEN
        CALL Set_Wavelength(FnWavelengthOfMenuOption(Iselection))
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
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheOption

      REAL, EXTERNAL :: WavelengthOf

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
! Subroutine for controlling the configuration of the menus and tool buttons in DASH
      SUBROUTINE SetModeMenuState(PeakOn, PawleyOn)
! If PeakOn is positive then peak fitting will be enabled
! If PawleyOn is positive then Pawley fitting will be enabled
! If PeakOn is negative then peak fitting will be disabled
! If PawleyOn is negative then Pawley fitting will be disabled
! If PeakOn is zero then the peak fitting state is left as is
! If PawleyOn is zero then the Pawley fitting state is left as is

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: PeakOn, PawleyOn

      LOGICAL         InSA
      COMMON /SADATA/ InSA

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      IF ( in_batch ) &
        RETURN

      IF (PeakOn .GT. 0) THEN
        CALL WMenuSetState(ID_Peak_Fitting_Mode, ItemEnabled, WintOn)
      ELSE IF (PeakOn .LT. 0) THEN
        CALL WMenuSetState(ID_Peak_Fitting_Mode, ItemEnabled, WintOff)
      ENDIF
      IF (PawleyOn .GT. 0) THEN
        CALL WMenuSetState(ID_Pawley_Refinement_Mode, ItemEnabled, WintOn)
      ELSE IF (PawleyOn .LT. 0) THEN
        CALL WMenuSetState(ID_Pawley_Refinement_Mode, ItemEnabled, WintOff)
      ENDIF
      IF (InSA) THEN
        CALL WMenuSetState(IDB_AnalyseSolutions, ItemEnabled, WintOff)
      ELSE
        CALL WMenuSetState(IDB_AnalyseSolutions, ItemEnabled, WintOn)
      ENDIF

      END SUBROUTINE SetModeMenuState
!
!*****************************************************************************
!
      SUBROUTINE SelectMode(TheMode)
!
! This subroutine selects "peak fitting" / "Pawley refinement" / "structure solution" mode,
! ensuring that exactly one mode is active at all times and
! updating the current mode in the menu and in the toolbar.
!
      USE WINTERACTER
      USE DRUID_HEADER
      
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheMode

      INCLUDE 'GLBVAR.INC'

      CALL PushActiveWindowID
      IF (WMenuGetState(TheMode, ItemEnabled) .EQ. Disabled) THEN
        CALL DebugErrorMessage('Programming error: requested menu item is greyed out')
        CALL WMenuSetState(TheMode, ItemEnabled, Enabled)
      ENDIF
! Update the status bar
      IF (TheMode .EQ. ID_Structure_Solution_Mode) THEN
        CALL SelectDASHDialog(IDD_Polyfitter_Wizard_01)
        CALL WDialogPutRadioButton(IDF_PW_Option3)
      ENDIF
! Update the menu + the toolbar
      CALL WMenuSetState(IDCurrent_Cursor_mode, ItemChecked, WintOff)
      IDCurrent_Cursor_mode = TheMode
      CALL WMenuSetState(IDCurrent_Cursor_mode, ItemChecked, WintOn)
      CALL PopActiveWindowID
      
      END SUBROUTINE SelectMode
!
!*****************************************************************************
!
      SUBROUTINE FitPeaks

      USE WINTERACTER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      REAL              PF_FWHM,                    PF_IntBreadth
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT),         &
                        PF_FWHM(MAX_NPFR),          PF_IntBreadth(MAX_NPFR)

      INTEGER tCurrentRange

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
      CALL UpdatePeaksButtonsStates
! Disable Pawley refinement button and 'Next >' button in Wizard window
      CALL CheckIfWeCanDoAPawleyRefinement

      END SUBROUTINE FitPeaks
!
!*****************************************************************************
!
      SUBROUTINE CheckIfPeaksFitted
!
! This subroutine:
! 1. checks if all peak-fit ranges have been fitted
! 2. if not:
!    a. remaining peaks are fitted (just like the fit-peak-range button)
!    b. the user is shown an info window telling them what happened
!    (this is deliberately not a confirm, because that would add complications)
!
      
      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'

      REAL              XPF_Range
      LOGICAL                                       RangeFitYN
      INTEGER           IPF_Lo,                     IPF_Hi
      INTEGER           NumPeakFitRange,            CurrentRange
      INTEGER           IPF_Range
      INTEGER           NumInPFR
      REAL              XPF_Pos,                    YPF_Pos
      INTEGER           IPF_RPt
      REAL              XPeakFit,                   YPeakFit
      REAL              PF_FWHM,                    PF_IntBreadth
      COMMON /PEAKFIT1/ XPF_Range(2,MAX_NPFR),      RangeFitYN(MAX_NPFR),        &
                        IPF_Lo(MAX_NPFR),           IPF_Hi(MAX_NPFR),            &
                        NumPeakFitRange,            CurrentRange,                &
                        IPF_Range(MAX_NPFR),                                     &
                        NumInPFR(MAX_NPFR),                                      & 
                        XPF_Pos(MAX_NPPR,MAX_NPFR), YPF_Pos(MAX_NPPR,MAX_NPFR),  &
                        IPF_RPt(MAX_NPFR),                                       &
                        XPeakFit(MAX_FITPT),        YPeakFit(MAX_FITPT),         &
                        PF_FWHM(MAX_NPFR),          PF_IntBreadth(MAX_NPFR)

      LOGICAL all_ranges_fitted
      INTEGER tCurrentRange

      all_ranges_fitted = .TRUE.
      DO tCurrentRange = 1, NumPeakFitRange
        IF (.NOT. RangeFitYN(tCurrentRange)) all_ranges_fitted = .FALSE.
      ENDDO
      IF (.NOT. all_ranges_fitted) THEN
        CALL FitPeaks
        CALL InfoMessage("Some peaks had not been fitted yet."//CHAR(13)//"They have been fitted automatically.")
      ENDIF
      
      END SUBROUTINE CheckIfPeaksFitted
!
!*****************************************************************************
!
      SUBROUTINE RealInt2NMoves(tReal, tInt, nmoves)

      IMPLICIT NONE

      REAL,    INTENT (IN   ) :: tReal
      INTEGER, INTENT (IN   ) :: tInt
      INTEGER, INTENT (  OUT) :: nmoves

      REAL    MaxMoves1, tMaxMoves
      INTEGER MaxMoves2

      MaxMoves1 = tReal
      MaxMoves2 = tInt
      IF (MaxMoves1 .LT.   0.001) &
        MaxMoves1 =   0.001
      IF (MaxMoves1 .GT. 100.0  ) &
        MaxMoves1 = 100.0
      IF (MaxMoves2 .LT. 1) &
        MaxMoves2 = 1
      IF (MaxMoves2 .GT. 8) &
        MaxMoves2 = 8
      tMaxMoves = MaxMoves1 * (10**FLOAT(MaxMoves2))
      IF (tMaxMoves .LT. 10.0) &
        tMaxMoves = 10.0
      IF (tMaxMoves .GT.  2.0E9) &
        tMaxMoves = 2.0E9
      nmoves = NINT(tMaxMoves)

      END SUBROUTINE RealInt2NMoves
!
!*****************************************************************************
!
      SUBROUTINE NMoves2RealInt(nmoves, tReal, tInt)

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: nmoves
      REAL,    INTENT (  OUT) :: tReal
      INTEGER, INTENT (  OUT) :: tInt

      CHARACTER*20, EXTERNAL :: Integer2String
      CHARACTER*20 tString

      tString = Integer2String(nmoves)
      tInt = LEN_TRIM(tString) 
      tInt = tInt - 1
      tReal = FLOAT(nmoves)/FLOAT(10**tInt)

      END SUBROUTINE NMoves2RealInt
!
!*****************************************************************************
!

      SUBROUTINE LogRetrieval(IFIELD)

      USE WINTERACTER
      INTEGER IFIELD

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch
      CHARACTER*20, EXTERNAL :: Integer2String

      IF ( IN_BATCH ) THEN
         CALL DebugErrorMessage('Dialog Access in Batch Mode: Field number '//Integer2String(IFIELD) )
      ENDIF

      END SUBROUTINE

      SUBROUTINE DASHWDialogGetInteger(IFIELD, IVALUE)


      INTEGER, INTENT (IN   ) :: IFIELD
      INTEGER, INTENT (INOUT) :: IVALUE

      CALL LogRetrieval(IFIELD)

      CALL WDialogGetInteger(IFIELD,IVALUE)

      END SUBROUTINE

      SUBROUTINE DASHWDialogGetRadioButton(IFIELD, ISET)

      USE WINTERACTER

      INTEGER, INTENT (IN   ) :: IFIELD
      INTEGER, INTENT (INOUT) :: ISET

      CALL LogRetrieval(IFIELD)

      CALL WDialogGetRadioButton(IFIELD,ISET)

      END SUBROUTINE


      SUBROUTINE DASHWDialogGetCheckBox(IFIELD, ISTATE)

      USE WINTERACTER

      INTEGER, INTENT (IN   ) :: IFIELD
      INTEGER, INTENT (INOUT) :: ISTATE

      CALL LogRetrieval(IFIELD)

      CALL WDialogGetCheckBox(IFIELD,ISTATE)

      END SUBROUTINE


      SUBROUTINE DASHWDialogGetMenu(IFIELD, IOPTION)
      
      USE WINTERACTER

      INTEGER,             INTENT (IN   ) :: IFIELD
      INTEGER,             INTENT (INOUT) :: IOPTION
!      CHARACTER*(*), OPTIONAL, INTENT (INOUT) :: CVALUE

      CALL LogRetrieval(IFIELD)

      CALL WDialogGetMenu(IFIELD,IOPTION)

      END SUBROUTINE

      SUBROUTINE DASHWDialogGetReal(IFIELD, RVALUE)

      USE WINTERACTER

      INTEGER, INTENT (IN   ) :: IFIELD
      REAL,    INTENT (INOUT) :: RVALUE

      CALL LogRetrieval(IFIELD)

      CALL WDialogGetReal(IFIELD,RVALUE)

      END SUBROUTINE


      SUBROUTINE DASHWDialogGetDouble(IFIELD, DVALUE)

      USE WINTERACTER

      INTEGER,            INTENT (IN   ) :: IFIELD
      DOUBLE PRECISION    , INTENT (INOUT) :: DVALUE

      CALL LogRetrieval(IFIELD)

      CALL WDialogGetDouble(IFIELD,DVALUE)

      END SUBROUTINE


      SUBROUTINE DASHWDialogGetString(IFIELD, CVALUE)

      USE WINTERACTER

      INTEGER,   INTENT (IN   ) :: IFIELD
      CHARACTER*(*), INTENT (INOUT) :: CVALUE

      INTEGER IERROR

      CALL LogRetrieval(IFIELD)
      
      IERROR = InfoError(1)
      CVALUE = ''
      CALL WDialogGetString(IFIELD,CVALUE)

      IERROR = InfoError(1)
      IF ( IERROR .GT. 0 ) THEN
         CVALUE = ''
      ENDIF

      END SUBROUTINE

      SUBROUTINE DASHWGridGetCellCheckBox(IFIELD,ICOL,IROW,IVALUE)

      USE WINTERACTER
      INTEGER,             INTENT (IN   ) :: IFIELD
      INTEGER,             INTENT (IN   ) :: IROW
      INTEGER,             INTENT (IN   ) :: ICOL
      INTEGER,             INTENT (INOUT) :: IVALUE

      CALL LogRetrieval(IFIELD)

      CALL WGridGetCellCheckBox(IFIELD,ICOL,IROW,IVALUE)
      
      END SUBROUTINE

      SUBROUTINE DASHWGridGetCheckBox(IFIELD,ICOL,IVALUES,NVALUES)

      USE WINTERACTER
      INTEGER,             INTENT (IN   ) :: IFIELD
      INTEGER,             INTENT (IN   ) :: ICOL
      INTEGER,             INTENT (INOUT) :: IVALUES(*)
      INTEGER,             INTENT (INOUT) :: NVALUES

      CALL LogRetrieval(IFIELD)

      CALL WGridGetCheckBox(IFIELD,ICOL,IVALUES,NVALUES)
      
      END SUBROUTINE

      SUBROUTINE DASHWGridGetCellReal(IFIELD,ICOL,IROW,RVALUE)

      USE WINTERACTER
      INTEGER,             INTENT (IN   ) :: IFIELD
      INTEGER,             INTENT (IN   ) :: IROW
      INTEGER,             INTENT (IN   ) :: ICOL
      REAL,                INTENT (INOUT) :: RVALUE

      CALL LogRetrieval(IFIELD)

      CALL WGridGetCellReal(IFIELD,ICOL,IROW,RVALUE)
      
      END SUBROUTINE

      SUBROUTINE DASHWGridGetCellString(IFIELD,ICOL,IROW,CVALUE)

      USE WINTERACTER
      INTEGER,             INTENT (IN   ) :: IFIELD
      INTEGER,             INTENT (IN   ) :: IROW
      INTEGER,             INTENT (IN   ) :: ICOL
      CHARACTER*(*),       INTENT (INOUT) :: CVALUE

      CALL LogRetrieval(IFIELD)

      CALL WGridGetCellString(IFIELD,ICOL,IROW,CVALUE)
      
      END SUBROUTINE
