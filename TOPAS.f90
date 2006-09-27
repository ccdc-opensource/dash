!
!*****************************************************************************
!
      SUBROUTINE CopyPattern2Backup

      USE DRUID_HEADER
      USE WINTERACTER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                   XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS, XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      REAL               TALAMBDA
      INTEGER                      TARADIATION, TANOBS
      REAL                                              TAXOBS,       TAYOBS,       TAEOBS
      COMMON /TAPROFOBS/ TALAMBDA, TARADIATION, TANOBS, TAXOBS(MOBS), TAYOBS(MOBS), TAEOBS(MOBS)

      ! Perhaps this pair of functions should also set and reset the For_TOPAS variable
      TANOBS = NOBS
      TAXOBS(1:NOBS) = XOBS(1:NOBS)
      TAYOBS(1:NOBS) = YOBS(1:NOBS)
      TAEOBS(1:NOBS) = EOBS(1:NOBS)
      TALAMBDA = ALambda
      TARADIATION = JRadOption
      ! Uncheck the "Truncate pattern at end" checkbox (but we don't store its current state)
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page5)
      CALL WDialogPutCheckBoxLogical(IDF_TruncateEndYN, .FALSE.)
      CALL WDialogFieldState(IDF_Max2Theta, Disabled)
      CALL WDialogFieldState(IDF_MaxResolution, Disabled)
      CALL WDialogFieldState(IDB_Convert, Disabled)
      CALL PopActiveWindowID

      END SUBROUTINE CopyPattern2Backup
!
!*****************************************************************************
!
      SUBROUTINE CopyBackup2Pattern

      USE DRUID_HEADER
      USE WINTERACTER

      IMPLICIT NONE

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NOBS
      REAL                   XOBS,       YOBS,       EOBS
      COMMON /PROFOBS/ NOBS, XOBS(MOBS), YOBS(MOBS), EOBS(MOBS)

      REAL               TALAMBDA
      INTEGER                      TARADIATION, TANOBS
      REAL                                              TAXOBS,       TAYOBS,       TAEOBS
      COMMON /TAPROFOBS/ TALAMBDA, TARADIATION, TANOBS, TAXOBS(MOBS), TAYOBS(MOBS), TAEOBS(MOBS)

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      NOBS = TANOBS
      XOBS(1:NOBS) = TAXOBS(1:NOBS)
      YOBS(1:NOBS) = TAYOBS(1:NOBS)
      EOBS(1:NOBS) = TAEOBS(1:NOBS)
      ALambda = TALAMBDA
      JRadOption = TARADIATION
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_PW_Page5)
      CALL WDialogPutCheckBoxLogical(IDF_TruncateEndYN, .TRUE.)
      CALL WDialogFieldState(IDF_Max2Theta, Enabled)
      CALL WDialogFieldState(IDF_MaxResolution, Enabled)
      CALL WDialogFieldState(IDB_Convert, Enabled)
      CALL PopActiveWindowID
      ! Must also restore Rebin_Profile
      LBIN = 1
      CALL Rebin_Profile()
      CALL Profile_Plot

      END SUBROUTINE CopyBackup2Pattern
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardTOPAS

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_RR_TOPAS)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_SAW_Page6a)
            CASE (IDCANCEL, IDCLOSE)
              IF ( For_TOPAS ) THEN
                CALL CopyBackup2Pattern()
                For_TOPAS = .FALSE.
              ENDIF
              CALL EndWizardPastPawley
            CASE (IDB_UsePrevious)
              TOPAS_stage = 2
              CALL WDialogSelect(IDD_SAW_Page7)
              CALL WDialogPutCheckBoxLogical(IDC_UseDASHRecommendation, .TRUE.)
              CALL UpdateTOPASCheckBoxes()
              CALL WizardWindowShow(IDD_SAW_Page7)
            CASE (IDB_Write_Pawley)
              For_TOPAS = .TRUE.
              CALL WizardWindowShow(IDD_PW_Page3)
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithWizardTOPAS
!
!*****************************************************************************
! 
      INTEGER FUNCTION WriteTOPASFilePawley(tFileName)

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: tFileName

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER hFileTOPAS, hTempFile
      INTEGER iLen, i
      CHARACTER*(1) tChar
      CHARACTER*(MaxPathLength) temp_file_name
      CHARACTER*(40) space_group_str
      INTEGER tStrLen

      ! The way this code has curently been written, this routine can only be called
      ! from one of the Wizard windows as part of a "For_TOPAS" Rietveld refinement
      ! Initialise to failure
      WriteTOPASFilePawley = 1
      hFileTOPAS = 116
      OPEN(UNIT=hFileTOPAS,FILE=tFileName,STATUS='unknown',ERR=999)
      WRITE(hFileTOPAS, '(A)', ERR=999) 'penalties_weighting_K1 5'
      WRITE(hFileTOPAS, '(A)', ERR=999) 'r_exp 1.0'
      WRITE(hFileTOPAS, '(A)', ERR=999) 'r_exp_dash 1.0'
      WRITE(hFileTOPAS, '(A)', ERR=999) 'r_wp 1.0'
      WRITE(hFileTOPAS, '(A)', ERR=999) 'r_wp_dash 1.0'
      WRITE(hFileTOPAS, '(A)', ERR=999) 'r_p 1.0'
      WRITE(hFileTOPAS, '(A)', ERR=999) 'r_p_dash 1.0'
      WRITE(hFileTOPAS, '(A)', ERR=999) 'weighted_Durbin_Watson 1.0'
      WRITE(hFileTOPAS, '(A)', ERR=999) 'gof 1.0'
      ! Next line contains the file name of the file containing the diffraction data
      ! TOPAS can read .raw files, but not if they contain multiple ranges, and
      ! TOPAS can read .xye files, but not if they contain a wavelength on the first line
      ! We need to write out the data to a temporary file
      ! This also has the advantage that the user can choose a different file
      ! from the one they used for structure solution.
      hTempFile = 117
      CALL IOsDirName(temp_file_name)
      iLen = LEN_TRIM(temp_file_name)
      temp_file_name = temp_file_name(1:iLen)//DIRSPACER//'temp.xye'
      iLen = iLen + 9
      OPEN(UNIT=hTempFile,FILE=temp_file_name(1:iLen),STATUS='unknown',ERR=998)
      DO i = 1, NBIN
        WRITE(hTempFile, '(F10.5,1X,F12.4,1X,F12.5)', ERR=998) XBIN(i), YOBIN(i), EBIN(i)
      ENDDO
      CLOSE(hTempFile)
      WRITE(hFileTOPAS, '(A)', ERR=999) 'xdd "'//temp_file_name(1:iLen)//'"'
      WRITE(hFileTOPAS, '(A)', ERR=999) '  bkg @ 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0 100.0'
      WRITE(hFileTOPAS, '(A,F12.4)', ERR=999) '  start_X ', XBIN(1)
      WRITE(hFileTOPAS, '(A,F12.4)', ERR=999) '  finish_X ', XBIN(NBIN)
      ! We do not write out the zero-point error in case the user has decided to use a different data set
      WRITE(hFileTOPAS, '(A)', ERR=999) '  Zero_Error(@, 0.0)'
      IF ( JRadOption .EQ. 1 ) THEN
        WRITE(hFileTOPAS, '(A)', ERR=999) '  LP_Factor( 26)'
      ELSE IF ( JRadOption .EQ. 2 ) THEN
        WRITE(hFileTOPAS, '(A)', ERR=999) '  LP_Factor( 90)'
      ELSE
        CALL ErrorMessage('Unknown radiation type.')
      ENDIF
      WRITE(hFileTOPAS, '(A)', ERR=999) '  auto_sparse_CG'
      WRITE(hFileTOPAS, '(A)', ERR=999) '  Rp 217.5'
      WRITE(hFileTOPAS, '(A)', ERR=999) '  Rs 217.5'
      WRITE(hFileTOPAS, '(A)', ERR=999) '  Simple_Axial_Model(@, 9.0)'
      WRITE(hFileTOPAS, '(A)', ERR=999) '  lam'
      WRITE(hFileTOPAS, '(A)', ERR=999) '    ymin_on_ymax 0.001'
      WRITE(hFileTOPAS, '(A,F9.6)', ERR=999) '    la 1 lo ', ALambda
      WRITE(hFileTOPAS, '(A,F8.6)', ERR=999) '  x_calculation_step ', (XBIN(NBIN) - XBIN(1)) / (NBIN - 1)
      WRITE(hFileTOPAS, '(A)', ERR=999) '  hkl_Is'
      WRITE(hFileTOPAS, '(A)', ERR=999) '    CS_L(@, 200.00)'
      WRITE(hFileTOPAS, '(A)', ERR=999) '    CS_G(@, 200.00)'
      WRITE(hFileTOPAS, '(A)', ERR=999) '    Strain_G(@, 0.1)'
      WRITE(hFileTOPAS, '(A)', ERR=999) '    Strain_L(@, 0.01)'
      IF ( CellParConstrained(1) ) THEN
        tChar = ' '
      ELSE
        tChar = '@'
      ENDIF
      WRITE(hFileTOPAS, '(A,F10.5)', ERR=999) '    a  '//tChar//' ', CELLPAR(1)
      IF ( CellParConstrained(2) ) THEN
        tChar = ' '
      ELSE
        tChar = '@'
      ENDIF
      WRITE(hFileTOPAS, '(A,F10.5)', ERR=999) '    b  '//tChar//' ', CELLPAR(2)
      IF ( CellParConstrained(3) ) THEN
        tChar = ' '
      ELSE
        tChar = '@'
      ENDIF
      WRITE(hFileTOPAS, '(A,F10.5)', ERR=999) '    c  '//tChar//' ', CELLPAR(3)
      IF ( CellParConstrained(4) ) THEN
        tChar = ' '
      ELSE
        tChar = '@'
      ENDIF
      WRITE(hFileTOPAS, '(A,F10.5)', ERR=999) '    al '//tChar//' ', CELLPAR(4)
      IF ( CellParConstrained(5) ) THEN
        tChar = ' '
      ELSE
        tChar = '@'
      ENDIF
      WRITE(hFileTOPAS, '(A,F10.5)', ERR=999) '    be '//tChar//' ', CELLPAR(5)
      IF ( CellParConstrained(6) ) THEN
        tChar = ' '
      ELSE
        tChar = '@'
      ENDIF
      WRITE(hFileTOPAS, '(A,F10.5)', ERR=999) '    ga '//tChar//' ', CELLPAR(6)
!      WRITE(hFileTOPAS, '(A,A)', ERR=999) 'phase_name ', 

      ! By writing the space group name last, the extra information on hkl's and intensities
      ! will be appended at the end of the file. That makes it a lot easier to read the .out
      ! file back in again and to just discard everything following after the space group.

      ! TODO ##### for space groups higher than orthorhombic we're almost certainly better off
      ! using the format space_group 222:1 rather than the space-group name. 

      space_group_str = SGHMaStr(NumberSGTable)(1:LEN_TRIM(SGHMaStr(NumberSGTable)))
      CALL StrRemoveSpaces(space_group_str, tStrLen)
      WRITE(hFileTOPAS, '(A)', ERR=999) '    space_group '//space_group_str(1:tStrLen)
      WriteTOPASFilePawley = 0
      CLOSE(hFileTOPAS)
      RETURN
  999 CALL ErrorMessage("Error writing TOPAS input file (Pawley).")
      CLOSE(hFileTOPAS)
  998 CALL ErrorMessage("Error writing temporary diffraction data file.")
      CLOSE(hTempFile)
      CLOSE(hFileTOPAS)

      END FUNCTION WriteTOPASFilePawley
!
!*****************************************************************************
! 
      SUBROUTINE UpdateTOPASCheckBoxes

      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page7)
      IF ( .NOT. WDialogGetCheckBoxLogical(IDC_UseDASHRecommendation) ) RETURN
      SELECT CASE ( TOPAS_stage )
        CASE ( 2 ) ! First Rietveld refinement: refine scale only
          CALL WDialogPutCheckBoxLogical(IDC_Scale,       .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,  .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,        .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_IncludeESDs, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,    .TRUE.)
        CASE ( 3 ) ! Second Rietveld refinement: include background
          CALL WDialogPutCheckBoxLogical(IDC_Scale,       .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,  .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,        .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_IncludeESDs, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,    .TRUE.)
        CASE ( 4 ) ! Third Rietveld refinement: include coordinates
          CALL WDialogPutCheckBoxLogical(IDC_Scale,       .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,  .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates, .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,        .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_IncludeESDs, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,    .TRUE.)
        CASE ( 5 ) ! Fourth Rietveld refinement: include Biso
          CALL WDialogPutCheckBoxLogical(IDC_Scale,       .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,  .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates, .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,        .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_IncludeESDs, .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,    .TRUE.)
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE UpdateTOPASCheckBoxes
!
!*****************************************************************************
! 
! The initialisation of this window is done in DealWithWizardWindowProfileRange()
! in Wizard_routines.f90
      SUBROUTINE DealWithRR_TOPAS

      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER, EXTERNAL :: WriteTOPASFileRietveld2
      CHARACTER*255 TOPASFileName, tDirName, tFileName, tExtension
      INTEGER ExtLength
      CHARACTER(LEN=75) FILTER
      INTEGER           iFlags, iFType 

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page7)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_RR_TOPAS)
            CASE (IDCANCEL, IDCLOSE)
              IF ( For_TOPAS ) THEN
                CALL CopyBackup2Pattern()
                For_TOPAS = .FALSE.
              ENDIF
              CALL EndWizardPastPawley
            CASE (IDBBROWSE)
              iFlags = LoadDialog + DirChange + PromptOn
              FILTER = 'All files (*.*)|*.*|'//&
                       'TOPAS input/output files (*.inp, *.out)|*.inp;*.out|'
              ! iFType specifies which of the file types in the list is the default
              iFType = 2
              tFileName = ' '
              CALL WSelectFile(FILTER, iFlags, tFileName, 'Open TOPAS file', iFType)
              ! Did the user press cancel?
              IF (WInfoDialog(ExitButtonCommon) .NE. CommonOK) RETURN
              CALL WDialogPutString(IDF_TOPAS_inp_file_name, tFileName)
            CASE (IDB_WRITE)
              CALL WDialogGetString(IDF_TOPAS_inp_file_name, TOPASFileName)
              IF ( LEN_TRIM(TOPASFileName) .EQ. 0 ) THEN
                CALL ErrorMessage('No file name given.')
                RETURN
              ENDIF
              ExtLength = 3
              CALL SplitPath2(TOPASFileName, tDirName, tFileName, tExtension, ExtLength)
              CALL StrUpperCase(tExtension)
              TOPASFileName = tDirName(1:LEN_TRIM(tDirName))//tFileName(1:LEN_TRIM(tFileName))//'.inp'
              TOPAS_output_file_name  = tDirName(1:LEN_TRIM(tDirName))//tFileName(1:LEN_TRIM(tFileName))//'.out'
              CALL WDialogPutString(IDF_TOPAS_inp_file_name, TOPAS_output_file_name)
              IF ( WriteTOPASFileRietveld2(TOPASFileName) .EQ. 0 ) THEN
                CALL InfoMessage('TOPAS .inp file has been written.')
                TOPAS_stage = TOPAS_stage + 1
                CALL UpdateTOPASCheckBoxes()
              ENDIF
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithRR_TOPAS
!
!*****************************************************************************
! 
! This function read and writes the file for Rietveld refinement (as opposed to Pawley).
      INTEGER FUNCTION WriteTOPASFileRietveld2(FileNameBase)

      USE DRUID_HEADER
      USE VARIABLES
      USE ATMVAR
      USE ZMVAR
      USE PO_VAR
      USE RRVAR
      USE SAMVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (INOUT) :: FileNameBase

      INTEGER         NATOM
      REAL                   Xato
      INTEGER                             KX
      REAL                                           AMULT,      TF
      INTEGER         KTF
      REAL                      SITE
      INTEGER                              KSITE,      ISGEN
      REAL            SDX,        SDTF,      SDSITE
      INTEGER                                             KOM17
      COMMON /POSNS / NATOM, Xato(3,150), KX(3,150), AMULT(150), TF(150),  &
                      KTF(150), SITE(150), KSITE(150), ISGEN(3,150),    &
                      SDX(3,150), SDTF(150), SDSITE(150), KOM17

      INTEGER           TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm
      COMMON  /ORDRATM/ TotNumOfAtoms, NumOfHydrogens, NumOfNonHydrogens, OrderedAtm(1:MaxAtm_3)

      INTEGER MaxAAStack
      PARAMETER (MaxAAStack = 150)

      INTEGER            AAStackPtr, AAStack
      COMMON  /AROMATIC/ AAStackPtr, AAStack(1:MaxAAStack)

      INTEGER, EXTERNAL :: StrFind, assembly_size
      CHARACTER*20, EXTERNAL :: Integer2String
      LOGICAL, EXTERNAL :: assembly_contains, has_aromatic_bond, WDialogGetCheckBoxLogical
      INTEGER hFileTOPAS, hOutputFile
      INTEGER iLen, iPos, iStrPos
      LOGICAL is_last_line
      CHARACTER(512) tLine
      CHARACTER(12) word
      INTEGER       word_len
      CHARACTER(6) b_str
      INTEGER b_str_len, tElement, J
      INTEGER ii, iTotal, iFrg, iAtom, iAtom1, iAtom2
      REAL    distance, angle
      CHARACTER(7) LabelStr, LabelStr1, LabelStr2
      CHARACTER*20 tStr
      INTEGER iLen1, iLen2, K
      CHARACTER*3 tElementStr
      INTEGER Ncon, J1, J2
      INTEGER Icon(30), Icob(30)
      CHARACTER(7) flatten_labels(MaxAtm_3)
      INTEGER nFlatten
      ! We need to remember which atoms have been assigned to an assembly
      INTEGER sum_of_assemblies(1:MaxAtm_3)
      INTEGER current_assembly(1:MaxAtm_3)
      CHARACTER(MaxPathLength) FileNameToRead, FileNameToWrite
      CHARACTER*255 tDirName, tFileName, tExtension
      INTEGER ExtLength
      LOGICAL was_Pawley

      was_Pawley = .FALSE.
      CALL WDialogSelect(IDD_SAW_Page7)
      ! Initialise to failure
      WriteTOPASFileRietveld2 = 1
      ExtLength = 3
      CALL SplitPath2(FileNameBase, tDirName, tFileName, tExtension, ExtLength)
      FileNameBase = tDirName(1:LEN_TRIM(tDirName))//tFileName
      FileNameToRead = FileNameBase(1:LEN_TRIM(FileNameBase))//'.out'
      FileNameToWrite = FileNameBase(1:LEN_TRIM(FileNameBase))//'.inp'
      hFileTOPAS = 116
      hOutputFile = 117 ! This is the file that is being *read*
      OPEN(UNIT=hFileTOPAS, FILE=FileNameToWrite, STATUS='unknown', ERR=999)
      OPEN(UNIT=hOutputFile, FILE=FileNameToRead, STATUS='unknown', ERR=998)
      is_last_line = .FALSE.
   10 CONTINUE
      READ(hOutputFile, '(A)', ERR=998, END=40) tLine
      iLen = LEN_TRIM(tLine)
      word_len = 12
      CALL FirstWord(tLine, word, word_len)
      CALL StrUpperCase(word)
      IF ( word(1:6) .EQ. 'HKL_IS' ) THEN
        was_Pawley = .TRUE.
        WRITE(hFileTOPAS, '(A)', ERR=999) '  str'
      ELSE IF ( word(1:11) .EQ. 'SPACE_GROUP' ) THEN
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
        IF ( was_Pawley ) THEN
          ! #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
          !                             BEGIN
          ! #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
          IF ( WDialogGetCheckBoxLogical(IDC_Scale) ) THEN
            WRITE(hFileTOPAS, '(A)', ERR=999) '    scale @ 1.0'
          ELSE
            WRITE(hFileTOPAS, '(A)', ERR=999) '    scale   1.0'
          ENDIF
! We must call ShowWizardWindowRietveld() here, which will fill
! Xato (and all the RR variables). The Wizard window is suppressed because of the For_TOPAS flag.
          CALL ShowWizardWindowRietveld(RR_SA_Sol)
! Also need to write out PO if used during SA
          IF ( PrefParExists ) THEN
            WRITE(hFileTOPAS, '(A,F5.3,A,I3,1X,I3,1X,I3,1X,A)', ERR=999) '    PO(@, ', RR_PO, ', , ', PO_Direction(1), PO_Direction(2), PO_Direction(3), ')'
            CALL InfoMessage('The preferred orientation that was used during the Simulated Annealing'//CHAR(13)//&
                             'has been written out to the TOPAS .inp file: if you are using'//CHAR(13)//&
                             'a different experimental pattern, you may need to remove this.')
          ELSE
            WRITE(hFileTOPAS, '(A)', ERR=999) '    PO( , 1.0, , 1 0 0)'
          ENDIF
          IF ( WDialogGetCheckBoxLogical(IDC_Coordinates) ) THEN
            WRITE(hFileTOPAS, '(A)', ERR=999) '    macro ref_flag { @ }'
          ELSE
            WRITE(hFileTOPAS, '(A)', ERR=999) '    macro ref_flag {   }'
          ENDIF
          ! #########################################################################################
          ! TOPAS needs unique atom labels ("site labels"), especially when applying restraints.
          ! This is a problem for DASH where we use user-supplied labels.
          ! This is especially a problem if there is more than one Z-matrix in the asymmetric unit.
          ! (Would it have been possible to solve this using the "scopes" in TOPAS?, I.e. define each
          ! Z-matrix in its own scope and then define its restraints within the same scope?)
          ! #########################################################################################
          iTotal = 0
          DO iFrg = 1, nFrag
            DO iAtom = 1, natoms(iFrg)
              tElement = zmElementCSD(iAtom,iFrg)
              IF ( (tElement .EQ. 2) .OR. (tElement .EQ. 27) ) THEN
                ! Hydrogen or deuterium
                b_str = 'bh'
                b_str_len = 2
              ELSE
                b_str = 'bnonh'
                b_str_len = 5
              ENDIF
              IF ( .NOT. WDialogGetCheckBoxLogical(IDC_Biso) ) THEN
                b_str = '!'//b_str
                b_str_len = b_str_len + 1
              ENDIF
              tStr = Integer2String(iTotal + iAtom)
              iLen1 = LEN_TRIM(tStr)
              tElementStr = ElementStr(tElement)
              iLen2 = LEN_TRIM(tElementStr)
              LabelStr = tElementStr(1:iLen2)//tStr(1:iLen1)
              ii = OrderedAtm(iTotal + iAtom) ! Needed for Xato()
              WRITE(hFileTOPAS, '(A,A7,A,F9.5,A,F9.5,A,F9.5,A,F6.3,A,1X,F6.3)', ERR=999) '    site  ', LabelStr, ' x ref_flag ', &
                Xato(1,ii), ' y ref_flag ', Xato(2,ii), ' z ref_flag ', Xato(3,ii), ' occ '//ElementStr(tElement)//'  ', & 
                occ(iAtom,iFrg), ' beq '//b_str(1:b_str_len), tiso(iAtom,iFrg)
            ENDDO
            iTotal = iTotal + natoms(iFrg)
          ENDDO
          WRITE(hFileTOPAS, '(A)', ERR=999) '    prm !bond_width 0'
          WRITE(hFileTOPAS, '(A)', ERR=999) '    prm !bond_weight 10000'
          WRITE(hFileTOPAS, '(A)', ERR=999) '    prm !angle_width 1'
          WRITE(hFileTOPAS, '(A)', ERR=999) '    prm !angle_weight 1'
          iTotal = 0
          DO iFrg = 1, nFrag
            ! Convert internal coordinates to orthogonal coordinates
            CALL makexyz(natoms(iFrg),RR_blen(1,iFrg),RR_alph(1,iFrg),RR_bet(1,iFrg),        &
                         IZ1(1,iFrg),IZ2(1,iFrg),IZ3(1,iFrg),axyzo)
            DO iAtom = 1, natoms(iFrg)
              aelem(iAtom) = zmElementCSD(iAtom,iFrg)
            ENDDO
            natcry = natoms(iFrg)
            ! Detect bonds and their types (to find benzene rings for Flatten macro)
            CALL SAMABO()
            ! ##### Distance restraints #####
            DO J = 1, nbocry
              iAtom1 = bond(J,1)
              iAtom2 = bond(J,2)
              CALL PLUDIJ(iAtom1, iAtom2, distance)
              CALL GenerateAtomLabel(zmElementCSD(iAtom1,iFrg), iTotal+iAtom1, LabelStr1)
              CALL GenerateAtomLabel(zmElementCSD(iAtom2,iFrg), iTotal+iAtom2, LabelStr2)
              WRITE(hFileTOPAS, '(A,A7,1X,A7,A,F9.5,A)', ERR=999) 'Distance_Restrain(', LabelStr1, LabelStr2, &
                ', ', distance, ', 1.0, bond_width, bond_weight)'
            ENDDO
            ! ##### Angle restraints #####
            DO iAtom = 1, natoms(iFrg)
              CALL SAMCON(iAtom, Ncon, Icon, Icob, 0)
! NCON        output number of connected atoms for iAtom
! ICON (1:30) output list of atoms connected to iAtom
! ICOB (1:30) output bond types for each connection in ICON
              DO J1 = 1, Ncon-1
                DO J2 = J1+1, Ncon
                  iAtom1 = Icon(J1)
                  iAtom2 = Icon(J2)
                  CALL SAMANF(iAtom1, iAtom, iAtom2, angle)
                  CALL GenerateAtomLabel(zmElementCSD(iAtom,iFrg), iTotal+iAtom, LabelStr)
                  CALL GenerateAtomLabel(zmElementCSD(iAtom1,iFrg), iTotal+iAtom1, LabelStr1)
                  CALL GenerateAtomLabel(zmElementCSD(iAtom2,iFrg), iTotal+iAtom2, LabelStr2)
                  WRITE(hFileTOPAS, '(A,A7,1X,A7,1X,A7,A,F9.5,A)', ERR=999) 'Angle_Restrain(', LabelStr1, &
                    LabelStr, LabelStr2, ', ', angle, ', 1.0, angle_width, angle_weight)'
                ENDDO
              ENDDO
            ENDDO
            ! ##### Flatten #####
            sum_of_assemblies = 0
            ! Loop over atoms
            DO iAtom = 1, natoms(iFrg)
              ! If in a previous assembly, continue to next atom
              IF ( assembly_contains(sum_of_assemblies, iAtom) ) GOTO 20
              ! If atom does not have any aromatic bond attached to it, continue to next atom
              IF ( .NOT. has_aromatic_bond(iAtom) ) GOTO 20
              ! Otherwise, use this atom as the start of the algorithm.
              current_assembly = 0 ! Empty the current assembly
              AAStackPtr = MaxAAStack ! Empty the stack
              ! Push the atom onto the stack
              CALL PushAA(iAtom)
              ! Then call the subroutine who does all the work.
              CALL find_aromatic_assembly(current_assembly)
              ! Write out this assembly with a flatten command
              ! We could add a test to check that the atoms that we have added are in a plane to start with
              ! This is not necessarily so: e.g. in a buckyball or in one of those fused benzene rings spiral
              nFlatten = 0
              DO J = 1, MaxAtm_3
                iAtom1 = current_assembly(J)
                IF ( iAtom1 .NE. 0 ) THEN
                  CALL GenerateAtomLabel(zmElementCSD(iAtom1,iFrg), iTotal+iAtom1, flatten_labels(J))
                  nFlatten = nFlatten + 1
                ENDIF
              ENDDO
              IF ( nFlatten .GT. 3 ) THEN
                WRITE(hFileTOPAS, '(A,999(1X,A7))', ERR=999) 'Flatten(', (flatten_labels(K),K=1,nFlatten), ', 0, 0,', '1000000',')      '
              ENDIF
              ! Add current assembly to sum_of_assemblies
              DO J = 1, nFlatten
                CALL assembly_add(sum_of_assemblies, current_assembly(J))
              ENDDO
   20         CONTINUE
            ENDDO
   30       CONTINUE
            iTotal = iTotal + natoms(iFrg)
          ENDDO
          ! #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
          !                            END
          ! #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@

          ! Probably need to improve mechanism to indicate end
          is_last_line = .TRUE.
        ENDIF
      ELSE IF ( word(1:3) .EQ. 'BKG' ) THEN
        IF ( WDialogGetCheckBoxLogical(IDC_Background) ) THEN
          tLine(7:7) = '@'
        ELSE
          tLine(7:7) = ' '
        ENDIF 
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ELSE IF ( word(1:5) .EQ. 'SCALE' ) THEN
        IF ( WDialogGetCheckBoxLogical(IDC_Scale) ) THEN
          tLine(11:11) = '@'
        ELSE
          tLine(11:11) = ' '
        ENDIF 
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ELSE IF ( word(1:11) .EQ. 'OUT_CIF_STR' ) THEN
        ! Do nothing: it will be added at the end if necessary
      ELSE IF ( word(1:9) .EQ. 'DO_ERRORS' ) THEN
        ! Do nothing: it will be added at the end if necessary
      ELSE IF ( word(1:4) .EQ. 'SITE' ) THEN
        ! Several possibilities here.
        ! The only thing that really needs to be done here is to insert or erase the !
        ! to indicate whether Biso is to be refined
        ! But we could also read the fractional coordinates back in and use them to
        ! overlay the refined solution with the original one.
        iPos = StrFind(tLine, iLen, '!', 1)
        IF ( iPos .NE. 0 ) THEN
          IF ( WDialogGetCheckBoxLogical(IDC_Biso) ) THEN
            tLine(iPos:iPos) = ' '
            WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
          ELSE
            ! Don't change anything if ! already present and Biso not to be refined
            WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
          ENDIF
        ELSE
          IF ( .NOT. WDialogGetCheckBoxLogical(IDC_Biso) ) THEN
            ! Find 'beq  bnonh' or 'beq  bh'
            iPos = StrFind(tLine, iLen, 'beq  bnonh', 10)
            IF ( iPos .EQ. 0 ) THEN
              iPos = StrFind(tLine, iLen, 'beq  bh', 7)
              IF ( iPos .EQ. 0 ) THEN
                CALL ErrorMessage('"beq" keyword not found on "site" line.')
              ELSE
                tLine(iPos+4:iPos+4) = '!'
              ENDIF
            ELSE
              tLine(iPos+4:iPos+4) = '!'
            ENDIF
            WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
          ELSE
            ! Don't change anything if ! not present and Biso requested to be refined
            WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
          ENDIF
        ENDIF
      ELSE IF ( word(1:5) .EQ. 'MACRO' ) THEN
        ! Check if rest is ref_flag
        IF ( StrFind(tLine, iLen, 'ref_flag', 8) .NE. 0 ) THEN
          IF ( WDialogGetCheckBoxLogical(IDC_Coordinates) ) THEN
            WRITE(hFileTOPAS, '(A)', ERR=999) '    macro ref_flag { @ }'
          ELSE
            WRITE(hFileTOPAS, '(A)', ERR=999) '    macro ref_flag {   }'
          ENDIF
        ENDIF
      ELSE IF ( word(1:2) .EQ. 'PO' ) THEN
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ELSE
        ! Change all the @'s to spaces.
        ! This switches off refinement of a, b, c, al, be, ga, zero-point error,
        ! CS_L, CS_G, Strain_L, Strain_G
        DO iStrPos = 1, iLen
          IF (tLine(iStrPos:iStrPos) .EQ. '@') &
            tLine(iStrPos:iStrPos) = ' '
        ENDDO
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ENDIF
      IF ( .NOT. is_last_line ) GOTO 10
   40 CLOSE(hOutputFile)

      IF ( WDialogGetCheckBoxLogical(IDC_IncludeESDs) ) THEN
        WRITE(hFileTOPAS, '(A)', ERR=999) '    do_errors'
      ENDIF
      IF ( WDialogGetCheckBoxLogical(IDC_WriteCIF) ) THEN
        WRITE(hFileTOPAS, '(A)', ERR=999) '    Out_CIF_STR("'//FileNameBase(1:LEN_TRIM(FileNameBase))//'.cif")'
      ENDIF

      WriteTOPASFileRietveld2 = 0
      CLOSE(hFileTOPAS)
      RETURN
  999 CALL ErrorMessage("Error writing TOPAS input file (Rietveld)")
      CLOSE(hFileTOPAS)
      RETURN
  998 CALL ErrorMessage("Error reading TOPAS output file")
      CLOSE(hOutputFile)
      CLOSE(hFileTOPAS)

      END FUNCTION WriteTOPASFileRietveld2
!
!*****************************************************************************
!
      SUBROUTINE find_aromatic_assembly(the_assembly)

      USE ATMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: the_assembly(1:MaxAtm_3)

      INTEGER MaxAAStack
      PARAMETER (MaxAAStack = 150)

      INTEGER            AAStackPtr, AAStack
      COMMON  /AROMATIC/ AAStackPtr, AAStack(1:MaxAAStack)

      INTEGER, EXTERNAL :: PopAA
      LOGICAL, EXTERNAL :: assembly_contains, AAStackEmpty, has_aromatic_bond
      INTEGER Ncon, I, iAtom, iAtom2
      INTEGER Icon(30), Icob(30)

   10 CONTINUE
      ! Get the atom from the top of the stack...
      iAtom = PopAA()
      ! Check that it is not yet in the assembly
      IF ( assembly_contains(the_assembly, iAtom) ) GOTO 20
      ! Add the atom to the assembly
      CALL assembly_add(the_assembly, iAtom)
      ! Get its neighbours
      CALL SAMCON(iAtom, Ncon, Icon, Icob, 0)
      ! Loop over neighbours
      DO I = 1, Ncon
        iAtom2 = Icon(I)
        ! Check that the neighbour is not in the assembly yet
        IF ( .NOT. assembly_contains(the_assembly, iAtom2) ) THEN
          ! If the neighbour has no aromatic bonds, add it to the assembly
          IF ( .NOT. has_aromatic_bond(iAtom2) ) THEN
            CALL assembly_add(the_assembly, iAtom2)
          ELSE
            ! Otherwise, push the neighbour onto the stack
            CALL PushAA(iAtom2)
          ENDIF
        ENDIF
      ENDDO
   20 CONTINUE
      ! If stack now empty, RETURN
      IF ( AAStackEmpty() ) RETURN
      GOTO 10

      END SUBROUTINE find_aromatic_assembly
!
!*****************************************************************************
!
      SUBROUTINE PushAA(the_value)
! AA = Aromatic Assembly
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: the_value

      INTEGER MaxAAStack
      PARAMETER (MaxAAStack = 150)

      INTEGER            AAStackPtr, AAStack
      COMMON  /AROMATIC/ AAStackPtr, AAStack(1:MaxAAStack)

! Check if stack full
      IF ( AAStackPtr .EQ. 0 ) THEN
        CALL DebugErrorMessage('AAStackPtr full.')
        RETURN
      ENDIF
! If not, store value
      AAStack(AAStackPtr) = the_value
! Dec(StackPtr)
      AAStackPtr = AAStackPtr - 1

      END SUBROUTINE PushAA
!
!*****************************************************************************
!
      INTEGER FUNCTION PopAA()

      IMPLICIT NONE

      INTEGER MaxAAStack
      PARAMETER (MaxAAStack = 150)

      INTEGER            AAStackPtr, AAStack
      COMMON  /AROMATIC/ AAStackPtr, AAStack(1:MaxAAStack)

! Check if stack empty
      IF ( AAStackPtr .EQ. MaxAAStack ) THEN
        CALL DebugErrorMessage('AAStack empty.')
        PopAA = 0
        RETURN
      ENDIF
! If not, Inc(StackPtr)
      AAStackPtr = AAStackPtr + 1
      PopAA = (AAStack(AAStackPtr))

      END FUNCTION PopAA
!
!*****************************************************************************
!
      LOGICAL FUNCTION AAStackEmpty()

      IMPLICIT NONE

      INTEGER MaxAAStack
      PARAMETER (MaxAAStack = 150)

      INTEGER            AAStackPtr, AAStack
      COMMON  /AROMATIC/ AAStackPtr, AAStack(1:MaxAAStack)

      AAStackEmpty = ( AAStackPtr .EQ. MaxAAStack )

      END FUNCTION AAStackEmpty
!
!*****************************************************************************
!
