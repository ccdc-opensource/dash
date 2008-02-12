!
!*****************************************************************************
!
      SUBROUTINE Launch_TOPAS(input_file_name)

      USE DRUID_HEADER
      USE WINTERACTER
      USE TAVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: input_file_name

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      CHARACTER*20, EXTERNAL :: Integer2String
      LOGICAL exists, run_TOPAS_in_background
      INTEGER M, I, tLen
      CHARACTER*20 stage_str
      CHARACTER*(255) tDirName, tFileName
      INTEGER hFile

      ! Launch TOPAS and wait for it to return
      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_Configuration)
      run_TOPAS_in_background = DASHWDialogGetCheckBoxLogical(IDC_TOPAS_in_background)
!      CALL DASHWDialogGetString(IDF_TOPASExe, TOPASEXE)
      CALL PopActiveWindowID
      tLen = LEN_TRIM(input_file_name)
      I = LEN_TRIM(TOPASEXE)
      CALL SplitPath(TOPASEXE(1:I), tDirName, tFileName)
      IF ( run_TOPAS_in_background ) THEN
        IF ( I .EQ. 0 ) THEN
          CALL ErrorMessage("DASH could not launch TOPAS. No executable is currently specified."//CHAR(13)//&
                            "This can be changed in the Configuration... window"//CHAR(13)//&
                            "under Options in the menu bar.")
          CALL WCursorShape(CurCrossHair)
          RETURN
        ENDIF
        INQUIRE(FILE = TOPASEXE(1:I),EXIST=exists)
        IF (.NOT. exists) GOTO 998
        M = InfoError(1) ! Clear errors
        CALL IOsDirChange(tDirName)
!        CALL IOSCommand('CMD.EXE /C '//TOPASEXE(1:I)//' '//input_file_name(1:tLen), ProcBlocked)
        CALL IOSCommand(TOPASEXE(1:I)//' '//'"'//input_file_name(1:tLen)//'"', ProcBlocked)
        IF ( InfoError(1) .NE. 0 ) GOTO 998
        CALL WCursorShape(CurCrossHair)
      ELSE
        ! Write out the launch_file.txt file
        IF ( ext_RR_stage .EQ. 1 ) THEN
          IF ( I .EQ. 0 ) THEN
            CALL InfoMessage("The launch_file.txt could not be written,"//CHAR(13)//&
                             "because no TOPAS directory has been entered."//CHAR(13)//&
                             "This can be changed in the Configuration... window"//CHAR(13)//&
                             "under Options in the menu bar.")
          ELSE
            hFile = 112
            OPEN(UNIT=hFile, FILE=TRIM(tDirName)//'launch_file.txt', STATUS='unknown', ERR=999)
            WRITE(hFile, '(A)', ERR=999) '"'//input_file_name(1:tLen)//'.inp"'
            GOTO 100
  999       CONTINUE ! Error writing out launch_file.txt: we don't really care.
!           CALL InfoMessage("launch_file.txt could not be written.")
            CALL DebugErrorMessage("launch_file.txt could not be written.")
  100       CLOSE(hFile) 
          ENDIF
        ENDIF
!        CALL InfoMessage('TOPAS .inp file for Pawley has been written.')
        stage_str = Integer2String(ext_RR_stage)
        CALL InfoMessage('TOPAS .inp file '//TRIM(stage_str)//' has been written.')
      ENDIF
      RETURN
  998 CALL ErrorMessage("DASH could not launch TOPAS. The executable is currently configured"//CHAR(13)//&
                        "to launch the program "//TOPASEXE(1:I)//CHAR(13)//&
                        "This can be changed in the Configuration... window"//CHAR(13)//&
                        "under Options in the menu bar.")
      CALL WCursorShape(CurCrossHair)
      RETURN

      END SUBROUTINE Launch_TOPAS
!
!*****************************************************************************
!
      INTEGER FUNCTION WriteTOPASFilePawley(TheFileName)

      USE DRUID_HEADER
      USE WINTERACTER
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INCLUDE 'PARAMS.INC'
      INCLUDE 'Lattice.inc'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      REAL, EXTERNAL :: FnWavelengthOfMenuOption
      INTEGER hFileTOPAS, hTempFile
      INTEGER iLen, iLen_1, i, tLen
      CHARACTER*(1) tChar
      CHARACTER*(MaxPathLength) tDirName, tFileName
      CHARACTER*(40) space_group_str
      INTEGER tStrLen
      INTEGER tIRadSelection
      CHARACTER*(6) bkg(4)
      DATA bkg/' 100.0', '-100.0', ' 10.0', ' 10.0'/

      ! The way this code has curently been written, this routine can only be called
      ! from one of the Wizard windows as part of a "iRietveldMethod" Rietveld refinement
      ! Initialise to failure
      WriteTOPASFilePawley = 1
      hFileTOPAS = 116
      tLen = LEN_TRIM(TheFileName)
      OPEN(UNIT=hFileTOPAS, FILE=TheFileName(1:tLen), STATUS='unknown', ERR=999)
      WRITE(hFileTOPAS, '(A)', ERR=999) '#include "'// &
                                        TRIM(InstallationDirectory)//'TOPAS.inc"'
      ! Ideally, we need to figure out if penalties_weighting_K1 can be made local to the "str" keyword.
      WRITE(hFileTOPAS, '(A)', ERR=999) 'penalties_weighting_K1 5'
      WRITE(hFileTOPAS, '(A)', ERR=999) "'do_errors"
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
      CALL SplitPath(TheFileName, tDirName, tFileName)
      iLen = LEN_TRIM(tDirName)
      tFileName = tDirName(1:iLen)//'temp.xye'
      iLen_1 = iLen + 8
      OPEN(UNIT=hTempFile,FILE=tFileName(1:iLen_1),STATUS='unknown',ERR=998)
      DO i = 1, NBIN
        WRITE(hTempFile, '(F10.5,1X,F12.4,1X,F12.5)', ERR=998) XBIN(i), YOBIN(i), EBIN(i)
      ENDDO
      CLOSE(hTempFile)
      WRITE(hFileTOPAS, '(A)', ERR=999) 'xdd "'//tFileName(1:iLen_1)//'" xye_format'
      WRITE(hFileTOPAS, '(A)', ERR=999) '  bkg @ '
      DO i = 1, NumOfBkgTerm
        IF ( i .LE. 4 ) THEN
          WRITE(hFileTOPAS, '(3X,A)', ERR=999) bkg(i)
        ELSE
          WRITE(hFileTOPAS, '(3X,A)', ERR=999) ' 0.0'
        ENDIF
      ENDDO
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
      WRITE(hFileTOPAS, '(A)', ERR=999) '  Rp 217.5'
      WRITE(hFileTOPAS, '(A)', ERR=999) '  Rs 217.5'
!      WRITE(hFileTOPAS, '(A)', ERR=999) '  Simple_Axial_Model(@, 9.0)'
      WRITE(hFileTOPAS, '(A)', ERR=999) '  axial_conv'
      IF ( JRadOption .EQ. 1 ) THEN
        WRITE(hFileTOPAS, '(A)', ERR=999) '    filament_length   @   6.0'
        WRITE(hFileTOPAS, '(A)', ERR=999) '    sample_length   @   3.5'
        WRITE(hFileTOPAS, '(A)', ERR=999) '    receiving_slit_length   @  12.0'
      ELSE
        WRITE(hFileTOPAS, '(A)', ERR=999) '    filament_length   @   1.0'
        WRITE(hFileTOPAS, '(A)', ERR=999) '    sample_length   @   1.0'
        WRITE(hFileTOPAS, '(A)', ERR=999) '    receiving_slit_length   @  3.0'
      ENDIF
      WRITE(hFileTOPAS, '(A)', ERR=999) '    axial_n_beta  20'
      WRITE(hFileTOPAS, '(A)', ERR=999) '    axial_del  0.0053'
      WRITE(hFileTOPAS, '(A)', ERR=999) '  lam'
      WRITE(hFileTOPAS, '(A)', ERR=999) '    ymin_on_ymax 0.001'
      CALL SelectDASHDialog(IDD_PW_Page4)
      IF ( DASHWDialogGetCheckBoxLogical(IDC_Monochromated) .OR. (JRadOption .NE. 1) ) THEN
        WRITE(hFileTOPAS, '(A,F9.6)', ERR=999) '    la 1 lo ', ALambda
      ELSE
        tIRadSelection = -1
        DO I = 2, 6
          IF (ABS(ALambda - FnWavelengthOfMenuOption(I)) .LT. 0.0003) tIRadSelection = I
        ENDDO
        SELECT CASE (tIRadSelection)
        CASE (2) ! Cu
          WRITE(hFileTOPAS, '(A)', ERR=999) '    la 0.6666667 lo 1.54060'
          WRITE(hFileTOPAS, '(A)', ERR=999) '    la 0.3333333 lo 1.54439'
        CASE (3) ! Mo
          WRITE(hFileTOPAS, '(A)', ERR=999) '    la 0.6666667 lo 0.70930'
          WRITE(hFileTOPAS, '(A)', ERR=999) '    la 0.3333333 lo 0.71359'
        CASE (4) ! Co
          WRITE(hFileTOPAS, '(A)', ERR=999) '    la 0.6666667 lo 1.78897'
          WRITE(hFileTOPAS, '(A)', ERR=999) '    la 0.3333333 lo 1.79285'
        CASE (5) ! Cr
          WRITE(hFileTOPAS, '(A)', ERR=999) '    la 0.6666667 lo 2.28970'
          WRITE(hFileTOPAS, '(A)', ERR=999) '    la 0.3333333 lo 2.29361'
        CASE (6) ! Fe
          WRITE(hFileTOPAS, '(A)', ERR=999) '    la 0.6666667 lo 1.93604'
          WRITE(hFileTOPAS, '(A)', ERR=999) '    la 0.3333333 lo 1.93998'
        CASE DEFAULT
          CALL InfoMessage("Wavelength not recognised as a standard anode material--monochromated assumed.")
          WRITE(hFileTOPAS, '(A,F9.6)', ERR=999) '    la 1 lo ', ALambda
        END SELECT
      ENDIF
      WRITE(hFileTOPAS, '(A,F8.6)', ERR=999) '  x_calculation_step ', (XBIN(NBIN) - XBIN(1)) / (NBIN - 1)
      WRITE(hFileTOPAS, '(A)', ERR=999) '  hkl_Is'
      WRITE(hFileTOPAS, '(A)', ERR=999) '    CS_L(@, 5000.00)' !200.00
      WRITE(hFileTOPAS, '(A)', ERR=999) '    CS_G(@, 200.00)'
      IF ( JRadOption .EQ. 1 ) THEN
        WRITE(hFileTOPAS, '(A)', ERR=999) '    Strain_G(@, 0.2)'
      ELSE
        WRITE(hFileTOPAS, '(A)', ERR=999) '    Strain_G(@, 0.01)'
      ENDIF
      WRITE(hFileTOPAS, '(A)', ERR=999) '    Strain_L(@, 0.1)' !0.01
      IF ( LatBrav .LT. 6 ) THEN ! Triclinic through orthorhombic: no constraints on a, b or c.
        WRITE(hFileTOPAS, '(A,F10.5)', ERR=999) '    a  @ ', CELLPAR(1)
        WRITE(hFileTOPAS, '(A,F10.5)', ERR=999) '    b  @ ', CELLPAR(2)
        WRITE(hFileTOPAS, '(A,F10.5)', ERR=999) '    c  @ ', CELLPAR(3)
      ELSE
        ! Unit-cell parameters b or c or both are constrained:
        ! we must define a in terms of a TOPAS "parameter".
        WRITE(hFileTOPAS, '(A,F10.5)', ERR=999) '    a  uc_prm ', CELLPAR(1)
        WRITE(hFileTOPAS, '(A)', ERR=999) '    b = uc_prm;'
        IF ( CellParConstrained(3) ) THEN
          WRITE(hFileTOPAS, '(A)', ERR=999) '    c = uc_prm;'
        ELSE
          WRITE(hFileTOPAS, '(A,F10.5)', ERR=999) '    c  @ ', CELLPAR(3)
        ENDIF
      ENDIF
      IF ( LatBrav .EQ. 8 ) THEN ! Rhombohedral
        ! Unit-cell parameters beta and gamma are constrained:
        ! we must define alpha in terms of a TOPAS "parameter".
        WRITE(hFileTOPAS, '(A,F10.5)', ERR=999) '    al  uc_ang_prm ', CELLPAR(4)
        WRITE(hFileTOPAS, '(A)', ERR=999) '    be = uc_ang_prm;'
        WRITE(hFileTOPAS, '(A)', ERR=999) '    ga = uc_ang_prm;'
      ELSE
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
      ENDIF
!      WRITE(hFileTOPAS, '(A)', ERR=999) '    phase_name dummy'
      ! By writing the space group name last, the extra information on hkl's and intensities
      ! will be appended at the end of the file. That makes it a lot easier to read the .out
      ! file back in again and to just discard everything following after the space group.

      ! TODO ##### for space groups higher than orthorhombic we're almost certainly better off
      ! using the format space_group 222:1 rather than the space-group name. 

      space_group_str = TRIM(SGHMaStr(NumberSGTable))
      CALL StrRemoveSpaces(space_group_str, tStrLen)
      WRITE(hFileTOPAS, '(A)', ERR=999) '    space_group '//space_group_str(1:tStrLen)
!      WRITE(hFileTOPAS, '(A,A,A)', ERR=999) '    Out_X_Ycalc("', tDirName(1:iLen), 'temp.txt")'
      WriteTOPASFilePawley = 0
      CLOSE(hFileTOPAS)

!      CALL InfoMessage(TheFileName(1:tLen-4))

      CALL Launch_TOPAS(TheFileName(1:tLen-4))
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
      USE TAVAR

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SAW_Page7_TOPAS)
      IF ( .NOT. DASHWDialogGetCheckBoxLogical(IDC_UseDASHRecommendation) ) RETURN
      SELECT CASE ( ext_RR_stage )
        CASE ( 2 ) ! Anisotropic Pawley refinement
          CALL WDialogFieldState(IDC_Anisotropic_broadening, Enabled)
          CALL WDialogPutCheckBoxLogical(IDC_Anisotropic_broadening, .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Scale,       .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,  .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,        .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_IncludeESDs, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_SaveProfile, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,    .TRUE.)
        CASE ( 3 ) ! First Rietveld refinement: refine scale only
          CALL WDialogFieldState(IDC_Anisotropic_broadening, Disabled)
          CALL WDialogPutCheckBoxLogical(IDC_Scale,       .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,  .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,        .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_IncludeESDs, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_SaveProfile, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,    .TRUE.)
        CASE ( 4 ) ! Second Rietveld refinement: include background
          CALL WDialogPutCheckBoxLogical(IDC_Scale,       .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,  .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,        .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_IncludeESDs, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_SaveProfile, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,    .TRUE.)
        CASE ( 5 ) ! Third Rietveld refinement: include Biso
          CALL WDialogPutCheckBoxLogical(IDC_Scale,       .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,  .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,        .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_IncludeESDs, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_SaveProfile, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,    .TRUE.)
        CASE ( 6 ) ! Fourth Rietveld refinement: include coordinates
          CALL WDialogPutCheckBoxLogical(IDC_Scale,       .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,  .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,        .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates, .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_IncludeESDs, .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_SaveProfile, .TRUE.)
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
      USE TAVAR

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      INTEGER, EXTERNAL :: WriteTOPASFileRietveld2
      INTEGER, EXTERNAL :: WriteTOPASPawleyAnisotropic

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SAW_Page7_TOPAS)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_RR_External)
            CASE (IDCANCEL, IDCLOSE)
              IF ( iRietveldMethod .NE. INTERNAL_RB ) THEN
                CALL CopyBackup2Pattern()
                iRietveldMethod = INTERNAL_RB
              ENDIF
              CALL EndWizardPastPawley
            CASE (IDB_WRITE)
              ! ## The way I have programmed it at the moment (inserting an extra stage "2" that
              ! is simply skipped when no aniso) means that we could move the two lines:
              !      ext_RR_stage = ext_RR_stage + 1
              !      CALL UpdateTOPASCheckBoxes()
              ! to Launch_TOPAS()

              IF ( ext_RR_stage .EQ. 2 ) THEN
                ! Anisotropic broadening...
                use_anisotropic_broadening = DASHWDialogGetCheckBoxLogical(IDC_Anisotropic_broadening)
                IF ( use_anisotropic_broadening ) THEN
                  IF ( WriteTOPASPawleyAnisotropic(ext_RR_input_file_name) .EQ. 0 ) THEN
                    CALL Launch_TOPAS(ext_RR_input_file_name)
                    ext_RR_stage = ext_RR_stage + 1
                    CALL UpdateTOPASCheckBoxes()
                  ENDIF
                  CALL PopActiveWindowID
                  RETURN
                ENDIF
                ext_RR_stage = ext_RR_stage + 1
                CALL UpdateTOPASCheckBoxes()
              ENDIF
              IF ( WriteTOPASFileRietveld2(ext_RR_input_file_name) .EQ. 0 ) THEN
                CALL Launch_TOPAS(ext_RR_input_file_name)
                ext_RR_stage = ext_RR_stage + 1
                CALL UpdateTOPASCheckBoxes()
              ENDIF
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithRR_TOPAS
!
!*****************************************************************************
! 
! This function reads the Pawley refinement file and and writes the file for
! Pawley refinement with anisotropic broadening.
      INTEGER FUNCTION WriteTOPASPawleyAnisotropic(FileNameBase)

      ! If we have just done the Pawley refinement with isotropic broadening, then the
      ! next step could be Pawley refinement with anisotropic broadening.
      ! To be written out after "Strain_L". All other parameters remain variable.

      USE DRUID_HEADER
      USE VARIABLES
      USE ATMVAR
      USE ZMVAR
      USE PO_VAR
      USE RRVAR
      USE SAMVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (INOUT) :: FileNameBase

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      INTEGER hFileTOPAS, hOutputFile
      INTEGER iLen
      LOGICAL is_last_line
      CHARACTER(512) tLine
      CHARACTER(30) word
      INTEGER       word_len
      CHARACTER(MaxPathLength) FileNameToRead, FileNameToWrite
      CHARACTER*255 tDirName, tFileName, tExtension
      INTEGER ExtLength

      ! Initialise to failure
      WriteTOPASPawleyAnisotropic = 1
      ExtLength = 3
      CALL SplitPath2(FileNameBase, tDirName, tFileName, tExtension, ExtLength)
      FileNameBase = TRIM(tDirName)//tFileName
      FileNameToRead = TRIM(FileNameBase)//'.out'
      FileNameToWrite = TRIM(FileNameBase)//'.inp'
      hFileTOPAS = 116
      hOutputFile = 117 ! This is the file that is being *read*
      OPEN(UNIT=hFileTOPAS, FILE=FileNameToWrite, STATUS='unknown', ERR=999)
      OPEN(UNIT=hOutputFile, FILE=FileNameToRead, STATUS='unknown', ERR=998)
      is_last_line = .FALSE.
   10 CONTINUE
      READ(hOutputFile, '(A)', ERR=998, END=40) tLine
      iLen = LEN_TRIM(tLine)
      word_len = 30
      CALL FirstWord(tLine, word, word_len)
      CALL StrUpperCase(word)
      WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      IF ( (word_len .EQ. 8) .AND. (word(1:8) .EQ. 'STRAIN_L') ) THEN
        WRITE(hFileTOPAS, '(A)', ERR=999) '    prm    sh_scale 0.01 min 0.0001'
        WRITE(hFileTOPAS, '(A)', ERR=999) '    spherical_harmonics_hkl sh'
        WRITE(hFileTOPAS, '(A)', ERR=999) '      sh_order 6'
        WRITE(hFileTOPAS, '(A)', ERR=999) '    lor_fwhm = sh_scale * sh;'
      ENDIF
      GOTO 10
   40 CLOSE(hOutputFile)
!      IF ( DASHWDialogGetCheckBoxLogical(IDC_IncludeESDs) ) THEN
!        WRITE(hFileTOPAS, '(A)', ERR=999) 'do_errors'
!      ENDIF
      WriteTOPASPawleyAnisotropic = 0
      CLOSE(hFileTOPAS)
      RETURN
  999 CALL ErrorMessage("Error writing TOPAS input file (Pawley anisotropic)")
      CLOSE(hFileTOPAS)
      RETURN
  998 CALL ErrorMessage("Error reading TOPAS output file (Pawley anisotropic)")
      CLOSE(hOutputFile)
      CLOSE(hFileTOPAS)

      END FUNCTION WriteTOPASPawleyAnisotropic
!
!*****************************************************************************
! 
! This function reads and writes the file for Rietveld refinement (as opposed to Pawley).
      INTEGER FUNCTION WriteTOPASFileRietveld2(FileNameBase)

      USE DRUID_HEADER
      USE VARIABLES
      USE PO_VAR
      USE RRVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (INOUT) :: FileNameBase

      INCLUDE 'Lattice.inc'

      INTEGER, EXTERNAL :: StrFind, assembly_size
      CHARACTER*20, EXTERNAL :: Integer2String
      LOGICAL, EXTERNAL :: assembly_contains, has_aromatic_bond, DASHWDialogGetCheckBoxLogical
      LOGICAL, EXTERNAL :: PutAtomsForSpecailPosition
      LOGICAL, EXTERNAL :: PutRestraints, TOPASWriteDistance, TOPASWriteAngle, TOPASWritePlane
      INTEGER hFileTOPAS, hOutputFile
      INTEGER iLen, iPos, iStrPos
      LOGICAL is_last_line
      CHARACTER(512) tLine
      CHARACTER(60) word
      INTEGER       word_len
      CHARACTER*20 tStr
      INTEGER K
      ! We need to remember which atoms have been assigned to an assembly
      CHARACTER(MaxPathLength) FileNameToRead, FileNameToWrite
      CHARACTER*255 tDirName, tFileName, tExtension
      INTEGER ExtLength, hSP_out_file
      LOGICAL was_Pawley
      LOGICAL in_spherical_harmonics
      INTEGER wscale_bond, wscale_angle, wscale_flatten

      was_Pawley = .FALSE. ! This is a remnant from when it was still possible
      ! to start from a previously generated .inp file.
      ! In principle, it looks at first glance as if we could now use ext_RR_stage .EQ. 3.
      ! However, it is much better to keep the existing mechanism based on interpreting the
      ! .out file: that way, if the user clicked "Write" twice in a row *without having run TOPAS*,
      ! either by accident or because they feel that scale and background can be refined together,
      ! the correct new .inp will be written out. If we used ext_RR_stage .EQ. 3,
      ! we would inadvertently miss the stage where the atomic coordinates are written out,
      ! which would be a disaster.

      CALL SelectDASHDialog(IDD_SAW_Page7_TOPAS)
      ! Initialise to failure
      WriteTOPASFileRietveld2 = 1
      ExtLength = 3
      CALL SplitPath2(FileNameBase, tDirName, tFileName, tExtension, ExtLength)
      FileNameBase = TRIM(tDirName)//tFileName
      FileNameToRead = TRIM(FileNameBase)//'.out'
      FileNameToWrite = TRIM(FileNameBase)//'.inp'
      hFileTOPAS = 116
      hOutputFile = 117 ! This is the file that is being *read*
      hSP_out_file = 60 ! Special positions output file
      OPEN(UNIT=hFileTOPAS, FILE=FileNameToWrite, STATUS='unknown', ERR=999)
      OPEN(UNIT=hOutputFile, FILE=FileNameToRead, STATUS='unknown', ERR=998)
      is_last_line = .FALSE.
      in_spherical_harmonics = .FALSE.
   10 CONTINUE
      READ(hOutputFile, '(A)', ERR=998, END=40) tLine
      iLen = LEN_TRIM(tLine)
      word_len = LEN(word)
      CALL FirstWord(tLine, word, word_len)
      CALL StrUpperCase(word)
      IF ( in_spherical_harmonics ) THEN
!!!!  prm sh_scale   0.00907`_0.00078 min .0001
!     spherical_harmonics_hkl sh
!       sh_order  6 load sh_Cij_prm {
!          y00   !sh_c00  1.00000
!          y20   sh_c20  -0.65363`_0.15875
!          y22p  sh_c22p -2.09821`_0.24794
!          y40   sh_c40  -1.90493`_0.23022
!          y42p  sh_c42p -0.59773`_0.09433
!          y44p  sh_c44p -1.38602`_0.17287
!          y60   sh_c60   0.11779`_0.02838
!          y62p  sh_c62p  0.03288`_0.18446
!          y64p  sh_c64p -0.25236`_0.14136
!          y66p  sh_c66p  0.19174`_0.15399
!          } 
!!!!  lor_fwhm = sh_scale * sh;
        IF ( (word_len .EQ. 8) .AND. (word(1:8) .EQ. 'LOR_FWHM') ) THEN
          in_spherical_harmonics = .FALSE.
        ELSE
          ! Undocumented TOPAS stuff, such as y20 or k61
          IF ( word(1:1) .EQ. 'Y' .OR. word(1:1) .EQ. 'K' ) THEN
            iPos = StrFind(tLine, iLen, '  sh_', 5)
            IF ( iPos .NE. 0 ) tLine(iPos+1:iPos+1) = '!'
          ENDIF
        ENDIF
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ELSE IF ( (word_len .EQ. 22) .AND. (word(1:22) .EQ. 'PENALTIES_WEIGHTING_K1') ) THEN
        CALL DASHWDialogGetInteger(IDF_K1, K)
        IF ( K .LT. 0 ) K = 1
        tStr = Integer2String(K)
        WRITE(hFileTOPAS, '(A)', ERR=999) 'penalties_weighting_K1 '//TRIM(tStr)
      ELSE IF ( (word_len .EQ. 6) .AND. (word(1:6) .EQ. 'HKL_IS') ) THEN
        was_Pawley = .TRUE.
        WRITE(hFileTOPAS, '(A)', ERR=999) '  str'
      ELSE IF ( (word_len .EQ. 11) .AND. (word(1:11) .EQ. 'SPACE_GROUP') ) THEN
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
        ! This is where the very first Rietveld file is written out
        IF ( was_Pawley ) THEN
          ! #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
          !                             BEGIN
          ! #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
          IF ( DASHWDialogGetCheckBoxLogical(IDC_Scale) ) THEN
            WRITE(hFileTOPAS, '(A)', ERR=999) '    scale @ 0.001'
          ELSE
            WRITE(hFileTOPAS, '(A)', ERR=999) '    scale   0.001'
          ENDIF
! We must call ShowWizardWindowRietveld() here, which will fill
! Xato (and all the RR variables). The Wizard window is suppressed because of the iRietveldMethod flag.
          CALL ShowWizardWindowRietveld(RR_SA_Sol)
! Also need to write out PO if used during SA
          IF ( PrefParExists ) THEN
            WRITE(hFileTOPAS, '(A,F5.3,A,I3,1X,I3,1X,I3,1X,A)', ERR=999) '    PO(@, ', RR_PO, ', , ', PO_Direction(1:3), ')'
            CALL InfoMessage('The preferred orientation that was used during the Simulated Annealing'//CHAR(13)//&
                             'has been written out to the TOPAS .inp file: if you are using'//CHAR(13)//&
                             'a different experimental pattern, you may need to remove this.')
          ELSE
            WRITE(hFileTOPAS, '(A)', ERR=999) "'    PO(@, 1.0, , 0 0 1)"
          ENDIF
          IF ( DASHWDialogGetCheckBoxLogical(IDC_Coordinates) ) THEN
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

          IF ( PutAtomsForSpecailPosition() ) GOTO 997
          OPEN(UNIT=hSP_out_file, FILE="special_positions.out", STATUS='unknown', ERR=996)
       45 CONTINUE
          READ(hSP_out_file, '(A)', ERR=996, END=50) tLine
          WRITE(hFileTOPAS, '(A)', ERR=999) TRIM(tLine)
          GOTO 45
       50 CONTINUE
          CLOSE(hSP_out_file)
          CALL DASHWDialogGetInteger(IDF_INTEGER_TOPAS_WSCALE_DIST, wscale_bond)
          CALL DASHWDialogGetInteger(IDF_INTEGER_TOPAS_WSCALE_ANGLE, wscale_angle)
          CALL DASHWDialogGetInteger(IDF_INTEGER_TOPAS_WSCALE_FLAT, wscale_flatten)
          WRITE(hFileTOPAS, '(A)', ERR=999) '    prm !bond_width 0'
          WRITE(hFileTOPAS, '(A,I10)', ERR=999) '    prm !bond_weight ', wscale_bond !10000
          WRITE(hFileTOPAS, '(A)', ERR=999) '    prm !angle_width 1'
          WRITE(hFileTOPAS, '(A,I10)', ERR=999) '    prm !angle_weight ', wscale_angle !1
          WRITE(hFileTOPAS, '(A)', ERR=999) '    prm !flatten_width 0'
          WRITE(hFileTOPAS, '(A,I10)', ERR=999) '    prm !flatten_weight ', wscale_flatten !1000000
          IF ( PutRestraints(hFileTOPAS, TOPASWriteDistance, TOPASWriteAngle, TOPASWritePlane) ) GOTO 999
          ! #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@
          !                            END
          ! #@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@

          ! Probably need to improve mechanism to indicate end
          is_last_line = .TRUE.
        ENDIF
      ELSE IF ( (word_len .EQ. 3) .AND. (word(1:3) .EQ. 'BKG') ) THEN
        IF ( DASHWDialogGetCheckBoxLogical(IDC_Background) ) THEN
          WRITE(hFileTOPAS, '(A)', ERR=999) '  bkg @ '
        ELSE
          WRITE(hFileTOPAS, '(A)', ERR=999) '  bkg '
        ENDIF 
      ELSE IF ( (word_len .EQ. 5) .AND. (word(1:5) .EQ. 'SCALE') ) THEN
        IF ( DASHWDialogGetCheckBoxLogical(IDC_Scale) ) THEN
          tLine(11:11) = '@'
        ELSE
          tLine(11:11) = ' '
        ENDIF 
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ELSE IF ( (word_len .EQ. 11) .AND. (word(1:11) .EQ. 'OUT_CIF_STR') ) THEN
        ! Do nothing: it will be added at the end if necessary
      ELSE IF ( (word_len .EQ. 16) .AND. (word(1:16) .EQ. 'OUT_CIF_STR_UISO') ) THEN
        ! Do nothing: it will be added at the end if necessary
      ELSE IF ( (word_len .EQ. 11) .AND. (word(1:11) .EQ. 'OUT_X_YCALC') ) THEN
        ! Do nothing: it will be added at the end if necessary
      ELSE IF ( (word_len .EQ. 10) .AND. (word(1:10) .EQ. 'OUT_X_YOBS') ) THEN
        ! Do nothing: it will be added at the end if necessary
      ELSE IF ( (word_len .EQ. 16) .AND. (word(1:16) .EQ. 'OUT_X_DIFFERENCE') ) THEN
        ! Do nothing: it will be added at the end if necessary
      ELSE IF ( (word_len .EQ. 29) .AND. (word(1:29) .EQ. 'OUT_YOBS_YCALC_AND_DIFFERENCE') ) THEN
        ! Do nothing: it will be added at the end if necessary
      ELSE IF ( (word_len .EQ. 11) .AND. (word(1:11) .EQ. 'OUT_PROFILE') ) THEN
        ! Do nothing: it will be added at the end if necessary
      ELSE IF ( (word_len .EQ. 8) .AND. (word(1:8) .EQ. 'OUT_TICK') ) THEN
        ! Do nothing: it will be added at the end if necessary
      ELSE IF ( (word_len .EQ. 19) .AND. (word(1:19) .EQ. 'OUT_POWDERDATABLOCK') ) THEN
        ! Do nothing: it will be added at the end if necessary
      ELSE IF ( ((word_len .EQ. 9) .AND. (word(1:9) .EQ. 'DO_ERRORS')) &
          .OR.  ((word_len .EQ. 10) .AND. (word(1:10) .EQ. "'DO_ERRORS")) ) THEN
        IF ( DASHWDialogGetCheckBoxLogical(IDC_IncludeESDs) ) THEN
          WRITE(hFileTOPAS, '(A)', ERR=999) 'do_errors'
        ELSE
          WRITE(hFileTOPAS, '(A)', ERR=999) "'do_errors"
        ENDIF
      ELSE IF ( (word_len .EQ. 3) .AND. (word(1:3) .EQ. 'PRM') ) THEN
        ! Need to check if this is the "prm sh_scale 1.0 min 0.0001" line.
        iPos = StrFind(tLine, iLen, 'sh_scale', 8)
        IF ( iPos .NE. 0 ) THEN
          tLine(iPos-1:iPos-1) = '!'
          in_spherical_harmonics = .TRUE.
        ELSE
          ! Need to check if this is the "prm bnonh 3.000" line.
          iPos = StrFind(tLine, iLen, 'bnonh', 5)
          IF ( iPos .NE. 0 ) THEN
            IF ( DASHWDialogGetCheckBoxLogical(IDC_Biso) ) THEN
              tLine(iPos-1:iPos-1) = ' '
            ELSE
              tLine(iPos-1:iPos-1) = '!'
            ENDIF
          ELSE
            ! Need to check if the line is for special_position eg "prm dash_2 0.4670".
            iPos = StrFind(tLine, iLen, 'dash_', 5)
            IF ( iPos .NE. 0 ) THEN
              IF ( DASHWDialogGetCheckBoxLogical(IDC_Coordinates) ) THEN
                tLine(iPos-1:iPos-1) = ' '
              ELSE
                tLine(iPos-1:iPos-1) = '!'
              ENDIF
              GOTO 20
            ENDIF
            IF ( INDEX(tLine, '!bond_weight') .GT. 0 ) THEN
              CALL DASHWDialogGetInteger(IDF_INTEGER_TOPAS_WSCALE_DIST, wscale_bond)
              WRITE(tLine, '(A,I10)', ERR=999) '    prm !bond_weight ', wscale_bond
            ELSE IF ( INDEX(tLine, '!angle_weight') .GT. 0 ) THEN
              CALL DASHWDialogGetInteger(IDF_INTEGER_TOPAS_WSCALE_ANGLE, wscale_angle)
              WRITE(tLine, '(A,I10)', ERR=999) '    prm !angle_weight ', wscale_angle
            ELSE IF ( INDEX(tLine, '!flatten_weight') .GT. 0 ) THEN
              CALL DASHWDialogGetInteger(IDF_INTEGER_TOPAS_WSCALE_FLAT, wscale_flatten)
              WRITE(tLine, '(A,I10)', ERR=999) '    prm !flatten_weight ', wscale_flatten
            ENDIF
          ENDIF
        ENDIF
 20     CONTINUE
        WRITE(hFileTOPAS, '(A)', ERR=999) TRIM(tLine)
      ELSE IF ( (word_len .EQ. 4) .AND. (word(1:4) .EQ. 'SITE') ) THEN
        ! We could read the fractional coordinates back in and use them to
        ! overlay the refined solution with the original one.
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ELSE IF ( (word_len .EQ. 5) .AND. (word(1:5) .EQ. 'MACRO') ) THEN
        ! Check if rest is ref_flag
        IF ( StrFind(tLine, iLen, 'ref_flag', 8) .NE. 0 ) THEN
          IF ( DASHWDialogGetCheckBoxLogical(IDC_Coordinates) ) THEN
            WRITE(hFileTOPAS, '(A)', ERR=999) '    macro ref_flag { @ }'
          ELSE
            WRITE(hFileTOPAS, '(A)', ERR=999) '    macro ref_flag {   }'
          ENDIF
        ENDIF
      ELSE IF ( (word_len .EQ. 2) .AND. (word(1:2) .EQ. 'PO') ) THEN
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ELSE IF ( (word_len .EQ. 1) .AND. (word(1:1) .EQ. 'A') ) THEN
        iPos = StrFind(tLine, iLen, 'uc_prm', 6)
        IF ( iPos .NE. 0 ) THEN
          IF ( DASHWDialogGetCheckBoxLogical(IDC_IncludeESDs) ) THEN
            tLine(iPos-1:iPos-1) = ' '
          ELSE
            tLine(iPos-1:iPos-1) = '!'
          ENDIF
        ELSE
          iPos = StrFind(tLine, iLen, 'a', 1)
          IF ( DASHWDialogGetCheckBoxLogical(IDC_IncludeESDs) ) THEN
            tLine(iPos+3:iPos+3) = '@'
          ELSE
            tLine(iPos+3:iPos+3) = ' '
          ENDIF
        ENDIF
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ELSE IF ( (word_len .EQ. 1) .AND. (word(1:1) .EQ. 'B') ) THEN
        IF ( .NOT. CellParConstrained(2) ) THEN
          iPos = StrFind(tLine, iLen, 'b', 1)
          IF ( DASHWDialogGetCheckBoxLogical(IDC_IncludeESDs) ) THEN
            tLine(iPos+3:iPos+3) = '@'
          ELSE
            tLine(iPos+3:iPos+3) = ' '
          ENDIF
        ENDIF
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ELSE IF ( (word_len .EQ. 1) .AND. (word(1:1) .EQ. 'C') ) THEN
        IF ( .NOT. CellParConstrained(3) ) THEN
          iPos = StrFind(tLine, iLen, 'c', 1)
          IF ( DASHWDialogGetCheckBoxLogical(IDC_IncludeESDs) ) THEN
            tLine(iPos+3:iPos+3) = '@'
          ELSE
            tLine(iPos+3:iPos+3) = ' '
          ENDIF
        ENDIF
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ELSE IF ( (word_len .EQ. 2) .AND. (word(1:2) .EQ. 'AL') ) THEN
        iPos = StrFind(tLine, iLen, 'uc_ang_prm', 10)
        IF ( iPos .NE. 0 ) THEN
          IF ( DASHWDialogGetCheckBoxLogical(IDC_IncludeESDs) ) THEN
            tLine(iPos-1:iPos-1) = ' '
          ELSE
            tLine(iPos-1:iPos-1) = '!'
          ENDIF
        ELSE
          iPos = StrFind(tLine, iLen, 'al', 2)
          IF ( DASHWDialogGetCheckBoxLogical(IDC_IncludeESDs) .AND. (.NOT. CellParConstrained(4)) ) THEN
            tLine(iPos+3:iPos+3) = '@'
          ELSE
            tLine(iPos+3:iPos+3) = ' '
          ENDIF
        ENDIF
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ELSE IF ( (word_len .EQ. 2) .AND. (word(1:2) .EQ. 'BE') ) THEN
        iPos = StrFind(tLine, iLen, 'uc_ang_prm', 10)
        IF ( iPos .EQ. 0 ) THEN
          iPos = StrFind(tLine, iLen, 'be', 2)
          IF ( DASHWDialogGetCheckBoxLogical(IDC_IncludeESDs) .AND. (.NOT. CellParConstrained(5)) ) THEN
            tLine(iPos+3:iPos+3) = '@'
          ELSE
            tLine(iPos+3:iPos+3) = ' '
          ENDIF
        ENDIF
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ELSE IF ( (word_len .EQ. 2) .AND. (word(1:2) .EQ. 'GA') ) THEN
        iPos = StrFind(tLine, iLen, 'uc_ang_prm', 10)
        IF ( iPos .EQ. 0 ) THEN
          iPos = StrFind(tLine, iLen, 'ga', 2)
          IF ( DASHWDialogGetCheckBoxLogical(IDC_IncludeESDs) .AND. (.NOT. CellParConstrained(6)) ) THEN
            tLine(iPos+3:iPos+3) = '@'
          ELSE
            tLine(iPos+3:iPos+3) = ' '
          ENDIF
        ENDIF
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ELSE IF ( word(1:1) .EQ. "'" ) THEN ! It's a comment
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ELSE
        ! Change all the @'s to spaces.
        ! This switches off refinement of zero-point error, CS_L, CS_G, Strain_L, Strain_G
        DO iStrPos = 1, iLen
          IF (tLine(iStrPos:iStrPos) .EQ. '@') &
            tLine(iStrPos:iStrPos) = ' '
        ENDDO
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ENDIF
      IF ( .NOT. is_last_line ) GOTO 10
   40 CLOSE(hOutputFile)
      IF ( DASHWDialogGetCheckBoxLogical(IDC_WriteCIF) ) THEN
!        WRITE(hFileTOPAS, '(A)', ERR=999) '    Out_CIF_STR("'//TRIM(FileNameBase)//'.cif")'
        WRITE(hFileTOPAS, '(A)', ERR=999) '    Out_CIF_STR_Uiso("'//TRIM(FileNameBase)//'.cif")'
      ENDIF
      IF ( DASHWDialogGetCheckBoxLogical(IDC_SaveProfile) ) THEN
        WRITE(hFileTOPAS, '(A)', ERR=999) '    Out_Profile("'//TRIM(FileNameBase)//'_plot.pro")'
        WRITE(hFileTOPAS, '(A)', ERR=999) '    Out_Tick("'//TRIM(FileNameBase)//'_plot.tic")'
        WRITE(hFileTOPAS, '(A)', ERR=999) '    Out_PowderDataBlock("'//TRIM(FileNameBase)//'_plot.pcif")'
      ENDIF
!      WRITE(hFileTOPAS, '(A)', ERR=999) '    Out_X_Ycalc("'//TRIM(FileNameBase)//'.pp")'
!      WRITE(hFileTOPAS, '(A)', ERR=999) '    Out_Yobs_Ycalc_and_Difference("'//TRIM(FileNameBase)//'_1.xco")'
      WriteTOPASFileRietveld2 = 0
      CLOSE(hFileTOPAS)
      RETURN
  999 CALL ErrorMessage("Error writing TOPAS input file (Rietveld).")
      CLOSE(hFileTOPAS)
      RETURN
  998 CALL ErrorMessage("Error reading TOPAS output file (Rietveld).")
      CLOSE(hOutputFile)
      CLOSE(hFileTOPAS)
  997 CALL ErrorMessage("Error writing special positions input file (Rietveld).")
      CLOSE(hFileTOPAS)
  996 CALL ErrorMessage("Error reading special positions output file (Rietveld).")
      CLOSE(hSP_out_file)
      CLOSE(hFileTOPAS)

      END FUNCTION WriteTOPASFileRietveld2
!
!*****************************************************************************
!
!      LOGICAL FUNCTION CheckTOPASFileName (name)
! 
!      USE TAVAR
!
!      IMPLICIT NONE
!
!      CHARACTER*(*), INTENT (INOUT) :: name
!
!      CHARACTER(MaxPathLength) :: DirName
!      CHARACTER(MaxPathLength) :: FileName
!      CHARACTER(3) :: Extension
!      INTEGER iLen, dLen, iPos
!      LOGICAL something_changed

      ! It turns out that TOPAS cannot cope with file names that have dots in them,
      ! e.g. the following gives an error message:
      ! "Daresbury9.1_Nov2006.inp"
      ! The error message refers to "Daresbury9", indicating that everything
      ! after the first "." was assumed to be the file extension.
!      CheckTOPASFileName = .FALSE.
!10    CONTINUE
!      ExtLength = 3
!      CALL SplitPath2(name, DirName, FileName, Extension, ExtLength)
!      iLen = LEN_TRIM(FileName)
!      iPos = StrFind(FileName, iLen, '.', 1)
!      IF ( iPos .NE. 0 ) THEN
!        FileName(iPos:iPos) = "_"
!        name = TRIM(DirName)//FileName(1:iLen)//'.inp'
!        CheckTOPASFileName = .TRUE.
!        ! There could be more "." in the file name
!        GOTO 10
!      ENDIF
!
!      END FUNCTION CheckTOPASFileName
!
!*****************************************************************************
!
      LOGICAL FUNCTION TOPASWriteDistance (hFile, iAtom1, iAtom2, iElement1, iElement2, Distance)

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: hFile, iAtom1, iAtom2, iElement1, iElement2
      REAL, INTENT (IN) :: Distance

      CHARACTER(7) label1, label2

      TOPASWriteDistance = .TRUE.
      CALL GenerateAtomLabel(iElement1, iAtom1, label1)
      CALL GenerateAtomLabel(iElement2, iAtom2, label2)
      WRITE(hFile, '(A,A7,1X,A7,A,F9.5,A)', ERR=999) '    Distance_Restrain(', label1, label2, &
                ', ', Distance, ', 1.0, bond_width, bond_weight)'
      TOPASWriteDistance = .FALSE.
  999 RETURN

      END FUNCTION TOPASWriteDistance
!
!*****************************************************************************
!
      LOGICAL FUNCTION TOPASWriteAngle (hFile, iAtom1, iAtom, iAtom2, &
                                        iElement1, iElement, iElement2, Angle)

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: hFile, iAtom1, iAtom, iAtom2, iElement1, iElement, iElement2
      REAL, INTENT (IN) :: Angle

      CHARACTER(7) label1, Label, label2

      TOPASWriteAngle = .TRUE.
      CALL GenerateAtomLabel(iElement1, iAtom1, label1)
      CALL GenerateAtomLabel(iElement, iAtom, Label)
      CALL GenerateAtomLabel(iElement2, iAtom2, label2)
      WRITE(hFile, '(A,A7,1X,A7,1X,A7,A,F9.5,A)', ERR=999) '    Angle_Restrain(', label1, &
                    Label, label2, ', ', Angle, ', 1.0, angle_width, angle_weight)'
      TOPASWriteAngle = .FALSE.
  999 RETURN

      END FUNCTION TOPASWriteAngle
!
!*****************************************************************************
!
      LOGICAL FUNCTION TOPASWritePlane (hFile, n, members, iElements)

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: hFile, n, members(n), iElements(n)

      INTEGER I
      CHARACTER*7 Label

      TOPASWritePlane = .TRUE.
      WRITE(hFile, '(A$)', ERR=999) '    Flatten('
      DO I = 1, n
        CALL GenerateAtomLabel(iElements(I), members(I), Label)
        WRITE(hFile, '(1X,A7$)', ERR=999) Label
      ENDDO
! There are two flatten macros in TOPAS3:
!   Flatten(sites, c, t_calc, tol, wscale)
!   Flatten(sites, t_calc, tol, wscale) and
! The second one is only a wrapper of the first. But it seems the arguments t_calc
! and tol were put in wrong place. Call the first one instead 
!      WRITE(hFile, '(A)', ERR=999) ', 0, flatten_width, flatten_weight)'
      WRITE(hFile, '(A)', ERR=999) ',, 0, flatten_width, flatten_weight)'
      TOPASWritePlane = .FALSE.
  999 RETURN

      END FUNCTION TOPASWritePlane
!
!*****************************************************************************
!
