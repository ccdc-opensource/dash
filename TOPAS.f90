!
!*****************************************************************************
!
      SUBROUTINE CopyPattern2Backup

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

      TANOBS = NOBS
      TAXOBS(1:NOBS) = XOBS(1:NOBS)
      TAYOBS(1:NOBS) = YOBS(1:NOBS)
      TAEOBS(1:NOBS) = EOBS(1:NOBS)
      TALAMBDA = ALambda
      TARADIATION = JRadOption

      END SUBROUTINE CopyPattern2Backup
!
!*****************************************************************************
!
      SUBROUTINE CopyBackup2Pattern

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
      ! Must also restore Rebin_Profile
      LBIN = 1
      CALL Rebin_Profile()

      END SUBROUTINE CopyBackup2Pattern
!
!*****************************************************************************
!
      SUBROUTINE DealWithWizardTOPAS

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      INTEGER                  OFBN_Len
      CHARACTER(MaxPathLength)           OutputFilesBaseName
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OFBN_Len, OutputFilesBaseName, SA_RunNumberStr

      INTEGER, EXTERNAL :: WriteTOPASFileRietveld
      INTEGER, EXTERNAL :: DiffractionFileBrowse
      LOGICAL, EXTERNAL :: FnPatternOK
      CHARACTER(MaxPathLength) :: TOPASFileName
      CHARACTER(LEN=45) :: FILTER
      INTEGER iFlags

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_RR_TOPAS)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL Unload_RR_TOPAS
              CALL WDialogLoad(IDD_SAW_Page6a)
              IF ( iRietveldMethod .EQ. 1 ) THEN
                CALL WDialogPutRadioButton(IDF_RADIO1)
              ELSE
                CALL WDialogPutRadioButton(IDF_RADIO2)
              ENDIF
              CALL WizardWindowShow(IDD_SAW_Page6a)
            CASE (IDCANCEL, IDCLOSE)
              CALL Unload_RR_TOPAS
              CALL EndWizardPastPawley
            CASE (IDB_Write_Pawley)
              For_TOPAS = .TRUE.
              CALL WizardWindowShow(IDD_PW_Page3)


            CASE (IDB_Browse) ! The TOPAS Pawley refinement output file

            CASE (IDB_Write_RR)
              iFlags = SaveDialog + AppendExt + PromptOn
              FILTER = 'TOPAS input file (*.inp)|*.inp|'
                  TOPASFileName = OutputFilesBaseName(1:LEN_TRIM(OutputFilesBaseName))//'.inp'
              CALL WSelectFile(FILTER, iFlags, TOPASFileName, 'Save TOPAS input file (Rietveld)')
              IF ((WinfoDialog(4) .EQ. CommonOk) .AND. (LEN_TRIM(TOPASFileName) .NE. 0)) THEN
                IF ( WriteTOPASFileRietveld(TOPASFileName) .EQ. 0 ) THEN
                  TOPAS_stage = 1 ! I think this variable is redundant
                  CALL WDialogFieldStateLogical(IDB_Write_RR,     .FALSE.)
                  CALL WDialogFieldStateLogical(IDB_Write_Pawley, .TRUE.)
                ENDIF
              ENDIF
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

      ! The way this code has curently been written, this routine can only be called
      ! from one of the Wizard windows as part of a "For_TOPAS" Rietveld refinement
      ! Initialise to failure
      WriteTOPASFilePawley = 1
      hFileTOPAS = 116
      OPEN(UNIT=hFileTOPAS,FILE=tFileName,STATUS='unknown',ERR=999)
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
      WRITE(hFileTOPAS, '(A)', ERR=999) '  LP_Factor( 26)'
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
      WRITE(hFileTOPAS, '(A)', ERR=999) '    space_group "'//SGHMaStr(NumberSGTable)(1:LEN_TRIM(SGHMaStr(NumberSGTable)))//'"' 
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
      INTEGER FUNCTION WriteTOPASFileRietveld(tFileName)

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: tFileName

      INTEGER hFileTOPAS

      ! Initialise to failure
      WriteTOPASFileRietveld = 1
      hFileTOPAS = 117
      OPEN(UNIT=hFileTOPAS,FILE=tFileName,STATUS='unknown',ERR=999)
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
      ! We need to write out the data to a temporary file, although this has the downside that
      ! the we can only use the background-subtracted data, and only out to the resolution
      ! that we used for Pawley... Hmmm... must come up with something different.
      WRITE(hFileTOPAS, '(A)', ERR=999) 'gof 1.0'

      WriteTOPASFileRietveld = 0
      RETURN
  999 CALL ErrorMessage("Error writing TOPAS input file (Rietveld)")

      END FUNCTION WriteTOPASFileRietveld
!
!*****************************************************************************
!
