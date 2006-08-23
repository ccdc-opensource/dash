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
      CHARACTER(LEN=60) :: FILTER
      INTEGER iFlags, iFType
      CHARACTER(LEN=MaxPathLength) tFileName

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
              CALL Unload_RR_TOPAS
              CALL WizardWindowShow(IDD_PW_Page3)
            CASE (IDB_Browse) ! The TOPAS Pawley refinement output file
              iFlags = LoadDialog + DirChange + PromptOn + AppendExt
              FILTER = 'All files (*.*)|*.*|'//&
                       'TOPAS ouput files (*.out)|*.out|'
              tFileName = ' '
! iFType specifies which of the file types in the list is the default
              iFType = 2
              CALL WSelectFile(FILTER, iFlags, tFileName, 'Open TOPAS output file', iFType)
! Did the user press cancel?
              IF ( (WInfoDialog(ExitButtonCommon) .EQ. CommonOK) .AND. (LEN_TRIM(tFileName) .NE. 0) ) THEN
                TOPAS_output_file_name = tFileName
                TOPAS_stage = 3 ! I think this variable is redundant
                CALL WDialogFieldStateLogical(IDB_Browse,   .FALSE.)
                CALL WDialogFieldStateLogical(IDB_Write_RR, .TRUE.)
              ENDIF
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
      CHARACTER*(40) space_group_str
      INTEGER tStrLen

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
      WRITE(hFileTOPAS, '(A)', ERR=999) "' When editing this file, leave space group as the last keyword: DASH uses it to detect the end of the input file" 

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
      INTEGER FUNCTION WriteTOPASFileRietveld(tFileName)

      USE VARIABLES
      USE ATMVAR
      USE ZMVAR
      USE PO_VAR
      USE RRVAR
      USE SAMVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: tFileName

      INTEGER                  OFBN_Len
      CHARACTER(MaxPathLength)           OutputFilesBaseName
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OFBN_Len, OutputFilesBaseName, SA_RunNumberStr

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

      INTEGER, EXTERNAL :: StrFind
      INTEGER hFileTOPAS, hOutputFile
      INTEGER iLen, iPos, iStrPos
      LOGICAL is_last_line
      CHARACTER(255) tLine
      CHARACTER(6) hkl_str
      CHARACTER(11) space_group_str
      CHARACTER(5) b_str
      INTEGER b_str_len, tElement, J
      INTEGER ii, iiact, iTotal, iFrg, iOrig, iAtom
      REAL    distance

      ! Initialise to failure
      WriteTOPASFileRietveld = 1
      hkl_str = 'hkl_Is'
      space_group_str = 'space_group'
      hFileTOPAS = 116
      hOutputFile = 117 ! This is the file that is being *read*
      OPEN(UNIT=hFileTOPAS, FILE=tFileName, STATUS='unknown', ERR=999)
      OPEN(UNIT=hOutputFile, FILE=TOPAS_output_file_name, STATUS='unknown', ERR=998)
      WRITE(hFileTOPAS, '(A)', ERR=999) 'penalties_weighting_K1 5'
      ! Basically, read and write every line up to and including the space_group line.
      is_last_line = .FALSE.
   10 CONTINUE
      READ(hOutputFile, '(A)', ERR=998) tLine
      iLen = LEN_TRIM(tLine)
      iPos = StrFind(tLine, iLen, hkl_str, 6)
      IF ( iPos .NE. 0 ) THEN
        WRITE(hFileTOPAS, '(A)', ERR=999) '  str'
      ELSE
        iPos = StrFind(tLine, iLen, space_group_str, 11)
        is_last_line = ( iPos .NE. 0 ) 
        ! We only want to refine the overall scale, so change all the other @'s to spaces.
        DO iStrPos = 1, iLen
          IF (tLine(iStrPos:iStrPos) .EQ. '@') &
            tLine(iStrPos:iStrPos) = ' '
        ENDDO
        WRITE(hFileTOPAS, '(A)', ERR=999) tLine(1:iLen)
      ENDIF
      IF ( .NOT. is_last_line ) GOTO 10
      CLOSE(hOutputFile)
      WRITE(hFileTOPAS, '(A)', ERR=999) '    scale @ 1.0'
! We must call ShowWizardWindowRietveld() here, which will fill
! Xato (and all the RR variables). The Wizard window is suppressed because of the For_TOPAS flag.
      CALL ShowWizardWindowRietveld(RR_SA_Sol)
! Also need to write out PO if used during SA
      IF ( PrefParExists ) THEN
        WRITE(hFileTOPAS, '(A,F5.3,A,I3,1X,I3,1X,I3,1X,A)', ERR=999) '    PO( , ', RR_PO, ', , ', PO_Direction(1), PO_Direction(2), PO_Direction(3), ')'
      ELSE
        WRITE(hFileTOPAS, '(A)', ERR=999) '    PO( , 1.0, , 1 0 0)'
      ENDIF
      WRITE(hFileTOPAS, '(A)', ERR=999) "'   Insert @ in the curly brackets to refine structural parameters"
      WRITE(hFileTOPAS, '(A)', ERR=999) '    macro ref_flag {   }'
      iiact = 0
      itotal = 0
      DO iFrg = 1, nFrag
        itotal = iiact
        DO iAtom = 1, natoms(iFrg)
          iiact = iiact + 1
          iOrig = izmbid(iAtom,iFrg)
          ii = OrderedAtm(itotal + iOrig)
          tElement = zmElementCSD(iOrig,iFrg)
          IF ( (tElement .EQ. 2) .OR. (tElement .EQ. 27) ) THEN
            ! Hydrogen or deuterium
            b_str = 'bh'
            b_str_len = 2
          ELSE
            b_str = 'bnonh'
            b_str_len = 5
          ENDIF
          WRITE(hFileTOPAS, '(A,A5,A,F9.5,A,F9.5,A,F9.5,A,F6.3,A,F6.3)', ERR=999) '    site  ', OriginalLabel(iOrig,iFrg), ' x ref_flag ', &
            Xato(1,ii), ' y ref_flag ', Xato(2,ii), ' z ref_flag ', Xato(3,ii), ' occ '//ElementStr(tElement)//'  ', & 
            occ(iOrig,iFrg), ' beq '//b_str(1:b_str_len), tiso(iOrig,iFrg)
        ENDDO
      ENDDO
      WRITE(hFileTOPAS, '(A)', ERR=999) '    prm !bond_width 0'
      WRITE(hFileTOPAS, '(A)', ERR=999) '    prm !bond_weight 10000'
      WRITE(hFileTOPAS, '(A)', ERR=999) '    prm !angle_width 1'
      WRITE(hFileTOPAS, '(A)', ERR=999) '    prm !angle_weight 1'
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
        DO J = 1, nbocry
          CALL PLUDIJ(bond(J,1), bond(J,2), distance)
          WRITE(hFileTOPAS, '(A,A5,1X,A5,A,F9.5,A)', ERR=999) 'Distance_Restrain(', OriginalLabel(bond(J,1),iFrg), OriginalLabel(bond(J,2),iFrg), &
            ', ', distance, ', 1.0, bond_width, bond_weight)'
        ENDDO

      ENDDO


      WRITE(hFileTOPAS, '(A)', ERR=999) '    do_errors'
      WRITE(hFileTOPAS, '(A)', ERR=999) '    Out_CIF_STR("'//OutputFilesBaseName(1:OFBN_Len)//'.cif")'
      WriteTOPASFileRietveld = 0
      CLOSE(hFileTOPAS)
      RETURN
  999 CALL ErrorMessage("Error writing TOPAS input file (Rietveld)")
      CLOSE(hFileTOPAS)
      RETURN
  998 CALL ErrorMessage("Error reading TOPAS output file")
      CLOSE(hOutputFile)
      CLOSE(hFileTOPAS)

      END FUNCTION WriteTOPASFileRietveld
!
!*****************************************************************************
!
