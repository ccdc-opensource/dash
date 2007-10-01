!
!*****************************************************************************
!
      INTEGER FUNCTION Launch_GSAS(input_file_name, fg_EXPGUI)

      USE DRUID_HEADER
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: input_file_name
      LOGICAL, INTENT (IN   ) :: fg_EXPGUI

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      INTEGER, EXTERNAL :: FillOptStr, CheckEXPGUIExe
      CHARACTER*20, EXTERNAL :: Integer2String

      INTEGER nCycle, hFile, ExtLength
      PARAMETER ( hFile = 116 )
      REAL factr_dist, factr_angle, factr_plane
      CHARACTER(MaxPathLength) tScriptName, tDirName, tFileNameRoot
      CHARACTER*40 opt_str
      CHARACTER*8 tExtension

      ! Initialise to failure
      Launch_GSAS = 1
      IF ( .NOT. fg_EXPGUI ) THEN
        IF ( FillOptStr(opt_str) .LE. 0 ) GOTO 996
        IF ( CheckEXPGUIExe(EXPGUIEXE, tScriptName, fg_EXPGUI) .NE. 0 ) RETURN
        CALL PushActiveWindowID
        CALL WDialogSelect(IDD_SAW_Page7_GSAS)
        CALL WDialogGetInteger(IDF_NCYCL, nCycle)
        CALL WDialogGetReal(IDF_REAL_GSAS_FACTR_DIST, factr_dist)
        CALL WDialogGetReal(IDF_REAL_GSAS_FACTR_ANGLE, factr_angle)
        CALL WDialogGetReal(IDF_REAL_GSAS_FACTR_PLANE, factr_plane)
        CALL PopActiveWindowID
        ExtLength = MIN(LEN_TRIM(input_file_name), LEN(tExtension))
        CALL SplitPath2(input_file_name, tDirName, tFileNameRoot, tExtension, ExtLength)
        OPEN(UNIT=hFile, FILE=TRIM(tDirName)//TRIM(tFileNameRoot)//'.fct', &
           STATUS='unknown', ERR=993)
        WRITE(hFile, '(3(A,F10.2))', ERR=993) 'FACT DIST ', factr_dist, ' ANGL ', factr_angle, &
                                              ' PLAN ', factr_plane
        CLOSE(hFile)
        ! Launch GSAS vis script and wait for it to return
        CALL InfoError(1) ! Clear errors
        CALL IOSCommand('"'//TRIM(EXPGUIEXE)//'" '// &
                        '"'//TRIM(tScriptName)//'" '// &
                        '"'//TRIM(input_file_name)//'" '// &
                        opt_str//' '//Integer2String(nCycle), ProcBlocked)
        IF ( InfoError(1) .NE. 0 ) GOTO 998
!        CALL IOsDeleteFile(TRIM(tDirName)//TRIM(tFileNameRoot)//'.fct')
      ELSE
        IF ( CheckEXPGUIExe(EXPGUIEXE, tScriptName, fg_EXPGUI) .NE. 0 ) RETURN
        CALL InfoError(1) ! Clear errors
!        CALL IOSCommand('CMD.EXE /C "'//TRIM(tFileName)//'"', ProcSilent)
        CALL IOSCommand('"'//TRIM(EXPGUIEXE)//'" '// &
                        '"'//TRIM(tScriptName)//'" '// &
                        '"'//TRIM(input_file_name)//'"', ProcSilent)
        IF ( InfoError(1) .NE. 0 ) GOTO 995
      ENDIF
      CALL WCursorShape(CurCrossHair)
      Launch_GSAS = 0
      RETURN

 993  CALL ErrorMessage('Error while saving weights on restraints to a tmp file.')
      CLOSE(hFile)
      RETURN
 995  CALL ErrorMessage('DASH could not launch EXPGUI. Make sure it is installed.')
      RETURN
 996  CALL ErrorMessage('DASH could not launch GSAS vis EXPGUI script. '// &
                        'No refine option selected.')
      CALL WCursorShape(CurCrossHair)
      RETURN
 998  CALL ErrorMessage('DASH could not launch GSAS vis EXPGUI script. '// &
                        'The executable'//CHAR(13)//&
                        'is currently configured to launch the program '// &
                        TRIM(EXPGUIEXE)//CHAR(13)//&
                        'This can be changed in the Configuration... window'//CHAR(13)//&
                        'under Options in the menu bar.')
      CALL WCursorShape(CurCrossHair)
      RETURN

      END FUNCTION Launch_GSAS
!
!*****************************************************************************
!
      INTEGER FUNCTION CheckEXPGUIExe (ExpguiExe, ScriptName, fg_EXPGUI)

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (  OUT) :: ScriptName
      CHARACTER*(*), INTENT (IN   ) :: ExpguiExe
      LOGICAL,       INTENT (IN   ) :: fg_EXPGUI

      CHARACTER*(*) SCRIPT_FILE, SCRIPT_DIR
!TMP      PARAMETER ( SCRIPT_FILE = 'dashexpcli', SCRIPT_DIR = 'dashexpcli\' )
      PARAMETER ( SCRIPT_FILE = 'expcli', SCRIPT_DIR = 'expcli\' )
      CHARACTER(MaxPathLength) :: tDirName, tFileName
      LOGICAL exists
      INTEGER L, ExtLength
      CHARACTER*8 tExtension

      ! Initialise to failure
      CheckEXPGUIExe = 1

      IF ( LEN_TRIM(ExpguiExe) .EQ. 0 ) GOTO 997
      INQUIRE(FILE=ExpguiExe, EXIST=exists)
      IF ( .NOT. exists ) GOTO 998
      ExtLength = len(tExtension)
      CALL SplitPath2(ExpguiExe, tDirName, tFileName, tExtension, ExtLength)
      CALL StrUpperCase(tExtension)
      IF ( tExtension .NE. 'EXE' ) goto 998

      L = LEN_TRIM(tDirName)
      IF ( fg_EXPGUI ) THEN
        ScriptName = tDirName(:L)//'expgui'
        INQUIRE(FILE=ScriptName, EXIST=exists)
        IF ( exists ) GOTO 10
        CALL ErrorMessage('DASH could not launch EXPGUI. Make sure it is installed.')
        RETURN
      ENDIF
      ScriptName = tDirName(:L)//SCRIPT_FILE
      INQUIRE(FILE=ScriptName, EXIST=exists)
      IF ( exists ) GOTO 10
      ! remove tailing "\"
      IF ( L .GT. 1 .AND. tDirName(L:L) .EQ. '\' .AND. tDirName(L-1:L-1) .NE. ':' ) L = L - 1
      ScriptName = tDirName(:L) 
      CALL SplitPath(ScriptName, tDirName, tFileName)
      ScriptName = TRIM(tDirName)//SCRIPT_DIR//SCRIPT_FILE
      INQUIRE(FILE=ScriptName, EXIST=exists)
      IF ( exists ) GOTO 10
      ScriptName = TRIM(InstallationDirectory)//SCRIPT_DIR//SCRIPT_FILE
      INQUIRE(FILE=ScriptName, EXIST=exists)
      IF ( .NOT. exists ) GOTO 999
 10   CheckEXPGUIExe = 0
      RETURN

 997 CALL ErrorMessage('DASH could not launch GSAS vis EXPGUI script. '// &
                        'No executable is currently'//CHAR(13)//&
                        'specified. This can be changed in the Configuration... window'//CHAR(13)//&
                        'under Options in the menu bar.')
      RETURN
 998  CALL ErrorMessage('DASH could not launch GSAS vis EXPGUI script. '// &
                        'The executable'//CHAR(13)//&
                        'is currently configured to launch the program: "'// &
                        TRIM(ExpguiExe)//'"'//CHAR(13)//&
                        'This can be changed in the Configuration... window'//CHAR(13)//&
                        'under Options in the menu bar.')
      RETURN
 999  CALL ErrorMessage('DASH could not launch GSAS vis EXPGUI script. '// &
                        'No script'//CHAR(13)//&
                        '"'//SCRIPT_FILE//'" can be found at default locations.'//CHAR(13))
      ScriptName = ''
      RETURN

      END FUNCTION CheckEXPGUIExe
!
!*****************************************************************************
!
      INTEGER FUNCTION FillOptStr(OptStr)

      USE DRUID_HEADER

      IMPLICIT NONE

      CHARACTER*(*), INTENT (  OUT) :: OptStr

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

      INTEGER opt

      FillOptStr = 0
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page7_GSAS)
      opt = 0
      IF ( WDialogGetCheckBoxLogical(IDC_Initialisation) ) opt = opt + Z'03'
      IF ( WDialogGetCheckBoxLogical(IDC_Background) ) opt = opt + Z'04'
      IF ( WDialogGetCheckBoxLogical(IDC_Scale) ) opt = opt + Z'08'
      IF ( WDialogGetCheckBoxLogical(IDC_PO) ) opt = opt + Z'10'
      IF ( WDialogGetCheckBoxLogical(IDC_Biso) ) opt = opt + Z'20'
      IF ( WDialogGetCheckBoxLogical(IDC_Coordinates_Non_H) ) opt = opt + Z'40'
      IF ( WDialogGetCheckBoxLogical(IDC_Coordinates_H) ) opt = opt + Z'80'
      IF ( WDialogGetCheckBoxLogical(IDC_WriteCIF) ) opt = opt + Z'100'
      CALL PopActiveWindowID

      WRITE(OptStr,'(Z32)') opt
      FillOptStr = opt

      END FUNCTION FillOptStr
!
!*****************************************************************************
!
      SUBROUTINE UpdateGSASCheckBoxes

      USE DRUID_HEADER
      USE TAVAR

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page7_GSAS)
      IF ( ext_RR_stage .GT. 2 .AND. &
           .NOT. WDialogGetCheckBoxLogical(IDC_UseDASHRecommendation) ) THEN
          CALL PopActiveWindowID
          RETURN
      ENDIF
      SELECT CASE ( ext_RR_stage )
        CASE ( 1 ) 
          ! initial
          CALL WDialogPutCheckBoxLogical(IDC_Initialisation,    .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Scale,             .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,        .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_PO,                .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,              .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates_Non_H, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates_H,     .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,          .FALSE.)
        CASE ( 2 ) 
          CALL WDialogPutCheckBoxLogical(IDC_Initialisation,    .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Scale,             .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,        .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_PO,                .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,              .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates_Non_H, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates_H,     .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,          .FALSE.)
        CASE ( 3 )
          CALL WDialogPutCheckBoxLogical(IDC_Initialisation,    .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Scale,             .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,        .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_PO,                .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,              .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates_Non_H, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates_H,     .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,          .FALSE.)
        CASE ( 4 )
          CALL WDialogPutCheckBoxLogical(IDC_Initialisation,    .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Scale,             .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,        .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_PO,                .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,              .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates_Non_H, .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates_H,     .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,          .FALSE.)
        CASE ( 5 )
          CALL WDialogPutCheckBoxLogical(IDC_Initialisation,    .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Scale,             .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,        .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_PO,                .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,              .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates_Non_H, .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates_H,     .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,          .TRUE.)
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE UpdateGSASCheckBoxes
!
!*****************************************************************************
!
! The initialisation of this window is done in DealWithWizardWindowProfileRange()
! in Wizard_routines.f90
      SUBROUTINE DealWithRR_GSAS

      USE DRUID_HEADER
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      INTEGER, EXTERNAL :: Launch_GSAS

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page7_GSAS)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_RR_External)
            CASE (IDCANCEL, IDCLOSE)
              IF ( iRietveldMethod .NE. INTERNAL_RB ) THEN
                CALL CopyBackup2Pattern()
                iRietveldMethod = INTERNAL_RB
                CALL WDialogUnload(IDD_SAW_Page7_GSAS)
              ENDIF
              CALL EndWizardPastPawley
            CASE (IDB_WRITE)
              IF ( WDialogGetCheckBoxLogical(IDC_Initialisation) ) THEN
                ext_RR_stage = 1
                CALL UpdateGSASCheckBoxes
              ENDIF
              IF ( Launch_GSAS(ext_RR_input_file_name, .FALSE.) .EQ. 0 ) THEN
                ext_RR_stage = ext_RR_stage + 1
                CALL UpdateGSASCheckBoxes
              ENDIF
            CASE (IDB_FG_EXPGUI)
              CALL Launch_GSAS(ext_RR_input_file_name, .TRUE.)
            CASE (IDB_View)
              ! 'ENT': the extension of pdb file generated by gsas2pdb
              CALL Launch_Viewer(ext_RR_input_file_name, 'ENT')
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithRR_GSAS
!
!*****************************************************************************
!
      INTEGER FUNCTION WriteGSASFiles(TheFileName)

      USE DRUID_HEADER
      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INTEGER, EXTERNAL :: WriteEXPGUIPhaseFile
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL, EXTERNAL :: FnWavelengthOfMenuOption
      INTEGER hFileRaw, hFileIns
      PARAMETER ( hFileRaw = 116, hFileIns = 117 )
      INTEGER iBaseLen, i, tLen, ExtLength
      CHARACTER (MaxPathLength) tDirName, tFileName, tFileNameRoot, FileNameBase
      CHARACTER (40) tInstName, tExtension
      INTEGER tIRadSelection
      CHARACTER (80) tLine
      INTEGER tNrec, i1, i2, j, iPola, iRad
      REAL Lambda1, Lambda2, Zero, Pola, KRatio, UVW(3), LXY(2), YScale, YMax
      PARAMETER ( Zero = 0.0, KRatio = 0.5 )

      ! The way this code has curently been written, this routine can only be called
      ! from one of the Wizard windows as part of a "iRietveldMethod" Rietveld refinement
      ! Initialise to failure
      WriteGSASFiles = 1
      ExtLength = MIN(LEN_TRIM(TheFileName), LEN(tExtension))
      CALL SplitPath2(TheFileName, tDirName, tFileNameRoot, tExtension, ExtLength)
      ! CALL IOsDirChange(tDirName)
      FileNameBase = TRIM(tDirName)//TRIM(tFileNameRoot)
      iBaseLen = LEN_TRIM(FileNameBase)
      tFileName = FileNameBase(1:iBaseLen)//'.gsa'
      tLen = iBaseLen + 4
      OPEN(UNIT=hFileRaw,FILE=tFileName(1:tLen),STATUS='unknown',ERR=999)
      ! GSAS Raw file: assume CONS and ESD
      WRITE(tLine, '(F10.6,5X,A)', ERR=999) ALambda, 'Exported by DASH'
      WRITE(hFileRaw, '(A80)', ERR=999) tLine
!      tFileName = TRIM(tFileNameRoot)//'.ins'
!      WRITE(tLine, '(A20,A)', ERR=999) 'Instrument parameter', ' '//TRIM(tFileName)
!      WRITE(hFileRaw, '(A80)', ERR=999) tLine
      tNrec = NBIN / 5
      IF ( MOD(NBIN,5) .NE. 0 ) tNrec = tNrec + 1
      WRITE(tLine, '(A,2(I6,1X),A,1X,2(F10.4,1X),A)', ERR=999) 'BANK 1 ', NBIN, tNrec, 'CONS', &
            XBIN(1) * 100.0,(XBIN(2) - XBIN(1)) * 100.0,'0.0 0.0 ESD'
      WRITE(hFileRaw, '(A80)', ERR=999) tLine
      ! Scale YOBIN, EBIN to fit F8.2, assume all positive
      YMax = 0.0
      YScale = 1.0
      DO I = 1, NBIN
        YMax = max(YMax,max(YOBIN(I),EBIN(I)))
      ENDDO
      DO WHILE ( YMax * YScale .GE. 1E5 )
        YScale = YScale / 10.0
      ENDDO
      i1 = 1
      DO I = 1, tNrec
        i2 = i1 + 4
        IF ( i2 .GT. NBIN ) i2 = NBIN
        WRITE(tLine, '(10F8.2)', ERR=999) (YOBIN(J) * YScale, EBIN(J) * YScale, J = i1, i2)
        WRITE(hFileRaw, '(A80)', ERR=999) tLine
        i1 = i2 + 1
      ENDDO
      CLOSE(hFileRaw)
      CALL WDialogSelect(IDD_PW_Page4)
      IF ( JRadOption .EQ. 1 ) THEN
        tInstName = 'Lab X-ray'
        ! typical Pseudo-Viogt profile parameters for X-ray from GSAS manual
        UVW(1) = 2.0
        UVW(2) = -2.0
        UVW(3) = 5.0
        LXY(1) = 1.0
        LXY(1) = 1.0
        tIRadSelection = -1
        Pola = 0.5 
        DO I = 2, 6
          IF (ABS(ALambda - FnWavelengthOfMenuOption(I)) .LT. 0.0003) tIRadSelection = I
        ENDDO
        SELECT CASE (tIRadSelection)
        CASE (2) ! Cu
          Lambda1 = 1.54060
          Lambda2 = 1.54439
          iRad = 3
        CASE (3) ! Mo
          Lambda1 = 0.70930
          Lambda2 = 0.71359
          iRad = 4
        CASE (4) ! Co
          Lambda1 = 1.78897
          Lambda2 = 1.79285
          iRad = 7
        CASE (5) ! Cr
          Lambda1 = 2.28970
          Lambda2 = 2.29361
          iRad = 1
        CASE (6) ! Fe
          Lambda1 = 1.93604
          Lambda2 = 1.93998
          iRad = 2
        CASE DEFAULT
          CALL InfoMessage('Wavelength not recognised as a standard anode '// &
                           'material--monochromated assumed.')
          Lambda1 = ALambda
          Lambda2 = 0.0
          iRad = 0
        END SELECT
        IF ( WDialogGetCheckBoxLogical(IDC_Monochromated) ) THEN
          iPola = 0 ! diffracted beam
          Lambda2 = 0.0
        ELSE
          iPola = 1 ! incident beam
        ENDIF
      ELSE IF ( JRadOption .EQ. 2 ) THEN
        tInstName = 'Synchrotron'
        iRad = 0
        Lambda1 = ALambda
        Lambda2 = 0.0
        UVW(1) = 1.0
        UVW(2) = -1.0
        UVW(3) = 2.0
        LXY(1) = 0.0
        LXY(1) = 0.0
        Pola = 0.0 
        iPola = 2 ! none
      ELSE
        CALL ErrorMessage('Unknown radiation type.')
        RETURN
      ENDIF
      ! GSAS ins file
      tFileName = FileNameBase(1:iBaseLen)//'.ins'
      tLen = iBaseLen + 4
      OPEN(UNIT=hFileIns,FILE=tFileName(1:tLen),STATUS='unknown',ERR=998)
      WRITE(tLine, '(12X,60I1)', ERR=998) ((mod(I,10), I=1,10), J=1,6)
      WRITE(hFileIns, '(A80)', ERR=998) tLine
      WRITE(tLine, '(A,A5)', ERR=998) 'INS   BANK  ','   1'
      WRITE(hFileIns, '(A80)', ERR=998) tLine
      WRITE(tLine, '(A,2X,A4)', ERR=998) 'INS   HTYPE ','PXCR'    ! for xray only
      WRITE(hFileIns, '(A80)', ERR=998) tLine
      WRITE(tLine, '(A,3F10.4, 9X,1H0,F10.3,I5,F10.3)', ERR=998) 'INS  1 ICONS', Lambda1, &
                                                                 Lambda2, ZERO, POLA, IPOLA, KRatio
      WRITE(hFileIns, '(A80)', ERR=998) tLine
      WRITE(tLine, '(A,I5)', ERR=998) 'INS  1 IRAD ', IRAD
      WRITE(hFileIns, '(A80)', ERR=998) tLine
      WRITE(tLine, '(A,2X,A)', ERR=998) 'INS  1I HEAD', 'Exported by DASH'
      WRITE(hFileIns, '(A80)', ERR=998) tLine
      WRITE(tLine, '(A,A5,3A10)', ERR=998) 'INS  1I ITYP', '0', '0.0000', '180.0000', '1'
      WRITE(hFileIns, '(A80)', ERR=998) tLine
      WRITE(tLine, '(A,2X,A)', ERR=998) 'INS  1INAME ', tInstName
      WRITE(hFileIns, '(A80)', ERR=998) tLine
      WRITE(tLine, '(A,2A5,A10)', ERR=998) 'INS  1PRCF1 ', '2', '6', '0.01000'
      WRITE(hFileIns, '(A80)', ERR=998) tLine
      WRITE(tLine, '(A,4E15.6E3)', ERR=998) 'INS  1PRCF11', (UVW(i), i=1,3), LXY(1)
      WRITE(hFileIns, '(A80)', ERR=998) tLine
      WRITE(tLine, '(A,E15.6E3,A15)', ERR=998) 'INS  1PRCF12', LXY(2), '0.000000E+000'
      WRITE(hFileIns, '(A80)', ERR=998) tLine
!      DO I = 2, 4
!        WRITE(tLine, '(A,I1,4A15)', ERR=998) 'INS  1PRCF1', I, ('0.000000E+000', J=1,4)
!        WRITE(hFileIns, '(A80)', ERR=998) tLine
!      ENDDO
      WRITE(tLine, '(A,4A15)', ERR=998) 'INS  1PRCF15', ('0.000000E+000', I=1,2)
      WRITE(hFileIns, '(A80)', ERR=998) tLine
      CLOSE(hFileIns)

      WriteGSASFiles = WriteEXPGUIPhaseFile(FileNameBase)
      RETURN

  999 CALL ErrorMessage('Error writing GSAS raw file.')
      CLOSE(hFileRaw)
      RETURN
  998 CALL ErrorMessage('Error writing GSAS instrument file.')
      CLOSE(hFileIns)
      RETURN

      END FUNCTION WriteGSASFiles
!
!*****************************************************************************
!
! This function writes phase infomation to a file, which will be processed by a script
! calling modified EXPGUI utilities to generate GSAS EXP file.
      INTEGER FUNCTION WriteEXPGUIPhaseFile(FileNameBase)

      USE VARIABLES
      USE ZMVAR
      USE PO_VAR
      USE RRVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN) :: FileNameBase

      INCLUDE 'Lattice.inc'

      INTEGER     mpdbops
      PARAMETER ( mpdbops = 192 )

      INTEGER         npdbops
      CHARACTER*20             cpdbops
      COMMON /pdbops/ npdbops, cpdbops(mpdbops)

      LOGICAL, EXTERNAL :: PutAtoms, GSASWriteAtom
      LOGICAL, EXTERNAL :: PutRestraints, GSASWriteDistance, GSASWriteAngle, GSASWritePlane
      INTEGER hFileGSAS
      PARAMETER ( hFileGSAS = 116 )
      INTEGER I
      CHARACTER*(40) space_group_str

      ! Initialise to failure
      WriteEXPGUIPhaseFile = 1
      OPEN(UNIT=hFileGSAS, FILE=TRIM(FileNameBase)//'.pha', &
           STATUS='unknown', ERR=999)
! We must call ShowWizardWindowRietveld() here, which will fill
! Xato (and all the RR variables). The Wizard window is suppressed because of the iRietveldMethod flag.
      CALL ShowWizardWindowRietveld(RR_SA_Sol)
      WRITE(hFileGSAS, '(A,6(F8.4,1X))', ERR=999) 'CELL ', (CellPar(I),I=1,6)
      space_group_str = TRIM(SGHMaStr(NumberSGTable))
! Strip ':*' from spg string for SPG.
! Note: GSAS exptool.exe accepts :R and :H (with ':' replaced by ' ')
!       but seems not for origin choices (:1 and :2)
! Note: WritePDBCommon() may have stripped :* and changed R...:H to H... for global SGHMaStr array
      i = SCAN(space_group_str, ':')
      IF ( i .GT. 0 ) THEN
        space_group_str(i:i) = ' '
        i = i + 1
        IF ( SCAN('RH', space_group_str(i:i)) .LE. 0 ) space_group_str(i:) = ' '
      ENDIF
      WRITE(hFileGSAS, '(A)', ERR=999) 'SPG '//TRIM(space_group_str)
      DO I = 1, npdbops
        WRITE(hFileGSAS, '(A)', ERR=999) 'SYMM '//TRIM(cpdbops(I))
      ENDDO
      IF ( PutAtoms(hFileGSAS, GSASWriteAtom) ) GOTO 999
      ! number of fragments for setting up atom damp factor
      WRITE(hFileGSAS, '(A,I)', ERR=999) 'NFRG ', nFrag
! Also need to write out PO if used during SA
      IF ( PrefParExists ) THEN
        WRITE(hFileGSAS, '(A)', ERR=999) '# MD preferred orientation'
        ! PREFO type 0
        WRITE(hFileGSAS, '(A,F5.3,3(1X,I3))', ERR=999) 'PREFO 0 ', RR_PO, &
              PO_Direction(1), PO_Direction(2), PO_Direction(3)
        CALL InfoMessage('The preferred orientation that was used during the '// &
                         'Simulated Annealing'//CHAR(13)//&
                         'will be written out to the GSAS .exp file: if you are using'//CHAR(13)//&
                         'a different experimental pattern, you may need to remove this.')
      ENDIF
      WRITE(hFileGSAS, '(A)', ERR=999) '# restraints'
      ! default weights
!      WRITE(hFileGSAS, '(A)', ERR=999) 'FACT DIST 100.0 ANGL 100.0 PLAN 100.0'
      IF ( PutRestraints(hFileGSAS, GSASWriteDistance, GSASWriteAngle, GSASWritePlane) ) GOTO 999
      WriteEXPGUIPhaseFile = 0
      CLOSE(hFileGSAS)
      RETURN

  999 CALL ErrorMessage('Error writing phase input file for EXPGUI script (Rietveld).')
      CLOSE(hFileGSAS)
      RETURN

      END FUNCTION WriteEXPGUIPhaseFile
!
!*****************************************************************************
!
      LOGICAL FUNCTION GSASWriteAtom (hFile, iElement, Label, XYZ, Occ, Biso)

      USE ATMVAR

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: hFile, iElement
      CHARACTER*(*), INTENT (IN) :: Label
      REAL, INTENT (IN) :: XYZ(3), Occ, Biso

      REAL B2U
      PARAMETER ( B2U = 1./(8.*3.14159*3.14159) )

      INTEGER I

      GSASWriteAtom = .TRUE.
      ! Biso to Uiso
      WRITE(hFile, '(A,3(X,F9.5),X,A,2(X,F6.3))', ERR=999) 'ATOM '//Label, (XYZ(I),I=1,3), &
            ElementStr(iElement), Occ, Biso*B2U
      GSASWriteAtom = .FALSE.
  999 RETURN

      END FUNCTION GSASWriteAtom
!
!*****************************************************************************
!
      LOGICAL FUNCTION GSASWriteDistance (hFile, iAtom1, iAtom2, iElement1, iElement2, Distance)

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: hFile, iAtom1, iAtom2, iElement1, iElement2
      REAL, INTENT (IN) :: Distance

      GSASWriteDistance = .TRUE.
      ! weight = 1/0.01^2
      WRITE(hFile, '(A,I3,1X,I3,F9.4,A)', ERR=999) 'DIST ', iAtom1, iAtom2, Distance, ' 0.01'
      GSASWriteDistance = .FALSE.
  999 RETURN
      ! No operation, only prevent compiler complians unused varibles
      IF ( .FALSE. .AND. iElement1+iElement2 .EQ. 0 ) RETURN 

      END FUNCTION GSASWriteDistance
!
!*****************************************************************************
!
      LOGICAL FUNCTION GSASWriteAngle (hFile, iAtom1, iAtom, iAtom2, &
                                       iElement1, iElement, iElement2, Angle)

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: hFile, iAtom1, iAtom, iAtom2, iElement1, iElement, iElement2
      REAL, INTENT (IN) :: Angle

      GSASWriteAngle = .TRUE.
      ! weight = 1/1.0^2
      WRITE(hFile, '(A,3(I3,1X),F9.4,A)', ERR=999) 'ANGL ', iAtom1, iAtom, iAtom2, Angle, ' 1.0'
      GSASWriteAngle = .FALSE.
  999 RETURN
      ! No operation, only prevent compiler complians unused varibles
      IF ( .FALSE. .AND. iElement1+iElement+iElement2 .EQ. 0 ) RETURN 

      END FUNCTION GSASWriteAngle
!
!*****************************************************************************
!
      LOGICAL FUNCTION GSASWritePlane (hFile, iNumbAtom, iAtoms, iElements)

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: hFile, iNumbAtom, iAtoms(iNumbAtom), iElements(iNumbAtom)

      INTEGER I

      GSASWritePlane = .TRUE.
      ! GSAS planar group only takes upto 12 atoms
      WRITE(hFile, '(A$)', ERR=999) 'PLAN '
      DO I = 1, MIN(12, iNumbAtom)
        WRITE(hFile, '(I3,X$)', ERR=999) iAtoms(I)
      ENDDO
      ! weight = 1/0.001^2
      WRITE(hFile, '(A)', ERR=999) ' 0.001'
      GSASWritePlane = .FALSE.
  999 RETURN
      ! No operation, only prevent compiler complians unused varibles
      IF ( .FALSE. .AND. iElements(1) .EQ. 0 ) RETURN 

      END FUNCTION GSASWritePlane
!
!*****************************************************************************
!
