!
!*****************************************************************************
!
      INTEGER FUNCTION Launch_RIETAN(input_file_name)

      USE DRUID_HEADER
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: input_file_name

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      INTEGER, EXTERNAL :: SetOptions, CheckRIETANExe
      CHARACTER*20, EXTERNAL :: Integer2String

      INTEGER nCycle, opt
      CHARACTER(MaxPathLength) tScriptName
      CHARACTER(1) tOptStr

      ! Initialise to failure
      Launch_RIETAN = 1
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page7_RIETAN)
      CALL WDialogGetInteger(IDF_NCYCL, nCycle)
      CALL PopActiveWindowID
      opt = SetOptions()
      IF ( opt .LE. 0 ) GOTO 996
      tOptStr = '0'
      IF ( IAND(opt, Z'100') .NE. 0 ) tOptStr = ''
      CALL SetRefineParameters(input_file_name, opt)
      IF ( CheckRIETANExe(RIETANEXE, tScriptName) .NE. 0 ) RETURN
      ! Launch RIETAN vis script and wait for it to return
      CALL InfoError(1) ! Clear errors
!        CALL IOSCommand('cmd.exe /k start /b '//TRIM(tScriptName)//' '// &
      CALL IOSCommand('"'//TRIM(tScriptName)//'" "'//TRIM(RIETANEXE)//'" '// &
                      '"'//TRIM(input_file_name)//'" '//tOptStr, ProcBlocked)
      IF ( InfoError(1) .NE. 0 ) GOTO 998

! Follow init, run RIETAN and ORFFE to generate a .ffe file
      IF ( IAND(opt, Z'01') .NE. 0 ) THEN
        ! RIETAN tends to break without parameters to be refined, set both bkg and scale
        CALL SetRefineParameters(input_file_name, Z'100C')
        tOptStr = '2'
        CALL IOSCommand('"'//TRIM(tScriptName)//'" "'//TRIM(RIETANEXE)//'" '// &
                      '"'//TRIM(input_file_name)//'" '//tOptStr, ProcBlocked)
        IF ( InfoError(1) .NE. 0 ) GOTO 998
        CALL CopyRIETANRestraints(input_file_name)
      ENDIF
      CALL WCursorShape(CurCrossHair)
      Launch_RIETAN = 0
      RETURN

 996  CALL ErrorMessage('DASH could not launch RIETAN. '// &
                        'No refine option selected.')
      CALL WCursorShape(CurCrossHair)
      RETURN
 998  CALL ErrorMessage('DASH could not launch RIETAN. The executable'//CHAR(13)//&
                        'is currently configured to launch the program '// &
                        TRIM(RIETANEXE)//CHAR(13)//&
                        'This can be changed in the Configuration... window'//CHAR(13)//&
                        'under Options in the menu bar.')
      CALL WCursorShape(CurCrossHair)
      RETURN

      END FUNCTION Launch_RIETAN
!
!*****************************************************************************
!
      INTEGER FUNCTION CheckRIETANExe (ExeName, ScriptName)

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (  OUT) :: ScriptName
      CHARACTER*(*), INTENT (IN   ) :: ExeName

      CHARACTER*(*) SCRIPT_FILE
      PARAMETER ( SCRIPT_FILE = 'RIETAN.CMD' )
      CHARACTER(MaxPathLength) :: tDirName, tFileName
      LOGICAL exists
      INTEGER ExtLength
      CHARACTER*8 tExtension

      ! Initialise to failure
      CheckRIETANExe = 1

      IF ( LEN_TRIM(ExeName) .EQ. 0 ) GOTO 997
      INQUIRE(FILE=ExeName, EXIST=exists)
      IF ( .NOT. exists ) GOTO 998
      ExtLength = len(tExtension)
      CALL SplitPath2(ExeName, tDirName, tFileName, tExtension, ExtLength)
      CALL StrUpperCase(tExtension)
      IF ( tExtension .NE. 'EXE' ) goto 998

      ScriptName = TRIM(InstallationDirectory)//SCRIPT_FILE
      INQUIRE(FILE=ScriptName, EXIST=exists)
      IF ( .NOT. exists ) GOTO 999
 10   CheckRIETANExe = 0
      RETURN

 997 CALL ErrorMessage('DASH could not launch RIETAN. '// &
                        'No executable is currently'//CHAR(13)//&
                        'specified. This can be changed in the Configuration... window'//CHAR(13)//&
                        'under Options in the menu bar.')
      RETURN
 998  CALL ErrorMessage('DASH could not launch RIETAN. The executable'//CHAR(13)//&
                        'is currently configured to launch the program: "'// &
                        TRIM(ExeName)//'"'//CHAR(13)//&
                        'This can be changed in the Configuration... window'//CHAR(13)//&
                        'under Options in the menu bar.')
      RETURN
 999  CALL ErrorMessage('DASH could not launch RIETAN script. '// &
                        'No script'//CHAR(13)//&
                        '"'//SCRIPT_FILE//'" can be found at default locations.'//CHAR(13))
      ScriptName = ''
      RETURN

      END FUNCTION CheckRIETANExe
!
!*****************************************************************************
!
      INTEGER FUNCTION SetOptions()

      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

      INTEGER opt

      SetOptions = 0
      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page7_RIETAN)
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

      SetOptions = opt

      END FUNCTION SetOptions
!
!*****************************************************************************
!
      SUBROUTINE UpdateRIETANCheckBoxes

      USE DRUID_HEADER
      USE TAVAR

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page7_RIETAN)
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
          CALL WDialogPutCheckBoxLogical(IDC_Biso,              .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates_Non_H, .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates_H,     .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,          .TRUE.)
        CASE ( 3 )
          CALL WDialogPutCheckBoxLogical(IDC_Initialisation,    .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Scale,             .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,        .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_PO,                .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,              .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates_Non_H, .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates_H,     .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,          .TRUE.)
        CASE ( 4 )
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

      END SUBROUTINE UpdateRIETANCheckBoxes
!
!*****************************************************************************
!
! The initialisation of this window is done in DealWithWizardWindowProfileRange()
! in Wizard_routines.f90
      SUBROUTINE DealWithRR_RIETAN

      USE DRUID_HEADER
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical
      INTEGER, EXTERNAL :: Launch_RIETAN

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SAW_Page7_RIETAN)
      SELECT CASE (EventType)
        CASE (PushButton) ! one of the buttons was pushed
          SELECT CASE (EventInfo%VALUE1)
            CASE (IDBACK)
              CALL WizardWindowShow(IDD_RR_External)
            CASE (IDCANCEL, IDCLOSE)
              IF ( iRietveldMethod .NE. INTERNAL_RB ) THEN
                CALL CopyBackup2Pattern()
                iRietveldMethod = INTERNAL_RB
                CALL WDialogUnload(IDD_SAW_Page7_RIETAN)
              ENDIF
              CALL EndWizardPastPawley
            CASE (IDB_WRITE)
              IF ( WDialogGetCheckBoxLogical(IDC_Initialisation) ) THEN
                ext_RR_stage = 1
                CALL UpdateRIETANCheckBoxes
              ENDIF
              IF ( Launch_RIETAN(ext_RR_input_file_name) .EQ. 0 ) THEN
                ext_RR_stage = ext_RR_stage + 1
                CALL UpdateRIETANCheckBoxes
              ENDIF
            CASE (IDB_View)
              CALL Launch_Viewer(ext_RR_input_file_name, 'cif')
          END SELECT
      END SELECT
      CALL PopActiveWindowID

      END SUBROUTINE DealWithRR_RIETAN
!
!*****************************************************************************
!
      INTEGER FUNCTION SetRefineParameters(TheFileName, OptWord)

      USE DRUID_HEADER
      USE WINTERACTER

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN) :: TheFileName
      INTEGER, INTENT (IN) :: OptWord
 
      CHARACTER*(*) P_MARK
      INTEGER hFileW, hFileR, iPMarkLen
      PARAMETER ( hFileW = 116, hFileR = 117, P_MARK = ' #@DASH@MARK@P ', iPMarkLen = LEN(P_MARK) )
      INTEGER p, flag, nCycle, kLen
      REAL tk, finc
      CHARACTER (40) tKeyWord
      CHARACTER (80) tLine, tLine2

      ! Initialise to failure
      SetRefineParameters = 1
      OPEN(UNIT=hFileW,FILE=TRIM(TheFileName)//'.tmp',STATUS='unknown',ERR=998)
      OPEN(UNIT=hFileR,FILE=TRIM(TheFileName),STATUS='old',ERR=997)

      DO WHILE ( .NOT. EOF(hFileR) )
        READ(hFileR, '(A)', ERR=997) tLine
        WRITE(hFileW, '(A)', ERR=998) TRIM(tLine)
        IF ( tLine(1:iPMarkLen) .NE. P_MARK ) CYCLE
        READ(tLine(iPMarkLen+1:),*, ERR=998) tKeyWord
        kLen = LEN_TRIM(tKeyWord)
        IF ( kLen .LE. 0 ) CYCLE
        READ(hFileR, '(A)', ERR=997) tLine2

        IF ( tKeyWord .EQ. 'NCYCL') THEN
          CALL PushActiveWindowID
          CALL WDialogSelect(IDD_SAW_Page7_RIETAN)
          CALL WDialogGetInteger(IDF_NCYCL, nCycle)
          CALL PopActiveWindowID
          WRITE(hFileW, '(A,I3)', ERR=998) ' NCYCL = ', nCycle
          CYCLE
        ELSE IF ( tKeyWord .EQ. 'TK') THEN
          CALL PushActiveWindowID
          CALL WDialogSelect(IDD_SAW_Page7_RIETAN)
          CALL WDialogGetReal(IDF_REAL_RIETAN_WEIGHT_TK, tk)
          CALL PopActiveWindowID
          WRITE(hFileW, '(A,F10.2)', ERR=998) ' TK = ', tk
          CYCLE
        ELSE IF ( tKeyWord .EQ. 'FINC') THEN
          CALL PushActiveWindowID
          CALL WDialogSelect(IDD_SAW_Page7_RIETAN)
          CALL WDialogGetReal(IDF_REAL_RIETAN_WEIGHT_FINC, finc)
          CALL PopActiveWindowID
          WRITE(hFileW, '(A,F10.2)', ERR=998) ' FINC = ', finc
          CYCLE
        ELSE IF ( IAND(OptWord, Z'01') .NE. 0 ) THEN
! Initial: Replace next line
          WRITE(hFileW, '(A)', ERR=998) TRIM(tLine(iPMarkLen+1:))
          CYCLE
        ENDIF

! Toggle options
        SELECT CASE (tKeyWord)
        CASE ('NMODE')
          flag = IAND(OptWord, Z'01')
        CASE ('BKGD1','BKGD2','BKGD3','BKGD4')
          flag = IAND(OptWord, Z'04')
        CASE ('SCALE')
          flag = IAND(OptWord, Z'08')
        CASE ('SHIFT0','GAUSS01','LORENTZ01','ASYM')
          flag = IAND(OptWord, Z'02')
        CASE ('PREF')
          flag = IAND(OptWord, Z'10')
        CASE ('NC')
          flag = IAND(OptWord, Z'C0') ! 0x40+0x80
        CASE ('NDA')
          flag = IAND(OptWord, Z'1000')
        CASE DEFAULT
!          p = SCAN(tLine, '/')
!          IF ( p .GT. 0 ) THEN
          p = iPMarkLen + SCAN(tLine(iPMarkLen+1:),tKeyWord(1:1)) + kLen
          IF ( tLine(p:p) .EQ. '/' ) THEN
            ! Atoms
            IF ( tLine(p+1:p+2) .NE. 'H ' ) THEN
              flag = IAND(OptWord, Z'40')
            ELSE
              flag = IAND(OptWord, Z'80')
            ENDIF
            CALL set_parameter_id(flag, 2, 4)
            CALL set_parameter_id(IAND(OptWord, Z'20'), 5, 5)
            WRITE(hFileW, '(A)', ERR=998) TRIM(tLine2)
            CYCLE
          ENDIF
          ! If any left
          flag = 0
        END SELECT
        CALL set_parameter_id(flag)
        WRITE(hFileW, '(A)', ERR=998) TRIM(tLine2)

      ENDDO

      CLOSE(hFileW)
      CLOSE(hFileR, STATUS='delete')
!      CALL IOSCommand('CMD.exe /C copy /Y "'//TRIM(TheFileName)//'.tmp" '// &
!                      '"'//TRIM(TheFileName)//'"', ProcSilent+ProcBlocked)
!      CALL IOsDeleteFile(TRIM(TheFileName))
      CALL InfoError(1)
      CALL IOsRenameFile(TRIM(TheFileName)//'.tmp', TRIM(TheFileName))
      IF ( InfoError(1) .NE. 0 ) GOTO 996
      SetRefineParameters = 0

  999 CLOSE(hFileW)
      CLOSE(hFileR)
      RETURN

  996 CALL ErrorMessage('Error renaming ins file.')
      GOTO 999
  997 CALL ErrorMessage('Error reading RIETAN old ins file.')
      GOTO 999
  998 CALL ErrorMessage('Error writing RIETAN new ins file.')
      GOTO 999
      RETURN
 
      CONTAINS
 
! This function either copies id (last word), or part of it when p1 p2 present, 
! from tLine to its counterpart on tLine2 or fills with zeros depends on flag.
      SUBROUTINE set_parameter_id (flag, p1, p2)

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: flag, p1, p2
      OPTIONAL ::  p1, p2

      CHARACTER (40) tIdStr
      INTEGER i

      i = SCAN(TRIM(tLine), ' ', BACK=.TRUE.)
      IF ( PRESENT(p1) .AND. PRESENT(p2) ) THEN
        tIdStr = tLine(i+p1:i+p2)
      ELSE
        tIdStr = tLine(i+1:)
      ENDIF
      IF ( flag .EQ. 0 ) tIdStr = REPEAT('0', LEN_TRIM(tIdStr))
      i = SCAN(TRIM(tLine2), ' ', BACK=.TRUE.)
      IF ( PRESENT(p1) .AND. PRESENT(p2) ) THEN
        tLine2(i+p1:i+p2) = TRIM(tIdStr)
      ELSE
        tLine2(i+1:) = TRIM(tIdStr)
      ENDIF

      END SUBROUTINE set_parameter_id

      END FUNCTION SetRefineParameters
!
!*****************************************************************************
!
      INTEGER FUNCTION WriteRIETANFiles(TheFileName)

      USE DRUID_HEADER
      USE VARIABLES
      USE ATMVAR
      USE ZMVAR
      USE PO_VAR
      USE TAVAR

      IMPLICIT NONE

      INCLUDE 'Lattice.inc'

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      INTEGER, EXTERNAL :: WriteRIETANPhaseInfo
      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      REAL, EXTERNAL :: FnWavelengthOfMenuOption
      CHARACTER*(*) MARK
      INTEGER hFileTmp, hFileIns, iMarkLen
      PARAMETER ( hFileIns = 116, hFileTmp = 117, MARK = ' #@DASH@MARK@ ', iMarkLen = LEN(MARK) )
      INTEGER iBaseLen, i, tLen, ExtLength
      CHARACTER (MaxPathLength) tDirName, tFileName, FileNameBase
      CHARACTER (40) tExtension, tKeyWord
      INTEGER tIRadSelection, iRad
      CHARACTER (80) tLine
      REAL Pola, yMax, y, r12
      INTEGER NumOfAtmPerElm(1:MaxElm), iFrg, spg_set, nBeam

      ! The way this code has curently been written, this routine can only be called
      ! from one of the Wizard windows as part of a "iRietveldMethod" Rietveld refinement
      ! Initialise to failure
      WriteRIETANFiles = 1
      ExtLength = MIN(LEN_TRIM(TheFileName), LEN(tExtension))
      CALL SplitPath2(TheFileName, tDirName, tFileName, tExtension, ExtLength)
      FileNameBase = TRIM(tDirName)//TRIM(tFileName)
      ! CALL IOsDirChange(tDirName)
      iBaseLen = LEN_TRIM(FileNameBase)
      tFileName = FileNameBase(1:iBaseLen)//'.int'
      tLen = iBaseLen + 4
      OPEN(UNIT=hFileTmp,FILE=tFileName(1:tLen),STATUS='unknown',ERR=993)
      ! RIETAN int file
      WRITE(hFileTmp, '(A,F10.6,A)', ERR=993) '* ', ALambda, ' Exported by DASH'
      WRITE(hFileTmp, *, ERR=993) NBIN, XBIN(1), XBIN(2) - XBIN(1)
      yMax = 0.0
      DO I = 1, NBIN
        y = YOBIN(I)
        IF ( y .EQ. 0.0 ) y = 1E-8 ! Rietan does not accept zero
        WRITE(hFileTmp, *, ERR=993) y
        yMax = max(yMax, y)
      ENDDO
      CLOSE(hFileTmp)
      IF ( XBIN(1) .LT. 1E-1 ) CALL InfoMessage('The diffraction pattern starts at '// &
                              'an extremely low point ( < 0.1 degree two-theta ).'//CHAR(13)// &
                              'If Rietan complains with "Bad diffraction angle(s)", you may '//&
                              'go back to truncate the data. ' )
      ! RIETAN Gnuplot file
      tFileName = FileNameBase(1:iBaseLen)//'.plt'
      tLen = iBaseLen + 4
      OPEN(UNIT=hFileTmp,FILE=tFileName(1:tLen),STATUS='unknown',ERR=993)
      WRITE(hFileTmp, '(A)') '# GNUPLOT script for plotting RIETAN2000 itx file'
      WRITE(hFileTmp, '(3(A,F10.2))') 'xmin=',XBIN(1),'; xmax=',XBIN(NBIN),'; ymax=', yMax
      WRITE(hFileTmp, '(A)') 'ymin=-ymax/4; offset_delta=-0.11*(ymax-ymin)'
      WRITE(hFileTmp, '(A)') 'len_bar=0.011*(ymax-ymin); y_phase1=-0.03*(ymax-ymin)'
      WRITE(hFileTmp, '(A)') 'set xrange [xmin:xmax]'
      WRITE(hFileTmp, '(A)') 'set yrange [ymin:ymax]'
      WRITE(hFileTmp, '(A)') 'set pointsize 0.7'
      WRITE(hFileTmp, '(A)') 'set mxtics 2'
      WRITE(hFileTmp, '(A)') 'set mytics 2'
      WRITE(hFileTmp, '(A)') 'set bar 0'
      WRITE(hFileTmp, '(A)') 'set ticscale 1 1'
      WRITE(hFileTmp, '(A)') 'set xlabel "2-theta/deg" 0.0, 0.2 font "Helvetica, 12"'
      WRITE(hFileTmp, '(A)') 'set ylabel "Intensity" 0.5, 0.0 font "Helvetica, 12"'
      WRITE(hFileTmp, '(A)') 'set terminal windows color "Helvetica" 10'
      WRITE(hFileTmp, '(A)') 'plot \'
      WRITE(hFileTmp, '(A)') ' "'//FileNameBase(LEN_TRIM(tDirName)+1:iBaseLen)//'.itx'// &
                             '" using 1:2 notitle with points 2, \'
      WRITE(hFileTmp, '(A)') ' "" using 1:3 notitle with lines linetype 8 linewidth 1, \'
      WRITE(hFileTmp, '(A)') ' "" using  1:($2 - $3 + offset_delta) notitle with lines '// &
                             'linetype 4 linewidth 1, \'
      WRITE(hFileTmp, '(A)') ' "" using  8:(y_phase1):(len_bar) notitle with yerrorbars '// &
                             'linetype 3 linewidth 1 pointtype 0'
      CLOSE(hFileTmp)
      CALL WDialogSelect(IDD_PW_Page4)
      r12 = 0.5
      IF ( JRadOption .EQ. 1 ) THEN
        nBeam = 1
        IF ( WDialogGetCheckBoxLogical(IDC_Monochromated) ) THEN
          Pola = 0.7998
! Fujio Izumi has confirmed that R12=0.0 means alpha1 rather than beta as stated in ins template!
          r12 = 0.0
        ELSE
          Pola = 1.0
        ENDIF
        tIRadSelection = -1
        DO I = 2, 6
          IF (ABS(ALambda - FnWavelengthOfMenuOption(I)) .LT. 0.0003) tIRadSelection = I
        ENDDO
        SELECT CASE (tIRadSelection)
        CASE (2) ! Cu
          iRad = 4
        CASE (3) ! Mo
          iRad = 2
        CASE (4) ! Co
          iRad = 5
        CASE (5) ! Cr
          iRad = 7
        CASE (6) ! Fe
          iRad = 6
        CASE DEFAULT
          CALL InfoMessage('Wavelength not recognised as a standard anode '// &
                           'material--monochromated assumed.')
          nBeam = 2
          iRad = 0
        END SELECT
      ELSE IF ( JRadOption .EQ. 2 ) THEN
        nBeam = 2
        iRad = 0
        Pola = 1.0 
      ELSE
        CALL ErrorMessage('Unknown radiation type.')
        RETURN
      ENDIF
      ! RIETAN instruction file: copy/fill template file
      IF ( INDEX(RIETANEXE, '-FP') .GT. 0 .OR. INDEX(RIETANEXE, '-fp') .GT. 0 ) THEN
        tFileName = TRIM(InstallationDirectory)//'rietanfp.tem'
      ELSE
        tFileName = TRIM(InstallationDirectory)//'rietan2000.tem'
      ENDIF
      CALL PopActiveWindowID
      OPEN(UNIT=hFileTmp,FILE=TRIM(tFileName),STATUS='old',ERR=997)
      tFileName = FileNameBase(1:iBaseLen)//'.ins'
      tLen = iBaseLen + 4
      OPEN(UNIT=hFileIns,FILE=tFileName(1:tLen),STATUS='unknown',ERR=998)
      NumOfAtmPerElm = 0
      DO iFrg = 1, nFrag
        DO i = 1, natoms(iFrg)
          CALL INC(NumOfAtmPerElm(zmElementCSD(i,iFrg)))
        ENDDO
      ENDDO
      DO WHILE ( .NOT. EOF(hFileTmp) )
        READ(hFileTmp, '(A)', ERR=997) tLine
        IF ( tLine(1:iMarkLen) .NE. MARK ) THEN
          WRITE(hFileIns, '(A)', ERR=998) TRIM(tLine)
          CYCLE
        ENDIF
        READ(tLine(iMarkLen+1:),*, ERR=998) tKeyWord
        SELECT CASE (tKeyWord)
        CASE ('NBEAM')
          WRITE(tLine, '(A,I1)', ERR=998) 'NBEAM = ', nBeam
        CASE ('NTARG')
          WRITE(tLine, '(A,I1)', ERR=998) ' NTARG = ', iRad
        CASE ('R12')
          WRITE(tLine, '(A,F6.4)', ERR=998) ' R12 = ', r12
        CASE ('CTHM1')
          WRITE(tLine, '(A,F6.4)', ERR=998) ' CTHM1 = ', Pola
        CASE ('XLMDX')
          WRITE(tLine, '(A,F10.6)', ERR=998) ' XLMDX = ', ALambda
        CASE ('CTHM2')
          WRITE(tLine, '(A,F6.4)', ERR=998) ' CTHM2 = ', Pola
        CASE ('SPECIES')
! DASH seems doesn't keep the chemical state of each species, use element
          WRITE(hFileIns, '(A)', ERR=998) ' # No chemical state kept by DASH, use element.'
          tLine = ''
          tLen = 1
          DO i = 1, MaxElm
            IF (NumOfAtmPerElm(i) .GT. 0) THEN
              tLine(tLen:) = " '"//TRIM(ElementStr(i))//"'"
              tLen = tLen + 5
            ENDIF
          ENDDO
          tLine = tLine(1:tLen)//' /'
        CASE ('SPECIES2')
! Can't find lamda dependent deltf' deltf'' for synchrotron, set to 0.0
          WRITE(hFileIns, '(A)', ERR=998) " # No Deltf' Deltf'' kept by DASH, set to zero."
          DO i = 1, MaxElm
            IF (NumOfAtmPerElm(i) .GT. 0) &
                WRITE(hFileIns, '(A)', ERR=998) ' 0.0 0.0'
          ENDDO
          CYCLE
        CASE ('VNS1')
          spg_set = GetSettingNumb(SGNumStr(NumberSGTable)(4:))
          IF ( spg_set .LE. 0 ) GOTO 990
          WRITE(tLine, '(A,I1,A1)', ERR=998) "VNS1 = 'A-"// &
                                       TRIM(ADJUSTL(SGNumStr(NumberSGTable)(1:3)))//"-",  &
                                       spg_set, "'"
        CASE ('HKLM1')
! RIETAN-FP (up to v1.51b) needs to explicitly enter H-M symbol for generating HKL and m.
! It seems the consistence checking between VNS and HKLM is not beyond crystal system.
!
! Strip ':*' from spg string for HKLM1.
! Note: RIETAN-FP spgr.daf neither distinguishes spg with :R, :H (though spgra does), 
!       nor (but be aware of) origin choices (:1 and :2)
! Note: WritePDBCommon() may have stripped :* and changed R...:H to H... for global SGHMaStr array
          i = SCAN(SGHMaStr(NumberSGTable), ':') - 1
          IF ( i .LT. 0 ) i = LEN_TRIM(SGHMaStr(NumberSGTable))
          WRITE(tLine, '(A)', ERR=998) "HKLM1 = '"//SGHMaStr(NumberSGTable)(:i)//" '"
        CASE ('CELLQ')
          WRITE(tLine, '(A,3(F9.5,1X),3(F7.3,1X),A)', ERR=998) 'CELLQ ', &
               (CellPar(I),I=1,6), '0.0 0000000'
        CASE ('IHP1')
          tLine = 'IHP1 = 1'  ! PO hkl can't be 000
          IF ( PrefParExists ) THEN
            WRITE(tLine, '(A,I3)', ERR=998) 'IHP1 = ', PO_Direction(1)
            CALL InfoMessage('The preferred orientation that was used during the '// &
                       'Simulated Annealing'//CHAR(13)//&
                       'will be written out to the RIETAN .ins file: if you are using'//CHAR(13)//&
                       'a different experimental pattern, you may need to remove this.')
          ENDIF
        CASE ('IKP1')
          WRITE(tLine, '(A,I3)', ERR=998) 'IKP1 = ', PO_Direction(2)
        CASE ('ILP1')
          WRITE(tLine, '(A,I3)', ERR=998) 'ILP1 = ', PO_Direction(3)
        CASE ('ATOMS')
          IF ( WriteRIETANPhaseInfo(FileNameBase, hFileIns) ) GOTO 998
          CYCLE
        CASE DEFAULT
          CYCLE
        END SELECT
! Replace next line
        WRITE(hFileIns, '(A)', ERR=998) TRIM(tLine)
        READ(hFileTmp, '(A)', ERR=997)
      ENDDO
      WriteRIETANFiles = 0

  999 CLOSE(hFileIns)
      CLOSE(hFileTmp)
      RETURN

  990 CALL ErrorMessage('The non-standard space group:'//CHAR(13)//CHAR(13)// &
                        TRIM(SGHMaStr(NumberSGTable))//CHAR(13)//CHAR(13)// &
                        'is used but can not be mapped to any one defined in '//CHAR(13)// &
                        'RIETAN spgra file (up to v1.51b).')
      GOTO 999
  993 CALL ErrorMessage('Error writing RIETAN int/plt file.')
      GOTO 999
  997 CALL ErrorMessage('Error opening/reading RIETAN template file.')
      GOTO 999
  998 CALL ErrorMessage('Error writing RIETAN ins file.')
      GOTO 999
      RETURN

      CONTAINS

      INTEGER FUNCTION GetSettingNumb(string)

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN) :: string

      INTEGER N, L

      L = LEN_TRIM(string)
      IF ( L .LT. 2 ) THEN
        N = 1
        GOTO 10
      ENDIF
      N = 0
      SELECT CASE (string(1:2))
      CASE (':a', ':-')
        N = 0
      CASE (':b')
        IF ( L .EQ. 2 ) THEN
          N = 1
        ELSE
          N = SCAN('123', string(3:3))
        ENDIF
      CASE (':c')
        IF ( L .EQ. 2 ) THEN
          N = 2
        ELSE
          N = SCAN('123', string(3:3))
          IF ( N .GT. 0 ) N = N + 3
        ENDIF
      CASE (':1', ':H')
        IF ( L .EQ. 2 ) N = 1
      CASE (':2', ':R')
        IF ( L .EQ. 2 ) N = 2
      END SELECT

 10   GetSettingNumb = N
      RETURN

      END FUNCTION GetSettingNumb

      END FUNCTION WriteRIETANFiles
!
!*****************************************************************************
!
! This function writes atom infomation to the ins file, 
! and restraints to a .pha file to be processed later.
      INTEGER FUNCTION WriteRIETANPhaseInfo(FileNameBase, hFileIns)

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN) :: FileNameBase
      INTEGER, INTENT (IN) :: hFileIns

      LOGICAL, EXTERNAL :: PutAtomsForSpecailPosition
      LOGICAL, EXTERNAL :: PutRestraints, RIETANWriteDistance, RIETANWriteAngle, RIETANWritePlane
      INTEGER hSP_file, hFileTmp
      PARAMETER ( hFileTmp = 118, hSP_file = 60 )
      INTEGER I, iNumAtom, id(4), p1, p2, prm_seq
      REAL xyz(3), sof, biso, biso0, biso1, prm_value
      CHARACTER*(120) tLine
      CHARACTER*(16) word, lab, lab1, prm_name
      CHARACTER*(2) symbol
      CHARACTER*(3) tKwStr(4)
      DATA tKwStr /' x ',' y ',' z ','occ'/

      ! Initialise to failure
      WriteRIETANPhaseInfo = 1

! We must call ShowWizardWindowRietveld() here, which will fill
! Xato (and all the RR variables). The Wizard window is suppressed because of the iRietveldMethod flag.
      CALL ShowWizardWindowRietveld(RR_SA_Sol)
      IF ( PutAtomsForSpecailPosition() ) GOTO 996
      OPEN(UNIT=hSP_file, FILE="special_positions.out", STATUS='old', ERR=996)
      OPEN(UNIT=hFileTmp, STATUS='SCRATCH', ERR=996)

      READ(hSP_file, *, ERR=996) word, word, biso0
      READ(hSP_file, *, ERR=996)
      iNumAtom = 0
      id(4) = 1
      DO WHILE ( .NOT. EOF(hSP_file) )
        READ(hSP_file, '(A)', ERR=996) tLine
        READ(tLine, *, ERR=996) word
        IF ( word .EQ. 'prm' ) THEN
          READ(tLine, *, ERR=996) word, prm_name, prm_value
          IF ( prm_name(1:1) .EQ. '!' ) prm_name = prm_name(2:)
          CYCLE
        ENDIF
        IF ( word .NE. 'site' ) CYCLE
        READ(tLine, *, ERR=996) word, lab
        p1 = INDEX(tLine, tKwStr(1))
        IF ( p1 .LE. 0 ) GOTO 996
        DO i = 1, 3
          p2 = index(tLine(p1:), tKwStr(i+1))
          IF ( p2 .LE. 0 ) GOTO 996
          p2 = p1 + p2 - 1
          id(i) = parse_xyz_string(tLine(p1+3:p2-1))
          IF ( id(i) .LT. 0 ) GOTO 996
          p1 = p2
        ENDDO
        READ(tLine(p2:), *, err=996) word, symbol, sof
! Temp fix for occ [i.e. sof]: Rietan requires true site occupancy (<=1.0). As 
! special_position.exe will multiply occ with site multiplicity, this may screw thing up.
        IF ( sof .GT. 1.0 ) sof = 1.0
        biso = biso0
        IF ( symbol .EQ. 'H ' ) biso = 1.2 * biso0
        WRITE(tLine, '(A,1X,F4.2,1X,4F10.6,1X,1H0,4I1)', ERR=999) TRIM(lab)//'/'//symbol, &
                                                                  sof,xyz(1:3),biso,id
        WRITE(hFileIns, '(A/A)', ERR=999) ' #@DASH@MARK@P  '//TRIM(tLine), TRIM(tLine)
        IF ( iNumAtom .EQ. 0 ) THEN
          id(4) = 2
          biso1 = biso
          lab1 = lab
        ELSE
! constraint Biso to the first atom's
          WRITE(word, '(F6.2)', ERR=999) biso / biso1
          WRITE(hFileTmp, '(1X,A)', ERR=999) 'A('//TRIM(lab)//',B)='//TRIM(word)// &
                                             '*A('//TRIM(lab1)//',B)'
        ENDIF
        iNumAtom = iNumAtom + 1
      END DO
      CLOSE(hSP_file)
      WRITE(hFileIns, '(/A/A/)', ERR=999) 'end if', '} End of atoms'
      WRITE(hFileIns, '(A)', ERR=999) 'If NMODE <> 1 and NMODE <> 4 then'
      WRITE(hFileIns, '(A/)', ERR=999) 'Constraints {'
      REWIND (hFileTmp)
      DO WHILE ( .NOT. EOF(hFileTmp) )
        READ(hFileTmp, '(A)', ERR=999) tLine
        WRITE(hFileIns, '(A)', ERR=999) tLine
      ENDDO
      IF ( iNumAtom .GT. 1 ) WRITE(hFileIns, '(/A/)', ERR=999) '} End of constraints'
      CLOSE(hFileTmp)

      OPEN(UNIT=hFileTmp, FILE=TRIM(FileNameBase)//'.pha', &
           STATUS='unknown', ERR=997)
      WRITE(hFileTmp, '(A)', ERR=997) '# restraints'
      IF ( PutRestraints(hFileTmp, RIETANWriteDistance, RIETANWriteAngle, &
                         RIETANWritePlane) ) GOTO 997
      CLOSE(hFileTmp)

      WriteRIETANPhaseInfo = 0
      RETURN

  996 CALL ErrorMessage('Error writing/reading special_positions (Rietveld).')
      CLOSE(hSP_file)
      CLOSE(hFileTmp)
      RETURN
  997 CALL ErrorMessage('Error writing restraint file for RIETAN (Rietveld).')
      CLOSE(hFileTmp)
      RETURN
  999 CALL ErrorMessage('Error writing RIETAN ins file (Rietveld).')
      CLOSE(hSP_file)
      CLOSE(hFileTmp)
      RETURN

      CONTAINS

! This function decodes a coordinate string generated by special_position.exe and
! re-write in Rietan format. Return Rietan id (0-2), or -1 (failed)
      INTEGER FUNCTION parse_xyz_string(string)

      IMPLICIT NONE

      CHARACTER*(*), INTENT (INOUT) :: string

      CHARACTER*(16), word
      INTEGER code, p
      REAL v

      p = SCAN(string,';')
      IF ( p .GT. 0 ) string(p:p) = ' '
      code = -1
      READ(string, *, ERR=10) word
      SELECT CASE (word)
      CASE ('ref_flag')
! normal variable
        READ(string, *, ERR=10) word, xyz(i)
        code = 1
      CASE ('=')
        p = INDEX(string, 'dash_')
        IF ( p .GT. 0 ) THEN
! constrainted
          READ(string(p:), *, ERR=10) word
          IF ( word .NE. prm_name ) GOTO 10
          WRITE(hFileTmp, '(1X,A)') 'A('//TRIM(lab)//','//tKwStr(i)(2:2)//')'//string(:p-1)// &
                                    'A('//TRIM(lab)//','//tKwStr(prm_seq)(2:2)//')'// &
                                    string(p+LEN_TRIM(word):)
          xyz(i) = 0.0
          code = 2
        ELSE
! fraction const
          READ(string, *, ERR=10) word, xyz(i)
          p = SCAN(string, '/')
          IF ( p .GT. 0 ) THEN
            READ(string(p+1:), *, ERR=10) v
            xyz(i) = xyz(i) / v
          ENDIF
          code = 0
        ENDIF
      CASE DEFAULT
        IF ( word(:5) .EQ. 'dash_' ) THEN
          IF ( word .NE. prm_name ) GOTO 10
! constraint prm
          xyz(i) = prm_value
          prm_seq = i
          code = 0
        ELSE
! const
          READ(string, *, ERR=10) xyz(i)
          code = 0
        ENDIF
      END SELECT
  10  parse_xyz_string = code
      RETURN

      END FUNCTION parse_xyz_string

      END FUNCTION WriteRIETANPhaseInfo
!
!*****************************************************************************
!
      INTEGER FUNCTION CopyRIETANRestraints (TheFileName)

      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      CHARACTER*(*) R_MARK
      INTEGER iRMarkLen, hFileFfe, hFileInsR, hFileInsW, hFilePha
      PARAMETER ( R_MARK = ' #@DASH@MARK@R ', iRMarkLen = LEN(R_MARK) )
      PARAMETER ( hFileFfe = 60, hFileInsR = 116, hFileInsW = 117, hFilePha = 118 )
      CHARACTER(MaxPathLength) :: tDirName, tFileName, FileNameBase
      CHARACTER*8 tExtension
      CHARACTER*120 tLine
      CHARACTER*1 w
      LOGICAL exists
      INTEGER ExtLength, iBaseLen, a1, a2, a3, n
      REAL value, std

      ! Initialise to failure
      CopyRIETANRestraints = 1

! width of atom sequence, different between 2000 and fp
      w = '2'
      IF ( INDEX(RIETANEXE, '-FP') .GT. 0 .OR. INDEX(RIETANEXE, '-fp') .GT. 0 ) w = '3'

      INQUIRE(FILE=TheFileName, EXIST=exists)
      IF ( .NOT. exists ) GOTO 996
      ExtLength = len(tExtension)
      CALL SplitPath2(TheFileName, tDirName, tFileName, tExtension, ExtLength)
      FileNameBase = TRIM(tDirName)//TRIM(tFileName)
      iBaseLen = LEN_TRIM(FileNameBase)
      INQUIRE(FILE=FileNameBase(:iBaseLen)//'.ffe', EXIST=exists)
      IF ( .NOT. exists ) GOTO 998
! Wind ffe file
      OPEN(UNIT=hFileFfe, FILE=FileNameBase(:iBaseLen)//'.ffe', STATUS='unknown', ERR=998)
      DO WHILE ( .NOT. EOF(hFileFfe) )
        READ(hFileFfe, '(A)', ERR=998) tLine
        IF ( tLine(1:34) .EQ. ' INTERATOMIC DISTANCE IN ANGSTROMS') EXIT
      END DO
! Copy ins up to restraint record
      OPEN(UNIT=hFileInsR, FILE=TheFileName, STATUS='old', ERR=996)
      OPEN(UNIT=hFileInsW, FILE=TRIM(TheFileName)//'.tmp', &
           STATUS='unknown', ERR=996)
      DO WHILE ( .NOT. EOF(hFileInsR) )
        READ(hFileInsR, '(A)', Err=996) tLine
        WRITE(hFileInsW, '(A)', Err=996) TRIM(tLine)
        IF ( tLine(:iRMarkLen) .EQ. R_Mark ) THEN
          DO WHILE ( .NOT. EOF(hFileInsR) )
            READ(hFileInsR, '(A)', Err=996) tLine
            IF ( tLine(:iRMarkLen+3) .EQ. R_Mark//'END' ) THEN
              BACKSPACE(hFileInsR)
              GOTO 20
            ENDIF
          END DO
          EXIT
        ENDIF
      END DO
      GOTO 996
! Copy distance and angle restraints from pha to ins and ffe files
  20  OPEN(UNIT=hFilePha, FILE=FileNameBase(:iBaseLen)//'.pha', STATUS='old', ERR=997)
      n = 0
      DO WHILE ( .NOT. EOF(hFilePha) )
        READ(hFilePha, '(A)', Err=997) tLine
        IF ( tLine(1:4) .NE. 'DIST') CYCLE
        READ(tLine(5:), *, Err=997) a1, a2, value, std
        n = n + 1
        WRITE(hFileFfe,'(I6,4X,2(A,I'//w//'),A/)', ERR=998) n,' (', a1,',    0) (',a2,',    0)'
        WRITE(hFileInsW,'(I6,2(X,F8.4))', Err=996) n, value, std
      END DO
      WRITE(hFileFfe,'(//A)') ' BOND ANGLE IN DEGREES.  CENTRAL ATOM IS VERTEX'
      REWIND(hFilePha, Err=997)
      DO WHILE ( .NOT. EOF(hFilePha) )
        READ(hFilePha, '(A)', Err=997) tLine
        IF ( tLine(1:4) .NE. 'ANGL') CYCLE
        READ(tLine(5:), *, Err=997) a1, a2, a3, value, std
        n = n + 1
        WRITE(hFileFfe,'(I6,4X,3(A,I'//w//'),A/)', ERR=998) n,' (', a1,',    0) (',a2, &
                         ',    0) (', a3,',    0)'
        WRITE(hFileInsW,'(I6,2(X,F8.2))', Err=996) n, value, std
      END DO
      CLOSE(hFileFfe)
      CLOSE(hFilePha)
! Copy rest of ins
      DO WHILE ( .NOT. EOF(hFileInsR) )
        READ(hFileInsR, '(A)', Err=996) tLine
        WRITE(hFileInsW, '(A)', Err=996) TRIM(tLine)
      END DO
      CLOSE(hFileInsW)
      CLOSE(hFileInsR, STATUS='delete')
      CALL InfoError(1)
      CALL IOsRenameFile(TRIM(TheFileName)//'.tmp', TRIM(TheFileName))
      IF ( InfoError(1) .NE. 0 ) GOTO 996
      CopyRIETANRestraints = 0

  999 CLOSE(hFileFfe)
      CLOSE(hFilePha)
      CLOSE(hFileInsW)
      CLOSE(hFileInsR)
      RETURN

  996 CALL ErrorMessage('Error copying RIETAN ins file (Rietveld).')
      GOTO 999
  997 CALL ErrorMessage('Error reading RIETAN pha file (Rietveld).')
      GOTO 999
  998 CALL ErrorMessage('Error reading/writing RIETAN ffe file (Rietveld).')
      GOTO 999
      RETURN

      END FUNCTION CopyRIETANRestraints
!
!*****************************************************************************
!
      LOGICAL FUNCTION RIETANWriteDistance (hFile, iAtom1, iAtom2, iElement1, iElement2, Distance)

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: hFile, iAtom1, iAtom2, iElement1, iElement2
      REAL, INTENT (IN) :: Distance

      RIETANWriteDistance = .TRUE.
      ! width = 0.0
      WRITE(hFile, '(A,I3,1X,I3,F9.4,A)', ERR=999) 'DIST ', iAtom1, iAtom2, Distance, ' 0.0'
      RIETANWriteDistance = .FALSE.
  999 RETURN
      ! No operation, only prevent compiler complians unused varibles
      IF ( .FALSE. .AND. iElement1+iElement2 .EQ. 0 ) RETURN 

      END FUNCTION RIETANWriteDistance
!
!*****************************************************************************
!
      LOGICAL FUNCTION RIETANWriteAngle (hFile, iAtom1, iAtom, iAtom2, &
                                       iElement1, iElement, iElement2, Angle)

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: hFile, iAtom1, iAtom, iAtom2, iElement1, iElement, iElement2
      REAL, INTENT (IN) :: Angle

      RIETANWriteAngle = .TRUE.
      ! width = 1.0
      WRITE(hFile, '(A,3(I3,1X),F9.4,A)', ERR=999) 'ANGL ', iAtom1, iAtom, iAtom2, Angle, ' 1.0'
      RIETANWriteAngle = .FALSE.
  999 RETURN
      ! No operation, only prevent compiler complians unused varibles
      IF ( .FALSE. .AND. iElement1+iElement+iElement2 .EQ. 0 ) RETURN 

      END FUNCTION RIETANWriteAngle
!
!*****************************************************************************
!
      LOGICAL FUNCTION RIETANWritePlane (hFile, iNumbAtom, iAtoms, iElements)

      IMPLICIT NONE

      INTEGER, INTENT (IN) :: hFile, iNumbAtom, iAtoms(iNumbAtom), iElements(iNumbAtom)

      INTEGER I

      RIETANWritePlane = .TRUE.
      ! GSAS planar group only takes upto 12 atoms
      WRITE(hFile, '(A$)', ERR=999) 'PLAN '
      DO I = 1, MIN(12, iNumbAtom)
        WRITE(hFile, '(I3,X$)', ERR=999) iAtoms(I)
      ENDDO
      ! width
      WRITE(hFile, '(A)', ERR=999) ' 0.0'
      RIETANWritePlane = .FALSE.
  999 RETURN
      ! No operation, only prevent compiler complians unused varibles
      IF ( .FALSE. .AND. iElements(1) .EQ. 0 ) RETURN 

      END FUNCTION RIETANWritePlane
!
!*****************************************************************************
!
