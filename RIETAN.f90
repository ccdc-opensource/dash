! This file is part of DASH.
! SPDX-Identifier: MIT
!
! Copyright 2007 Cambridge Crystallographic Data Centre
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
!*****************************************************************************
!
      INTEGER FUNCTION Launch_RIETAN(input_file_name)

      USE WINTERACTER
      USE DRUID_HEADER
      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: input_file_name

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      INTEGER, EXTERNAL :: SetOptions, CheckRIETANExe
      CHARACTER*20, EXTERNAL :: Integer2String

      INTEGER nCycle, opt, IErrCode
      CHARACTER(MaxPathLength) tScriptName
      CHARACTER(1) tOptStr
      INTEGER IDummy
      INTEGER,EXTERNAL:: SetRefineParameters, CopyRIETANRestraints

      ! Initialise to failure
      Launch_RIETAN = 1
      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SAW_Page7_RIETAN)
      CALL DASHWDialogGetInteger(IDF_NCYCL, nCycle)
      CALL PopActiveWindowID
      opt = SetOptions()
      IF ( opt .LE. 0 ) GOTO 996
      tOptStr = '0'
      IF ( IAND(opt, Z'100') .NE. 0 ) tOptStr = ''
      IDummy =  SetRefineParameters(input_file_name, opt)
      IF ( CheckRIETANExe(RIETANEXE, tScriptName) .NE. 0 ) RETURN
      ! Launch RIETAN vis script and wait for it to return
      IErrCode = InfoError(1) ! Clear errors
!        CALL IOSCommand('cmd.exe /k start /b '//TRIM(tScriptName)//' '// &
      CALL IOSCommand('"'//TRIM(tScriptName)//'" "'//TRIM(RIETANEXE)//'" '// &
                      '"'//TRIM(input_file_name)//'" '//tOptStr, ProcBlocked)
      IF ( InfoError(1) .NE. 0 ) GOTO 998

! Follow init, run RIETAN and ORFFE to generate a .ffe file
      IF ( IAND(opt, Z'01') .NE. 0 ) THEN
        ! RIETAN tends to break without parameters to be refined, set both bkg and scale
        IDummy = SetRefineParameters(input_file_name, Z'100C')
        tOptStr = '2'
        CALL IOSCommand('"'//TRIM(tScriptName)//'" "'//TRIM(RIETANEXE)//'" '// &
                      '"'//TRIM(input_file_name)//'" '//tOptStr, ProcBlocked)
        IF ( InfoError(1) .NE. 0 ) GOTO 998
        IDummy = CopyRIETANRestraints(input_file_name)
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

#ifdef _WIN32
      CHARACTER*(*), PARAMETER :: ctScriptFile = 'RIETAN.CMD'
#else
      CHARACTER*(*), PARAMETER :: ctScriptFile = 'rietan.sh'
#endif
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
#ifdef _WIN32
      CALL StrUpperCase(tExtension)
      IF ( tExtension .NE. 'EXE' ) goto 998
#endif
      ScriptName = TRIM(InstallationDirectory)//ctScriptFile
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
                        '"'//ctScriptFile//'" can be found at default locations.'//CHAR(13))
      ScriptName = ''
      RETURN

      END FUNCTION CheckRIETANExe
!
!*****************************************************************************
!
      INTEGER FUNCTION SetOptions()

      USE DRUID_HEADER

      IMPLICIT NONE

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      INTEGER opt

      SetOptions = 0
      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SAW_Page7_RIETAN)
      opt = 0
      IF ( DASHWDialogGetCheckBoxLogical(IDC_Initialisation) ) opt = opt + Z'03'
      IF ( DASHWDialogGetCheckBoxLogical(IDC_Background) ) opt = opt + Z'04'
      IF ( DASHWDialogGetCheckBoxLogical(IDC_Scale) ) opt = opt + Z'08'
      IF ( DASHWDialogGetCheckBoxLogical(IDC_PO) ) opt = opt + Z'10'
      IF ( DASHWDialogGetCheckBoxLogical(IDC_Biso) ) opt = opt + Z'20'
      IF ( DASHWDialogGetCheckBoxLogical(IDC_Coordinates) ) opt = opt + Z'40' + Z'80'
      IF ( DASHWDialogGetCheckBoxLogical(IDC_WriteCIF) ) opt = opt + Z'100'
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

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SAW_Page7_RIETAN)
      IF ( ext_RR_stage .GT. 2 .AND. &
           .NOT. DASHWDialogGetCheckBoxLogical(IDC_UseDASHRecommendation) ) THEN
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
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates,       .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,          .FALSE.)
        CASE ( 2 ) 
          CALL WDialogPutCheckBoxLogical(IDC_Initialisation,    .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Scale,             .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,        .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_PO,                .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,              .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates,       .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_WriteCIF,          .TRUE.)
        CASE ( 3 )
          CALL WDialogPutCheckBoxLogical(IDC_Initialisation,    .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Scale,             .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Background,        .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_PO,                .FALSE.)
          CALL WDialogPutCheckBoxLogical(IDC_Biso,              .TRUE.)
          CALL WDialogPutCheckBoxLogical(IDC_Coordinates,       .TRUE.)
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

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical
      INTEGER, EXTERNAL :: Launch_RIETAN

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SAW_Page7_RIETAN)
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
              IF ( DASHWDialogGetCheckBoxLogical(IDC_Initialisation) ) THEN
                ext_RR_stage = 1
                CALL UpdateRIETANCheckBoxes
              ENDIF
              IF ( Launch_RIETAN(ext_RR_input_file_name) .EQ. 0 ) THEN
                ext_RR_stage = ext_RR_stage + 1
                CALL UpdateRIETANCheckBoxes
              ENDIF
            CASE (IDB_View)
              CALL Launch_Viewer(ext_RR_input_file_name, '.cif')
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
      USE TAVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN) :: TheFileName
      INTEGER, INTENT (IN) :: OptWord
 
      CHARACTER*(*), PARAMETER :: ctPMark = ' #@DASH@MARK@P '
      INTEGER, PARAMETER :: chFileW = 116, chFileR = 117, ciPMarkLen = LEN(ctPMark)
      INTEGER p, flag, nCycle, kLen, iNBT, IErrCode
      REAL tk, finc
      CHARACTER (40) tKeyWord
      CHARACTER (80) tLine, tLine2

      ! Initialise to failure
      SetRefineParameters = 1

      OPEN(UNIT=chFileW,FILE=TRIM(TheFileName)//'.tmp',STATUS='unknown',ERR=998)
      OPEN(UNIT=chFileR,FILE=TRIM(TheFileName),STATUS='old',ERR=997)
      iNBT = NumOfBkgTerm

      DO WHILE ( .TRUE. )
        READ(chFileR, '(A)', ERR=997, END=300) tLine
        WRITE(chFileW, '(A)', ERR=998) TRIM(tLine)
        IF ( tLine(1:ciPMarkLen) .NE. ctPMark ) CYCLE
        READ(tLine(ciPMarkLen+1:),*, ERR=998) tKeyWord
        kLen = LEN_TRIM(tKeyWord)
        IF ( kLen .LE. 0 ) CYCLE
        READ(chFileR, '(A)', ERR=997, END=300) tLine2

        IF ( tKeyWord .EQ. 'NCYCL') THEN
          CALL PushActiveWindowID
          CALL SelectDASHDialog(IDD_SAW_Page7_RIETAN)
          CALL DASHWDialogGetInteger(IDF_NCYCL, nCycle)
          CALL PopActiveWindowID
          WRITE(chFileW, '(A,I3)', ERR=998) ' NCYCL = ', nCycle
          CYCLE
        ELSE IF ( tKeyWord .EQ. 'TK') THEN
          CALL PushActiveWindowID
          CALL SelectDASHDialog(IDD_SAW_Page7_RIETAN)
          CALL DASHWDialogGetReal(IDF_REAL_RIETAN_WEIGHT_TK, tk)
          CALL PopActiveWindowID
          WRITE(chFileW, '(A,F10.2)', ERR=998) ' TK = ', tk
          CYCLE
        ELSE IF ( tKeyWord .EQ. 'FINC') THEN
          CALL PushActiveWindowID
          CALL SelectDASHDialog(IDD_SAW_Page7_RIETAN)
          CALL DASHWDialogGetReal(IDF_REAL_RIETAN_WEIGHT_FINC, finc)
          CALL PopActiveWindowID
          WRITE(chFileW, '(A,F10.2)', ERR=998) ' FINC = ', finc
          CYCLE
        ELSE IF ( IAND(OptWord, Z'01') .NE. 0 ) THEN
! Initial: Replace next line. 
! Note: The ctPMark line in template file for refine parameters gives the initial values 
!       and their refine IDs (with all refinable set to on). As the initial run is LeBail 
!       when those should not present (eg. atoms) are excluded by if-then, the IDs will
!       only be toggled according to GUI setting in the following runs.
          IF ( tKeyWord(:4) .EQ. 'BKGD' ) THEN
! As the number of bkg terms is set by user, we also need to limit it at initial stage
            tLine2 = tLine(ciPMarkLen+1:)
            IF ( iNBT .LE. 0 ) CALL set_parameter_id(0)
            iNBT = iNBT - 1
            WRITE(chFileW, '(A)', ERR=998) TRIM(tLine2)
          ELSE
            WRITE(chFileW, '(A)', ERR=998) TRIM(tLine(ciPMarkLen+1:))
          ENDIF
          CYCLE
        ENDIF

! Toggle options
        SELECT CASE (tKeyWord)
        CASE ('NMODE')
          flag = IAND(OptWord, Z'01')
        CASE ('BKGD0','BKGD1','BKGD2','BKGD3','BKGD4','BKGD5', &
              'BKGD6','BKGD7','BKGD8','BKGD9','BKGD10','BKGD11')
          flag = IAND(OptWord, Z'04')
          IF ( iNBT .LE. 0 ) flag = 0
          iNBT = iNBT - 1
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
          p = ciPMarkLen + SCAN(tLine(ciPMarkLen+1:),tKeyWord(1:1)) + kLen
          IF ( tLine(p:p) .EQ. '/' ) THEN
            ! Atoms
            IF ( tLine(p+1:p+2) .NE. 'H ' ) THEN
              flag = IAND(OptWord, Z'40')
            ELSE
              flag = IAND(OptWord, Z'80')
            ENDIF
            CALL set_parameter_id(flag, 2, 4)
            CALL set_parameter_id(IAND(OptWord, Z'20'), 5, 5)
            WRITE(chFileW, '(A)', ERR=998) TRIM(tLine2)
            CYCLE
          ENDIF
          ! If any left
          flag = 0
        END SELECT
        CALL set_parameter_id(flag)
        WRITE(chFileW, '(A)', ERR=998) TRIM(tLine2)

      ENDDO
 300  CONTINUE
      CLOSE(chFileW)
      CLOSE(chFileR, STATUS='delete')
!      CALL IOSCommand('CMD.exe /C copy /Y "'//TRIM(TheFileName)//'.tmp" '// &
!                      '"'//TRIM(TheFileName)//'"', ProcSilent+ProcBlocked)
!      CALL IOsDeleteFile(TRIM(TheFileName))
      IErrCode = InfoError(1)
      CALL IOsRenameFile(TRIM(TheFileName)//'.tmp', TRIM(TheFileName))
      IF ( InfoError(1) .NE. 0 ) GOTO 996
      SetRefineParameters = 0

  999 CLOSE(chFileW)
      CLOSE(chFileR)
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
      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      INCLUDE 'params.inc'
      INCLUDE 'GLBVAR.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      LOGICAL, EXTERNAL :: IsConstantStepWidth, Confirm
      REAL, EXTERNAL :: FnWavelengthOfMenuOption
      CHARACTER*(*), PARAMETER :: ctMark = ' #@DASH@MARK@ '
      INTEGER, PARAMETER :: chFileIns = 116, chFileTmp = 117, ciMarkLen = LEN(ctMark)
      INTEGER iBaseLen, i, j, tLen, ExtLength, i1, i2, iNRec
      CHARACTER (MaxPathLength) tDirName, tFileName, FileNameBase
      CHARACTER (40) tExtension, tKeyWord
      INTEGER tIRadSelection, iRad
      CHARACTER (80) tLine
      CHARACTER(2), PARAMETER :: GSAS_LINE_END = CHAR(13)//CHAR(10)
      REAL Pola, yMax, Y, r12, YScale, YEsdScale, StepWidth, tmpY(5), tmpE(5)
      INTEGER NumOfAtmPerElm(1:MaxElm), iFrg, spg_set, nBeam
      LOGICAL YScaled, SetMinY

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
      OPEN(UNIT=chFileTmp,FILE=tFileName(1:tLen),STATUS='unknown',ERR=993)
      IF ( Rietan_FP ) THEN
        ! RIETAN-FP supports GSAS ESD file and reads its esd data
        ! GSAS Raw file: assume CONS and ESD
        StepWidth = XBIN(2) - XBIN(1)
        IF (.NOT. IsConstantStepWidth(5E-6)) THEN
          IF ( .NOT. Confirm( &
               'The pattern step width seems non-constant.'//CHAR(13)// &
               'As a GSAS raw file is used (to include ESD), this format requires a'//CHAR(13)// &
               'constant step width. To resolve this, the step width can be set to'//CHAR(13)// &
               'the average of whole pattern.'//CHAR(13)//CHAR(13)// &
               'Do you want to use an averaged step width?') &
             ) GOTO 999
          StepWidth = (XBIN(NBIN) - XBIN(1)) / (NBIN - 1)
        ENDIF
        WRITE(tLine, '(F10.6,5X,A)', ERR=993) ALambda, 'Exported by DASH'
        WRITE(chFileTmp, '(A80,A2$)', ERR=993) tLine, GSAS_LINE_END
        iNRec = NBIN / 5
        IF ( MOD(NBIN,5) .NE. 0 ) iNRec = iNRec + 1
        WRITE(tLine, '(A,2(I6,1X),A,1X,2(F10.4,1X),A)', ERR=993) 'BANK 1 ', NBIN, iNRec, 'CONS', &
              XBIN(1) * 100.0,StepWidth * 100.0,'0.0 0.0 ESD'
        WRITE(chFileTmp, '(A80,A2$)', ERR=993) tLine, GSAS_LINE_END
        ! Scale YOBIN, EBIN to fit F8.2, allowing negative values
        YScale = 1.0
        YScaled = .FALSE.
        YMax = MAX(MAXVAL(YOBIN(1:NBIN)), MAXVAL(EBIN(1:NBIN)))
        DO WHILE ( YMax * YScale .GE. 1E4 )
          YScale = YScale / 10.0
          YScaled = .TRUE.
        ENDDO
        YMax = YMax * YScale
        YEsdScale = SQRT(YScale)
        i1 = 0
        DO I = 1, iNRec
          DO J = 1, 5
            i1 = i1 + 1
            IF ( i1 .GT. NBIN ) EXIT
            tmpY(j) = YOBIN(i1) * YScale
            tmpE(j) =  EBIN(i1) * YEsdScale
            ! Rietan does not accept zero, fix to 0.01 (F8.2)
            IF ( ABS(tmpY(j)) .LT. 0.01 ) tmpY(j) = SIGN(0.01, tmpY(j))
          END DO
          IF ( J .GT. 1 ) THEN
            i2 = J - 1
            WRITE(tLine, '(10F8.2)', ERR=993) (tmpY(J), tmpE(J), J = 1, i2)
            WRITE(chFileTmp, '(A80,A2$)', ERR=993) tLine, GSAS_LINE_END
          ENDIF
        ENDDO
        IF ( YScaled ) CALL DebugErrorMessage('Intensity is rescaled to fit GSAS ESD format')
      ELSE ! RIETAN-FP
        ! RIETAN format int file
        SetMinY = .FALSE.
        IF (MINVAL(YOBIN(1:NBIN)) .LT. 1.0) THEN
          SetMinY = Confirm( &
               'There is at least one data point in the pattern with intensity < 1.0.'//CHAR(13)// &
               'RIETAN may have difficulty to process zero or negative intensity.'//CHAR(13)//CHAR(13)// &
               'If this is caused by DASH background subtraction,'//CHAR(13)// &
               'it can be switched off in the Wizard to pass raw intensity.'//CHAR(13)//CHAR(13)// &
               'Do you want to set all intensities >= 1.0 ?')
        ENDIF
!        WRITE(chFileTmp, '(A,F10.6,A)', ERR=993) '* ', ALambda, ' Exported by DASH'
!        WRITE(chFileTmp, *, ERR=993) NBIN, XBIN(1), XBIN(2) - XBIN(1)
        WRITE(chFileTmp, '(A)', ERR=993) 'GENERAL$'
        WRITE(chFileTmp, *, ERR=993) NBIN
        yMax = 0.0
        DO I = 1, NBIN
          Y = YOBIN(I)
! Rietan format seems not accept zero or negative, as it needs to work out ESD from intensity
          IF ( SetMinY ) Y = MAX(Y, 1.0) ! Y = SIGN(MAX(ABS(Y), 1.0), Y)
!          WRITE(chFileTmp, *, ERR=993) Y
          WRITE(chFileTmp, '(F10.5,1X,1PG15.7E2 )', ERR=993) XBIN(I), Y
          yMax = MAX(yMax, Y)
        ENDDO
      ENDIF
      CLOSE(chFileTmp)
      IF ( XBIN(1) .LT. 0.1 ) CALL InfoMessage( &
         'The diffraction pattern starts at '// &
         'an extremely low point ( < 0.1 degree two-theta ).'//CHAR(13)// &
         'If Rietan complains with "Bad diffraction angle(s)", you may '//&
         'go back to truncate the data. ' )
      ! RIETAN Gnuplot file
      tFileName = FileNameBase(1:iBaseLen)//'.plt'
      tLen = iBaseLen + 4
      OPEN(UNIT=chFileTmp,FILE=tFileName(1:tLen),STATUS='unknown',ERR=993)
      WRITE(chFileTmp, '(A)') '# GNUPLOT script for plotting RIETAN2000 itx file'
      WRITE(chFileTmp, '(3(A,F10.2))') 'xmin=',XBIN(1),'; xmax=',XBIN(NBIN),'; ymax=', yMax
      WRITE(chFileTmp, '(A)') 'ymin=-ymax/4; offset_delta=-0.11*(ymax-ymin)'
      WRITE(chFileTmp, '(A)') 'len_bar=0.011*(ymax-ymin); y_phase1=-0.03*(ymax-ymin)'
      WRITE(chFileTmp, '(A)') 'set xrange [xmin:xmax]'
      WRITE(chFileTmp, '(A)') 'set yrange [ymin:ymax]'
      WRITE(chFileTmp, '(A)') 'set pointsize 0.7'
      WRITE(chFileTmp, '(A)') 'set mxtics 2'
      WRITE(chFileTmp, '(A)') 'set mytics 2'
      WRITE(chFileTmp, '(A)') 'set bar 0'
      WRITE(chFileTmp, '(A)') 'set ticscale 1 1'
      WRITE(chFileTmp, '(A)') 'set xlabel "2-theta/deg" 0.0, 0.2 font "Helvetica, 12"'
      WRITE(chFileTmp, '(A)') 'set ylabel "Intensity" 0.5, 0.0 font "Helvetica, 12"'
      WRITE(chFileTmp, '(A)') 'set terminal windows color "Helvetica" 10'
      WRITE(chFileTmp, '(A)') 'plot \'
      WRITE(chFileTmp, '(A)') ' "'//FileNameBase(LEN_TRIM(tDirName)+1:iBaseLen)//'.itx'// &
                              '" using 1:2 notitle with points 2, \'
      WRITE(chFileTmp, '(A)') ' "" using 1:3 notitle with lines linetype 8 linewidth 1, \'
      WRITE(chFileTmp, '(A)') ' "" using  1:($2 - $3 + offset_delta) notitle with lines '// &
                              'linetype 4 linewidth 1, \'
      WRITE(chFileTmp, '(A)') ' "" using  8:(y_phase1):(len_bar) notitle with yerrorbars '// &
                              'linetype 3 linewidth 1 pointtype 0'
      CLOSE(chFileTmp)
      CALL SelectDASHDialog(IDD_PW_Page4)
      r12 = 0.5
      IF ( JRadOption .EQ. 1 ) THEN
        nBeam = 1
        IF ( DASHWDialogGetCheckBoxLogical(IDC_Monochromated) ) THEN
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
      IF ( Rietan_FP ) THEN
        tFileName = TRIM(InstallationDirectory)//'RIETANFP.tem'
      ELSE
        tFileName = TRIM(InstallationDirectory)//'RIETAN2000.tem'
      ENDIF
      OPEN(UNIT=chFileTmp,FILE=TRIM(tFileName),STATUS='old',ERR=997)
      tFileName = FileNameBase(1:iBaseLen)//'.ins'
      tLen = iBaseLen + 4
      OPEN(UNIT=chFileIns,FILE=tFileName(1:tLen),STATUS='unknown',ERR=998)
      NumOfAtmPerElm = 0
      DO iFrg = 1, nFrag
        DO i = 1, natoms(iFrg)
          CALL INC(NumOfAtmPerElm(zmElementCSD(i,iFrg)))
        ENDDO
      ENDDO
      DO WHILE ( .TRUE. )
        READ(chFileTmp, '(A)', ERR=997, END=300) tLine
        IF ( tLine(1:ciMarkLen) .NE. ctMark ) THEN
          WRITE(chFileIns, '(A)', ERR=998) TRIM(tLine)
          CYCLE
        ENDIF
        READ(tLine(ciMarkLen+1:),*, ERR=998) tKeyWord
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
          WRITE(chFileIns, '(A)', ERR=998) ' # No chemical state kept by DASH, use element.'
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
          WRITE(chFileIns, '(A)', ERR=998) " # No Deltf' Deltf'' kept by DASH, set to zero."
          DO i = 1, MaxElm
            IF (NumOfAtmPerElm(i) .GT. 0) &
                WRITE(chFileIns, '(A)', ERR=998) ' 0.0 0.0'
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
          ENDIF
        CASE ('IKP1')
          WRITE(tLine, '(A,I3)', ERR=998) 'IKP1 = ', PO_Direction(2)
        CASE ('ILP1')
          WRITE(tLine, '(A,I3)', ERR=998) 'ILP1 = ', PO_Direction(3)
        CASE ('ATOMS')
          IF ( WriteRIETANPhaseInfo(FileNameBase, chFileIns) .NE. 0 ) GOTO 998
          CYCLE
        CASE DEFAULT
          CYCLE
        END SELECT
! Replace next line
        WRITE(chFileIns, '(A)', ERR=998) TRIM(tLine)
        READ(chFileTmp, '(A)', ERR=997, END=300)
      ENDDO
 300  CONTINUE
      WriteRIETANFiles = 0

  999 CLOSE(chFileIns)
      CLOSE(chFileTmp)
      RETURN

  990 CALL ErrorMessage('The non-standard space group:'//CHAR(13)//CHAR(13)// &
                        TRIM(SGHMaStr(NumberSGTable))//CHAR(13)//CHAR(13)// &
                        'is used but can not be mapped to any one defined in '//CHAR(13)// &
                        'RIETAN spgra file (up to v1.51b).'//CHAR(13)//CHAR(13)// &
                        'You may re-process the diffraction pattern by choosing'//CHAR(13)// &
                        'a standard space group.')
      GOTO 999
  993 CALL ErrorMessage('Error writing RIETAN int or plt file.')
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

      LOGICAL, EXTERNAL :: PutAtomsForSpecialPosition
      LOGICAL, EXTERNAL :: PutRestraints, RIETANWriteDistance, RIETANWriteAngle, RIETANWritePlane
      INTEGER, PARAMETER :: chFileTmp = 118, chSPFile = 60
      INTEGER I, iNumAtom, id(4), p1, p2, sp(2,3), prm_pos, prm_len
      LOGICAL warnned, constrain
      REAL xyz(3), sof, biso, biso0, biso1, prm_value
      CHARACTER*(120) tLine
      CHARACTER*(80) tCoordStr
      CHARACTER*(16) word, lab, lab1, prm_name
      CHARACTER*(2) symbol
      CHARACTER*(3) tKwStr(4)
      DATA tKwStr /' x ',' y ',' z ','occ'/

      ! Initialise to failure
      WriteRIETANPhaseInfo = 1

! We must call ShowWizardWindowRietveld() here, which will fill
! Xato (and all the RR variables). The Wizard window is suppressed because of the iRietveldMethod flag.
      CALL ShowWizardWindowRietveld(RR_SA_Sol)
      IF ( PutAtomsForSpecialPosition(.FALSE.) ) GOTO 996
      OPEN(UNIT=chSPFile, FILE="special_positions.out", STATUS='old', ERR=996)
      OPEN(UNIT=chFileTmp, STATUS='SCRATCH', ERR=996)

      READ(chSPFile, *, ERR=996, END=996) word, word, biso0
      READ(chSPFile, *, ERR=996, END=996)
      warnned = .FALSE.
      prm_len = 0
      iNumAtom = 0
      id(4) = 1
      DO WHILE ( .TRUE. )
        READ(chSPFile, '(A)', ERR=996, END=300) tLine
        IF ( LEN_TRIM(tLine) .LE. 0 ) CYCLE
        READ(tLine, *, ERR=996, END=996) word
        IF ( word .EQ. 'prm' ) THEN
          READ(tLine, *, ERR=996, END=996) word, prm_name, prm_value
          IF ( prm_name(1:1) .EQ. '!' ) prm_name = prm_name(2:)
          prm_len = LEN_TRIM(prm_name)
          CYCLE
        ENDIF
        IF ( word .NE. 'site' ) CYCLE
        READ(tLine, *, ERR=996, END=996) word, lab
! Fisrt pass: parse into x,y,z strings and locate first one referred to current prm_name
! Note: 
!  unlike prm in TOPAS, no free-stand refinenable parameter for constraint
! Note:
!  following comments are extracted from special_position.cpp
! // I'm assuming that only the following combinations are possible:
! // x, x, 0
! // x, -x, 0
! // x, 2x, 0
! // x, x+1/2, 0
! // x, -x+1/2, 0
        p1 = INDEX(tLine, tKwStr(1))
        IF ( p1 .LE. 0 ) GOTO 996
        prm_pos = 0
        constrain = .TRUE. 
        DO i = 1, 3
          p2 = index(tLine(p1:), tKwStr(i+1))
          IF ( p2 .LE. 0 ) GOTO 996
          p2 = p1 + p2 - 1
          ! save string boundary
          sp(1,i) = p1+3
          sp(2,i) = p2-1
          p1 = p2
          IF ( sp(1,i) .GT. sp(2,i) ) GOTO 996
          IF ( prm_len .LE. 0 ) CYCLE
          tCoordStr = tLine(sp(1,i):sp(2,i))
          IF ( INDEX(tCoordStr, prm_name(:prm_len)) .LE. 0 ) CYCLE
          IF ( SCAN (tCoordStr, '+-*/') .GT. 0 ) CYCLE
          IF ( prm_pos .EQ. 0 )  prm_pos = i ! first occur
        ENDDO
        IF ( prm_len .GT.0 .AND. prm_pos .EQ. 0 ) THEN
          IF ( .NOT. warnned )  CALL WarningMessage( &
             'Failed to generate RIETAN symmetry-constraint for atom(s) at certain'//CHAR(13)// &
             'special positions (e.g. x+1/24, -x-1/24, -x+11/24 in P21 3). This'//CHAR(13)// &
             'may be avoided by using their symmetry equivalents.'//CHAR(13)//CHAR(13)// &
             'The coordinates of such atom(s) are not constrained.')
          warnned = .TRUE.
          constrain = .FALSE. 
        ENDIF
        DO i = 1, 3
          tCoordStr = tLine(sp(1,i):sp(2,i))
          p1 = SCAN(tCoordStr,';')
          IF ( p1 .GT. 0 ) tCoordStr(p1:p1) = ' '
          IF ( .NOT. parse_xyz_string(tCoordStr, id(i)) ) GOTO 996
        ENDDO
        READ(tLine(p2:), *, ERR=996, END=996) word, symbol, sof
! Temp fix for occ [i.e. sof]: Rietan requires true site occupancy (<=1.0). As 
! special_position.exe will multiply occ with site multiplicity, this may screw thing up.
        IF ( sof .GT. 1.0 ) sof = 1.0
        biso = biso0
        IF ( symbol .EQ. 'H ' ) biso = 1.2 * biso0
        WRITE(tLine, '(A,1X,F5.2,1X,4F10.6,1X,1H0,4I1)', ERR=999) &
                     TRIM(lab)//'/'//symbol, sof,xyz(1:3),biso,id
        WRITE(hFileIns, '(A/A)', ERR=999) ' #@DASH@MARK@P  '//TRIM(tLine), TRIM(tLine)
        IF ( iNumAtom .EQ. 0 ) THEN
! Save the first atom for setting up Biso constraints of others
          id(4) = 2
          biso1 = biso
          lab1 = lab
        ELSE
! constrain Biso to the first atom's
          word = ''
          IF ( abs(biso - biso1) .GE. 0.01 ) & ! 0.01: the rounding error on F6.2
            WRITE(word, '(F6.2,1H*)', ERR=999) biso / biso1
          WRITE(chFileTmp, '(1X,A)', ERR=999) 'A('//TRIM(lab)//',B)='//TRIM(word)// &
                                              'A('//TRIM(lab1)//',B)'
        ENDIF
        iNumAtom = iNumAtom + 1
        prm_len = 0 ! only valid for single atom line
      END DO
 300  CONTINUE
      CLOSE(chSPFile)
      WRITE(hFileIns, '(/A/A/)', ERR=999) 'end if', '} End of atoms'
      WRITE(hFileIns, '(A)', ERR=999) 'If NMODE <> 1 and NMODE <> 4 then'
      WRITE(hFileIns, '(A/)', ERR=999) 'Constraints {'
      REWIND (chFileTmp)
      DO WHILE ( .TRUE. )
        READ(chFileTmp, '(A)', ERR=999, END=302) tLine
        WRITE(hFileIns, '(A)', ERR=999) tLine
      ENDDO
 302  CONTINUE
      IF ( iNumAtom .GT. 1 ) WRITE(hFileIns, '(/A/)', ERR=999) '} End of constraints'
      CLOSE(chFileTmp)

      OPEN(UNIT=chFileTmp, FILE=TRIM(FileNameBase)//'.pha', &
           STATUS='unknown', ERR=997)
      WRITE(chFileTmp, '(A)', ERR=997) '# restraints'
      IF ( PutRestraints(chFileTmp, RIETANWriteDistance, RIETANWriteAngle, &
                         RIETANWritePlane) ) GOTO 997
      CLOSE(chFileTmp)

      WriteRIETANPhaseInfo = 0
      RETURN

  996 CALL ErrorMessage('Error writing/reading special_positions (Rietveld).')
      CLOSE(chSPFile)
      CLOSE(chFileTmp)
      RETURN
  997 CALL ErrorMessage('Error writing restraint file for RIETAN (Rietveld).')
      CLOSE(chFileTmp)
      RETURN
  999 CALL ErrorMessage('Error writing RIETAN ins file (Rietveld).')
      CLOSE(chSPFile)
      CLOSE(chFileTmp)
      RETURN

      CONTAINS

! This function decodes a coordinate string generated by special_position.exe and
! re-write in Rietan format. Rietan id (0-2) is in code.
! Return false if failed
!
      LOGICAL FUNCTION parse_xyz_string(string, code)

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: string
      INTEGER,       INTENT (  OUT) :: code

      CHARACTER*(16), tWord
      CHARACTER*(1), op
      INTEGER p
      REAL v

      parse_xyz_string = .FALSE.
      READ(string, *, ERR=90, END=90) tWord
      SELECT CASE (tWord)
      CASE ('ref_flag')
! normal variable
        READ(string, *, ERR=90, END=90) tWord, xyz(i)
        code = 1
      CASE ('=')
        p = INDEX(string, 'dash_')
        IF ( p .LE. 0 ) THEN
! fraction const
          IF ( .NOT. fraction_to_real(string(LEN_TRIM(tWord)+1:), xyz(i)) ) GOTO 90
          code = 0
        ELSE
! parameter involving in constraint: prm_pos must be set now
          !IF ( prm_pos .LT. 1 .OR. prm_pos .GT. 3 ) GOTO 90
          IF ( prm_pos .EQ. i ) THEN
! parameter constrained to
            xyz(i) = prm_value
            code = 1
          ELSE
! parameter to be constrained, parse the relation, eg. 2*y-1/2, y, z
            READ(string(p:), *, ERR=90, END=90) tWord
            IF ( tWord .NE. prm_name ) GOTO 90
            xyz(i) = prm_value
            IF ( constrain ) WRITE(chFileTmp, '(1X,A$)') 'A('//TRIM(lab)//','//tKwStr(i)(2:2)//')='
            IF ( fraction_to_real(string(INDEX(string, '=')+1:p-1), v) ) THEN
              IF ( constrain ) WRITE(chFileTmp, '(F6.3,A1$)') v, '*'
              xyz(i) = v * xyz(i)
            ENDIF
            IF ( constrain ) WRITE(chFileTmp, '(A$)') 'A('//TRIM(lab)//','//tKwStr(prm_pos)(2:2)//')'
            READ(string(p+prm_len:), *, ERR=90, END=10) op
            IF ( fraction_to_real(string(p+prm_len+2:), v) ) THEN
              IF ( op .EQ. '-' ) v = -v
              IF ( constrain ) WRITE(chFileTmp, '(SP,F6.3$)') v
              xyz(i) = xyz(i) + v
            ENDIF
  10        IF ( constrain ) WRITE(chFileTmp, *)
            code = 2
            IF ( .NOT. constrain ) code = 1
          ENDIF
        ENDIF
      CASE DEFAULT
        IF ( tWord(:5) .EQ. 'dash_' ) THEN
          IF ( tWord .NE. prm_name ) GOTO 90
          IF ( prm_pos .LT. 1 .OR. prm_pos .GT. 3 ) GOTO 90
          xyz(i) = prm_value
          IF ( prm_pos .EQ. i ) THEN
! parameter constrained to
            code = 1
          ELSE
! parameter to be constrained
            WRITE(chFileTmp, '(1X,A)') 'A('//TRIM(lab)//','//tKwStr(i)(2:2)//')='// &
                                       'A('//TRIM(lab)//','//tKwStr(prm_pos)(2:2)//')'
            code = 2
          ENDIF
        ELSE
! const
          READ(string, *, ERR=90, END=90) xyz(i)
          code = 0
        ENDIF
      END SELECT
      parse_xyz_string = .TRUE.
  90  RETURN

      END FUNCTION parse_xyz_string

! Convert a fraction string (digits/digits) into a real value; 
! value is set to the first number if no '/' found. 
! Return false on any error, and value is undefined.
      LOGICAL FUNCTION fraction_to_real(string, value)

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: string
      REAL,          INTENT (  OUT) :: value

      INTEGER p
      REAL dv

      fraction_to_real = .false.
      READ(string, *, ERR=90, END=90) value
      p = SCAN(string, '/')
      IF ( p .GT. 0 ) THEN
        READ(string(p+1:), *, ERR=90, END=90) dv
        IF ( 1.0+dv .EQ. 1.0 ) GOTO 90 ! test dv=0.0
        value =  value / dv
      ENDIF
      fraction_to_real = .true.
 90   RETURN

      END FUNCTION fraction_to_real

      END FUNCTION WriteRIETANPhaseInfo
!
!*****************************************************************************
!
      INTEGER FUNCTION CopyRIETANRestraints (TheFileName)

      USE VARIABLES
      USE TAVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileName

      CHARACTER*(*), PARAMETER :: ctRMark = ' #@DASH@MARK@R '
      INTEGER, PARAMETER :: ciRMarkLen = LEN(ctRMark), chFileFfe = 60, &
                            chFileInsR = 116, chFileInsW = 117, chFilePha = 118
      CHARACTER(MaxPathLength) :: tDirName, tFileName, FileNameBase
      CHARACTER*8 tExtension
      CHARACTER*120 tLine
      CHARACTER*1 w
      LOGICAL exists
      INTEGER ExtLength, iBaseLen, a1, a2, a3, n, IErrCode
      REAL value, std

      ! Initialise to failure
      CopyRIETANRestraints = 1

! width of atom sequence, different between 2000 and fp
      w = '2'
      IF ( Rietan_FP ) w = '3'

      INQUIRE(FILE=TheFileName, EXIST=exists)
      IF ( .NOT. exists ) GOTO 996
      ExtLength = len(tExtension)
      CALL SplitPath2(TheFileName, tDirName, tFileName, tExtension, ExtLength)
      FileNameBase = TRIM(tDirName)//TRIM(tFileName)
      iBaseLen = LEN_TRIM(FileNameBase)
      INQUIRE(FILE=FileNameBase(:iBaseLen)//'.ffe', EXIST=exists)
      IF ( .NOT. exists ) GOTO 998
! Wind ffe file
      OPEN(UNIT=chFileFfe, FILE=FileNameBase(:iBaseLen)//'.ffe', STATUS='unknown', ERR=998)
      DO WHILE ( .TRUE. )
        READ(chFileFfe, '(A)', ERR=998, END=300) tLine
        IF ( tLine(1:34) .EQ. ' INTERATOMIC DISTANCE IN ANGSTROMS') EXIT
      END DO
 300  CONTINUE
! Copy ins up to restraint record
      OPEN(UNIT=chFileInsR, FILE=TheFileName, STATUS='old', ERR=996)
      OPEN(UNIT=chFileInsW, FILE=TRIM(TheFileName)//'.tmp', &
           STATUS='unknown', ERR=996)
      DO WHILE ( .TRUE. )
        READ(chFileInsR, '(A)', Err=996, END=302) tLine
        WRITE(chFileInsW, '(A)', Err=996) TRIM(tLine)
        IF ( tLine(:ciRMarkLen) .EQ. ctRMark ) THEN
          DO WHILE ( .TRUE. )
            READ(chFileInsR, '(A)', Err=996, END=301) tLine
            IF ( tLine(:ciRMarkLen+3) .EQ. ctRMark//'END' ) THEN
              BACKSPACE(chFileInsR)
              GOTO 20
            ENDIF
          END DO
 301      CONTINUE
          EXIT
        ENDIF
      END DO
 302  CONTINUE
      GOTO 996
! Copy distance and angle restraints from pha to ins and ffe files
  20  OPEN(UNIT=chFilePha, FILE=FileNameBase(:iBaseLen)//'.pha', STATUS='old', ERR=997)
      n = 0
      DO WHILE ( .TRUE. )
        READ(chFilePha, '(A)', Err=997, END=304) tLine
        IF ( tLine(1:4) .NE. 'DIST') CYCLE
        READ(tLine(5:), *, Err=997) a1, a2, value, std
        n = n + 1
        WRITE(chFileFfe,'(I6,4X,2(A,I'//w//'),A/)', ERR=998) n,' (', a1,',    0) (',a2,',    0)'
        WRITE(chFileInsW,'(I6,2(1X,F8.4))', Err=996) n, value, std
      END DO
 304  CONTINUE
      WRITE(chFileFfe,'(//A)') ' BOND ANGLE IN DEGREES.  CENTRAL ATOM IS VERTEX'
      REWIND(chFilePha, Err=997)
      DO WHILE ( .TRUE. )
        READ(chFilePha, '(A)', Err=997, END=306) tLine
        IF ( tLine(1:4) .NE. 'ANGL') CYCLE
        READ(tLine(5:), *, Err=997) a1, a2, a3, value, std
        n = n + 1
        WRITE(chFileFfe,'(I6,4X,3(A,I'//w//'),A/)', ERR=998) n,' (', a1,',    0) (',a2, &
                         ',    0) (', a3,',    0)'
        WRITE(chFileInsW,'(I6,2(1X,F8.2))', Err=996) n, value, std
      END DO
 306  CONTINUE
      CLOSE(chFileFfe)
      CLOSE(chFilePha)
! Copy rest of ins
      DO WHILE ( .TRUE. )
        READ(chFileInsR, '(A)', Err=996, END=308) tLine
        WRITE(chFileInsW, '(A)', Err=996) TRIM(tLine)
      END DO
 308  CONTINUE
      CLOSE(chFileInsW)
      CLOSE(chFileInsR, STATUS='delete')
      IErrCode = InfoError(1)
      CALL IOsRenameFile(TRIM(TheFileName)//'.tmp', TRIM(TheFileName))
      IF ( InfoError(1) .NE. 0 ) GOTO 996
      CopyRIETANRestraints = 0

  999 CLOSE(chFileFfe)
      CLOSE(chFilePha)
      CLOSE(chFileInsW)
      CLOSE(chFileInsR)
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
        WRITE(hFile, '(I3,1X$)', ERR=999) iAtoms(I)
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
