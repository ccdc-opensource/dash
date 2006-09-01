!
!*****************************************************************************
!
      SUBROUTINE BatchMode(ArgString)

      USE WINTERACTER
      USE ZMVAR
      USE PRJVAR
      USE PO_VAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: ArgString

      INCLUDE 'PARAMS.INC'

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      REAL            T0, RT
      COMMON /saparl/ T0, RT

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      REAL            X_init,       x_unique,       lb,       ub
      COMMON /values/ X_init(MVAR), x_unique(MVAR), lb(MVAR), ub(MVAR)

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                                          iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(mvar), RowNumber, iRadio, iX, iUB, iLB

      CHARACTER*255 line, keyword, tString
      INTEGER iTem, hFile, nl, I, iLen, iFrg, iDummy
      REAL    rTem
      REAL    MaxMoves1
      INTEGER MaxMoves2

      in_batch = .TRUE.
      hFile = 63
      iFrg = 0
      OPEN (UNIT=hFile, FILE=ArgString, STATUS='OLD', ERR=999)
! Loop over all records
      DO WHILE ( .TRUE. )
 10     READ(hFile, '(A)', END=100, ERR=999) line
        nl = LEN_TRIM(line)
        IF ( nl .EQ. 0 ) GOTO 10 ! Blank line
        IF ( line(1:1) .EQ. "#" ) GOTO 10 ! It's a comment
        CALL INextString(line, keyword)
        CALL IUpperCase(keyword)
        SELECT CASE(keyword(1:LEN_TRIM(keyword)))
          CASE ('SDI') ! The .sdi file, containing wavelength, pattern, space group etc. etc.
            I = InfoError(1) ! reset the errors
            IF (InfoError(1) .NE. 0) GOTO 999
            ! Problem here: should be function, we don't have a way of telling if this call succeeded
            CALL SDIFileLoad(line(:nl))
          CASE ('OUT') ! The output file, must have extension .dash
            I = InfoError(1) ! reset the errors
            IF (InfoError(1) .NE. 0) GOTO 999
            PrjFileName = line(:nl)
          CASE ('T0') ! T0, the starting temperature for the SA
            I = InfoError(1) ! reset the errors
            CALL INextReal(line, rTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            T0 = rTem
          CASE ('COOL') ! The cooling rate for the SA
            I = InfoError(1) ! reset the errors
            CALL INextReal(line, rTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            RT = rTem
          CASE ('N1') ! N1/NS in the SA
            I = InfoError(1) ! reset the errors
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            NS = iTem
          CASE ('N2') ! N2/NT in the SA
            I = InfoError(1) ! reset the errors
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            NT = iTem
          CASE ('SEED1') ! Random generator seed 1
            I = InfoError(1) ! reset the errors
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            iseed1 = iTem
          CASE ('SEED2') ! Random generator seed 2
            I = InfoError(1) ! reset the errors
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            iseed2 = iTem
          CASE ('NRUNS') ! Termination criterion: number of SA runs
            I = InfoError(1) ! reset the errors
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            MaxRuns = iTem
          CASE ('MAXMOVES') ! Maximum number of moves, stored as e.g. 3.0 7 meaning 3.0E7
            I = InfoError(1) ! reset the errors
            CALL INextReal(line, MaxMoves1)
            IF (InfoError(1) .NE. 0) &
              GOTO 999
            I = InfoError(1) ! reset the errors
            CALL INextInteger(line, MaxMoves2)
            IF (InfoError(1) .NE. 0) &
              GOTO 999
            CALL RealInt2NMoves(MaxMoves1, MaxMoves2, MaxMoves)
          CASE ('MULTIPLIER') ! Termination criterion: profile chi-sqrd multiplier
            I = InfoError(1) ! reset the errors
            CALL INextReal(line, rTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            ChiMult = rTem
          CASE ('USECCOM') ! Use crystallographic centre of mass
            I = InfoError(1) ! reset the errors
            CALL INextString(line, tString)
            IF (InfoError(1) .NE. 0) GOTO 999
            UseCCoM = (tString(1:iLen) .EQ. "TRUE")
          CASE ('AUTOALIGN') ! Auto align
            I = InfoError(1) ! reset the errors
            CALL INextString(line, tString)
            IF (InfoError(1) .NE. 0) GOTO 999
            LAlign = (tString(1:iLen) .EQ. "TRUE")
          CASE ('HYDROGEN') ! Hydrogen treatment
            I = InfoError(1) ! reset the errors
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            CALL Set_HydrogenTreatment(iTem)
          CASE ('AUTOMIN') ! Auto local minimise
            I = InfoError(1) ! reset the errors
            CALL INextString(line, tString)
            IF (InfoError(1) .NE. 0) GOTO 999
            AutoMinimise = (tString(1:iLen) .EQ. "TRUE")
          CASE ('MINIMISEHYDR') ! Use Hydrogens for auto local minimise
            I = InfoError(1) ! reset the errors
            CALL INextString(line, tString)
            IF (InfoError(1) .NE. 0) GOTO 999
            UseHAutoMin = (tString(1:iLen) .EQ. "TRUE")
          CASE ('RANDOMISE') ! Randomise initial SA-parameter values
            I = InfoError(1) ! reset the errors
            CALL INextString(line, tString)
            IF (InfoError(1) .NE. 0) GOTO 999
            iLen = LEN_TRIM(tString)
            RandomInitVal = (tString(1:iLen) .EQ. "TRUE")
          CASE ('ZMATRIX')
            I = InfoError(1) ! reset the errors
            iFrg = iFrg + 1
            frag_file(iFrg) = line(:nl)
            CALL Read_One_Zm(iFrg)
          CASE ('PO_DIR')
            I = InfoError(1) ! reset the errors
            CALL INextInteger(line, PO_Direction(1))
            IF (InfoError(1) .NE. 0) GOTO 999
            CALL INextInteger(line, PO_Direction(2))
            IF (InfoError(1) .NE. 0) GOTO 999
            CALL INextInteger(line, PO_Direction(3))
            IF (InfoError(1) .NE. 0) GOTO 999
            PrefParExists = .TRUE.
          CASE ('LIMITS')
            nFrag = iFrg
            CALL SA_Parameter_Set
            I = InfoError(1) ! reset the errors
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            DO I = 1, iTem
              READ(hFile, '(A)', END=100, ERR=999) line
              nl = LEN_TRIM(line)  ! Since we only read iTem lines, this would crash
              IF ( nl .NE. 0 ) THEN ! Blank line
                IF ( line(1:1) .NE. "#" ) THEN ! It's a comment
                  CALL IUpperCase(line(:nl))
                  iDummy = InfoError(1) ! reset the errors
                  CALL INextReal(line, X_init(i)) ! This is the initial value
                  IF ( InfoError(1) .NE. 0 ) GOTO 999
                  CALL INextString(line, tString)
                  SELECT CASE(tString(1:LEN_TRIM(tString)))
                    CASE ('LBUB') ! Lower Bound/Upper Bound pair
                      CALL INextReal(line, LB(i)) ! Lower bound
                      IF ( InfoError(1) .NE. 0 ) GOTO 999
                      CALL INextReal(line, UB(i)) ! Upper bound
                      IF ( InfoError(1) .NE. 0 ) GOTO 999
                    CASE ('FIXED') ! Fixed
                    CASE ('BIMODAL') ! Bimodal
                      ModalFlag(i) = 2
                      CALL INextReal(line, LB(i)) ! Lower bound
                      IF ( InfoError(1) .NE. 0 ) GOTO 999
                      CALL INextReal(line, UB(i)) ! Upper bound
                      IF ( InfoError(1) .NE. 0 ) GOTO 999
                    CASE ('TRIMODAL') ! Trimodal
                      ModalFlag(i) = 3
                      CALL INextReal(line, LB(i)) ! Lower bound
                      IF ( InfoError(1) .NE. 0 ) GOTO 999
                      CALL INextReal(line, UB(i)) ! Upper bound
                      IF ( InfoError(1) .NE. 0 ) GOTO 999
                    CASE DEFAULT
                      GOTO 999 ! Error
                  END SELECT
           !       CALL ParseRawInput(I)
                ENDIF
              ENDIF
            ENDDO
        END SELECT
      ENDDO       
  100 CONTINUE 
      CLOSE(hFile)
      CALL BeginSA
      ! Exit DASH, but before doing so, save output to a file.
      ! it is probably best to save the output to a file that is specified in
      ! the .duff file, because that makes it easier for the user to later relate
      ! the output back to the input.
      CALL PrjReadWrite(PrjFileName, cWrite)
      CALL DoExit
  999 CONTINUE
      CLOSE(hFile)
      CALL DoExit

      END SUBROUTINE BatchMode
!
!*****************************************************************************
!
      SUBROUTINE DownLoadSAOPT
      ! Fills COMMON block /SAOPT/ with values from GUI.

      USE WINTERACTER
      USE DRUID_HEADER

      IMPLICIT NONE   

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      LOGICAL, EXTERNAL :: WDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL WDialogSelect(IDD_SA_input4)
      CALL WDialogGetRadioButton(IDR_HydrogensIgnore, HydrogenTreatment)
      UseCCoM = WDialogGetCheckBoxLogical(IDF_CrystallographicCoM)
      AutoMinimise = WDialogGetCheckBoxLogical(IDF_AutoLocalOptimise)
      UseHAutoMin = WDialogGetCheckBoxLogical(IDF_UseHydrogensAuto)
      LAlign = WDialogGetCheckBoxLogical(IDF_Align)
      CALL PopActiveWindowID

      END SUBROUTINE DownLoadSAOPT
!
!*****************************************************************************
!
      INTEGER FUNCTION BatchFileSaveAs
!
! RETURNS 0 for success
!
      USE WINTERACTER
      USE VARIABLES

      IMPLICIT NONE      

      INTEGER                  OFBN_Len
      CHARACTER(MaxPathLength)           OutputFilesBaseName
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OFBN_Len, OutputFilesBaseName, SA_RunNumberStr

      CHARACTER(MaxPathLength) :: tFileName
      CHARACTER(LEN=45) :: FILTER
      INTEGER iFLAGS
      
! Save the batch file
      BatchFileSaveAs = 1 ! Failure
      iFLAGS = SaveDialog + AppendExt + PromptOn
      FILTER = 'DASH batch files (*.duff)|*.duff|'
      tFileName = OutputFilesBaseName(1:OFBN_Len)//'.duff'
      CALL WSelectFile(FILTER,iFLAGS,tFileName,'Save project file')
      IF ((WinfoDialog(4) .EQ. CommonOK) .AND. (LEN_TRIM(tFileName) .NE. 0)) THEN
        CALL WriteBatchFile(tFileName, .FALSE.)
        BatchFileSaveAs = 0
      ENDIF

      END FUNCTION BatchFileSaveAs
!
!*****************************************************************************
!
      SUBROUTINE WriteBatchFile(FileName, TruncateSDIFileName)

      USE VARIABLES
      USE PO_VAR
      USE ZMVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: FileName
      LOGICAL,       INTENT (IN   ) :: TruncateSDIFileName ! Quick hack

      INCLUDE 'PARAMS.INC'

      INTEGER                  OFBN_Len
      CHARACTER(MaxPathLength)           OutputFilesBaseName
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OFBN_Len, OutputFilesBaseName, SA_RunNumberStr

      INTEGER         nvar, ns, nt, iSeed1, iSeed2
      COMMON /sapars/ nvar, ns, nt, iSeed1, iSeed2

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      REAL            T0, RT
      COMMON /saparl/ T0, RT

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign
      INTEGER                                                                    HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, LAlign, HydrogenTreatment

      REAL            X_init,       x_unique,       lb,       ub
      COMMON /values/ X_init(MVAR), x_unique(MVAR), lb(MVAR), ub(MVAR)

      INTEGER                ModalFlag,       RowNumber, iRadio
      REAL                                                                          iX, iUB, iLB  
      COMMON /ModalTorsions/ ModalFlag(mvar), RowNumber, iRadio, iX, iUB, iLB

      INTEGER iHandle, i, iFrg
      REAL    tReal
      INTEGER tInt
      INTEGER ExtLength
      CHARACTER*255 tDirName, tFileName, tExtension

      CALL DownLoadSAOPT
      iHandle = 10
      OPEN(UNIT=iHandle, FILE=FileName, ERR=999)
      WRITE(iHandle,'(A)',ERR=999) '# This is a generated file.'
      WRITE(iHandle,'(A)',ERR=999) '# Editing OUT, SEED1, SEED2 and NRUNS is safe and may be necessary,'
      WRITE(iHandle,'(A)',ERR=999) '# editing the items near the end of the file is bound to crash your DASH run.'
      WRITE(iHandle,'(A)',ERR=999) '# Input file'
      IF ( TruncateSDIFileName ) THEN
        WRITE(iHandle,'(A)',ERR=999) 'SDI '//OutputFilesBaseName(1:OFBN_Len-4)//'.sdi'
      ELSE
        WRITE(iHandle,'(A)',ERR=999) 'SDI '//OutputFilesBaseName(1:OFBN_Len)//'.sdi'
      ENDIF
      WRITE(iHandle,'(A)',ERR=999) '# Ouput file'
      WRITE(iHandle,'(A)',ERR=999) 'OUT '//OutputFilesBaseName(1:OFBN_Len)//'.dash'
      WRITE(iHandle,'(A)',ERR=999) '# When starting multiple jobs with the same .sdi file, make sure that'
      WRITE(iHandle,'(A)',ERR=999) '# SEED1 and SEED2 have different values in every file.'
      WRITE(iHandle,'(A)',ERR=999) '# DASH increments SEED1 and SEED2 with the number of the run at the start of'
      WRITE(iHandle,'(A)',ERR=999) '# each run.'
      WRITE(iHandle,'(A,X,I6)',ERR=999) 'SEED1', iSeed1
      WRITE(iHandle,'(A,X,I6)',ERR=999) 'SEED2', iSeed2
      WRITE(iHandle,'(A)',ERR=999) '# On a distributed system, NRUNS should probably be set to:'
      WRITE(iHandle,'(A)',ERR=999) '#     (total number of runs / number of processors)'
      WRITE(iHandle,'(A,X,I3)',ERR=999) 'NRUNS', MaxRuns
      WRITE(iHandle,'(A)',ERR=999) '# Maximum number of moves: real followed by integer e.g. "3.0 7" means "3.0E7"'
      CALL NMoves2RealInt(MaxMoves, tReal, tInt)
      WRITE(iHandle,'(A,X,F9.5,X,I2)',ERR=999) 'MAXMOVES', tReal, tInt
      WRITE(iHandle,'(A)',ERR=999) '# Termination criterion: a Simulated Annealing run stops when '
      WRITE(iHandle,'(A)',ERR=999) '# the profile chi-sqrd is lower than MULTIPLIER times the Pawley chi-sqrd'
      WRITE(iHandle,'(A,X,F9.5)',ERR=999) 'MULTIPLIER', ChiMult
      WRITE(iHandle,'(A)',ERR=999) '# Changing settings below this point is probably not necessary'
      WRITE(iHandle,'(A,X,F9.5)',ERR=999) 'T0', T0
      WRITE(iHandle,'(A,X,F9.5)',ERR=999) 'COOL', RT
      WRITE(iHandle,'(A,X,I3)',ERR=999) 'N1', NS
      WRITE(iHandle,'(A,X,I3)',ERR=999) 'N2', NT
      WRITE(iHandle,'(A)',ERR=999) '# Randomise initial values'
      IF ( RandomInitVal ) THEN
        WRITE(iHandle,'(A)',ERR=999) 'RANDOMISE TRUE'
      ELSE
        WRITE(iHandle,'(A)',ERR=999) 'RANDOMISE FALSE'
      ENDIF
      WRITE(iHandle,'(A)',ERR=999) '# Use crystallographic Centre of Mass'
      IF ( UseCCoM ) THEN
        WRITE(iHandle,'(A)',ERR=999) 'USECCOM TRUE'
      ELSE
        WRITE(iHandle,'(A)',ERR=999) 'USECCOM FALSE'
      ENDIF
      WRITE(iHandle,'(A)',ERR=999) '# 1 = ignore, 2 = absorb, 3 = explicit'
      WRITE(iHandle,'(A,X,I2)',ERR=999) 'HYDROGEN', HydrogenTreatment
      WRITE(iHandle,'(A)',ERR=999) '# Auto local minimise'
      IF ( AutoMinimise ) THEN
        WRITE(iHandle,'(A)',ERR=999) 'AUTOMIN TRUE'
      ELSE
        WRITE(iHandle,'(A)',ERR=999) 'AUTOMIN FALSE'
      ENDIF
      WRITE(iHandle,'(A)',ERR=999) '# Use Hydrogens for auto local minimise'
      IF ( UseHAutoMin ) THEN
        WRITE(iHandle,'(A)',ERR=999) 'MINIMISEHYDR TRUE'
      ELSE
        WRITE(iHandle,'(A)',ERR=999) 'MINIMISEHYDR FALSE'
      ENDIF
      WRITE(iHandle,'(A)',ERR=999) '# Auto align'
      IF ( LAlign ) THEN
        WRITE(iHandle,'(A)',ERR=999) 'AUTOALIGN TRUE'
      ELSE
        WRITE(iHandle,'(A)',ERR=999) 'AUTOALIGN FALSE'
      ENDIF
      WRITE(iHandle,'(A)',ERR=999) '# WARNING beyond this point the number and order of the lines becomes important.'
      WRITE(iHandle,'(A)',ERR=999) '# Do not edit beyond this point.'
      IF ( PrefParExists ) THEN
        WRITE(iHandle,'(A)',ERR=999) '#Preferred Orientation DIRection'
        WRITE(iHandle,'(A,3(X,I3))',ERR=999) '#PO_DIR', (PO_Direction(i),i=1,3)
      ENDIF
      DO iFrg = 1, nfrag
        IF ( TruncateSDIFileName ) THEN
          ExtLength = 7
          CALL SplitPath2(frag_file(iFrg)(1:LEN_TRIM(frag_file(iFrg))), tDirName, tFileName, tExtension, ExtLength)
          WRITE(iHandle,'(A)',ERR=999) "ZMATRIX "//tFileName(1:LEN_TRIM(tFileName))//'.zmatrix'
        ELSE
          WRITE(iHandle,'(A)',ERR=999) "ZMATRIX "//frag_file(iFrg)(1:LEN_TRIM(frag_file(iFrg)))
        ENDIF
      ENDDO
!C Need limits for all parameters. Since these are by index, we need to write all of them out
      WRITE(iHandle,'(A,X,I3)',ERR=999) 'LIMITS', nvar
      DO i = 1, nvar
        SELECT CASE ( ModalFlag(i) )
          CASE ( 0, 1 ) ! Not a torsion/ a unimodal torsion
            WRITE(iHandle,'(F10.5,X,A,X,F10.5,X,F10.5)',ERR=999) X_init(i), 'LBUB', LB(i), UB(i)
          CASE ( 2 ) ! Bimodal torsion
            WRITE(iHandle,'(F10.5,X,A,X,F10.5,X,F10.5)',ERR=999) X_init(i), 'BIMODAL', LB(i), UB(i)
          CASE ( 3 ) ! Trimodal torsion
            WRITE(iHandle,'(F10.5,X,A,X,F10.5,X,F10.5)',ERR=999) X_init(i), 'TRIMODAL', LB(i), UB(i)
        END SELECT
      ENDDO
!C  0.0000 LBUB 0.000 1.000
!C  0.0000 FIXED
!C  0.0000 BIMODAL 120.000 150.000
!C  0.0000 TRIMODAL -30.000 30.000

      CLOSE(iHandle) 
      RETURN
 999  CALL ErrorMessage('Error writing .duff file.')
      CLOSE(iHandle) 

      END SUBROUTINE WriteBatchFile
!
!*****************************************************************************
!
      SUBROUTINE MergeDASHFiles(DirName, OutputFileName)

! Note: at the end of the day, we will have to be able to read everything into DASH. DASH is
! limited to 999 (= MaxRun) solutions, so this routine will also always be limited to 999 solutions at a time.

      USE WINTERACTER
      USE PRJVAR
      USE SOLVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: DirName
      CHARACTER*(*), INTENT (IN   ) :: OutputFileName

      INCLUDE 'PARAMS.INC'
      INCLUDE 'GLBVAR.INC'

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      CHARACTER*255    dash_files(1:MaxRun2), tDirName, FileName, tExtension
      REAL All_BestValuesDoF(1:100,1:MaxRun2)  ! mvar, MaxRun
      REAL All_ProfileChiSqd(1:MaxRun2)
      REAL All_IntensityChiSqd(1:MaxRun2)
      INTEGER tot_nruns, number_of_dash_files, I, iRun, iDoF, iLen
      INTEGER nvar_expected, ExtLength

      in_batch = .TRUE.
      number_of_dash_files = MaxRun2
      CALL IOsDirInfo(DirName, "*.dash", dash_files, number_of_dash_files)
      tDirName = DirName
      iLen = LEN_TRIM(tDirName)
      IF ( tDirName(iLen:iLen) .NE. "\" ) THEN
        tDirName = tDirName(1:iLen)//"\"
        iLen = iLen + 1
      ENDIF
      tot_nruns = 0
      DO I = 1, number_of_dash_files ! Loop over .dash files
        CALL PrjFileLoad(tDirName(1:iLen)//dash_files(I))
        !C Check that the number of variables is consistent in all .dash files
        IF ( I .EQ. 1 ) THEN
          nvar_expected = nvar
        ELSE
          IF ( nvar .NE. nvar_expected ) STOP
        ENDIF
        !C BestValuesDoF(1:100, 1:nruns) now contains the variables for this .dash file:
        !C store in All_BestValuesDoF
        DO iRun = 1, NumOf_SA_Runs
          tot_nruns = tot_nruns + 1
          IF ( tot_nruns .GT. MaxRun ) &
            tot_nruns = MaxRun
          All_ProfileChiSqd(tot_nruns)   = ProfileChiSqd(iRun)
          All_IntensityChiSqd(tot_nruns) = IntensityChiSqd(iRun)
          DO iDoF = 1, nvar
            All_BestValuesDoF(iDoF, tot_nruns) = BestValuesDoF(iDoF, iRun)
          ENDDO
        ENDDO
      ENDDO
      NumOf_SA_Runs = tot_nruns
      DO iRun = 1, NumOf_SA_Runs
        ProfileChiSqd(iRun)   = All_ProfileChiSqd(iRun)
        IntensityChiSqd(iRun) = All_IntensityChiSqd(iRun)
        DO iDoF = 1, nvar
          BestValuesDoF(iDoF, iRun) = All_BestValuesDoF(iDoF, iRun)
        ENDDO
      ENDDO
      CALL SORT_REAL(IntensityChiSqd, iSol2Run, NumOf_SA_Runs)
      !C Fabricate output file name from input files.
      iLen = LEN_TRIM(OutputFileName)
      IF ( iLen .GT. 0 ) THEN
        FileName = OutputFileName
        IF ( iLen .GT. 5 ) THEN ! Strip possible .dash extension
          tExtension = FileName(iLen-4:iLen)
          CALL StrUpperCase(tExtension)
          IF ( tExtension .EQ. ".DASH" ) &
            iLen = iLen - 5
        ENDIF
      ELSE
        ExtLength = 4
        CALL SplitPath2(dash_files(1), tDirName, FileName, tExtension, ExtLength)
        iLen = LEN_TRIM(FileName)
        !C Try to convert "Example1.dash" or "Example01.dash" to "Example.dash"
        IF ( IntValueOfChar(FileName(iLen:iLen)) .GE. 0 ) THEN
          iLen = iLen - 1
          IF ( IntValueOfChar(FileName(iLen:iLen)) .GE. 0 ) &
            iLen = iLen - 1
        ELSE
          FileName = "output"
          iLen = LEN_TRIM(FileName)
        ENDIF
      ENDIF
      CALL PrjReadWrite(FileName(1:iLen)//".dash", cWrite)

      END SUBROUTINE MergeDASHFiles
!
!*****************************************************************************
!
