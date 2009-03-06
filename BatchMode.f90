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

      INCLUDE 'params.inc'
      INCLUDE 'SA_restrain.inc'

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

      LOGICAL, EXTERNAL :: SDIFileLoad, ParseDistribution, ParseSADistRestraint
      INTEGER, EXTERNAL :: Read_One_zm

      INTEGER, PARAMETER :: hFile = 63
      CHARACTER*255 line, keyword, tString
      INTEGER iTem, I, iLen, iFrg, iDummy, iP
      REAL    rTem
      REAL    MaxMoves1
      INTEGER MaxMoves2
      LOGICAL tSdiIn, tZmIn

!     in_batch = .TRUE.
      iFrg = 0
      tSdiIn = .FALSE.
      tZmIn  = .FALSE.
      CALL InitSADistRestraint
      OPEN (UNIT=hFile, FILE=ArgString, STATUS='OLD', ERR=998)
! Loop over all records
      DO WHILE ( .TRUE. )
        READ(hFile, '(A)', END=100, ERR=998) line
        iP = VERIFY(line, ' '//CHAR(9)) ! Locate first non space/tab char
        IF ( iP .EQ. 0 ) CYCLE ! White spaces only
        line = line(iP:) ! Skip leading white spaces
        IF ( line(1:1) .EQ. "#" ) CYCLE ! It's a comment
        iDummy = InfoError(1) ! reset the errors
        CALL INextString(line, keyword)
        CALL IUpperCase(keyword)
        IF (InfoError(1) .NE. 0) GOTO 999
        iP = VERIFY(line, ' '//CHAR(9)) ! Locate first non space/tab char
        IF ( iP .EQ. 0 ) GOTO 999 ! All keywords MUST followed by at least one value
        line = line(iP:) ! Skip leading white spaces
        SELECT CASE(keyword)
          CASE ('SDI') ! The .sdi file, containing wavelength, pattern, space group etc. etc.
            IF ( .NOT. SDIFileLoad(line)) GOTO 999
            tSdiIn = .TRUE.
          CASE ('OUT') ! The output file, must have extension .dash
            PrjFileName = line
          CASE ('SAVE')
            CALL INextString(line, tString)
            IF (InfoError(1) .NE. 0) GOTO 999

            iLen = LEN_TRIM(tString)
            IF      (tString(1:iLen) .EQ. "TBL")  THEN
              CALL Set_SaveParamAtEnd(.TRUE.)
            ELSE IF (tString(1:iLen) .EQ. "CHI")  THEN
              CALL Set_OutputChi2vsMoves(.TRUE.)
            ELSE IF (tString(1:iLen) .EQ. "CIF")  THEN
              CALL Set_SaveCIF(.TRUE.)
            ELSE IF (tString(1:iLen) .EQ. "PDB")  THEN
              CALL Set_SavePDB(.TRUE.)
            ELSE IF (tString(1:iLen) .EQ. "PRO")  THEN
              CALL Set_SavePRO(.TRUE.)
            ELSE IF (tString(1:iLen) .EQ. "CCL")  THEN
              CALL Set_SaveCCL(.TRUE.)
            ELSE IF (tString(1:iLen) .EQ. "CSSR") THEN
              CALL Set_SaveCSSR(.TRUE.)
            ELSE IF (tString(1:iLen) .EQ. "RES")  THEN
              CALL Set_SaveRES(.TRUE.)
            ENDIF

          CASE ('T0') ! T0, the starting temperature for the SA
            CALL INextReal(line, rTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            T0 = rTem
          CASE ('COOL') ! The cooling rate for the SA
            CALL INextReal(line, rTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            RT = rTem
          CASE ('N1') ! N1/NS in the SA
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            NS = iTem
          CASE ('N2') ! N2/NT in the SA
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            NT = iTem
          CASE ('SEED1') ! Random generator seed 1
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            iseed1 = iTem
          CASE ('SEED2') ! Random generator seed 2
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            iseed2 = iTem
          CASE ('NRUNS') ! Termination criterion: number of SA runs
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            MaxRuns = iTem
          CASE ('MAXMOVES') ! Maximum number of moves, stored as e.g. 3.0 7 meaning 3.0E7
            CALL INextReal(line, MaxMoves1)
            IF (InfoError(1) .NE. 0) &
              GOTO 999
            CALL INextInteger(line, MaxMoves2)
            IF (InfoError(1) .NE. 0) &
              GOTO 999
            CALL RealInt2NMoves(MaxMoves1, MaxMoves2, MaxMoves)
          CASE ('MULTIPLIER') ! Termination criterion: profile chi-sqrd multiplier
            CALL INextReal(line, rTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            ChiMult = rTem
          CASE ('USECCOM') ! Use crystallographic centre of mass
            CALL INextString(line, tString)
            IF (InfoError(1) .NE. 0) GOTO 999
            UseCCoM = (TRIM(tString) .EQ. "TRUE")
          CASE ('AUTOALIGN') ! Auto align
            CALL INextString(line, tString)
            IF (InfoError(1) .NE. 0) GOTO 999
            LAlign = (TRIM(tString) .EQ. "TRUE")
          CASE ('HYDROGEN') ! Hydrogen treatment
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            CALL Set_HydrogenTreatment(iTem)
          CASE ('AUTOMIN') ! Auto local minimise
            CALL INextString(line, tString)
            IF (InfoError(1) .NE. 0) GOTO 999
            AutoMinimise = (TRIM(tString) .EQ. "TRUE")
          CASE ('MINIMISEHYDR') ! Use Hydrogens for auto local minimise
            CALL INextString(line, tString)
            IF (InfoError(1) .NE. 0) GOTO 999
            UseHAutoMin = (TRIM(tString) .EQ. "TRUE")
          CASE ('RANDOMISE') ! Randomise initial SA-parameter values
            CALL INextString(line, tString)
            IF (InfoError(1) .NE. 0) GOTO 999
            RandomInitVal = (TRIM(tString) .EQ. "TRUE")
          CASE ('ZMATRIX')
            iFrg = iFrg + 1
            frag_file(iFrg) = line
            iDummy = Read_One_Zm(iFrg)
            IF (iDummy .NE. 0) THEN
             CALL FileErrorPopup(frag_file(iFrg), iDummy)
             GOTO 999 ! reading failed
            ENDIF
            tZmIn = .TRUE.
          CASE ('DIST_RESTRAINT','BOND_RESTRAINT') ! This line must appear later than ZMATRIX in .dbf
            IF (.NOT. ParseSADistRestraint(line, iFrg)) GOTO 999
          CASE ('PO_DIR')
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
            I = 0
            DO WHILE (I .LT. iTem)
              READ(hFile, '(A)', END=100, ERR=998) line
              iP = VERIFY(line, ' '//CHAR(9)) ! Locate first non space/tab char
              IF ( iP .EQ. 0 ) CYCLE ! White spaces only
              line = line(iP:) ! Skip leading white spaces
              IF ( line(1:1) .EQ. "#" ) CYCLE ! It's a comment
              I = I + 1
              iDummy = InfoError(1) ! reset the errors
              CALL INextReal(line, X_init(i)) ! This is the initial value
              IF ( InfoError(1) .NE. 0 ) GOTO 999
              CALL INextString(line, tString)
              CALL IUpperCase(tString)
              IF ( InfoError(1) .NE. 0 ) GOTO 999
              SELECT CASE(TRIM(tString))
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
                CASE ('MDB','MOGU') ! Distribution
                  CALL INextReal(line, LB(i)) ! Lower bound
                  IF ( InfoError(1) .NE. 0 ) GOTO 999
                  CALL INextReal(line, UB(i)) ! Upper bound
                  IF ( InfoError(1) .NE. 0 ) GOTO 999
                  IF (.NOT. ParseDistribution(line, i)) GOTO 999
                  ModalFlag(i) = 4
                CASE DEFAULT
                  GOTO 999 ! Error
              END SELECT
              CALL ParseRawInput(I)
            ENDDO
          CASE DEFAULT
            CALL InfoMessage('Ignore the line begin with unknown keyword: '//TRIM(keyword))
        END SELECT
      ENDDO       
  100 CONTINUE 
      IF ( ( .NOT. tSdiIn ) .OR. ( .NOT. tZmIn ) ) GOTO 996
      CLOSE(hFile)
      CALL BeginSA
      ! Exit DASH, but before doing so, save output to a file.
      ! it is probably best to save the output to a file that is specified in
      ! the .dbf file, because that makes it easier for the user to later relate
      ! the output back to the input.
      CALL PrjReadWrite(PrjFileName, cWrite)
      GOTO 892

  996 CALL ErrorMessage('Failed to read SDI or ZMATRIX line from '//TRIM(ArgString))
      GOTO 990
  998 CALL ErrorMessage('Problem occurred while open or read '//TRIM(ArgString))
      GOTO 990
  999 CALL ErrorMessage('Problem occurred while processing '//TRIM(keyword)//' record(s)')
  990 CLOSE(hFile)
  892 IF (in_batch) CALL DoExit
      RETURN

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

      LOGICAL, EXTERNAL :: DASHWDialogGetCheckBoxLogical

      CALL PushActiveWindowID
      CALL SelectDASHDialog(IDD_SA_input4)
      CALL DASHWDialogGetRadioButton(IDR_HydrogensIgnore, HydrogenTreatment)
      UseCCoM = DASHWDialogGetCheckBoxLogical(IDF_CrystallographicCoM)
      AutoMinimise = DASHWDialogGetCheckBoxLogical(IDF_AutoLocalOptimise)
      UseHAutoMin = DASHWDialogGetCheckBoxLogical(IDF_UseHydrogensAuto)
      LAlign = DASHWDialogGetCheckBoxLogical(IDF_Align)
      CALL PopActiveWindowID

      END SUBROUTINE DownLoadSAOPT
!
!*****************************************************************************
!
      INTEGER FUNCTION BatchFileSaveAs()
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
      FILTER = 'DASH batch files (*.dbf)|*.dbf|'
      tFileName = OutputFilesBaseName(1:OFBN_Len)//'.dbf'
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

      INCLUDE 'SA_restrain.inc'

      CHARACTER*(*), INTENT (IN   ) :: FileName
      LOGICAL,       INTENT (IN   ) :: TruncateSDIFileName ! Quick hack

      INCLUDE 'params.inc'

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

      INTEGER NumMogulBins(MVAR), MogulBins(MaxMogulBin, MVAR)
      REAL MogulDistributions(-180:180, MVAR)
      COMMON /MDB/ NumMogulBins, MogulBins, MogulDistributions

      LOGICAL , EXTERNAL :: Get_SavePRO, SavePDB
      LOGICAL , EXTERNAL :: SaveCSSR, SaveCCL, SaveCIF, SaveRes
      LOGICAL , EXTERNAL :: Get_SaveParamAtEnd
      LOGICAL , EXTERNAL :: Get_OutputChi2vsMoves
      CHARACTER*20, EXTERNAL :: Integer2String

      INTEGER iHandle, i, j, iFrg, kk, fragID(2), atomSeq(2)
      REAL    tReal
      INTEGER tInt
      INTEGER ExtLength
      CHARACTER*255 tDirName, tFileName, tExtension
      CHARACTER*36 parlabel(mvar)

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
      WRITE(iHandle,'(A,1X,I6)',ERR=999) 'SEED1', iSeed1
      WRITE(iHandle,'(A,1X,I6)',ERR=999) 'SEED2', iSeed2

      WRITE(iHandle,'(A)',ERR=999) '# You can customize output by uncommenting some of the lines below'

      IF ( Get_SavePRO() ) THEN
        WRITE(iHandle,'(A)',ERR=999) 'SAVE PRO'
      ELSE
        WRITE(iHandle,'(A)',ERR=999) '# SAVE PRO'
      ENDIF

      IF ( SaveCIF() )  THEN
        WRITE(iHandle,'(A)',ERR=999) 'SAVE CIF'
      ELSE
        WRITE(iHandle,'(A)',ERR=999) '# SAVE CIF'
      ENDIF

      IF ( SaveRES() ) THEN
        WRITE(iHandle,'(A)',ERR=999) 'SAVE RES'
      ELSE
        WRITE(iHandle,'(A)',ERR=999) '# SAVE RES'
      ENDIF

      IF ( SaveCCL() ) THEN
        WRITE(iHandle,'(A)',ERR=999) 'SAVE CCL'
      ELSE
        WRITE(iHandle,'(A)',ERR=999) '# SAVE CCL'
      ENDIF
      
      IF ( SaveCSSR() ) THEN
        WRITE(iHandle,'(A)',ERR=999) 'SAVE CSSR'
      ELSE
        WRITE(iHandle,'(A)',ERR=999) '# SAVE CSSR'
      ENDIF

      IF ( SavePDB() ) THEN
        WRITE(iHandle,'(A)',ERR=999) 'SAVE PDB'
      ELSE
        WRITE(iHandle,'(A)',ERR=999) '# SAVE PDB'
      ENDIF

      IF ( Get_SaveParamAtEnd() ) THEN
        WRITE(iHandle,'(A)',ERR=999) 'SAVE TBL'
      ELSE
        WRITE(iHandle,'(A)',ERR=999) '# SAVE TBL'
      ENDIF

      IF ( Get_OutputChi2vsMoves() ) THEN    
        WRITE(iHandle,'(A)',ERR=999) 'SAVE CHI'
      ELSE
        WRITE(iHandle,'(A)',ERR=999) '# SAVE CHI'
      ENDIF
  
      WRITE(iHandle,'(A)',ERR=999) '# On a distributed system, NRUNS should probably be set to:'
      WRITE(iHandle,'(A)',ERR=999) '#     (total number of runs / number of processors)'
      WRITE(iHandle,'(A,1X,I3)',ERR=999) 'NRUNS', MaxRuns
      WRITE(iHandle,'(A)',ERR=999) '# Maximum number of moves: real followed by integer e.g. "3.0 7" means "3.0E7"'
      CALL NMoves2RealInt(MaxMoves, tReal, tInt)
      WRITE(iHandle,'(A,1X,F9.5,1X,I2)',ERR=999) 'MAXMOVES', tReal, tInt
      WRITE(iHandle,'(A)',ERR=999) '# Termination criterion: a Simulated Annealing run stops when '
      WRITE(iHandle,'(A)',ERR=999) '# the profile chi-sqrd is lower than MULTIPLIER times the Pawley chi-sqrd'
      WRITE(iHandle,'(A,1X,F9.5)',ERR=999) 'MULTIPLIER', ChiMult
      WRITE(iHandle,'(A)',ERR=999) '# Changing settings below this point is probably not necessary'
      WRITE(iHandle,'(A,1X,F9.5)',ERR=999) 'T0', T0
      WRITE(iHandle,'(A,1X,F9.5)',ERR=999) 'COOL', RT
      WRITE(iHandle,'(A,1X,I3)',ERR=999) 'N1', NS
      WRITE(iHandle,'(A,1X,I3)',ERR=999) 'N2', NT
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
      WRITE(iHandle,'(A,1X,I2)',ERR=999) 'HYDROGEN', HydrogenTreatment
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
        WRITE(iHandle,'(A,3(1X,I3))',ERR=999) 'PO_DIR', (PO_Direction(i),i=1,3)
      ENDIF
      kk = 0
      DO iFrg = 1, nfrag
        IF ( TruncateSDIFileName ) THEN
          ExtLength = 7
          CALL SplitPath2(TRIM(frag_file(iFrg)), tDirName, tFileName, tExtension, ExtLength)
          WRITE(iHandle,'(A)',ERR=999) "ZMATRIX "//TRIM(tFileName)//'.zmatrix'
        ELSE
          WRITE(iHandle,'(A)',ERR=999) "ZMATRIX "//TRIM(frag_file(iFrg))
        ENDIF
        DO i = 1, izmpar(iFrg)
          kk = kk + 1
          parlabel(kk) = czmpar(i, iFrg)
        ENDDO
      ENDDO
      IF (DRestrNumb .GT. 0) THEN
        WRITE(iHandle,'(A)',ERR=999) '# Frag1, Atom1, Frag2, Atom2, Distance, Width, Weight, Spring_flag(0=no,1=yes)'
        DO i = 1, DRestrNumb
          DO j = 1, 2
            CALL AtomID2Frag( DRestrAtomIDs(j,I), fragID(j), atomSeq(j) )
          ENDDO
          WRITE(iHandle,'(A,2(1X,I2,A))',ERR=999) '#', (fragID(j), ':'//OriginalLabel(atomSeq(j), fragID(j)), j=1, 2)
! Avoid using atom label: as "relabel" may change internal labels, but those in zmatrics remain unchanged
          WRITE(iHandle,'(A,1X,2(I2,1X,I3,1X),2(F8.4,1X),F8.2,1X,I1)',ERR=999) 'DIST_RESTRAINT', &
                  (fragID(j), atomSeq(j), j=1, 2), &
                  DRestrLens(I), DRestrWidths(I), DRestrWeights(I), DRestrSpringOpts(I)
        ENDDO
      ENDIF
!C Need limits for all parameters. Since these are by index, we need to write all of them out
      WRITE(iHandle,'(A,1X,I3)',ERR=999) 'LIMITS', nvar
      DO i = 1, nvar
        WRITE(iHandle,'(2A)',ERR=999) '# ', TRIM(parlabel(i))
        SELECT CASE ( ModalFlag(i) )
          CASE ( 0, 1 ) ! Not a torsion/ a unimodal torsion
            WRITE(iHandle,'(F10.5,1X,A,1X,F10.5,1X,F10.5)',ERR=999) X_init(i), 'LBUB', LB(i), UB(i)
          CASE ( 2 ) ! Bimodal torsion
            WRITE(iHandle,'(F10.5,1X,A,1X,F10.5,1X,F10.5)',ERR=999) X_init(i), 'BIMODAL', LB(i), UB(i)
          CASE ( 3 ) ! Trimodal torsion
            WRITE(iHandle,'(F10.5,1X,A,1X,F10.5,1X,F10.5)',ERR=999) X_init(i), 'TRIMODAL', LB(i), UB(i)
          CASE ( 4 ) ! Mogul distribution torsion
            WRITE(iHandle,'(F10.5,1X,A,1X,F10.5,1X,F10.5$)',ERR=999) X_init(i), 'MDB', LB(i), UB(i)
            WRITE(iHandle,'(1X,I2,200(1X,A))',ERR=999) NumMogulBins(i), &
                 (TRIM(Integer2String(MogulBins(j, i))), j=1,NumMogulBins(i))
        END SELECT
      ENDDO
!C  0.0000 LBUB 0.000 1.000
!C  0.0000 FIXED
!C  0.0000 BIMODAL 120.000 150.000
!C  0.0000 TRIMODAL -30.000 30.000

      CLOSE(iHandle) 
      RETURN
 999  CALL ErrorMessage('Error writing .dbf file.')
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

      INCLUDE 'params.inc'
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
