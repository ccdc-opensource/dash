!
!*****************************************************************************
!
      SUBROUTINE BatchMode(ArgString)

      USE WINTERACTER
      USE ZMVAR
      USE PRJVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: ArgString

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM
      INTEGER                                                            HydrogenTreatment
      COMMON /SAOPT/  AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, HydrogenTreatment

      REAL            T0, RT
      COMMON /saparl/ T0, RT

      INTEGER         nvar, ns, nt, iseed1, iseed2
      COMMON /sapars/ nvar, ns, nt, iseed1, iseed2

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      CHARACTER*255 line, keyword, tString
      INTEGER iTem, hFile, nl, I, iLen, iFrg, iDummy
      REAL    rTem
      REAL    MaxMoves1, tMaxMoves
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
            MaxMoves = NINT(tMaxMoves)
          CASE ('MULTIPLIER') ! Termination criterion: profile chi-sqrd multiplier
            I = InfoError(1) ! reset the errors
            CALL INextReal(line, rTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            ChiMult = rTem
          CASE ('USECCOM') ! Auto local minimise
            I = InfoError(1) ! reset the errors
            CALL INextString(line, tString)
            IF (InfoError(1) .NE. 0) GOTO 999
            UseCCoM = (tString(1:iLen) .EQ. "TRUE")
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
          CASE ('RANDOMISE') 
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
          CASE ('LIMITS')
            nFrag = iFrg
            CALL SA_Parameter_Set
            I = InfoError(1) ! reset the errors
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            DO I = 1, iTem
              READ(hFile, '(A)', END=100, ERR=999) line
              nl = LEN_TRIM(line)
              IF ( nl .NE. 0 ) THEN ! Blank line
                IF ( line(1:1) .NE. "#" ) THEN ! It's a comment
                  CALL IUpperCase(line(:nl))
                  iDummy = InfoError(1) ! reset the errors
                  CALL INextReal(line, rTem) ! This is the initial value
                  !...
                  CALL INextString(line, tString)
                  SELECT CASE(tString(1:LEN_TRIM(tString)))
                    CASE ('LBUB') ! Lower Bound/Upper Bound pair
                      CALL INextReal(line, rTem) ! Lower bound
                      !...
                      CALL INextReal(line, rTem) ! Upper bound
                      !...
                    CASE ('FIXED') ! Fixed
                    CASE ('BIMODAL') ! Bimodal
                    CASE ('TRIMODAL') ! Trimodal
                    CASE DEFAULT
                      GOTO 999 ! Error
                  END SELECT
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
      SUBROUTINE WriteBatchFile

      IMPLICIT NONE

      END SUBROUTINE WriteBatchFile
!
!*****************************************************************************
!
      SUBROUTINE MergeDASHFiles(DirName, OutputFileName)

! Note: at the end of the day, we will have to be able to read everything into DASH. DASH is
! limited to 99 (= MaxRun) solutions, so this routine will also always be limited to 99 solutions at a time.

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

      CHARACTER*255    dash_files(1:MaxRun), tDirName, FileName, tExtension
      REAL All_BestValuesDoF(1:100,1:MaxRun)  ! mvar, MaxRun
      REAL All_ProfileChiSqd(1:MaxRun)
      REAL All_IntensityChiSqd(1:MaxRun)
      INTEGER tot_nruns, number_of_dash_files, I, iRun, iDoF, iLen
      INTEGER nvar_expected, ExtLength

      in_batch = .TRUE.
      number_of_dash_files = MaxRun
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
