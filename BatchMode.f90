!
!*****************************************************************************
!
      SUBROUTINE BatchMode(ArgString)

      USE WINTERACTER
      USE DRUID_HEADER
      USE ZMVAR
      USE PRJVAR

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: ArgString

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      ! COMMON block for settings when in batch mode that are usually done via the GUI

      LOGICAL         AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM
      INTEGER                                                            HydrogenTreatment
      COMMON /BATSET/ AutoMinimise, UseHAutoMin, RandomInitVal, UseCCoM, HydrogenTreatment

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
            CALL WDialogGetInteger(IDF_MaxMoves2, MaxMoves2)
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
            HydrogenTreatment = iTem
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
      SUBROUTINE CreateBatchFile

      IMPLICIT NONE

      END SUBROUTINE CreateBatchFile
!
!*****************************************************************************
!
