!
!*****************************************************************************
!
      SUBROUTINE BatchMode(ArgString)

      USE WINTERACTER
      USE ZMVAR
!      USE DRUID_HEADER

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: ArgString

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      CHARACTER*255 line, keyword, tString
      INTEGER iTem, hFile, nl, I, iLen, iFrg, iDummy
      REAL    rTem

      hFile = 63
      iFrg = 0
      OPEN (UNIT=hFile, FILE=ArgString, STATUS='OLD', ERR=999)
! Loop over all records
      DO WHILE ( .TRUE. )
 10     READ(hFile, '(A)', END=100, ERR=999) line
        nl = LEN_TRIM(line)
        IF ( nl .EQ. 0 ) GOTO 10 ! Blank line
        IF ( line(1:1) .EQ. "#" ) GOTO 10 ! It's a comment
        CALL IUpperCase(line(:nl))
        CALL INextString(line, keyword)
        SELECT CASE(keyword(1:LEN_TRIM(keyword)))
          CASE ('SDI') ! The .sdi file, containing wavelength, pattern, space group etc. etc.
            I = InfoError(1) ! reset the errors
            IF (InfoError(1) .NE. 0) GOTO 999
            CALL SDIFileLoad(line(:nl))
          CASE ('T0') ! T0, the starting temperature for the SA
            I = InfoError(1) ! reset the errors
            CALL INextReal(line, rTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            !...
          CASE ('COOL') ! The cooling rate for the SA
            I = InfoError(1) ! reset the errors
            CALL INextReal(line, rTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            !...
          CASE ('N1') ! N1 in the SA
            I = InfoError(1) ! reset the errors
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            !...
          CASE ('N2') ! N2 in the SA
            I = InfoError(1) ! reset the errors
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            !...
          CASE ('SEED1') ! Random generator seed 1
            I = InfoError(1) ! reset the errors
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            !...
          CASE ('SEED2') ! Random generator seed 2
            I = InfoError(1) ! reset the errors
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            !...
          CASE ('NRUNS') ! Termination criterion: number of SA runs
            I = InfoError(1) ! reset the errors
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            !...
          CASE ('MAXMOVES') ! Maximum number of moves, stored as e.g. 3.0 7 meaning 3.0E7
            I = InfoError(1) ! reset the errors
            CALL INextReal(line, rTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            !...
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            !...
          CASE ('MULTIPLIER') ! Termination criterion: profile chi-sqrd multiplier
            I = InfoError(1) ! reset the errors
            CALL INextReal(line, rTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            !...
          CASE ('RANDOMISE') 
            I = InfoError(1) ! reset the errors
            CALL INextString(line, tString)
            IF (InfoError(1) .NE. 0) GOTO 999
            iLen = LEN_TRIM(tString)
          !  IF ( tString(1:iLen) .EQ. "TRUE" )
          CASE ('ZMATRIX')
            I = InfoError(1) ! reset the errors
            iFrg = iFrg + 1
            frag_file(iFrg) = line(:nl)
            CALL Read_One_Zm(iFrg)
            !...
          CASE ('LIMITS')
            nFrag = iFrg
            I = InfoError(1) ! reset the errors
            CALL INextInteger(line, iTem)
            IF (InfoError(1) .NE. 0) GOTO 999
            DO I = 1, iTem
              READ(hFile, '(A)', END=100, ERR=999) line
              nl = LEN_TRIM(line)
              IF ( nl .NE. 0 ) THEN ! Blank line
                IF ( line(1:1) .EQ. "#" ) THEN ! It's a comment
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
      in_batch = .TRUE.
      CALL SA_Parameter_Set
      CALL BeginSA

      ! Exit DASH, but before doing so, save output to a file.
      ! it is probably best to save the output to a file that is specified in
      ! the .duff file, because that makes it easier for the user to later relate
      ! the output back to the input.

  999 CONTINUE
      CLOSE(hFile)

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
