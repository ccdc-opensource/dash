!
!*****************************************************************************
! 
      SUBROUTINE FileWriteString(TheFileHandle, TheRecNr, TheString)
!
! Routine to write a string to a binary file.
! Records in binary files must be 4 bytes (default in FORTRAN)
! First the length of the string is written to the file (in the first four bytes)
! The number of characters written is padded with spaces so as to be a multiple of 4.
! Note that the length written is the actual length though!
! This way, it is possible to read and write strings of any length, e.g. CHARACTER(3)
!
      IMPLICIT NONE

      INTEGER,       INTENT (IN   ) :: TheFileHandle
      INTEGER,       INTENT (INOUT) :: TheRecNr
      CHARACTER*(*), INTENT (IN   ) :: TheString

      INTEGER ErrorCode

      INTEGER     I, NumOfFourByteRecs, Remainder
      INTEGER*4   tActualLength
      CHARACTER*4 C4

      ErrorCode = 1
      tActualLength = LEN_TRIM(TheString)
      WRITE(TheFileHandle,REC=TheRecNr,ERR=999) tActualLength
      TheRecNr = TheRecNr + 1
! Find out how many 4 byte records we can write
      NumOfFourByteRecs = tActualLength / 4
      IF (NumOfFourByteRecs .NE. 0) THEN
        DO I = 1, NumOfFourByteRecs
          C4(1:4) = TheString((I-1)*4+1:(I-1)*4+4)
          WRITE(TheFileHandle,REC=TheRecNr,ERR=999) C4
          TheRecNr = TheRecNr + 1
        ENDDO
      ENDIF
! Now write the remainder, padded with spaces
      Remainder = MOD(tActualLength,4)
      IF (Remainder .NE. 0) THEN
        C4(1:4) = '    '
        C4(1:Remainder) = TheString(NumOfFourByteRecs*4+1:NumOfFourByteRecs*4+Remainder)
        WRITE(TheFileHandle,REC=TheRecNr,ERR=999) C4
        TheRecNr = TheRecNr + 1
      ENDIF
      ErrorCode = 0
  999 RETURN

      END SUBROUTINE FileWriteString
!
!*****************************************************************************
! 
