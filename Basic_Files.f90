!
! Routines for reading / writing binary files
! Records in binary files must be 4 bytes (default in FORTRAN)
! File must be direct access (i.e. non-sequential)
!
!*****************************************************************************
! 
      SUBROUTINE FileWriteString(TheFileHandle, TheRecNr, TheString)
!
! Routine to write a string to a binary file.
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
      SUBROUTINE FileReadString(TheFileHandle, TheRecNr, TheString)
!
! Routine to read a string from a binary file.
! see FileWriteString
!
      IMPLICIT NONE

      INTEGER,       INTENT (IN   ) :: TheFileHandle
      INTEGER,       INTENT (INOUT) :: TheRecNr
      CHARACTER*(*), INTENT (  OUT) :: TheString

      INTEGER ErrorCode

      INTEGER     I, NumOfFourByteRecs, Remainder
      INTEGER*4   tActualLength
      CHARACTER*4 C4

      ErrorCode = 1
      TheString = ''
! Read length
      READ(TheFileHandle,REC=TheRecNr,ERR=999) tActualLength
      TheRecNr = TheRecNr + 1
! Find out how many 4 byte records we can read
      NumOfFourByteRecs = tActualLength / 4
      IF (NumOfFourByteRecs .NE. 0) THEN
        DO I = 1, NumOfFourByteRecs
          READ(TheFileHandle,REC=TheRecNr,ERR=999) C4
          TheRecNr = TheRecNr + 1
          TheString((I-1)*4+1:(I-1)*4+4) = C4(1:4)
        ENDDO
      ENDIF
! Now write the remainder, padded with spaces
      Remainder = MOD(tActualLength,4)
      IF (Remainder .NE. 0) THEN
        READ(TheFileHandle,REC=TheRecNr,ERR=999) C4
        TheRecNr = TheRecNr + 1
        TheString(NumOfFourByteRecs*4+1:NumOfFourByteRecs*4+Remainder) = C4(1:Remainder)
      ENDIF
      ErrorCode = 0
  999 RETURN

      END SUBROUTINE FileReadString
!
!*****************************************************************************
! 
      SUBROUTINE FileWriteInteger(TheFileHandle, TheRecNr, TheInteger)
!
! Routine to write an integer*4 to a binary file.
!
      IMPLICIT NONE

      INTEGER,   INTENT (IN   ) :: TheFileHandle
      INTEGER,   INTENT (INOUT) :: TheRecNr
      INTEGER*4, INTENT (IN   ) :: TheInteger

      INTEGER ErrorCode

      ErrorCode = 1
      WRITE(TheFileHandle,REC=TheRecNr,ERR=999) TheInteger
      TheRecNr = TheRecNr + 1
      ErrorCode = 0
  999 RETURN

      END SUBROUTINE FileWriteInteger
!
!*****************************************************************************
! 
      SUBROUTINE FileReadInteger(TheFileHandle, TheRecNr, TheInteger)
!
! Routine to read an integer*4 from a binary file.
!
      IMPLICIT NONE

      INTEGER,   INTENT (IN   ) :: TheFileHandle
      INTEGER,   INTENT (INOUT) :: TheRecNr
      INTEGER*4, INTENT (  OUT) :: TheInteger

      INTEGER ErrorCode

      ErrorCode = 1
      READ(TheFileHandle,REC=TheRecNr,ERR=999) TheInteger
      TheRecNr = TheRecNr + 1
      ErrorCode = 0
  999 RETURN

      END SUBROUTINE FileReadInteger
!
!*****************************************************************************
! 
      SUBROUTINE FileWriteReal(TheFileHandle, TheRecNr, TheReal)
!
! Routine to write a real*4 to a binary file.
!
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheFileHandle
      INTEGER, INTENT (INOUT) :: TheRecNr
      REAL*4,  INTENT (IN   ) :: TheReal

      INTEGER ErrorCode

      ErrorCode = 1
      WRITE(TheFileHandle,REC=TheRecNr,ERR=999) TheReal
      TheRecNr = TheRecNr + 1
      ErrorCode = 0
  999 RETURN

      END SUBROUTINE FileWriteReal
!
!*****************************************************************************
! 
      SUBROUTINE FileReadReal(TheFileHandle, TheRecNr, TheReal)
!
! Routine to read a real*4 from a binary file.
!
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheFileHandle
      INTEGER, INTENT (INOUT) :: TheRecNr
      REAL*4,  INTENT (  OUT) :: TheReal

      INTEGER ErrorCode

      ErrorCode = 1
      READ(TheFileHandle,REC=TheRecNr,ERR=999) TheReal
      TheRecNr = TheRecNr + 1
      ErrorCode = 0
  999 RETURN

      END SUBROUTINE FileReadReal
!
!*****************************************************************************
! 
      SUBROUTINE FileWriteLogical(TheFileHandle, TheRecNr, TheLogical)
!
! Routine to write a logical*4 to a binary file.
!
      IMPLICIT NONE

      INTEGER,   INTENT (IN   ) :: TheFileHandle
      INTEGER,   INTENT (INOUT) :: TheRecNr
      LOGICAL*4, INTENT (IN   ) :: TheLogical

      INTEGER ErrorCode

      ErrorCode = 1
      WRITE(TheFileHandle,REC=TheRecNr,ERR=999) TheLogical
      TheRecNr = TheRecNr + 1
      ErrorCode = 0
  999 RETURN

      END SUBROUTINE FileWriteLogical
!
!*****************************************************************************
! 
      SUBROUTINE FileReadLogical(TheFileHandle, TheRecNr, TheLogical)
!
! Routine to read a logical*4 from a binary file.
!
      IMPLICIT NONE

      INTEGER,   INTENT (IN   ) :: TheFileHandle
      INTEGER,   INTENT (INOUT) :: TheRecNr
      LOGICAL*4, INTENT (  OUT) :: TheLogical

      INTEGER ErrorCode

      ErrorCode = 1
      READ(TheFileHandle,REC=TheRecNr,ERR=999) TheLogical
      TheRecNr = TheRecNr + 1
      ErrorCode = 0
  999 RETURN

      END SUBROUTINE FileReadLogical
!
!*****************************************************************************
! 
