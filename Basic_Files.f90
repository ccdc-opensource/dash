!
! Routines for reading / writing binary files
! Records in binary files must be 4 bytes (default in FORTRAN)
! File must be direct access (i.e. non-sequential)
!
!*****************************************************************************
! 
      INTEGER FUNCTION GetBFIOError

      IMPLICIT NONE

      INTEGER     BFIOErrorCode
      COMMON /IO/ BFIOErrorCode

      GetBFIOError = BFIOErrorCode
      BFIOErrorCode = 0

      END FUNCTION GetBFIOError
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
! On exit, IOErrorCode in /IO/ holds any error, 0 = success
!
      IMPLICIT NONE

      INTEGER,       INTENT (IN   ) :: TheFileHandle
      INTEGER,       INTENT (INOUT) :: TheRecNr
      CHARACTER*(*), INTENT (IN   ) :: TheString

      INTEGER     BFIOErrorCode
      COMMON /IO/ BFIOErrorCode

      INTEGER     I, NumOfFourByteRecs, Remainder
      INTEGER*4   tActualLength
      CHARACTER*4 C4
      INTEGER*4   I4
      EQUIVALENCE (C4,I4)

      tActualLength = LEN_TRIM(TheString)
      CALL FileWriteInteger(TheFileHandle,TheRecNr,tActualLength)
      IF (BFIOErrorCode .NE. 0) RETURN
! Find out how many 4 byte records we can write
      NumOfFourByteRecs = tActualLength / 4
      IF (NumOfFourByteRecs .NE. 0) THEN
        DO I = 1, NumOfFourByteRecs
          C4(1:4) = TheString((I-1)*4+1:(I-1)*4+4)
          CALL FileWriteInteger(TheFileHandle,TheRecNr,I4)
          IF (BFIOErrorCode .NE. 0) RETURN
        ENDDO
      ENDIF
! Now write the remainder, padded with spaces
      Remainder = MOD(tActualLength,4)
      IF (Remainder .NE. 0) THEN
        C4(1:4) = '    '
        C4(1:Remainder) = TheString(NumOfFourByteRecs*4+1:NumOfFourByteRecs*4+Remainder)
        CALL FileWriteInteger(TheFileHandle,TheRecNr,I4)
        IF (BFIOErrorCode .NE. 0) RETURN
      ENDIF

      END SUBROUTINE FileWriteString
!
!*****************************************************************************
! 
      SUBROUTINE FileReadString(TheFileHandle, TheRecNr, TheString)
!
! Routine to read a string from a binary file.
! see FileWriteString
! On exit, IOErrorCode in /IO/ holds any error, 0 = success
!
      IMPLICIT NONE

      INTEGER,       INTENT (IN   ) :: TheFileHandle
      INTEGER,       INTENT (INOUT) :: TheRecNr
      CHARACTER*(*), INTENT (  OUT) :: TheString

      INTEGER     BFIOErrorCode
      COMMON /IO/ BFIOErrorCode

      INTEGER     I, NumOfFourByteRecs, Remainder
      INTEGER*4   tActualLength
      CHARACTER*4 C4
      INTEGER*4   I4
      EQUIVALENCE (C4,I4)

      TheString = ''
! Read length
      CALL FileReadInteger(TheFileHandle,TheRecNr,tActualLength)
      IF (BFIOErrorCode .NE. 0) RETURN
! Find out how many 4 byte records we can read
      NumOfFourByteRecs = tActualLength / 4
      IF (NumOfFourByteRecs .NE. 0) THEN
        DO I = 1, NumOfFourByteRecs
          CALL FileReadInteger(TheFileHandle,TheRecNr,I4)
          IF (BFIOErrorCode .NE. 0) RETURN
          TheString((I-1)*4+1:(I-1)*4+4) = C4(1:4)
        ENDDO
      ENDIF
! Now Read the remainder, padded with spaces
      Remainder = MOD(tActualLength,4)
      IF (Remainder .NE. 0) THEN
        CALL FileReadInteger(TheFileHandle,TheRecNr,I4)
        IF (BFIOErrorCode .NE. 0) RETURN
        TheString(NumOfFourByteRecs*4+1:NumOfFourByteRecs*4+Remainder) = C4(1:Remainder)
      ENDIF

      END SUBROUTINE FileReadString
!
!*****************************************************************************
! 
      SUBROUTINE FileWriteInteger(TheFileHandle, TheRecNr, TheInteger)
!
! Routine to write an integer*4 to a binary file.
! On exit, IOErrorCode in /IO/ holds any error, 0 = success
!
      IMPLICIT NONE

      INTEGER,   INTENT (IN   ) :: TheFileHandle
      INTEGER,   INTENT (INOUT) :: TheRecNr
      INTEGER*4, INTENT (IN   ) :: TheInteger

      INTEGER     BFIOErrorCode
      COMMON /IO/ BFIOErrorCode

      BFIOErrorCode = 1
      WRITE(TheFileHandle,REC=TheRecNr,ERR=999) TheInteger
      TheRecNr = TheRecNr + 1
      BFIOErrorCode = 0
  999 RETURN

      END SUBROUTINE FileWriteInteger
!
!*****************************************************************************
! 
      SUBROUTINE FileReadInteger(TheFileHandle, TheRecNr, TheInteger)
!
! Routine to read an integer*4 from a binary file.
! On exit, IOErrorCode in /IO/ holds any error, 0 = success
!
      IMPLICIT NONE

      INTEGER,   INTENT (IN   ) :: TheFileHandle
      INTEGER,   INTENT (INOUT) :: TheRecNr
      INTEGER*4, INTENT (  OUT) :: TheInteger

      INTEGER     BFIOErrorCode
      COMMON /IO/ BFIOErrorCode

      BFIOErrorCode = 1
      READ(TheFileHandle,REC=TheRecNr,ERR=999) TheInteger
      TheRecNr = TheRecNr + 1
      BFIOErrorCode = 0
  999 RETURN

      END SUBROUTINE FileReadInteger
!
!*****************************************************************************
! 
      SUBROUTINE FileWriteReal(TheFileHandle, TheRecNr, TheReal)
!
! Routine to write a real*4 to a binary file.
! On exit, IOErrorCode in /IO/ holds any error, 0 = success
!
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheFileHandle
      INTEGER, INTENT (INOUT) :: TheRecNr
      REAL*4,  INTENT (IN   ) :: TheReal

      REAL*4    R4
      INTEGER*4 I4
      EQUIVALENCE (R4,I4)

      R4 = TheReal
      CALL FileWriteInteger(TheFileHandle,TheRecNr,I4)

      END SUBROUTINE FileWriteReal
!
!*****************************************************************************
! 
      SUBROUTINE FileReadReal(TheFileHandle, TheRecNr, TheReal)
!
! Routine to read a real*4 from a binary file.
! On exit, IOErrorCode in /IO/ holds any error, 0 = success
!
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheFileHandle
      INTEGER, INTENT (INOUT) :: TheRecNr
      REAL*4,  INTENT (  OUT) :: TheReal

      REAL*4    R4
      INTEGER*4 I4
      EQUIVALENCE (R4,I4)

      CALL FileReadInteger(TheFileHandle,TheRecNr,I4)
      TheReal = R4

      END SUBROUTINE FileReadReal
!
!*****************************************************************************
! 
      SUBROUTINE FileWriteLogical(TheFileHandle, TheRecNr, TheLogical)
!
! Routine to write a logical*4 to a binary file.
! On exit, IOErrorCode in /IO/ holds any error, 0 = success
!
      IMPLICIT NONE

      INTEGER,   INTENT (IN   ) :: TheFileHandle
      INTEGER,   INTENT (INOUT) :: TheRecNr
      LOGICAL*4, INTENT (IN   ) :: TheLogical

      LOGICAL*4 L4
      INTEGER*4 I4
      EQUIVALENCE (L4,I4)

      L4 = TheLogical
      CALL FileWriteInteger(TheFileHandle,TheRecNr,I4)

      END SUBROUTINE FileWriteLogical
!
!*****************************************************************************
! 
      SUBROUTINE FileReadLogical(TheFileHandle, TheRecNr, TheLogical)
!
! Routine to read a logical*4 from a binary file.
! On exit, IOErrorCode in /IO/ holds any error, 0 = success
!
      IMPLICIT NONE

      INTEGER,   INTENT (IN   ) :: TheFileHandle
      INTEGER,   INTENT (INOUT) :: TheRecNr
      LOGICAL*4, INTENT (  OUT) :: TheLogical

      LOGICAL*4 L4
      INTEGER*4 I4
      EQUIVALENCE (L4,I4)

      CALL FileReadInteger(TheFileHandle,TheRecNr,I4)
      TheLogical = L4

      END SUBROUTINE FileReadLogical
!
!*****************************************************************************
!
      SUBROUTINE SplitPath(PathName,DirName,FileName)
!
! This routine splits a full filename into the name of the file and its path.
! If a path was present, it will still have its '\' at the end.

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: PathName
      CHARACTER*(*), INTENT (  OUT) :: DirName, FileName

      INTEGER I

      I = LEN_TRIM(PathName)
      DO WHILE ((I .GT. 0) .AND. (PathName(I:I) .NE. DIRSPACER))
        I = I - 1
      ENDDO
      IF (I .EQ. 0) THEN
        DirName  = ''
        FileName = PathName
      ELSE
        DirName  = PathName(1:I)
        FileName = PathName(I+1:LEN_TRIM(PathName))
      ENDIF

      END SUBROUTINE SplitPath
!
!*****************************************************************************
! 
