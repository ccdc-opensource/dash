!
! Routines for reading / writing binary files
! Records in binary files must be 4 bytes (default in FORTRAN)
! File must be direct access (i.e. non-sequential)
!
!*****************************************************************************
! 
      INTEGER FUNCTION GetBFIOError()

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
      CALL FileWriteInteger(TheFileHandle, TheRecNr, tActualLength)
      IF (BFIOErrorCode .NE. 0) RETURN
! Find out how many 4 byte records we can write
      NumOfFourByteRecs = tActualLength / 4
      IF (NumOfFourByteRecs .NE. 0) THEN
        DO I = 1, NumOfFourByteRecs
          C4(1:4) = TheString((I-1)*4+1:(I-1)*4+4)
          CALL FileWriteInteger(TheFileHandle, TheRecNr, I4)
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
      CALL FileReadInteger(TheFileHandle, TheRecNr, tActualLength)
      IF (BFIOErrorCode .NE. 0) RETURN
! Find out how many 4 byte records we can read
      NumOfFourByteRecs = tActualLength / 4
      IF (NumOfFourByteRecs .NE. 0) THEN
        DO I = 1, NumOfFourByteRecs
          CALL FileReadInteger(TheFileHandle, TheRecNr, I4)
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

      LOGICAL FUNCTION FileErrorOccurred()

      INTEGER     BFIOErrorCode
      COMMON /IO/ BFIOErrorCode

      FileErrorOccurred = ( BFIOErrorCode .EQ. 1 )

      END FUNCTION FileErrorOccurred
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

      CALL FileReadInteger(TheFileHandle, TheRecNr, I4)
      TheLogical = L4

      END SUBROUTINE FileReadLogical
!
!*****************************************************************************
!
      SUBROUTINE FileRWString(TheFileHandle, TheRecNr, RW, TheString)

      IMPLICIT NONE

      INTEGER,       INTENT (IN   ) :: TheFileHandle
      INTEGER,       INTENT (INOUT) :: TheRecNr
      INTEGER,       INTENT (IN   ) :: RW
      CHARACTER*(*), INTENT (INOUT) :: TheString

      IF (RW .EQ. 1) THEN
        CALL FileReadString(TheFileHandle, TheRecNr, TheString)
      ELSE
        CALL FileWriteString(TheFileHandle, TheRecNr, TheString)
      ENDIF

      END SUBROUTINE FileRWString
!
!*****************************************************************************
!
      SUBROUTINE FileRWInteger(TheFileHandle, TheRecNr, RW, TheInteger)

      IMPLICIT NONE

      INTEGER,   INTENT (IN   ) :: TheFileHandle
      INTEGER,   INTENT (INOUT) :: TheRecNr
      INTEGER,   INTENT (IN   ) :: RW
      INTEGER*4, INTENT (INOUT) :: TheInteger

      IF (RW .EQ. 1) THEN
        CALL FileReadInteger(TheFileHandle, TheRecNr, TheInteger)
      ELSE
        CALL FileWriteInteger(TheFileHandle, TheRecNr, TheInteger)
      ENDIF

      END SUBROUTINE FileRWInteger
!
!*****************************************************************************
!
      SUBROUTINE FileRWReal(TheFileHandle, TheRecNr, RW, TheReal)

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheFileHandle
      INTEGER, INTENT (INOUT) :: TheRecNr
      INTEGER, INTENT (IN   ) :: RW
      REAL*4,  INTENT (INOUT) :: TheReal

      IF (RW .EQ. 1) THEN
        CALL FileReadReal(TheFileHandle, TheRecNr, TheReal)
      ELSE
        CALL FileWriteReal(TheFileHandle, TheRecNr, TheReal)
      ENDIF

      END SUBROUTINE FileRWReal
!
!*****************************************************************************
!
      SUBROUTINE FileRWLogical(TheFileHandle, TheRecNr, RW, TheLogical)

      IMPLICIT NONE

      INTEGER,   INTENT (IN   ) :: TheFileHandle
      INTEGER,   INTENT (INOUT) :: TheRecNr
      INTEGER,   INTENT (IN   ) :: RW
      LOGICAL*4, INTENT (INOUT) :: TheLogical

      IF (RW .EQ. 1) THEN
        CALL FileReadLogical(TheFileHandle, TheRecNr, TheLogical)
      ELSE
        CALL FileWriteLogical(TheFileHandle, TheRecNr, TheLogical)
      ENDIF

      END SUBROUTINE FileRWLogical
!
!*****************************************************************************
!
      SUBROUTINE SplitPath(PathName, DirName, FileName)
!
! This routine splits a full filename into the name of the file and its path.
! If a path was present, it will still have its '\' at the end.

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: PathName
      CHARACTER*(*), INTENT (  OUT) :: DirName, FileName

      INTEGER I

      I = LEN_TRIM(PathName)
      IF ( I .GT. 0 ) THEN
        DO WHILE ((I .GT. 0) .AND. (PathName(I:I) .NE. DIRSPACER))
          I = I - 1
        ENDDO
      END IF
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
      SUBROUTINE SplitPath2(FullName, DirName, FileName, Extension, ExtLength)
!
! This routine splits a full filename into the name of the file, its extension and its path.
! If a path was present, it will still have its '\' at the end.

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: FullName
      CHARACTER*(*), INTENT (  OUT) :: DirName, FileName
      CHARACTER*(*), INTENT (  OUT) :: Extension
      INTEGER,       INTENT (INOUT) :: ExtLength      ! The length of the extension.

      INTEGER I, iLen, iPos

      I = LEN_TRIM(FullName)
      DO WHILE ((I .GT. 0) .AND. (FullName(I:I) .NE. DIRSPACER))
        I = I - 1
      ENDDO
      IF (I .EQ. 0) THEN
        DirName  = ''
        FileName = FullName
      ELSE
        DirName  = FullName(1:I)
        FileName = FullName(I+1:LEN_TRIM(FullName))
      ENDIF
      iLen = LEN_TRIM(FileName)
! Find the last occurence of '.' in FileName
      iPos = iLen - 1 ! Last character of FileName is not tested
! The longest extension possible is ExtLength
      DO WHILE ((iPos .NE. 0) .AND. (FileName(iPos:iPos) .NE. DIRSPACER) .AND. (FileName(iPos:iPos) .NE. '.') .AND. (iPos .NE. (iLen-ExtLength-1)))
        iPos = iPos - 1
      ENDDO
! If we haven't found a '.' by now, we cannot deal with the extension anyway
      Extension = ''
      IF (iPos .EQ. 0 .OR. FileName(iPos:iPos) .NE. '.') THEN
        ExtLength = 0
      ELSE
        Extension = FileName(iPos+1:iLen)
        ExtLength = LEN_TRIM(Extension)
        DO I = iPos, iLen
          FileName(I:I) = " "
        ENDDO
      ENDIF

      END SUBROUTINE SplitPath2
!
!*****************************************************************************
! 
      SUBROUTINE FileGetExtension(TheFile, TheExtension, TheLength)
!
! OUTPUT  TheExtension = the extension, without the dot
!
      USE VARIABLES
      
      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFile
      CHARACTER*(*), INTENT (  OUT) :: TheExtension
      INTEGER,       INTENT (INOUT) :: TheLength      ! The length of the extension.

      INTEGER iLen, iPos

      iLen = LEN_TRIM(TheFile)
! Find the last occurence of '.' in TheFile
      iPos = iLen - 1 ! Last character of TheFile is not tested
! The longest extension possible is TheLength
      DO WHILE ((iPos .NE. 0) .AND. (TheFile(iPos:iPos) .NE. DIRSPACER) .AND. (TheFile(iPos:iPos) .NE. '.') .AND. (iPos .NE. (iLen-TheLength-1)))
        iPos = iPos - 1
      ENDDO
! If we haven't found a '.' by now, we cannot deal with the extension anyway
      TheExtension = ''
      IF (TheFile(iPos:iPos) .NE. '.') THEN
        TheLength = 0
      ELSE
        TheExtension = TheFile(iPos+1:iLen)
        TheLength = LEN_TRIM(TheExtension)
      ENDIF

      END SUBROUTINE FileGetExtension
!
!*****************************************************************************
! 
      SUBROUTINE StripPathIfInvalid(FileName)
!
! This routine first locates the rightmost forward or backward slash, which splitting
! FileName into path and filename. If a non-empty path does not exist, strips it
! from FileName to force it in current directory. Otherwise, does nothing.
      USE WINTERACTER

      IMPLICIT NONE

      CHARACTER*(*), INTENT (INOUT) :: FileName

      LOGICAL, EXTERNAL :: Confirm
      LOGICAL, SAVE :: Warn
      DATA Warn/.TRUE./
      INTEGER iPos

      iPos = SCAN(FileName, '/\', BACK=.TRUE.)
      IF (iPos .LE. 0) & ! no path
        RETURN

      IF (IOsDirExists(FileName(:iPos))) & ! valid path
        RETURN

      IF (Warn) &
        Warn = Confirm('The path to file:'//CHAR(13)//CHAR(13)// &
                     TRIM(FileName)//CHAR(13)//CHAR(13)// &
                     'does not exist or is invalid, so is stripped.'//CHAR(13)// &
                     'The current directory will be used by default.'//CHAR(13)//CHAR(13)// &
                     'Display this kind warning again next time?')
      ! strip path
      IF (iPos .GE. LEN(FileName)) THEN
        FileName = ''
      ELSE
        FileName = FileName(iPos+1:)
      ENDIF
 
      END SUBROUTINE StripPathIfInvalid
!
!*****************************************************************************
! 
