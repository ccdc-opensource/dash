!
!*****************************************************************************
!
      CHARACTER*1 FUNCTION ChrLowerCase(TheChar)

      IMPLICIT NONE

      CHARACTER*1, INTENT (IN   ) :: TheChar

      CHARACTER*26 lc, UC
      PARAMETER (lc = 'abcdefghijklmnopqrstuvwxyz')
      PARAMETER (UC = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ')
      INTEGER POS

      ChrLowerCase = TheChar
      DO POS = 1, 26
        IF (UC(POS:POS) .EQ. TheChar) THEN
          ChrLowerCase = lc(POS:POS)
          RETURN
        ENDIF
      ENDDO

      END FUNCTION ChrLowerCase
!
!*****************************************************************************
!
      CHARACTER*1 FUNCTION ChrUpperCase(TheChar)

      IMPLICIT NONE

      CHARACTER*1, INTENT (IN   ) :: TheChar

      CHARACTER*26 lc, UC
      PARAMETER (lc = 'abcdefghijklmnopqrstuvwxyz')
      PARAMETER (UC = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ')
      INTEGER POS

      ChrUpperCase = TheChar
      DO POS = 1, 26
        IF (lc(POS:POS) .EQ. TheChar) THEN
          ChrUpperCase = UC(POS:POS)
          RETURN
        ENDIF
      ENDDO

      END FUNCTION ChrUpperCase
!
!*****************************************************************************
!
      SUBROUTINE StrUpperCase(TheString)

      IMPLICIT NONE

      CHARACTER*(*), INTENT (INOUT) :: TheString

      INTEGER I
      CHARACTER*1 ChrUpperCase

      DO I = 1, LEN_TRIM(TheString)
        TheString(I:I) = ChrUpperCase(TheString(I:I))
      ENDDO

      END SUBROUTINE StrUpperCase
!
!*****************************************************************************
!
      LOGICAL FUNCTION ChrIsLetter(TheChar)

      IMPLICIT NONE

      CHARACTER*1, INTENT (IN   ) :: TheChar

      CHARACTER*26 lc, UC
      PARAMETER (lc = 'abcdefghijklmnopqrstuvwxyz')
      PARAMETER (UC = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ')
      INTEGER POS

      ChrIsLetter = .FALSE.
      DO POS = 1, 26
        IF ((TheChar .EQ. lc(POS:POS)) .OR. (TheChar .EQ. UC(POS:POS))) THEN
          ChrIsLetter = .TRUE.
          RETURN
        ENDIF
      ENDDO

      END FUNCTION ChrIsLetter
!
!*****************************************************************************
!
      INTEGER FUNCTION GetNumOfColumns(TheString)
!
! This function determines the number of columns in a string
! a column is a consecutive sequence of non-blank characters embedded in two blank-characters
! A blank character is either a space or a tab
!
! JvdS 27 July 2001
!
! INPUT   : TheString = the string
!
! RETURNS : Number of columns
!

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheString

      INTEGER POS
      INTEGER tNumOfColumns
      INTEGER StrLen

      StrLen = LEN_TRIM(TheString)
      tNumOfColumns = 0
      POS = 1
      DO WHILE (POS .LE. StrLen)
! Skip spaces
        DO WHILE ((POS .LE. StrLen) .AND. ((TheString(POS:POS) .EQ. ' ') .OR. (TheString(POS:POS) .EQ. CHAR(9))))
          POS = POS + 1
        ENDDO
! If we hit a non-space: it's a column
        IF ((POS .LE. StrLen) .AND. ((TheString(POS:POS) .NE. ' ') .AND. (TheString(POS:POS) .NE. CHAR(9)))) tNumOfColumns = tNumOfColumns + 1
! Scan past rest of column (find next space / end of string)
        DO WHILE ((POS .LE. StrLen) .AND. ((TheString(POS:POS) .NE. ' ') .AND. (TheString(POS:POS) .NE. CHAR(9))))
          POS = POS + 1
        ENDDO
      ENDDO
      GetNumOfColumns = tNumOfColumns

      END FUNCTION GetNumOfColumns
!
!*****************************************************************************
!
      SUBROUTINE GetSubString(TheString, TheDelimiter, TheSubString)
!
! This subroutine returns the substring of a string until a certain character is found.
! This way, individual fields delimited by a character can be extracted from a string.
!
! JvdS 24 Aug 2001
!
! INPUT  : TheString    = the string that should be scanned.
!          TheDelimiter = the character that separates fields. The beginning and the end of TheString
!                         are separators as well.
!
! OUTPUT : TheSubString contains the substring, excluding the delimiter
!          TheString    has the substring and the delimiter removed on exit.
!
      IMPLICIT NONE

      CHARACTER*(*), INTENT (INOUT) :: TheString
      CHARACTER    , INTENT (IN   ) :: TheDelimiter
      CHARACTER*(*), INTENT (  OUT) :: TheSubString

      INTEGER POS, tLEN, I, tPOS

      tLEN = LEN_TRIM(TheString)
      TheSubString = ' '
      IF (tLEN .EQ. 0) RETURN
! Find first occurrence of TheDelimiter
      POS = 1
      DO WHILE ((POS .LT. tLEN) .AND. (TheString(POS:POS) .NE. TheDelimiter))
        POS = POS + 1
      ENDDO
! The delimiter itself should not be copied
      IF (TheString(POS:POS) .EQ. TheDelimiter) THEN
        tPOS = POS - 1
      ELSE
        tPOS = POS
      ENDIF
      IF (tPOS .NE. 0) THEN
! Fill the substring
        TheSubString(1:tPOS) = TheString(1:tPOS)
      ENDIF
! Remove the substring, including the delimiter, from the string
      IF (POS .EQ. tLEN) THEN
        TheString = ' '
      ELSE
        DO I = 1, tLEN-POS
          TheString(I:I) = TheString(I+POS:I+POS)
        ENDDO
! Overwrite remainder with spaces
        DO I = 1+(tLEN-POS), tLEN
          TheString(I:I) = ' '
        ENDDO
      ENDIF

      END SUBROUTINE GetSubString
!
!*****************************************************************************
!
      SUBROUTINE StrClean(TheString, TheLength)
!
! This function cleans up a string
! Spaces from start and end are stripped
! Each tab is converted into a space (this could be made much more general:
! we could have a separate routine that takes a list of characters that are to be
! interpreted as delimiters and convert all of them to one character [not necessarily a space])
! Multiple spaces are replaced by a single space
!
! JvdS 9 Oct 2001
!
! INPUT   : TheString = the string
!        
! OUTPUT  : TheString = the cleaned up version of the string
!           TheLength = the length of the final string
!
      IMPLICIT NONE

      CHARACTER*(*), INTENT (INOUT) :: TheString
      INTEGER,       INTENT (  OUT) :: TheLength

      INTEGER POS, NewPos, J
      INTEGER StrLen, OriginalLength
      LOGICAL LastCharWasSpace

      StrLen = LEN_TRIM(TheString)
      OriginalLength = StrLen
      IF (StrLen .EQ. 0) THEN
        TheLength = 0
        RETURN
      ENDIF
! Replace tabs by spaces
      DO POS = 1, StrLen
        IF (TheString(POS:POS) .EQ. CHAR(9)) TheString(POS:POS) = ' '
      ENDDO
      StrLen = LEN_TRIM(TheString)
      IF (StrLen .EQ. 0) THEN
        TheLength = 0
        RETURN
      ENDIF
! Skip spaces at start
      POS = 1
      DO WHILE (TheString(POS:POS) .EQ. ' ')
        POS = POS + 1
      ENDDO
      IF (POS .NE. 1) THEN
        POS = POS - 1
        DO J = 1, StrLen-POS
          TheString(J:J) = TheString(J+POS:J+POS)
        ENDDO
        StrLen = StrLen-POS
      ENDIF
! Replace multiple occurrences of a space by a single space
      LastCharWasSpace = .FALSE.
      NewPos = 1
      DO POS = 1, StrLen
        IF (TheString(POS:POS) .EQ. ' ') THEN
          IF (.NOT. LastCharWasSpace) THEN
            TheString(NewPos:NewPos) = TheString(POS:POS)
            NewPos = NewPos + 1
            LastCharWasSpace = .TRUE.
          ENDIF
        ELSE
          TheString(NewPos:NewPos) = TheString(POS:POS)
          NewPos = NewPos + 1
          LastCharWasSpace = .FALSE.
        ENDIF
      ENDDO
      TheLength = NewPos - 1
      IF (TheLength .EQ. OriginalLength) RETURN
! Pad rest of the old string with spaces
      DO J = TheLength+1, OriginalLength
        TheString(J:J) = ' '
      ENDDO

      END SUBROUTINE StrClean
!
!*****************************************************************************
!
      CHARACTER*20 FUNCTION Integer2String(TheInteger)
!
! Converts an INTEGER to a left-justified CHARACTER*20
!
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheInteger

      CHARACTER*20 tString
      INTEGER POS, J

      WRITE(tString,'(I20)') TheInteger
! Skip spaces at start
      POS = 1
      DO WHILE (tString(POS:POS) .EQ. ' ')
        POS = POS + 1
      ENDDO
      IF (POS .NE. 1) THEN
        POS = POS - 1
        DO J = 1, 20-POS
          tString(J:J) = tString(J+POS:J+POS)
        ENDDO
! Pad rest of the old string with spaces
        DO J = 20-POS+1, 20
          tString(J:J) = ' '
        ENDDO
      ENDIF
      Integer2String = tString

      END FUNCTION Integer2String
!
!*****************************************************************************
!
