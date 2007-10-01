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
      END DO

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
      END DO

      END FUNCTION ChrUpperCase
!
!*****************************************************************************
!
      INTEGER FUNCTION GetNumOfColumns(TheString)
!
! This function determines the number of columns in a string
! a column is a consecutive sequence of non-blank characters embedded in two blank-characters
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
        DO WHILE ((POS .LE. StrLen) .AND. (TheString(POS:POS) .EQ. ' '))
          POS = POS + 1
        END DO
! If we hit a non-space: it's a column
        IF ((POS .LE. StrLen) .AND. (TheString(POS:POS) .NE. ' ')) tNumOfColumns = tNumOfColumns + 1
! Scan past rest of column (find next space / end of string)
        DO WHILE ((POS .LE. StrLen) .AND. (TheString(POS:POS) .NE. ' '))
          POS = POS + 1
        END DO
      END DO
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
