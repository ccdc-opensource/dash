!
!*****************************************************************************
!
      INTEGER FUNCTION DateToday
!
! RETURNS : today's date as an INTEGER. '20010215' for 15 Feb 2001  
!
      IMPLICIT NONE

      CHARACTER*8 tDate
      INTEGER     Today

      CALL DATE_AND_TIME(tDate)
      READ(tDate,*,ERR=99) Today
      DateToday = Today
      RETURN
   99 CALL DebugErrorMessage("Unable to obtain today's date")
      DateToday = 25000101 ! This way, all licences will have 'expired' when getting the date fails.

      END FUNCTION DateToday
!
!*****************************************************************************
!
      SUBROUTINE Date2String(TheDate,TheDateString, TheLength)
!
! INPUT  TheDate as an integer, e.g. '20010215' for 15 Feb 2001
! OUTPUT date, e.g. '7 March 2001'
! TheDate must have a minimum length of 17
!
      IMPLICIT NONE

      INTEGER,       INTENT (IN   ) :: TheDate
      CHARACTER*(*), INTENT (  OUT) :: TheDateString
      INTEGER,       INTENT (  OUT) :: TheLength

      CHARACTER*9  MonthStr(1:12)
      DATA MonthStr / 'January', 'February', 'March',     'April',   'May',      'June',             &
                      'July',    'August',   'September', 'October', 'November', 'December' /
      INTEGER Day, Month, Year
      CHARACTER*4 C4
      CHARACTER*2 C2
      INTEGER iLen
      INTEGER tDate ! call by reference / call by value ambiguity

      tDate = TheDate
      Year  = tDate / 10000
      tDate = tDate - Year * 10000
      Month = tDate /   100
      tDate = tDate - Month * 100
      Day   = tDate
      TheDateString = ''
      WRITE (C2,'(I2)') Day
      WRITE (C4,'(I4)') Year
      TheDateString = C2(1:2)//' '//MonthStr(Month)//' '//C4(1:4)
      CALL StrClean(TheDateString,iLen)
      TheLength = iLen

      END SUBROUTINE Date2String
!
!*****************************************************************************
!
      LOGICAL FUNCTION IsLeapYear(TheYear)

      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheYear

      IsLeapYear = ((MOD(TheYear,4) .EQ. 0) .AND. (MOD(TheYear,100) .NE. 0))

      END FUNCTION IsLeapYear 
!
!*****************************************************************************
!
      SUBROUTINE DateAddDays(TheDate, TheNumberOfDays)

      IMPLICIT NONE

      INTEGER, INTENT (INOUT) :: TheDate           ! '20010215' for 15 Feb 2001
      INTEGER, INTENT (IN   ) :: TheNumberOfDays

      INTEGER DaysOfMonth(1:12)
      DATA DaysOfMonth / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
      INTEGER Day, Month, Year
      LOGICAL, EXTERNAL :: IsLeapYear
      INTEGER tNumberOfDays ! call by reference / call by value ambiguity

      tNumberOfDays = TheNumberOfDays
      Year  = TheDate / 10000
      TheDate = TheDate - (Year*10000)
      Month = TheDate /   100
      TheDate = TheDate - (Month*100)
      Day   = TheDate
      IF (IsLeapYear(Year)) THEN
        DaysOfMonth(2) = 29
      ELSE
        DaysOfMonth(2) = 28
      ENDIF
      DO WHILE (tNumberOfDays .GT. 0)
        Day = Day + 1
        tNumberOfDays = tNumberOfDays - 1
        IF (Day .GT. DaysOfMonth(Month)) THEN
          Month = Month + 1
          Day = 1
        ENDIF
        IF (Month .GT. 12) THEN
          Year = Year + 1
          Month = 1
          IF (IsLeapYear(Year)) THEN
            DaysOfMonth(2) = 29
          ELSE
            DaysOfMonth(2) = 28
          ENDIF
        ENDIF
      ENDDO
      TheDate = Year * 10000 + Month * 100 + Day

      END SUBROUTINE DateAddDays 
!
!*****************************************************************************
!
