! This file is part of DASH.
! SPDX-Identifier: MIT
!
! Copyright 2001 Science and Technology Facilities Council
! Copyright 2001 Cambridge Crystallographic Data Centre
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
!*****************************************************************************
!
      INTEGER FUNCTION TimeNowSeconds()
!
! RETURNS : current time in seconds elapsed since 00:00 (midnight)
!
      IMPLICIT NONE

      TYPE TimeType
        INTEGER hours
        INTEGER minutes
        INTEGER seconds
      END TYPE

      INTEGER hours
      INTEGER minutes
      INTEGER seconds

      CHARACTER*8 tDate
      CHARACTER*10 tTime

      CALL DATE_AND_TIME(tDate, tTime) ! Returns 24-hour clock, I checked
      READ(tTime(1:2),*,ERR=999) hours
      READ(tTime(3:4),*,ERR=999) minutes
      READ(tTime(5:6),*,ERR=999) seconds
      TimeNowSeconds = hours*3600 + minutes*60 + seconds
      RETURN
  999 CALL DebugErrorMessage("Unable to obtain current time")
      TimeNowSeconds = 0

      END FUNCTION TimeNowSeconds
!
!*****************************************************************************
!
      SUBROUTINE TimeNow(CurrentTime)
!
! RETURNS : current time
!
      IMPLICIT NONE

      TYPE TimeType
        INTEGER hours
        INTEGER minutes
        INTEGER seconds
      END TYPE

      TYPE(TimeType), INTENT (  OUT) :: CurrentTime

      CHARACTER*8 tDate
      CHARACTER*10 tTime

      CALL DATE_AND_TIME(tDate, tTime)
      READ(tTime(1:2),*,ERR=999) CurrentTime%hours
      READ(tTime(3:4),*,ERR=999) CurrentTime%minutes
      READ(tTime(5:6),*,ERR=999) CurrentTime%seconds
      RETURN
  999 CALL DebugErrorMessage("Unable to obtain current time")
      CurrentTime%hours = 12
      CurrentTime%minutes = 0
      CurrentTime%seconds = 0

      END SUBROUTINE TimeNow
!
!*****************************************************************************
!
      INTEGER FUNCTION DateToday()
!
! RETURNS : today's date as an INTEGER. '20010215' for 15 Feb 2001  
!
      IMPLICIT NONE

      CHARACTER*8 tDate
      INTEGER     Today

      CALL DATE_AND_TIME(tDate)
      READ(tDate,*,ERR=999) Today
      DateToday = Today
      RETURN
  999 CALL DebugErrorMessage("Unable to obtain today's date")
      DateToday = 25000101 ! This way, all licences will have 'expired' when getting the date fails.

      END FUNCTION DateToday
!
!*****************************************************************************
!
      SUBROUTINE Date2String(TheDate, TheDateString, TheLength)
!
! INPUT  TheDate as an integer, e.g. '20010215' for 15 Feb 2001
! OUTPUT TheDateString, e.g. '7 March 2001'
! TheDateString must have a minimum length of 17
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

      LOGICAL, EXTERNAL :: IsLeapYear
      INTEGER DaysOfMonth(1:12)
      DATA DaysOfMonth / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
      INTEGER Day, Month, Year
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
      INTEGER FUNCTION DateDaysElapsed(TheDate1, TheDate2)
!
! RETURNS : TheDate2 - TheDate1, i.e. the number of days in going from TheDate1 to TheDate2
!
      IMPLICIT NONE

      INTEGER, INTENT (IN   ) :: TheDate1, TheDate2           ! '20010215' for 15 Feb 2001

      LOGICAL, EXTERNAL :: IsLeapYear
      INTEGER DaysOfMonth(1:12)
      DATA DaysOfMonth / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
      INTEGER Day, Month, Year
      INTEGER StartDate, EndDate, tStartDate
      INTEGER tDaysElapsed
      LOGICAL neg

      IF (TheDate1 .GT. TheDate2) THEN
        neg = .TRUE.
        StartDate = TheDate2
        EndDate   = TheDate1
      ELSE
        neg = .FALSE.
        StartDate = TheDate1
        EndDate   = TheDate2
      ENDIF
      tStartDate = StartDate
      Year      = tStartDate / 10000
      tStartDate = tStartDate - (Year*10000)
      Month     = tStartDate /   100
      tStartDate = tStartDate - (Month*100)
      Day       = tStartDate
      IF (IsLeapYear(Year)) THEN
        DaysOfMonth(2) = 29
      ELSE
        DaysOfMonth(2) = 28
      ENDIF
      tDaysElapsed = 0
      DO WHILE (StartDate .NE. EndDate)
        tDaysElapsed = tDaysElapsed + 1
        Day = Day + 1
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
        StartDate = Year * 10000 + Month * 100 + Day
      ENDDO
      IF (neg) tDaysElapsed = -tDaysElapsed
      DateDaysElapsed = tDaysElapsed

      END FUNCTION DateDaysElapsed 
!
!*****************************************************************************
!
      SUBROUTINE TimeElapsed(StartDate, StartTime, EndDate, EndTime, DurationString, StrLen)

      IMPLICIT NONE

      INTEGER,       INTENT (IN   ) :: StartDate, StartTime
      INTEGER,       INTENT (IN   ) :: EndDate, EndTime
      CHARACTER*(*), INTENT (  OUT) :: DurationString
      INTEGER,       INTENT (  OUT) :: StrLen

      INTEGER, EXTERNAL :: DateDaysElapsed
      CHARACTER*20, EXTERNAL :: Integer2String
      INTEGER DaysElapsed, NItems
      INTEGER DiffSecs, nweeks, nhours, nminutes
      INTEGER OneDay ! The number of seconds in one day
      CHARACTER*20 tIntStr

      OneDay = 24 * 60 * 60
      DaysElapsed = DateDaysElapsed(StartDate, EndDate)
      DurationString = ""
      StrLen = LEN_TRIM(DurationString)
      NItems = 0
      DiffSecs = EndTime - StartTime
      IF (DaysElapsed .GT. 0) THEN
        IF (DiffSecs .LT. 0) THEN
          DaysElapsed = DaysElapsed - 1
          DiffSecs = DiffSecs + OneDay
        ENDIF
!C Divide number of days into weeks and days
        nweeks = DaysElapsed / 7
        IF (nweeks .NE. 0) THEN
          DaysElapsed = DaysElapsed - nweeks*7
          tIntStr = Integer2String(nweeks)
          DurationString = DurationString(1:StrLen)//tIntStr(1:LEN_TRIM(tIntStr))//" weeks"
          StrLen = LEN_TRIM(DurationString)
          IF (nweeks .EQ. 1) StrLen = StrLen - 1
          NItems = NItems + 1
        ENDIF
        IF (DaysElapsed .NE. 0) THEN
          IF (NItems .GT. 0) THEN
            DurationString = DurationString(1:StrLen)//", "
            StrLen = StrLen + 2
          ENDIF
          tIntStr = Integer2String(DaysElapsed)
          DurationString = DurationString(1:StrLen)//tIntStr(1:LEN_TRIM(tIntStr))//" days"
          StrLen = LEN_TRIM(DurationString)
          IF (DaysElapsed .EQ. 1) StrLen = StrLen - 1
          NItems = NItems + 1
        ENDIF
      ENDIF
!C Divide number of seconds into hours, minutes and seconds
      nhours = DiffSecs / 3600
      IF (nhours .NE. 0) THEN
        DiffSecs = DiffSecs - nhours*3600
        IF (NItems .GT. 0) THEN
          DurationString = DurationString(1:StrLen)//", "
          StrLen = StrLen + 2
        ENDIF
        tIntStr = Integer2String(nhours)
        DurationString = DurationString(1:StrLen)//tIntStr(1:LEN_TRIM(tIntStr))//" hours"
        StrLen = LEN_TRIM(DurationString)
        IF (nhours .EQ. 1) StrLen = StrLen - 1
        NItems = NItems + 1
      ENDIF
      nminutes = DiffSecs / 60
      IF (nminutes .NE. 0) THEN
        DiffSecs = DiffSecs - nminutes*60
        IF (NItems .GT. 0) THEN
          DurationString = DurationString(1:StrLen)//", "
          StrLen = StrLen + 2
        ENDIF
        tIntStr = Integer2String(nminutes)
        DurationString = DurationString(1:StrLen)//tIntStr(1:LEN_TRIM(tIntStr))//" minutes"
        StrLen = LEN_TRIM(DurationString)
        IF (nminutes .EQ. 1) StrLen = StrLen - 1
        NItems = NItems + 1
      ENDIF
      IF (DiffSecs .NE. 0) THEN
        IF (NItems .GT. 0) THEN
          DurationString = DurationString(1:StrLen)//", "
          StrLen = StrLen + 2
        ENDIF
        tIntStr = Integer2String(DiffSecs)
        DurationString = DurationString(1:StrLen)//tIntStr(1:LEN_TRIM(tIntStr))//" seconds"
        StrLen = LEN_TRIM(DurationString)
        IF (DiffSecs .EQ. 1) StrLen = StrLen - 1
        NItems = NItems + 1
      ENDIF

      END SUBROUTINE TimeElapsed
!
!*****************************************************************************
!
