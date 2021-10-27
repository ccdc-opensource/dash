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
      INTEGER FUNCTION PawleyErrorLog(Mode)
!
! Routine for error checking the pawley fit
! Modes can be as follows
! Mode = 1 : Add an error into local array
! Mode = 2 : Reset
!
! RETURNS : the number of errors that occurred
!
      INTEGER, INTENT (IN   ) :: Mode

      INTEGER Nerr

      SAVE Nerr
      DATA Nerr / 0 /

      PawleyErrorLog = Nerr
      SELECT CASE (Mode)
        CASE (1)
          Nerr = Nerr + 1
        CASE (2)
          Nerr = 0
      END SELECT

      END FUNCTION PawleyErrorLog
!
!*****************************************************************************
!
      SUBROUTINE PawleyWarning

      USE WINTERACTER
      USE DRUID_HEADER

      INTEGER IRetVal, IHan
      CHARACTER*4 NextLine
      CHARACTER*5000 text

      NextLine = CHAR(13)//CHAR(10)
      text = 'The extracted intensities are ill conditioned due to a high degree of'//NextLine// &
             'overlap in the data set and so could be unreliable in certain ranges.'//NextLine// &
             'Full details of the problems are recorded in the fit list file, polyp.lis.'//NextLine// &
             'You may be able to fix the problem by decreasing the data range in'//NextLine// &
             '2-theta or by increasing the overlap criterion.'//NextLine//NextLine// &
             'To proceed, click Yes, to view the list file, click No.'

      CALL WMessageBox(YesNo, QuestionIcon, CommonOK, text(1:LEN_TRIM(text)), 'Errors detected during Pawley fit')

      IF ( WInfoDialog(ExitButtonCommon) .NE. CommonYes ) THEN
        CALL WindowOpenChild(WIN_STYLE(HideWindow,-1,-1,-1,-1,0,'View pawley fit log file'),IHan)
        CALL WEditFile('polyp.lis', Modal, 0, 0, SystemFixed)
        IRetVal = InfoError(1)
      ENDIF 
   !   CALL PopActiveWindowID
            
      END SUBROUTINE PawleyWarning
!
!*****************************************************************************
!
