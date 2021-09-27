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
      SUBROUTINE sa_SetOutputFiles(TheFileHead)

      USE VARIABLES

      IMPLICIT NONE

      CHARACTER*(*), INTENT (IN   ) :: TheFileHead 

      INTEGER                  OFBN_Len
      CHARACTER(MaxPathLength)           OutputFilesBaseName
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OFBN_Len, OutputFilesBaseName, SA_RunNumberStr

! Find the last occurrence of '.'
      OFBN_Len = LEN_TRIM(TheFileHead)
      DO WHILE ((OFBN_Len .GT. 0) .AND. (TheFileHead(OFBN_Len:OFBN_Len) .NE. '.'))
        OFBN_Len = OFBN_Len - 1
      ENDDO
! If no '.' present, pretend there is one after the filename
      IF (OFBN_Len .EQ. 0) OFBN_Len = LEN_TRIM(TheFileHead) + 1
! Now point to the position just before the '.'
      OFBN_Len = OFBN_Len - 1
! We will append '_001.cssr', which is nine characters, and after that the total length shouldn't exceed 255
      IF (OFBN_Len .GT. (MaxPathLength-9)) THEN
        CALL DebugErrorMessage('File name too long in sa_SetOutputFiles()')
        OFBN_Len = MaxPathLength-9
      ENDIF
      OutputFilesBaseName = TheFileHead(1:OFBN_Len)

      END SUBROUTINE sa_SetOutputFiles
!
!*****************************************************************************
!
