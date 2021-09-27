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
      SUBROUTINE SA_soln_store

!ep July 2001
!   This subroutine is called by SA_Structure_Output.for.  It writes out
!   the powder diffraction pattern (2theta and yobs) and the calculated fit
!   (ycal) to a .pro file when a new minimum found.  File is overwritten with 
!   data for each new minimum.  
!   Like the .pdb files a new .pro file is written for each SA run and the 
!   file name appended with the run number.

      USE VARIABLES

      IMPLICIT NONE

!   The common blocks below contain the info required for .pro file
      INCLUDE 'params.inc'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER                  OFBN_Len
      CHARACTER(MaxPathLength)           OutputFilesBaseName
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OFBN_Len, OutputFilesBaseName, SA_RunNumberStr

      INTEGER         Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                           ChiMult
      COMMON /MULRUN/ Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      LOGICAL         in_batch
      COMMON /BATEXE/ in_batch

      CHARACTER*20, EXTERNAL :: GetSeed1SuffixString
      LOGICAL, EXTERNAL :: Get_SavePRO
      INTEGER I
      INTEGER    hFile
      CHARACTER*20 tString

      IF (.NOT. Get_SavePRO()) RETURN

      hFile = 61
      tString = ''
      IF (in_batch) tString = GetSeed1SuffixString()
      OPEN(UNIT=hFile,FILE=OutputFilesBaseName(1:OFBN_Len)//TRIM(tString)// &
           SA_RunNumberStr//'.pro',status='unknown',ERR=999)
      WRITE(hFile,'((F12.4,3("'//CHAR(9)//'",F12.4)))',ERR=999) &
           (XBIN(I), YOBIN(I), YCBIN(I), EBIN(I), I=1, NBIN)
! to overwrite:
      CLOSE(hFile)
      RETURN
  999 CALL ErrorMessage('Error writing .pro file.')
      CLOSE(hFile)

      END SUBROUTINE SA_soln_store
!
!*****************************************************************************
!
