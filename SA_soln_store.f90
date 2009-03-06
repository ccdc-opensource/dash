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
