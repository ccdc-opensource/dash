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
	INCLUDE 'PARAMS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN,       AVGESD
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS), AVGESD

      INTEGER                  OFBN_Len
      CHARACTER(MaxPathLength)           OutputFilesBaseName
      CHARACTER(3)                                            SA_RunNumberStr
      COMMON /basnam/          OFBN_Len, OutputFilesBaseName, SA_RunNumberStr

      LOGICAL         RESTART
      INTEGER                  Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves
      REAL                                                                    ChiMult
      COMMON /MULRUN/ RESTART, Curr_SA_Run, NumOf_SA_Runs, MaxRuns, MaxMoves, ChiMult

      INTEGER I
      LOGICAL, EXTERNAL :: Get_SavePRO
      INTEGER    hFile

      IF (.NOT. Get_SavePRO()) RETURN
      hFile = 61
      OPEN(UNIT=hFile,FILE=OutputFilesBaseName(1:OFBN_Len)//'_'//SA_RunNumberStr//'.pro',status='unknown',ERR=999)
      DO I = 1, NBIN
        WRITE(hFile,12,ERR=999) XBIN(I), CHAR(9), YOBIN(I), CHAR(9), YCBIN(I), CHAR(9), EBIN(I)
   12   FORMAT(F12.4,3(A,F12.4))
      ENDDO
! to overwrite:
      CLOSE(hFile)
      RETURN
  999 CALL ErrorMessage('Error writing .pro file.')
      CLOSE(hFile)

      END SUBROUTINE SA_soln_store
!
!*****************************************************************************
!
