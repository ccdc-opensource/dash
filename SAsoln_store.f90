!
!*****************************************************************************
!
    SUBROUTINE SA_soln_store()
    
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
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      CHARACTER*80       cssr_file, pdb_file, ccl_file, log_file, pro_file
      COMMON /outfilnam/ cssr_file, pdb_file, ccl_file, log_file, pro_file

      INTEGER            cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen
      COMMON /outfillen/ cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen

      LOGICAL         RESTART
      INTEGER                  SA_Run_Number
      INTEGER                                 MaxRuns, MaxMoves
      REAL                                                       ChiMult
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MaxMoves, ChiMult

      LOGICAL          PRO_saved
      COMMON /PROCOM/  PRO_saved(1:MaxRun)

      INTEGER I
      LOGICAL, EXTERNAL :: Get_SavePRO
      INTEGER    tFileHandle

      PRO_saved(SA_Run_Number) = .FALSE.
      IF (.NOT. Get_SavePRO()) RETURN
      tFileHandle = 61
      OPEN(UNIT=tFileHandle,FILE=pro_file(1:pro_flen),status='unknown',ERR=999)
      DO I = 1, NBIN
        WRITE(tFileHandle,12,ERR=999) XBIN(I), CHAR(9), YOBIN(I), CHAR(9), YCBIN(I), CHAR(9), EBIN(I)
12      FORMAT(F12.4,3(A,F12.4))
      ENDDO
! to overwrite:
      CLOSE(tFileHandle)
      PRO_saved(SA_Run_Number) = .TRUE.
      RETURN
  999 CALL ErrorMessage('Error while writing .pro file.')
      CLOSE(tFileHandle)

      END SUBROUTINE SA_soln_store
!
!*****************************************************************************
!
