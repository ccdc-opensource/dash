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

      CHARACTER*80       cssr_file, pdb_file, ccl_file, log_file, pro_file, bin_file   
      COMMON /outfilnam/ cssr_file, pdb_file, ccl_file, log_file, pro_file, bin_file

      INTEGER            cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen, bin_flen
      COMMON /outfillen/ cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen, bin_flen

      LOGICAL         RESTART
      INTEGER                  SA_Run_Number
      INTEGER                                 MaxRuns, MaxMoves
      REAL                                                       ChiMult
      COMMON /MULRUN/ RESTART, SA_Run_Number, MaxRuns, MaxMoves, ChiMult

      LOGICAL          PRO_saved
      COMMON /PROCOM/  PRO_saved(1:30)

      INTEGER I
      LOGICAL, EXTERNAL :: Get_SavePRO
      CHARACTER*MaxPathLength tFileName
      INTEGER    tFileHandle
      INTEGER    RecNr

      PRO_saved(SA_Run_Number+1) = .FALSE.
      IF (.NOT. Get_SavePRO()) RETURN
      OPEN(UNIT=61,FILE=pro_file(1:pro_flen),status='unknown')
      DO I = 1, NBIN
        WRITE(61,12) CHAR(9), XBIN(I), CHAR(9), YOBIN(I), CHAR(9), YCBIN(I)
12      FORMAT(3(A,F12.4))
      ENDDO
! to overwrite:
      CLOSE(61)
! Write out a binary file
      tFileName = pro_file(1:pro_flen-3)//'bin'
      tFileHandle = 10
! Open the file as direct access (i.e. non-sequential) unformatted with a record length of 1 (=4 bytes)
      OPEN(UNIT=tFileHandle,FILE=tFileName,ACCESS='DIRECT',RECL=1,FORM='UNFORMATTED',ERR=999)
      RecNr = 1
      DO I = 1, NBIN
        CALL FileWriteReal(tFileHandle,RecNr,XBIN (I))
        CALL FileWriteReal(tFileHandle,RecNr,YOBIN(I))
        CALL FileWriteReal(tFileHandle,RecNr,YCBIN(I))
      ENDDO
      PRO_saved(SA_Run_Number+1) = .TRUE.
      CLOSE(tFileHandle)
      RETURN
  999 CALL DebugErrorMessage("Couldn't open binary file.")
      CLOSE(tFileHandle)

      END SUBROUTINE SA_soln_store
!
!*****************************************************************************
!
