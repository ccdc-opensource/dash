
    SUBROUTINE SA_soln_store()
    
!ep July 2001
!   This subroutine is called by SA_Structure_Output.for.  It writes out
!   the powder diffraction pattern (2theta and yobs) and the calculated fit
!   (ycal) to a .pro file when a new minimum found.  File is overwritten with 
!   data for each new minimum.  
!   Like the .pdb files a new .pro file is written for each SA run and the 
!   file name appended with the run number.

      IMPLICIT NONE
!
!   The common blocks below contain the info required for .pro file
	INCLUDE 'PARAMS.INC'

      INTEGER          NBIN, LBIN
      REAL                         XBIN,       YOBIN,       YCBIN,       YBBIN,       EBIN
      COMMON /PROFBIN/ NBIN, LBIN, XBIN(MOBS), YOBIN(MOBS), YCBIN(MOBS), YBBIN(MOBS), EBIN(MOBS)

      CHARACTER*80       cssr_file, pdb_file, ccl_file, log_file, pro_file   
      COMMON /outfilnam/ cssr_file, pdb_file, ccl_file, log_file, pro_file

      INTEGER            cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen
      COMMON /outfillen/ cssr_flen, pdb_flen, ccl_flen, log_flen, pro_flen

      INTEGER I

    open(unit=61,file=pro_file(1:pro_flen),status='unknown')
    do 10 i = 1, NBIN
    write(61,12) char(9), XBIN(i), char(9), YOBIN(i),char(9), YCBIN(i)
12  format(3(a,f12.4))
10  continue

!   to overwrite:
    close(61)

    end subroutine SA_soln_store