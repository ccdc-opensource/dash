
    Subroutine SA_soln_store()
    
!ep July 2001
!   This subroutine is called by SA_Structure_Output.for.  It writes out
!   the powder diffraction pattern (2theta and yobs) and the calculated fit
!   (ycal) to a .pro file when a new minimum found.  File is overwritten with 
!   data for each new minimum.  
!   Like the .pdb files a new .pro file is written for each SA run and the 
!   file name appended with the run number.
    USE druid_header
!
!   The common blocks below contain the info required for .pro file
      PARAMETER (MOBS=15000)
      COMMON /PROFOBS/ NOBS,XOBS(MOBS),YOBS(MOBS),&
      YCAL(MOBS),YBAK(MOBS),EOBS(MOBS)
      COMMON /PROFBIN/ NBIN,LBIN,XBIN(MOBS),YOBIN(MOBS),&
      YCBIN(MOBS),YBBIN(MOBS),EBIN(MOBS)
      COMMON /PROFRAN/ XPMIN,XPMAX,YPMIN,YPMAX,XPGMIN,XPGMAX,&
      YPGMIN,YPGMAX,XPGMINOLD,XPGMAXOLD,YPGMINOLD,YPGMAXOLD,& 
      XGGMIN,XGGMAX,YGGMIN,YGGMAX
      character*80 logsa_file,cssr_file,pdb_file,ccl_file,log_file,pro_file   
      common /outfilnam/ logsa_file,cssr_file,pdb_file,ccl_file,log_file, pro_file
      common /outfillen/ logsa_flen,cssr_flen,pdb_flen,ccl_flen,log_flen, pro_flen
    


    open(unit=61,file=pro_file(1:pro_flen),status='unknown')
    do 10 i = 1,nobs
    write(61,12) xobs(i), yobs(i), ycal(i)
12  format(3(f12.6))
10  continue

!   to overwrite:
    close(61)

    end subroutine SA_soln_store