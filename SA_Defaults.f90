    subroutine sa_Defaults()
!
    character*80 logsa_file,cssr_file,pdb_file,ccl_file,log_file
    common /outfilnam/ logsa_file,cssr_file,pdb_file,ccl_file,log_file
    common /outfillen/ logsa_flen,cssr_flen,pdb_flen,ccl_flen,log_flen
	logical outfilset
	common /outfileset/ outfilset
	data outfilset/ .FALSE. /
!
    logsa_file='Druid.lsa'
	logsa_flen=len_trim(logsa_file)
	IF ( .NOT. outfilset ) THEN
	    cssr_file='SA_best.cssr'
		pdb_file='SA_best.pdb'
		ccl_file='SA_best.ccl'
		log_file='SA_best.log'
!
		cssr_flen=len_trim(cssr_file)
		pdb_flen=len_trim(pdb_file)
		ccl_flen=len_trim(ccl_file)
		log_flen=len_trim(ccl_file)
	END IF
!
    end

!C>> JCC New subroutine to set the output file names

	subroutine sa_SetOutputFiles(filehead)
!
	character*75 filehead ! Maximum permissible length.
    character*80 logsa_file,cssr_file,pdb_file,ccl_file,log_file
    common /outfilnam/ logsa_file,cssr_file,pdb_file,ccl_file,log_file
    common /outfillen/ logsa_flen,cssr_flen,pdb_flen,ccl_flen,log_flen
!
	logical outfilset
	common /outfileset/ outfilset
	integer i

	i = len_trim(filehead) + 1
!C>> By default, set the output files to <sdifile head>.cssr etc
!C Next code allows us to pass in a full filename (strips out the extension if present)
	 do while (i .GT. 0 .AND. filehead(i:i) .NE. '.')
		i = i - 1
	 end do
	 if (i .EQ. 0) i = len_trim(filehead) + 1
	 i = i - 1

    cssr_file= filehead(1:i)//'.cssr'
    pdb_file=  filehead(1:i)//'.pdb'
    ccl_file=  filehead(1:i)//'.ccl'
    log_file=  filehead(1:i)//'.log'	
    cssr_flen=len_trim(cssr_file)
    pdb_flen=len_trim(pdb_file)
    ccl_flen=len_trim(ccl_file)
	log_flen=len_trim(log_file)
	outfilset = .TRUE.
	end subroutine sa_SetOutputFiles